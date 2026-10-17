/* Our libguile's C core (docs/libguile.md).
 *
 * libguile-3.0.so.1 is a thin C layer over Guile mode, which is Lisp.
 * Programs and extensions built against GNU Guile 3.0's headers call
 * the functions api.c defines (generated from src/libguile/table.lisp),
 * and each of those calls the Lisp side through one entry point,
 * psg_entry.  What is here is what must be C:
 *
 *   - the heap C sees: Guile objects in Guile's own layout, allocated
 *     by the Boehm collector (as in Guile), each with two hidden words
 *     before it that tie it to the Lisp object it mirrors;
 *   - finalization, which tells the Lisp side when C can no longer
 *     reach a mirror;
 *   - registering threads with the collector;
 *   - the variadic functions (scm_list_n, scm_call, ...), which a
 *     generated stub can't express.
 *
 * This file is our own; it uses no code from GNU Guile.  Guile's ABI
 * facts (type tags, layouts) are in src/libguile/abi.lisp, measured
 * from a real Guile build by tests/libguile/probe.c.  */

#define _GNU_SOURCE 1
#define GC_THREADS 1
#include <gc/gc.h>
#include <gc/gc_mark.h>
#include <dlfcn.h>
#include <pthread.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "psg.h"

/* ------------------------------------------------------------------ */
/* The Lisp side's entry point: ID names an API function (table.lisp's
   order), ARGS are its arguments, each widened to a word.  The Lisp
   side sets it when it loads us (psg_set_entry).  */

static uintptr_t boot_entry (uintptr_t id, uintptr_t *args);

psg_entry_t psg_entry_fn = boot_entry;

void
psg_set_entry (psg_entry_t f)
{
  psg_entry_fn = f;
}

/* Lisp's profile (PSEUDOSCHEME_LIBGUILE_PROFILE) is printed at exit.  */
void
psg_at_exit (void (*f) (void))
{
  atexit (f);
}

/* ------------------------------------------------------------------ */
/* Embedding.  When a C program linked against us (rather than Lisp
   loading us as an extension's libguile) makes its first call, there
   is no Lisp yet: boot_entry starts one.  It loads SBCL's runtime as a
   shared library (libsbcl.so) and starts it on the Guile mode's core
   (pseudoscheme-guile.core, built by `make core`), whose start-up
   sets the entry point (start-embedded in src/libguile/impl.lisp) and returns here.

   PSEUDOSCHEME_GUILE_CORE names the core (default: the one beside this
   library), PSEUDOSCHEME_LIBSBCL the runtime (default: libsbcl.so on
   the library path), PSEUDOSCHEME_HEAP the heap's limit (8GB).  */

/* Written by the core's start-up: its callable export, psg-entry.  */
psg_entry_t psg_entry = 0;

static pthread_mutex_t boot_lock = PTHREAD_MUTEX_INITIALIZER;

static void
boot_failed (const char *what, const char *detail)
{
  fprintf (stderr, "libguile (Pseudoscheme): %s%s%s\n", what,
           detail ? ": " : "", detail ? detail : "");
  abort ();
}

static void
boot_lisp (void)
{
  char core[4096];
  const char *env = getenv ("PSEUDOSCHEME_GUILE_CORE");
  if (env)
    snprintf (core, sizeof core, "%s", env);
  else
    {
      Dl_info info;
      if (!dladdr ((void *) boot_lisp, &info) || !info.dli_fname)
        boot_failed ("can't find where this library is", 0);
      snprintf (core, sizeof core, "%s", info.dli_fname);
      char *slash = strrchr (core, '/');
      snprintf (slash ? slash + 1 : core, sizeof core - (slash ? slash + 1 - core : 0),
                "pseudoscheme-guile.core");
    }
  const char *libsbcl = getenv ("PSEUDOSCHEME_LIBSBCL");
  void *runtime = dlopen (libsbcl ? libsbcl : "libsbcl.so", RTLD_NOW | RTLD_GLOBAL);
  if (!runtime)
    boot_failed ("can't load SBCL's runtime", dlerror ());
  int (*initialize_lisp) (int, char **, char **)
    = (int (*) (int, char **, char **)) dlsym (runtime, "initialize_lisp");
  if (!initialize_lisp)
    boot_failed ("no initialize_lisp in SBCL's runtime", dlerror ());
  const char *heap = getenv ("PSEUDOSCHEME_HEAP");
  char *argv[] = { "pseudoscheme", "--core", core,
                   "--dynamic-space-size", (char *) (heap ? heap : "8GB"),
                   "--control-stack-size", "256MB",
                   "--noinform", "--disable-ldb", "--lose-on-corruption",
                   "--end-runtime-options", 0 };
  extern char **environ;
  initialize_lisp (sizeof argv / sizeof argv[0] - 1, argv, environ);
  if (psg_entry_fn == boot_entry)
    {
      if (!psg_entry)
        boot_failed ("the core didn't start the Guile mode", core);
      psg_entry_fn = psg_entry;
    }
}

static uintptr_t
boot_entry (uintptr_t id, uintptr_t *args)
{
  pthread_mutex_lock (&boot_lock);
  if (psg_entry_fn == boot_entry)
    boot_lisp ();
  pthread_mutex_unlock (&boot_lock);
  return psg_entry_fn (id, args);
}

/* ------------------------------------------------------------------ */
/* Threads.  The collector must know every thread that can hold
   pointers into its heap on its stack: a thread is registered the
   first time it allocates or calls in, and unregistered when it
   exits (through a thread-specific key's destructor).  */

static pthread_key_t registered_key;
static pthread_once_t key_once = PTHREAD_ONCE_INIT;

static void
unregister_thread (void *ignore)
{
  (void) ignore;
  GC_unregister_my_thread ();
}

static void
make_key (void)
{
  pthread_key_create (&registered_key, unregister_thread);
}

void
psg_ensure_thread (void)
{
  if (pthread_getspecific (registered_key))
    return;
  pthread_once (&key_once, make_key);
  if (!GC_thread_is_registered ())
    {
      struct GC_stack_base sb;
      GC_get_stack_base (&sb);
      GC_register_my_thread (&sb);
    }
  pthread_setspecific (registered_key, (void *) 1);
}

/* ------------------------------------------------------------------ */
/* The heap.  An object's two hidden words: its handle (the Lisp side's
   index of the object it mirrors, or 0) and its flags (bit 0: exported
   since it was last found unreachable; the rest: the epoch of its last
   synchronization, which the Lisp side keeps).  */

static int initialized = 0;
static void init_marked_kind (void);

static void
psg_init_gc (void)
{
  if (initialized)
    return;
  initialized = 1;
  GC_set_all_interior_pointers (1);
  GC_set_finalize_on_demand (1);
  GC_INIT ();
  GC_allow_register_threads ();
  init_marked_kind ();
  pthread_once (&key_once, make_key);
  pthread_setspecific (registered_key, (void *) 1);
}

__attribute__ ((constructor)) static void
psg_constructor (void)
{
  psg_init_gc ();
}

void *
psg_alloc (size_t bytes)
{
  psg_ensure_thread ();
  char *base = GC_MALLOC (PSG_HIDDEN_BYTES + bytes);
  if (!base)
    abort ();
  return base + PSG_HIDDEN_BYTES;
}

void *
psg_alloc_pointerless (size_t bytes)
{
  psg_ensure_thread ();
  char *base = GC_MALLOC_ATOMIC (PSG_HIDDEN_BYTES + bytes);
  if (!base)
    abort ();
  memset (base, 0, PSG_HIDDEN_BYTES + bytes);
  return base + PSG_HIDDEN_BYTES;
}

/* Smobs whose type has a mark function (scm_set_smob_mark).  Their
   data may sit where the collector doesn't look (memory from new or
   malloc), reachable only through what the mark function marks, as in
   Guile.  They are allocated of a kind of their own, whose mark
   procedure scans the cell, calls the type's mark function and has
   scm_gc_mark push what that marks.  */
extern uintptr_t scm_smobs[];

static int marked_kind = -1;
static __thread struct GC_ms_entry *mark_top, *mark_limit;
static __thread void **mark_source;

static struct GC_ms_entry *
marked_mark (GC_word *addr, struct GC_ms_entry *top, struct GC_ms_entry *limit,
             GC_word env)
{
  size_t words = GC_size (addr) / sizeof (GC_word), i;
  uintptr_t *cell = (uintptr_t *) ((char *) addr + PSG_HIDDEN_BYTES);
  (void) env;
  for (i = 0; i < words; i++)
    top = GC_MARK_AND_PUSH ((void *) addr[i], top, limit, (void **) addr);
  if (words > 2 && (cell[0] & 0x7f) == 0x77)
    {
      uintptr_t (*mark) (uintptr_t) = (uintptr_t (*) (uintptr_t)) scm_smobs[((cell[0] >> 8) & 0xff) * 8 + 2];
      if (mark)
        {
          uintptr_t next;
          mark_top = top;
          mark_limit = limit;
          mark_source = (void **) addr;
          next = mark ((uintptr_t) cell);
          top = mark_top;
          if (next)
            top = GC_MARK_AND_PUSH ((void *) next, top, limit, (void **) addr);
          mark_top = 0;
        }
    }
  return top;
}

/* scm_gc_mark, called by a mark function.  */
uintptr_t
scm_gc_mark (uintptr_t x)
{
  if (mark_top)
    mark_top = GC_MARK_AND_PUSH ((void *) x, mark_top, mark_limit, mark_source);
  return x;
}

static void
init_marked_kind (void)
{
  void **free_list = (void **) GC_new_free_list ();
  unsigned proc = GC_new_proc (marked_mark);
  marked_kind = GC_new_kind (free_list, GC_MAKE_PROC (proc, 0), 0, 1);
}

void *
psg_alloc_marked (size_t bytes)
{
  psg_ensure_thread ();
  char *base = GC_generic_malloc (PSG_HIDDEN_BYTES + bytes, marked_kind);
  if (!base)
    abort ();
  return base + PSG_HIDDEN_BYTES;
}

/* Is P an object of our heap (with hidden words), rather than static
   data or memory from elsewhere?  */
int
psg_heap_object_p (void *p)
{
  return GC_base (p) == (void *) ((char *) p - PSG_HIDDEN_BYTES);
}

/* Finalization: mirrors (objects with a handle) are registered, and
   when the collector finds one unreachable its address is queued.  The
   Lisp side takes the queue (psg_take_unreachable) at its next entry,
   and either drops the mirror (clearing its handle) or, if it was
   exported again meanwhile, registers it again (psg_watch).  A queued
   object stays alive: the queue is uncollectable memory.  */

static pthread_mutex_t queue_lock = PTHREAD_MUTEX_INITIALIZER;
static void **queue = 0;
static size_t queue_length = 0, queue_size = 0;

static void
on_unreachable (void *base, void *ignore)
{
  (void) ignore;
  pthread_mutex_lock (&queue_lock);
  if (queue_length == queue_size)
    {
      size_t n = queue_size ? 2 * queue_size : 1024;
      void **q = GC_MALLOC_UNCOLLECTABLE (n * sizeof (void *));
      if (queue)
        {
          memcpy (q, queue, queue_length * sizeof (void *));
          GC_FREE (queue);
        }
      queue = q;
      queue_size = n;
    }
  queue[queue_length++] = (char *) base + PSG_HIDDEN_BYTES;
  pthread_mutex_unlock (&queue_lock);
}

void
psg_watch (void *p)
{
  GC_REGISTER_FINALIZER_NO_ORDER ((char *) p - PSG_HIDDEN_BYTES,
                                  on_unreachable, 0, 0, 0);
}

/* Run the pending finalizations, then move up to MAX queued objects
   to OUT; the number moved.  */
size_t
psg_take_unreachable (void **out, size_t max)
{
  size_t n;
  GC_invoke_finalizers ();
  pthread_mutex_lock (&queue_lock);
  n = queue_length < max ? queue_length : max;
  memcpy (out, queue + (queue_length - n), n * sizeof (void *));
  queue_length -= n;
  pthread_mutex_unlock (&queue_lock);
  return n;
}

/* Whether psg_take_unreachable has anything to do.  */
int
psg_reap_needed (void)
{
  return GC_should_invoke_finalizers () || queue_length;
}

/* Roots for C objects the Lisp side holds: a cell the collector
   always scans, holding the object.  */
void **
psg_make_root (void *p)
{
  void **cell = GC_MALLOC_UNCOLLECTABLE (sizeof (void *));
  *cell = p;
  return cell;
}

void
psg_free_root (void **cell)
{
  GC_FREE (cell);
}

void
psg_collect (void)
{
  GC_gcollect ();
}

/* ------------------------------------------------------------------ */
/* Allocation as libguile exports it.  Guile's inline scm_cell,
   scm_double_cell and scm_words call scm_gc_malloc outside libguile,
   so every object C makes comes through here.  */

void *
scm_gc_malloc (size_t size, const char *what)
{
  (void) what;
  void *p = psg_alloc (size);
  memset (p, 0, size);
  return p;
}

void *
scm_gc_malloc_pointerless (size_t size, const char *what)
{
  (void) what;
  return psg_alloc_pointerless (size);
}

void *
scm_gc_calloc (size_t size, const char *what)
{
  return scm_gc_malloc (size, what);
}

void *
scm_gc_realloc (void *mem, size_t old_size, size_t new_size, const char *what)
{
  void *p = scm_gc_malloc (new_size, what);
  if (mem)
    memcpy (p, mem, old_size < new_size ? old_size : new_size);
  return p;
}

void
scm_gc_free (void *mem, size_t size, const char *what)
{
  (void) mem;
  (void) size;
  (void) what;
}

char *scm_gc_strndup (const char *str, size_t n, const char *what);

char *
scm_gc_strdup (const char *str, const char *what)
{
  return scm_gc_strndup (str, strlen (str), what);
}

char *
scm_gc_strndup (const char *str, size_t n, const char *what)
{
  char *s = scm_gc_malloc_pointerless (n + 1, what);
  memcpy (s, str, n);
  s[n] = 0;
  return s;
}

void *
scm_malloc (size_t size)
{
  void *p = malloc (size);
  if (!p)
    abort ();
  return p;
}

void *
scm_calloc (size_t size)
{
  void *p = calloc (1, size);
  if (!p)
    abort ();
  return p;
}

void *
scm_realloc (void *mem, size_t size)
{
  void *p = realloc (mem, size);
  if (!p)
    abort ();
  return p;
}

char *
scm_strdup (const char *str)
{
  return strdup (str);
}

char *
scm_strndup (const char *str, size_t n)
{
  return strndup (str, n);
}

void
scm_gc_register_allocation (size_t size)
{
  (void) size;
}

/* ------------------------------------------------------------------ */
/* Variadic functions: their arguments, up to the SCM_UNDEFINED that
   ends them, become a vector the Lisp side reads (count first).  */

#define MAX_VARARGS 256

static uintptr_t
call_with_list (uintptr_t id, uintptr_t first_arg, uintptr_t elt1, va_list ap)
{
  uintptr_t a[MAX_VARARGS + 2];
  size_t n = 0;
  uintptr_t x = elt1;
  a[1] = first_arg;
  while (x != PSG_SCM_UNDEFINED && n < MAX_VARARGS)
    {
      a[2 + n++] = x;
      x = va_arg (ap, uintptr_t);
    }
  a[0] = n;
  return psg_entry_fn (id, a);
}

uintptr_t
scm_list_n (uintptr_t elt, ...)
{
  va_list ap;
  uintptr_t r;
  va_start (ap, elt);
  r = call_with_list (PSG_ID_LIST_N, 0, elt, ap);
  va_end (ap);
  return r;
}

uintptr_t
scm_call (uintptr_t proc, ...)
{
  va_list ap;
  uintptr_t r, first;
  va_start (ap, proc);
  first = va_arg (ap, uintptr_t);
  r = call_with_list (PSG_ID_CALL, proc, first, ap);
  va_end (ap);
  return r;
}

/* scm_c_export's names end with a null pointer.  */
void
scm_c_export (const char *name, ...)
{
  uintptr_t a[MAX_VARARGS + 1];
  size_t n = 0;
  va_list ap;
  const char *s = name;
  va_start (ap, name);
  while (s && n < MAX_VARARGS)
    {
      a[1 + n++] = (uintptr_t) s;
      s = va_arg (ap, const char *);
    }
  va_end (ap);
  a[0] = n;
  psg_entry_fn (PSG_ID_C_EXPORT, a);
}

/* A smob whose type has a free function: the function runs when the
   collector finds the smob unreachable (the descriptors, data.c).  */
static void
free_smob (void *base, void *ignore)
{
  (void) ignore;
  uintptr_t *smob = (uintptr_t *) ((char *) base + PSG_HIDDEN_BYTES);
  size_t n = (smob[0] >> 8) & 0xff;
  size_t (*free_fn) (uintptr_t) = (size_t (*) (uintptr_t)) scm_smobs[n * 8 + 3];
  if (free_fn)
    free_fn ((uintptr_t) smob);
}

void
psg_finalize_smob (void *p)
{
  GC_REGISTER_FINALIZER_NO_ORDER ((char *) p - PSG_HIDDEN_BYTES, free_smob, 0, 0, 0);
}
