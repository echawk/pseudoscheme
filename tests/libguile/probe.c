/* Guile's ABI facts, asked of a real Guile (docs/libguile.md): built
   against GNU Guile 3.0.11's headers and run with its libguile, this
   prints, as Lisp data, the values and layouts our libguile must
   share with it.  src/libguile/abi.lisp records what it printed, and
   tests/libguile/run.sh checks that it still does.

     cc probe.c $(pkg-config --cflags --libs guile-3.0) -o probe && ./probe

   Layouts are found by making objects and reading their words, not
   from Guile's definitions of them.  */

#include <libguile.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

static void
fact (const char *name, scm_t_bits value)
{
  printf ("  (%s #x%lx)\n", name, (unsigned long) value);
}

/* The first N words of heap object X.  */
static void
words (const char *name, SCM x, int n)
{
  scm_t_bits *p = (scm_t_bits *) SCM_UNPACK (x);
  printf ("  (%s", name);
  for (int i = 0; i < n; i++)
    printf (" #x%lx", (unsigned long) p[i]);
  printf (")\n");
}

/* Where in X's words the bit pattern of D is, as a word index.  */
static int
double_index (SCM x, double d)
{
  scm_t_bits *p = (scm_t_bits *) SCM_UNPACK (x);
  for (int i = 0; i < 4; i++)
    if (memcmp (&p[i], &d, sizeof d) == 0)
      return i;
  return -1;
}

static void *
probe (void *ignore)
{
  (void) ignore;
  printf ("(:guile-version \"%s\"\n", scm_to_utf8_string (scm_version ()));

  /* immediates */
  fact (":false", SCM_UNPACK (SCM_BOOL_F));
  fact (":true", SCM_UNPACK (SCM_BOOL_T));
  fact (":null", SCM_UNPACK (SCM_EOL));
  fact (":elisp-nil", SCM_UNPACK (SCM_ELISP_NIL));
  fact (":unspecified", SCM_UNPACK (SCM_UNSPECIFIED));
  fact (":undefined", SCM_UNPACK (SCM_UNDEFINED));
  fact (":eof", SCM_UNPACK (SCM_EOF_VAL));
  fact (":fixnum-0", SCM_UNPACK (scm_from_int (0)));
  fact (":fixnum-1", SCM_UNPACK (scm_from_int (1)));
  fact (":fixnum--1", SCM_UNPACK (scm_from_int (-1)));
  fact (":char-a", SCM_UNPACK (SCM_MAKE_CHAR ('a')));
  fact (":most-positive-fixnum", SCM_MOST_POSITIVE_FIXNUM);

  /* heap objects: their first words */
  words (":pair", scm_cons (scm_from_int (1), scm_from_int (2)), 2);
  words (":vector-3", scm_c_make_vector (3, SCM_BOOL_T), 4);
  {
    SCM bv = scm_c_make_bytevector (5);
    words (":bytevector-5", bv, 4);
    printf ("  (:bytevector-contents-index %ld)\n",
            (long) ((scm_t_bits *) SCM_UNPACK (bv))[2]
            - (long) SCM_BYTEVECTOR_CONTENTS (bv));
  }
  words (":string-abc", scm_from_utf8_string ("abc"), 4);
  words (":symbol-abc", scm_from_utf8_symbol ("abc"), 3);
  words (":keyword-abc", scm_from_utf8_keyword ("abc"), 2);
  words (":variable", scm_make_variable (scm_from_int (7)), 2);
  words (":pointer", scm_from_pointer ((void *) 0x1234, NULL), 2);
  {
    SCM d = scm_from_double (1.5);
    words (":flonum", d, 3);
    printf ("  (:flonum-index %d)\n", double_index (d, 1.5));
  }
  words (":bignum", scm_from_uint64 (0xffffffffffffffffULL), 2);
  words (":fraction", scm_divide (scm_from_int (1), scm_from_int (3)), 3);
  {
    SCM z = scm_c_make_rectangular (1.5, 2.5);
    words (":complex", z, 4);
    printf ("  (:complex-indexes %d %d)\n", double_index (z, 1.5), double_index (z, 2.5));
  }
  words (":procedure", scm_c_eval_string ("car"), 2);
  words (":hash-table", scm_c_make_hash_table (7), 1);
  words (":module", scm_current_module (), 1);
  words (":fluid", scm_make_fluid (), 1);
  words (":port", scm_current_output_port (), 1);

  /* structs: the word that says a struct's vtable */
  {
    SCM vtable = scm_make_vtable (scm_from_utf8_string ("pwpw"), SCM_BOOL_F);
    SCM s = scm_make_struct_no_tail (vtable, scm_list_2 (scm_from_int (1), scm_from_int (2)));
    words (":struct", s, 3);
    fact (":struct-vtable", SCM_UNPACK (vtable));
  }

  /* smobs and their descriptor table */
  {
    scm_t_bits tag = scm_make_smob_type ("probe", 0);
    fact (":smob-tag", tag);
    SCM s = scm_new_smob (tag, 0x55);
    words (":smob", s, 2);
    printf ("  (:smob-descriptor-size %lu)\n", (unsigned long) sizeof (scm_smob_descriptor));
    printf ("  (:smob-descriptor-offsets (:name %lu :size %lu :mark %lu :free %lu :print %lu :equalp %lu :apply %lu))\n",
            (unsigned long) offsetof (scm_smob_descriptor, name),
            (unsigned long) offsetof (scm_smob_descriptor, size),
            (unsigned long) offsetof (scm_smob_descriptor, mark),
            (unsigned long) offsetof (scm_smob_descriptor, free),
            (unsigned long) offsetof (scm_smob_descriptor, print),
            (unsigned long) offsetof (scm_smob_descriptor, equalp),
            (unsigned long) offsetof (scm_smob_descriptor, apply));
  }

  /* flags */
  fact (":wind-explicitly", SCM_F_WIND_EXPLICITLY);
  fact (":dynwind-rewindable", SCM_F_DYNWIND_REWINDABLE);

  /* structures callers allocate and our functions fill */
  printf ("  (:sizeof-array-handle %lu)\n", (unsigned long) sizeof (scm_t_array_handle));
  printf ("  (:sizeof-c-hook %lu)\n", (unsigned long) sizeof (scm_t_c_hook));
  printf ("  (:sizeof-array-dim %lu)\n", (unsigned long) sizeof (scm_t_array_dim));
  printf (")\n");
  return NULL;
}

int
main (void)
{
  scm_with_guile (probe, NULL);
  return 0;
}
