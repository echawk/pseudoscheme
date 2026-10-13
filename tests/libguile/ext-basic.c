/* A Guile extension, built against GNU Guile 3.0.11's headers, that
   uses the parts of the ABI C code depends on most: the inline macros
   (SCM_CAR, scm_is_pair, scm_cons, SCM_SIMPLE_VECTOR_REF, the
   bytevector contents), conversions, errors, calls back into Scheme,
   smobs, foreign objects and dynwind.  tests/libguile/basic.scm loads
   it into real Guile and into ours, and run.sh compares the two.  */

#include <libguile.h>
#include <string.h>

/* (sum-list '(1 2 3)) => 6, walking the list with the inline macros */
static SCM
sum_list (SCM lst)
{
  long total = 0;
  for (; scm_is_pair (lst); lst = SCM_CDR (lst))
    total += scm_to_long (SCM_CAR (lst));
  return scm_from_long (total);
}

/* (iota-list 3) => (0 1 2), consing in C */
static SCM
iota_list (SCM n)
{
  SCM result = SCM_EOL;
  for (long i = scm_to_long (n) - 1; i >= 0; i--)
    result = scm_cons (scm_from_long (i), result);
  return result;
}

/* (swap-pair! p): exchanges car and cdr in place */
static SCM
swap_pair_x (SCM p)
{
  SCM a = SCM_CAR (p);
  SCM_SETCAR (p, SCM_CDR (p));
  SCM_SETCDR (p, a);
  return SCM_UNSPECIFIED;
}

/* (vector-double! v): each element times two, in place */
static SCM
vector_double_x (SCM v)
{
  size_t n = SCM_SIMPLE_VECTOR_LENGTH (v);
  for (size_t i = 0; i < n; i++)
    SCM_SIMPLE_VECTOR_SET (v, i, scm_from_long (2 * scm_to_long (SCM_SIMPLE_VECTOR_REF (v, i))));
  return v;
}

/* (bytevector-fill-ramp! bv): byte i is i, through the contents pointer */
static SCM
bytevector_fill_ramp_x (SCM bv)
{
  size_t n = SCM_BYTEVECTOR_LENGTH (bv);
  unsigned char *p = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);
  for (size_t i = 0; i < n; i++)
    p[i] = (unsigned char) i;
  return bv;
}

/* (string-info s) => (length upcased-utf8 symbol) */
static SCM
string_info (SCM s)
{
  char *c = scm_to_utf8_string (s);
  for (char *q = c; *q; q++)
    if (*q >= 'a' && *q <= 'z')
      *q -= 32;
  SCM result = scm_list_3 (scm_from_size_t (scm_c_string_length (s)),
                           scm_from_utf8_string (c), scm_from_utf8_symbol (c));
  free (c);
  return result;
}

/* (call-twice f x) => (f (f x)) */
static SCM
call_twice (SCM f, SCM x)
{
  return scm_call_1 (f, scm_call_1 (f, x));
}

/* (checked-sqrt x): an error unless x is a non-negative real */
static SCM
checked_sqrt (SCM x)
{
  SCM_ASSERT_TYPE (scm_is_real (x), x, SCM_ARG1, "checked-sqrt", "real");
  double d = scm_to_double (x);
  if (d < 0)
    scm_out_of_range ("checked-sqrt", x);
  return scm_from_double (__builtin_sqrt (d));
}

/* (optional-args a [b [c]]) => what was given */
static SCM
optional_args (SCM a, SCM b, SCM c)
{
  return scm_list_3 (a, SCM_UNBNDP (b) ? scm_from_utf8_symbol ("none") : b,
                     SCM_UNBNDP (c) ? scm_from_utf8_symbol ("none") : c);
}

/* (rest-args a . rest) => (a rest-length) */
static SCM
rest_args (SCM a, SCM rest)
{
  return scm_list_2 (a, scm_length (rest));
}

/* a smob: a counter */
static scm_t_bits counter_tag;
static int counters_freed = 0;

static SCM
make_counter (SCM start)
{
  long *n = scm_gc_malloc (sizeof (long), "counter");
  *n = scm_to_long (start);
  return scm_new_smob (counter_tag, (scm_t_bits) n);
}

static SCM
counter_next_x (SCM c)
{
  scm_assert_smob_type (counter_tag, c);
  long *n = (long *) SCM_SMOB_DATA (c);
  return scm_from_long ((*n)++);
}

static SCM
counter_p (SCM x)
{
  return scm_from_bool (SCM_SMOB_PREDICATE (counter_tag, x));
}

static int
print_counter (SCM c, SCM port, scm_print_state *pstate)
{
  (void) pstate;
  scm_puts ("#<counter ", port);
  scm_display (scm_from_long (*(long *) SCM_SMOB_DATA (c)), port);
  scm_puts (">", port);
  return 1;
}

/* a foreign object: a point */
static SCM point_type;

static SCM
make_point (SCM x, SCM y)
{
  long *p = scm_gc_malloc_pointerless (2 * sizeof (long), "point");
  p[0] = scm_to_long (x);
  p[1] = scm_to_long (y);
  return scm_make_foreign_object_1 (point_type, p);
}

static SCM
point_coords (SCM pt)
{
  scm_assert_foreign_object_type (point_type, pt);
  long *p = scm_foreign_object_ref (pt, 0);
  return scm_list_2 (scm_from_long (p[0]), scm_from_long (p[1]));
}

/* dynwind: (with-cleanup thunk) calls thunk; the cleanup runs however
   it exits, and (cleanups) counts them */
static int cleanup_count = 0;

static void
cleanup (void *data)
{
  (void) data;
  cleanup_count++;
}

static SCM
with_cleanup (SCM thunk)
{
  SCM result;
  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler (cleanup, NULL, SCM_F_WIND_EXPLICITLY);
  result = scm_call_0 (thunk);
  scm_dynwind_end ();
  return result;
}

static SCM
cleanups (void)
{
  return scm_from_int (cleanup_count);
}

/* (lookup-and-call "name" x): a procedure from the current module */
static SCM
lookup_and_call (SCM name, SCM x)
{
  char *c = scm_to_utf8_string (name);
  SCM var = scm_c_lookup (c);
  free (c);
  return scm_call_1 (scm_variable_ref (var), x);
}

/* (assoc-update alist key value): a new alist, through the Scheme-level
   functions */
static SCM
assoc_update (SCM alist, SCM key, SCM value)
{
  return scm_acons (key, value, scm_assoc_remove_x (scm_list_copy (alist), key));
}

/* (hash-roundtrip): a table filled and read in C */
static SCM
hash_roundtrip (void)
{
  SCM table = scm_c_make_hash_table (7);
  for (int i = 0; i < 5; i++)
    scm_hashq_set_x (table, scm_from_int (i), scm_from_int (i * i));
  return scm_hashq_ref (table, scm_from_int (3), SCM_BOOL_F);
}

void
init_basic (void)
{
  counter_tag = scm_make_smob_type ("counter", sizeof (long));
  scm_set_smob_print (counter_tag, print_counter);
  point_type = scm_make_foreign_object_type (scm_from_utf8_symbol ("point"),
                                             scm_list_1 (scm_from_utf8_symbol ("data")),
                                             NULL);
  scm_c_define_gsubr ("sum-list", 1, 0, 0, sum_list);
  scm_c_define_gsubr ("iota-list", 1, 0, 0, iota_list);
  scm_c_define_gsubr ("swap-pair!", 1, 0, 0, swap_pair_x);
  scm_c_define_gsubr ("vector-double!", 1, 0, 0, vector_double_x);
  scm_c_define_gsubr ("bytevector-fill-ramp!", 1, 0, 0, bytevector_fill_ramp_x);
  scm_c_define_gsubr ("string-info", 1, 0, 0, string_info);
  scm_c_define_gsubr ("call-twice", 2, 0, 0, call_twice);
  scm_c_define_gsubr ("checked-sqrt", 1, 0, 0, checked_sqrt);
  scm_c_define_gsubr ("optional-args", 1, 2, 0, optional_args);
  scm_c_define_gsubr ("rest-args", 1, 0, 1, rest_args);
  scm_c_define_gsubr ("make-counter", 1, 0, 0, make_counter);
  scm_c_define_gsubr ("counter-next!", 1, 0, 0, counter_next_x);
  scm_c_define_gsubr ("counter?", 1, 0, 0, counter_p);
  scm_c_define_gsubr ("make-point", 2, 0, 0, make_point);
  scm_c_define_gsubr ("point-coords", 1, 0, 0, point_coords);
  scm_c_define_gsubr ("with-cleanup", 1, 0, 0, with_cleanup);
  scm_c_define_gsubr ("cleanups", 0, 0, 0, cleanups);
  scm_c_define_gsubr ("lookup-and-call", 2, 0, 0, lookup_and_call);
  scm_c_define_gsubr ("assoc-update", 3, 0, 0, assoc_update);
  scm_c_define_gsubr ("hash-roundtrip", 0, 0, 0, hash_roundtrip);
  (void) counters_freed;
}
