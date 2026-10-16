/* A program that embeds Guile (docs/libguile.md, "Embedding"): main
   calls scm_with_guile, evaluates Scheme, defines gsubrs, a smob and a
   foreign object type, and prints.  run.sh builds it against real Guile
   and against ours: the output must be the same.  */

#include <libguile.h>
#include <stdio.h>

static scm_t_bits counter_tag;
static SCM point_type;

static SCM
c_add (SCM a, SCM b)
{
  return scm_sum (a, b);
}

static SCM
c_apply_twice (SCM f, SCM x)
{
  return scm_call_1 (f, scm_call_1 (f, x));
}

static SCM
make_counter (SCM start)
{
  return scm_new_smob (counter_tag, (scm_t_bits) scm_to_long (start));
}

static SCM
counter_next_x (SCM c)
{
  scm_assert_smob_type (counter_tag, c);
  long n = (long) SCM_SMOB_DATA (c) + 1;
  SCM_SET_SMOB_DATA (c, (scm_t_bits) n);
  return scm_from_long (n);
}

static int
print_counter (SCM c, SCM port, scm_print_state *state)
{
  scm_puts ("#<counter ", port);
  scm_display (scm_from_long ((long) SCM_SMOB_DATA (c)), port);
  scm_puts (">", port);
  return 1;
}

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

static void
show (const char *label, SCM value)
{
  scm_puts (label, scm_current_output_port ());
  scm_puts (" => ", scm_current_output_port ());
  scm_write (value, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_force_output (scm_current_output_port ());
}

static void *
guile_main (void *data)
{
  scm_c_define_gsubr ("c-add", 2, 0, 0, c_add);
  scm_c_define_gsubr ("c-apply-twice", 2, 0, 0, c_apply_twice);
  counter_tag = scm_make_smob_type ("counter", sizeof (long));
  scm_set_smob_print (counter_tag, print_counter);
  point_type = scm_make_foreign_object_type (scm_from_utf8_symbol ("point"),
                                             scm_list_1 (scm_from_utf8_symbol ("data")), NULL);
  scm_c_define_gsubr ("make-counter", 1, 0, 0, make_counter);
  scm_c_define_gsubr ("counter-next!", 1, 0, 0, counter_next_x);
  scm_c_define_gsubr ("make-point", 2, 0, 0, make_point);
  scm_c_define_gsubr ("point-coords", 1, 0, 0, point_coords);

  show ("arith", scm_c_eval_string ("(+ 1 2 3)"));
  show ("list", scm_c_eval_string ("(map (lambda (x) (* x x)) '(1 2 3 4))"));
  show ("module", scm_c_eval_string ("(module-name (current-module))"));
  show ("gsubr", scm_c_eval_string ("(c-add 40 2)"));
  show ("callback", scm_c_eval_string ("(c-apply-twice (lambda (x) (* 2 x)) 5)"));
  show ("smob", scm_c_eval_string ("(let ((c (make-counter 10))) (counter-next! c) (counter-next! c) c)"));
  show ("foreign", scm_c_eval_string ("(point-coords (make-point 3 4))"));
  show ("define-then-lookup",
        (scm_c_eval_string ("(define embedded-answer 42)"),
         scm_variable_ref (scm_c_lookup ("embedded-answer"))));
  show ("catch", scm_c_eval_string ("(catch #t (lambda () (car 5)) (lambda (key . args) key))"));
  show ("string", scm_from_utf8_string ("embedded"));
  scm_c_eval_string ("(display \"from Scheme\") (newline)");
  return NULL;
}

int
main (int argc, char **argv)
{
  scm_with_guile (guile_main, NULL);
  fflush (stdout);
  printf ("done\n");
  return 0;
}
