/* A program started as Guile's own main does: scm_boot_guile calls
   inner_main with the arguments, and then exits.  Compared with real
   Guile's output, as embed-basic.c is (run.sh).  */

#include <libguile.h>

static SCM
c_triple (SCM x)
{
  return scm_product (x, scm_from_int (3));
}

static void
inner_main (void *closure, int argc, char **argv)
{
  scm_c_define_gsubr ("c-triple", 1, 0, 0, c_triple);
  scm_display (scm_from_int (argc), scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_write (scm_c_eval_string ("(list (c-triple 14) (string-append \"a\" \"b\"))"),
             scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_write (scm_c_eval_string ("(length (program-arguments))"), scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_c_eval_string ("(use-modules (srfi srfi-1)) (write (fold + 0 '(1 2 3))) (newline)");
  scm_c_eval_string ("(format #t \"~a and ~s~%\" \"x\" \"y\")");
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}
