/* Changes made on one side seen by the other, around calls that only
   read (caar, hashq-set!, assq): in-place changes from C at each depth
   of a structure, changes from Scheme, and variables.  Compared with
   real Guile's output (run.sh).  */

#include <libguile.h>
#include <stdio.h>

static void
show (const char *label, SCM value)
{
  scm_puts (label, scm_current_output_port ());
  scm_puts (" => ", scm_current_output_port ());
  scm_write (value, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
}

static void *
guile_main (void *data)
{
  /* an alist of 300 entries, read by index as LilyPond reads a font's glyphs */
  SCM alist = SCM_EOL;
  for (int i = 299; i >= 0; i--)
    alist = scm_cons (scm_cons (scm_from_int (i), scm_from_int (i * i)), alist);
  SCM table = scm_c_make_hash_table (16);
  long sum = 0;
  for (SCM s = alist; scm_is_pair (s); s = SCM_CDR (s))
    {
      scm_hashq_set_x (table, scm_caar (s), scm_cdar (s));
      sum += scm_to_long (scm_cdar (s));
    }
  show ("sum", scm_from_long (sum));
  show ("table", scm_hashq_ref (table, scm_from_int (17), SCM_BOOL_F));

  /* C changes the structure in place, one level down, then two */
  SCM_SETCDR (SCM_CAR (alist), scm_from_int (-1));
  show ("cdar after C set the entry's cdr", scm_cdar (alist));
  SCM_SETCAR (SCM_CAR (SCM_CDR (alist)), scm_from_int (-2));
  show ("caadr after C set a nested car", scm_caar (SCM_CDR (alist)));
  SCM_SETCDR (SCM_CDR (alist), scm_list_1 (scm_from_int (-3)));
  show ("length after C cut the spine", scm_length (alist));
  show ("assq", scm_sloppy_assq (scm_from_int (0), alist));

  /* Scheme changes it, with reads in between */
  SCM nested = scm_c_eval_string ("(define nested (list (list 1 2) (list 3 4)))  nested");
  show ("nested", scm_cdar (nested));
  scm_c_eval_string ("(set-car! (cdr (car nested)) 'x)");
  show ("cdar after Scheme set-car!", scm_cdar (nested));
  show ("caar again", scm_caar (nested));
  scm_c_eval_string ("(set-cdr! (car nested) '(y z))");
  show ("cdar after Scheme set-cdr!", scm_cdar (nested));
  scm_length (nested);		/* passes it again: C reads it inline now */
  show ("C sees the pair directly", SCM_CDR (SCM_CAR (nested)));
  scm_c_eval_string ("(set-car! nested 'whole)");
  scm_length (nested);
  show ("car replaced", SCM_CAR (nested));

  /* and a variable, which C reads and writes in place */
  scm_c_define ("v", scm_from_int (1));
  SCM var = scm_c_lookup ("v");
  SCM_VARIABLE_SET (var, scm_from_int (5));
  scm_hashq_set_x (table, scm_from_int (1), scm_from_int (1));
  show ("Scheme reads what C stored", scm_c_eval_string ("v"));
  scm_c_eval_string ("(set! v 7)");
  scm_hashq_ref (table, scm_from_int (1), SCM_BOOL_F);
  show ("C reads what Scheme stored", SCM_VARIABLE_REF (var));
  return NULL;
}

int
main (int argc, char **argv)
{
  scm_with_guile (guile_main, NULL);
  return 0;
}
