/* Lists and symbols as LilyPond's lexer and parser use them: split a
   string, join lists destructively, reverse, check pairs inline, object
   properties.  Compared with real Guile's output (run.sh).  */

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

static SCM
symbols (SCM strings)
{
  SCM out = SCM_EOL;
  for (; scm_is_pair (strings); strings = SCM_CDR (strings))
    out = scm_cons (scm_string_to_symbol (SCM_CAR (strings)), out);
  return scm_reverse_x (out, SCM_EOL);
}

static void *
guile_main (void *data)
{
  SCM path = scm_string_split (scm_from_utf8_string ("Staff.NoteCollision.merge-dotted"),
                               SCM_MAKE_CHAR ('.'));
  show ("split", path);
  SCM syms = symbols (path);
  show ("symbols", syms);
  show ("pair?", scm_from_bool (scm_is_pair (syms) && scm_is_pair (scm_cdr (syms))
                                && scm_is_pair (scm_cddr (syms))
                                && scm_is_null (SCM_CDDDR (syms))));
  SCM joined = scm_append_x (scm_list_2 (scm_list_2 (scm_from_int (1), scm_from_int (2)),
                                         scm_list_2 (scm_from_int (3), scm_from_int (4))));
  show ("append!", joined);
  show ("append", scm_append (scm_list_3 (scm_list_1 (scm_from_int (1)), SCM_EOL,
                                         scm_list_2 (scm_from_int (2), scm_from_int (3)))));
  show ("reverse", scm_reverse (joined));
  show ("reverse!", scm_reverse_x (scm_list_3 (scm_from_int (1), scm_from_int (2), scm_from_int (3)),
                                   scm_list_1 (scm_from_int (0))));
  SCM sym = scm_from_utf8_symbol ("Staff");
  scm_set_object_property_x (sym, scm_from_utf8_symbol ("is-grob?"), SCM_BOOL_T);
  show ("object-property", scm_object_property (sym, scm_from_utf8_symbol ("is-grob?")));
  show ("object-property-missing", scm_object_property (scm_from_utf8_symbol ("Other"),
                                                       scm_from_utf8_symbol ("is-grob?")));
  show ("cons2", scm_cons2 (scm_from_int (1), scm_from_int (2), scm_from_int (3)));
  show ("memq", scm_c_memq (sym, scm_list_2 (scm_from_int (7), sym)));
  show ("ilength", scm_from_long (scm_ilength (syms)));
  show ("difference", scm_difference (scm_from_int (10), SCM_UNDEFINED));
  show ("divide", scm_divide (scm_from_int (6), scm_from_int (4)));
  show ("max", scm_max (scm_from_int (2), scm_from_double (3.5)));
  return NULL;
}

int
main (int argc, char **argv)
{
  scm_with_guile (guile_main, NULL);
  return 0;
}
