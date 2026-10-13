/* libguile's exported data, with the sizes GNU Guile 3.0.11 gives it
   (tests/libguile/probe.c, `nm -D -S`): programs linked against Guile
   may have copied it into themselves, so the sizes are part of the ABI.
   The Lisp side fills in the values when it starts (bridge.lisp,
   INITIALIZE-DATA).  */

#include <stdint.h>

/* one word each: SCM values, smob tags, counters */
uintptr_t scm_after_gc_hook = 0;
uintptr_t scm_applicable_struct_vtable_vtable = 0;
uintptr_t scm_applicable_struct_with_setter_vtable_vtable = 0;
uintptr_t scm_arg_type_key = 0;
uintptr_t scm_args_number_key = 0;
uintptr_t scm_char_set_ascii = 0;
uintptr_t scm_char_set_blank = 0;
uintptr_t scm_char_set_digit = 0;
uintptr_t scm_char_set_empty = 0;
uintptr_t scm_char_set_full = 0;
uintptr_t scm_char_set_graphic = 0;
uintptr_t scm_char_set_hex_digit = 0;
uintptr_t scm_char_set_iso_control = 0;
uintptr_t scm_char_set_letter = 0;
uintptr_t scm_char_set_letter_and_digit = 0;
uintptr_t scm_char_set_lower_case = 0;
uintptr_t scm_char_set_printing = 0;
uintptr_t scm_char_set_punctuation = 0;
uintptr_t scm_char_set_symbol = 0;
uintptr_t scm_char_set_title_case = 0;
uintptr_t scm_char_set_upper_case = 0;
uintptr_t scm_char_set_whitespace = 0;
uintptr_t scm_endianness_big = 0;
uintptr_t scm_endianness_little = 0;
uintptr_t scm_file_port_type = 0;
uintptr_t scm_global_locale = 0;
uintptr_t scm_misc_error_key = 0;
uintptr_t scm_module_goops = 0;
uintptr_t scm_module_tag = 0;
uintptr_t scm_num_overflow_key = 0;
uintptr_t scm_out_of_range_key = 0;
uintptr_t scm_print_state_vtable = 0;
uintptr_t scm_stack_type = 0;
uintptr_t scm_standard_vtable_vtable = 0;
uintptr_t scm_string_port_type = 0;
uintptr_t scm_sym_apply = 0;
uintptr_t scm_sym_args = 0;
uintptr_t scm_sym_arrow = 0;
uintptr_t scm_sym_at = 0;
uintptr_t scm_sym_atat = 0;
uintptr_t scm_sym_begin = 0;
uintptr_t scm_sym_case = 0;
uintptr_t scm_sym_column = 0;
uintptr_t scm_sym_cond = 0;
uintptr_t scm_sym_copy = 0;
uintptr_t scm_sym_define = 0;
uintptr_t scm_sym_dot = 0;
uintptr_t scm_sym_else = 0;
uintptr_t scm_sym_eval_when = 0;
uintptr_t scm_sym_filename = 0;
uintptr_t scm_sym_if = 0;
uintptr_t scm_sym_lambda = 0;
uintptr_t scm_sym_let = 0;
uintptr_t scm_sym_letrec = 0;
uintptr_t scm_sym_letstar = 0;
uintptr_t scm_sym_line = 0;
uintptr_t scm_sym_name = 0;
uintptr_t scm_sym_quasiquote = 0;
uintptr_t scm_sym_quote = 0;
uintptr_t scm_sym_set_x = 0;
uintptr_t scm_sym_system_procedure = 0;
uintptr_t scm_sym_unquote = 0;
uintptr_t scm_sym_uq_splicing = 0;
uintptr_t scm_system_error_key = 0;
uintptr_t scm_tc16_condvar = 0;
uintptr_t scm_tc16_dir = 0;
uintptr_t scm_tc16_hook = 0;
uintptr_t scm_tc16_malloc = 0;
uintptr_t scm_tc16_mutex = 0;
uintptr_t scm_tc16_port_with_ps = 0;
uintptr_t scm_tc16_promise = 0;
uintptr_t scm_tc16_regex = 0;
uintptr_t scm_tc16_rstate = 0;
uintptr_t scm_tc16_thread = 0;
uintptr_t scm_usage_name = 0;
uintptr_t scm_var_random_state = 0;
uintptr_t scm_gc_ports_collected = 0;
uintptr_t scm_numsmob = 0;
uintptr_t scm_c_time_units_per_second = 0;

/* ints */
int scm_initialized_p = 0;
int scm_install_gmp_memory_functions = 0;
int scm_module_system_booted_p = 0;
int scm_stack_checking_enabled_p = 0;
int scm_tc16_charset = 0;

/* scm_t_c_hook: three words */
uintptr_t scm_after_gc_c_hook[3], scm_after_sweep_c_hook[3], scm_before_gc_c_hook[3],
  scm_before_mark_c_hook[3], scm_before_sweep_c_hook[3];

/* the smob descriptors: 256 of 64 bytes (abi.lisp) */
uintptr_t scm_smobs[256 * 8];

/* scm_masktab: 256 bytes; scm_the_rng: 48 */
unsigned char scm_masktab[256];
uintptr_t scm_the_rng[6];
