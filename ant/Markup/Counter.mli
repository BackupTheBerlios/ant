
open Runtime;
open Unicode.Types;

(* counters *)

type counter_table = 'a;

value empty_table : counter_table;
value new_counter : UCStream.location -> counter_table -> uc_string -> int -> option uc_string -> counter_table;
value get_counter : UCStream.location -> counter_table -> uc_string -> int;
value set_counter : UCStream.location -> counter_table -> uc_string -> int -> counter_table;

(* format functions *)

value int_to_arabic     : int -> uc_list;
value int_to_roman      : int -> uc_list;
value int_to_ROMAN      : int -> uc_list;
value int_to_alphabetic : int -> uc_list;
value int_to_ALPHABETIC : int -> uc_list;
value int_to_strings    : int -> list uc_list -> uc_list;

