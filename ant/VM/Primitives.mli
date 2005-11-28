
open Unicode.Types;
open Types;

value uc_string_to_char_list : uc_string -> partial_value;
value uc_list_to_char_list   : uc_list -> partial_value;
value ascii_to_char_list     : string -> partial_value;
value evaluate_char_list     : string -> unknown -> uc_list;

value bind_primitive : Scope.scope -> string -> partial_value -> unit;
value initial_scope  : unit -> Scope.scope;

