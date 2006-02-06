
open XNum;
open Types;
open Runtime;
open Unicode.Types;

type scope = VMPrivate.Scope.scope;

value make_scope             : unit -> scope;
value bind_primitive         : scope -> string -> partial_value -> unit;
value bind_bin_op_l          : scope -> string -> int -> partial_value -> unit;
value bind_bin_op_n          : scope -> string -> int -> partial_value -> unit;
value bind_bin_op_r          : scope -> string -> int -> partial_value -> unit;
value bind_pre_op            : scope -> string -> partial_value -> unit;
value bind_post_op           : scope -> string -> partial_value -> unit;

value string_to_symbol       : uc_string -> Unicode.SymbolTable.symbol;
value symbol_to_string       : Unicode.SymbolTable.symbol -> uc_string;

value execute_declarations   : scope -> UCStream.istream -> unit;
value evaluate_expression    : scope -> UCStream.istream -> partial_value;
value evaluate_string_expr   : string -> scope -> UCStream.istream -> uc_list;
value evaluate_monad_expr    : scope -> UCStream.istream -> partial_value -> partial_value;

value evaluate               : unknown -> unit;
value evaluate_unknown       : unknown -> unit;
value evaluate_lin_form      : unknown -> LinForm.lin_form unknown -> unit;
value unify                  : ref bool -> unknown -> unknown -> unit;
value continue               : (unit -> unit) -> unit;
value continue2              : (unit -> unit) -> (unit -> unit) -> unit;
value continue3              : (unit -> unit) -> (unit -> unit) -> (unit -> unit) -> unit;
value continue4              : (unit -> unit) -> (unit -> unit) -> (unit -> unit) -> (unit -> unit) -> unit;
value set_unknown            : unknown -> partial_value -> unit;

value uc_string_to_char_list : uc_string -> partial_value;
value uc_list_to_char_list   : uc_list -> partial_value;
value ascii_to_char_list     : string -> partial_value;
value evaluate_num           : string -> ref num -> unknown -> unit;
value decode_list            : string -> unknown -> list unknown;
value decode_string          : string -> unknown -> uc_list;
value decode_function        : string -> unknown -> list unknown -> unknown;
value decode_num             : string -> unknown -> num;

