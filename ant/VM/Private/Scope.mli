
open Unicode.Types;
open Unicode.SymbolTable;
open Types;

type scope = 'a;

value create        : unit -> scope;
value copy          : scope -> scope;
value symbol_table  : scope -> Hashtbl.t uc_string Lexer.token_class;
value add_bin_op    : scope -> int -> Lexer.assoc -> symbol -> unit;
value add_pre_op    : scope -> symbol -> unit;
value add_post_op   : scope -> symbol -> unit;
value add_global    : scope -> symbol -> partial_value -> unit;
value push          : scope -> list symbol -> (scope * int);
value lookup_local  : scope -> symbol -> (int * int);
value lookup_global : scope -> symbol -> unknown;
value lookup        : scope -> symbol -> term;

