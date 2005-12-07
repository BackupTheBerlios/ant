
open Unicode.Types;
open Unicode.SymbolTable;
open Types;

type scope = 'a;

value create        : unit -> scope;
value symbol_table  : scope -> Hashtbl.t uc_string Lexer.token_class;
value copy          : scope -> scope;
value add_global    : scope -> symbol -> partial_value -> unit;
value push          : scope -> list symbol -> (scope * int);
value lookup_local  : scope -> symbol -> (int * int);
value lookup_global : scope -> symbol -> unknown;
value lookup        : scope -> symbol -> term;

