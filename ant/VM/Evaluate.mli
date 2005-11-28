
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module Environment:
sig
  type environment = list (array unknown);
end;

value add_unknowns      : unknown -> unknown -> partial_value;
value sub_unknowns      : unknown -> unknown -> partial_value;
value mul_unknowns      : unknown -> unknown -> partial_value;
value div_unknowns      : unknown -> unknown -> partial_value;

value evaluate_list     : string -> unknown -> list unknown;

value check_patterns    : list pattern_check -> unknown -> array unknown -> array unknown -> bool;

value evaluate          : unknown -> unit;
value evaluate_lin_form : unknown -> LinForm.lin_form unknown -> unit;
value unify             : unknown -> unknown -> bool;
value forced_unify      : unknown -> unknown -> unit;

