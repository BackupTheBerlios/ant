
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module Environment:
sig
  type environment = list (array unknown);
end;

value add_unknowns      : unknown -> unknown -> unknown -> unit;
value sub_unknowns      : unknown -> unknown -> unknown -> unit;
value mul_unknowns      : unknown -> unknown -> unknown -> unit;
value div_unknowns      : unknown -> unknown -> unknown -> unit;

value evaluate_list     : string -> ref (list unknown) -> unknown -> unit;

value check_patterns    : ref bool -> list pattern_check -> unknown -> array unknown -> array unknown -> unit;

value evaluate_unknown     : unknown -> unit;
value evaluate_lin_form    : unknown -> LinForm.lin_form unknown -> unit;
value evaluate_application : unknown -> partial_value -> list unknown -> unit;
value unify                : ref bool -> unknown -> unknown -> unit;
value forced_unify         : unknown -> unknown -> unit;

