
open XNum;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

exception Syntax_error of UCStream.location and uc_string;
exception Runtime_error of uc_string;

value runtime_error : string -> 'a;

(*
type compare = [ Equal | Less | LessEqual | Greater | GreaterEqual | Inequal ];

type relation = unit; (* FIX *)
*)

type unknown = ref partial_value

and partial_value =
[ Unbound
| Constraint of list unknown
| Bool of bool
| Number of num
| Char of uc_char
| Symbol of symbol
| LinForm of LinForm.lin_form unknown
| UnevalT of environment and term           (* unevaluated term      *)
| Primitive1 of unknown -> unknown -> unit
| Primitive2 of unknown -> unknown -> unknown -> unit
| PrimitiveN of int and (unknown -> list unknown -> unit)
| SimpleFunction of int and environment and term
                                            (* arity, environment, and body           *)
| PatternFunction of int and environment and int and int and
                     list (list pattern_check * option term * term)
                                            (* arity, stack_depth, num_vars, patterns *)
| Relation of int and list statement        (* aritiy, local variables, and equations *)
| Application of partial_value and list unknown
| Nil
| List of unknown and unknown
| Tuple of array unknown
| Dictionary of SymbolMap.t unknown
| Opaque of Opaque.opaque unknown
]
(*
and constraints =
{
  equal_to    : list unknown;            (* unknowns that are equal to this one *)
  equations   : list linear_constraint; (* equations containing the unknown    *)
  inequations : list linear_constraint; (* inequations containing it          *)
  relations   : list relation           (* relations containing it            *)
}
and linear_constraint = (compare * LinForm.lin_form unknown)
                (* b = a_0x_0 +...+ a_nx_n   =>   (Equal, b, [(a_0,x_0); ...; (a_n,x_n)]) *)
*)
and term =
[ TConstant of partial_value
| TGlobal of unknown
| TVariable of int and int
| TLinForm of LinForm.lin_form term
| TConsTuple of array term
| TConsList  of term and term
| TApplication of term and list term
| TDictionary of list (symbol * term)
| TSimpleFunction of int and term
| TPatternFunction of int and int and int and list (list pattern_check * option term * term)
| TIfThenElse of term and term and term
| TLocalScope of array term and term
| TSequence of array statement and term
| TMatch of term and int and int and list (list pattern_check * option term * term)
| TUnify of term and term
| TTrigger of statement
]
and statement =
[ SEquation of term and term
| SIfThen of term and statement
| SIfThenElse of term and statement and statement
| SRelation of list unknown
]
and pattern_check =
[ PCAnything
| PCVariable of int
| PCNumber of num
| PCChar of uc_char
| PCSymbol of symbol
| PCTuple of int
| PCNil
| PCConsList
| PCAssign of int
]
and environment = list (array unknown);

value identical         : unknown -> unknown -> bool;
value compare_unknowns  : unknown -> unknown -> LinForm.compare_result;

value create_unbound    : 'a -> unknown;
value create_unknown    : partial_value -> unknown;
value add_constraint    : unknown -> list unknown -> list unknown;
value merge_constraints : list unknown -> list unknown -> list unknown;

value type_name         : partial_value -> string;

