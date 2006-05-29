
open XNum;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)

exception Syntax_error of UCStream.location and uc_string;
exception Runtime_error of uc_string;

value runtime_error msg = raise (Runtime_error (Array.of_list (UString.of_ascii msg)));

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
| Chain of array unknown
| Relation of int and list statement        (* aritiy, local variables, and equations *)
| Application of partial_value and list unknown
| Nil
| List of unknown and unknown
| Tuple of array unknown
| Dictionary of SymbolMap.t unknown
| Opaque of Opaque.opaque unknown
]
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
| TDo of array term
| TMatch of term and int and int and list (list pattern_check * option term * term)
| TUnify of term and term
| TTrigger of statement
]
and statement =
[ SEquation of term and term
| SIfThen of term and statement
| SIfThenElse of term and statement and statement
| SRelation of list unknown
| SForce of array term
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

value create_unbound _ = ref Unbound;
value create_unknown v = ref v;

(* |identical <x> <y>| checks whether too unknowns are identical. Unknowns are considered identical
   if the constraint <x = y> exists. *)

value identical x y = do
{
  if x == y then
    True
  else match (!x, !y) with
  [ (Constraint c, Constraint d) -> c == d
  | _                            -> False
  ]
};

value compare_unknowns x y = match (!x, !y) with
[ (Constraint c, Constraint d) -> do
  {
    let a = List.hd c in
    let b = List.hd d in

    if a == b then
      LinForm.Eq
    else if a < b then
      LinForm.Lt
    else
      LinForm.Gt
  }
| _ -> do
  {
    if x == y then
      LinForm.Eq
    else if x < y then
      LinForm.Lt
    else
      LinForm.Gt
  }
];

value rec add_constraint x cs = match cs with
[ [] -> [x]
| [y::ys] -> match compare_unknowns x y with
  [ LinForm.Lt -> [x :: cs]
  | LinForm.Gt -> [y :: add_constraint x ys]
  | LinForm.Eq -> cs
  ]
];

value merge_constraints c0 c1 = do
{
  if c0 == c1 then
    c0
  else
    iter c0 c1

  where rec iter c0 c1 = match (c0, c1) with
  [ ([], _) -> c0
  | (_, []) -> c1
  | ([x::xs], [y::ys]) -> match compare_unknowns x y with
    [ LinForm.Lt -> [x :: iter xs c1]
    | LinForm.Gt -> [y :: iter c0 ys]
    | LinForm.Eq -> [x :: iter xs ys]
    ]
  ]
};

value type_name x = match x with
[ Unbound       -> "<unbound>"
| Constraint _  -> "<constraint>"
| Bool _        -> "bool"
| Number _      -> "number"
| Char _        -> "character"
| Symbol _      -> "symbol"
| LinForm _     -> "linear form"
| UnevalT _ _   -> "<unevaluated>"
| Primitive1 _  -> "function"
| Primitive2 _  -> "function"
| PrimitiveN _ _            -> "function"
| SimpleFunction _ _ _      -> "function"
| PatternFunction _ _ _ _ _ -> "function"
| Chain _                   -> "function"
| Relation _ _              -> "relation"
| Application _ _           -> "<application>"
| Nil           -> "nil"
| List _ _      -> "list"
| Tuple _       -> "tuple"
| Dictionary _  -> "dictionary"
| Opaque y      -> Opaque.type_name y
];

