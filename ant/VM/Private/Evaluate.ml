
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;
open CStack;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)

module Environment =
struct

type environment = list (array unknown);

value empty () = [];

value push env arr = [arr :: env];

value push_unbound env n = [Array.init n create_unbound :: env];

value lookup env lvl idx = do
{
  iter lvl env

  where rec iter lvl env = match env with
  [ []      -> assert False
  | [e::es] -> if lvl <= 0 then
                 e.(idx)
               else
                 iter (lvl-1) es
  ]
};

end;

value rec binom n k = do
{
  iter (if 2 * k > n then n-k else k) n num_one

  where rec iter k n x = do
  {
    if k <= 0 then
      x
    else
      iter (k-1) (n-1) (num_of_ints n k */ x)
  }
};

(* defer evaluation of <term> but check first whether it is already evaluated *)

value constant env term = match term with
[ TConstant _       -> term
| TGlobal _         -> term
| TVariable lvl idx -> TGlobal (Environment.lookup env lvl idx)
| _                 -> TConstant (UnevalT env term)
];

value unevaluated env term = match term with
[ TConstant x       -> create_unknown x
| TGlobal x         -> x
| TVariable lvl idx -> Environment.lookup env lvl idx
| _                 -> create_unknown (UnevalT env term)
];

value make_tuple elements = match elements with
[ []  -> assert False
| [x] -> x
| _   -> TConsTuple (Array.of_list elements)
];

value env_push_terms env terms = do
{
  (* Since the terms are evaluated w.r.t. to the new environment we have to create
     a cyclic structure. *)

  let vars    = Array.make (Array.length terms) (create_unbound ()) in
                (* We can use the same unknown here since the entries will be replaced below. *)
  let new_env = [vars :: env] in

  Array.iteri
    (fun i _ -> vars.(i) := unevaluated new_env terms.(i))
    vars;

  new_env
};

(* |evaluate_unkown <x>| performs one evaluation step for the given unknown. *)

value rec evaluate_unknown x = match !x with
[ UnevalT env term -> evaluate_term x env term
| _                -> ()
]

and evaluate_term x env term = do
{
  (*
    We know that |!x = UnevalT env term|. To detect loops and to allow unification we set it to
    |Unbound|.
  *)

  !x := Unbound;

  match term with
  [ TConstant v -> !x := v
  | TGlobal y   -> do
    {
      cont2
        (fun () -> evaluate_unknown y)
        (fun () -> bind_unknown x y);
    }
  | TVariable lvl idx -> do
    {
      let y = Environment.lookup env lvl idx in

      cont2
        (fun () -> evaluate_unknown y)
        (fun () -> bind_unknown x y);
    }
  | TLinForm lin -> do
    {
      let lf = LinForm.map
                 compare_unknowns
                 (fun t -> do
                   {
                     let y = ref Unbound in

                     cont (fun () -> evaluate_term y env t);

                     y
                   })
                 lin
      in

      !x := LinForm lf
    }
  | TConsTuple xs -> do
    {
      !x := Tuple (Array.init
                    (Array.length xs)
                    (fun i -> unevaluated env xs.(i)))
    }
  | TConsList y z              -> !x := List (unevaluated env y) (unevaluated env z)
  | TSimpleFunction arity term -> !x := SimpleFunction arity env term
  | TPatternFunction a s n p   -> !x := PatternFunction a env s n p
  | TDictionary dict           -> do
    {
      !x := Dictionary
              (List.fold_left
                (fun m (s,t) -> SymbolMap.add s (unevaluated env t) m)
                SymbolMap.empty
                dict)
    }
  | TApplication f args -> do
    {
      let f1 = ref Unbound in

      cont2
        (fun () -> evaluate_term f1 env f)
        (fun () -> evaluate_application x !f1 (List.map (unevaluated env) args));
    }
  | TIfThenElse p e0 e1 -> do
    {
      let y = ref Unbound in

      cont2
        (fun () -> evaluate_term y env p)
        (fun () -> match !y with
                   [ Bool True    -> evaluate_term x env e0
                   | Bool False   -> evaluate_term x env e1
                   | Unbound
                   | Constraint _ -> runtime_error "unknown condition"
                   | _            -> runtime_error "type error: boolean expected"
                   ]);
    }
  | TLocalScope defs term -> do
    {
      let new_env = env_push_terms env defs in

      evaluate_term x new_env term
    }
  | TSequence stmts term -> do
    {
      let len = Array.length stmts in

      !x := UnevalT env term;

      cont (fun () -> evaluate_unknown x);

      for i = 1 to len do
      {
        cont (fun () -> execute env stmts.(len - i))
      }
    }
  | TMatch term stack_depth num_vars patterns -> do
    {
      let stack = Array.init stack_depth create_unbound in
      let vars  = Array.init num_vars    create_unbound in
      let expr  = ref Unbound in

      cont2
        (fun () -> evaluate_term expr env term)
        (fun () -> iter patterns)

      where rec iter patterns = match patterns with
      [ []                -> runtime_error "matching error"
      | [(c, g, b) :: ps] -> do
        {
          let r = ref False in

          cont2
            (fun () -> check_patterns r c expr stack vars)
            (fun () -> do
              {
                if !r then do
                {
                  let new_env = Environment.push env vars in

                  match g with
                  [ None       -> evaluate_term x new_env b
                  | Some guard -> do
                    {
                      let z = ref Unbound in

                      cont
                        (fun () -> match !z with
                                   [ Bool True    -> evaluate_term x new_env b
                                   | Bool False   -> iter ps
                                   | Unbound
                                   | Constraint _ -> runtime_error "guard unknown"
                                   | _            -> runtime_error "illegal guard"
                                   ]);
                      evaluate_term z new_env guard
                    }
                  ]
                }
                else
                  iter ps
              })
        }
      ]
    }
  | TUnify t1 t2 -> do
    {
      let u1 = unevaluated env t1 in
      let u2 = unevaluated env t2 in

      cont2
        (fun () -> forced_unify u1 u2)
        (fun () -> bind_unknown x u2)
    }
  | TTrigger stmt -> do
    {
      cont (fun () -> execute env stmt)
    }
  ]
}

and check_patterns res checks expr stack vars = do
{
  iter checks expr 0

  where rec iter checks expr used_stack = do
  {
    let continue cs = match cs with
    [ [] -> !res := True
    | _  -> iter cs (stack.(used_stack-1)) (used_stack-1)
    ]
    in

    match checks with
    [ []      -> !res := True
    | [c::cs] -> match c with
      [ PCAnything   -> continue cs
      | PCVariable i -> do
        {
          vars.(i) := expr;
          continue cs
        }
      | PCAssign i -> do
        {
          vars.(i) := expr;
          iter cs expr used_stack
        }
      | PCNumber n -> match !expr with
        [ Number i    -> if n <>/ i then
                           !res := False
                         else
                           continue cs
        | UnevalT e t -> do
          {
            cont (fun () -> iter checks expr used_stack);

            evaluate_term expr e t
          }
        | _ -> !res := False
        ]
      | PCChar c -> match !expr with
        [ Char d      -> if c <> d then
                           !res := False
                         else
                           continue cs
        | UnevalT e t -> do
          {
            cont (fun () -> iter checks expr used_stack);

            evaluate_term expr e t
          }
        | _ -> !res := False
        ]
      | PCSymbol sym -> match !expr with
        [ Symbol s    -> if sym <> s then
                           !res := False
                         else
                           continue cs
        | UnevalT e t -> do
          {
            cont (fun () -> iter checks expr used_stack);

            evaluate_term expr e t;
          }
        | _ -> !res := False
        ]
      | PCTuple arity -> match !expr with
        [ Tuple xs -> do
          {
            if arity = Array.length xs then do
            {
              for i = 0 to Array.length xs - 2 do
              {
                stack.(used_stack + i) := xs.(Array.length xs - i - 1);
              };

              iter cs xs.(0) (used_stack + Array.length xs - 1)
            }
            else
              !res := False
          }
        | UnevalT e t -> do
          {
            cont (fun () -> iter checks expr used_stack);

            evaluate_term expr e t
          }
        | _ -> !res := False
        ]
      | PCNil -> match !expr with
        [ Nil         -> continue cs
        | UnevalT e t -> do
          {
            cont (fun () -> iter checks expr used_stack);

            evaluate_term expr e t
          }
        | _ -> !res := False
        ]
      | PCConsList -> match !expr with
        [ List x y -> do
          {
            stack.(used_stack) := y;
            iter cs x (used_stack + 1)
          }
        | UnevalT e t -> do
          {
            cont (fun () -> iter checks expr used_stack);

            evaluate_term expr e t
          }
        | _ -> !res := False
        ]
      ]
    ]
  }
}

(* evaluation of builtin operations *)

and add_unknowns res x y = match (!x, !y) with
[ (Number m,  Number n)  -> !res := Number (m +/ n)
| (LinForm m, Number n)  -> !res := LinForm (LinForm.add_const m n)
| (Number m,  LinForm n) -> !res := LinForm (LinForm.add_const n m)
| (LinForm m, LinForm n) -> !res := LinForm (LinForm.add m n)
| (List _ _, Nil)        -> !res := !x
| (Nil, List _ _)        -> !res := !y
| (List a b, List _ _)   -> do
  {
    let c = ref Unbound in

    !res := List a c;

    add_unknowns c b y
  }
| (Tuple xs, Tuple ys)   -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "+: tuples differ in length"
    else do
    {
      let len = Array.length xs               in
      let zs  = Array.init len create_unbound in

      !res := Tuple zs;

      for i = 1 to len do
      {
        cont (fun () -> add_unknowns zs.(len - i) xs.(len - i) ys.(len - i))
      }
    }
  }
| (LinForm lin, Tuple xs)
| (Tuple xs, LinForm lin) -> do
  {
    let dim = Array.length xs in

    if lin.LinForm.const <>/ num_zero then
      runtime_error "+: invalid arguments"
    else
      iter
        (Array.init
          dim
          (fun i -> LinForm.of_unknown compare_unknowns xs.(i)))
        lin.LinForm.terms

    where rec iter result terms = match terms with
    [ []            -> !res := Tuple (Array.map (fun l -> ref (LinForm l)) result)
    | [(a,y) :: ys] -> do
      {
        let z = Array.init dim create_unbound in

        cont2
          (fun () -> forced_unify y (ref (Tuple z)))
          (fun () -> do
            {
              let u = Array.make dim (LinForm.lin_zero compare_unknowns) in

              cont (fun () -> iter u ys);

              for i = 1 to dim do
              {
                cont
                  (fun () ->
                    u.(dim - i) := LinForm.add_unknown result.(dim - i) a z.(dim - i))
              }
            })
      }
    ]
  }
| (UnevalT e t, _)  -> do
  {
    cont2
      (fun () -> evaluate_term x e t)
      (fun () -> add_unknowns res x y);
  }
| (_, UnevalT e t)  -> do
  {
    cont2
      (fun () -> evaluate_term y e t)
      (fun () -> add_unknowns res x y);
  }
| (Unbound, _)      -> !res := LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                                    (LinForm.of_unknown compare_unknowns y))
| (_, Unbound)      -> !res := LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                                    (LinForm.of_unknown compare_unknowns y))
| (Constraint _, _) -> !res := LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                                    (LinForm.of_unknown compare_unknowns y))
| (_, Constraint _) -> !res := LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                                    (LinForm.of_unknown compare_unknowns y))
| (Number n, _) when n =/ num_zero -> !res := !y    (* Allow addition of 0 to everything so we *)
| (_, Number n) when n =/ num_zero -> !res := !x    (* do not need two versions of lin_form.   *)
| _                 -> runtime_error "+: invalid argument"
]

and sub_unknowns res x y = match (!x, !y) with
[ (Number m,  Number n)  -> !res := Number (m -/ n)
| (LinForm m, Number n)  -> !res := LinForm (LinForm.add_const m (minus_num n))
| (Number m,  LinForm n) -> !res := LinForm (LinForm.sub (LinForm.of_num compare_unknowns m) n )
| (LinForm m, LinForm n) -> !res := LinForm (LinForm.sub m n)
| (Tuple xs,  Tuple ys)  -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "-: I cannot subtract tuples of different length"
    else do
    {
      let len = Array.length xs               in
      let zs  = Array.init len create_unbound in

      !res := Tuple zs;

      for i = 1 to len do
      {
        cont (fun () -> sub_unknowns zs.(len - i) xs.(len - i) ys.(len - i))
      }
    }
  }
| (UnevalT e t, _)  -> do
  {
    cont2
      (fun () -> evaluate_term x e t)
      (fun () -> sub_unknowns res x y)
  }
| (_, UnevalT e t)  -> do
  {
    cont2
      (fun () -> evaluate_term y e t)
      (fun () -> sub_unknowns res x y)
  }
| (Unbound, _)           -> !res := LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                                         (LinForm.of_scaled_unknown compare_unknowns num_minus_one y))
| (_, Unbound)           -> !res := LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x) 
                                                         (LinForm.of_scaled_unknown compare_unknowns num_minus_one y))
| _                      -> runtime_error "-: invalid argument"
]

and mul_unknowns res x y = match (!x, !y) with
[ (Number m, Number n)   -> !res := Number (m */ n)
| (Number m, LinForm l)  -> !res := LinForm (LinForm.scale m l)
| (LinForm l, Number n)  -> !res := LinForm (LinForm.scale n l)
| (LinForm m, LinForm n) -> do
  {
    cont3
      (fun () -> evaluate_lin_form x m)
      (fun () -> evaluate_lin_form y n)
      (fun () -> match (!x, !y) with
                 [ (LinForm _, LinForm _) -> runtime_error "*: non-linear equation"
                 | _                      -> mul_unknowns res x y
                 ])
  }
| (Number m,     Unbound)
| (Number m,     Constraint _) -> !res := LinForm (LinForm.of_scaled_unknown compare_unknowns m y)
| (Unbound,      Number n)
| (Constraint _, Number n)     -> !res := LinForm (LinForm.of_scaled_unknown compare_unknowns n x)
| (Number _,     Tuple ys)
| (Unbound,      Tuple ys)
| (Constraint _, Tuple ys) -> do
  {
    let len = Array.length ys               in
    let zs  = Array.init len create_unbound in

    !res := Tuple zs;

    for i = 1 to len  do
    {
      cont (fun () -> mul_unknowns zs.(len - i) x ys.(len - i))
    }
  }
| (Tuple xs, Number _)
| (Tuple xs, Unbound)
| (Tuple xs, Constraint _) -> do
  {
    let len = Array.length xs               in
    let zs  = Array.init len create_unbound in

    !res := Tuple zs;

    for i = 1 to len  do
    {
      cont (fun () -> mul_unknowns zs.(len - i) xs.(len - i) y)
    }
  }
| (Tuple xs, Tuple ys) -> do
  {
    let len = Array.length xs in

    if Array.length ys <> len then
      runtime_error "*: tuples differ in length"
    else do
    {
      let zs = Array.init len create_unbound in

      cont
        (fun () -> do
          {
            iter 0 (LinForm.lin_zero compare_unknowns)

            where rec iter i lin = do
            {
              if i >= len then
                !res := LinForm lin
              else
                iter (i+1)
                     (LinForm.add lin (LinForm.of_unknown compare_unknowns zs.(i)))
            }
          });

      for i = 1 to len do
      {
        cont (fun () -> mul_unknowns zs.(len - i) xs.(len - i) ys.(len - i))
      }
    }
  }
| (Number _, Primitive1 f) -> do
  {
    !res := Primitive1
              (fun z a -> do
                {
                  let y = ref Unbound in
                  cont2
                    (fun () -> f y a)
                    (fun () -> mul_unknowns z x y)
                })
  }
| (Number _, Primitive2 f) -> do
  {
    !res := Primitive2
              (fun z a b -> do
                {
                  let y = ref Unbound in
                  cont2
                    (fun () -> f y a b)
                    (fun () -> mul_unknowns z x y)
                })
  }
| (Number _, PrimitiveN ar f) -> do
  {
    !res := PrimitiveN ar
              (fun z a -> do
                {
                  let y = ref Unbound in
                  cont2
                    (fun () -> f y a)
                    (fun () -> mul_unknowns z x y)
                })
  }
| (Number _, SimpleFunction ar env body) -> do
  {
    !res := SimpleFunction ar env
              (TApplication (TConstant (Primitive2 mul_unknowns))
                            [TConstant !x; body])
  }
| (Number _, PatternFunction a b c d body) -> do
  {
    !res := PatternFunction a b c d
              (List.map
                (fun (pc, g, t) ->
                  (pc, g, (TApplication (TConstant (Primitive2 mul_unknowns))
                                        [TConstant !x; t])))
              body)
  }
| (_, List _ _) -> do   (*  x * [y,z]  =>  y + x*(z-y) *)
  {
    let points = ref [] in

    cont2
      (fun () -> evaluate_list "*" points y)
      (fun () -> match !points with
                 [ [a]    -> bind_unknown res a
                 | [a; b] -> do  (* treat the common case separately *)
                   {
                     let c = ref Unbound in
                     let d = ref Unbound in
 
                     cont3
                       (fun () -> sub_unknowns c b a)
                       (fun () -> mul_unknowns d x c)
                       (fun () -> add_unknowns res a d)
                   }
                 | [a :: b] -> do
                   {
                     cont2
                       (fun () -> evaluate_unknown x)
                       (fun () -> match !x with
                                  [ Number t -> do
                                    {
                                      let n = List.length !points - 1 in
                                      let s = num_one -/ t            in

                                      let (_, lin) =
                                        List.fold_left
                                          (fun (k, lin) c ->
                                            (k + 1,
                                             LinForm.add lin
                                               (LinForm.of_scaled_unknown
                                                  compare_unknowns
                                                 (binom n k
                                                   */ power_num s (num_of_int (n-k))
                                                   */ power_num t (num_of_int k))
                                                 c)
                                            ))
                                          (1, LinForm.of_scaled_unknown
                                                compare_unknowns
                                                (power_num s (num_of_int n))
                                                a)
                                          b
                                      in

                                      !res := LinForm lin
                                    }
                                  | _ -> runtime_error ("*: invalid argument, expected number but got " ^ type_name !x)
                                  ])
                   }
                 | [] -> assert False
                 ])
  }
| (UnevalT e t, _)  -> do
  {
    cont2
      (fun () -> evaluate_term x e t)
      (fun () -> mul_unknowns res x y)
  }
| (_, UnevalT e t)  -> do
  {
    cont2
      (fun () -> evaluate_term y e t)
      (fun () -> mul_unknowns res x y)
  }
| _ -> runtime_error ("*: invalid argument of type " ^ type_name !x ^ " and " ^ type_name !y)
]

and div_unknowns res x y = match (!x, !y) with
[ (Number m,  Number n) -> !res := Number (m // n)
| (LinForm l, Number n) -> !res := LinForm (LinForm.scale (num_one // n) l)
| (Unbound,   Number n) -> !res := LinForm (LinForm.of_scaled_unknown compare_unknowns (num_one // n) x)
| (Tuple xs,  Number _) -> do
  {
    let len = Array.length xs               in
    let zs  = Array.init len create_unbound in

    !res := Tuple zs;

    for i = 1 to len do
    {
      cont (fun () -> div_unknowns zs.(len - i) xs.(len - i) y)
    }
  }
| (UnevalT e t, _)  -> do
  {
    cont2
      (fun () -> evaluate_term x e t)
      (fun () -> div_unknowns res x y)
  }
| (_, UnevalT e t)  -> do
  {
    cont2
      (fun () -> evaluate_term y e t)
      (fun () -> div_unknowns res x y)
  }
| (_, LinForm l)   -> do
  {
    cont2
      (fun () -> evaluate_lin_form y l)
      (fun () -> match !y with
                 [ Number _ -> div_unknowns res x y
                 | _        -> runtime_error ("/: invalid argument")
                 ])
  }
| _ -> runtime_error "/: invalid argument"
]

and evaluate_list name res x = match !x with
[ Nil      -> !res := []
| List a b -> do
  {
    let z = ref [] in
    cont2
      (fun () -> evaluate_list name z b)
      (fun () -> !res := [a :: !z])
  }
| UnevalT e t -> do
  {
    cont2
      (fun () -> evaluate_term x e t)
      (fun () -> evaluate_list name res x)
  }
| Unbound
| Constraint _ -> runtime_error (name ^ ": argument undefined")
| _            -> runtime_error (name ^ ": invalid argument")
]

and evaluate_lin_form x lin = do
{
  (* We check for unbound, constraint, and other unkowns. *)

  !x := Unbound;

  let rec collect res terms = match terms with
  [ [] -> ()
  | [(a, y) :: ts] -> do
    {
      cont2
        (fun () -> evaluate_unknown y)
        (fun () -> do
          {
            let z = ref !res in

            cont2
              (fun () -> collect z ts)
              (fun () -> do
                {
                  let (lin, coeff, const) = !z in

                  match !y with
                  [ Unbound -> do
                    {
                      if identical x y then
                        !res := (lin, coeff +/ a, const)
                      else
                        !res := (LinForm.add_unknown lin a y, coeff, const)
                    }
                  | Constraint _ -> do
                    {
                      !res := (LinForm.add_unknown lin a y, coeff, const)
                    }
                  | _ -> do
                    {
                      let z1 = ref Unbound in
                      let z2 = ref Unbound in

                      cont3
                        (fun () -> mul_unknowns z1 (ref (Number a)) y)
                        (fun () -> add_unknowns z2 const z1)
                        (fun () -> !res := (lin, coeff, z2))
                    }
                  ]
                })
          })
    }
  ]
  in
  let compute_x x coeff sum = do
  {
    if coeff =/ num_zero then
      !x := !sum
    else if coeff =/ num_one then do
    {
      let z = ref Unbound in

      cont2
        (fun () -> sub_unknowns z sum x)
        (fun () -> forced_unify z (ref (Number num_zero)))
    }
    else
      cont
        (fun () -> mul_unknowns x
                     (ref (Number (num_one // (num_one -/ coeff))))
                     sum)
  }
  in

  let col = ref (LinForm.lin_zero compare_unknowns,
                 num_zero,
                 ref (Number lin.LinForm.const))
            in
  let sum = ref Unbound in

  cont2
    (fun () -> collect col lin.LinForm.terms)
    (fun () -> do
      {
        let (lin, coeff, const) = !col in

        cont2
          (fun () ->
            if LinForm.is_constant lin then
              add_unknowns sum const (ref (Number lin.LinForm.const))
            else
              add_unknowns sum const (ref (LinForm lin)))
          (fun () -> compute_x x coeff sum)
      })
}

and evaluate_application x f args = match f with
[ Primitive1 p -> match args with
  [ [a]      -> p x a
  | [a :: b] -> do
    {
      let c = ref Unbound in

      cont2
        (fun () -> p c a)
        (fun () -> evaluate_application x !c b)
    }
  | _ -> assert False
  ]
| Primitive2 p -> match args with
  [ [a; b]      -> p x a b
  | [a; b :: c] -> do
    {
      let d = ref Unbound in

      cont2
        (fun () -> p d a b)
        (fun () -> evaluate_application x !d c)
    }
  | [_] -> !x := Application f args
  | _   -> assert False
  ]
| PrimitiveN arity p -> do
  {
    let n = List.length args in

    if arity > n then
      !x := Application f args
    else if arity = n then
      p x args
    else do
    {
      let (vars, rest) = XList.split_at arity args in
      let result       = ref Unbound in

      cont2
        (fun () -> p result vars)
        (fun () -> evaluate_application x !result rest)
    }
  }
| SimpleFunction arity e body -> do
  {
    let n = List.length args in

    if arity > n then
      !x := Application f args
    else if arity = n then
      evaluate_term x (Environment.push e (Array.of_list args)) body
    else do
    {
      let (vars, rest) = XList.split_at arity args in
      let new_env      = Environment.push e (Array.of_list vars) in

      let result = ref Unbound in

      cont2
        (fun () -> evaluate_term result new_env body)
        (fun () -> evaluate_application x !result rest)
    }
  }
| PatternFunction arity e stack_depth num_vars pats -> do
  {
    let n = List.length args in

    if arity > n then
      !x := Application f args
    else if arity = n then
      evaluate_term
        x e
        (TMatch
          (make_tuple (List.map (fun v -> TGlobal v) args))
          stack_depth
          num_vars
          pats)
    else do
    {
      let (used, rest) = XList.split_at arity args in

      let result = ref Unbound in

      cont2
        (fun () ->
          evaluate_term result e
            (TMatch
              (make_tuple (List.map (fun v -> TGlobal v) used))
              stack_depth
              num_vars
              pats))
        (fun () -> evaluate_application x !result rest)
    }
  }
| Application f2 args2 -> do
  {
    evaluate_application x f2 (args2 @ args)
  }

(* access methods *)

| Dictionary dict -> match args with
  [ [a :: b] -> match !a with
    [ Symbol s -> do
      {
        try do
        {
          let y = SymbolMap.find s dict in

          match b with
          [ [] -> !x := !y
          | _  -> do
            {
              cont2
                (fun () -> evaluate_unknown y)
                (fun () -> evaluate_application x !y b)
            }
          ]
        }
        with
        [ Not_found -> runtime_error "entry not found in dictionary" ]
      }
    | UnevalT env t -> do
      {
        cont2
          (fun () -> evaluate_term a env t)
          (fun () -> evaluate_application x f args)
      }
    | _ -> runtime_error "index not a symbol"
    ]
  | _ -> assert False
  ]
| Tuple ys -> match args with
  [ [a :: b] -> match !a with
    [ Number n -> do
      {
        if is_integer_num n then do
        {
          let i = int_of_num n in

          if i >= 0 && i < Array.length ys then match b with
          [ [] -> !x := !(ys.(i))
          | _  -> do
            {
              cont2
                (fun () -> evaluate_unknown ys.(i))
                (fun () -> evaluate_application x !(ys.(i)) b)
            }
          ]
          else
            runtime_error "index out of range"
        }
        else
          runtime_error "non-integral index"
      }
    | UnevalT env t -> do
      {
        cont2
          (fun () -> evaluate_term a env t)
          (fun () -> evaluate_application x f args)
      }
    | _ -> runtime_error "non-integral index"
    ]
  | _ -> assert False
  ]
| List _ _ -> match args with
  [ [a :: b] -> match !a with
    [ Number n -> do
      {
        if is_integer_num n then do
        {
          iter (ref f) (int_of_num n)

          where rec iter lst i = match !lst with
          [ Nil      -> runtime_error "index out of range"
          | List y z -> do
            {
              if i = 0 then match b with
              [ [] -> !x := !y
              | _  -> evaluate_application x !y b
              ]
              else if i > 0 then
                iter z (i-1)
              else
                runtime_error "index out of range"
            }
          | UnevalT env t -> do
            {
              cont2
                (fun () -> evaluate_term lst env t)
                (fun () -> iter lst i)
            }
          | _ -> runtime_error "malformed list"
          ]
        }
        else
          runtime_error "non-integral index"
      }
    | UnevalT env t -> do
      {
        cont2
          (fun () -> evaluate_term a env t)
          (fun () -> evaluate_application x f args)
      }
    | _ -> runtime_error "non-integral index"
    ]
  | _ -> assert False
  ]
| Nil      -> runtime_error "index out of range"
| Opaque y -> match args with
  [ [a]      -> bind_unknown x (Opaque.apply y a)
  | [a :: b] -> do
    {
      let g = Opaque.apply y a in

      evaluate_application x !g b
    }
  | _ -> assert False
  ]
| Unbound
| Constraint _ -> runtime_error "application of unknown function"
| _            -> runtime_error "application of non-function"
]

and execute env stmt = match stmt with
[ SEquation t1 t2 -> do
  {
    let u1 = unevaluated env t1 in
    let u2 = unevaluated env t2 in

    forced_unify u1 u2
  }
| SIfThen p s0 -> do
  {
    let y = unevaluated env p in

    cont2
      (fun () -> evaluate_unknown y)
      (fun () -> match !y with
                 [ Bool False   -> ()
                 | Bool True    -> execute env s0
                 | Unbound
                 | Constraint _ -> runtime_error "if: condition is undefined"
                 | _            -> runtime_error "if: condition is not boolean"
                 ])
  }
| SIfThenElse p s0 s1 -> do
  {
    let y = unevaluated env p in

    cont2
      (fun () -> evaluate_unknown y)
      (fun () -> match !y with
                 [ Bool True    -> execute env s0
                 | Bool False   -> execute env s1
                 | Unbound
                 | Constraint _ -> runtime_error "if: condition is undefined"
                 | _            -> runtime_error "if: condition is not boolean"
                 ])
  }
| SRelation _ -> ()  (* FIX *)
]

(* Set <x> to the value of <y>. *)

and bind_unknown x y = match !y with
[ Unbound -> do
  {
    let c = Constraint [x; y] in

    !x := c;
    !y := c
  }
| Constraint c -> do
  {
    let us    = add_constraint x c in
    let new_c = Constraint us      in

    List.iter (fun z -> !z := new_c) us
  }
| LinForm lin -> do
  {
    let a = LinForm.coefficient lin x in

    if a =/ num_zero then
      !x := !y
    else do
    {
      let l = LinForm.sub lin (LinForm.of_scaled_unknown compare_unknowns a x) in

      if a =/ num_one then do
      {
        !x := Unbound;
        forced_unify (ref (LinForm l)) (ref (Number num_zero))
      }
      else
        !x := LinForm (LinForm.scale (num_one // (num_one -/ a)) l)
    }
  }
| _ -> !x := !y
]

and forced_unify x y = do
{
  let res = ref False in

  cont2
    (fun () -> unify res x y)
    (fun () -> if not !res then
                 runtime_error "unification error"
               else ())
}

and unify res x y = do
{
  let set_unknowns c v = do
  {
    List.iter (fun x -> !x := v) c
  }
  in

  match (!x, !y) with
  [ (UnevalT e t, _) -> do
    {
      cont2
        (fun () -> evaluate_term x e t)
        (fun () -> unify res x y)
    }
  | (_, UnevalT e t) -> do
    {
      cont2
        (fun () -> evaluate_term y e t)
        (fun () -> unify res x y)
    }
  | (Unbound, _) -> do { !res := True; bind_unknown x y }
  | (_, Unbound) -> do { !res := True; bind_unknown y x }
  | (Constraint a, Constraint b) -> do
      {
        let c = merge_constraints a b in
        set_unknowns c (Constraint c);
        !res := True
      }
  | (Constraint a, _)        -> do { set_unknowns a !y; !res := True }
  | (_, Constraint b)        -> do { set_unknowns b !x; !res := True }
  | (Bool a,     Bool b)     -> !res := (a = b)
  | (Char a,     Char b)     -> !res := (a = b)
  | (Symbol a,   Symbol b)   -> !res := (a = b)
  | (Nil,        Nil)        -> !res := True
  | (List a1 a2, List b1 b2) -> do
    {
      let r = ref False in

      cont2
        (fun () -> unify r a1 b1)
        (fun () -> do
          {
            if !r then
              unify res a2 b2
            else
              !res := False
          })
    }
  | (Number a,   Number b)   -> !res := (a =/ b)
  | (Number a,  LinForm lin) -> do
      {
        cont2
          (fun () -> evaluate_lin_form y lin)
          (fun () -> match !y with
           [ Number b    -> !res := (b =/ a)
           | LinForm lin -> match lin.LinForm.terms with
             [ [(c, z)]      -> unify res z (ref (Number ((a -/ lin.LinForm.const) // c)))
             | [(c, z) :: _] -> do
               {
                 unify res z (ref (LinForm (LinForm.add_const
                                         (LinForm.scale
                                           (minus_num num_one // c)
                                           (LinForm.remove_first_term lin))
                                         a)))
               }
             | _ -> assert False
             ]
           | _ -> assert False
           ])
      }
  | (LinForm lin, Number a) -> do
      {
        cont2
          (fun () -> evaluate_lin_form x lin)
          (fun () -> match !x with
           [ Number b    -> !res := (b =/ a)
           | LinForm lin -> match lin.LinForm.terms with
             [ [(c, z)]      -> unify res z (ref (Number ((a -/ lin.LinForm.const) // c)))
             | [(c, z) :: _] -> do
               {
                 unify res z (ref (LinForm (LinForm.add_const
                                         (LinForm.scale
                                           (minus_num num_one // c)
                                           (LinForm.remove_first_term lin))
                                         a)))
               }
             | _ -> assert False
             ]
           | _ -> assert False
           ])
      }
  | (LinForm a, LinForm b) -> do
      {
        cont3
          (fun () -> evaluate_lin_form x a)
          (fun () -> evaluate_lin_form y b)
          (fun () -> do
            {
              let l = LinForm.lin_comb num_one a (minus_num num_one) b in
              let z = ref (LinForm l)                                  in

              cont2
                (fun () -> evaluate_lin_form z l)
                (fun () -> match !z with
                 [ Number c    -> !res := (c =/ num_zero)
                 | LinForm lin -> match lin.LinForm.terms with
                   [ [(c, u)]      -> unify res u (ref (Number (minus_num lin.LinForm.const // c)))
                   | [(c, u) :: _] -> do
                     {
                       unify res u (ref (LinForm (LinForm.scale
                                                   (minus_num num_one // c)
                                                   (LinForm.remove_first_term lin))))
                     }
                   | _ -> assert False
                   ]
                 | _ -> assert False
                 ])
            })
      }
  | (Tuple a, Tuple b) -> do
      {
        if Array.length a <> Array.length b then
          !res := False
        else
          iter 0

        where rec iter i = do
        {
          if i >= Array.length a then
            !res := True
          else do
          {
            let r = ref False in

            cont2
              (fun () -> unify r a.(i) b.(i))
              (fun () -> do
                {
                  if !r then
                    iter (i+1)
                  else
                    !res := False
                })
          }
        }
      }
  | (Dictionary a, Dictionary b) -> do
      {
        let l0 = map_to_list a in
        let l1 = map_to_list b in

        iter l0 l1

        where rec iter l0 l1 = match (l0, l1) with
        [ ([], []) -> !res := True
        | ([], _)  -> !res := False
        | (_, [])  -> !res := False
        | ([(k0, v0) :: kv0],
           [(k1, v1) :: kv1]) -> do
           {
             if k0 <> k1 then
               !res := False
             else do
             {
               let r = ref False in

               cont2
                 (fun () -> unify r v0 v1)
                 (fun () -> do
                   {
                     if !r then
                       iter kv0 kv1
                     else
                       !res := False
                   })
             }
           }
        ]
      }
  | (Primitive1 a,     Primitive1 b)     -> !res := (a == b)
  | (Primitive2 a,     Primitive2 b)     -> !res := (a == b)
  | (PrimitiveN a1 a2, PrimitiveN b1 b2) -> !res := (a1 = b1 && a2 == b2)
  | (SimpleFunction a1 a2 a3,
     SimpleFunction b1 b2 b3)            -> !res := (a1 = b1 && a2 = b2 && a3 = b3)
  | (PatternFunction a1 a2 a3 a4 a5,
     PatternFunction b1 b2 b3 b4 b5)     -> !res := (a1 = b1 && a2 = b2 && a3 = b3 && a4 = b4 && a5 = b5)
  | (Relation a1 a2,   Relation b1 b2)   -> !res := (a1 = b1 && a2 = b2)
  | (Opaque a,         Opaque b)         -> !res := (Opaque.same_type a b && Opaque.unify a b)
  | _ -> !res := False
  ]
};

value evaluate_num name res x = do
{
  cont2
    (fun () -> evaluate_unknown x)
    (fun () -> match !x with
    [ Number n  -> !res := n
    | LinForm l -> do
      {
        cont2
          (fun () -> evaluate_lin_form x l)
          (fun () -> match !x with
          [ Number n -> !res := n
          | _        -> runtime_error (name ^ ": number expected but got " ^ type_name !x)
          ])
      }
    | _ -> runtime_error (name ^ ": number expected but got " ^ type_name !x)
    ]);
};

value rec evaluate_opaque opaque_name unwrapper name res x = match !x with
[ Opaque y -> do
  {
    try !res := unwrapper y with
    [ Opaque.Type_error -> runtime_error (name ^ ": " ^ opaque_name ^ " expected but got " ^ type_name !x) ]
  }
| UnevalT _ _ -> do
  {
    cont2
      (fun () -> evaluate_unknown x)
      (fun () -> evaluate_opaque opaque_name unwrapper name res x)
  }
| Unbound
| Constraint _ -> runtime_error (name ^ ": argument not defined")
| _            -> runtime_error (name ^ ": " ^ opaque_name ^ " expected but got " ^ type_name !x)
];

