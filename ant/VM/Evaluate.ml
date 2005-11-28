
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)

module Environment =
struct

type environment = list (array unknown);

value empty () = [];

value push env arr = [arr :: env];

value push_unbound env n =
  [Array.init
     n
     (fun _ -> create_unknown Unbound)
   :: env];

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
[ TConstant x       -> term
| TGlobal x         -> term
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
  (* Since the terms are evaluated w.r.t. to new environment we have to create
     a cyclic structure. *)

  let vars    = Array.make (Array.length terms) (create_unknown Unbound) in
  let new_env = [vars :: env] in

  Array.iteri
    (fun i _ -> vars.(i) := unevaluated new_env terms.(i))
    vars;

  new_env
};

(* |evaluate <x>| evaluates the given unknown. *)

value rec evaluate x = match !x with
[ UnevalT env term -> evaluate_term x env term
| _ -> ()
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
      evaluate y;
      bind_unknown x y
    }
  | TVariable lvl idx -> do
    {
      let y = Environment.lookup env lvl idx in

      evaluate y;
      bind_unknown x y
    }
  | TLinForm lin -> do
    {
      !x := LinForm
              (LinForm.map
                compare_unknowns
                (fun t -> do
                  {
                    let y = ref Unbound in
                    evaluate_term y env t;
                    y
                  })
                lin)
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

      evaluate_term f1 env f;

      evaluate_application x !f1 (List.map (unevaluated env) args)
    }
  | TIfThenElse p e0 e1 -> do
    {
      let y = ref Unbound in

      evaluate_term y env p;

      match !y with
      [ Bool True  -> evaluate_term x env e0
      | Bool False -> evaluate_term x env e1
      | _          -> runtime_error "type error: boolean expected"
      ]
    }
  | TLocalScope defs term -> do
    {
      let new_env = env_push_terms env defs in

      evaluate_term x new_env term
    }
  | TSequence stmts term -> do
    {
      !x := UnevalT env term;

      Array.iter (execute env) stmts;

      evaluate x
    }
  | TMatch term stack_depth num_vars patterns -> do
    {
      let stack = Array.init
                    stack_depth
                    (fun _ -> create_unknown Unbound)
                  in
      let vars  = Array.init
                    num_vars
                    (fun _ -> create_unknown Unbound)
                  in
      let expr  = ref Unbound in

      evaluate_term expr env term;

      iter patterns

      where rec iter patterns = match patterns with
      [ []                -> runtime_error "matching error"
      | [(c, g, b) :: ps] -> do
        {
          if check_patterns c expr stack vars then do
          {
            let new_env = Environment.push env vars in

            match g with
            [ None       -> evaluate_term x new_env b
            | Some guard -> do
              {
                let z = ref Unbound in

                evaluate_term z new_env guard;

                match !z with
                [ Bool True  -> evaluate_term x new_env b
                | Bool False -> iter ps
                | _          -> runtime_error "illegal guard"
                ]
              }
            ]
          }
          else
            iter ps
        }
      ]
    }
  | TUnify t1 t2 -> do
    {
      let u1 = unevaluated env t1 in
      let u2 = unevaluated env t2 in

      forced_unify u1 u2;
      bind_unknown x  u2
    }
  | TTrigger stmt -> do
    {
      execute env stmt
    }
  ]
}

and check_patterns checks expr stack vars = do
{
  iter checks expr 0

  where rec iter checks expr used_stack = do
  {
    let continue cs = match cs with
    [ [] -> True
    | _  -> iter cs (stack.(used_stack-1)) (used_stack-1)
    ]
    in

    match checks with
    [ []      -> True
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
        [ Number i    -> n =/ i && continue cs
        | UnevalT e t -> do
          {
            evaluate_term expr e t;
            iter checks expr used_stack
          }
        | _               -> False
        ]
      | PCChar c -> match !expr with
        [ Char d      -> c = d && continue cs
        | UnevalT e t -> do
          {
            evaluate_term expr e t;
            iter checks expr used_stack
          }
        | _               -> False
        ]
      | PCSymbol sym -> match !expr with
        [ Symbol s    -> sym = s && continue cs
        | UnevalT e t -> do
          {
            evaluate_term expr e t;
            iter checks expr used_stack
          }
        | _        -> False
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
              False
          }
        | UnevalT e t -> do
          {
            evaluate_term expr e t;
            iter checks expr used_stack
          }
        | _        -> False
        ]
      | PCNil -> match !expr with
        [ Nil         -> continue cs
        | UnevalT e t -> do
          {
            evaluate_term expr e t;
            iter checks expr used_stack
          }
        | _   -> False
        ]
      | PCConsList -> match !expr with
        [ List x y -> do
          {
            stack.(used_stack) := y;
            iter cs x (used_stack + 1)
          }
        | UnevalT e t -> do
          {
            evaluate_term expr e t;
            iter checks expr used_stack
          }
        | _ -> False
        ]
      ]
    ]
  }
}

(* evaluation of builtin operations *)

and add_unknowns x y = match (!x, !y) with
[ (Number m,  Number n)  -> Number (m +/ n)
| (LinForm m, Number n)  -> LinForm (LinForm.add_const m n)
| (Number m,  LinForm n) -> LinForm (LinForm.add_const n m)
| (LinForm m, LinForm n) -> LinForm (LinForm.add m n)
| (List _ _, Nil)        -> !x
| (Nil, List _ _)        -> !y
| (List a b, List _ _)   -> List a (ref (add_unknowns b y))
| (Tuple xs, Tuple ys)   -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "+: tuples differ in length"
    else
      Tuple
        (Array.init
          (Array.length xs)
          (fun i -> ref (add_unknowns xs.(i) ys.(i))))
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
    [ []            -> Tuple (Array.map (fun l -> ref (LinForm l)) result)
    | [(a,y) :: ys] -> do
      {
        let z = Array.init dim (fun _ -> ref Unbound) in

        forced_unify y (ref (Tuple z));

        iter
          (Array.mapi (fun i l -> LinForm.add_unknown l a z.(i)) result)
          ys
      }
    ]
  }
| (UnevalT e t, _)  -> do { evaluate_term x e t; add_unknowns x y }
| (_, UnevalT e t)  -> do { evaluate_term y e t; add_unknowns x y }
| (Unbound, _)      -> LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                            (LinForm.of_unknown compare_unknowns y))
| (_, Unbound)      -> LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                            (LinForm.of_unknown compare_unknowns y))
| (Constraint _, _) -> LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                            (LinForm.of_unknown compare_unknowns y))
| (_, Constraint _) -> LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                            (LinForm.of_unknown compare_unknowns y))
| (Number n, _) when n =/ num_zero -> !y    (* Allow addition of 0 to everything so we *)
| (_, Number n) when n =/ num_zero -> !x    (* do not need two versions of lin_form.   *)
| _                 -> runtime_error "+: invalid argument"
]

and sub_unknowns x y = match (!x, !y) with
[ (Number m,  Number n)  -> Number (m -/ n)
| (LinForm m, Number n)  -> LinForm (LinForm.add_const m (minus_num n))
| (Number m,  LinForm n) -> LinForm (LinForm.sub (LinForm.of_num compare_unknowns m) n )
| (LinForm m, LinForm n) -> LinForm (LinForm.sub m n)
| (Tuple xs,  Tuple ys)  -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "-: I cannot subtract tuples of different length"
    else
      Tuple
        (Array.init
          (Array.length xs)
          (fun i -> ref (sub_unknowns xs.(i) ys.(i))))
  }
| (UnevalT e t, _)       -> do { evaluate_term x e t; sub_unknowns x y }
| (_, UnevalT e t)       -> do { evaluate_term y e t; sub_unknowns x y }
| (Unbound, _)           -> LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x)
                                                 (LinForm.of_scaled_unknown compare_unknowns num_minus_one y))
| (_, Unbound)           -> LinForm (LinForm.add (LinForm.of_unknown compare_unknowns x) 
                                                 (LinForm.of_scaled_unknown compare_unknowns num_minus_one y))
| _                      -> runtime_error "-: invalid argument"
]

and mul_unknowns x y = match (!x, !y) with
[ (Number m, Number n)   -> Number (m */ n)
| (Number m, LinForm l)  -> LinForm (LinForm.scale m l)
| (LinForm l, Number n)  -> LinForm (LinForm.scale n l)
| (LinForm m, LinForm n) -> do
  {
    evaluate_lin_form x m;
    evaluate_lin_form y n;

    match (!x, !y) with
    [ (LinForm _, LinForm _) -> runtime_error "*: non-linear equation"
    | _                      -> mul_unknowns x y
    ]
  }
| (Number m,     Unbound)
| (Number m,     Constraint _) -> LinForm (LinForm.of_scaled_unknown compare_unknowns m y)
| (Unbound,      Number n)
| (Constraint _, Number n)     -> LinForm (LinForm.of_scaled_unknown compare_unknowns n x)
| (Number _,     Tuple ys)
| (Unbound,      Tuple ys)
| (Constraint _, Tuple ys) -> do
  {
    Tuple
      (Array.init
        (Array.length ys)
        (fun i -> ref (mul_unknowns x ys.(i))))
  }
| (Tuple xs, Number _)
| (Tuple xs, Unbound)
| (Tuple xs, Constraint _) -> do
  {
    Tuple
      (Array.init
        (Array.length xs)
        (fun i -> ref (mul_unknowns xs.(i) y)))
  }
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "*: tuples differ in length"
    else do
    {
      let (_, lin) =
        Array.fold_left
          (fun (i, lin) _ ->
            (i+1,
             LinForm.add
               lin
               (LinForm.of_unknown
                 compare_unknowns
                 (ref (mul_unknowns xs.(i) ys.(i)))))
          )
          (0, LinForm.lin_zero compare_unknowns)
          xs
      in

      LinForm lin
    }
  }
| (Number _, Primitive1 f) ->
    Primitive1 (fun a -> mul_unknowns x (ref (f a)))
| (Number _, Primitive2 f) ->
    Primitive2 (fun a b -> mul_unknowns x (ref (f a b)))
| (Number _, PrimitiveN ar f) ->
    PrimitiveN ar (fun a -> mul_unknowns x (ref (f a)))
| (Number _, SimpleFunction ar env body) ->
    SimpleFunction ar env (TApplication (TConstant (Primitive2 mul_unknowns)) [TConstant !x; body])
| (Number _, PatternFunction a b c d body) -> do
  {
    PatternFunction a b c d
      (List.map
        (fun (pc, g, t) ->
          (pc, g, (TApplication (TConstant (Primitive2 mul_unknowns)) [TConstant !x; t])))
      body)
  }
| (_, List _ _) -> do   (*  x * [y,z]  =>  y + x*(z-y) *)
  {
    let points = evaluate_list "*" y in

    match points with
    [ [a]    -> !a
    | [a; b] -> do  (* treat the common case separately *)
      {
        let c = ref (sub_unknowns b a) in
        let d = ref (mul_unknowns x c) in

        add_unknowns a d
      }
    | [a :: b] -> do
      {
        evaluate x;

        match !x with
        [ Number t -> do
          {
            let n = List.length points - 1 in
            let s = num_one -/ t           in

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

            LinForm lin
          }
        | _ -> runtime_error "*: invalid argument"
        ]
      }
    | [] -> assert False
    ]
  }
| (UnevalT e t, _) -> do { evaluate_term x e t; mul_unknowns x y }
| (_, UnevalT e t) -> do { evaluate_term y e t; mul_unknowns x y }
| _                -> runtime_error "*: invalid argument"
]

and div_unknowns x y = match (!x, !y) with
[ (Number m,  Number n) -> Number (m // n)
| (LinForm l, Number n) -> LinForm (LinForm.scale (num_one // n) l)
| (Unbound,   Number n) -> LinForm (LinForm.of_scaled_unknown compare_unknowns (num_one // n) x)
| (Tuple xs,  Number _) -> do
  {
    Tuple
      (Array.init
        (Array.length xs)
        (fun i -> ref (div_unknowns xs.(i) y)))
  }
| (UnevalT e t, _) -> do { evaluate_term x e t; div_unknowns x y }
| (_, UnevalT e t) -> do { evaluate_term y e t; div_unknowns x y }
| (_, LinForm l)   -> do
  {
    evaluate_lin_form y l;

    match !y with
    [ Number _ -> div_unknowns x y
    | _        -> runtime_error ("/: invalid argument")
    ]
  }
| _ -> runtime_error "/: invalid argument"
]

and evaluate_list name x = match !x with
[ Nil          -> []
| List a b     -> [a :: evaluate_list name b]
| UnevalT e t  -> do { evaluate_term x e t; evaluate_list name x }
| Unbound
| Constraint _ -> runtime_error (name ^ ": argument undefined")
| _            -> runtime_error (name ^ ": invalid argument")
]

and evaluate_lin_form x lin = do
{
  (* We check for unbound, constraint, and other unkowns. *)

  !x := Unbound;

  let (lin, coeff, const) = collect lin.LinForm.terms
    where rec collect terms = match terms with
    [ [] -> (LinForm.lin_zero compare_unknowns, num_zero, ref (Number lin.LinForm.const))
    | [((a, y) as t) :: ts] -> do
      {
        evaluate y;

        let (lin, coeff, const) = collect ts in

        match !y with
        [ Unbound      -> if identical x y then
                            (lin, coeff +/ a, const)
                          else
                            (LinForm.add_unknown lin a y, coeff, const)
        | Constraint _ -> (LinForm.add_unknown lin a y, coeff, const)
        | _            -> (lin,
                           coeff,
                           ref (add_unknowns
                                 const
                                 (ref (mul_unknowns (ref (Number a)) y))))
        ]
      }
    ]
  in

  let sum = if LinForm.is_constant lin then
              add_unknowns const (ref (Number lin.LinForm.const))
            else
              add_unknowns const (ref (LinForm lin))
            in

  if coeff =/ num_zero then
    !x := sum
  else if coeff =/ num_one then
    forced_unify
      (ref (sub_unknowns (ref sum) x))
      (ref (Number num_zero))
  else
    !x := mul_unknowns
            (ref (Number (num_one // (num_one -/ coeff))))
            (ref sum)
(*
  collect [] [] (ref (Number lin.LinForm.const)) num_zero lin.LinForm.terms

  where rec collect unbound constr known coeff terms = match terms with
  [ [((a, y) as t) :: ts] -> do
    {
      evaluate y;

      match !y with
      [ Unbound      -> if identical x y then
                          collect unbound constr known (coeff +/ a) ts
                        else
                          collect [t :: unbound] constr known coeff ts
      | Constraint _ -> collect unbound [t :: constr] known coeff ts
      | _            -> collect
                          unbound
                          constr
                          (ref (add_unknowns
                                 known
                                 (ref (mul_unknowns (ref (Number a)) y))))
                          coeff
                          ts
      ]
    }
  | [] -> do
    {
      (* Some unknowns may appear twice in |constr|. *)

      (*
      assert (check unbound  (* check that all terms in <unbound> are different *)
      where rec check terms = match terms with
      [ [] -> True
      | [(_,y)::ts] -> List.for_all (fun (_,z) -> y != z) ts && check ts
      ]);
      *)

      merge_constraint (constr @  unbound) []

      where rec merge_constraint constr terms = match constr with
      [ [] -> do
        {
          let lin = LinForm.of_terms compare_unknowns terms in
          let sum = if LinForm.is_constant lin then
                      add_unknowns known (ref (Number lin.LinForm.const))
                    else
                      add_unknowns known (ref (LinForm lin))
                    in

          if coeff =/ num_zero then
            !x := sum
          else if coeff =/ num_one then
            forced_unify
              (ref (sub_unknowns (ref sum) x))
              (ref (Number num_zero))
          else
            !x := mul_unknowns
                    (ref (Number (num_one // (num_one -/ coeff))))
                    (ref sum)
        }
      | [(a, y) :: ys] -> do
        {
          find_identical a y [] ys

          where rec find_identical a y zs ys = match ys with
          [ []                    -> merge_constraint zs [(a, y) :: terms]
          | [((c, u) as t) :: us] -> if identical u y then
                                       find_identical (a +/ c) y zs us
                                     else
                                       find_identical a y [t :: zs] us
          ]
        }
      ]
    }
  ]
*)
(*  match List.fold_right
         (fun (b, y) (lin, coef) -> do
           {
             if identical x y then
               (lin, Some b)
             else do
             {
               evaluate y;

               match !y with
               [ LinForm lf   -> (LinForm.lin_comb num_one lin b lf, coef)
               | Number c     -> (LinForm.add_const lin c,           coef)
               | Unbound
               | Constraint _
               | UnevalT _ _  -> (LinForm.add lin (LinForm.of_scaled_unknown identical b y), coef)
               | _            -> runtime_error "type error"
               ]
             }
           })
         lin.LinForm.terms
         (LinForm.of_num identical lin.LinForm.const, None)
  with
  [ (lin, Some b) -> !x := LinForm (LinForm.scale (num_one // (num_one -/ b)) lin)
  | (lin, None)   -> match lin.LinForm.terms with
                     [ [] -> !x := Number lin.LinForm.const
                     | _  -> !x := LinForm lin
                     ]
  ]*)
}

and evaluate_application x f args = match f with
[ Primitive1 p -> match args with
  [ [a]      -> !x := p a
  | [a :: b] -> do
    {
      let g = p a in

(*      !x := Application g b;*)

      evaluate_application x g b
    }
  | _ -> assert False
  ]
| Primitive2 p -> match args with
  [ [a; b]      -> !x := p a b
  | [a; b :: c] -> do
    {
      let g = p a b in

(*      !x := Application g c;*)

      evaluate_application x g c
    }
  | [a] -> !x := Application f args
  | _   -> assert False
  ]
| PrimitiveN arity p -> do
  {
    let n = List.length args in

    if arity > n then
      !x := Application f args
    else do
    {
      let (vars, rest) = XList.split_at arity args in
      let result       = p vars in

      if n > arity then do
      {
(*        !x := Application result rest;*)

        evaluate_application x result rest
      }
      else
        !x := result
    }
  }
| SimpleFunction arity e body -> do
  {
    let n = List.length args in

    if arity > n then
      !x := Application f args
    else do
    {
      if n > arity then do
      {
        let (vars, rest) = XList.split_at arity args in
        let new_env      = Environment.push e (Array.of_list vars) in

        let result = ref Unbound in

        evaluate_term result new_env body;

(*        !x := Application !result rest;*)

        evaluate_application x !result rest
      }
      else
        evaluate_term x (Environment.push e (Array.of_list args)) body
    }
  }
| PatternFunction arity e stack_depth num_vars pats -> do
  {
    let n = List.length args in

    if arity > n then
      !x := Application f args
    else if n > arity then do
    {
      let (used, rest) = XList.split_at arity args in

      let result = ref Unbound in

      evaluate_term result e
        (TMatch
          (make_tuple (List.map (fun v -> TGlobal v) used))
          stack_depth
          num_vars
          pats);

(*      !x := Application !result args;*)

      evaluate_application x !result args
    }
    else
      evaluate_term
        x e
        (TMatch
          (make_tuple (List.map (fun v -> TGlobal v) args))
          stack_depth
          num_vars
          pats)
  }
| Application f2 args2 -> do
  {
    let a = args2 @ args in

(*    !x := Application f2 a;*)

    evaluate_application x f2 a
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
              evaluate y;

              evaluate_application x !y b
            }
          ]
        }
        with
        [ Not_found -> runtime_error "entry not found in dictionary" ]
      }
    | UnevalT env t -> do
      {
        evaluate_term a env t;
        evaluate_application x f args
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
              evaluate ys.(i);
              evaluate_application x !(ys.(i)) b
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
        evaluate_term a env t;
        evaluate_application x f args
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
              | _  -> do
                {
(*                  !x := Application !y b;*)

                  evaluate_application x !y b
                }
              ]
              else if i > 0 then
                iter z (i-1)
              else
                runtime_error "index out of range"
            }
          | UnevalT env t -> do
            {
              evaluate_term lst env t;
              iter lst i
            }
          | _ -> runtime_error "malformed list"
          ]
        }
        else
          runtime_error "non-integral index"
      }
    | UnevalT env t -> do
      {
        evaluate_term a env t;
        evaluate_application x f args
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

(*      !x := Application !g b;*)

      evaluate_application x !g b
    }
  | _ -> assert False
  ]
| _ -> runtime_error "application of non-function"
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

    evaluate y;

    match !y with
    [ Bool False   -> ()
    | Bool True    -> execute env s0
    | Unbound
    | Constraint _ -> runtime_error "if: condition is undefined"
    | _            -> runtime_error "if: condition is not boolean"
    ]
  }
| SIfThenElse p s0 s1 -> do
  {
    let y = unevaluated env p in

    evaluate y;

    match !y with
    [ Bool True    -> execute env s0
    | Bool False   -> execute env s1
    | Unbound
    | Constraint _ -> runtime_error "if: condition is undefined"
    | _            -> runtime_error "if: condition is not boolean"
    ]
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
    let new_c = Constraint (add_constraint x c) in

    !x := new_c;
    !y := new_c
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
  if not (unify x y) then
    runtime_error "unification error"
  else ()
}

and unify x y = do
{
  let set_unknowns c v = do
  {
    List.iter (fun x -> !x := v) c
  }
  in

  match (!x, !y) with
  [ (UnevalT e t, _)   -> do { evaluate_term x e t; unify x y }
  | (_, UnevalT e t)   -> do { evaluate_term y e t; unify x y }
  | (Unbound, _)       -> do { bind_unknown x y; True }
  | (_, Unbound)       -> do { bind_unknown y x; True }
  | (Constraint a, Constraint b) -> do
      {
        let c = merge_constraints a b in
        set_unknowns c (Constraint c);
        True
      }
  | (Constraint a, _)        -> do { set_unknowns a !y; True }
  | (_, Constraint b)        -> do { set_unknowns b !x; True }
  | (Bool a,     Bool b)     -> a = b
  | (Char a,     Char b)     -> a = b
  | (Symbol a,   Symbol b)   -> a = b
  | (Nil,        Nil)        -> True
  | (List a1 a2, List b1 b2) -> unify a1 b1 && unify a2 b2
  | (Number a,   Number b)   -> a =/ b
  | (Number a,  LinForm lin) -> do
      {
        evaluate_lin_form y lin;

        match !y with
        [ Number b    -> b =/ a
        | LinForm lin -> match lin.LinForm.terms with
          [ [(c, z)]       -> unify z (ref (Number ((a -/ lin.LinForm.const) // c)))
          | [(c, z) :: zs] -> do
            {
              unify z (ref (LinForm (LinForm.add_const
                                      (LinForm.scale
                                        (minus_num num_one // c)
                                        (LinForm.remove_first_term lin))
                                      a)))
            }
          | _ -> assert False
          ]
        | _ -> assert False
        ]
      }
  | (LinForm lin, Number a) -> do
      {
        evaluate_lin_form x lin;

        match !x with
        [ Number b    -> b =/ a
        | LinForm lin -> match lin.LinForm.terms with
          [ [(c, z)]       -> unify z (ref (Number ((a -/ lin.LinForm.const) // c)))
          | [(c, z) :: zs] -> do
            {
              unify z (ref (LinForm (LinForm.add_const
                                      (LinForm.scale
                                        (minus_num num_one // c)
                                        (LinForm.remove_first_term lin))
                                      a)))
            }
          | _ -> assert False
          ]
        | _ -> assert False
        ]
      }
  | (LinForm a, LinForm b) -> do
      {
        evaluate_lin_form x a;
        evaluate_lin_form y b;

        let l = LinForm.lin_comb num_one a (minus_num num_one) b in
        let z = ref (LinForm l)                                  in

        evaluate_lin_form z l;

        match !z with
        [ Number c    -> c =/ num_zero
        | LinForm lin -> match lin.LinForm.terms with
          [ [(c, u)]       -> unify u (ref (Number (minus_num lin.LinForm.const // c)))
          | [(c, u) :: us] -> do
            {
              unify u (ref (LinForm (LinForm.scale
                                       (minus_num num_one // c)
                                       (LinForm.remove_first_term lin))))
            }
          | _ -> assert False
          ]
        | _ -> assert False
        ]
      }
  | (Tuple a, Tuple b) -> do
      {
        if Array.length a <> Array.length b then
          False
        else
          iter 0

        where rec iter i = do
        {
          (i >= Array.length a) || (unify a.(i) b.(i) && iter (i+1))
        }
      }
  | (Dictionary a, Dictionary b) -> do
      {
        let l0 = map_to_list a in
        let l1 = map_to_list b in

        iter l0 l1

        where rec iter l0 l1 = match (l0, l1) with
        [ ([], []) -> True
        | ([], _)  -> False
        | (_, [])  -> False
        | ([(k0, v0) :: kv0],
           [(k1, v1) :: kv1]) -> k0 = k1 && unify v0 v1 && iter kv0 kv1
        ]
      }
  | (Primitive1 a,     Primitive1 b)     -> a == b
  | (Primitive2 a,     Primitive2 b)     -> a == b
  | (PrimitiveN a1 a2, PrimitiveN b1 b2) -> a1 = b1 && a2 == b2
  | (SimpleFunction a1 a2 a3,
     SimpleFunction b1 b2 b3)            -> a1 = b1 && a2 = b2 && a3 = b3
  | (PatternFunction a1 a2 a3 a4 a5,
     PatternFunction b1 b2 b3 b4 b5)     -> a1 = b1 && a2 = b2 && a3 = b3 && a4 = b4 && a5 = b5
  | (Relation a1 a2,   Relation b1 b2)   -> a1 = b1 && a2 = b2
  | (Opaque a,         Opaque b)         -> Opaque.same_type a b && Opaque.unify a b
  | _ -> False
  ]
};

