
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)

value true_sym  = string_to_symbol (UString.uc_string_of_ascii "True");
value false_sym = string_to_symbol (UString.uc_string_of_ascii "False");

value make_tuple_pat elements = match elements with
[ []  -> assert False
| [x] -> x
| _   -> Parser.PTuple elements
];

value compile_pattern pat = do
{
  let (stack_depth, num_vars, var_names, checks) = compile pat (0, 0, [], []) in

  (stack_depth, num_vars, List.rev var_names, checks)
}
where rec compile pat ((stack_depth, num_vars, var_names, checks) as state) = match pat with
[ Parser.PAnything   -> (stack_depth,
                         num_vars,
                         var_names,
                         [PCAnything :: checks])
| Parser.PId sym     -> (stack_depth,
                         num_vars + 1,
                         [sym :: var_names],
                         [PCVariable num_vars :: checks])
| Parser.PAssign x p -> do
  {
    let (s,n,v,c) = compile p state in

    (s, n + 1, [x :: v], [PCAssign n :: c])
  }
| Parser.PNumber x   -> (stack_depth,
                         num_vars,
                         var_names,
                         [PCNumber x :: checks])
| Parser.PChar x     -> (stack_depth,
                         num_vars,
                         var_names,
                         [PCChar x :: checks])
| Parser.PSymbol sym -> (stack_depth,
                         num_vars,
                         var_names,
                         [PCSymbol sym :: checks])
| Parser.PTuple xs   -> do
  {
    let len       = List.length xs in
    let (s,n,v,c) = List.fold_right compile xs state in

    (s + len - 1, n, v, [PCTuple len :: c])
  }
| Parser.PList xs -> do
  {
    List.fold_right
      (fun x s1 -> do
        {
          let (s,n,v,c) = compile x s1 in
          (max s (stack_depth + 1), n, v, [PCConsList :: c])
        })
      xs
      (stack_depth, num_vars, var_names, [PCNil :: checks])
  }
| Parser.PListTail xs tail -> do
  {
    List.fold_right
      (fun x s1 -> do
        {
          let (s,n,v,c) = compile x s1 in
          (max s (stack_depth + 1), n, v, [PCConsList :: c])
        })
      xs
      (compile tail state)
  }
];

value rec compile_lazy_pattern scope pat = match pat with
[ Parser.PAnything       -> ([], TConstant Unbound)
| Parser.PId sym         -> ([sym], Scope.lookup scope sym)
| Parser.PNumber n       -> ([], TConstant (Number n))
| Parser.PChar c         -> ([], TConstant (Char c))
| Parser.PSymbol sym     -> ([], TConstant (Symbol sym))
| Parser.PAssign sym pat -> do
  {
    let (s, p) = compile_lazy_pattern scope pat in

    ([sym :: s], TUnify (Scope.lookup scope sym) p)
  }
| Parser.PTuple pats -> do
  {
    let (s, ps) =
      List.fold_right
        (fun p (ss, ps) -> do
          {
            let (s2, p2) = compile_lazy_pattern scope p in

            (s2 @ ss, [p2 :: ps])
          })
          pats
          ([], [])
    in

    (s, TConsTuple (Array.of_list ps))
  }
| Parser.PList pats -> do
  {
    List.fold_right
      (fun p (ss, ps) -> do
        {
          let (s2, p2) = compile_lazy_pattern scope p in

          (s2 @ ss, TConsList p2 ps)
        })
        pats
        ([], TConstant Nil)
  }
| Parser.PListTail pats tail -> do
  {
    List.fold_right
      (fun p (ss, ps) -> do
        {
          let (s2, p2) = compile_lazy_pattern scope p in

          (s2 @ ss, TConsList p2 ps)
        })
        pats
        (compile_lazy_pattern scope tail)
  }
];

value rec compile_expr scope expr = match expr with
[ Parser.TUnbound -> TConstant Unbound
| Parser.TId x    -> do
  {
    try
      Scope.lookup scope x
    with
    [ Not_found -> raise (Syntax_error
                            ("", 0, 0)
                            (UString.append (UString.uc_string_of_ascii "undefined symbol ")
                                            (symbol_to_string x)))
    ]
  }
| Parser.TNumber x -> TConstant (Number x)
| Parser.TChar x   -> TConstant (Char x)
| Parser.TSymbol x -> do
  {
    if x = true_sym then
      TConstant (Bool True)
    else if x = false_sym then
      TConstant (Bool False)
    else
      TConstant (Symbol x)
  }
| Parser.TApp f args -> TApplication
                          (compile_expr scope f)
                          (List.map (compile_expr scope) args)
| Parser.TTuple xs   -> TConsTuple (Array.of_list (List.map (compile_expr scope) xs))
| Parser.TList xs    -> do
  {
    List.fold_right
      (fun x y -> TConsList (compile_expr scope x) y)
      xs
      (TConstant Nil)
  }
| Parser.TListTail xs tail   -> do
  {
    List.fold_right
      (fun x y -> TConsList (compile_expr scope x) y)
      xs
      (compile_expr scope tail)
  }
| Parser.TFun cases -> do
  {
    iter (TConstant (Dictionary SymbolMap.empty)) cases

    where rec iter result cases = match cases with
    [ []                          -> result
    | [(pats, guard, term) :: cs] ->
        iter (merge_function_cases
               result
               (compile_function scope pats guard term))
             cs
    ]
  }
| Parser.TLocal decls expr -> do
  {
    let (new_scope, init) = compile_local_declarations scope decls in

    TLocalScope
      init
      (compile_expr new_scope expr)
  }
| Parser.TSequence stmts expr -> do
  {
    TSequence 
      (Array.of_list (List.map (compile_statement scope) stmts))
      (compile_expr scope expr)
  }
| Parser.TIfThenElse p e0 e1 -> TIfThenElse
                                  (compile_expr scope p)
                                  (compile_expr scope e0)
                                  (compile_expr scope e1)
| Parser.TMatch expr pats    -> do
  {
    let (stack_depth, num_vars, patterns) =
      List.fold_left
        (fun (stack, num_vars, patterns) (p,g,e) -> do
          {
            let (s,n,v,c)      = compile_pattern p        in
            let (new_scope, _) = Scope.push scope v       in
            let t              = compile_expr new_scope e in

            let guard = match g with
            [ None      -> None
            | Some expr -> Some (compile_expr new_scope expr)
            ]
            in

            (max stack s, max num_vars n, [(c,guard,t) :: patterns])
          })
        (0, 0, [])
        pats
    in

    TMatch
      (compile_expr scope expr)
      stack_depth
      num_vars
      (List.rev patterns)
  }
]

and compile_function scope pats guard term = do
{
  let simple_pat p = match p with
  [ Parser.PAnything
  | Parser.PId _     -> True
  | _                -> False
  ]
  in

  let rec finite_pat p = match p with
  [ Parser.PSymbol s -> Some s
  | _                -> None
  ]
  in
(*
  let rec finite_pat p = match p with
  [ Parser.PNumber n -> Some (Number n)
  | Parser.PChar c   -> Some (Char c)
  | Parser.PSymbol s -> Some (Symbol s)
  | Parser.PTuple ps -> do
    {
      match
        List.fold_right
          (fun p l -> match l with
           [ None   -> None
           | Some x -> match finite_pat p with
             [ None   -> None
             | Some y -> Some [ref y :: x]
             ]
           ])
          ps
          (Some [])
      with
      [ None   -> None
      | Some l -> Some (Tuple (Array.of_list l))
      ]
    }
  | Parser.PList ps -> do
    {
      List.fold_right
        (fun p l -> match l with
         [ None   -> None
         | Some x -> match finite_pat p with
           [ None   -> None
           | Some y -> Some (List (ref y) (ref x))
           ]
         ])
        ps
        (Some Nil)
    }
  | Parser.PListTail ps p -> match finite_pat p with
    [ None   -> None
    | Some z -> do
      {
        List.fold_right
          (fun p l -> match l with
           [ None   -> None
           | Some x -> match finite_pat p with
             [ None   -> None
             | Some y -> Some (List (ref y) (ref x))
             ]
           ])
          ps
          (Some z)
      }
    ]
  | Parser.PAnything
  | Parser.PId _
  | Parser.PAssign _ _ -> None
  ]
  in
*)

  let to_dictionary pats = match pats with
  [ [p] -> if guard = None then
             finite_pat p
           else
             None
  | _   -> None
  ]
  in

  match to_dictionary pats with
  [ Some s -> TDictionary [(s, compile_expr scope term)]
  | None   -> do
    {
      if List.for_all simple_pat pats && guard = None then do
      {
        let (n, vars) = iter 0 pats

        where rec iter n pats = match pats with
        [ []      -> (n, [])
        | [p::ps] -> match p with
          [ Parser.PAnything -> let (k, v) = iter (n+1) ps in
                                (k, [(-1) :: v])
          | Parser.PId sym   -> let (k, v) = iter (n+1) ps in
                                (k, [sym :: v])
          | _                -> assert False
          ]
        ]
        in

        let (new_scope, _) = if n > 0 then
                               Scope.push scope vars
                             else
                               (scope, 0)
                             in

        TSimpleFunction n (compile_expr new_scope term)
      }
      else do
      {
        let (stack_depth, num_vars, var_names, checks) = compile_pattern (make_tuple_pat pats) in

        let n = List.length pats in

        let (new_scope, _) = if n > 0 then
                               Scope.push scope var_names
                             else
                               (scope, 0)
                             in
        let g = match guard with
        [ None      -> None
        | Some expr -> Some (compile_expr new_scope expr)
        ]
        in

        TPatternFunction
          n
          stack_depth
          num_vars
          [(checks, g, compile_expr new_scope term)]
      }
    }
  ]
}
and merge_function_cases term new_case = match term with
[ TConstant _         -> new_case
| TSimpleFunction _ _ -> term
| TDictionary dict -> match new_case with
  [ TDictionary d -> TDictionary (d @ dict)
  | TSimpleFunction arity term -> do
    {
      if arity = 1 then
        TPatternFunction
          arity
          0
          1
          [([PCVariable 0], None, term)
           :: List.map (fun (s,t) -> ([PCSymbol s], None, t)) dict]
      else
        raise (Syntax_error ("", 0, 0) (UString.uc_string_of_ascii "mismatching arity"))
    }
  | TPatternFunction arity stack_depth num_vars pats -> do
    {
      if arity = 1 then
        TPatternFunction
          arity
          stack_depth
          num_vars
          (pats @ List.map (fun (s,t) -> ([PCSymbol s], None, t)) dict)
      else
        raise (Syntax_error ("", 0, 0) (UString.uc_string_of_ascii "mismatching arity"))
    }
  | _ -> assert False
  ]
| TPatternFunction arity stack_depth num_vars pats -> match new_case with
  [ TPatternFunction a s n p -> do
    {
      if a = arity then
        TPatternFunction
          arity
          (max s stack_depth)
          (max n num_vars)
          (p @ pats)
      else
        raise (Syntax_error ("", 0, 0) (UString.uc_string_of_ascii "mismatching arity"))
    }
  | TSimpleFunction a body -> do
    {
      if a = arity then do
      {
        TPatternFunction
          arity
          (max (a-1) stack_depth)
          (max a num_vars)
          [(if a > 1 then [PCTuple a :: vars 0] else vars 0,
            None,
            body)
           :: pats]

        where rec vars n = do
        {
          if n >= arity then
            []
          else
            [PCVariable n :: vars (n+1)]
        }
      }
      else
        raise (Syntax_error ("", 0, 0) (UString.uc_string_of_ascii "mismatching arity"))
    }
  | TDictionary dict -> do
    {
      if arity = 1 then
        TPatternFunction
          arity
          stack_depth
          num_vars
          (List.fold_right
            (fun (s,t) pats ->
              [([PCSymbol s], None, t) :: pats])
            dict
            pats)
      else
        raise (Syntax_error ("", 0, 0) (UString.uc_string_of_ascii "mismatching arity"))
    }
  | _ -> assert False
  ]
| _ -> assert False
]
and compile_local_declarations scope decls = do
{
  let table = Hashtbl.create 32 in

  let add name term = do
  {
    try
      Hashtbl.replace
        table
        name
        (merge_function_cases
          (Hashtbl.find table name)
          term)
    with
    [ Not_found          -> Hashtbl.add table name term
    | Syntax_error loc _ ->
       raise (Syntax_error
               loc
               (UString.append (UString.uc_string_of_ascii "mismatching arity in definition of ")
                               (symbol_to_string name)))
    ]
  }
  in

  (* add new symbols to the scope *)

  let rec add_names names decls = match decls with
  [ []                             -> Scope.push scope names
  | [Parser.DFun name _ _ _ :: ds] -> add_names [name :: names] ds
  | [Parser.DPattern p _ :: ds]    -> do
    {
      add_names (iter names p) ds

      where rec iter names p = match p with
      [ Parser.PId name       -> [name :: names]
      | Parser.PAnything
      | Parser.PNumber _
      | Parser.PChar _
      | Parser.PSymbol _      -> names
      | Parser.PTuple ps      -> List.fold_left iter names ps
      | Parser.PList ps       -> List.fold_left iter names ps
      | Parser.PListTail ps p -> List.fold_left iter (iter names p) ps
      | Parser.PAssign name p -> [name :: iter names p]
      ]
    }
  ]
  in

  let (new_scope, num_vars) = add_names [] decls in

  (* define the new symbols *)

  iter [] decls

  where rec iter equations decls = match decls with
  [ [] -> do
    {
      let init = Array.make num_vars (TConstant Unbound) in

      Hashtbl.iter
        (fun name term -> match term with
          [ TPatternFunction a s n pats -> do
            {
              let (_, var) = Scope.lookup_local new_scope name in

              init.(var) := TPatternFunction a s n (List.rev pats)
            }
          | TSimpleFunction a t -> do
            {
              let (_, var) = Scope.lookup_local new_scope name in

              if a > 0 then
                init.(var) := term
              else
                init.(var) := t
            }
          | TDictionary dict -> do
            {
              let (_, var) = Scope.lookup_local new_scope name in

              init.(var) := term
            }
          | TTrigger _ -> do
            {
              let (_, var) = Scope.lookup_local new_scope name in

              init.(var) := term
            }
          | _ -> assert False
          ]
        )
        table;

      (new_scope, init)
    }
  | [Parser.DFun name pats guard term :: ds] -> do
    {
      add name (compile_function new_scope pats guard term);

      iter equations ds
    }
  | [Parser.DPattern pat term :: ds] -> do
    {
      let (syms, pat) = compile_lazy_pattern new_scope pat          in
      let eq          = SEquation pat (compile_expr new_scope term) in

      List.iter (fun s -> add s (TTrigger eq)) syms;

      iter [eq :: equations] ds
    }
  ]
}
and compile_statement scope stmt = match stmt with
[ Parser.SEquation x y       -> SEquation   (compile_expr scope x)
                                            (compile_expr scope y)
| Parser.SIfThen p s         -> SIfThen     (compile_expr scope p)
                                            (compile_statement scope s)
| Parser.SIfThenElse p s0 s1 -> SIfThenElse (compile_expr scope p)
                                            (compile_statement scope s0)
                                            (compile_statement scope s1)
];

value rec compile_global_declarations scope decls = do
{
  let table = Hashtbl.create 32 in

  let add name term = do
  {
    try
      Hashtbl.replace
        table
        name
        (merge_function_cases
          (Hashtbl.find table name)
          term)
    with
    [ Not_found          -> Hashtbl.add table name term
    | Syntax_error loc _ ->
       raise (Syntax_error
               loc
               (UString.append (UString.uc_string_of_ascii "mismatching arity in definition of ")
                               (symbol_to_string name)))
    ]
  }
  in

  (* add new global symbols to the scope *)

  let rec add_names decls = match decls with
  [ []                             -> ()
  | [Parser.DFun name _ _ _ :: ds] -> do
    {
      Scope.add_global scope name Unbound;
      add_names ds
    }
  | [Parser.DPattern p _ :: ds] -> assert False (* FIX *)
  ]
  in

  add_names decls;

  (* define the new symbols *)

  iter decls

  where rec iter decls = match decls with
  [ [] -> do
    {
      Hashtbl.iter
        (fun name term -> match term with
          [ TPatternFunction a s n pats -> do
            {
              let var = Scope.lookup_global scope name in

              !var := PatternFunction a [] s n (List.rev pats)
            }
          | TSimpleFunction a t -> do
            {
              let var = Scope.lookup_global scope name in

              if a > 0 then
                !var := SimpleFunction a [] t
              else
                !var := UnevalT [] t
            }
          | TDictionary dict -> do
            {
              let d =
                List.fold_left
                  (fun m (s,v) ->
                    SymbolMap.add s (ref (UnevalT [] v)) m)
                  SymbolMap.empty
                  dict
              in
              let var = Scope.lookup_global scope name in

              !var := Dictionary d
            }
          | _ -> assert False
          ])
        table
    }
  | [Parser.DFun name pats guard term :: ds] -> do
    {
      add name (compile_function scope pats guard term);

      iter ds
    }
  | [Parser.DPattern pat term :: ds] -> do
    {
      (* FIX *)
      assert False
    }
  ]
};


(*
value compile decls = do
{
  let scope = Scope.create () in

  compile_global_declarations scope decls;

  scope
};
*)

value compile_declarations scope stream = do
{
  let lexer = Lexer.make_lexer
                (Scope.symbol_table scope)
                stream
              in
  let decls = Parser.parse_program lexer in

  compile_global_declarations scope decls
};

value compile_expression scope stream = do
{
  let lexer = Lexer.make_lexer
                (Scope.symbol_table scope)
                stream
              in
  let expr  = Parser.parse_expression lexer in

  compile_expr scope expr
};

