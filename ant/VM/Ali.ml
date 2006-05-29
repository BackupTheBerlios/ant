
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;
open VMPrivate;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)

value print_list p sep l = match l with
[ []      -> ()
| [x]     -> p x
| [x::xs] -> do
  {
    p x;
    List.iter (fun y -> do { Printf.printf "%s" sep; p y }) xs
  }
];

(*
value rec print_p_decl (id, ps, e) = do
{
  Printf.printf "fun <%s>" (UString.to_string (symbol_to_string id));
  print_list print_p_pat "" ps;
  Printf.printf " =";
  print_p_expr e;
}
and print_p_expr e = match e with
[ Parser.TUnbound  -> Printf.printf " _"
| Parser.TId x     -> Printf.printf " %s" (UString.to_string (symbol_to_string x))
| Parser.TNumber x -> Printf.printf " %f" (float_of_num x)
| Parser.TChar x   -> Printf.printf " '\\x%x'" x
| Parser.TSymbol x -> Printf.printf " %s" (UString.to_string (symbol_to_string x))
| Parser.TApp f args -> do
  {
    Printf.printf "(";
    print_p_expr f;
    List.iter print_p_expr args;
    Printf.printf ")\n"
  }
| Parser.TTuple xs -> do
  {
    Printf.printf " (";
    print_list print_p_expr "," xs;
    Printf.printf ")\n"
  }
| Parser.TList xs -> do
  {
    Printf.printf " [";
    print_list print_p_expr "," xs;
    Printf.printf "]\n"
  }
| Parser.TListTail xs tl -> do
  {
    Printf.printf " [";
    print_list print_p_expr "," xs;
    Printf.printf " : ";
    print_p_expr tl;
    Printf.printf "]\n"
  }
| Parser.TFun pats -> do
  {
    Printf.printf " {";
    print_list
      (fun (p,g,t) -> do
        {
          print_list print_p_pat " " p;
          match g with
          [ None   -> ()
          | Some e -> do
            {
              Printf.printf " &";
              print_p_expr e
            }
          ];
          Printf.printf " :=";
          print_p_expr t
        })
      " |"
      pats;
    Printf.printf " }\n"
  }
| Parser.TLocal x e -> do
  {
    Printf.printf " local ... in";
(*    List.iter (fun s -> Printf.printf "<%s> " (UString.to_string (symbol_to_string s))) x;
    Printf.printf "in "; *)
    print_p_expr e
  }
| Parser.TSequence s e -> do
  {
    Printf.printf " begin";
(*    List.iter (fun s -> do { print_stmt s; Printf.printf "," }) s; *)
    print_p_expr e;
    Printf.printf " end";
  }
| Parser.TIfThenElse p e0 e1 -> do
  {
    Printf.printf " if";
    print_p_expr p;
    Printf.printf " then";
    print_p_expr e0;
    Printf.printf " else";
    print_p_expr e1;
    Printf.printf "\n"
  }
| Parser.TMatch e ps -> do
  {
    Printf.printf " match";
    print_p_expr e;
    Printf.printf "\n{";
    print_list
      (fun (p, g, e) -> do {
        print_p_pat p;
        match g with
        [ None   -> ()
        | Some t -> do
          {
            Printf.printf " &";
            print_p_expr t
          }
        ];
        Printf.printf " :=";
        print_p_expr e })
      "\n|" ps;
    Printf.printf "}\n"
  }
]
and print_p_pat p = match p with
[ Parser.PAnything -> Printf.printf " _"
| Parser.PId x -> Printf.printf " <%s>" (UString.to_string (symbol_to_string x))
| Parser.PAssign x p -> do
  {
    Printf.printf " <%s> = " (UString.to_string (symbol_to_string x));
    print_p_pat p
  }
| Parser.PNumber x -> Printf.printf " %f" (float_of_num x)
| Parser.PChar x -> Printf.printf " '\\x%x'" x
| Parser.PSymbol sym -> Printf.printf " '%s" (UString.to_string (symbol_to_string sym))
| Parser.PTuple xs -> do
  {
    Printf.printf " (";
    print_list print_p_pat "," xs;
    Printf.printf ")\n"
  }
| Parser.PList xs -> do
  {
    Printf.printf " [";
    print_list print_p_pat "," xs;
    Printf.printf "]\n"
  }
| Parser.PListTail xs tl -> do
  {
    Printf.printf " [";
    print_list print_p_pat "," xs;
    Printf.printf " : ";
    print_p_pat tl;
    Printf.printf "]\n"
  }
];
*)

value rec print_partial x = match x with
[ Unbound         -> Printf.printf " <unbound>"
| Constraint _    -> Printf.printf " <contraint>"
| Bool b          -> Printf.printf "%s" (if b then " True" else " False")
| Number n        -> Printf.printf " %f" (float_of_num n)
| Char c          -> Printf.printf " '\\x%x'" c
| Symbol s        -> Printf.printf " %s" (UString.to_string (Array.to_list (symbol_to_string s)))
| LinForm lin     -> do
  {
    Printf.printf " (%f" (float_of_num lin.LinForm.const);
    List.iter
      (fun (b,x) -> do
        {
          Printf.printf " + %f *" (float_of_num b);
          print_partial !x
        })
      lin.LinForm.terms;
    Printf.printf ")"
  }
| UnevalT _ _     -> Printf.printf " <unevaluated>"
| Application _ _ -> Printf.printf " <unevaluated>"
| Primitive1 _    -> Printf.printf " <primitive>"
| Primitive2 _    -> Printf.printf " <primitive>"
| PrimitiveN _ _  -> Printf.printf " <primitive>"
| SimpleFunction _ _ _      -> Printf.printf " <sfun>"
| PatternFunction _ _ _ _ _ -> Printf.printf " <pfun>"
| Chain _                   -> Printf.printf " <cfun>"
| Relation _ _ -> Printf.printf " <rel>"
| Nil -> Printf.printf " []"
| List a b -> do
  {
    Printf.printf " [";
    print_partial !a;

    iter b

    where rec iter x = match !x with
    [ Nil -> Printf.printf "]"
    | List a b -> do
      {
        Printf.printf ",";
        print_partial !a;
        iter b
      }
    | _ -> do
      {
        Printf.printf " :";
        print_partial !x
      }
    ]
  }
| Tuple y -> do
  {
    Printf.printf " (";
    print_list
      (fun a -> print_partial !a)
      ","
      (Array.to_list y);
    Printf.printf ")"
  }
| Dictionary _ -> do
  {
    Printf.printf " <dict>"
  }
| Opaque _ -> Printf.printf " <opaque>"
];

(*
value rec print_term t = match t with
[ TConstant c -> do
  {
    Printf.printf " <const";
    print_partial c;
    Printf.printf ">"
  }
| TGlobal x -> do
  {
    Printf.printf " <gobal>"
  }
| TVariable l i -> Printf.printf " <var %d %d>" l i
| TConsTuple xs -> do
  {
    Printf.printf " (";
    Array.iter
      (fun a -> do
        {
          print_term a;
          Printf.printf ",";
        })
        xs
  }
| TConsList x y -> do
  {
    Printf.printf " (";
    print_term x;
    Printf.printf " :";
    print_term y;
    Printf.printf ")"
  }
| TApplication f args -> do
  {
    print_term f;
    List.iter print_term args
  }
| TSimpleFunction a body -> do
  {
    Printf.printf " fun/%d ->" a;
    print_term body
  }
| TPatternFunction a s n ps -> do
  {
    Printf.printf " fun(%d,%d,%d) ->" a s n;

  }
| TIfThenElse p e0 e1 -> do
  {
    Printf.printf " if";
    print_term p;
    Printf.printf " then";
    print_term e0;
    Printf.printf " else";
    print_term e1;
    Printf.printf " end";
  }
| TLocalScope d x -> do
  {
    Printf.printf " local/%d" (Array.length d);
    print_term x;
  }
| TSequence s e -> do
  {
    Printf.printf " begin";
    (*List.iter (fun s -> do { print_stmt s; Printf.printf "," }) s;*)
    print_term e;
    Printf.printf " end"
  }
| TMatch x s n ps -> do
  {
    Printf.printf " match(%d,%d)" s n;
    print_term x;
    Printf.printf " with";

  }
];
*)

(*
value print_scope s = do
{
  Hashtbl.iter
    (fun n d -> do
      {
        Printf.printf "global %s =" (UString.to_string (symbol_to_string n));
        print_partial !d;
        Printf.printf "\n"
      })
    s.Scope.global_symbols;

  Hashtbl.iter
    (fun n (l, i) ->
      Printf.printf "local %s = <var %d %d>\n"
        (UString.to_string (symbol_to_string n))
        l i)
    s.Scope.local_symbols
};
*)

value main () = do
{
  Unicode.UString.set_string_format `UTF8;

  print_string "This is ali, version 0.1.\nIf you need help, please type \"quit\".\n";
  flush stdout;

  try do
  {
    let scope = Primitives.initial_scope () in

    for i = 1 to Array.length Sys.argv - 1 do
    {
      print_string "Loading ";
      print_string Sys.argv.(i);
      print_string "...";
      flush stdout;
      Compile.compile_declarations scope (UCStream.of_file Sys.argv.(i));
      print_string " done\n";
    };

    iter ()

    where rec iter () = do
    {
      print_string "> ";
      flush stdout;

      let expr = read_line () in

      if expr = "quit" then
        ()
      else do
      {
        try do
        {
          let e = Compile.compile_expression scope (UCStream.of_list (UString.of_string expr)) in

          let result = ref (UnevalT [] e) in
          Machine.evaluate result;

          print_partial !result;
          print_newline ();
        }
        with
        [ Syntax_error (f,l,c) err ->
            Printf.printf "\n%s:%d:%d syntax error: %s\n" f l c (UString.to_string (Array.to_list err))
        | Runtime_error err -> Printf.printf "\nruntime error: %s\n" (UString.to_string (Array.to_list err))
        ];

        iter ()
      }
    };
  }
  with
  [ Syntax_error (f,l,c) err ->
      Printf.printf "\n%s:%d:%d syntax error: %s\n" f l c (UString.to_string (Array.to_list err))
  | Runtime_error err -> Printf.printf "\nruntime error: %s\n" (UString.to_string (Array.to_list err))
  ];

  flush stderr
};

main ();

