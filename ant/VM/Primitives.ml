
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)
module UChar   = Unicode.UChar;

value rec uc_list_to_char_list str = match str with
[ []      -> Nil
| [c::cs] -> List (ref (Char c)) (ref (uc_list_to_char_list cs))
];

value uc_string_to_char_list str = do
{
  iter (Array.length str - 1) Nil

  where rec iter i list = do
  {
    if i < 0 then
      list
    else
      iter (i-1) (List (ref (Char str.(i))) (ref list))
  }
};

value ascii_to_char_list str = do
{
  uc_string_to_char_list (UString.uc_string_of_ascii str)
};

value rec evaluate_char_list name x = match !x with
[ Nil      -> []
| List a b -> do
  {
    Evaluate.evaluate a;

    match !a with
    [ Char c -> [c :: evaluate_char_list name b]
    | _      -> runtime_error (name ^ ": invalid argument")
    ]
  }
| UnevalT _ _  -> do { Evaluate.evaluate x; evaluate_char_list name x }
| Unbound
| Constraint _ -> runtime_error (name ^ ": argument undefined")
| _            -> runtime_error (name ^ ": invalid argument")
];

(* control *)

value prim_error msg = do
{
  let str = try
    Array.of_list (evaluate_char_list "error" msg)
  with
  [ Runtime_error x ->
     raise (Runtime_error (UString.append (UString.uc_string_of_ascii "error: ") x))
  ]
  in

  raise (Runtime_error str)
};


(* logic *)

value rec prim_or x y = match (!x, !y) with
[ (Bool a, Bool b)         -> Bool (a || b)
| (UnevalT _ _, Bool True) -> Bool True
| (Bool True, UnevalT _ _) -> Bool True
| (UnevalT e t, _)         -> do { Evaluate.evaluate x; prim_or x y }
| (_, UnevalT e t)         -> do { Evaluate.evaluate y; prim_or x y }
| _                        -> runtime_error "||: invalid argument"
];

value rec prim_and x y = match (!x, !y) with
[ (Bool a, Bool b)          -> Bool (a && b)
| (UnevalT _ _, Bool False) -> Bool False
| (Bool False, UnevalT _ _) -> Bool False
| (UnevalT e t, _)          -> do { Evaluate.evaluate x; prim_and x y }
| (_, UnevalT e t)          -> do { Evaluate.evaluate y; prim_and x y }
| _                         -> runtime_error "&&: invalid argument"
];

value rec prim_not x = match !x with
[ Bool a          -> Bool (not a)
| UnevalT e t     -> do { Evaluate.evaluate x; prim_not x }
| _               -> runtime_error "not: invalid argument"
];

(* comparisons *)

value rec cmp x y = match (!x, !y) with
[ (UnevalT _ _, _)   -> do { Evaluate.evaluate x ; cmp x y }
| (_, UnevalT _ _)   -> do { Evaluate.evaluate y ; cmp x y }
| (Unbound, _)       -> runtime_error "argument unbound during comparison"
| (_, Unbound)       -> runtime_error "argument unbound during comparison"
| (Constraint a, Constraint b) -> a == b
| (Bool a,       Bool b)       -> a = b
| (Char a,       Char b)       -> a = b
| (Symbol a,     Symbol b)     -> a = b
| (Nil,          Nil)          -> True
| (List a1 a2,   List b1 b2)   -> cmp a1 b1 && cmp a2 b2
| (Number a,     Number b)     -> a =/ b
| (Number a,     LinForm lin)  -> do
    {
      Evaluate.evaluate_lin_form y lin;

      match !y with
      [ Number b -> b =/ a
      | _        -> False
      ]
    }
| (LinForm lin, Number a) -> do
    {
      Evaluate.evaluate_lin_form x lin;

      match !x with
      [ Number b -> b =/ a
      | _        -> False
      ]
    }
| (LinForm a, LinForm b) -> do
    {
      Evaluate.evaluate_lin_form x a;
      Evaluate.evaluate_lin_form y b;

      let l = LinForm.lin_comb num_one a (minus_num num_one) b in
      let z = ref (LinForm l)                                  in

      Evaluate.evaluate_lin_form z l;

      match !z with
      [ Number c -> c =/ num_zero
      | _        -> False
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
        (i >= Array.length a) || (cmp a.(i) b.(i) && iter (i+1))
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
         [(k1, v1) :: kv1]) -> k0 = k1 && cmp v0 v1 && iter kv0 kv1
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
| _                                    -> False
];

value prim_eq  x y = Bool (cmp x y);
value prim_neq x y = Bool (not (cmp x y));

value rec prim_gt x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m >/ n)
| (Char a,   Char b)   -> Bool (a > b)
| (Nil, Nil)           -> Bool False
| (Nil, List _ _)      -> Bool False
| (List _ _, Nil)      -> Bool True
| (List a b, List c d) -> match prim_gt a c with
                          [ Bool True  -> Bool True
                          | _          -> if cmp a b then prim_gt b d else Bool False
                          ]
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error ">: invalid argument"
    else
      Bool (iter 0)

    where rec iter i = do
    {
      if i >= Array.length xs then
        False
      else match prim_gt xs.(i) ys.(i) with
      [ Bool True -> True
      | _         -> cmp xs.(i) ys.(i) && iter (i+1)
      ]
    }
  }
| (UnevalT e t, _) -> do { Evaluate.evaluate x; prim_gt x y }
| (_, UnevalT e t) -> do { Evaluate.evaluate y; prim_gt x y }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_gt x y
    | _        -> runtime_error (">: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> prim_gt x y
    | _        -> runtime_error (">: invalid argument")
    ]
  }
| _ -> runtime_error ">: invalid argument"
];

value rec prim_lt x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m </ n)
| (Char a,   Char b)   -> Bool (a < b)
| (Nil, Nil)           -> Bool False
| (Nil, List _ _)      -> Bool True
| (List _ _, Nil)      -> Bool False
| (List a b, List c d) -> match prim_lt a c with
                          [ Bool True  -> Bool True
                          | _          -> if cmp a b then prim_lt b d else Bool False
                          ]
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "<: invalid argument"
    else
      Bool (iter 0)

    where rec iter i = do
    {
      if i >= Array.length xs then
        False
      else match prim_lt xs.(i) ys.(i) with
      [ Bool True -> True
      | _         -> cmp xs.(i) ys.(i) && iter (i+1)
      ]
    }
  }
| (UnevalT e t, _) -> do { Evaluate.evaluate x; prim_lt x y }
| (_, UnevalT e t) -> do { Evaluate.evaluate y; prim_lt x y }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_lt x y
    | _        -> runtime_error ("<: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> prim_lt x y
    | _        -> runtime_error ("<: invalid argument")
    ]
  }
| _ -> runtime_error "<: invalid argument"
];

value rec prim_ge x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m >=/ n)
| (Char a,   Char b)   -> Bool (a >= b)
| (Nil, Nil)           -> Bool True
| (Nil, List _ _)      -> Bool False
| (List _ _, Nil)      -> Bool True
| (List a b, List c d) -> match prim_gt a c with
                          [ Bool True  -> Bool True
                          | _          -> if cmp a b then prim_ge b d else Bool False
                          ]
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error ">=: invalid argument"
    else
      Bool (iter 0)

    where rec iter i = do
    {
      if i >= Array.length xs then
        True
      else match prim_gt xs.(i) ys.(i) with
      [ Bool True -> True
      | _         -> cmp xs.(i) ys.(i) && iter (i+1)
      ]
    }
  }
| (UnevalT e t, _) -> do { Evaluate.evaluate x; prim_ge x y }
| (_, UnevalT e t) -> do { Evaluate.evaluate y; prim_ge x y }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_ge x y
    | _        -> runtime_error (">=: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> prim_ge x y
    | _        -> runtime_error (">=: invalid argument")
    ]
  }
| _ -> runtime_error ">=: invalid argument"
];

value rec prim_le x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m <=/ n)
| (Char a,   Char b)   -> Bool (a <= b)
| (Nil, Nil)           -> Bool True
| (Nil, List _ _)      -> Bool True
| (List _ _, Nil)      -> Bool False
| (List a b, List c d) -> match prim_lt a c with
                          [ Bool True  -> Bool True
                          | _          -> if cmp a b then prim_le b d else Bool False
                          ]
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "<=: invalid argument"
    else
      Bool (iter 0)

    where rec iter i = do
    {
      if i >= Array.length xs then
        True
      else match prim_lt xs.(i) ys.(i) with
      [ Bool True -> True
      | _         -> cmp xs.(i) ys.(i) && iter (i+1)
      ]
    }
  }
| (UnevalT e t, _) -> do { Evaluate.evaluate x; prim_le x y }
| (_, UnevalT e t) -> do { Evaluate.evaluate y; prim_le x y }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_le x y
    | _        -> runtime_error ("<=: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> prim_le x y
    | _        -> runtime_error ("<=: invalid argument")
    ]
  }
| _ -> runtime_error "<=: invalid argument"
];

value prim_min x y = match prim_le x y with
[ Bool True -> !x
| _         -> !y
];

value prim_max x y = match prim_ge x y with
[ Bool True -> !x
| _         -> !y
];

(* general arithmetic *)

value rec unary_number_prim f name x = match !x with
[ Number n    -> try Number (f n) with
                 [ _ -> runtime_error (name ^ ": invalid argument") ]
| UnevalT _ _ -> do { Evaluate.evaluate x; unary_number_prim f name x }
| LinForm l   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number n -> try Number (f n) with
                  [ _ -> runtime_error (name ^ ": invalid argument") ]
    | _        -> runtime_error (name ^ ": invalid argument")
    ]
  }
| _ -> runtime_error (name ^ ": invalid argument")
];

value rec binary_number_prim f name x y = match (!x, !y) with
[ (Number m, Number n) -> try Number (f m n) with
                          [ _ -> runtime_error (name ^ ": invalid argument") ]
| (UnevalT e t, _)     -> do { Evaluate.evaluate x; binary_number_prim f name x y }
| (_, UnevalT e t)     -> do { Evaluate.evaluate y; binary_number_prim f name x y }
| (LinForm l, _) -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> binary_number_prim f name x y
    | _        -> runtime_error (name ^ ": invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> binary_number_prim f name x y
    | _        -> runtime_error (name ^ ": invalid argument")
    ]
  }
| _ -> runtime_error (name ^ ": invalid argument")
];

value prim_quot = binary_number_prim quo_num "quot";
value prim_mod  = binary_number_prim mod_num "mod";
value prim_pow  = binary_number_prim
                    (fun x y -> if is_integer_num y then
                                  power_num x y
                                else
                                  num_of_float (float_of_num x ** float_of_num y))
                    "^";

value rec prim_negate x = match !x with
[ Number n    -> Number (minus_num n)
| LinForm l   -> LinForm (LinForm.scale num_minus_one l)
| Unbound     -> LinForm (LinForm.of_scaled_unknown compare_unknowns num_minus_one x)
| Tuple xs    -> Tuple
                   (Array.init
                     (Array.length xs)
                     (fun i -> ref (prim_negate xs.(i))))
| UnevalT e t -> do { Evaluate.evaluate x; prim_negate x }
| _           -> runtime_error "~: invalid argument"
];

(* integer arithmetic *)

value prim_round    = unary_number_prim round_num   "round";
value prim_truncate = unary_number_prim integer_num "trunacate";
value prim_ceiling  = unary_number_prim ceiling_num "ceiling";
value prim_floor    = unary_number_prim floor_num   "floor";

value prim_land = binary_number_prim land_num "land";
value prim_lor  = binary_number_prim lor_num  "lor";
value prim_lxor = binary_number_prim lxor_num "lxor";
value prim_lneg = unary_number_prim  lneg_num "lneg";

value num_two = num_of_int 2;

value prim_lsr = binary_number_prim (fun m n -> m // power_num num_two n) "lsr";
value prim_lsl = binary_number_prim (fun m n -> m */ power_num num_two n) "lsl";

(* "real" arithmetic *)

value pi      = 4.0 *. atan 1.0;
value pi_inv  = 1.0 /. pi;
value num_180 = num_of_int 180;

value float_wrapper f x = num_of_float (f (float_of_num x));

value sind x    = num_of_float (sin (pi *. float_of_num (x // num_180)));
value cosd x    = num_of_float (cos (pi *. float_of_num (x // num_180)));
value tand x    = num_of_float (tan (pi *. float_of_num (x // num_180)));
value arcsind x = num_of_float (pi_inv *. asin (float_of_num x)) */ num_180;
value arccosd x = num_of_float (pi_inv *. acos (float_of_num x)) */ num_180;
value arctand x = num_of_float (pi_inv *. atan (float_of_num x)) */ num_180;

value arcsinh x = log (x +. sqrt(x *. x +. 1.0));
value arccosh x = log (x +. sqrt(x *. x -. 1.0));
value arctanh x = 0.5 *. (log (1.0 +. x) -. log (1.0 -. x));

value prim_sqrt    = unary_number_prim (float_wrapper sqrt)    "sqrt";
value prim_exp     = unary_number_prim (float_wrapper exp)     "exp";
value prim_log     = unary_number_prim (float_wrapper log)     "log";
value prim_sin     = unary_number_prim (float_wrapper sin)     "sin";
value prim_cos     = unary_number_prim (float_wrapper cos)     "cos";
value prim_tan     = unary_number_prim (float_wrapper tan)     "tan";
value prim_arcsin  = unary_number_prim (float_wrapper asin)    "arcsin";
value prim_arccos  = unary_number_prim (float_wrapper acos)    "arccos";
value prim_arctan  = unary_number_prim (float_wrapper atan)    "arctan";
value prim_sind    = unary_number_prim sind                    "sind";
value prim_cosd    = unary_number_prim cosd                    "cosd";
value prim_tand    = unary_number_prim tand                    "tand";
value prim_arcsind = unary_number_prim arcsind                 "arcsind";
value prim_arccosd = unary_number_prim arcsind                 "arccosd";
value prim_arctand = unary_number_prim arcsind                 "arctand";
value prim_sinh    = unary_number_prim (float_wrapper sinh)    "sinh";
value prim_cosh    = unary_number_prim (float_wrapper cosh)    "cosh";
value prim_tanh    = unary_number_prim (float_wrapper tanh)    "tanh";
value prim_arcsinh = unary_number_prim (float_wrapper arcsinh) "arcsinh";
value prim_arccosh = unary_number_prim (float_wrapper arccosh) "arccosh";
value prim_arctanh = unary_number_prim (float_wrapper arctanh) "arctanh";

value rec prim_abs x = match !x with
[ Number n    -> Number (abs_num n)
| UnevalT _ _ -> do { Evaluate.evaluate x; prim_abs x }
| LinForm l   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number n -> Number (abs_num n)
    | _        -> runtime_error "abs: invalid argument"
    ]
  }
| Tuple xs -> do
  {
    Number
      (float_wrapper sqrt
        (Array.fold_left
           (fun a b -> match Evaluate.mul_unknowns b b with
            [ Number c -> a +/ c
            | _        -> runtime_error "abs: invalid argument"
            ])
           num_zero
           xs))
  }
| _ -> runtime_error "abs: invalid argument"
];

(* lists *)

value prim_length x = do
{
  Number (num_of_int (len x))

  where rec len x = match !x with
  [ Nil          -> 0
  | List _ a     -> 1 + len a
  | Tuple xs     -> Array.length xs
  | Dictionary d -> SymbolMap.fold (fun _ _ n -> n + 1) d 0
  | UnevalT _ _  -> do { Evaluate.evaluate x; len x }
  | Unbound
  | Constraint _ -> runtime_error "length: argument undefined"
  | _            -> 1
  ]
};

(* FIX: tuples and lists *)

value rec prim_to_string x = match !x with
[ Number n -> do
  {
    if n </ num_zero then
      List (ref (Char 126)) (ref (ascii_to_char_list (string_of_num (minus_num n))))
    else do
    {
      let str = string_of_num n in

      (* If |n| is an integer we remove the suffix "/1". *)

      if str.[String.length str - 2] = '/' &&
         str.[String.length str - 1] = '1' then
        ascii_to_char_list (String.sub str 0 (String.length str - 2))
      else
        ascii_to_char_list str
    }
  }
| Bool b    -> if b then ascii_to_char_list "True" else ascii_to_char_list "False"
| Char c    -> List x (ref Nil)
| Symbol s  -> uc_string_to_char_list (symbol_to_string s)
| Nil       -> ascii_to_char_list "[]"
| LinForm l -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number n -> prim_to_string x
    | _        -> runtime_error "to_string: argument undefined"
    ]
  }
| UnevalT _ _  -> do { Evaluate.evaluate x; prim_to_string x }
| Unbound
| Constraint _ -> runtime_error "to_string: argument undefined"
| _            -> runtime_error "to_string: invalid argument"
];

value prim_to_tuple x = do
{
  let lst = Evaluate.evaluate_list "to_tuple" x in

  Tuple (Array.of_list lst)
};

value rec prim_to_list x = match !x with
[ Tuple xs     -> Array.fold_right
                    (fun a b -> List a (ref b))
                    xs
                    Nil
| UnevalT _ _  -> do { Evaluate.evaluate x; prim_to_list x }
| Unbound
| Constraint _ -> runtime_error "to_list: argument undefined"
| _            -> runtime_error "to_list: invalid argument"
];

value rec unary_vec2_prim f name x = match !x with
[ Tuple [| a; b |] -> do
  {
    Evaluate.evaluate a;
    Evaluate.evaluate b;

    match (!a, !b) with
    [ (Number n, Number m) ->
        try f n m with
        [ _ -> runtime_error (name ^ ": invalid argument") ]
    | _ -> runtime_error (name ^ ": invalid argument")
    ]
  }
| UnevalT _ _ -> do { Evaluate.evaluate x; unary_vec2_prim f name x }
| LinForm l   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Tuple _ -> unary_vec2_prim f name x
    | _       -> runtime_error (name ^ ": invalid argument")
    ]
  }
| _ -> runtime_error (name ^ ": invalid argument")
];

value rec prim_dir x = do
{
  Tuple [| ref (prim_cosd x); ref (prim_sind x) |]
};

value prim_angle = unary_vec2_prim
  (fun x y -> match sign_num x with
    [  1 -> Number (arctand (y // x))
    |  0 -> if y >/ num_zero then
              Number (90 /: 1)
            else if y =/ num_zero then
              runtime_error "angle: invalid argument"
            else
              Number (270 /: 1)
    | -1 -> Number (180 /: 1 -/ arctand (minus_num y // x))
    | _  -> assert False
    ])
  "angle";

value rec prim_rotate a = match !a with
[ Number n    -> unary_vec2_prim
                   (fun x y -> do
                     {
                       let x2 = cosd n */ x -/ sind n */ y in
                       let y2 = sind n */ x +/ cosd n */ y in

                       Tuple [| ref (Number x2); ref (Number y2) |]
                     })
                   "rotate"
| UnevalT _ _ -> do { Evaluate.evaluate a; prim_rotate a }
| LinForm l   -> do
  {
    Evaluate.evaluate_lin_form a l;

    match !a with
    [ Number _ -> prim_rotate a
    | _        -> runtime_error "rotate: invalid argument"
    ]
  }
| _ -> runtime_error "rotate: invalid argument"
];

(* characters *)

value rec unary_char_prim f name x = match !x with
[ Char n      -> try f n with
                 [ _ -> runtime_error (name ^ ": invalid argument") ]
| UnevalT _ _ -> do { Evaluate.evaluate x; unary_char_prim f name x }
| _           -> runtime_error (name ^ ": invalid argument")
];

value prim_is_letter    = unary_char_prim (fun c -> Bool (UChar.is_letter c))    "is_letter";
value prim_is_mark      = unary_char_prim (fun c -> Bool (UChar.is_mark c))      "is_mark";
value prim_is_number    = unary_char_prim (fun c -> Bool (UChar.is_number c))    "is_number";
value prim_is_punct     = unary_char_prim (fun c -> Bool (UChar.is_punct c))     "is_punct";
value prim_is_symbol    = unary_char_prim (fun c -> Bool (UChar.is_symbol c))    "is_symbol";
value prim_is_separator = unary_char_prim (fun c -> Bool (UChar.is_separator c)) "is_separator";
value prim_is_control   = unary_char_prim (fun c -> Bool (UChar.is_control c))   "is_control";
value prim_is_space     = unary_char_prim (fun c -> Bool (UChar.is_space c))     "is_space";
value prim_to_upper     = unary_char_prim (fun c -> Char (UChar.to_upper c))     "to_upper";
value prim_to_lower     = unary_char_prim (fun c -> Char (UChar.to_lower c))     "to_lower";
value prim_to_title     = unary_char_prim (fun c -> Char (UChar.to_title c))     "to_title";
value prim_char_name    = unary_char_prim (fun c -> ascii_to_char_list (UChar.name c))  "char_name";

value symbol_Lu = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lu"));
value symbol_Ll = Symbol (string_to_symbol (UString.uc_string_of_ascii "Ll"));
value symbol_Lt = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lt"));
value symbol_Lm = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lm"));
value symbol_Lo = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lo"));
value symbol_Mn = Symbol (string_to_symbol (UString.uc_string_of_ascii "Mn"));
value symbol_Mc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Mc"));
value symbol_Me = Symbol (string_to_symbol (UString.uc_string_of_ascii "Me"));
value symbol_Nd = Symbol (string_to_symbol (UString.uc_string_of_ascii "Nd"));
value symbol_Nl = Symbol (string_to_symbol (UString.uc_string_of_ascii "Nl"));
value symbol_No = Symbol (string_to_symbol (UString.uc_string_of_ascii "No"));
value symbol_Pc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pc"));
value symbol_Pd = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pd"));
value symbol_Ps = Symbol (string_to_symbol (UString.uc_string_of_ascii "Ps"));
value symbol_Pe = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pe"));
value symbol_Pi = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pi"));
value symbol_Pf = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pf"));
value symbol_Po = Symbol (string_to_symbol (UString.uc_string_of_ascii "Po"));
value symbol_Sm = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sm"));
value symbol_Sc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sc"));
value symbol_Sk = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sk"));
value symbol_So = Symbol (string_to_symbol (UString.uc_string_of_ascii "So"));
value symbol_Zs = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zs"));
value symbol_Zl = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zl"));
value symbol_Zp = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zp"));
value symbol_Cc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cc"));
value symbol_Cf = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cf"));
value symbol_Cs = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cs"));
value symbol_Co = Symbol (string_to_symbol (UString.uc_string_of_ascii "Co"));
value symbol_Cn = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cn"));

value prim_char_category =
  unary_char_prim
    (fun c -> match UChar.category c with
     [ UChar.Lu -> symbol_Lu
     | UChar.Ll -> symbol_Ll
     | UChar.Lt -> symbol_Lt
     | UChar.Lm -> symbol_Lm
     | UChar.Lo -> symbol_Lo
     | UChar.Mn -> symbol_Mn
     | UChar.Mc -> symbol_Mc
     | UChar.Me -> symbol_Me
     | UChar.Nd -> symbol_Nd
     | UChar.Nl -> symbol_Nl
     | UChar.No -> symbol_No
     | UChar.Pc -> symbol_Pc
     | UChar.Pd -> symbol_Pd
     | UChar.Ps -> symbol_Ps
     | UChar.Pe -> symbol_Pe
     | UChar.Pi -> symbol_Pi
     | UChar.Pf -> symbol_Pf
     | UChar.Po -> symbol_Po
     | UChar.Sm -> symbol_Sm
     | UChar.Sc -> symbol_Sc
     | UChar.Sk -> symbol_Sk
     | UChar.So -> symbol_So
     | UChar.Zs -> symbol_Zs
     | UChar.Zl -> symbol_Zl
     | UChar.Zp -> symbol_Zp
     | UChar.Cc -> symbol_Cc
     | UChar.Cf -> symbol_Cf
     | UChar.Cs -> symbol_Cs
     | UChar.Co -> symbol_Co
     | UChar.Cn -> symbol_Cn
     ])
    "char_category";

value prim_to_symbol x = do
{
  let str = evaluate_char_list "to_symbol" x in

  Symbol (string_to_symbol (Array.of_list str))
};

value prim_generate_symbol _ = do
{
  Symbol (alloc_symbol ())
};

value bind_primitive scope name v = do
{
  Scope.add_global scope (string_to_symbol (UString.uc_string_of_ascii name)) v
};

value initial_scope () = do
{
  let scope = Scope.create () in

  let add = bind_primitive scope in

  let add1 name f   = bind_primitive scope name (Primitive1 f)   in
  let add2 name f   = bind_primitive scope name (Primitive2 f)   in
  let addn name n f = bind_primitive scope name (PrimitiveN n f) in

  (* control *)

  add1 "error"    prim_error;

  (* logical operators *)

  add2 "||"       prim_or;
  add2 "&&"       prim_and;
  add1 "not"      prim_not;

  (* comparisons *)

  add2 "=="       prim_eq;
  add2 "<>"       prim_neq;
  add2 ">"        prim_gt;
  add2 "<"        prim_lt;
  add2 ">="       prim_ge;
  add2 "<="       prim_le;
  add2 "min"      prim_min;
  add2 "max"      prim_max;

  (* general arithmetic *)

  add2 "+"        Evaluate.add_unknowns;
  add2 "-"        Evaluate.sub_unknowns;
  add2 "*"        Evaluate.mul_unknowns;
  add2 "/"        Evaluate.div_unknowns;
  add2 "^"        prim_pow;
  add2 "quot"     prim_quot;
  add2 "mod"      prim_mod;
  add1 "~"        prim_negate;
  add1 "abs"      prim_abs;

  (* integer arithmetic *)

  add1 "round"    prim_round;
  add1 "truncate" prim_truncate;
  add1 "ceiling"  prim_ceiling;
  add1 "floor"    prim_floor;
  add2 "land"     prim_land;
  add2 "lor"      prim_lor;
  add2 "lxor"     prim_lxor;
  add2 "lneg"     prim_lxor;
  add2 "lsr"      prim_lsr;
  add2 "lsl"      prim_lsl;

  (* "real" arithmetic *)

  add  "pi"  (Number (num_of_float pi));

  add1 "sqrt"     prim_sqrt;
  add1 "exp"      prim_exp;
  add1 "log"      prim_log;
  add1 "sin"      prim_sin;
  add1 "cos"      prim_cos;
  add1 "tan"      prim_tan;
  add1 "arcsin"   prim_arcsin;
  add1 "arccos"   prim_arccos;
  add1 "arctan"   prim_arctan;
  add1 "sind"     prim_sind;
  add1 "cosd"     prim_cosd;
  add1 "tand"     prim_tand;
  add1 "arcsind"  prim_arcsind;
  add1 "arccosd"  prim_arccosd;
  add1 "arctand"  prim_arctand;
  add1 "sinh"     prim_sinh;
  add1 "cosh"     prim_cosh;
  add1 "tanh"     prim_tanh;
  add1 "arcsinh"  prim_arcsinh;
  add1 "arccosh"  prim_arccosh;
  add1 "arctanh"  prim_arctanh;

  (* lists and tuples *)

  add1 "length"    prim_length;
  add1 "to_string" prim_to_string;
  add1 "to_list"   prim_to_list;
  add1 "to_tuple"  prim_to_tuple;
  add1 "dir"       prim_dir;
  add1 "angle"     prim_angle;
  add2 "rotate"    prim_rotate;

  (* characters *)

  add1 "is_letter"     prim_is_letter;
  add1 "is_mark"       prim_is_mark;
  add1 "is_number"     prim_is_number;
  add1 "is_punct"      prim_is_punct;
  add1 "is_symbol"     prim_is_symbol;
  add1 "is_separator"  prim_is_separator;
  add1 "is_control"    prim_is_control;
  add1 "is_space"      prim_is_space;
  add1 "to_upper"      prim_to_upper;
  add1 "to_lower"      prim_to_lower;
  add1 "to_title"      prim_to_title;
  add1 "char_name"     prim_char_name;
  add1 "char_category" prim_char_category;

  (* symbols *)

  add1 "to_symbol"       prim_to_symbol;
  add1 "generate_symbol" prim_generate_symbol;

  (* dimensions *)

  add "pt" (Number num_one);
  add "bp" (Number ( 7227 /:  7200));
  add "cc" (Number (14856 /:  1157));
  add "cm" (Number ( 7227 /:   254));
  add "dd" (Number ( 1238 /:  1157));
  add "in" (Number ( 7227 /:   100));           (* FIX: this keyword is reserved! *)
  add "mm" (Number ( 7227 /:  2540));
  add "pc" (Number (   12 /:     1));
  add "sp" (Number (    1 /: 65536));

  scope
};

(*
  char:   ucode_of_char, char_of_ucode
  string: xxx_to_string, string_to_xxx
  list:   head, tail, reverse, append, concat, flatten, map, foldl, foldr, member,
          find, filter, partition, assoc, mem_assoc, remove_assoc, zip, unzip, sort,
          sub_list
  path:   point t of p, tangent t of p, concat, .., --, {xx}..{xx},
          flex [z_1,...,z_n]
*)

