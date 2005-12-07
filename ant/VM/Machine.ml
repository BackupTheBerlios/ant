
open Types;
open VMPrivate;
open Runtime;

(* scopes *)

type scope = Scope.scope;

value make_scope     = Primitives.initial_scope;
value bind_primitive = Primitives.bind_primitive;

(* symbols *)

value string_to_symbol = Unicode.SymbolTable.string_to_symbol;
value symbol_to_string = Unicode.SymbolTable.symbol_to_string;

(* evaluation *)

value uc_string_to_char_list = Primitives.uc_string_to_char_list;
value uc_list_to_char_list   = Primitives.uc_list_to_char_list;
value ascii_to_char_list     = Primitives.ascii_to_char_list;

value evaluate_list          = Evaluate.evaluate_list;
value evaluate_string        = Primitives.evaluate_char_list;
value execute_declarations   = Compile.compile_declarations;
value evaluate               = Evaluate.evaluate;
value evaluate_lin_form      = Evaluate.evaluate_lin_form;
value unify                  = Evaluate.unify;

value set_unknown x v = Evaluate.forced_unify x (ref v);

value evaluate_expression scope stream = do
{
  let e = Compile.compile_expression scope stream in

  let result = ref (UnevalT [] e) in

  Evaluate.evaluate result;

  !result
};

value evaluate_string_expr name scope stream = do
{
  let term = Compile.compile_expression scope stream in

  Primitives.evaluate_char_list name (ref (UnevalT [] term))
};

value evaluate_monad_expr scope stream init = do
{
  let term = Compile.compile_expression scope stream in

  let result = ref (UnevalT [] (TApplication term [TConstant init])) in

  Evaluate.evaluate result;

  !result
};

value evaluate_function _name f args = do
{
  let result = ref (UnevalT [] (TApplication
                                  (TGlobal f)
                                  (List.map (fun a -> TGlobal a) args)))
  in

  Evaluate.evaluate result;

  result
};

value evaluate_num name x = do
{
  evaluate x;

  match !x with
  [ Number n  -> n
  | LinForm l -> do
    {
      evaluate_lin_form x l;

      match !x with
      [ Number n -> n
      | _        -> runtime_error (name ^ ": number expected")
      ]
    }
  | _ -> runtime_error (name ^ ": number expected")
  ]
};

