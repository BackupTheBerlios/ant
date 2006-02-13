
open XNum;
open Types;
open VMPrivate;
open Runtime;

(* scopes *)

type scope = Scope.scope;

value make_scope     = Primitives.initial_scope;
value bind_primitive = Primitives.bind_primitive;
value bind_bin_op_l  = Primitives.bind_bin_op_l;
value bind_bin_op_n  = Primitives.bind_bin_op_n;
value bind_bin_op_r  = Primitives.bind_bin_op_r;
value bind_pre_op    = Primitives.bind_pre_op;
value bind_post_op   = Primitives.bind_post_op;

(* symbols *)

value string_to_symbol = Unicode.SymbolTable.string_to_symbol;
value symbol_to_string = Unicode.SymbolTable.symbol_to_string;

(* evaluation *)

value uc_string_to_char_list = Primitives.uc_string_to_char_list;
value uc_list_to_char_list   = Primitives.uc_list_to_char_list;
value ascii_to_char_list     = Primitives.ascii_to_char_list;

value execute_declarations   = Compile.compile_declarations;
value evaluate_unknown       = Evaluate.evaluate_unknown;
value evaluate_lin_form      = Evaluate.evaluate_lin_form;
value evaluate_num           = Evaluate.evaluate_num;
value evaluate_opaque        = Evaluate.evaluate_opaque;
value unify                  = Evaluate.unify;
value continue               = CStack.cont;
value continue2              = CStack.cont2;
value continue3              = CStack.cont3;
value continue4              = CStack.cont4;

value set_unknown x v = Evaluate.forced_unify x (ref v);

value evaluate x = do
{
  CStack.start_vm ();

  Evaluate.evaluate_unknown x;

  CStack.end_vm ()
};

value evaluate_expression scope stream = do
{
  let e = Compile.compile_expression scope stream in

  let result = ref (UnevalT [] e) in

  evaluate result;

  !result
};

value evaluate_string_expr name scope stream = do
{
  let term = Compile.compile_expression scope stream in
  let lst  = ref [] in
  let x    = ref (UnevalT [] term) in

  CStack.start_vm ();

  CStack.cont2
    (fun () -> Evaluate.evaluate_unknown x)
    (fun () -> Primitives.evaluate_char_list name lst x);

  CStack.end_vm ();

  !lst
};

value evaluate_monad_expr scope stream init = do
{
  let term = Compile.compile_expression scope stream in

  let result = ref (UnevalT [] (TApplication term [TConstant init])) in

  evaluate result;

  !result
};

value decode_function _name f args = do
{
  let result = ref Unbound in

  CStack.start_vm ();

  CStack.cont2
    (fun () -> Evaluate.evaluate_unknown f)
    (fun () -> Evaluate.evaluate_application result !f args);

  CStack.end_vm ();

  result
};

value decode_string name str = do
{
  let lst = ref [] in

  CStack.start_vm ();

  CStack.cont2
    (fun () -> Evaluate.evaluate_unknown str)
    (fun () -> Primitives.evaluate_char_list name lst str);

  CStack.end_vm ();

  !lst
};

value decode_list name lst = do
{
  let l = ref [] in

  CStack.start_vm ();

  CStack.cont2
    (fun () -> Evaluate.evaluate_unknown lst)
    (fun () -> Evaluate.evaluate_list name l lst);

  CStack.end_vm ();

  !l
};

value decode_num name x = do
{
  let result = ref num_zero in

  CStack.start_vm ();
  evaluate_num name result x;
  CStack.end_vm ();

  !result
};

