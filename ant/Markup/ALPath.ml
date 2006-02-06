
open XNum;
open Runtime;
open VM;
open ALCoding;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* Opaque type for path specifications *)

value apply_ps _ _ = Types.runtime_error "application of non-function";

value cmp_ps p1 p2 = p1 == p2;

value (ps_wrapper, ps_unwrapper) = Opaque.declare_type "path-specification" apply_ps cmp_ps cmp_ps;

value wrap_ps ps = Types.Opaque (ps_wrapper ps);

value unwrap_ps = evaluate_opaque "path-specification" ps_unwrapper;

value decode_ps = decode_opaque "path-specification" ps_unwrapper;

value rec p_make_path res px py = do
{
  let x = ref num_zero in
  let y = ref num_zero in

  Machine.continue3
    (fun () -> Machine.evaluate_num "p_make_path" x px)
    (fun () -> Machine.evaluate_num "p_make_path" y py)
    (fun () -> !res := wrap_ps (Bezier.make_spec !x !y))
};

value p_close_path res cycle spec = do
{
  let encode_pair x y =
    Types.Tuple [|ref (Types.Number x);
                  ref (Types.Number y)|]
  in

  let ps = ref (Bezier.make_spec num_zero num_zero) in

  Machine.continue3
    (fun () -> unwrap_ps "p_close_path" ps spec)
    (fun () -> Machine.evaluate_unknown cycle)
    (fun () -> match !cycle with
     [ Types.Bool c -> do
       {
         !res :=
           Array.fold_right
             (fun (x0,y0,x1,y1,x2,y2,x3,y3) lst ->
               Types.List
                 (ref (Types.Tuple
                        [|ref (encode_pair x0 y0);
                          ref (encode_pair x1 y1);
                          ref (encode_pair x2 y2);
                          ref (encode_pair x3 y3)|]))
                 (ref lst))
             (Bezier.close_spec !ps c)
             Types.Nil
       }
     | _ -> Types.runtime_error ("p_close_path: boolean expected but got " ^ Types.type_name !cycle)
     ])
};

value p_add_point res args = match args with
[ [px; py; spec] -> do
  {
    let ps = ref (Bezier.make_spec num_zero num_zero) in
    let x  = ref num_zero in
    let y  = ref num_zero in

    Machine.continue4
      (fun () -> unwrap_ps "p_add_point" ps spec)
      (fun () -> Machine.evaluate_num "p_add_point" x px)
      (fun () -> Machine.evaluate_num "p_add_point" y py)
      (fun () -> !res := wrap_ps (Bezier.add_point !ps !x !y))
  }
| _ -> assert False
];

value p_add_in_dir res angle spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let a  = ref num_zero in

  Machine.continue3
    (fun () -> unwrap_ps "p_add_in_dir" ps spec)
    (fun () -> Machine.evaluate_num "p_add_in_dir" a angle)
    (fun () -> !res := wrap_ps (Bezier.add_in_dir !ps (float_of_num !a)))
};

value p_add_in_curl res curl spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let c  = ref num_zero in

  Machine.continue3
    (fun () -> unwrap_ps "p_add_in_curl" ps spec)
    (fun () -> Machine.evaluate_num "p_add_in_curl" c curl)
    (fun () -> !res := wrap_ps (Bezier.add_in_curl !ps !c))
};

value p_add_in_tension res tension spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let t  = ref num_zero in

  Machine.continue3
    (fun () -> unwrap_ps "p_add_in_tension" ps spec)
    (fun () -> Machine.evaluate_num "p_add_in_tension" t tension)
    (fun () -> !res := wrap_ps (Bezier.add_in_tension !ps !t))
};

value p_add_out_dir res angle spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let a  = ref num_zero in

  Machine.continue3
    (fun () -> unwrap_ps "p_add_out_dir" ps spec)
    (fun () -> Machine.evaluate_num "p_add_out_dir" a angle)
    (fun () -> !res := wrap_ps (Bezier.add_out_dir !ps (float_of_num !a)))
};

value p_add_out_curl res curl spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let c  = ref num_zero in

  Machine.continue3
    (fun () -> unwrap_ps "p_add_out_curl" ps spec)
    (fun () -> Machine.evaluate_num "p_add_out_curl" c curl)
    (fun () -> !res := wrap_ps (Bezier.add_out_curl !ps !c))
};

value p_add_out_tension res tension spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let t  = ref num_zero in

  Machine.continue3
    (fun () -> unwrap_ps "p_add_out_tension" ps spec)
    (fun () -> Machine.evaluate_num "p_add_out_tension" t tension)
    (fun () -> !res := wrap_ps (Bezier.add_out_tension !ps !t))
};

value p_add_control_points res args = match args with
[ [px1; py1; px2; py2; spec] -> do
  {
    let ps = ref (Bezier.make_spec num_zero num_zero) in
    let x1 = ref num_zero in
    let y1 = ref num_zero in
    let x2 = ref num_zero in
    let y2 = ref num_zero in

    (* Recall that continuations are pushed in reverse order.
       So we first add the second batch. *)
    Machine.continue3
      (fun () -> Machine.evaluate_num "p_add_control_points" x2 px2)
      (fun () -> Machine.evaluate_num "p_add_control_points" y2 py2)
      (fun () -> !res := wrap_ps (Bezier.add_control_points !ps !x1 !y1 !x2 !y2));
    Machine.continue3
      (fun () -> unwrap_ps "p_add_control_points" ps spec)
      (fun () -> Machine.evaluate_num "p_add_control_points" x1 px1)
      (fun () -> Machine.evaluate_num "p_add_control_points" y1 py1)
  }
| _ -> assert False
];

