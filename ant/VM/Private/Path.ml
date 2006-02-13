
open XNum;
open Types;
open Runtime;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* Opaque type for path specifications *)

value apply_ps _ _ = runtime_error "application of non-function";

value cmp_ps p1 p2 = p1 == p2;

value (ps_wrapper, ps_unwrapper) = Opaque.declare_type "path-specification" apply_ps cmp_ps cmp_ps;

value wrap_ps ps = Opaque (ps_wrapper ps);

value unwrap_ps = Evaluate.evaluate_opaque "path-specification" ps_unwrapper;

value evaluate_vec name res_x res_y v = do
{
  CStack.cont2
    (fun () -> Evaluate.evaluate_unknown v)
    (fun () -> match !v with
    [ Tuple [|x; y|] -> do
      {
        CStack.cont2
          (fun () -> Evaluate.evaluate_num name res_x x)
          (fun () -> Evaluate.evaluate_num name res_y y)
      }
    | _ -> runtime_error (name ^ ": pair expected but got " ^ type_name !v)
    ]);
};

value rec make_path res p = do
{
  let x = ref num_zero in
  let y = ref num_zero in

  CStack.cont2
    (fun () -> evaluate_vec "make_path" x y p)
    (fun () -> !res := wrap_ps (Bezier.make_spec !x !y))
};

value close_path res cycle spec = do
{
  let encode_pair x y =
    Tuple [|ref (Number x); ref (Number y)|]
  in

  let ps = ref (Bezier.make_spec num_zero num_zero) in

  CStack.cont3
    (fun () -> unwrap_ps "close_path" ps spec)
    (fun () -> Evaluate.evaluate_unknown cycle)
    (fun () -> match !cycle with
     [ Bool c -> do
       {
         !res :=
           Array.fold_right
             (fun (x0,y0,x1,y1,x2,y2,x3,y3) lst ->
               List
                 (ref (Tuple
                        [|ref (encode_pair x0 y0);
                          ref (encode_pair x1 y1);
                          ref (encode_pair x2 y2);
                          ref (encode_pair x3 y3)|]))
                 (ref lst))
             (Bezier.close_spec !ps c)
             Nil
       }
     | _ -> runtime_error ("close_path: boolean expected but got " ^ type_name !cycle)
     ])
};

value add_point res p spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let x  = ref num_zero in
  let y  = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_point" ps spec)
    (fun () -> evaluate_vec "path_add_point" x y p)
    (fun () -> !res := wrap_ps (Bezier.add_point !ps !x !y))
};

value add_in_dir res dir spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let dx = ref num_zero in
  let dy = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_in_dir" ps spec)
    (fun () -> evaluate_vec "path_add_in_dir" dx dy dir)
    (fun () -> !res := wrap_ps (Bezier.add_in_dir !ps (Bezier.angle_of_vec !dx !dy)))
};

value add_in_angle res angle spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let a  = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_in_angle" ps spec)
    (fun () -> Evaluate.evaluate_num "path_add_in_angle" a angle)
    (fun () -> !res := wrap_ps (Bezier.add_in_dir !ps (float_of_num !a)))
};

value add_in_curl res curl spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let c  = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_in_curl" ps spec)
    (fun () -> Evaluate.evaluate_num "path_add_in_curl" c curl)
    (fun () -> !res := wrap_ps (Bezier.add_in_curl !ps !c))
};

value add_in_tension res tension spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let t  = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_in_tension" ps spec)
    (fun () -> Evaluate.evaluate_num "path_add_in_tension" t tension)
    (fun () -> !res := wrap_ps (Bezier.add_in_tension !ps !t))
};

value add_out_dir res dir spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let dx = ref num_zero in
  let dy = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_out_dir" ps spec)
    (fun () -> evaluate_vec "path_add_out_dir" dx dy dir)
    (fun () -> !res := wrap_ps (Bezier.add_out_dir !ps (Bezier.angle_of_vec !dx !dy)))
};

value add_out_angle res angle spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let a  = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_out_angle" ps spec)
    (fun () -> Evaluate.evaluate_num "path_add_out_angle" a angle)
    (fun () -> !res := wrap_ps (Bezier.add_out_dir !ps (float_of_num !a)))
};

value add_out_curl res curl spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let c  = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_out_curl" ps spec)
    (fun () -> Evaluate.evaluate_num "path_add_out_curl" c curl)
    (fun () -> !res := wrap_ps (Bezier.add_out_curl !ps !c))
};

value add_out_tension res tension spec = do
{
  let ps = ref (Bezier.make_spec num_zero num_zero) in
  let t  = ref num_zero in

  CStack.cont3
    (fun () -> unwrap_ps "path_add_out_tension" ps spec)
    (fun () -> Evaluate.evaluate_num "path_add_out_tension" t tension)
    (fun () -> !res := wrap_ps (Bezier.add_out_tension !ps !t))
};

value add_control_points res args = match args with
[ [p1; p2; spec] -> do
  {
    let ps = ref (Bezier.make_spec num_zero num_zero) in
    let x1 = ref num_zero in
    let y1 = ref num_zero in
    let x2 = ref num_zero in
    let y2 = ref num_zero in

    CStack.cont4
      (fun () -> unwrap_ps "path_add_control_points" ps spec)
      (fun () -> evaluate_vec "path_add_control_points" x1 y1 p1)
      (fun () -> evaluate_vec "path_add_control_points" x2 y2 p2)
      (fun () -> !res := wrap_ps (Bezier.add_control_points !ps !x1 !y1 !x2 !y2));
  }
| _ -> assert False
];

