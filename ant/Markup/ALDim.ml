
open XNum;
open Runtime;
open VM;
open ALCoding;

(* opaque type for dimensions *)

value apply_dim dim x = do
{
  Machine.evaluate x;

  match !x with
  [ Types.Symbol s -> do
    {
      if s = sym_Base then
        ref (Types.Number dim.Dim.d_base)
      else if s = sym_Stretch       then
        ref (Types.Tuple [|ref (Types.Number dim.Dim.d_stretch_factor);
                           ref (Types.Number (num_of_int dim.Dim.d_stretch_order))|])
      else if s = sym_StretchFactor then
        ref (Types.Number dim.Dim.d_stretch_factor)
      else if s = sym_StretchOrder  then
        ref (Types.Number (num_of_int dim.Dim.d_stretch_order))
      else if s = sym_Shrink        then
        ref (Types.Tuple [|ref (Types.Number dim.Dim.d_shrink_factor);
                           ref (Types.Number (num_of_int dim.Dim.d_shrink_order))|])
      else if s = sym_ShrinkFactor  then
        ref (Types.Number dim.Dim.d_shrink_factor)
      else if s = sym_ShrinkOrder   then
        ref (Types.Number (num_of_int dim.Dim.d_shrink_order))
      else
        Types.runtime_error "invalid argument"
    }
  | _ -> Types.runtime_error "invalid argument"
  ]
};

value cmp_dim = Dim.dim_equal;

value (dim_wrapper, dim_unwrapper) = Opaque.declare_type "dimension" apply_dim cmp_dim cmp_dim;

value wrap_dim dim = Types.Opaque (dim_wrapper dim);

value unwrap_dim = decode_opaque "dimension" dim_unwrapper;

(* primitives *)

value prim_make_dim res args = match args with
[ [base; st; st_ord; sh; sh_ord] -> do
  {
    let a = Machine.evaluate_num "make_dim" base   in
    let b = Machine.evaluate_num "make_dim" st     in
    let c = decode_int           "make_dim" st_ord in
    let d = Machine.evaluate_num "make_dim" sh     in
    let e = decode_int           "make_dim" sh_ord in

    !res :=
      wrap_dim
        {
          Dim.d_base           = a;
          Dim.d_stretch_factor = b;
          Dim.d_stretch_order  = c;
          Dim.d_shrink_factor  = d;
          Dim.d_shrink_order   = e
        }
  }
| _ -> assert False
];

value prim_fixed_dim res base = do
{
  let x = Machine.evaluate_num "fixed_dim" base in

  !res := wrap_dim (Dim.fixed_dim x)
};

value prim_dim_zero   = wrap_dim Dim.dim_zero;
value prim_dim_1pt    = wrap_dim Dim.dim_1pt;
value prim_dim_12pt   = wrap_dim Dim.dim_12pt;
value prim_dim_fil    = wrap_dim Dim.dim_fil;
value prim_dim_fill   = wrap_dim Dim.dim_fill;
value prim_dim_ss     = wrap_dim Dim.dim_ss;
value prim_dim_filneg = wrap_dim Dim.dim_filneg;

value prim_dim_equal res dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_equal" dim0 in
  let d1 = unwrap_dim "dim_equal" dim1 in

  !res := Types.Bool (Dim.dim_equal d0 d1)
};

value prim_dim_add res dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_add" dim0 in
  let d1 = unwrap_dim "dim_add" dim1 in

  !res := wrap_dim (Dim.dim_add d0 d1)
};

value prim_dim_neg res dim = do
{
  let d = unwrap_dim "dim_neg" dim in

  !res := wrap_dim (Dim.dim_neg d)
};

value prim_dim_sub res dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_sub" dim0 in
  let d1 = unwrap_dim "dim_sub" dim1 in

  !res := wrap_dim (Dim.dim_sub d0 d1)
};

value prim_dim_mult res x dim = do
{
  let a = Machine.evaluate_num "dim_mult" x   in
  let d = unwrap_dim           "dim_mult" dim in

  !res := wrap_dim (Dim.dim_mult a d)
};

value prim_dim_max res dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_max" dim0 in
  let d1 = unwrap_dim "dim_max" dim1 in

  !res := wrap_dim (Dim.dim_max d0 d1)
};

value prim_dim_min res dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_min" dim0 in
  let d1 = unwrap_dim "dim_min" dim1 in

  !res := wrap_dim (Dim.dim_min d0 d1)
};

value prim_dim_max_stretch res dim = do
{
  let d = unwrap_dim "dim_max_stretch" dim in

  !res := Types.Number (Dim.dim_max_stretch d)
};

value prim_dim_max_shrink res dim = do
{
  let d = unwrap_dim "dim_max_shrink" dim in

  !res := Types.Number (Dim.dim_max_shrink d)
};

value prim_dim_max_value res dim = do
{
  let d = unwrap_dim "dim_max_value" dim in

  !res := Types.Number (Dim.dim_max_value d)
};

value prim_dim_min_value res dim = do
{
  let d = unwrap_dim "dim_min_value" dim in

  !res := Types.Number (Dim.dim_min_value d)
};

value prim_dim_shift_base res dim x = do
{
  let d = unwrap_dim           "dim_shift_base" dim in
  let y = Machine.evaluate_num "dim_shift_base" x   in

  !res := wrap_dim (Dim.dim_shift_base d y)
};

value prim_dim_shift_base_upto res dim x = do
{
  let d = unwrap_dim           "dim_shift_base_upto" dim in
  let y = Machine.evaluate_num "dim_shift_base_upto" x   in

  !res := wrap_dim (Dim.dim_shift_base_upto d y)
};

value prim_dim_inc_upto res dim x = do
{
  let d = unwrap_dim           "dim_inc_upto" dim in
  let y = Machine.evaluate_num "dim_inc_upto" x   in

  !res := wrap_dim (Dim.dim_inc_upto d y)
};

value prim_dim_dec_upto res dim x = do
{
  let d = unwrap_dim           "dim_dec_upto" dim in
  let y = Machine.evaluate_num "dim_dec_upto" x   in

  !res := wrap_dim (Dim.dim_dec_upto d y)
};

value prim_dim_resize_upto res dim x = do
{
  let d = unwrap_dim           "dim_resize_upto" dim in
  let y = Machine.evaluate_num "dim_resize_upto" x   in

  !res := wrap_dim (Dim.dim_resize_upto d y)
};

value prim_adjustment_ratio res dim x = do
{
  let d = unwrap_dim           "dim_adjustment_ratio" dim in
  let y = Machine.evaluate_num "dim_adjustment_ratio" x   in

  let (a,b) = Dim.adjustment_ratio d y in

  !res := Types.Tuple [|ref (Types.Number a); ref (Types.Number (num_of_int b))|]
};

value prim_dim_scale_badness res ratio = do
{
  Machine.evaluate ratio;

  match !ratio with
  [ Types.Tuple [|x; y|] -> do
    {
      let a = Machine.evaluate_num "dim_scale_badness" x in
      let b = decode_int           "dim_scale_badness" y in

      !res := Types.Number (Dim.dim_scale_badness (a,b))
    }
  | _ -> Types.runtime_error "dim_scale_badness: invalid argument"
  ]
};

value prim_dim_scale res dim ratio = do
{
  let d = unwrap_dim "dim_scale" dim in

  Machine.evaluate ratio;

  match !ratio with
  [ Types.Tuple [|y; z|] -> do
    {
      let a = Machine.evaluate_num "dim_scale" y in
      let b = decode_int           "dim_scale" z in

      !res := wrap_dim (Dim.dim_scale d (a,b))
    }
  | _ -> Types.runtime_error "dim_scale: invalid argument"
  ]
};

value prim_dim_scale_upto res dim ratio = do
{
  let d = unwrap_dim "dim_scale_upto" dim in

  Machine.evaluate ratio;

  match !ratio with
  [ Types.Tuple [|y; z|] -> do
    {
      let a = Machine.evaluate_num "dim_scale_upto" y in
      let b = decode_int           "dim_scale_upto" z in

      !res := wrap_dim (Dim.dim_scale_upto d (a,b))
    }
  | _ -> Types.runtime_error "dim_scale_upto: invalid argument"
  ]
};

