
(* stub to replace Num by Gmp *)

open Gmp;

type num = Q.t;

value num_of_int  x   = Q.from_ints x 1;
value num_of_ints x y = Q.from_ints x y;
value float_of_num    = Q.to_float;
value string_of_num   = Q.to_string;

value num_zero      = Q.zero;
value num_one       = num_of_int 1;
value num_minus_one = num_of_int (-1);
value num_two  = num_of_int 2;
value num_ten  = num_of_int 10;

value add_num     = Q.add;
value minus_num   = Q.neg;
value sub_num     = Q.sub;
value mult_num    = Q.mul;
value div_num     = Q.div;
value sign_num    = Q.sgn;
value compare_num = Q.compare;

value square_num x = mult_num x x;

value is_integer_num x = Z.equal_int (Q.get_den x) 1;

value power_num_int x exp = do
{
  if exp = 0 then
    num_one
  else if exp > 0 then
    Q.from_zs (Z.pow_ui (Q.get_num x) exp)
              (Z.pow_ui (Q.get_den x) exp)
  else
    Q.from_zs (Z.pow_ui (Q.get_den x) (~-exp))
              (Z.pow_ui (Q.get_num x) (~-exp))
};

value power_num x y = do
{
  if is_integer_num y then
    power_num_int x (Z.to_int (Q.get_num y))
  else
    invalid_arg "power_num"
};

value abs_num x = do
{
  match Q.sgn x with
  [ (-1) -> Q.neg x
  | _    -> x
  ]
};

value succ_num x = Q.add x num_one;
value pred_num x = Q.sub x num_one;

value incr_num x = !x := succ_num !x;
value decr_num x = !x := pred_num !x;

value floor_num   x = Q.from_z (Z.fdiv_q (Q.get_num x) (Q.get_den x));
value ceiling_num x = Q.from_z (Z.cdiv_q (Q.get_num x) (Q.get_den x));

value integer_num x = do
{
  let n = Q.get_num x in
  let d = Q.get_den x in

  let (q,r) = Z.tdiv_qr n d in

  if Z.cmp_si n 0 < 0 then
    if Z.add d r < r then
      Q.from_z (Z.sub_ui q 1)
    else
      Q.from_z q
  else
    if Z.sub d r < r then
      Q.from_z (Z.add_ui q 1)
    else
      Q.from_z q
};
value round_num x = do
{
  let n = Q.get_num x in
  let d = Q.get_den x in

  let (q,r) = Z.tdiv_qr n d in

  if Z.cmp_si n 0 < 0 then
    if Z.add d r <= r then
      Q.from_z (Z.sub_ui q 1)
    else
      Q.from_z q
  else
    if Z.sub d r <= r then
      Q.from_z (Z.add_ui q 1)
    else
      Q.from_z q
};

value quo_num x y = floor_num (div_num x y);
value mod_num x y = sub_num x (mult_num y (quo_num x y));

value eq_num x y = (Q.cmp x y) =  0;
value lt_num x y = (Q.cmp x y) <  0;
value le_num x y = (Q.cmp x y) <= 0;
value gt_num x y = (Q.cmp x y) >  0;
value ge_num x y = (Q.cmp x y) >= 0;

value max_num x y = do
{
  if lt_num x y then y else x
};
value min_num x y = do
{
  if gt_num x y then y else x
};

value land_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Q.from_z (Z.band (Q.get_num x) (Q.get_num y))
  else
    invalid_arg "land_num"
};

value lor_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Q.from_z (Z.bior (Q.get_num x) (Q.get_num y))
  else
    invalid_arg "lor_num"
};

value lxor_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Q.from_z (Z.bxor (Q.get_num x) (Q.get_num y))
  else
    invalid_arg "lxor_num"
};

value lneg_num x = do
{
  if is_integer_num x then
    Q.from_z (Z.bcom (Q.get_num x))
  else
    invalid_arg "lneg_num"
};

value num_of_string s = do
{
  try
    let n = String.index s '/' in

    Q.from_zs (Z.from_string (String.sub s 0 n))
              (Z.from_string (String.sub s (n+1) (String.length s - n - 1)))
  with
  [ Not_found -> Q.from_z (Z.from_string s) ]
};

value int_of_num x = do
{
  if is_integer_num x then
    Z.to_int (Q.get_num x)
  else
    failwith "integer argument required"
};

value num_of_float x = do
{
  let (f, n) = frexp x                 in
  let factor = power_num_int num_two n in
  let str    = string_of_float f       in
  let len    = String.length str       in

  if str.[0] = '-' then do
  {
    let factor2 = power_num_int num_ten (len - 3) in
    let z       = if str.[1] = '1' then         (* check whether str = "-1." *)
                    num_one
                  else
                    num_of_string (String.sub str 3 (len - 3))
                  in

    minus_num (z */ factor // factor2)
  }
  else do
  {
    let factor2 = power_num_int num_ten (len - 2) in
    let z       = if str.[0] = '1' then         (* check whether str = "1." *)
                    num_one
                  else
                    num_of_string (String.sub str 2 (len - 2))
                  in

    z */ factor // factor2
  }
};

