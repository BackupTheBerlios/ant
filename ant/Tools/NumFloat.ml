
(* stub to replace Num by Gmp *)

type num = float;

value num_of_int      = float_of_int;
value num_of_ints x y = float_of_int x /. float_of_int y;
value float_of_num x  = x;
value string_of_num   = string_of_float;

value num_zero      = 0.0;
value num_one       = 1.0;
value num_minus_one = ~-. 1.0;
value num_two       = 2.0;
value num_ten       = 10.0;

value add_num x y  = x +. y;
value minus_num x  = ~-. x;
value sub_num x y  = x -. y;
value mult_num x y = x *. y;
value div_num x y  = x /. y;
value sign_num x   =      if x < 0.0 then -1
                     else if x > 0.0 then 1
                     else                 0;
value compare_num x y = compare x y;

value square_num x = x *. x;

value is_integer_num x = (x = floor x);

value power_num_int x exp = x ** float_of_int exp;

value power_num x y = x ** y;

value abs_num = abs_float;
value succ_num x = x +. 1.0;
value pred_num x = x -. 1.0;

value incr_num x = !x := succ_num !x;
value decr_num x = !x := pred_num !x;

value floor_num   = floor;
value ceiling_num = ceil;

value integer_num x = do
{
  let (f,z) = modf x in

  if f > 0.5 then
    z +. 1.0
  else if f < (-0.5) then
    z -. 1.0
  else
    z
};
value round_num x = do
{
  let (f,z) = modf x in

  if f >= 0.5 then
    z +. 1.0
  else if f <= (-0.5) then
    z -. 1.0
  else
    z
};

value quo_num x y = floor_num (div_num x y);
value mod_num x y = sub_num x (mult_num y (quo_num x y));

value eq_num x y = (x = y);
value lt_num x y = (x < y);
value le_num x y = (x <= y);
value gt_num x y = (x > y);
value ge_num x y = (x >= y);

value max_num x y = max x y;
value min_num x y = min x y;
value land_num x y = do
{
  float_of_int (int_of_float x land int_of_float y);
};

value lor_num x y = do
{
  float_of_int (int_of_float x lor int_of_float y);
};

value lxor_num x y = do
{
  float_of_int (int_of_float x lxor int_of_float y);
};

value lneg_num x = do
{
  float_of_int (lnot (int_of_float x));
};

value num_of_string s = do
{
  try
    let n = String.index s '/' in

    float_of_string (String.sub s 0 n) /.
    float_of_string (String.sub s (n+1) (String.length s - n - 1))
  with
  [ Not_found -> float_of_string s ]
};

value int_of_num = int_of_float;

value num_of_float x = x;

