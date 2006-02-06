
include Num;

value num_zero      = Int 0;
value num_one       = Int 1;
value num_minus_one = Int (-1);
value num_two       = Int 2;
value num_three     = Int 3;
value num_ten       = Int 10;

value num_of_ints x y = Int x // Int y;

value power_num_int x exp = power_num x (Int exp);

value land_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x land int_of_num y)
  else
    invalid_arg "land_num"
};

value lor_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x lor int_of_num y)
  else
    invalid_arg "lor_num"
};

value lxor_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x lxor int_of_num y)
  else
    invalid_arg "lxor_num"
};

value lneg_num x = do
{
  if is_integer_num x then
    Int (lnot (int_of_num x))
  else
    invalid_arg "lneg_num"
};

value old_num_of_string = num_of_string;

value num_of_string s = do
{
  try
    let n = String.index s '/' in

    old_num_of_string (String.sub s 0 n) //
    old_num_of_string (String.sub s (n+1) (String.length s - n - 1))
  with
  [ Not_found -> old_num_of_string (s ^ "/1") ]
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

