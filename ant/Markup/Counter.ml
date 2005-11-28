
open Runtime;
open Unicode;
open Logging;

(* counters *)

type counter =
{
  number : mutable int;            (* value of the counter *)
  reset  : mutable (list counter)  (* counters which arer reset everytime this counter changes *)
};

value make val =
{
  number = val;
  reset  = []
};

value rec set ctr val = do
{
  ctr.number := val;

  List.iter reset ctr.reset
}
and reset ctr = set ctr 0;

value add_reset ctr sub_ctr = do
{
  if List.memq sub_ctr ctr.reset then
    ()
  else
    ctr.reset := [sub_ctr :: ctr.reset]
};

(* counter table *)

type counter_table = DynUCTrie.t counter;

value empty_table = DynUCTrie.empty;

value new_counter loc table name val super = do
{
  if DynUCTrie.mem_string name table then do
  {
    log_warn loc "counter \"";
    log_uc_string name;
    log_string "\" redefined!\n"
  }
  else ();

  let new_ctr = make val in

  let new_table = DynUCTrie.add_string name new_ctr table in

  match super with
  [ None     -> ()
  | Some ctr -> do
    {
      try
        let c = DynUCTrie.find_string ctr new_table in

        add_reset c new_ctr
      with
      [ Not_found -> do
        {
          log_warn loc "counter \"";
          log_uc_string name;
          log_string "\" undefined!\n"
        }
      ]
    }
  ];

  new_table
};

value get_counter loc table name = do
{
  try
    let c = DynUCTrie.find_string name table in

    c.number
  with
  [ Not_found -> do
    {
      log_warn loc "counter \"";
      log_uc_string name;
      log_string "\" undefined!\n";
      0
    }
  ]
};

value set_counter loc table name val = do
{
  try do
  {
    let c = DynUCTrie.find_string name table in

    set c val;

    table
  }
  with
  [ Not_found -> new_counter loc table name val None ]
};

(* format functions *)

value int_to_arabic n = do
{
  UString.of_ascii (string_of_int n)
};

value roman_digit d i v x = match d with
[ 1 -> [i]
| 2 -> [i; i]
| 3 -> [i; i; i]
| 4 -> [i; v]
| 5 -> [v]
| 6 -> [v; i]
| 7 -> [v; i; i]
| 8 -> [v; i; i; i]
| 9 -> [i; x]
| _ -> []
];

value rec int_to_roman n = do
{
  if n < 0 then
    [45 :: int_to_roman (~-n)]    (* - n *)
  else if n = 0 then
    [48]                          (* 0 *)
  else do
  {
    let m = n / 1000 in
    let c = n / 100            - 10*m in
    let x = n / 10     - 100*m - 10*c in
    let i = n - 1000*m - 100*c - 10*x in

      XList.repeat m 109          (* m *)
    @ roman_digit c  99 100 109   (* c d m *)
    @ roman_digit x 120 108  99   (* x l c *)
    @ roman_digit i 105 118 120   (* i v x *)
  }
};

value rec int_to_ROMAN n = do
{
  if n < 0 then
    [45 :: int_to_ROMAN (~-n)]    (* - n *)
  else if n = 0 then
    [48]                          (* 0 *)
  else do
  {
    let m = n / 1000 in
    let c = n / 100            - 10*m in
    let x = n / 10     - 100*m - 10*c in
    let i = n - 1000*m - 100*c - 10*x in

      XList.repeat m 77           (* M *)
    @ roman_digit c 67 68 77      (* C D M *)
    @ roman_digit x 88 76 67      (* X L C *)
    @ roman_digit i 73 86 88      (* I V X *)
  }
};

value rec int_to_alphabetic n = do
{
  if n < 0 then
    [45 :: int_to_alphabetic (~-n)]
  else if n = 0 then
    [48]
  else do
  {
    let k = n / 26   in
    let i = n mod 26 in

    XList.repeat (k+1) (97 + i)
  }
};

value rec int_to_ALPHABETIC n = do
{
  if n < 0 then
    [45 :: int_to_ALPHABETIC (~-n)]
  else if n = 0 then
    [48]
  else do
  {
    let k = (n-1) / 26   in
    let i = (n-1) mod 26 in

    XList.repeat (k+1) (65 + i)
  }
};

value rec int_to_strings n strings = do
{
  if n < 0 then
    [45 :: int_to_strings (~-n) strings]
  else if n = 0 then
    [48]
  else do
  {
    let k = n / 26   in
    let i = n mod 26 in

    List.concat (XList.repeat (k+1) (List.nth strings i))
  }
};

