
open XNum;
open Types;
open VMPrivate;
open Unicode.Types;
open Unicode.SymbolTable;

value write_string os str = do
{
  IO.printf os "%d" (Array.length str);

  Array.iter
    (fun c -> IO.write_be_u32 os (num_of_int c))
    str
};

value rec serialise_unknown os x = match !x with
[ Bool False   -> IO.write_string os "b0"
| Bool True    -> IO.write_string os "b1"
| Number n     -> do
  {
    IO.write_char os 'n'
    (* FIX *)
  }
| Char c -> do
  {
    IO.printf os "c%d" c
  }
| Symbol s -> do
  {
    IO.write_char os 's';
    write_string os (symbol_to_string s)
  }
| UnevalT _ _ -> do
  {
    Evaluate.evaluate_unknown x;
    serialise_unknown os x
  }
| Nil      -> IO.write_char os ']'
| List a b -> do
  {
    IO.write_char os '[';
    serialise_unknown os a;
    serialise_unknown os b
  }
| Tuple xs -> do
  {
    IO.printf os "(%d" (Array.length xs);
    Array.iter (serialise_unknown os) xs
  }
| Dictionary d -> do
  {
    let size = SymbolMap.fold (fun _ _ n -> n + 1) d 0 in

    IO.printf os "{%d" size;

    SymbolMap.iter
      (fun k v -> do
       {
         write_string os (symbol_to_string k);
         serialise_unknown os v
       })
      d
  }
| Unbound
| Constraint _
| LinForm _
| Primitive1 _
| Primitive2 _
| PrimitiveN _ _
| SimpleFunction _ _ _
| PatternFunction _ _ _ _ _
| Chain _
| Relation _ _
| Application _ _
| Opaque _ -> IO.write_char os '?'
];

value read_integer is = do
{
  iter 0

  where rec iter n = do
  {
    let c = int_of_char (IO.peek_char is 0) in

    if c >= 48 && c <= 57 then do
    {
      IO.skip is 1;
      iter (10 * n + c - 48)
    }
    else
      n
  }
};

value read_string is = do
{
  let l = read_integer is in

  Array.init l (fun _ -> int_of_num (IO.read_be_u32 is))
};


value rec unserialise_unknown is = do
{
  match IO.read_char is with
  [ '?' -> Unbound
  | 'b' -> match IO.read_char is with
    [ '0' -> Bool False
    | '1' -> Bool True
    | _   -> runtime_error "Corrupt data."
    ]
  | 'c' -> Char (read_integer is)
  | 's' -> Symbol (string_to_symbol (read_string is))
  | 'n' -> do
    {
      Number num_zero (* FIX *)
    }
  | ']' -> Nil
  | '[' -> do
    {
      let a = unserialise_unknown is in
      let b = unserialise_unknown is in
      List (ref a) (ref b)
    }
  | '(' -> do
    {
      let l = read_integer is in

      Tuple (Array.init l
               (fun _ -> ref (unserialise_unknown is)))
    }
  | '{' -> do
    {
      let l = read_integer is in

      iter l SymbolMap.empty

      where rec iter i d = do
      {
        if i <= 0 then
          Dictionary d
        else do
        {
          let k = string_to_symbol (read_string is) in
          let v = unserialise_unknown is in
          iter (i-1) (SymbolMap.add k (ref v) d)
        }
      }
    }
  | _ -> runtime_error "Corrupt data."
  ]
};

