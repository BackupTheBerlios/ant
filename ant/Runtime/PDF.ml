
open XNum;

type pdf_value =
[ Null
| Bool of bool
| Int of int
| Float of float
| String of IO.iorstream
| Stream of pdf_dictionary and IO.iorstream
| Symbol of string
| Array of list pdf_value
| Dictionary of pdf_dictionary
| Reference of int and int
]
and pdf_dictionary = list (string * pdf_value);

type pdf_object =
{
  id   : int;
  rev  : int;
  data : pdf_value
};

type pdf_file 'a =
{
  file    : 'a;
  version : mutable float;
  root    : mutable pdf_value;
  info    : mutable pdf_value;
  revs    : mutable array int;
  xrefs   : mutable array int
};

value create_pdf file version =
{
  file    = file;
  version = version;
  root    = Null;
  info    = Null;
  revs  = [| -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1 |];
  xrefs = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0 |]
};

value dict_lookup dict key = do
{
  try
    List.assoc key dict
  with
  [ _ -> Null ]
};

value rec alloc_object pdf = do
{
  if pdf.xrefs.(0) > 0 then do
  {
    (* The free list is nonempty. *)

    let o = pdf.xrefs.(0) in

    pdf.xrefs.(0) := pdf.xrefs.(o);

    o
  }
  else do
  {
    (* Enlarge the arrays. *)

    let len = Array.length pdf.xrefs in

    pdf.revs := Array.init (2 * len)
                  (fun i ->
                    if i < len then
                      pdf.revs.(i)
                    else
                      -1);
    pdf.xrefs := Array.init (2 * len)
                   (fun i ->
                     if i < len then do
                     {
                       if i > 0 then
                         pdf.xrefs.(i)
                       else
                         len
                     }
                     else if i < 2*len - 1 then
                       i+1
                     else
                       0
                   );

     alloc_object pdf
  }
};

value set_root pdf val = do
{
  pdf.root := val
};

(* parsing *)

value is_white_space c = match c with
[ '\000' | '\t' | '\n' | '\012' | '\r' | ' ' -> True
| _ -> False
];

value is_special c = match c with
[ '(' | ')' | '{' | '}' | '<' | '>' | '[' | ']' | '/' | '%' -> True
| _ -> False
];

value hex_to_dec d = do
{
  if d >= '0' && d <= '9' then
    int_of_char d - 48
  else if d >= 'A' && d <= 'F' then
    int_of_char d - 55
  else if d >= 'a' && d <= 'f' then
    int_of_char d - 87
  else
    0
};

value value_to_int v = match v with
[ Int x   -> x
| Float x -> int_of_float x
| _       -> 0
];

(* basic parsing *)

value skip_eol is = do
{
  if IO.peek_char is 0 = '\n' then do
  {
    IO.read_char is;
    True
  }
  else if IO.peek_char is 0 = '\r' then do
  {
    IO.read_char is;

    if IO.peek_char is 0 = '\n' then
      ignore (IO.read_char is)
    else ();

    True
  }
  else
    False
};

value skip_white_space is = do
{
  while is_white_space (IO.peek_char is 0) do
  {
    IO.read_char is
  }
};

value rec skip_comment is = do
{
  if skip_eol is then
    ()
  else
    skip_comment is
};

value rec skip_white_space_and_comments is = do
{
  skip_white_space is;

  if IO.peek_char is 0 = '%' then do
  {
    skip_comment is;
    skip_white_space_and_comments is
  }
  else ()
};

value read_keyword is str = do
{
  skip_white_space_and_comments is;

  let look_ahead = IO.read_string is (String.length str) in

  if look_ahead = str then do
  {
    let c = IO.peek_char is 0 in

    if is_special c || is_white_space c then
      True
    else do
    {
      IO.skip is (-String.length look_ahead);
      False
    }
  }
  else do
  {
    IO.skip is (-String.length look_ahead);
    False
  }
};

(*
value read_token is = do
{
  skip_white_space is;

  let c = IO.read_char is in

  if is_special c then
    [c]
  else
    iter [c]

  where rec iter token = do
  {
    let c = IO.peek_char is 0 in

    if is_special c || is_white_space c then
      List.rev token
    else do
    {
      IO.read_char is;
      iter [c :: token]
    }
  }
};
*)

(* filters *)

value ascii_hex_decode stream = do
{
  let cs = IO.make_buffer_stream (IO.size stream) in

  iter ()

  where rec iter () = do
  {
    skip_white_space stream;

    if IO.eof stream then do
    {
      IO.seek cs 0;
      cs
    }
    else do
    {
      let c = IO.read_char stream in

      if c = '>' then do
      {
        IO.seek cs 0;
        cs
      }
      else do
      {
        skip_white_space stream;

        let d = IO.read_char stream in

        if d = '>' then do
        {
          IO.write_byte cs (16 * hex_to_dec c);
          IO.seek cs 0;
          cs
        }
        else do
        {
          IO.write_byte cs (16 * hex_to_dec c + hex_to_dec d);
          iter ()
        }
      }
    }
  }
};

value ascii_85_decode stream = do
{
  let cs = IO.make_buffer_stream (IO.size stream) in

  iter ()

  where rec iter () = do
  {
    if IO.eof stream then do
    {
      IO.seek cs 0;
      cs
    }
    else do
    {
      let c = IO.read_byte stream in

      if c = 126 then do
      {
        IO.seek cs 0;
        cs
      }
      else if c = 122 then do
      {
        IO.write_byte cs 0;
        IO.write_byte cs 0;
        IO.write_byte cs 0;
        IO.write_byte cs 0;
        iter ()
      }
      else if c >= 33 && c <= 117 then do
      {
        IO.skip_while stream (fun x -> x < '\033' || (x > '\117' && x <> '\126'));

        let d = IO.read_byte stream in

        if d < 33 || d > 117 then
          iter ()
        else do
        {
          let c = 85 * c + d in

          IO.skip_while stream (fun x -> x < '\033' || (x > '\117' && x <> '\126'));

          let d = IO.read_byte stream in

          if d < 33 || d > 117 then do
          {
            IO.write_byte cs (int_of_num (num_of_int 614125 */ num_of_int c // num_of_int 0x1000000));
            iter ()
          }
          else do
          {
            let c = 85 * c + d in

            IO.skip_while stream (fun x -> x < '\033' || (x > '\117' && x <> '\126'));

            let d = IO.read_byte stream in

            if d < 33 || d > 117 then do
            {
              IO.write_be_u16 cs (int_of_num (num_of_int 7225 */ num_of_int c // num_of_int 0x10000));
              iter ()
            }
            else do
            {
              let c = 85 * c + d in

              IO.skip_while stream (fun x -> x < '\033' || (x > '\117' && x <> '\126'));

              let d = IO.read_byte stream in

              if d < 33 || d > 117 then do
              {
                IO.write_be_u24 cs (int_of_num (num_of_int 85 */ num_of_int c // num_of_int 0x100));
                iter ()
              }
              else do
              {
                IO.write_be_u32 cs (num_of_int 85 */ num_of_int c +/ num_of_int d);
                iter ()
              }
            }
          }
        }
      }
      else
        iter ()
    }
  }
};

value run_length_decode stream = do
{
  let cs = IO.make_buffer_stream (IO.size stream) in

  iter ()

  where rec iter () = do
  {
    if IO.eof stream then do
    {
      IO.seek cs 0;
      cs
    }
    else do
    {
      let len = IO.read_byte stream in

      if len < 128 then do
      {
        IO.write_string cs (IO.read_string stream (len+1));
        iter ()
      }
      else do
      {
        IO.write_string cs (String.make (257 - len) (IO.read_char stream));
        iter ()
      }
    }
  }
};

value apply_filter filter param stream = do
{
  let filters = match filter with
  [ Symbol f -> [Symbol f]
  | Array fs -> fs
  | _        -> []
  ]
  in
  let params = match param with
  [ Dictionary d -> [Dictionary d]
  | Array ds     -> ds
  | _            -> []
  ]
  in

  iter stream filters params

  where rec iter stream filters params = match (filters, params) with
  [ ([f::fs], [p::ps]) -> iter (apply f p    stream) fs ps
  | ([f::fs], [])      -> iter (apply f Null stream) fs []
  | ([], _)            -> stream
  ]
  where apply filter param stream = match filter with
  [ Symbol "ASCIIHexDecode"  -> ascii_hex_decode stream
  | Symbol "ASCII85Decode"   -> ascii_85_decode stream
  | Symbol "RunLengthDecode" -> run_length_decode stream
  | Symbol "FlateDecode"     -> IO.uncompress stream
  | Symbol "LZWDecode"       -> stream (* FIX *)
  | Symbol "CCITTFaxDecode"  -> stream (* FIX *)
  | Symbol "JBIG2Decode"     -> stream (* FIX *)
  | Symbol "DCTDecode"       -> stream (* FIX *)
  | _                        -> stream
  ]
};

(* parsing values *)

value read_bool is = do
{
  skip_white_space_and_comments is;

  if read_keyword is "true" then
    True
  else if read_keyword is "false" then
    False
  else
    False
};

value rec read_int is = do
{
  skip_white_space_and_comments is;

  match IO.peek_char is 0 with
  [ '+' -> do
    {
      IO.read_char is;
      read_int is
    }
  | '-' -> do
    {
      IO.read_char is;
      ~- (read_int is)
    }
  | _ -> do
    {
      iter 0

      where rec iter x = do
      {
        let c = IO.peek_char is 0 in

        if c < '0' || c > '9' then
          x
        else do
        {
          IO.read_char is;

          iter (10 * x + int_of_char c - 48)
        }
      }
    }
  ]
};

value rec read_float is = do
{
  skip_white_space_and_comments is;

  match IO.peek_char is 0 with
  [ '+' -> do
    {
      IO.read_char is;
      read_float is
    }
  | '-' -> do                            (* - *)
    {
      IO.read_char is;
      ~-. (read_float is)
    }
  | _ -> do
    {
      let x = read_int is in

      if IO.peek_char is 0 <> '.' then    (* . *)
        float_of_int x
      else do
      {
        IO.read_char is;

        iter (float_of_int x) 0.1
      }

      where rec iter x f = do
      {
        let c = IO.peek_char is 0 in

        if c < '0' || c > '9' then
          x
        else do
        {
          IO.read_char is;

          iter (x +. f *. float_of_int (int_of_char c - 48)) (f *. 0.1)
        }
      }
    }
  ]
};

value read_literal_string is = do
{
  let str = IO.make_buffer_stream 0x100 in

  IO.read_char is;   (* ( *)

  iter 0

  where rec iter nest = do
  {
    match IO.read_char is with
    [ ')'  -> if nest = 0 then
                str
              else do
              {
                IO.write_char str ')';
                iter (nest - 1)
              }
    | '('  -> do { IO.write_char str '('; iter (nest + 1) }
    | '\n' -> do { IO.write_char str '\r'; iter nest }
    | '\r' -> do
      {
        if IO.peek_char is 0 = '\n' then
          IO.skip is 1
        else ();

        IO.write_char str '\r';
        iter nest
      }
    | '\\' -> do
      {
        match IO.peek_char is 0 with
        [ 'n' -> do
          {
            IO.read_char is;
            IO.write_char str '\n';
            iter nest
          }
        | 'r' -> do
          {
            IO.read_char is;
            IO.write_char str '\n';
            iter nest
          }
        | 't' -> do
          {
            IO.read_char is;
            IO.write_char str '\t';
            iter nest
          }
        | 'b' -> do
          {
            IO.read_char is;
            IO.write_char str '\008';
            iter nest
          }
        | 'f' -> do
          {
            IO.read_char is;
            IO.write_char str '\012';
            iter nest
          }
        | '(' -> do
          {
            IO.read_char is;
            IO.write_char str '(';
            iter nest
          }
        | ')' -> do
          {
            IO.read_char is;
            IO.write_char str ')';
            iter nest
          }
        | '\\' -> do
          {
            IO.read_char is;
            IO.write_char str '\\';
            iter nest
          }
        | '\n' -> do
          {
            IO.read_char is;
            iter nest
          }
        | '\r' -> do
          {
            IO.read_char is;

            if IO.peek_char is 0 = '\n' then
              IO.skip is 1
            else ();

            iter nest
          }
        | c -> do
          {
            IO.read_char is;

            if c >= '0' || c <= '7' then do
            {
              let d = IO.peek_char is 0 in

              if d < '0' || d > '7' then
                IO.write_char str c
              else do
              {
                IO.read_char is;

                let c = 8 * int_of_char c + int_of_char d in
                let d = IO.peek_char is 0 in

                if d < '0' || d > '7' then
                  IO.write_byte str c
                else do
                {
                  IO.read_char is;
                  IO.write_byte str (8 * c + int_of_char d)
                }
              }
            }
            else ();

            iter nest
          }
        ]
      }
    | c -> do { IO.write_char str c; iter nest }
    ]
  }
};

value read_hex_string is = do
{
  let str = IO.make_buffer_stream 0x100 in

  IO.read_char is;   (* < *)

  iter ()

  where rec iter () = do
  {
    skip_white_space is;

    let c = IO.read_char is in

    if c = '>' then
      str
    else do
    {
      skip_white_space is;

      let d = IO.read_char is in

      if d = '>' then do
      {
        IO.write_byte str (16 * hex_to_dec c);
        str
      }
      else do
      {
        IO.write_byte str (16 * hex_to_dec c + hex_to_dec d);
        iter ()
      }
    }
  }
};

value read_string is = do
{
  skip_white_space_and_comments is;

  match IO.peek_char is 0 with
  [ '(' -> read_literal_string is
  | '<' -> read_hex_string is
  | _   -> IO.make_buffer_stream 4
  ]
};

value read_symbol is = do
{
  skip_white_space_and_comments is;

  if IO.peek_char is 0 <> '/' then
    ""
  else do
  {
    let sym = IO.make_buffer_stream 0x100 in

    IO.read_char is;

    iter ()

    where rec iter () = do
    {
      let c = IO.peek_char is 0 in

      if is_special c || is_white_space c then
        IO.to_string sym
      else if c = '#' then do
      {
        IO.read_char is;

        let c = IO.read_char is in
        let d = IO.read_char is in

        IO.write_byte sym (16 * hex_to_dec c + hex_to_dec d);

        iter ()
      }
      else do
      {
        IO.read_char is;
        IO.write_char sym c;

        iter ()
      }
    }
  }
};

value read_reference is = do
{
  let id  = read_int is in
  let rev = read_int is in

  if read_keyword is "R" then
    Reference id rev
  else
    Null
};

value read_float_or_reference is = do
{
  skip_digits 0

  where rec skip_digits i = do
  {
    let c = IO.peek_char is i in

    if c >= '0' && c <= '9' then
      skip_digits (i+1)
    else
      skip_space (i+1)
  }
  where rec skip_space i = do
  {
    let c = IO.peek_char is i in

    if is_white_space c then
      skip_space (i+1)
    else if c >= '0' && c <= '9' then
      read_reference is
    else
      Float (read_float is)
  }
};

value rec read_array pdf = do
{
  skip_white_space_and_comments pdf.file;

  if IO.peek_char pdf.file 0 <> '[' then
    []
  else do
  {
    let arr = ListBuilder.make () in

    IO.read_char pdf.file;

    iter ()

    where rec iter () = do
    {
      skip_white_space_and_comments pdf.file;

      if IO.peek_char pdf.file 0 = ']' then
        ListBuilder.get arr
      else do
      {
        ListBuilder.add arr (read_value pdf);

        iter ()
      }
    }
  }
}
and read_dictionary pdf = do
{
  skip_white_space_and_comments pdf.file;

  if IO.peek_string pdf.file 0 2 <> "<<" then
    []
  else do
  {
    let dict = ListBuilder.make () in

    IO.skip pdf.file 2;

    iter ()

    where rec iter () = do
    {
      skip_white_space_and_comments pdf.file;

      if IO.peek_string pdf.file 0 2 = ">>" then do
      {
        IO.skip pdf.file 2;
        ListBuilder.get dict
      }
      else do
      {
        let k = read_symbol pdf.file in

        skip_white_space_and_comments pdf.file;

        if IO.peek_string pdf.file 0 2 = ">>" then do
        {
          IO.skip pdf.file 2;
          ListBuilder.get dict
        }
        else do
        {
          match read_value pdf with
          [ Null -> ()
          | v    -> ListBuilder.add dict (k,v)
          ];

          iter ()
        }
      }
    }
  }
}
and read_file pdf file_desc = do
{
  let file_name = match lookup_reference pdf file_desc with
  [ String str      -> str
  | Dictionary dict -> do
    {
      match lookup_reference pdf (dict_lookup dict "F") with
      [ String str -> str
      | _          -> match lookup_reference pdf (dict_lookup dict "Unix") with
        [ String str -> str
        | _          -> match lookup_reference pdf (dict_lookup dict "Mac") with
          [ String str -> str           (* FIX: translate name *)
          | _          -> match lookup_reference pdf (dict_lookup dict "DOS") with
            [ String str -> str         (* FIX: translate name *)
            | _          -> IO.make_buffer_stream 4
            ]
          ]
        ]
      ]
    }
  | _ -> IO.make_buffer_stream 4
  ]
  in

  if IO.size file_name > 0 then do
  {
    let ic = open_in_bin (IO.to_string file_name) in
    let cs = IO.make_buffer_stream 0x10000        in

    IO.append_channel cs ic;

    close_in ic;

    cs
  }
  else
    IO.make_buffer_stream 4
}
and read_stream pdf dict = do
{
  skip_white_space_and_comments pdf.file;

  if not (read_keyword pdf.file "stream") then
    IO.make_buffer_stream 4
  else do
  {
    if IO.read_char pdf.file = '\r' then do
    {
      if IO.peek_char pdf.file 0 = '\n' then
        IO.skip pdf.file 1
      else ()
    }
    else ();

    let len  = value_to_int (lookup_reference pdf (dict_lookup dict "Length")) in
    let data = (IO.sub_stream pdf.file len) in

    skip_eol pdf.file;

    if not (read_keyword pdf.file "endstream") then
      IO.make_buffer_stream 4
    else do
    {
      match lookup_reference pdf (dict_lookup dict "F") with
      [ Null -> do
        {
          let filter = lookup_reference pdf (dict_lookup dict "Filter")      in
          let params = lookup_reference pdf (dict_lookup dict "DecodeParms") in

          apply_filter filter params data
        }
      | file -> do
        {
          (* read external file *)

          let data   = read_file pdf file in
          let filter = lookup_reference pdf (dict_lookup dict "FFilter")      in
          let params = lookup_reference pdf (dict_lookup dict "FDecodeParms") in

          apply_filter filter params data
        }
      ]
    }
  }
}
and read_value pdf = do
{
  skip_white_space_and_comments pdf.file;

  match IO.peek_char pdf.file 0 with
  [ '(' -> String (read_string pdf.file)
  | '<' -> do
    {
      if IO.peek_char pdf.file 1 = '<' then do
      {
        let dict = read_dictionary pdf in

        skip_white_space_and_comments pdf.file;

        if IO.peek_string pdf.file 0 6 = "stream" then
          Stream dict (read_stream pdf dict)
        else
          Dictionary dict
      }
      else
        String (read_string pdf.file)
    }
  | '[' -> Array (read_array pdf)
  | '/' -> Symbol (read_symbol pdf.file)
  | '+' -> Float (read_float pdf.file)
  | '-' -> Float (read_float pdf.file)
  | c -> do
    {
      if c >= '0' && c <= '9' then
        read_float_or_reference pdf.file
      else if read_keyword pdf.file "null" then
        Null
      else if read_keyword pdf.file "true" then
        Bool True
      else if read_keyword pdf.file "false" then
        Bool False
      else do
      {
        IO.skip pdf.file 1;
        read_value pdf
      }
    }
  ]
}
and read_object pdf = do
{
  let id  = read_int pdf.file in
  let rev = read_int pdf.file in

  skip_white_space_and_comments pdf.file;

  if not (read_keyword pdf.file "obj") then
    { id = id; rev = rev; data = Null }
  else do
  {
    let data = read_value pdf in

    skip_white_space_and_comments pdf.file;

    if read_keyword pdf.file "endobj" then
      { id = id; rev = rev; data = data }
    else
      { id = id; rev = rev; data = Null }
  }
}
and load_object pdf i = do
{
  IO.seek pdf.file pdf.xrefs.(i);

  read_object pdf
}
and lookup_reference pdf val = do
{
  iter val []

  where rec iter val ids = match val with
  [ Reference id _ -> do
    {
      if List.mem id ids then           (* cyclic chain of references! *)
        Null
      else do
      {
        let obj = load_object pdf id in

        iter obj.data [id :: ids]
      }
    }
  | _ -> val
  ]
};

value rec read_xrefs is file pos = do
{
  IO.seek is pos;

  if IO.read_string is 4 <> "xref" then
    ()
  else do
  {
    read_subsection ()

    where rec read_subsection () = do
    {
      let start = read_int is in
      let len   = read_int is in

      for i = 0 to len - 1 do
      {
        let entry = IO.read_string is 20 in

        Scanf.sscanf entry "%10d %5d %c"
          (fun pos rev c -> do
            {
              if rev > file.revs.(start + i) then do
              {
                file.revs.(start + i)  := rev;
                file.xrefs.(start + i) := pos;
              }
              else ()
            })
      };

       skip_white_space_and_comments is;

       let c = IO.peek_char is 0 in

       if c >= '0' && c <= '9' then
         read_subsection ()
       else do
       {
         let trailer = read_dictionary file in

         match dict_lookup trailer "Prev" with
         [ Int   pos -> read_xrefs is file pos
         | Float pos -> read_xrefs is file (int_of_float pos)
         | _         -> ()
         ]
       }
    }
  }
};

value read_pdf_file filename = do
{
  try do
  {
    let is  = IO.make_rand_in_stream filename in
    let pdf = create_pdf is 1.4               in

    if IO.read_string is 5 <> "%PDF-" then
      pdf
    else do
    {
      let version = read_float is in

      iter (IO.size is - 5)

      where rec iter pos = do
      {
        if pos < 0 then
          pdf
        else if IO.peek_string is pos 7 = "trailer" then
          match read_trailer pos with
          [ None   -> iter (pos - 7)
          | Some x -> x
          ]
        else
          iter (pos - 1)
      }
      where read_trailer pos = do
      {
        IO.seek is pos;

        let trailer = read_dictionary pdf in

        let size = match dict_lookup trailer "Size" with
                   [ Int x   -> x
                   | Float x -> int_of_float x
                   | _       -> 0
                   ]
                   in

        let pdf =
        {
          file    = is;
          version = version;
          root    = dict_lookup trailer "Root";
          info    = dict_lookup trailer "Info";
          revs    = Array.make size (-1);
          xrefs   = Array.make size 0
        }
        in

        if size <= 0 || pdf.root = Null then
          None
        else do
        {
          skip_white_space_and_comments is;

          if IO.read_string is 9 <> "startxref" then
            None
          else do
          {
            let xref_pos = read_int is in

            skip_white_space is;

            if IO.read_string is 5 <> "%%EOF" then
              None
            else do
            {
              read_xrefs is pdf xref_pos;
(*
              for i = 0 to Array.length pdf.objects.ids - 1 do
              {
                if pdf.objects.ids.(i) >= 0 && pdf.values.(i) = Null then
                  load_object pdf i
                else ()
              };
*)
              Some pdf
            }
          }
        }
      }
    }
  }
  with [ _ -> create_pdf (IO.make_buffer_stream 0x10 :> IO.irstream) 1.0 ]
};

(* writing PDF *)

value write_bool os x = do
{
  if x then
    IO.write_string os "true\n"
  else
    IO.write_string os "false\n"
};

value write_int os x = do
{
  IO.printf os "%d\n" x
};

value write_float os x = do
{
  IO.printf os "%f\n" x
};

value write_string os str = do
{
  IO.seek str 0;
  IO.write_string os "(";

  while not (IO.eof str) do
  {
    match IO.read_char str with
    [ '('  -> IO.write_string os "\\("
    | ')'  -> IO.write_string os "\\)"
    | '\\' -> IO.write_string os "\\\\"
    |  c   -> IO.write_char os c
    ]
  };
  IO.write_string os ")\n"
};

value write_symbol os sym = do
{
  IO.write_string os "/";

  for i = 0 to String.length sym - 1 do
  {
    let c = sym.[i] in

    if is_special c || is_white_space c || c = '#' then
      IO.printf os "#%2x" (int_of_char c)
    else
      IO.write_char os c
  };
  IO.write_string os "\n"
};

value write_reference os id rev = do
{
  write_int os id;
  write_int os rev;
  IO.write_string os "R\n"
};

value rec write_array os arr = do
{
  IO.write_string os "[ ";
  List.iter (write_value os) arr;
  IO.write_string os "]\n";
}
and write_dictionary os dict = do
{
  IO.write_string os "<< ";
  List.iter
    (fun (k,v) -> do
     {
       if v <> Null then do
       {
         write_symbol os k;
         write_value  os v
       }
       else ()
     })
    dict;
  IO.write_string os ">>\n";
}
and write_stream os dict str = do
{
  IO.seek str 0;

  let compress = IO.size str > 256 in
  let data     = if compress then
                   IO.compress str 9
                 else
                   str
                 in
  let len      = IO.size data in

  iter [] dict

  where rec iter result dict = match dict with
  [ [] -> do
    {
      write_dictionary os
        [("Length", Int len);
         ("Filter", if compress then
                      Symbol "FlateDecode"
                    else
                      Null)
        :: result]
    }
  | [(k, v) :: kvs] -> do
    {
      if k = "Length" || k = "Filter" then
        iter result kvs
      else
        iter [(k, v) :: result] kvs
     }
  ];

  IO.write_string os "stream\n";

  IO.seek data 0;

  IO.append os data;

  IO.write_string os "endstream\n"
}
and write_value os val = match val with
[ Null          -> IO.write_string os "null\n"
| Bool x        -> write_bool os x
| Int x         -> write_int os x
| Float x       -> write_float os x
| String x      -> write_string os x
| Stream d x    -> write_stream os d x
| Symbol x      -> write_symbol os x
| Array x       -> write_array os x
| Dictionary x  -> write_dictionary os x
| Reference x y -> write_reference os x y
];

value write_object os id rev val = do
{
  write_int os id;
  write_int os rev;

  IO.write_string os "obj ";
  write_value os val;
  IO.write_string os "endobj\n"
};

value set_object pdf id val = do
{
  let new_rev = pdf.revs.(id) + 1 in

  pdf.revs.(id)  := new_rev;
  pdf.xrefs.(id) := IO.bytes_written pdf.file;

  write_object pdf.file id new_rev val
};

value create_pdf_file filename version = do
{
  let pdf = create_pdf (IO.make_out_stream filename) version in

  IO.printf pdf.file "%%PDF-%s\n%%\255\255\255\255\n" (string_of_float version);

  pdf
};

value finish_pdf_file pdf = do
{
  let rec count_objects last n = do
  {
    if n >= Array.length pdf.revs then
      (last + 1)
    else if pdf.revs.(n) < 0 then
      count_objects last (n+1)
    else
      count_objects n    (n+1)
  }
  in

  let num_objects = count_objects 0 0 in

  let rec next_free i = do
  {
    if i >= num_objects then
      0
    else if pdf.revs.(i) < 0 then
      i
    else
      next_free (i+1)
  }
  in

  let xref_pos = IO.bytes_written pdf.file in

  IO.printf pdf.file "xref\n0 %d\n" num_objects;

  for n = 0 to num_objects - 1 do
  {
    if pdf.revs.(n) < 0 then
      IO.printf pdf.file "%010d %05d f \n" (next_free (n+1)) (if n = 0 then 65535 else 0)
    else
      IO.printf pdf.file "%010d %05d n \n" pdf.xrefs.(n) pdf.revs.(n);
  };

  IO.write_string pdf.file "trailer\n";

  write_dictionary pdf.file
     [("Size", Int num_objects);
      ("Root", pdf.root);
      ("Info", pdf.info)];

  IO.printf pdf.file "startxref\n%d\n%%%%EOF" xref_pos
};

