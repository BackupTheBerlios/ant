
open XNum;
open Runtime;
open Logging;
open VM;
open Typesetting;
open Engine;
open ParseState;
open ALCoding;
open ALEnvironment;
open ALDim;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* gfx-commands *)

value encode_gfx_cmd cmd = match cmd with
[ Graphic.PutBox x y b  -> Types.Tuple [|ref (Types.Symbol sym_PutBox);
                                         ref (encode_dim_arg x);
                                         ref (encode_dim_arg y);
                                         ref Types.Unbound|]  (* FIX *)
| Graphic.Draw pc p     -> do
  {
    let sym = match pc with
    [ Graphic.Stroke -> sym_Stroke
    | Graphic.Fill   -> sym_Fill
    | Graphic.Clip   -> sym_Clip
    ]
    in

    Types.Tuple [|ref (Types.Symbol sym); ref (encode_path p)|]
  }
| Graphic.SetColour     c -> Types.Tuple [|ref (Types.Symbol sym_SetColour);
                                           ref (encode_colour c)|]
| Graphic.SetBgColour   c -> Types.Tuple [|ref (Types.Symbol sym_SetBgColour);
                                           ref (encode_colour c)|]
| Graphic.SetAlpha      a -> Types.Tuple [|ref (Types.Symbol sym_SetAlpha);
                                           ref (Types.Number a)|]
| Graphic.SetLineWidth  w -> Types.Tuple [|ref (Types.Symbol sym_SetLineWidth);
                                           ref (Types.Number w)|]
| Graphic.SetLineCap    c -> do
  {
    let cap = match c with
    [ Graphic.Butt   -> sym_Butt
    | Graphic.Circle -> sym_Circle
    | Graphic.Square -> sym_Square
    ]
    in

    Types.Tuple [|ref (Types.Symbol sym_SetLineCap);
                  ref (Types.Symbol cap)|]
  }
| Graphic.SetLineJoin   j -> do
  {
    let join = match j with
    [ Graphic.Miter -> sym_Miter
    | Graphic.Round -> sym_Round
    | Graphic.Bevel -> sym_Bevel
    ]
    in

    Types.Tuple [|ref (Types.Symbol sym_SetLineJoin);
                  ref (Types.Symbol join)|]
  }
| Graphic.SetMiterLimit l -> Types.Tuple [|ref (Types.Symbol sym_SetMiterLimit);
                                           ref (Types.Number l)|]
]
where rec encode_path path = match path with
[ [] -> Types.Nil
| [(ax,ay,bx,by,cx,cy,dx,dy) :: ps] ->
  Types.List
    (ref (Types.Tuple [|ref (encode_dim_arg ax); ref (encode_dim_arg ay);
                        ref (encode_dim_arg bx); ref (encode_dim_arg by);
                        ref (encode_dim_arg cx); ref (encode_dim_arg cy);
                        ref (encode_dim_arg dx); ref (encode_dim_arg dy)|]))
    (ref (encode_path ps))
];

value decode_gfx_cmd name cmd = do
{
  let arr = decode_tuple name cmd in

  if Array.length arr < 1 then
    Types.runtime_error (name ^ ": invalid argument")
  else do
  {
    let s = decode_symbol name arr.(0) in

    if s = sym_PutBox then do
    {
      if Array.length arr <> 4 then
        Types.runtime_error (name ^ ": PutBox expects 3 arguments")
      else
        Graphic.PutBox (decode_dim_arg name arr.(1)) (decode_dim_arg name arr.(2))
          Box.empty_box (* FIX *)
    }
    else if s = sym_Stroke then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": Stroke expects 1 argument")
      else
        Graphic.Draw Graphic.Stroke (decode_path name arr.(1))
    }
    else if s = sym_Fill then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": Fill expects 1 argument")
      else
        Graphic.Draw Graphic.Fill (decode_path name arr.(1))
    }
    else if s = sym_Clip then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": Fill expects 1 argument")
      else
        Graphic.Draw Graphic.Clip (decode_path name arr.(1))
    }
    else if s = sym_SetColour then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetColour expects 1 argument")
      else
        Graphic.SetColour (decode_colour name arr.(1))
    }
    else if s = sym_SetBgColour then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetBgColour expects 1 argument")
      else
        Graphic.SetBgColour (decode_colour name arr.(1))
    }
    else if s = sym_SetAlpha then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetAlpha expects 1 argument")
      else
        Graphic.SetAlpha (Machine.evaluate_num name arr.(1))
    }
    else if s = sym_SetLineWidth then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetLineWidth expects 1 argument")
      else
        Graphic.SetLineWidth (Machine.evaluate_num name arr.(1))
    }
    else if s = sym_SetLineCap then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetLineCap expects 1 argument")
      else do
      {
        let s = decode_symbol name arr.(1) in

        if s = sym_Butt then
          Graphic.SetLineCap Graphic.Butt
        else if s = sym_Circle then
          Graphic.SetLineCap Graphic.Circle
        else if s = sym_Square then
          Graphic.SetLineCap Graphic.Square
        else
          Types.runtime_error (name ^ ": invalid line cap")
      }
    }
    else if s = sym_SetLineJoin then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetLineJoin expects 1 argument")
      else do
      {
        let s = decode_symbol name arr.(1) in

        if s = sym_Miter then
          Graphic.SetLineJoin Graphic.Miter
        else if s = sym_Round then
          Graphic.SetLineJoin Graphic.Round
        else if s = sym_Bevel then
          Graphic.SetLineJoin Graphic.Bevel
        else
          Types.runtime_error (name ^ ": invalid line join")
      }
    }
    else if s = sym_SetMiterLimit then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetMiterLimit expects 1 argument")
      else
        Graphic.SetMiterLimit (Machine.evaluate_num name arr.(1))
    }
    else
      Types.runtime_error (name ^ ": invalid graphics command")
  }
}
where rec decode_path name path = do
{
  List.map
    (fun x -> match decode_tuple name x with
      [ [|ax; ay; bx; by; cx; cy; dx; dy|] -> do
        {
          (decode_dim_arg name ax, decode_dim_arg name ay,
           decode_dim_arg name bx, decode_dim_arg name by,
           decode_dim_arg name cx, decode_dim_arg name cy,
           decode_dim_arg name dx, decode_dim_arg name dy)
        }
      | _ -> Types.runtime_error (name ^ ": invalid path segment")
      ])
    (Machine.evaluate_list name path)
};

