
open XNum;
open Substitute;
open FontMetric;

module LigKern =
struct

type lig_kern_cmd =
[ LigCmd  of int and int and int and int
| KernCmd of int and int and num
];

(* access methods *)

value is_lig lk = match lk with
[ LigCmd _ _ _ _ -> True
| KernCmd _ _ _  -> False
];

value is_kern lk = match lk with
[ LigCmd _ _ _ _ -> False
| KernCmd _ _ _  -> True
];

value skip lk = match lk with
[ LigCmd s _ _ _ -> s
| KernCmd s _ _  -> s
];

value next lk = match lk with
[ LigCmd _ n _ _ -> n
| KernCmd _ n _  -> n
];

value operand lk = match lk with
[ LigCmd _ _ o _ -> o
| KernCmd _ _ _  -> raise (Invalid_argument "LigKern.operand applied to kern!")
];

value remainder lk = match lk with
[ LigCmd _ _ _ r -> r
| KernCmd _ _ _  -> raise (Invalid_argument "LigKern.remainder applied to kern!")
];

value kern lk = match lk with
[ LigCmd _ _ _ _ -> raise (Invalid_argument "LigKern.kern applied to ligature!")
| KernCmd _ _ k  -> k
];

(* |next_lig_kern| can be used to enumerate all lig-kern pairs. *)

value next_lig_kern lk_array pos = do
{
  let lk = lk_array.(pos) in
  let s  = skip lk in
  let n  = next lk in

  let next_pos = if s < 128 then
    pos + s + 1
  else
    -1
  in

  if s <= 128 then do
  {
    if is_lig lk then do
    {
      let op = operand lk in

      (next_pos, n,
       Ligature (remainder lk) (op lsr 2) ((op lsr 1) land 1 = 1) (op land 1 = 1))
    }
    else
      (next_pos, n, Kern (kern lk))
  }
  else
    (-1, -1, NoLigKern)
};

(*
  Determines the ligature or kerning of two characterpos. lk_array is the array of LigKern commands,
  pos the position corresponding to the current character, and next_char the next character.
*)

value rec get_lig_kern lk_array pos next_char = do
{
  let lk = lk_array.(pos) in
  let s  = skip lk in
  let n  = next lk in

  if (n = next_char) && (s <= 128) then
    if is_lig lk then do
    {
      let op = operand lk in

      Ligature (remainder lk) (op lsr 2) ((op lsr 1) land 1 = 1) (op land 1 = 1)
    }
    else
      Kern (kern lk)
  else if s < 128 then
    get_lig_kern lk_array (pos + s + 1) next_char
  else
    NoLigKern
};

end;

value num_0x100     = num_of_int 0x100;
value num_0x10000   = num_of_int 0x10000;
value num_0x100000  = num_of_int 0x100000;
value num_0x1000000 = num_of_int 0x1000000;

value read_fix ic = IO.read_be_i32 ic // num_0x100000;

value read_4 ic = do
{
  let x1 = IO.read_be_u8 ic in
  let x2 = IO.read_be_u8 ic in
  let x3 = IO.read_be_u8 ic in
  let x4 = IO.read_be_u8 ic in

  (x1, x2, x3, x4)
};

value read_array ic read_fun len = do
{
  if len <= 0 then
    [| |]
  else do
  {
    let a = Array.make len (read_fun ic) in

    for i = 1 to len - 1 do
    {
      a.(i) := read_fun ic
    };

    a
  }
};

value tfm_kerning lig_kern font c1 c2 = do
{
  match (get_glyph_metric font (Simple c1)).gm_extra with
  [ GXI_LigKern lk -> LigKern.get_lig_kern lig_kern lk c2
  | _              -> NoLigKern
  ];
};

(*
value make_ligature_trie first_char metrics lig_kern = do
{
  let (_, trie) =
    Array.fold_left
      (fun (i, trie) gm -> match gm.gm_extra with
        [ GXI_LigKern lk -> do
          {
            iter lk trie

            where rec iter lk trie = match LigKern.next_lig_kern lig_kern lk with
            [ (_, _, LigKern.None)                 -> (i+1, trie)
            | (next, glyph, LigKern.Lig c s k1 k2) -> do
              {
                let repl = if k1 then [ConstGlyph (Simple i)] else []
                         @ [ConstGlyph (Simple c)]
                         @ if k2 then [ConstGlyph (Simple glyph)] else []
                in

                iter
                  next
                  (GlyphTrie.add_list
                    [Simple i; Simple glyph]
                    (repl, s)
                    trie)
              }
            | (next, _, _) -> iter next trie
            ]
          }
        | _ -> (i+1, trie)
        ])
      (first_char, GlyphTrie.empty)
      metrics
  in

  trie
};

value make_kerning_trie first_char metrics lig_kern = do
{
  GlyphTrie.empty
};
*)

value get_glyph_bitmap bitmaps fm code = do
{
  if !bitmaps = None then do
  {
    match FontPK.read_pk_font fm !default_bitmap_resolution with
    [ None         -> ()
    | Some (_, gs) -> !bitmaps := Some gs
    ]
  }
  else ();

  match !bitmaps with
  [ None    -> Glyph.empty_glyph
  | Some bm -> bm.(code - fm.first_glyph)
  ]
};

value make_lig_kern kern (x1,x2,x3,x4) = do
{
  if x3 < 128 then
    LigKern.LigCmd x1 x2 x3 x4
  else
    LigKern.KernCmd x1 x2 kern.(0x100 * (x3 - 128) + x4)
};

value make_glyph_metric width height depth italic lig exten (w,x1,x2,r) = do
{
  let h  = x1 lsr 4    in
  let d  = x1 land 0xf in
  let i  = x2 lsr 2    in
  let t  = x2 land 0x3 in

  (* If italic.(i) = 0 then we do not need to allocate a new structure. *)
  let kern_info = if italic.(i) <>/ num_zero then
    {
      (zero_kern_info) with ki_before_foreign = italic.(i)
    }
  else
    zero_kern_info
  in

  match t with
  [ 0 -> {
           gm_width      = width.(w);
           gm_height     = height.(h);
           gm_depth      = depth.(d);
           gm_italic     = italic.(i);
           gm_extra      = GXI_Normal;
           gm_extra_kern = kern_info
         }
  | 1 -> let lk = lig.(r) in
         {
           gm_width      = width.(w);
           gm_height     = height.(h);
           gm_depth      = depth.(d);
           gm_italic     = italic.(i);
           gm_extra      = GXI_LigKern
                             (if LigKern.is_lig lk && LigKern.skip lk > 128 then
                                256 * LigKern.operand lk + LigKern.remainder lk
                              else
                                r
                             );
           gm_extra_kern = kern_info
         }
  | 2 -> {
           gm_width      = width.(w);
           gm_height     = height.(h);
           gm_depth      = depth.(d);
           gm_italic     = italic.(i);
           gm_extra      = GXI_List r;
           gm_extra_kern = kern_info
         }
  | _ -> let (t,m,b,r) = exten.(r) in
         {
           gm_width      = width.(w);
           gm_height     = height.(h);
           gm_depth      = depth.(d);
           gm_italic     = italic.(i);
           gm_extra      = GXI_Extendable t m b r;
           gm_extra_kern = kern_info
         }
  ]
};

value tfm_composer fm _ _ = do
{
  simple_composer fm (simple_ligature_substitution fm)
};

value read_tfm file name size = do
{
  let ic = IO.make_in_stream file in

  let _file_length  = IO.read_be_u16 ic in
  let header_length = IO.read_be_u16 ic in
  let first_char    = IO.read_be_u16 ic in
  let last_char     = IO.read_be_u16 ic in

  let glyph_metric_table_len = last_char - first_char + 1 in

  let width_table_len  = IO.read_be_u16 ic in
  let height_table_len = IO.read_be_u16 ic in
  let depth_table_len  = IO.read_be_u16 ic in
  let italic_table_len = IO.read_be_u16 ic in
  let lig_table_len    = IO.read_be_u16 ic in
  let kern_table_len   = IO.read_be_u16 ic in
  let ext_table_len    = IO.read_be_u16 ic in
  let param_table_len  = IO.read_be_u16 ic in

  let check_sum   = IO.read_be_u32 ic in
  let design_size = read_fix ic       in

  IO.skip ic (4 * header_length - 8);

  let glyph_metric = read_array ic read_4   glyph_metric_table_len in
  let width     = Array.map (fun x -> x */ size) (read_array ic read_fix width_table_len)  in
  let height    = Array.map (fun x -> x */ size) (read_array ic read_fix height_table_len) in
  let depth     = Array.map (fun x -> x */ size) (read_array ic read_fix depth_table_len)  in
  let italic    = Array.map (fun x -> x */ size) (read_array ic read_fix italic_table_len) in
  let lig       = read_array ic read_4   lig_table_len       in
  let kern      = Array.map (fun x -> x */ size) (read_array ic read_fix kern_table_len)   in
  let ext       = read_array ic read_4   ext_table_len       in
  let param     = read_array ic read_fix param_table_len     in

  let lig_cmds  = Array.map (fun x -> make_lig_kern kern x) lig in
  let gm_table  = Array.map (fun x -> make_glyph_metric width height depth italic lig_cmds ext x) glyph_metric in

  {
    name                = name;
    ps_name             = name;
    file_name           = file;
    font_type           = Other;
    first_glyph         = first_char;
    last_glyph          = last_char;
    design_size         = design_size;
    at_size             = size;
    check_sum           = check_sum;
    get_glyph           = Encodings.raw_encoding;
    get_unicode         = Encodings.raw_decoding;
    draw_simple_glyph   = draw_simple_glyph;
    accent_base_point   = accent_base_point_x_height;
    accent_attach_point = accent_attach_point_top;
    get_composer        = tfm_composer;
    kerning             = tfm_kerning lig_cmds;
    get_glyph_bitmap    = (get_glyph_bitmap (ref None));
    get_glyph_name      = (fun g -> Printf.sprintf "c%d" g);
    parameter           =
      {
        hyphen_glyph     = Simple 45;
        skew_glyph       = Undef;
        slant            = if param_table_len >  0 then param.( 0) else num_zero;
        space            = if param_table_len >  1 then size */ param.( 1) else num_zero;
        space_stretch    = if param_table_len >  2 then size */ param.( 2) else num_zero;
        space_shrink     = if param_table_len >  3 then size */ param.( 3) else num_zero;
        x_height         = if param_table_len >  4 then size */ param.( 4) else num_zero;
        quad             = if param_table_len >  5 then size */ param.( 5) else num_zero;
        extra_space      = if param_table_len >  6 then size */ param.( 6) else num_zero;
        num_shift_1      = if param_table_len >  7 then size */ param.( 7) else num_zero;
        num_shift_2      = if param_table_len >  8 then size */ param.( 8) else num_zero;
        num_shift_3      = if param_table_len >  9 then size */ param.( 9) else num_zero;
        denom_shift_1    = if param_table_len > 10 then size */ param.(10) else num_zero;
        denom_shift_2    = if param_table_len > 11 then size */ param.(11) else num_zero;
        super_shift_1    = if param_table_len > 12 then size */ param.(12) else num_zero;
        super_shift_2    = if param_table_len > 13 then size */ param.(13) else num_zero;
        super_shift_3    = if param_table_len > 14 then size */ param.(14) else num_zero;
        sub_shift_1      = if param_table_len > 15 then size */ param.(15) else num_zero;
        sub_shift_2      = if param_table_len > 16 then size */ param.(16) else num_zero;
        super_drop       = if param_table_len > 17 then size */ param.(17) else num_zero;
        sub_drop         = if param_table_len > 18 then size */ param.(18) else num_zero;
        delim_1          = if param_table_len > 19 then size */ param.(19) else num_zero;
        delim_2          = if param_table_len > 20 then size */ param.(20) else num_zero;
        axis_height      = if param_table_len > 21 then size */ param.(21) else num_zero;
        rule_thickness   = if param_table_len >  7 then size */ param.( 7) else num_zero;
        big_op_spacing_1 = if param_table_len >  8 then size */ param.( 8) else num_zero;
        big_op_spacing_2 = if param_table_len >  9 then size */ param.( 9) else num_zero;
        big_op_spacing_3 = if param_table_len > 10 then size */ param.(10) else num_zero;
        big_op_spacing_4 = if param_table_len > 11 then size */ param.(11) else num_zero;
        big_op_spacing_5 = if param_table_len > 12 then size */ param.(12) else num_zero
      };
    glyph_metric = gm_table
  }
};

