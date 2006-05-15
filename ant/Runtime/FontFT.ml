
open XNum;
open Maps;
open Unicode;
open Substitute;
open GlyphMetric;
open FontMetric;
open FreeType;
open OpenType;

value ft_kerning face scale c1 c2 = do
{
  let (x,y) = ft_get_kerning face c1 c2 ft_kerning_unscaled in

  if x <> 0 then
    Kern (scale */ num_of_int x)
  else
    NoLigKern
};

value make_glyph_metric params scale glyph = do
{
  let (width, height, h_off, v_off, adv) = glyph_metrics glyph in

  let left_bound  = max 0 (~-h_off)             in
  let right_bound = max 0 (h_off + width - adv) in

  let l = scale */ num_of_int left_bound  in
  let r = scale */ num_of_int right_bound in

  (* If these values are zero we do not need to allocate a new structure. *)
  let kern_info = if left_bound <> 0 || right_bound <> 0 then
    {
      (zero_kern_info)
      with
      ki_after_foreign  = l;
      ki_before_foreign = r
    }
  else
    zero_kern_info
  in

  {
    gm_width      = scale */ num_of_int adv +/ num_two */ params.flp_letter_spacing;
    gm_height     = scale */ num_of_int v_off;
    gm_depth      = scale */ num_of_int (height - v_off);
    gm_italic     = r;
    gm_extra      = GXI_Normal;
    gm_extra_kern = kern_info
  }
};

value get_glyph_metric params scale face = do
{
  let num_glyphs = face_num_glyphs face                     in
  let gm         = Array.make num_glyphs empty_glyph_metric in

  for i = 1 to num_glyphs - 1 do
  {
    ft_load_glyph face i (ft_load_no_hinting + ft_load_no_scale + ft_load_linear_design);

    gm.(i - 1) := make_glyph_metric params scale (face_glyph face)
  };

  gm
};

value get_glyph_bitmap face fm code = do
{
  let gm = fm.glyph_metric.(code - fm.first_glyph) in

  ft_set_char_size face fm.at_size !default_bitmap_resolution;
  ft_load_glyph face code ft_load_monochrome;

  let (x, y, b) = glyph_to_bitmap (face_glyph face) in
  let dpp       = num_of_ints 100 7227 */ num_of_int !default_bitmap_resolution in

  let g = GlyphBitmap.make
            code
            (int_of_num (round_num (gm.gm_width  */ dpp)))
            (int_of_num (round_num (gm.gm_height */ dpp)))
            (int_of_num (round_num (gm.gm_depth  */ dpp)))
            (float_of_num dpp)
            (float_of_num dpp)
            (x, y - b.Bitmap.bm_height + 1)
            (x + b.Bitmap.bm_width - 1, y)
  in

  { (g) with GlyphBitmap.g_bitmap = b }
};

value builtin_encoding face char = match ft_get_char_index face char with
[ 0 -> Undef
| g -> Simple g
];

value builtin_decoding face glyph = do
{
  let lookup glyph = do
  {
    iter (ft_get_first_char face)

    where rec iter (c,g) = do
    {
      if g = 0 then
        []
      else if g = glyph then
        [c]
      else
        iter (ft_get_next_char face (c,g))
    }
  }
  in

  Array.of_list (decode glyph)

  where rec decode glyph = match glyph with
  [ Undef              -> []
  | Border _           -> []
  | Simple g           -> lookup g
  | Accent a g         -> lookup a @ lookup g
  | Sequence gs        -> List.concat (List.map lookup gs)
  | Extendable t m b _ -> decode t @ decode m @ decode b
  ]
};

module Composer =
struct

module SymbolTrie = SymbolSet.SymbolTrie;

value empty_table = [];

value rec add_composer table script lang features composer = match table with
[ []              -> [(script, lang, SymbolTrie.add features composer SymbolTrie.empty)]
| [(s,l,m) :: xs] -> if s = script && l = lang then
                       [(s,l, SymbolTrie.add features composer m) :: xs]
                     else
                       add_composer xs script lang features composer
];

value rec lookup_composer table script lang features = match table with
[ []              -> raise Not_found
| [(s,l,m) :: xs] -> if s = script && l = lang then
                       SymbolTrie.find features m
                     else
                       lookup_composer xs script lang features
];

(* move the next glyph by |pos.p_x_off| and |pos.p_y_off| *)

value pos_to_pre_kern scale pos = do
{
  ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_x_off)
            (scale */ num_of_int pos.OTF_Pos_Subst.p_y_off)
};

(* increase the advance width of the preceding glyph by |pos.p_x_adv| *)

value pos_to_post_kern scale pos = do
{
  (* FIX: Should this be |h_adv_off - x_off| ? *)
  ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_h_adv_off)
            num_zero
};

value position_to_adj scale p = do
{
  (single_positioning_cmd
    (scale */ num_of_int p.OTF_Pos_Subst.p_x_off)
    (scale */ num_of_int p.OTF_Pos_Subst.p_y_off)
    (scale */ num_of_int p.OTF_Pos_Subst.p_h_adv_off),
   0)
};

value kern_to_adj scale p1 p2 = do
{
  (pair_positioning_cmd
    (scale */ num_of_int p1.OTF_Pos_Subst.p_x_off)
    (scale */ num_of_int p1.OTF_Pos_Subst.p_y_off)
    (scale */ num_of_int (p1.OTF_Pos_Subst.p_h_adv_off + p2.OTF_Pos_Subst.p_x_off))
    (scale */ num_of_int p2.OTF_Pos_Subst.p_y_off)
    (scale */ num_of_int p2.OTF_Pos_Subst.p_h_adv_off),
   1)
};

value substitution_to_adj g = do
{
  (replace_with_single_glyph_cmd 1 (Simple g),
   0)
};

value multi_subst_to_adj glyphs = do
{
  (replace_with_multiple_glyphs_cmd 1 (Array.map (fun g -> Simple g) glyphs),
    0)
};

value ligature_to_adj n lig = do
{
  (replace_with_single_glyph_cmd n (Simple lig),
   0)
};

value pos_rule_to_adj scale glyphs rule = do
{
  let lookups = Array.make (Array.length glyphs) [||] in

  Array.iter
    (fun l -> do
      {
        lookups.(l.OTF_Pos_Subst.prr_seq_idx) :=
          l.OTF_Pos_Subst.prr_lookup.OTF_Pos_Subst.l_commands
      })
    rule;

  let adjs = ListBuilder.make () in

  ListBuilder.add adjs (CopyCommands 0 0);

  for i = 0 to Array.length glyphs - 1 do
  {
    let cmds = lookups.(i) in

    iter_cmds 0

    where rec iter_cmds k = do
    {
      if k >= Array.length cmds then do
      {
        ListBuilder.add_list adjs
          [ConstGlyph (Simple glyphs.(i));
           CopyCommands (i+1) (i+1)];
      }
      else match cmds.(k) with
      [ OTF_Pos_Subst.Position pos -> do
        {
          try do
          {
            let p = IntMap.find glyphs.(i) pos in

            ListBuilder.add_list adjs
              [pos_to_pre_kern scale p;
               ConstGlyph (Simple glyphs.(i));
               pos_to_post_kern scale p;
               CopyCommands (i+1) (i+1)];
          }
          with
          [ Not_found -> iter_cmds (k+1) ]
        }
      | _ -> iter_cmds (k+1)
      ]
    }
  };

  (ListBuilder.get adjs, 0)
};

value subst_rule_to_adj glyphs rule = do
{
  let lookups = Array.make (Array.length glyphs) [||] in

  Array.iter
    (fun l -> do
      {
        lookups.(l.OTF_Pos_Subst.prr_seq_idx) :=
          l.OTF_Pos_Subst.prr_lookup.OTF_Pos_Subst.l_commands
      })
    rule;

  let adjs = ListBuilder.make () in

  ListBuilder.add adjs (CopyCommands 0 0);

  for i = 0 to Array.length glyphs - 1 do
  {
    let cmds = lookups.(i) in

    iter_cmds 0

    where rec iter_cmds k = do
    {
      if k >= Array.length cmds then do
      {
        ListBuilder.add_list adjs
          [ConstGlyph (Simple glyphs.(i));
           CopyCommands (i+1) (i+1)];
      }
      else match cmds.(k) with
      [ OTF_Pos_Subst.Substitution subst -> do
        {
          try do
          {
            let g2 = IntMap.find glyphs.(i) subst in

            ListBuilder.add_list adjs
              [ConstGlyph (Simple g2);
               CopyCommands (i+1) (i+1)];
          }
          with
          [ Not_found -> iter_cmds (k+1) ]
        }
      | _ -> iter_cmds (k+1)
      ]
    }
  };

  (ListBuilder.get adjs, 0)
};

value pos_subst_to_adjsutment scale cmd = match cmd with
[ OTF_Pos_Subst.NoCommand    -> NoAdjustment
| OTF_Pos_Subst.Position pos -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g p trie -> DynUCTrie.add_list [g] (position_to_adj scale p) trie)
        pos
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.CursiveAnchors entry exit -> NoAdjustment (* FIX *)
| OTF_Pos_Subst.MarkToBaseAnchors _ _
| OTF_Pos_Subst.MarkToLigAnchors  _ _
| OTF_Pos_Subst.MarkToMarkAnchors _ _ -> NoAdjustment
| OTF_Pos_Subst.Kern kerns -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g1 m trie ->
          IntMap.fold
            (fun g2 (p1,p2) trie ->
              DynUCTrie.add_list
                [g1; g2]
                (kern_to_adj scale p1 p2)
                trie)
            m
            trie)
        kerns
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.KernClass n classes1 classes2 pos1 pos2 -> do
  {
    ClassPairLookup n classes1 classes2
      (Array.init (Array.length pos1)
        (fun i -> kern_to_adj scale pos1.(i) pos2.(i)))
  }
| OTF_Pos_Subst.Substitution subst -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g s trie -> DynUCTrie.add_list [g] (substitution_to_adj s) trie)
        subst
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.Multiple map -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g s trie -> DynUCTrie.add_list [g] (multi_subst_to_adj s) trie)
        map
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.Alternate map -> NoAdjustment (* FIX *)
| OTF_Pos_Subst.Ligature ligs -> do
  {
    DirectLookup
      (DynUCTrie.mapi
        (fun gs s -> ligature_to_adj (Array.length gs) s)
        ligs)
  }
| OTF_Pos_Subst.ContextGlyphPos rules -> do
  {
    DirectLookup
      (DynUCTrie.mapi
        (fun gs r -> pos_rule_to_adj scale gs r)
        rules)
  }
| OTF_Pos_Subst.ContextGlyphSubst rules -> do
  {
    DirectLookup
      (DynUCTrie.mapi
        (fun gs r -> subst_rule_to_adj gs r)
        rules)
  }
(*
| OTF_Pos_Subst.ContextClassPos      of IntMap.t int and array (pos_subst_rule (array int))
| OTF_Pos_Subst.ContextClassSubst    of IntMap.t int and array (pos_subst_rule (array int))
| OTF_Pos_Subst.ContextCoveragePos   of array (pos_subst_rule (array (array int)))
| OTF_Pos_Subst.ContextCoverageSubst of array (pos_subst_rule (array (array int)))
| OTF_Pos_Subst.ChainGlyphPos        of (array (pos_subst_rule (array int * array int * array int)))
| OTF_Pos_Subst.ChainGlyphSubst      of (array (pos_subst_rule (array int * array int * array int)))
| OTF_Pos_Subst.ChainClassPos        of IntMap.t int and IntMap.t int and IntMap.t int and
                                        array (pos_subst_rule (array int * array int * array int))
| OTF_Pos_Subst.ChainClassSubst      of IntMap.t int and IntMap.t int and IntMap.t int and
                                        array (pos_subst_rule (array int * array int * array int))
| OTF_Pos_Subst.ChainCoveragePos     of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| OTF_Pos_Subst.ChainCoverageSubst   of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| OTF_Pos_Subst.ReverseSubst         of array int and array int and array (array int) and array (array int)
*)
| _ -> NoAdjustment
];

value lookup_to_adjustment scale lookups = do
{
  List.map
    (fun l -> Array.map
                (pos_subst_to_adjsutment scale)
                l.OTF_Pos_Subst.l_commands)
    lookups
};

value make_matcher memo_table scale pos_subst script features = do
{
  let s = UString.uc_string_to_ascii script in

  let (s_tag, l_tag) =
    if Array.length script = 9 && s.[4] = '.' then
      (Tag.make_tag (String.sub s 0 4),
       Tag.make_tag (String.sub s 5 4))
    else if Array.length script = 4 then
      (Tag.make_tag s, Tag.dflt_tag)
    else
      (Tag.latn_tag, Tag.dflt_tag)
  in

  let trie =
    try
      lookup_composer !memo_table s_tag l_tag features
    with
    [ Not_found -> do
      {
        (* FIX: instead of translating the lookups to adjustments every time, do it just once *)
        let f_tags = SymbolSet.fold
                       (fun set f ->
                         Tag.TagSet.add
                           (Tag.make_tag_uc (SymbolTable.symbol_to_string f))
                           set)
                       Tag.TagSet.empty
                       features
                     in
        let lookups = OTF_Pos_Subst.get_lookups pos_subst s_tag l_tag f_tags in
        let adj     = lookup_to_adjustment scale lookups in

        let max_depth = max2_adjustment_depth adj in

        let is_empty (n, _)        = (n > max_depth)      in
        let prefix (n, glyphs) g   = (n+1, [g :: glyphs]) in
        let root_value (_, glyphs) =
          lookup2_adjustments adj (List.rev glyphs)
        in

        let trie = (is_empty, prefix, root_value) in

        !memo_table := add_composer !memo_table s_tag l_tag features trie;

        trie
      }
    ]
  in

  match_substitution_trie trie (0, [])
};

value get_composer pos_subst p_table s_table scale = match pos_subst with
[ (None,   None)   -> fun fm _   _    -> simple_composer    fm (simple_ligature_substitution fm)
| (None,   Some s) -> fun fm scr feat -> simple_composer    fm (make_matcher s_table scale s scr feat)
| (Some p, None)   -> fun fm scr feat -> simple_composer    fm (make_matcher p_table scale p scr feat)
| (Some p, Some s) -> fun fm scr feat -> two_phase_composer fm (make_matcher s_table scale s scr feat)
                                                               (make_matcher p_table scale p scr feat)
];

end;

value read_ft file name params = do
{
  let face = ft_new_face file in

  match ft_get_module_name face with
  [ "type1" -> do
    {
      (* look for an afm file *)

      if String.length file >= 4 then
        ignore (ft_attach_file face (String.sub file 0 (String.length file - 4) ^ ".afm"))
      else ();

      ft_attach_file face (file ^ ".afm");
      ()
    }
  | _ -> ()
  ];

  let (em, asc, desc, _height, _ul_pos, _ul_thick) =
    face_metrics face
  in

  let (tables, pos_subst) = try do
    {
      let tables    = read_font_tables file in
      let pos_subst = get_pos_subst tables  in

      (tables, pos_subst)
    }
    with
    [ _ -> (Tag.TagMap.empty, (None, None)) ]
  in

  let font_type    = match ft_get_module_name face with
                     [ "type1"    -> PostScript
                     | "truetype" -> if is_cff tables then
                                       OpenTypeCFF
                                     else
                                       TrueType
                     | "cff"       -> OpenTypeCFF
                     | _           -> Other
                     ]
                     in

  let size         = params.flp_size                    in
  let scale        = size // num_of_int em              in
  let design_size  = scale */ num_of_int (asc - desc)   in
  let glyph_metric = get_glyph_metric params scale face in
  let space_glyph  = ft_get_char_index face 32          in
  let x_glyph      = ft_get_char_index face 102         in
  let m_glyph      = ft_get_char_index face 77          in
  let space        = if space_glyph > 0 then                               (* width of " "  *)
                       glyph_metric.(space_glyph - 1).gm_width
                     else
                       size // num_of_int 3
                     in
  let x_height     = if x_glyph > 0 then                                   (* height of "x" *)
                       glyph_metric.(x_glyph - 1).gm_width
                     else
                       size // num_of_int 2
                     in
  let quad         = if m_glyph > 0 then                                   (* width of "M"  *)
                       glyph_metric.(m_glyph - 1).gm_width
                     else
                       size
                     in
  let hyphen_glyph = match params.flp_hyphen_glyph with
                     [ Undef -> builtin_encoding face 45
                     | h     -> h
                     ]
                     in

  let (enc,dec) = match params.flp_encoding with
  [ [| |] -> (builtin_encoding face,
              builtin_decoding face)
  | m     -> (Encodings.charmap_encoding (Encodings.fake_encoding m),
              Encodings.array_decoding m)
  ]
  in

  let s_table         = ref Composer.empty_table in
  let p_table         = ref Composer.empty_table in
  let composer fm s f = Composer.get_composer pos_subst p_table s_table scale fm s f in

  {
    name                = name;
    ps_name             = ft_get_postscript_name face;
    file_name           = file;
    font_type           = font_type;
    first_glyph         = 1;
    last_glyph          = face_num_glyphs face - 1;
    design_size         = design_size;
    at_size             = size;
    check_sum           = num_zero;
    get_glyph           = enc;
    get_unicode         = dec;
    get_composer        = composer;
    kerning             = fun _ c1 c2 -> ft_kerning face scale c1 c2;
    draw_simple_glyph   = draw_simple_glyph;
    accent_base_point   = accent_base_point_x_height;
    accent_attach_point = accent_attach_point_top;
    get_glyph_bitmap    = get_glyph_bitmap face;
    get_glyph_name      = ft_get_glyph_name face;
    parameter           =
      {
        hyphen_glyph     = hyphen_glyph;
        skew_glyph       = params.flp_skew_glyph;
        slant            = num_zero;
        space            = space;
        space_stretch    = space // num_of_int 2;
        space_shrink     = space // num_of_int 3;
        x_height         = x_height;
        quad             = quad;
        extra_space      = num_zero;
        num_shift_1      = num_zero;
        num_shift_2      = num_zero;
        num_shift_3      = num_zero;
        denom_shift_1    = num_zero;
        denom_shift_2    = num_zero;
        super_shift_1    = num_zero;
        super_shift_2    = num_zero;
        super_shift_3    = num_zero;
        sub_shift_1      = num_zero;
        sub_shift_2      = num_zero;
        super_drop       = num_zero;
        sub_drop         = num_zero;
        delim_1          = num_zero;
        delim_2          = num_zero;
        axis_height      = num_zero;
        rule_thickness   = num_zero;
        big_op_spacing_1 = num_zero;
        big_op_spacing_2 = num_zero;
        big_op_spacing_3 = num_zero;
        big_op_spacing_4 = num_zero;
        big_op_spacing_5 = num_zero
      };
    glyph_metric = glyph_metric
  }
};

