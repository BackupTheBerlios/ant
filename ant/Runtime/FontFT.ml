
open XNum;
open Unicode;
open Substitute;
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

value make_glyph_metric scale glyph = do
{
  let (width, height, h_off, v_off, adv) = glyph_metrics glyph in
  let i = h_off + width - adv in

  {
    gm_width  = scale */ num_of_int adv;
    gm_height = scale */ num_of_int v_off;
    gm_depth  = scale */ num_of_int (height - v_off);
    gm_italic = scale */ num_of_int (if i > 0 then i else 0);
    gm_extra  = GXI_Normal
  }
};

value get_glyph_metric scale face = do
{
  let num_glyphs = face_num_glyphs face in
  let gm         = Array.make
                     num_glyphs
                     {
                       gm_width  = num_zero;
                       gm_height = num_zero;
                       gm_depth  = num_zero;
                       gm_italic = num_zero;
                       gm_extra  = GXI_Normal
                     }
                   in

  for i = 1 to num_glyphs - 1 do
  {
    ft_load_glyph face i (ft_load_no_hinting + ft_load_no_scale + ft_load_linear_design);

    gm.(i - 1) := make_glyph_metric scale (face_glyph face)
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

  let g = Glyph.make
            code
            (int_of_num (round_num (gm.gm_width  */ dpp)))
            (int_of_num (round_num (gm.gm_height */ dpp)))
            (int_of_num (round_num (gm.gm_depth  */ dpp)))
            (float_of_num dpp)
            (float_of_num dpp)
            (x, y - b.Bitmap.bm_height + 1)
            (x + b.Bitmap.bm_width - 1, y)
  in

  {
    (g)

    with

    Glyph.g_bitmap = b
  }
};

value builtin_encoding face char = match ft_get_char_index face char with
[ 0 -> Undef
| g -> Simple g
];

value rec builtin_decoding face glyph = do
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

  match glyph with
  [ Undef              -> []
  | Border _           -> []
  | Simple g           -> lookup g
  | Accent a g         -> lookup a @ lookup g
  | Sequence gs        -> List.concat (List.map lookup gs)
  | Extendable t m b _ -> builtin_decoding face t
                        @ builtin_decoding face m
                        @ builtin_decoding face b
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

(*
value add_lookup_to_trie scale trie l = do
{
  (* move the next glyph by |pos.p_x_off| and |pos.p_y_off| *)
  let pos_to_pre_kern pos =
    ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_x_off)
              (scale */ num_of_int pos.OTF_Pos_Subst.p_y_off)
  in

  (* increase the advance width of the preceding glyph by |pos.p_x_adv| *)
  let pos_to_post_kern pos =
    ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_h_adv_off)
              num_zero
  in

  Array.fold_left (add l.OTF_Pos_Subst.l_flags) trie l.OTF_Pos_Subst.l_commands

  where add flags trie cmd = match cmd with
  [ OTF_Pos_Subst.Position pos -> do
    {
      Tag.IntMap.fold
        (fun g p trie ->
          DynUCTrie.add_list
            [g]
            ([CopyCommands 0 0;
              pos_to_pre_kern p;
              ConstGlyph (Simple g);
              pos_to_post_kern p;
              CopyCommands 1 1],
              1)
            trie)
        pos
        trie
    }
  | OTF_Pos_Subst.CursiveAnchors anchors -> trie (* FIX *)
  | OTF_Pos_Subst.MarkToBaseAnchors _ _
  | OTF_Pos_Subst.MarkToLigAnchors  _ _
  | OTF_Pos_Subst.MarkToMarkAnchors _ _ -> trie
  | OTF_Pos_Subst.Kern kerns -> do (* g1 g2 pos1 pos2 -> do *)
    {
      let add_kern g1 g2 p1 p2 trie =
        DynUCTrie.add_list
          [g1; g2]
          ([CopyCommands 0 0;
            pos_to_pre_kern p1;
            ConstGlyph (Simple g1);
            pos_to_post_kern p1;
            CopyCommands 1 1;
            pos_to_pre_kern p2;
            ConstGlyph (Simple g2);
            pos_to_post_kern p2;
            CopyCommands 2 2],
            1)
          trie
      in

      Tag.IntMap.fold
        (fun g1 k trie ->
          Tag.IntMap.fold
            (fun g2 (p1,p2) trie -> add_kern g1 g2 p1 p2 trie)
            k
            trie)
        kerns
        trie
    }
  | OTF_Pos_Subst.KernClass classes1 classes2 pos1 pos2 -> do
    {
      let n = 1 + Tag.IntMap.fold (fun _ c n -> max c n) classes2 0 in

      Tag.IntMap.fold
        (fun g1 c1 trie ->
          Tag.IntMap.fold
            (fun g2 c2 trie -> do
              {
                let p1 = pos1.(n * c1 + c2) in
                let p2 = pos2.(n * c1 + c2) in

                DynUCTrie.add_list
                  [g1; g2]
                  ([CopyCommands 0 0;
                    pos_to_pre_kern p1;
                    ConstGlyph (Simple g1);
                    pos_to_post_kern p1;
                    CopyCommands 1 1;
                    pos_to_pre_kern p2;
                    ConstGlyph (Simple g2);
                    pos_to_post_kern p2;
                    CopyCommands 2 2],
                    1)
                  trie
              })
            classes2
            trie)
        classes1
        trie
    }
  | OTF_Pos_Subst.Substitution subst -> do
    {
      Tag.IntMap.fold
        (fun g1 g2 trie ->
          DynUCTrie.add_list
            [g1]
            ([CopyCommands 0 0;
              ConstGlyph (Simple g2);
              CopyCommands 1 1],
              0)
            trie)
        subst
        trie
    }
  | OTF_Pos_Subst.Multiple map -> do
    {
      Tag.IntMap.fold
        (fun g gs trie ->
          DynUCTrie.add_list
            [g]
            ([CopyCommands 0 0 ::
              Array.fold_right
                (fun g2 cmds ->
                   [ConstGlyph (Simple g2) :: cmds])
                gs
                [CopyCommands 1 1]],
              0)
            trie)
        map
        trie
    }
  | OTF_Pos_Subst.Alternate map -> trie (* FIX *)
  | OTF_Pos_Subst.Ligature ligs -> do
    {
      Array.fold_left
        (fun trie (lig, glyphs) -> do
          {
            DynUCTrie.add_string
              glyphs
              ([CopyCommands 0 0;
                ConstGlyph (Simple lig);
                CopyCommands 1 1],
                0)
              trie
          })
          trie
          ligs
    }
  | OTF_Pos_Subst.ContextGlyphPos rules -> do
    {
      let make_repl lookups g (r, i) = do
      {
        let cmds = lookups.(i).OTF_Pos_Subst.l_commands in

        iter 0

        where rec iter k = do
        {
          if k >= Array.length cmds then
            ([ConstGlyph (Simple g);
              CopyCommands i i
              :: r],
             i - 1)
          else match cmds.(k) with
          [ OTF_Pos_Subst.Position pos -> do
            {
              try
                let p = Tag.IntMap.find g pos in

                ([pos_to_pre_kern p;
                  ConstGlyph (Simple g);
                  pos_to_post_kern p;
                  CopyCommands i i
                  :: r],
                 i - 1)
              with
              [ Not_found -> iter (k+1) ]
            }
          | _ -> iter (k+1)
          ]
        }
      }
      in
      let add_rule rule trie = do
      {
        let lookups = Array.make
                        (Array.length rule.OTF_Pos_Subst.psr_data)
                        { OTF_Pos_Subst.l_flags = 0; OTF_Pos_Subst.l_commands = [||] }
                      in

        Array.iter
          (fun l -> do
            {
              lookups.(l.OTF_Pos_Subst.prr_seq_idx) := l.OTF_Pos_Subst.prr_lookup;
            })
          rule.OTF_Pos_Subst.psr_lookups;

        let (repl, _) = Array.fold_right
                          (make_repl lookups)
                          rule.OTF_Pos_Subst.psr_data
                          ([], Array.length rule.OTF_Pos_Subst.psr_data)
                        in

        DynUCTrie.add_string
          rule.OTF_Pos_Subst.psr_data
          ([CopyCommands 0 0 :: repl], 0)
          trie
      }
      in

      Array.fold_right add_rule rules trie
    }
  | OTF_Pos_Subst.ContextGlyphSubst rules -> do
    {
      let make_repl lookups g (r, i) = do
      {
        let cmds = lookups.(i).OTF_Pos_Subst.l_commands in

        iter 0

        where rec iter k = do
        {
          if k >= Array.length cmds then
            ([ConstGlyph (Simple g);
              CopyCommands i i
              :: r],
             i - 1)
          else match cmds.(k) with
          [ OTF_Pos_Subst.Substitution subst -> do
            {
              try
                let g2 = Tag.IntMap.find g subst in
                ([ConstGlyph (Simple g2);
                  CopyCommands i i
                  :: r],
                 i - 1)
              with
              [ Not_found -> iter (k+1) ]
            }
          | _ -> iter (k+1)
          ]
        }
      }
      in
      let add_rule rule trie = do
      {
        let lookups = Array.make
                        (Array.length rule.OTF_Pos_Subst.psr_data)
                        { OTF_Pos_Subst.l_flags = 0; OTF_Pos_Subst.l_commands = [||] }
                      in

        Array.iter
          (fun l -> do
            {
              lookups.(l.OTF_Pos_Subst.prr_seq_idx) := l.OTF_Pos_Subst.prr_lookup;
            })
          rule.OTF_Pos_Subst.psr_lookups;

        let (repl, _) = Array.fold_right
                          (make_repl lookups)
                          rule.OTF_Pos_Subst.psr_data
                          ([], Array.length rule.OTF_Pos_Subst.psr_data)
                        in

        DynUCTrie.add_string
          rule.OTF_Pos_Subst.psr_data
          ([CopyCommands 0 0 :: repl], 0)
          trie
      }
      in

      Array.fold_right add_rule rules trie
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
  | _ -> trie
  ]
};
*)

(* move the next glyph by |pos.p_x_off| and |pos.p_y_off| *)

value pos_to_pre_kern scale pos = do
{
  ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_x_off)
            (scale */ num_of_int pos.OTF_Pos_Subst.p_y_off)
};

(* increase the advance width of the preceding glyph by |pos.p_x_adv| *)

value pos_to_post_kern scale pos = do
{
  ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_h_adv_off)
            num_zero
};

value position_to_repl scale g p = do
{
  ([pos_to_pre_kern scale p;
    ConstGlyph (Simple g);
    pos_to_post_kern scale p;
    CopyCommands 1 1],
    0)
};

value kern_to_repl scale g1 g2 p1 p2 = do
{
  ([CopyCommands 0 0;
   pos_to_pre_kern scale p1;
   ConstGlyph (Simple g1);
   pos_to_post_kern scale p1;
   CopyCommands 1 1;
   pos_to_pre_kern scale p2;
   ConstGlyph (Simple g2);
   pos_to_post_kern scale p2;
   CopyCommands 2 2],
   1)
};

value substitution_to_repl g = do
{
  ([CopyCommands 0 0;
    ConstGlyph (Simple g);
    CopyCommands 1 1],
    0)
};

value multi_subst_to_repl glyphs = do
{
  ([CopyCommands 0 0 ::
    Array.fold_right
      (fun g cmds -> [ConstGlyph (Simple g) :: cmds])
      glyphs
      [CopyCommands 1 1]],
    0)
};

value ligature_to_repl lig = do
{
  ([CopyCommands 0 0;
    ConstGlyph (Simple lig);
    CopyCommands 1 1],
    0)
};

value pos_rule_to_repl scale glyphs rule = do
{
  let lookups = Array.make
                  (List.length glyphs)
                  { OTF_Pos_Subst.l_flags = 0; OTF_Pos_Subst.l_commands = [||] }
                in

  Array.iter
    (fun l -> do
      {
        lookups.(l.OTF_Pos_Subst.prr_seq_idx) := l.OTF_Pos_Subst.prr_lookup;
      })
    rule;

  let repl = ListBuilder.make () in

  ListBuilder.add repl (CopyCommands 0 0);

  iter_glyphs 0 glyphs

  where rec iter_glyphs i glyphs = match glyphs with
  [ []      -> (ListBuilder.get repl, 0)
  | [g::gs] -> do
    {
      let cmds = lookups.(i).OTF_Pos_Subst.l_commands in

      iter_cmds 0

      where rec iter_cmds k = do
      {
        if k >= Array.length cmds then do
        {
          ListBuilder.add_list repl [ConstGlyph (Simple g); CopyCommands i i];

          iter_glyphs (i+1) gs
        }
        else match cmds.(k) with
        [ OTF_Pos_Subst.Position pos -> do
          {
            try do
            {
              let p = Tag.IntMap.find g pos in

              ListBuilder.add_list repl
                [pos_to_pre_kern scale p;
                 ConstGlyph (Simple g);
                 pos_to_post_kern scale p;
                 CopyCommands i i];

              iter_glyphs (i+1) gs
            }
            with
            [ Not_found -> iter_cmds (k+1) ]
          }
        | _ -> iter_cmds (k+1)
        ]
      }
    }
  ]
};

value subst_rule_to_repl scale glyphs rule = do
{
  let lookups = Array.make
                  (List.length glyphs)
                  { OTF_Pos_Subst.l_flags = 0; OTF_Pos_Subst.l_commands = [||] }
                in

  Array.iter
    (fun l -> do
      {
        lookups.(l.OTF_Pos_Subst.prr_seq_idx) := l.OTF_Pos_Subst.prr_lookup;
      })
    rule;

  let repl = ListBuilder.make () in

  ListBuilder.add repl (CopyCommands 0 0);

  iter_glyphs 0 glyphs

  where rec iter_glyphs i glyphs = match glyphs with
  [ []      -> (ListBuilder.get repl, 0)
  | [g::gs] -> do
    {
      let cmds = lookups.(i).OTF_Pos_Subst.l_commands in

      iter_cmds 0

      where rec iter_cmds k = do
      {
        if k >= Array.length cmds then do
        {
          ListBuilder.add_list repl [ConstGlyph (Simple g); CopyCommands i i];

          iter_glyphs (i+1) gs
        }
        else match cmds.(k) with
        [ OTF_Pos_Subst.Substitution subst -> do
          {
            try do
            {
              let g2 = Tag.IntMap.find g subst in

              ListBuilder.add_list repl
                [ConstGlyph (Simple g2);
                 CopyCommands i i];

               iter_glyphs (i+1) gs
            }
            with
            [ Not_found -> iter_cmds (k+1) ]
          }
        | _ -> iter_cmds (k+1)
        ]
      }
    }
  ]
};

value command_to_repl scale cmd glyphs = match cmd with
[ OTF_Pos_Subst.NoCommand    -> None
| OTF_Pos_Subst.Position pos -> match glyphs with
  [ [g] -> do
    {
      try
        let p = Tag.IntMap.find g pos in

        Some (position_to_repl scale g p)
      with
      [ Not_found -> None ]
    }
  | _ -> None
  ]
| OTF_Pos_Subst.CursiveAnchors anchors -> None (* FIX *)
| OTF_Pos_Subst.MarkToBaseAnchors _ _
| OTF_Pos_Subst.MarkToLigAnchors  _ _
| OTF_Pos_Subst.MarkToMarkAnchors _ _ -> None
| OTF_Pos_Subst.Kern kerns -> match glyphs with
  [ [g1; g2] -> do
    {
      try
        let k        = Tag.IntMap.find g1 kerns in
        let (p1, p2) = Tag.IntMap.find g2 k in

        Some (kern_to_repl scale g1 g2 p1 p2)
      with
      [ Not_found -> None ]
    }
  | _ -> None
  ]
| OTF_Pos_Subst.KernClass n classes1 classes2 pos1 pos2 -> match glyphs with
  [ [g1; g2] -> do
    {
      try
        let c1 = Tag.IntMap.find g1 classes1 in
        let c2 = Tag.IntMap.find g2 classes2 in
        let p1 = pos1.(n * c1 + c2) in
        let p2 = pos2.(n * c1 + c2) in

        Some (kern_to_repl scale g1 g2 p1 p2)
      with
      [ Not_found -> None ]
    }
  | _ -> None
  ]
| OTF_Pos_Subst.Substitution subst -> match glyphs with
  [ [g] -> do
    {
      try
        let g2 = Tag.IntMap.find g subst in

        Some (substitution_to_repl g2)
      with
      [ Not_found -> None ]
    }
  | _ -> None
  ]
| OTF_Pos_Subst.Multiple map -> match glyphs with
  [ [g] -> do
    {
      try
        let gs = Tag.IntMap.find g map in

        Some (multi_subst_to_repl gs)
      with
      [ Not_found -> None ]
    }
  | _ -> None
  ]
| OTF_Pos_Subst.Alternate map -> None (* FIX *)
| OTF_Pos_Subst.Ligature ligs -> do
  {
    match DynUCTrie.lookup_list glyphs ligs with
    [ None     -> None
    | Some lig -> Some (ligature_to_repl lig)
    ]
  }
| OTF_Pos_Subst.ContextGlyphPos rules -> do
  {
    try
      let rule = DynUCTrie.find_list glyphs rules in

      Some (pos_rule_to_repl scale glyphs rule)
    with
    [ Not_found -> None ]
  }
| OTF_Pos_Subst.ContextGlyphSubst rules -> do
  {
    try
      let rule = DynUCTrie.find_list glyphs rules in

      Some (subst_rule_to_repl scale glyphs rule)
    with
    [ Not_found -> None ]
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
| _ -> None
];

value lookup_repl scale lookups glyphs = do
{
  iter_lookups lookups

  where rec iter_lookups lookups = match lookups with
  [ []      -> None
  | [l::ls] -> do
    {
      let cmds = l.OTF_Pos_Subst.l_commands in

      iter_commands 0

      where rec iter_commands k = do
      {
        if k >= Array.length cmds then
          iter_lookups ls
        else match command_to_repl scale cmds.(k) glyphs with
        [ None   -> iter_commands (k+1)
        | Some r -> Some r
        ]
      }
    }
  ]
};

value max_lookup_depth lookups = do
{
  5
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
        let f_tags = SymbolSet.fold
                       (fun set f ->
                         Tag.TagSet.add
                           (Tag.make_tag_uc (SymbolTable.symbol_to_string f))
                           set)
                       Tag.TagSet.empty
                       features
                     in
        let lookups = OTF_Pos_Subst.get_lookups pos_subst s_tag l_tag f_tags in

        let max_depth = max_lookup_depth lookups in

        let is_empty (n, _)        = (n > max_depth) in
        let prefix (n, glyphs) g   = (n+1, [g :: glyphs]) in
        let root_value (n, glyphs) = lookup_repl scale lookups (List.rev glyphs) in

        let trie    = (is_empty, prefix, root_value) in

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
(*
value get_subst_table memo_table scale pos_subst script features = do
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
        let f_tags = SymbolSet.fold
                       (fun set f ->
                         Tag.TagSet.add
                           (Tag.make_tag_uc (SymbolTable.symbol_to_string f))
                           set)
                       Tag.TagSet.empty
                       features
                     in
        let lookups = OTF_Pos_Subst.get_lookups pos_subst s_tag l_tag f_tags            in
        let trie    = List.fold_left (add_lookup_to_trie scale) DynUCTrie.empty lookups in

        !memo_table := add_composer !memo_table s_tag l_tag features trie;

        trie
      }
    ]
  in

  match_substitution_trie (DynUCTrie.is_empty, DynUCTrie.prefix, DynUCTrie.root_value) trie
};

value get_composer pos_subst p_table s_table scale = match pos_subst with
[ (None,   None)   -> fun fm _   _    -> simple_composer    fm (simple_ligature_substitution fm)
| (None,   Some s) -> fun fm scr feat -> simple_composer    fm (get_subst_table s_table scale s scr feat)
| (Some p, None)   -> fun fm scr feat -> simple_composer    fm (get_subst_table p_table scale p scr feat)
| (Some p, Some s) -> fun fm scr feat -> two_phase_composer fm (get_subst_table s_table scale s scr feat)
                                                               (get_subst_table p_table scale p scr feat)
];
*)

end;

value read_ft file name size = do
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

  let (em, asc, desc, height, ul_pos, ul_thick) =
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

  let scale        = size // num_of_int em            in
  let design_size  = scale */ num_of_int (asc - desc) in
  let glyph_metric = get_glyph_metric scale face      in
  let space_glyph  = ft_get_char_index face 32        in
  let x_glyph      = ft_get_char_index face 102       in
  let m_glyph      = ft_get_char_index face 77        in
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
    get_glyph           = builtin_encoding face;
    get_unicode         = builtin_decoding face;
    get_composer        = composer;
    kerning             = fun _ c1 c2 -> ft_kerning face scale c1 c2;
    draw_simple_glyph   = draw_simple_glyph;
    accent_base_point   = accent_base_point_x_height;
    accent_attach_point = accent_attach_point_top;
    get_glyph_bitmap    = get_glyph_bitmap face;
    get_glyph_name      = ft_get_glyph_name face;
    parameter           =
      {
        hyphen_glyph     = builtin_encoding face 45;
        skew_glyph       = Undef;
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

