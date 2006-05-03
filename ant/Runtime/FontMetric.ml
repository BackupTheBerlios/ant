
open XNum;
open Unicode.Types;
open Dim;
open Graphic;
open Substitute;

(* glyph metrics *)

type glyph_extra_info =
[ GXI_Normal
| GXI_LigKern    of int
| GXI_List       of int
| GXI_Extendable of int and int and int and int
];

type extra_kern_info =
{
  ki_after_space    : num;
  ki_before_space   : num;
  ki_after_margin   : num;
  ki_before_margin  : num;
  ki_after_foreign  : num;
  ki_before_foreign : num
};

type glyph_metric =
{
  gm_width      : num;
  gm_height     : num;
  gm_depth      : num;
  gm_italic     : num;
  gm_extra      : glyph_extra_info;
  gm_extra_kern : extra_kern_info
};

value zero_kern_info =
{
  ki_after_space    = num_zero;
  ki_before_space   = num_zero;
  ki_after_margin   = num_zero;
  ki_before_margin  = num_zero;
  ki_after_foreign  = num_zero;
  ki_before_foreign = num_zero
};

value empty_glyph_metric =
{
  gm_width      = num_zero;
  gm_height     = num_zero;
  gm_depth      = num_zero;
  gm_italic     = num_zero;
  gm_extra      = GXI_Normal;
  gm_extra_kern = zero_kern_info
};

(* The the after values from <left> and the before ones from <right>. *)

value merge_kern_infos left right =
{
  ki_after_space    = left.ki_after_space;
  ki_before_space   = right.ki_before_space;
  ki_after_margin   = left.ki_after_margin;
  ki_before_margin  = right.ki_before_margin;
  ki_after_foreign  = left.ki_after_foreign;
  ki_before_foreign = right.ki_before_foreign
};

(* font metrics *)

type font_parameter =
{
  hyphen_glyph     : glyph_desc;
  skew_glyph       : glyph_desc;
  slant            : num;
  space            : num;
  space_stretch    : num;
  space_shrink     : num;
  x_height         : num;
  quad             : num;
  extra_space      : num;
  num_shift_1      : num;
  num_shift_2      : num;
  num_shift_3      : num;
  denom_shift_1    : num;
  denom_shift_2    : num;
  super_shift_1    : num;
  super_shift_2    : num;
  super_shift_3    : num;
  sub_shift_1      : num;
  sub_shift_2      : num;
  super_drop       : num;
  sub_drop         : num;
  delim_1          : num;
  delim_2          : num;
  axis_height      : num;
  rule_thickness   : num;
  big_op_spacing_1 : num;
  big_op_spacing_2 : num;
  big_op_spacing_3 : num;
  big_op_spacing_4 : num;
  big_op_spacing_5 : num
};

type lig_kern =
[ NoLigKern
| Ligature of int and int and bool and bool  (* glyph skip keep-first? keep-second? *)
| Kern of num
];

type font_type =
[ PostScript
| OpenTypeCFF
| TrueType
| Other
];

type font_metric =
{
  name                : string;
  ps_name             : string;
  file_name           : string;
  font_type           : font_type;
  first_glyph         : int;
  last_glyph          : int;
  design_size         : num;
  at_size             : num;
  check_sum           : num;
  parameter           : font_parameter;
  get_glyph           : uc_char -> glyph_desc;
  get_unicode         : glyph_desc -> uc_list;
  get_composer        : !'box 'cmd . get_composer_type 'box 'cmd;
  kerning             : font_metric -> int -> int -> lig_kern;
  draw_simple_glyph   : font_metric -> int -> simple_box;
  accent_base_point   : font_metric -> glyph_metric -> (num * num);
  accent_attach_point : font_metric -> glyph_metric -> (num * num);
  get_glyph_bitmap    : font_metric -> uc_char -> Glyph.glyph;
  get_glyph_name      : int -> string;
  glyph_metric        : array glyph_metric
}

and get_composer_type 'box 'cmd = font_metric -> uc_string -> SymbolSet.t -> glyph_composer font_metric 'box 'cmd

and simple_box =
[ Empty
| SimpleGlyph of int and font_metric
| Rule of num and num
| Image of num and num and string
| Group of list (graphic_command num simple_box)
| Command of simple_cmd
]

and simple_cmd =
[= `DVI_Special of string
];

type char_item 'box 'cmd =
[= `Char of uc_char
|  `Kern of (num * num)
|  `Box of 'box
|  `Command of 'cmd
|  `Break of (num * bool * list (char_item 'box 'cmd) * list (char_item 'box 'cmd) * list (char_item 'box 'cmd))
];

(* pages *)

type page =
{
  p_contents : simple_box;
  p_number   : int;
  p_width    : num;
  p_height   : num
};

(* resolution used to load bitmap fonts *)

value default_bitmap_resolution = ref 1200;
value default_mf_mode           = ref "ljfzzz";

(* |get_glyph <font> <char>| returns the glyph corresponding to the unicode <char> and
   |get_unicode <font> <glyph>| computes the unicode of the given glyph.
*)

value get_glyph   font char  = font.get_glyph   char;
value get_unicode font glyph = font.get_unicode glyph;

(* |glyph_exists <font> <glyph>| tests whether the font contains a glyph with the number <glyph>. *)

value glyph_exists font glyph = do
{
  (glyph <= font.last_glyph && glyph >= font.first_glyph)
};

(*
  |index_to_glyph <font> <index>| checks whether <font> contains a glyph of the given index and returns
  the corresponding |glyph_desc|.
*)

value index_to_glyph font idx = do
{
  if glyph_exists font idx then
    Simple idx
  else
    Undef
};

(*
  |accent_base_point <font> <glyph-metric>| returns the point where the glyph is attached to another glyph
  when it is used as an accent.
*)

value accent_base_point font gm = do
{
  font.accent_base_point font gm
};

(*
  |accent_attach_point <font> <glyph-metric>| returns the point where an accent should be attached
  to the glyph.
*)

value accent_attach_point font gm = do
{
  font.accent_attach_point font gm
};

(*
  |accent_position <acc-font> <acc-gm> <chr-font> <chr-gm>| calculates the position of the accent glyph
  in a composite glyph.
*)

value accent_position acc_font acc_gm chr_font chr_gm = do
{
  let (acc_x, acc_y) = accent_base_point   acc_font acc_gm in
  let (chr_x, chr_y) = accent_attach_point chr_font chr_gm in

(*  let pos_y  = chr_gm.gm_height -/ acc_font.parameter.x_height in
  let pos_x  = (chr_gm.gm_width -/ acc_gm.gm_width) // num_of_int 2
               +/ (chr_font.parameter.slant */ chr_gm.gm_height
               -/ acc_font.parameter.slant */ acc_font.parameter.x_height) in*)

  (chr_x -/ acc_x, chr_y -/ acc_y)
};

value rec get_glyph_metric font glyph = match glyph with
[ Undef
| Border _           -> empty_glyph_metric
| Simple g           -> font.glyph_metric.(g - font.first_glyph)
| Accent a g         -> construct_accent font (Simple a) font (Simple g)
| Sequence gs        -> construct_sequence font gs
| Extendable t m b _ -> do
  {
    let tgm = get_glyph_metric font t in
    let mgm = get_glyph_metric font m in
    let bgm = get_glyph_metric font b in

    {
      gm_width      = max_num tgm.gm_width (max_num mgm.gm_width bgm.gm_width);
      gm_height     = tgm.gm_height +/ tgm.gm_depth  +/ mgm.gm_height;
      gm_depth      = mgm.gm_depth  +/ bgm.gm_height +/ bgm.gm_depth;
      gm_italic     = num_zero;
      gm_extra      = GXI_Normal;
      gm_extra_kern = zero_kern_info
    }
  }
]

(*
  |construct_accent <acc-font> <acc> <chr-font> <chr>| constructs a glyph_metric structure for the
  composite glyph.
*)

and construct_accent acc_font acc chr_font chr = do
{
  let acc_gm = get_glyph_metric acc_font acc in
  let chr_gm = get_glyph_metric chr_font chr in

  let (_, pos_y) = accent_position acc_font acc_gm chr_font chr_gm in
  {
    (chr_gm)

    with

    gm_height = max_num chr_gm.gm_height (acc_gm.gm_height +/ pos_y);
    gm_depth  = max_num chr_gm.gm_depth  (acc_gm.gm_depth  -/ pos_y)
  }
}

and construct_sequence font glyphs = match glyphs with
[ []            -> empty_glyph_metric
| [g]           -> get_glyph_metric font (Simple g)
| [first :: gs] -> do
  {
    let m1 = get_glyph_metric font (Simple first) in

    iter m1.gm_width m1.gm_height m1.gm_depth gs

    where rec iter width height depth glyphs = match glyphs with
    [ [last] -> do
      {
        let m2 = get_glyph_metric font (Simple last)  in
        {
          gm_width  = width +/ m2.gm_width;
          gm_height = max_num height m2.gm_height;
          gm_depth  = max_num depth  m2.gm_depth;
          gm_italic = m2.gm_italic;
          gm_extra  = GXI_Normal;
          gm_extra_kern = merge_kern_infos m1.gm_extra_kern m2.gm_extra_kern
        }
      }
    | [g::gs] -> do
      {
        let m = get_glyph_metric font (Simple g) in

        iter
          (width +/ m.gm_width)
          (max_num height m.gm_height)
          (max_num depth  m.gm_depth)
          gs
      }
    | [] -> assert False
    ]
  }
];

(* Default value for |accent_base_point| where the base point lies at the x-height. *)

value accent_base_point_x_height font gm = do
{
  let pos_x = (gm.gm_width // num_of_int 2) +/ font.parameter.slant */ font.parameter.x_height in

  (pos_x, font.parameter.x_height)
};

(* Default value for |accent_attach_point| where the accent is attached to the top of the glyph. *)

value accent_attach_point_top font gm = do
{
  let pos_x = (gm.gm_width // num_of_int 2) +/ font.parameter.slant */ gm.gm_height in

  (pos_x, gm.gm_height)
};

(*
  |next_glyph <font> <glyph>| returns the next variant of a character, or |Undef| if there is none.
*)

value next_glyph font glyph = do
{
  match (get_glyph_metric font glyph).gm_extra with
  [ GXI_List n -> match (get_glyph_metric font (Simple n)).gm_extra with
                  [ GXI_Extendable t m b r -> Extendable (Simple t) (Simple m) (Simple b) (Simple r)
                  | _                      -> Simple n
                  ]
  | _             -> Undef
  ]
};

value get_after_kerning font border glyph = do
{
  let m = get_glyph_metric font glyph in

  let k = match border with
  [ Space   -> m.gm_extra_kern.ki_after_space
  | Margin  -> m.gm_extra_kern.ki_after_margin
  | Foreign -> m.gm_extra_kern.ki_after_foreign
  ]
  in

  if k <>/ num_zero then
    Kern k
  else
    NoLigKern
};

value get_before_kerning font glyph border = do
{
  let m = get_glyph_metric font glyph in

  let k = match border with
  [ Space   -> m.gm_extra_kern.ki_before_space
  | Margin  -> m.gm_extra_kern.ki_before_margin
  | Foreign -> m.gm_extra_kern.ki_before_foreign
  ]
  in

  if k <>/ num_zero then
    Kern k
  else
    NoLigKern
};

(*
  |get_lig_kern <font> <glyph-1> <glyph-2>| returns either the amount of kerning between two glyphs
  or the ligature formed by them.
*)

value get_lig_kern font glyph1 glyph2 = match (glyph1, glyph2) with
[ (Simple g1,   Simple g2)   -> font.kerning font g1 g2
| (Simple g1,   Accent _ g2) -> font.kerning font g1 g2
| (Accent _ g1, Simple g2)   -> font.kerning font g1 g2
| (Accent _ g1, Accent _ g2) -> font.kerning font g1 g2
| (Border b,    g)           -> get_after_kerning  font b g
| (g,           Border b)    -> get_before_kerning font g b
| _                          -> NoLigKern
];

value get_glyph_composer font script features = font.get_composer font script features;

value simple_ligature_substitution font items = do
{
  let rec find_first_glyph prefix items = match items with
  [ []               -> (prefix, (Undef, font), [])
  | [`Glyph g :: is] -> (prefix, g, is)
  | [i :: is]        -> find_first_glyph [i :: prefix] is
  ]
  in
  let (p1, ((g1,_) as gf1), rest1) = find_first_glyph []                 items in
  let (p2, ((g2,_) as gf2), rest2) = find_first_glyph [`Glyph gf1 :: p1] rest1 in

  match get_lig_kern font g1 g2 with
  [ Ligature c s k1 k2 -> do
    {
      (* FIX: copy commands *)
      let repl = if k1 then
                   [CopyCommands 0 0; ConstGlyph g1; CopyCommands 1 1]
                 else
                   [CopyCommands 0 1]
               @ [ConstGlyph (Simple c)]
               @ if k2 then
                   [ConstGlyph g2; CopyCommands 2 2]
                 else
                   [CopyCommands 2 2]
      in

      Some ([`Glyph gf2 ::  p2], rest2, (repl, s))
    }
  | Kern x -> do
    {
      let repl =
        [CopyCommands 0 0;
         ConstGlyph g1;
         CopyCommands 1 1;
         ConstKern x num_zero;
         ConstGlyph g2;
         CopyCommands 2 2]
      in

      Some ([`Glyph gf2 ::  p2], rest2, (repl, 1))
    }
  | NoLigKern -> None
  ]
};

value simple_composer = Substitute.substitute;

value two_phase_composer font find_subst1 find_subst2 items = do
{
  Substitute.substitute font find_subst2
    (Substitute.substitute font find_subst1 items)
};

(* Default value for |draw_simple_glyph|. *)

value draw_simple_glyph font glyph = SimpleGlyph glyph font;

value rec draw_glyph font glyph = match glyph with
[ Undef
| Border _   -> Empty
| Simple g   -> font.draw_simple_glyph font g
| Accent a g -> do
  {
    let a_gm = get_glyph_metric font (Simple a) in
    let g_gm = get_glyph_metric font (Simple g) in

    let (pos_x, pos_y) = accent_position font a_gm font g_gm in

    Group
      [Graphic.PutBox num_zero num_zero (SimpleGlyph g font);
       Graphic.PutBox pos_x    pos_y    (SimpleGlyph a font)];
  }
| Sequence gs -> do
  {
    let cmds = ListBuilder.make () in

    iter num_zero gs

    where rec iter x glyphs = match glyphs with
    [ []      -> Group (ListBuilder.get cmds)
    | [g::gs] -> do
      {
        let m = get_glyph_metric font (Simple g) in

        ListBuilder.add cmds (Graphic.PutBox x num_zero (draw_glyph font (Simple g)));

        iter (x +/ m.gm_width) gs
      }
    ]
  }
| Extendable t m b _ -> do
  {
    let tgm = get_glyph_metric font t in
    let mgm = get_glyph_metric font m in
    let bgm = get_glyph_metric font b in

    Group
      [Graphic.PutBox num_zero (tgm.gm_depth +/ mgm.gm_height) (draw_glyph font t);
       Graphic.PutBox num_zero num_zero                        (draw_glyph font m);
       Graphic.PutBox num_zero (mgm.gm_depth +/ bgm.gm_height) (draw_glyph font b)]
  }
];

value set_encoding font enc dec =
{
  (font)

  with

  get_glyph   = enc;
  get_unicode = dec
};

value set_hyphen_char font c =
{
  (font)

  with

  parameter = { (font.parameter) with hyphen_glyph = c }
};

value set_skew_char font c =
{
  (font)

  with

  parameter = { (font.parameter) with skew_glyph = c }
};

(* Shorthand to access the dimension of a normal and an extended space of the font. *)

value space_glue font =
{
  d_base           = font.parameter.space;
  d_stretch_factor = font.parameter.space_stretch;
  d_stretch_order  = 0;
  d_shrink_factor  = font.parameter.space_shrink;
  d_shrink_order   = 0
};

value xspace_glue font =
{
  d_base           = font.parameter.space +/ font.parameter.extra_space;
  d_stretch_factor = font.parameter.space_stretch;
  d_stretch_order  = 0;
  d_shrink_factor  = font.parameter.space_shrink;
  d_shrink_order   = 0
};

value empty_parameter =
{
  hyphen_glyph     = Undef;
  skew_glyph       = Undef;
  slant            = num_zero;
  space            = num_zero;
  space_stretch    = num_zero;
  space_shrink     = num_zero;
  x_height         = num_zero;
  quad             = num_zero;
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

value empty_font =
{
  name                = "<null>";
  ps_name             = "";
  file_name           = "<internal>";
  font_type           = Other;
  first_glyph         = 0;
  last_glyph          = -1;
  design_size         = num_one;
  at_size             = num_one;
  check_sum           = num_zero;
  get_glyph           = (fun _ -> Undef);
  get_unicode         = (fun _ -> []);
  kerning             = (fun _ -> assert False);
  get_composer        = (fun _ _ _ _ -> []);
  draw_simple_glyph   = (fun _ -> assert False);
  accent_base_point   = (fun _ -> assert False);
  accent_attach_point = (fun _ -> assert False);
  get_glyph_bitmap    = (fun _ -> assert False);
  get_glyph_name      = (fun _ -> assert False);
  glyph_metric        = [| |];
  parameter           = empty_parameter
};

