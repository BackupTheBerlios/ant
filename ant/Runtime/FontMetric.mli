
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


value zero_kern_info     : extra_kern_info;
value empty_glyph_metric : glyph_metric;
value merge_kern_infos   : extra_kern_info -> extra_kern_info -> extra_kern_info;

value default_bitmap_resolution : ref int;
value default_mf_mode           : ref string;

value get_glyph        : font_metric -> uc_char -> glyph_desc;
value get_unicode      : font_metric -> glyph_desc -> uc_list;
value index_to_glyph   : font_metric -> int -> glyph_desc;
value glyph_exists     : font_metric -> int -> bool;

value simple_ligature_substitution : font_metric -> substitution font_metric 'box 'cmd;
value simple_composer              : font_metric
                                       -> substitution font_metric 'box 'cmd
                                       -> glyph_composer font_metric 'box 'cmd;
value two_phase_composer           : font_metric
                                       -> substitution font_metric 'box 'cmd
                                       -> substitution font_metric 'box 'cmd
                                       -> glyph_composer font_metric 'box 'cmd;

value get_glyph_metric   : font_metric -> glyph_desc -> glyph_metric;
value next_glyph         : font_metric -> glyph_desc -> glyph_desc;
value get_glyph_composer : get_composer_type 'box 'cmd;
value get_lig_kern       : font_metric -> glyph_desc -> glyph_desc -> lig_kern;
value draw_simple_glyph  : font_metric -> int -> simple_box;
value draw_glyph         : font_metric -> glyph_desc -> simple_box;
value set_encoding       : font_metric -> (uc_char -> glyph_desc) -> (glyph_desc -> uc_list) -> font_metric;
value set_hyphen_char    : font_metric -> glyph_desc -> font_metric;
value set_skew_char      : font_metric -> glyph_desc -> font_metric;

value accent_base_point          : font_metric -> glyph_metric -> (num * num);
value accent_attach_point        : font_metric -> glyph_metric -> (num * num);
value accent_base_point_x_height : font_metric -> glyph_metric -> (num * num);
value accent_attach_point_top    : font_metric -> glyph_metric -> (num * num);
value accent_position            : font_metric -> glyph_metric ->
                                   font_metric -> glyph_metric -> (num * num);
value construct_accent           : font_metric -> glyph_desc -> font_metric -> glyph_desc -> glyph_metric;

(* Shorthand to access the dimension of a normal and an extended space of the font. *)

value space_glue  : font_metric -> dim;
value xspace_glue : font_metric -> dim;

value empty_font      : font_metric;
value empty_parameter : font_parameter;

