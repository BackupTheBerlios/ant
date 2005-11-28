
open XNum;
open Unicode.Types;

type border_glyph =
[ Space   (* for kerning with spaces                *)
| Margin  (* for margin kerning                     *)
| Foreign (* for kerning with glyphs of other fonts *)
];

type glyph_desc =
[ Undef
| Simple     of int
| Accent     of int and int
| Sequence   of list int
| Extendable of glyph_desc and glyph_desc and glyph_desc and glyph_desc
| Border     of border_glyph
];

module OrderedGlyphDesc :
sig

  type t = glyph_desc;

  value compare : glyph_desc -> glyph_desc -> int;

end;

module GlyphMap  : Map.S with type key = glyph_desc;
module GlyphSet  : Set.S with type elt = glyph_desc;
module GlyphTrie : DynamicTrie.S with type elt = glyph_desc;

type glyph_item 'font 'box 'cmd =
[= `Glyph of (glyph_desc * 'font)
|  `Kern of (num * num)
|  `Box of 'box
|  `Command of 'cmd
|  `Break of (num * bool
                  * array (glyph_item 'font 'box 'cmd)
                  * array (glyph_item 'font 'box 'cmd)
                  * array (glyph_item 'font 'box 'cmd))
];

type replacement_cmd =
[ ConstGlyph of glyph_desc
| ConstKern of num and num
| CopyGlyph of int
| CopyCommands of int and int
];

type replacement = (list replacement_cmd * int);

type glyph_composer 'font 'box 'cmd =
  list (glyph_item 'font 'box 'cmd) ->
  list (glyph_item 'font 'box 'cmd);

type substitution 'font 'box 'cmd =
  list (glyph_item 'font 'box 'cmd) ->
  option (list (glyph_item 'font 'box 'cmd) * list (glyph_item 'font 'box 'cmd) * replacement);

value first_real_item_list : list (glyph_item 'font 'box 'cmd) -> option (glyph_item 'font 'box 'cmd);
value first_real_item      : array (glyph_item 'font 'box 'cmd) -> int -> int -> option (glyph_item 'font 'box 'cmd);
value last_real_item       : array (glyph_item 'font 'box 'cmd) -> int -> int -> option (glyph_item 'font 'box 'cmd);

value match_substitution_dyntrie : DynUCTrie.t replacement -> substitution 'font 'box 'cmd;
value match_substitution_trie    : (('a -> bool) * ('a -> uc_char -> 'a) * ('a -> option replacement)) -> 'a -> substitution 'font 'box 'cmd;

value substitute           : 'font -> substitution 'font 'box 'cmd -> glyph_composer 'font 'box 'cmd;

