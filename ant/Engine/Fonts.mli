
open XNum;
open Unicode.Types;
open Runtime;
open Substitute;
open FontMetric;
open Typesetting;
open Box;

type font_definition =
{
  fd_name         : uc_string;
  fd_encoding     : uc_string;
  fd_family       : uc_string;
  fd_series       : uc_string;
  fd_shape        : uc_string;
  fd_min_size     : num;
  fd_max_size     : num;
  fd_loaded_sizes : mutable list (num * font_metric);
  fd_data         : (glyph_desc * glyph_desc * num)
};

type font =
{
  f_font_def : font_definition;
  f_metric   : font_metric;
  f_size     : num
};

value get_font     : uc_string -> uc_string -> uc_string -> num -> option font;
value declare_font : uc_string -> uc_string -> uc_string -> uc_string ->
                     uc_string -> (num * num) -> (glyph_desc * glyph_desc * num) -> unit;

value initialise_font_table : unit -> unit;

value make_virtual_font : string -> num -> array box -> array num -> list ((int * int) * lig_kern) ->
                          array num -> font_metric;

