
open XNum;
open Unicode.Types;
open FontMetric;

(* |load_font <name> <size>| tries to load the given font. *)

value load_font : string -> num -> font_metric;

