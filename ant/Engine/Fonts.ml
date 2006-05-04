
open XNum;
open Unicode;
open Types;
open Runtime;
open Substitute;
open FontMetric;
open Logging;
open Dim;
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
  fd_data         : font_load_params
};

type font =
{
  f_font_def : font_definition;
  f_metric   : font_metric;
  f_size     : num
};

(* table of fonts *)

value font_table = ref DynUCTrie.empty;

value get_font_list family = do
{
  try
    DynUCTrie.find_string family !font_table
  with
  [ Not_found -> [] ]
};

value add_font font_def = do
{
  let rec add_entry font_list = match font_list with
  [ []      -> [font_def]
  | [f::fs] -> if f.fd_name     =  font_def.fd_name     &&
                  f.fd_encoding =  font_def.fd_encoding &&
                  f.fd_family   =  font_def.fd_family   &&
                  f.fd_series   =  font_def.fd_series   &&
                  f.fd_shape    =  font_def.fd_shape    &&
                  f.fd_min_size =/ font_def.fd_min_size &&
                  f.fd_max_size =/ font_def.fd_max_size then
                 font_list
               else
                 [f :: add_entry fs]
  ]
  in

  let font_list = get_font_list font_def.fd_family in

  !font_table := DynUCTrie.add_string
                   font_def.fd_family
                   (add_entry font_list)
                   !font_table
};

value load_font fd size = do
{
  (* |fd.fd_data.flp_size| does not contain the real size, but a scale factor. *)
  let params =
    {
      (fd.fd_data)
      with
      flp_size = fd.fd_data.flp_size */ size
    }
  in

  let font = LoadFont.load_font (UString.to_string (Array.to_list fd.fd_name)) params in

  let fm1 = match fd.fd_encoding with
  [ [|79; 84; 49|]   -> FontMetric.set_encoding font                       (* OT1 *)
                         (Encodings.charmap_encoding Encodings.uc_to_ot1)
                         (Encodings.array_decoding   Encodings.ot1_to_uc)
  | [|84; 49|]       -> FontMetric.set_encoding font                       (* T1  *)
                         (Encodings.charmap_encoding Encodings.uc_to_t1)
                         (Encodings.array_decoding   Encodings.t1_to_uc)
  | [|79; 84; 84|]   -> FontMetric.set_encoding font                       (* OTT *)
                         (Encodings.charmap_encoding Encodings.uc_to_ott)
                         (Encodings.array_decoding   Encodings.ott_to_uc)
  | [|79; 77; 83|]   -> FontMetric.set_encoding font                       (* OMS *)
                         (Encodings.charmap_encoding Encodings.uc_to_oms)
                         (Encodings.array_decoding   Encodings.oms_to_uc)
  | [|79; 77; 76|]   -> FontMetric.set_encoding font                       (* OML *)
                         (Encodings.charmap_encoding Encodings.uc_to_oml)
                         (Encodings.array_decoding   Encodings.oml_to_uc)
  | [|114; 97; 119|] -> FontMetric.set_encoding font                       (* raw *)
                         Encodings.raw_encoding
                         Encodings.raw_decoding
  | [|98; 117; 105; 108; 116; 105; 110|] -> font                           (* builtin *)
  | _ -> do
    {
      log_warn ("",0,0) "Unknown font encoding: ";
      log_uc_string fd.fd_encoding;
      log_string "\n";
      font
    }
  ]
  in

  let fm2 = match fd.fd_data.flp_hyphen_glyph with
    [ Undef -> fm1
    | h     -> FontMetric.set_hyphen_char fm1 h
    ]
  in
  let fm3 = match fd.fd_data.flp_skew_glyph with
    [ Undef -> fm2
    | s     -> FontMetric.set_skew_char fm2 s
    ]
  in

  fd.fd_loaded_sizes := add fd.fd_loaded_sizes

  where rec add sizes = match sizes with
  [ []        -> [(size, fm3)]
  | [s :: ss] -> if fst s </ size then
                   [s :: add ss]
                 else if fst s >/ size then
                   [(size, fm3) :: sizes]
                 else
                   sizes
  ];

  {
    f_font_def = fd;
    f_metric   = fm3;
    f_size     = size
  }
};

value get_font family series shape size = do
{
  let rec choose_font fonts = match fonts with
  [ []      -> None
  | [f::fs] -> do
    {
      if f.fd_series <> series then
        choose_font fs
      else if f.fd_shape <> shape then
        choose_font fs
      else do
      {
        if f.fd_min_size <=/ size &&
           (size </ f.fd_max_size || f.fd_max_size =/ num_zero ||
            (size =/ f.fd_max_size && f.fd_max_size =/ f.fd_min_size)) then
          Some f
        else
          choose_font fs
      }
    }
  ]
  in

  let load fd size = do
  {
    try
      Some (load_font fd size)
    with
    [ Not_found -> do
      {
        log_warn ("",0,0) "Cannot load font file `";
        log_uc_string fd.fd_name;
        log_string "'!";
        None
      }
    ]
  }
  in

  match choose_font (get_font_list family) with
  [ None    -> None
  | Some fd -> do
    {
      iter fd.fd_loaded_sizes

      where rec iter sizes = match sizes with
      [ []            -> load fd size
      | [(s,f) :: ss] -> if s </ size then
                           iter ss
                         else if s =/ size then
                           Some {
                                  f_font_def = fd;
                                  f_metric   = f;
                                  f_size     = size
                                }
                         else
                           load fd size
      ]
    }
  ]
};

value declare_font name encoding family series shape size params = do
{
  add_font
  {
    fd_name         = name;
    fd_encoding     = encoding;
    fd_family       = family;
    fd_series       = series;
    fd_shape        = shape;
    fd_min_size     = fst size;
    fd_max_size     = snd size;
    fd_loaded_sizes = [];
    fd_data         = params
  }
};

(* |initialise_font_table ()| declares some base fonts. *)

value initialise_font_table () = do
{
  declare_font
    [||] [||] [||] [||] [||]
    (num_zero, num_of_int 10000)
    empty_load_params;

  let decl name encoding family series shape size_min size_max skew_char =
    declare_font (UString.uc_string_of_ascii name)
                 (UString.uc_string_of_ascii encoding)
                 (UString.uc_string_of_ascii family)
                 (UString.uc_string_of_ascii series)
                 (UString.uc_string_of_ascii shape)
                 ((num_of_int size_min), (num_of_int size_max))
                 {
                   (empty_load_params)

                   with

                   flp_size       = num_one;
                   flp_skew_glyph = skew_char
                 }
  in

  (* FIX: Remove these hardwired values, "empty" is enough. *)

  decl "cmr5.tfm"     "OT1" "Computer Modern Roman"  "medium"         "normal"      5  5  Undef;
  decl "cmr6.tfm"     "OT1" "Computer Modern Roman"  "medium"         "normal"      6  6  Undef;
  decl "cmr7.tfm"     "OT1" "Computer Modern Roman"  "medium"         "normal"      7  7  Undef;
  decl "cmr8.tfm"     "OT1" "Computer Modern Roman"  "medium"         "normal"      8  8  Undef;
  decl "cmr9.tfm"     "OT1" "Computer Modern Roman"  "medium"         "normal"      9  9  Undef;
  decl "cmr10.tfm"    "OT1" "Computer Modern Roman"  "medium"         "normal"     10 10  Undef;
  decl "cmr12.tfm"    "OT1" "Computer Modern Roman"  "medium"         "normal"     12 12  Undef;
  decl "cmr17.tfm"    "OT1" "Computer Modern Roman"  "medium"         "normal"     17 17  Undef;
  decl "cmti7.tfm"    "OT1" "Computer Modern Roman"  "medium"         "italic"      7  7  Undef;
  decl "cmti8.tfm"    "OT1" "Computer Modern Roman"  "medium"         "italic"      8  8  Undef;
  decl "cmti9.tfm"    "OT1" "Computer Modern Roman"  "medium"         "italic"      9  9  Undef;
  decl "cmti10.tfm"   "OT1" "Computer Modern Roman"  "medium"         "italic"     10 10  Undef;
  decl "cmti12.tfm"   "OT1" "Computer Modern Roman"  "medium"         "italic"     12 12  Undef;
  decl "cmbx5.tfm"    "OT1" "Computer Modern Roman"  "bold extended"  "normal"      5  5  Undef;
  decl "cmbx6.tfm"    "OT1" "Computer Modern Roman"  "bold extended"  "normal"      6  6  Undef;
  decl "cmbx7.tfm"    "OT1" "Computer Modern Roman"  "bold extended"  "normal"      7  7  Undef;
  decl "cmbx8.tfm"    "OT1" "Computer Modern Roman"  "bold extended"  "normal"      8  8  Undef;
  decl "cmbx9.tfm"    "OT1" "Computer Modern Roman"  "bold extended"  "normal"      9  9  Undef;
  decl "cmbx10.tfm"   "OT1" "Computer Modern Roman"  "bold extended"  "normal"     10 10  Undef;
  decl "cmbx12.tfm"   "OT1" "Computer Modern Roman"  "bold extended"  "normal"     12 12  Undef;
  decl "cmbxti7.tfm"  "OT1" "Computer Modern Roman"  "bold extended"  "italic"      7  7  Undef;
  decl "cmbxti10.tfm" "OT1" "Computer Modern Roman"  "bold extended"  "italic"     10 10  Undef;
  decl "cmbxti12.tfm" "OT1" "Computer Modern Roman"  "bold extended"  "italic"     12 12  Undef;

  decl "cmmi5.tfm"    "OML" "Computer Modern Math Italic"      "medium"  "italic"   5  5   (Simple 127);
  decl "cmmi6.tfm"    "OML" "Computer Modern Math Italic"      "medium"  "italic"   6  6   (Simple 127);
  decl "cmmi7.tfm"    "OML" "Computer Modern Math Italic"      "medium"  "italic"   7  7   (Simple 127);
  decl "cmmi8.tfm"    "OML" "Computer Modern Math Italic"      "medium"  "italic"   8  8   (Simple 127);
  decl "cmmi9.tfm"    "OML" "Computer Modern Math Italic"      "medium"  "italic"   9  9   (Simple 127);
  decl "cmmi10.tfm"   "OML" "Computer Modern Math Italic"      "medium"  "italic"  10 10   (Simple 127);
  decl "cmmi12.tfm"   "OML" "Computer Modern Math Italic"      "medium"  "italic"  12 12   (Simple 127);
  decl "cmmib10.tfm"  "OML" "Computer Modern Math Italic"      "bold"    "italic"  10 10   (Simple 127);

  decl "cmsy5.tfm"    "OMS" "Computer Modern Math Symbols"     "medium"  "normal"   5  5   (Simple  48);
  decl "cmsy6.tfm"    "OMS" "Computer Modern Math Symbols"     "medium"  "normal"   6  6   (Simple  48);
  decl "cmsy7.tfm"    "OMS" "Computer Modern Math Symbols"     "medium"  "normal"   7  7   (Simple  48);
  decl "cmsy8.tfm"    "OMS" "Computer Modern Math Symbols"     "medium"  "normal"   8  8   (Simple  48);
  decl "cmsy9.tfm"    "OMS" "Computer Modern Math Symbols"     "medium"  "normal"   9  9   (Simple  48);
  decl "cmsy10.tfm"   "OMS" "Computer Modern Math Symbols"     "medium"  "normal"  10 10   (Simple  48);
  decl "cmbsy10.tfm"  "OMS" "Computer Modern Math Symbols"     "bold"    "normal"  10 10   (Simple  48);

  decl "cmex9.tfm"    "raw" "Computer Modern Math Extensions"  "medium"  "normal"   9  9  Undef;
  decl "cmex10.tfm"   "raw" "Computer Modern Math Extensions"  "medium"  "normal"  10 10  Undef
};

(* virtual fonts *)

value make_virtual_font name size glyphs italic lig_kern params = do
{
  let pi = {
             pi_width     = num_zero;
             pi_height    = num_zero;
             pi_page_no   = -1;
             pi_old_marks = [];
             pi_new_marks = []
           }
  in
  let vg = Array.init
             (Array.length glyphs)
             (fun i ->
               {
                 FontVirtual.vg_width  = glyphs.(i).b_width.d_base;
                 FontVirtual.vg_height = glyphs.(i).b_height.d_base;
                 FontVirtual.vg_depth  = glyphs.(i).b_depth.d_base;
                 FontVirtual.vg_italic = italic.(i);
                 FontVirtual.vg_glyph  = Box.draw_box pi num_zero num_zero glyphs.(i)
               })
  in

  FontVirtual.make_virtual_font name size vg lig_kern params
};

