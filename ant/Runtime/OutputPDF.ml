
open XNum;
open Unicode.Types;
open FontMetric;
open Logging;

type font_ref =
{
  font              : font_metric;
  first_glyph_index : int;        (* minimal glyph index (from font-metric)    *)
  used_glyphs       : mutable int;(* number of used glyphs                     *)
  glyph_map         : array int   (* mapping internal glyph-index -> PDF index *)
};

type image_ref = (string * int);  (* filename, object id    *)

type text_state =
{
  font_major : int;
  font_minor : int;
  font_size  : float;
  x_pos      : float;
  y_pos      : float
};

type state =
{
  pdf        : PDF.pdf_file IO.ostream;
  os         : mutable IO.iorstream;
  fonts      : mutable list font_ref;
  images     : mutable list image_ref;
  text_state : mutable option text_state
};

value new_state filename =
{
  pdf        = PDF.create_pdf_file filename 1.4;
  os         = IO.make_buffer_stream 0x10000;
  fonts      = [];
  images     = [];
  text_state = None
};

value pt_to_bp x = float_of_num (num_of_ints 7200 7227 */ x);

value write_bitmap cs bm = do
{
  if bm.Bitmap.bm_width > 0 && bm.Bitmap.bm_height > 0 then do
  {
    IO.printf cs "BI /IM true /D [1 0] /W %d /H %d ID "
      bm.Bitmap.bm_width
      bm.Bitmap.bm_height;
    IO.write_string cs bm.Bitmap.bm_data;
    IO.write_string cs "EI "
  }
  else ()
};

value bitmap_to_type3_glyph state fm g = do
{
  let gm = fm.glyph_metric.(g - fm.first_glyph) in
  let g  = fm.get_glyph_bitmap fm g             in

  let cs = IO.make_buffer_stream 0x100 in

  let hdpi  = 72.27 /. 72.0 *. g.Glyph.g_hdpp      in
  let vdpi  = 72.27 /. 72.0 *. g.Glyph.g_vdpp      in
  let min_x = float_of_int g.Glyph.g_min_x /. hdpi in
  let min_y = float_of_int g.Glyph.g_min_y /. vdpi in
  let max_x = float_of_int g.Glyph.g_max_x /. hdpi in
  let max_y = float_of_int g.Glyph.g_max_y /. vdpi in

  IO.printf cs "%f 0 %f %f %f %f d1 "
    (pt_to_bp gm.gm_width) min_x min_y max_x max_y;

  if g.Glyph.g_bitmap.Bitmap.bm_width > 0 &&
     g.Glyph.g_bitmap.Bitmap.bm_height > 0 then do
  {
    IO.printf cs "1 0 0 1 %f %f cm %f 0 0 %f 0 0 cm "
      min_x min_y
      (float_of_int (g.Glyph.g_max_x - g.Glyph.g_min_x + 1) /. hdpi)
      (float_of_int (g.Glyph.g_max_y - g.Glyph.g_min_y + 1) /. vdpi);
    write_bitmap cs g.Glyph.g_bitmap;
  }
  else ();

  let contents = PDF.alloc_object state.pdf in

  PDF.set_object state.pdf contents (PDF.Stream [] cs);

  ((min_x, min_y, max_x, max_y), PDF.Reference contents 0)
};

value font_encoding fm encoding = do
{
  let rec get_name_list i list = do
  {
    if i < 0 then
      list
    else
      get_name_list (i-1) [ PDF.Symbol (fm.get_glyph_name encoding.(i)) :: list ]
  }
  in

  PDF.Dictionary
    [("Type",        PDF.Symbol "Encoding");
     ("Differences", PDF.Array [PDF.Int 0
                                 :: get_name_list (Array.length encoding - 1) [] ])]
};

value write_cmap stream fm font_name encoding = do
{
  IO.write_string stream "%!PS-Adobe-3.0 Resource-CMap\n";
  IO.write_string stream "%%DocumentNeededResources: ProcSet (CIDInit)\n";
  IO.write_string stream "%%IncludeResource: ProcSet (CIDInit)\n";
  IO.printf       stream "%%BeginResource: CMap (CM-%s)\n" font_name;
  IO.printf       stream "%%Title: (CM-%s ant %s 0)\n" font_name font_name;
  IO.write_string stream "%%Version: 1.000\n";
  IO.write_string stream "%%EndComments\n";

  IO.write_string stream "/CIDInit /ProcSet findresource begin\n";
  IO.write_string stream "12 dict begin\n";
  IO.write_string stream "begincmap\n";

  IO.write_string stream "/CIDSystemInfo\n";
  IO.write_string stream "<< /Registry (ant)\n";
  IO.printf       stream "/Ordering (%s)\n" font_name;
  IO.write_string stream "/Supplement 0\n";
  IO.write_string stream ">> def\n";
  IO.printf       stream "/CMapName /CM-%s def\n" font_name;
  IO.write_string stream "/CMapType 2 def\n";

  IO.write_string stream "1 begincodespacerange\n";
  IO.write_string stream "<00> <FF>\n";
  IO.write_string stream "endcodespacerange\n";

  IO.printf       stream "%d beginbfchar\n" (Array.length encoding);

  for i = 0 to Array.length encoding - 1 do
  {
    IO.printf stream "<%02x> <" i;

    match get_unicode fm (Substitute.Simple encoding.(i)) with
    [ []  -> IO.write_string stream "0000"
    | str -> List.iter (fun c -> IO.printf stream "%04x" c) str
    ];

    IO.write_string stream ">\n"
  };

  IO.write_string stream "endbfchar\n";

  IO.write_string stream "endcmap\n";
  IO.write_string stream "CMapName currentdict /CMap defineresource pop\n";
  IO.write_string stream "end\n";
  IO.write_string stream "end\n";
  IO.write_string stream "%%EndResource\n";
  IO.write_string stream "%%EOF\n"
};

value new_type3_font state font_name fm encoding = do
{
  let scale = pt_to_bp (num_one // fm.at_size) in

  let width_array = Array.map
    (fun n -> do
      {
        let gm = fm.glyph_metric.(n - fm.first_glyph) in
        PDF.Float (pt_to_bp gm.gm_width)
      })
    encoding
  in

  let rec calc_glyph_data n dict ((min_x, min_y, max_x, max_y) as bounds) = do
  {
    if n >= Array.length encoding then
      (bounds, dict)
    else do
    {
      let g = encoding.(n) in
      let ((x1, y1, x2, y2), bitmap) = bitmap_to_type3_glyph state fm g in

      calc_glyph_data
        (n + 1)
        [ (fm.get_glyph_name g, bitmap)
          :: dict]
        (min min_x x1, min min_y y1, max max_x x2, max max_y y2)
    }
  }
  in
  let ((min_x, min_y, max_x, max_y), char_procs) =
        calc_glyph_data 0 [] (0.0, 0.0, 0.0, 0.0)
  in

  let cmap = PDF.alloc_object state.pdf in
  let obj  = PDF.alloc_object state.pdf in

  let cmap_data = IO.make_buffer_stream 0x1000 in

  write_cmap cmap_data fm font_name encoding;

  PDF.set_object state.pdf cmap (PDF.Stream [] cmap_data);

  PDF.set_object state.pdf obj
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "Font");
        ("Subtype",        PDF.Symbol "Type3");
        ("FontBBox",       PDF.Array [PDF.Float min_x;
                                      PDF.Float min_y;
                                      PDF.Float max_x;
                                      PDF.Float max_y]);
        ("FontMatrix",     PDF.Array [PDF.Float scale; PDF.Int 0;
                                      PDF.Int 0; PDF.Float scale;
                                      PDF.Int 0; PDF.Int 0]);
        ("FirstChar",      PDF.Int    0);
        ("LastChar",       PDF.Int    (Array.length encoding - 1));
        ("Widths",         PDF.Array  (Array.to_list width_array));
        ("CharProcs",      PDF.Dictionary char_procs);
        ("Encoding",       font_encoding fm encoding);
        ("Resources",      PDF.Dictionary []);
        ("ToUnicode",      PDF.Reference cmap 0)
      ]);
  obj
};

value new_type1_font state font_name fm encoding = do
{
  let scale x = float_of_num (x */ num_of_int 1000 // fm.at_size) in
  let is_cff  = match fm.font_type with
  [ OpenTypeCFF -> True
  | PostScript  -> False
  | _           -> assert False
  ]
  in

  let width_array = Array.map
    (fun n -> do
      {
        let gm = fm.glyph_metric.(n - fm.first_glyph) in
        PDF.Float (scale gm.gm_width)
      })
    encoding
  in

  let (max_width, max_height, max_depth) =
    Array.fold_left
      (fun (w,h,d) n -> do
        {
          let gm = fm.glyph_metric.(n - fm.first_glyph) in
          (max_num w gm.gm_width,
           max_num h gm.gm_height,
           max_num d gm.gm_depth)
         })
      (num_zero, num_zero, num_zero)
      encoding
  in

  let cmap = PDF.alloc_object state.pdf in
  let ff   = PDF.alloc_object state.pdf in
  let fd   = PDF.alloc_object state.pdf in
  let obj  = PDF.alloc_object state.pdf in

  let cmap_data = IO.make_buffer_stream 0x1000 in

  write_cmap cmap_data fm font_name encoding;

  PDF.set_object state.pdf cmap (PDF.Stream [] cmap_data);

  if is_cff then do
  {
    let font      = OpenType.read_font_tables fm.file_name in
    let cff       = OpenType.get_cff font                  in
    let font_data = IO.from_string cff                     in

    PDF.set_object state.pdf ff
      (PDF.Stream [("Subtype", PDF.Symbol "Type1C")] font_data)
  }
  else do
  {
    let font_data          = IO.make_buffer_stream 0x1000                   in
    let (len1, len2, len3) = Type1.embedd_type1_font fm.file_name font_data in

    PDF.set_object state.pdf ff
      (PDF.Stream
        [("Length1", PDF.Int len1);
         ("Length2", PDF.Int len2);
         ("Length3", PDF.Int len3)]
        font_data)
  };

  PDF.set_object state.pdf fd
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "FontDescriptor");
        ("FontName",       PDF.Symbol fm.ps_name);
        ("Flags",          PDF.Int 0x4);
        ("FontBBox",       PDF.Array [PDF.Int 0;
                                      PDF.Float (scale (minus_num max_depth));
                                      PDF.Float (scale max_width);
                                      PDF.Float (scale max_height)]);
        ("ItalicAngle",    PDF.Int 0);
        ("Ascent",         PDF.Float (scale max_height));
        ("Descent",        PDF.Float (scale (minus_num max_depth)));
        ("CapHeight",      PDF.Float (scale max_height));
        ("StemV",          PDF.Float (scale fm.parameter.rule_thickness));
        (if is_cff then
          "FontFile3"
         else
          "FontFile",      PDF.Reference ff 0)
      ]);

  PDF.set_object state.pdf obj
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "Font");
        ("Subtype",        PDF.Symbol "Type1");
        ("BaseFont",       PDF.Symbol fm.ps_name);
        ("FirstChar",      PDF.Int    0);
        ("LastChar",       PDF.Int    (Array.length encoding - 1));
        ("Widths",         PDF.Array  (Array.to_list width_array));
        ("Encoding",       font_encoding fm encoding);
        ("ToUnicode",      PDF.Reference cmap 0);
        ("FontDescriptor", PDF.Reference fd 0)
      ]);

  obj
};

value new_truetype_font state font_name fm encoding = do
{
  let scale x = float_of_num (x */ num_of_int 1000 // fm.at_size) in

  let width_array = Array.map
    (fun n -> do
      {
        let gm = fm.glyph_metric.(n - fm.first_glyph) in
        PDF.Float (scale gm.gm_width)
      })
    encoding
  in

  let (max_width, max_height, max_depth) =
    Array.fold_left
      (fun (w,h,d) n -> do
        {
          let gm = fm.glyph_metric.(n - fm.first_glyph) in
          (max_num w gm.gm_width,
           max_num h gm.gm_height,
           max_num d gm.gm_depth)
         })
      (num_zero, num_zero, num_zero)
      encoding
  in

  let cmap = PDF.alloc_object state.pdf in
  let ff   = PDF.alloc_object state.pdf in
  let fd   = PDF.alloc_object state.pdf in
  let obj  = PDF.alloc_object state.pdf in

  let cmap_data = IO.make_buffer_stream 0x1000 in

  write_cmap cmap_data fm font_name encoding;

  PDF.set_object state.pdf cmap (PDF.Stream [] cmap_data);

  let font   = OpenType.read_font_tables fm.file_name in
  let subset = IO.make_buffer_stream 0x10000          in

  OpenType.write_subset subset font encoding;

  let len = IO.bytes_written subset in

  PDF.set_object state.pdf ff (PDF.Stream [("Length1", PDF.Int len)] subset);

  PDF.set_object state.pdf fd
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "FontDescriptor");
        ("FontName",       PDF.Symbol fm.ps_name);
        ("Flags",          PDF.Int 0x4);
        ("FontBBox",       PDF.Array [PDF.Int 0;
                                      PDF.Float (scale (minus_num max_depth));
                                      PDF.Float (scale max_width);
                                      PDF.Float (scale max_height)]);
        ("ItalicAngle",    PDF.Int 0);
        ("Ascent",         PDF.Float (scale max_height));
        ("Descent",        PDF.Float (scale (minus_num max_depth)));
        ("CapHeight",      PDF.Float (scale max_height));
        ("StemV",          PDF.Float (scale fm.parameter.rule_thickness));
        ("FontFile2",      PDF.Reference ff 0)
      ]);

  PDF.set_object state.pdf obj
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "Font");
        ("Subtype",        PDF.Symbol "TrueType");
        ("BaseFont",       PDF.Symbol fm.ps_name);
                                                 (* font subset: "AAAAAA+" ^ fm.ps_name *)
        ("FirstChar",      PDF.Int    0);
        ("LastChar",       PDF.Int    (Array.length encoding - 1));
        ("Widths",         PDF.Array  (Array.to_list width_array));
        ("ToUnicode",      PDF.Reference cmap 0);
        ("FontDescriptor", PDF.Reference fd 0)
      ]);

  obj
};

value new_font state font = do
{
  let n  = List.length state.fonts in
  let fd =
    {
      font              = font;
      first_glyph_index = font.first_glyph;
      used_glyphs       = 0;
      glyph_map         = Array.make (font.last_glyph - font.first_glyph + 1) (-1)
    }
  in

  state.fonts := state.fonts @ [fd];

  (n, fd)
};

value make_font_obj state font_number font_def = do
{
  iter 0 []

  where rec iter i objs = do
  {
    if i >= font_def.used_glyphs then
      objs
    else do
    {
      let encoding = Array.make (min 0x100 (font_def.used_glyphs - i)) (-1) in

      (* select all glyphs mapped to numbers between i * 0x100 and i * 0x100 + 0xff *)

      Array.iteri
        (fun x y -> do
          {
            if y >= i && y < i + 0x100 then
              encoding.(y - i) := x + font_def.first_glyph_index
            else
              ()
          })
        font_def.glyph_map;

      let new_font = match font_def.font.font_type with
      [ PostScript
      | OpenTypeCFF -> new_type1_font
      | TrueType    -> new_truetype_font
      | Other       -> new_type3_font
      ]
      in

      let obj = new_font state (Printf.sprintf "F%d.%d" font_number i) font_def.font encoding in

      iter (i + 0x100) [obj :: objs]
    }
  }
};

value get_glyph_index font_def char = do
{
  let i = font_def.glyph_map.(char - font_def.first_glyph_index) in

  if i >= 0 then
    i
  else do
  {
    font_def.glyph_map.(char - font_def.first_glyph_index) := font_def.used_glyphs;
    font_def.used_glyphs := font_def.used_glyphs + 1;
    font_def.used_glyphs - 1
  }
};

value load_font state font char = do
{
  find 0 state.fonts

  where rec find i fonts = match fonts with
  [ [f :: fs] -> do
    {
      if f.font == font then
        (i, get_glyph_index f char)
      else
        find (i + 1) fs
    }
  | [] -> do
    {
      let (i, fd) = new_font state font in

      (i, get_glyph_index fd char)
    }
  ]
};

(* images *)

value new_image state file = do
{
  let conv_id cs s = do
  {
    IO.write_string cs s
  }
  in
  let conv_rgba cs s = do
  {
    for i = 0 to String.length s / 4 - 1 do
    {
      IO.write_char cs s.[3*i];
      IO.write_char cs s.[3*i+1];
      IO.write_char cs s.[3*i+2]
    }
  }
  in
  let get_colourmap x = do
  {
    let cs = IO.make_buffer_stream 0x100  in
    let cm = x.CamlImages.Index8.colormap in

    for i = 0 to cm.CamlImages.Color.max do
    {
      let (r,g,b) = CamlImages.get_index8_colourmap x i in

      IO.write_byte cs r;
      IO.write_byte cs g;
      IO.write_byte cs b
    };
    cs
  }
  in
  let image_info img = match img with
  [ CamlImages.Images.Index8 x  -> (PDF.Array [PDF.Symbol "Indexed";
                                               PDF.Symbol "DeviceRGB";
                                               PDF.Int 255;
                                               PDF.String (get_colourmap x)],
                                    8,
                                    CamlImages.Index8.get_scanline x,
                                    conv_id)
  | CamlImages.Images.Index16 x -> (PDF.Symbol "DeviceRGB",
                                    8,
                                    CamlImages.Rgb24.get_scanline (CamlImages.Index16.to_rgb24 x),
                                    conv_id)
  | CamlImages.Images.Rgb24 x   -> (PDF.Symbol "DeviceRGB",
                                    8,
                                    CamlImages.Rgb24.get_scanline x,
                                    conv_id)
  | CamlImages.Images.Rgba32 x  -> (PDF.Symbol "DeviceRGB",
                                    8,
                                    CamlImages.Rgba32.get_scanline x,
                                    conv_rgba)
  | CamlImages.Images.Cmyk32 x  -> (PDF.Symbol "DeviceCMYK",
                                    8,
                                    CamlImages.Cmyk32.get_scanline x,
                                    conv_id)
  ]
  in
  let image_data scan conv img = do
  {
    let (w, h) = CamlImages.Images.size img in
    let cs     = IO.make_buffer_stream (w * h / 8) in


    for i = 0 to h - 1 do
    {
      let s = scan i in

      conv cs s
    };
    cs
  }
  in
  let obj = PDF.alloc_object state.pdf in
  let n   = List.length state.images   in

  state.images := state.images @ [(file, obj)];

  let img             = CamlImages.Images.load file [] in
  let (width, height) = CamlImages.Images.size img     in
  let (colsp, bits, scan, conv) = image_info img       in

  PDF.set_object state.pdf obj
    (PDF.Stream
      [
        ("Type",             PDF.Symbol "XObject");
        ("Subtype",          PDF.Symbol "Image");
        ("Name",             PDF.Symbol (Printf.sprintf "G%d" n));
        ("Width",            PDF.Int width);
        ("Height",           PDF.Int height);
        ("ColorSpace",       colsp);
        ("BitsPerComponent", PDF.Int bits)
      ]
      (image_data scan conv img));

  (n, obj)
};

value load_image state file = do
{
  find 0 state.images

  where rec find n images = match images with
  [ [(i, _) :: is] -> do
    {
      if i = file then
        n
      else
        find (n + 1) is
    }
  | [] -> fst (new_image state file)
  ]
};

value select_image state file = do
{
  let n = load_image state file in

  IO.printf state.os " /G%d " n
};

(* text mode *)

value begin_text_mode state font_major font_minor font_size x_pos y_pos = do
{
  let x    = pt_to_bp x_pos     in
  let y    = pt_to_bp y_pos     in
  let size = pt_to_bp font_size in

  match state.text_state with
  [ None    -> IO.printf state.os "BT /F%d.%d %f Tf %f %f Td " font_major font_minor size x y
  | Some ts -> do
    {
      if ts.font_major <> font_major || ts.font_minor <> font_minor || ts.font_size <> size then
        IO.printf state.os "/F%d.%d %f Tf " font_major font_minor size
      else ();

      IO.printf state.os "%f %f Td " (x -. ts.x_pos) (y -. ts.y_pos)
    }
  ];

  state.text_state :=
    Some {
           font_major = font_major;
           font_minor = font_minor;
           font_size  = size;
           x_pos      = x;
           y_pos      = y
         }
};

value end_text_mode state = match state.text_state with
[ None   -> ()
| Some _ -> do
  {
    IO.write_string state.os "ET ";
    state.text_state := None
  }
];

(* page contents *)

value rec write_box state x y box = match box with
[ Empty           -> ()
| Rule w h        -> write_rule  state x y w h
| SimpleGlyph g f -> write_char  state x y g f
| Image w h f     -> write_image state x y w h f
| Group bs        -> write_group state x y bs
| Command cmd     -> match cmd with
    [ `DVI_Special _ -> ()
    ]
]
and write_rule state x y width height = do
{
  end_text_mode state;

  IO.printf state.os "%f %f %f %f re f "
      (pt_to_bp x) (pt_to_bp y) (pt_to_bp width) (pt_to_bp height)
  ; ()
}
and write_char state x y c f = do
{
  let (fn, cn) = load_font state f c in

  begin_text_mode state fn (cn / 0x100) f.at_size x y;

  IO.printf state.os "<%02x> Tj " (cn land 0xff);
  ()
}
and write_image state x y width height file = do
{
  end_text_mode state;

  IO.printf       state.os "q %f 0 0 %f %f %f cm "
                           (pt_to_bp width) (pt_to_bp height) (pt_to_bp x) (pt_to_bp y);
  select_image    state    file;
  IO.write_string state.os "Do Q "
}
and write_path state x y path_cmd path = do
{
  let rec draw_path cur_x cur_y path = match path with
  [ [] -> match path_cmd with
          [ Graphic.Stroke -> IO.write_string state.os "S "
          | Graphic.Fill   -> IO.write_string state.os "f "
          | Graphic.Clip   -> IO.write_string state.os "W n "
          ]
  | [(ax,ay,bx,by,cx,cy,dx,dy) :: ps] -> do
    {
      if ax <>/ cur_x || ay <>/ cur_y then do
      {
        match path_cmd with
        [ Graphic.Stroke -> IO.write_string state.os "S "
        | Graphic.Fill   -> IO.write_string state.os "f "
        | Graphic.Clip   -> IO.write_string state.os "W n "
        ];
        IO.printf state.os "%f %f m " (pt_to_bp (x +/ ax)) (pt_to_bp (y +/ ay))
      }
      else ();

      IO.printf state.os "%f %f %f %f %f %f c "
        (pt_to_bp (x +/ bx)) (pt_to_bp (y +/ by))
        (pt_to_bp (x +/ cx)) (pt_to_bp (y +/ cy))
        (pt_to_bp (x +/ dx)) (pt_to_bp (y +/ dy));

      draw_path dx dy ps
    }
  ]
  in

  match path with
  [ [] -> ()
  | [(ax,ay,_,_,_,_,_,_) :: _] -> do
    {
      end_text_mode state;

      (* Choose arbitrary coordinates for the current point.
         Just make sure they are different from the first point of the path. *)

      IO.printf state.os "%f %f m " (pt_to_bp (x +/ ax)) (pt_to_bp (y +/ ay));

      draw_path ax ay path
    }
  ]
}
and write_group state x y gfx_cmds = do
{
  let set_colour col = match col with
  [ Graphic.Grey x       -> do
    {
      IO.printf state.os "%f g\n" (float_of_num x)
      ; ()
    }
  | Graphic.RGB r g b    -> do
    {
      IO.printf state.os "%f %f %f rg\n"
                (float_of_num r) (float_of_num g) (float_of_num b)
      ; ()
    }
  | Graphic.CMYK c m y k -> do
    {
      IO.printf state.os "%f %f %f %f k\n"
                (float_of_num c) (float_of_num m) (float_of_num y) (float_of_num k)
      ; ()
    }
  ]
  in
  let set_alpha a = do
  {
    (* FIX *)
    ()
  }
  in
  let set_line_width w = do
  {
    end_text_mode state;
    IO.printf state.os "%f w\n" (pt_to_bp w)
  }
  in
  let set_line_cap c = do
  {
    end_text_mode state;

    match c with
    [ Graphic.Butt   -> IO.write_string state.os "0 J\n"
    | Graphic.Circle -> IO.write_string state.os "1 J\n"
    | Graphic.Square -> IO.write_string state.os "2 J\n"
    ]
  }
  in
  let set_line_join j = do
  {
    end_text_mode state;

    match j with
    [ Graphic.Miter -> IO.write_string state.os "0 j\n"
    | Graphic.Round -> IO.write_string state.os "1 j\n"
    | Graphic.Bevel -> IO.write_string state.os "2 j\n"
    ]
  }
  in
  let set_miter_limit l = do
  {
    end_text_mode state;
    IO.printf state.os "%f M\n" (float_of_num l)
  }
  in

  let write_gfx cmd = match cmd with
  [ Graphic.PutBox dx dy b  -> write_box state (x +/ dx) (y +/ dy) b
  | Graphic.Draw pc p       -> write_path state x y pc p
  | Graphic.SetColour col   -> set_colour col
  | Graphic.SetAlpha a      -> set_alpha a
  | Graphic.SetBgColour _   -> assert False
  | Graphic.SetLineWidth  w -> set_line_width  w
  | Graphic.SetLineCap    c -> set_line_cap    c
  | Graphic.SetLineJoin   j -> set_line_join   j
  | Graphic.SetMiterLimit l -> set_miter_limit l
  ]
  in

  end_text_mode state;

  IO.write_string state.os "q ";

  List.iter write_gfx gfx_cmds;

  end_text_mode state;

  IO.write_string state.os "Q "
};

(* document structure *)

value create_page state parent page = do
{
  let page_obj = PDF.alloc_object state.pdf in
  let contents = PDF.alloc_object state.pdf in

  let old_os   = state.os in                    (* save old stream *)

  state.os := IO.make_buffer_stream 0x1000;

  write_box state num_zero page.p_height page.p_contents;

  PDF.set_object state.pdf contents (PDF.Stream [] state.os);

  PDF.set_object state.pdf page_obj
    (PDF.Dictionary
      [
        ("Type",     PDF.Symbol "Page");
        ("Parent",   PDF.Reference parent 0);
        ("MediaBox", PDF.Array [PDF.Int 0;
                                PDF.Int 0;
                                PDF.Float (pt_to_bp page.p_width);
                                PDF.Float (pt_to_bp page.p_height)]);
        ("Contents", PDF.Reference contents 0)
      ]);

  state.os := old_os;                           (* restore stream *)

  page_obj
};

value create_pdf state pages = do
{
  let rec get_font_refs i fonts = match fonts with
  [ []        -> []
  | [f :: fs] -> do
    {
      let objs = make_font_obj state i f in

      iter 0 objs

      where rec iter n objs = match objs with
      [ []      -> get_font_refs (i + 1) fs
      | [o::os] -> [ ( Printf.sprintf "F%d.%d" i n, PDF.Reference o 0 )
                     :: iter (n + 1) os]
      ]
    }
  ]
  in
  let rec get_image_refs n images = match images with
  [ []               -> []
  | [(_, obj) :: is] -> [( Printf.sprintf "G%d" n, PDF.Reference obj 0 )
                          :: get_image_refs (n + 1) is]
  ]
  in

  let root_obj  = PDF.alloc_object state.pdf in
  let pages_obj = PDF.alloc_object state.pdf in
  let page_refs = List.map (create_page state pages_obj) pages in

  PDF.set_root state.pdf (PDF.Reference root_obj 0);

  PDF.set_object state.pdf pages_obj
    (PDF.Dictionary
      [
        ("Type",      PDF.Symbol "Pages");
        ("Resources", PDF.Dictionary
                        [ ("Font",    PDF.Dictionary (get_font_refs  0 state.fonts));
                          ("XObject", PDF.Dictionary (get_image_refs 0 state.images))
                        ]);
        ("Count",     PDF.Int (List.length pages));
        ("Kids",      PDF.Array (List.map (fun obj -> PDF.Reference obj 0) page_refs))
      ]);

  PDF.set_object state.pdf root_obj
    (PDF.Dictionary
      [
        ("Type",  PDF.Symbol "Catalog");
        ("Pages", PDF.Reference pages_obj 0)
      ])
};

value write_pdf_file name _comment pages = do
{
  let state = new_state name in

  create_pdf state pages;

  PDF.finish_pdf_file state.pdf
};

