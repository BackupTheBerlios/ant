
open XNum;
open Unicode;

type format =
[ Bitmap
| Bmp
| PostScript
| PDF
];

type bitmap_format =
[ Indexed of string
| RGB
| RGBA
| CMYK
];

type bitmap =
{
  bm_format   : bitmap_format;
  bm_width    : int;
  bm_height   : int;
  bm_depth    : int;
  bm_scanline : int -> string
};

value get_dimensions file = do
{
  try
    let (fmt, header) = CamlImages.Images.file_format file in

    let f = match fmt with
    [ CamlImages.Images.Ps  -> PostScript
    | CamlImages.Images.Bmp -> Bmp
    | _                     -> Bitmap
    ]
    in

    let dpi = match CamlImages.Images.dpi header.CamlImages.Images.header_infos with
    [ Some dpi -> num_of_ints (int_of_float (10000.0 *. dpi)) 10000
    | None     -> num_zero
    ]
    in

    let factor = if dpi >=/ num_zero then
                   Dim.inch // dpi
                 else
                   Dim.inch // num_of_int 100
    in

    let width  = factor */ num_of_int header.CamlImages.Images.header_width  in
    let height = factor */ num_of_int header.CamlImages.Images.header_height in

    (f, width, height, dpi)
  with
  [ _ -> do
    {
      let s   = IO.make_in_stream file in
      let buf = IO.read_string s 6     in

      if buf = "%%PDF-" then do
      {
        (Bitmap, num_zero, num_zero, num_zero)
      }
      else
        (Bitmap, num_zero, num_zero, num_zero)
    }]
};

value get_bounding_box = CamlImages.Ps.get_bounding_box;

value get_colour_map img = do
{
  let cs = IO.make_buffer_stream 0x300    in
  let cm = img.CamlImages.Index8.colormap in

  for i = 0 to cm.CamlImages.Color.max do
  {
    let (r,g,b) = CamlImages.get_index8_colourmap img i in

    IO.write_byte cs r;
    IO.write_byte cs g;
    IO.write_byte cs b
  };
  IO.to_string cs
};

value read_bitmap file = do
{
  try
    let img             = CamlImages.Images.load file [] in
    let (width, height) = CamlImages.Images.size img     in

    match img with
    [ CamlImages.Images.Index8 x ->
      {
        bm_format   = Indexed (get_colour_map x);
        bm_width    = width;
        bm_height   = height;
        bm_depth    = 8;
        bm_scanline = CamlImages.Index8.get_scanline x
      }
    | CamlImages.Images.Index16 x ->
      {
        bm_format   = RGB;
        bm_width    = width;
        bm_height   = height;
        bm_depth    = 8;
        bm_scanline = CamlImages.Rgb24.get_scanline (CamlImages.Index16.to_rgb24 x)
      }
    | CamlImages.Images.Rgb24 x ->
      {
        bm_format   = RGB;
        bm_width    = width;
        bm_height   = height;
        bm_depth    = 8;
        bm_scanline = CamlImages.Rgb24.get_scanline x
      }
    | CamlImages.Images.Rgba32 x ->
      {
        bm_format   = RGBA;
        bm_width    = width;
        bm_height   = height;
        bm_depth    = 8;
        bm_scanline = CamlImages.Rgba32.get_scanline x
      }
    | CamlImages.Images.Cmyk32 x ->
      {
        bm_format   = CMYK;
        bm_width    = width;
        bm_height   = height;
        bm_depth    = 8;
        bm_scanline = CamlImages.Cmyk32.get_scanline x
      }
    ]
  with
  [ _ -> { bm_width = 0;  bm_height = 0;  bm_depth = 0;
           bm_format = RGB;  bm_scanline = fun _ -> assert False } ]
};

