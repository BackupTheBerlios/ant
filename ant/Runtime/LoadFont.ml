
open FontMetric;

value supported_formats =
[
  (".tfm", `TFM);
  (".TFM", `TFM);
  (".pfb", `Type1);
  (".PFB", `Type1);
  (".pfa", `Type1);
  (".PFA", `Type1);
  (".otf", `OpenType);
  (".OTF", `OpenType);
  (".ttf", `TrueType);
  (".TTF", `TrueType);
  (".ttc", `TrueType);
  (".TTC", `TrueType)
];

value find_font name = do
{
  let rec lookup_format fmts = match fmts with
  [ []                -> raise Not_found
  | [(suf, id) :: fs] -> if XString.match_suffix name suf then
                           (id, String.sub name 0 (String.length name - String.length suf))
                         else
                           lookup_format fs
  ]
  in

  let (fmt, basename) = lookup_format supported_formats in

  match KPathSea.find_file name fmt True with
  [ ""       -> raise Not_found
  | filename -> (filename, basename, fmt)
  ]
};

value load_font name load_params = do
{
  if name = "" then
    FontMetric.empty_font
  else match find_font name with
  [ (file, basename, `TFM)      -> FontTFM.read_tfm file basename load_params
  | (file, basename, `Type1)    -> FontFT.read_ft   file basename load_params
  | (file, basename, `OpenType) -> FontFT.read_ft   file basename load_params
  | (file, basename, `TrueType) -> FontFT.read_ft   file basename load_params
  | _                           -> assert False
  ]
};

