
open Runtime;
open Logging;

type output_format = [ DVI | PDF | PS | SVG ];

value time          = Unix.localtime (Unix.time ());
value argv          = Sys.argv;
value jobname       = ref "";
value input_file    = ref "";
value output_file   = ref "";
value log_file      = ref "";
value output_format = ref PDF;

value start_job name = do
{
  let basename = do
  {
    try
      String.sub name 0 (String.rindex name '.')
    with
    [ Not_found -> name ]
  }
  in

  !jobname     := basename;
  !input_file  := name;
  !log_file    := basename ^ ".log";

  match !output_format with
  [ DVI -> !output_file := basename ^ ".dvi"
  | PDF -> !output_file := basename ^ ".pdf"
  | PS  -> !output_file := basename ^ ".ps"
  | SVG -> !output_file := basename ^ ".svg"
  ];

  KPathSea.init argv.(0) !FontMetric.default_bitmap_resolution !FontMetric.default_mf_mode;
  FreeType.ft_init_freetype ();

  log_open !log_file
};

