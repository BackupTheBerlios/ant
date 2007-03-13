
open Runtime;
open Logging;

type output_format = [ DVI | XDVI | PDF | PS | SVG ];

value time             = Unix.localtime (Unix.time ());
value argv             = Sys.argv;
value output_format    = ref PDF;
value source_specials  = ref False;
value jobname          = ref "";
value input_file       = ref "";
value output_file      = ref "";
value src_special_file = ref "";
value log_file         = ref "";

value src_special_stream = ref (IO.make_buffer_stream 10 :> IO.ostream);

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

  !jobname          := basename;
  !input_file       := name;
  !log_file         := basename ^ ".log";
  !src_special_file := basename ^ ".pdfsync";

  match !output_format with
  [ DVI  -> !output_file := basename ^ ".dvi"
  | XDVI -> !output_file := basename ^ ".xdvi"
  | PDF  -> do
    {
      !output_file := basename ^ ".pdf";

      if !source_specials then do
      {
        !src_special_stream := IO.make_out_stream !src_special_file;
        IO.write_string !src_special_stream basename;
        IO.write_string !src_special_stream "\nversion 0\n";
      }
      else ()
    }
  | PS  -> !output_file := basename ^ ".ps"
  | SVG -> !output_file := basename ^ ".svg"
  ];

  KPathSea.init argv.(0) !FontMetric.default_bitmap_resolution !FontMetric.default_mf_mode;
  FreeType.ft_init_freetype ();

  log_open !log_file
};

