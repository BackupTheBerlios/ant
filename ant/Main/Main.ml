
open Runtime;
open Logging;
open Markup;

module Job = Engine.Job;

value print_help () = do
{
  print_string "This is ant, version 0.8.\n\n";
  print_string "USAGE: ant [options] <input-file>\n\n";
  print_string "Supported options are:\n\n";
  print_string "--format=<fmt>    where <fmt> is \"dvi\", \"xdvi\", \"ps\", \"pdf\", or \"svg\"\n";
  print_string "--src-specials    enables the generation of source specials\n";
  print_string "--debug=<flags>   where <flags> may contain the following letters:\n";
  print_string "                    e   engine\n";
  print_string "                    i   input\n";
  print_string "                    l   line breaks\n";
  print_string "                    m   macro expansion\n";
  print_string "                    p   page breaks\n";
  print_string "                    s   various stacks\n";
  print_string "--help            print this message and exists\n"
};

value rec process_options file args = match args with
[ [] -> match file with
  [ None -> do
    {
      print_string "No input file specified!\n\n";
      print_help ();
      None
    }
  | Some _ -> file
  ]
| [arg::args] -> do
  {
    let len = String.length arg in

    if len = 0 then
      process_options file args
    else if arg.[0] = '-' && len > 1 then do
    {
      let v = if arg.[1] = '-' then
                String.sub arg 2 (len - 2)
              else
                String.sub arg 1 (len - 1)
              in
      let len = String.length v in

      if v = "help" then do
      {
        print_help ();
        None
      }
      else if len >= 6 && String.sub v 0 6 = "format" then do
      {
        if len = 6 || v.[6] <> '=' then do
        {
          print_string "Unknown option!\n\n";
          print_help ();
          None
        }
        else match String.lowercase (String.sub v 7 (len - 7)) with
        [ "dvi" -> do
          {
            !Job.output_format := Job.DVI;
            process_options file args
          }
        | "xdvi" -> do
          {
            !Job.output_format := Job.XDVI;
            process_options file args
          }
        | "pdf" -> do
          {
            !Job.output_format := Job.PDF;
            process_options file args
          }
        | "ps" -> do
          {
            !Job.output_format := Job.PS;
            process_options file args
          }
        | "svg" -> do
          {
            !Job.output_format := Job.SVG;
            process_options file args
          }
        | _     -> do
          {
            print_string ("File format `" ^ v ^ "' not supported!\n\n");
            print_help ();
            None
          }
        ];
      }
      else if len >= 5 && String.sub v 0 5 = "debug" then do
      {
        if len = 5 then do
        {
          !ParseState.tracing_macros := True;              (* default: --debug=m *)
          process_options file args
        }
        else if v.[5] <> '=' then do
        {
          print_string "Unknown option!\n\n";
          print_help ();
          None
        }
        else do
        {
          for i = 6 to len - 1 do
          {
            match v.[i] with
            [ 'e' -> !Engine.Evaluate.tracing_engine             := True
            | 'i' -> !ParseState.tracing_input                   := True
            | 'l' -> !Typesetting.ParLayout.tracing_line_breaks  := True
            | 'm' -> !ParseState.tracing_macros                  := True
            | 'g' -> !Typesetting.AreaGalley.tracing_page_breaks := True
            | 'p' -> !Typesetting.PageLayout.tracing_page_layout := True
            | 's' -> !ParseState.tracing_stacks                  := True
            | _   -> ()
            ]
          };
          process_options file args
        }
      }
      else if len >= 12 && String.sub v 0 12 = "src-specials" then do
      {
        !Job.source_specials := True;
        process_options file args
      }
      else do
      {
        print_string "Unknown option!\n\n";
        print_help ();
        None
      }
    }
    else match file with
    [ None   -> process_options (Some arg) args
    | Some _ -> do
                {
                  print_string "More than one input file given!\n\n";
                  print_help ();
                  None
                }
    ]
  }
];

value main () = do
{
  Unicode.UString.set_string_format `UTF8;

  match process_options None (List.tl (Array.to_list Sys.argv)) with
  [ None      -> ()
  | Some file -> do
    {
      (* Check whether the file exists. *)
      try
        Unix.stat file
      with
      [ Unix.Unix_error err _ _ -> do
        {
          log_string Sys.argv.(0);
          log_string ": ";
          log_string (Unix.error_message err);
          log_string "\n";
          exit 1
        }
      ];

      Job.start_job file;
      Run.initialise ();

      let (ast, ps) = Run.parse_file !Job.input_file in

      let pages     = Engine.Evaluate.evaluate ast in

      if ParseState.compare_references ps then do
      {
        log_string "\nSome references have changed.\nYou might need to rerun ant to get a correct output.\n"
      }
      else ();

      ParseState.write_references ps (!Job.jobname ^ ".refdb");

      Engine.Output.output_pages !Job.output_format pages
    }
  ]
};

(* main (); *)
try
  main ()
with
[ e -> do
  {
    print_string ("uncaught exception: " ^ (Printexc.to_string e) ^ "!\n");
    flush stdout;
    raise e
  }
];

