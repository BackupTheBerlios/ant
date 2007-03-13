
open XNum;
open Runtime;
open Logging;
open Dim;
open Typesetting;
open Box;

value output_pages format pages = match pages with
[ [] -> ()
| _  -> do
  {
    let comment = " ant output "
                ^ string_of_int (Job.time.Unix.tm_year + 1900) ^ "."
                ^ string_of_int (Job.time.Unix.tm_mon + 1) ^ "."
                ^ string_of_int Job.time.Unix.tm_mday ^ ":"
                ^ string_of_int Job.time.Unix.tm_hour
                ^ string_of_int Job.time.Unix.tm_min
    in

    match format with
    [ Job.DVI  -> GenerateDVI.write_dvi_file               !Job.output_file comment pages
    | Job.XDVI -> GenerateDVI.write_xdvi_file              !Job.output_file comment pages
    | Job.PDF  -> GeneratePDF.write_pdf_file               !Job.output_file comment pages
    | Job.PS   -> GeneratePostScript.write_postscript_file !Job.output_file comment pages
    | Job.SVG  -> GenerateSVG.write_svg_file               !Job.output_file comment pages
    ]
  }
];

