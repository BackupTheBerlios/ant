
type output_format = [ DVI | PDF | PS | SVG ];

value time             : Unix.tm;
value argv             : array string;
value output_format    : ref output_format;
value source_specials  : ref bool;
value jobname          : ref string;
value input_file       : ref string;
value output_file      : ref string;
value src_special_file : ref string;
value log_file         : ref string;

value src_special_stream : ref IO.ostream;

value start_job : string -> unit;

