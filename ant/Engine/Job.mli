
type output_format = [ DVI | PDF | PS | SVG ];

value time          : Unix.tm;
value argv          : array string;
value jobname       : ref string;
value input_file    : ref string;
value output_file   : ref string;
value log_file      : ref string;
value output_format : ref output_format;

value start_job : string -> unit;

