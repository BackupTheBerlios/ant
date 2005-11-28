
open Runtime;
open Unicode.Types;
open Engine;
open ParseState;

(* hooks for loaded modules *)

value register_parse_state_hook : (parse_state -> unit) -> unit;
value register_init_hook        : (unit -> unit) -> unit;

(* routines to start ant *)

value initialise     : unit -> unit;
value parse_document : parse_state -> list Evaluate.node_type;
value parse_file     : string    -> (list Evaluate.node_type * parse_state);
value parse_string   : uc_string -> (list Evaluate.node_type * parse_state);

