
open Runtime;
open Unicode.Types;
open Engine;
open ParseState;

(* hooks for loaded modules *)

value register_parse_state_hook : (parse_state -> unit) -> unit;
value register_init_hook        : (unit -> unit) -> unit;

(* routines to start ant *)

value initialise     : unit -> unit;
value parse_document : parse_state -> list Node.node;
value parse_file     : string    -> (list Node.node * parse_state);
value parse_string   : uc_string -> (list Node.node * parse_state);

