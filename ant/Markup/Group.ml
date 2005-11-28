
open ParseState;

(* |begin_group| starts a new group. |end_group| ends the current group. *)

value begin_group ps = do
{
  add_node ps (`BeginGroup (location ps))
(*  open_node_list ps (current_mode ps); *)
};

value end_group ps = do
{
  add_node ps (`EndGroup (location ps))
(*  let nodes = close_node_list ps (current_mode ps) in

  add_node ps (`Group nodes)
*)
};

