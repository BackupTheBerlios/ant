
(* stack of continuations *)

value old_stacks    = ref [];
value current_stack = ref [];

value is_empty () = match !current_stack with
[ [] -> True
| _  -> False
];

value push cont = do
{
  !current_stack := [cont :: !current_stack]
};

value pop () = match !current_stack with
[ [c::cs] -> do
  {
    !current_stack := cs;
    c
  }
| [] -> fun () -> ()
];

value process_stack () = do
{
  while not (is_empty ()) do
  {
    (pop ()) ()
  }
};

value save_stack () = do
{
  !old_stacks    := [!current_stack :: !old_stacks];
  !current_stack := []
};

value restore_stack () = match !old_stacks with
[ [] -> do
  {
    !current_stack := []
  }
| [s::ss] -> do
  {
    !current_stack := s;
    !old_stacks    := ss
  }
];

(* public interface *)

value start_vm () = do
{
  save_stack ()
};
value end_vm () = do
{
  process_stack ();
  restore_stack ()
};

value cont = push;

value cont2 c1 c2 = do
{
  (* push c1 and c2 in reverse order *)
  push c2;
  push c1
};

value cont3 c1 c2 c3 = do
{
  (* push c1, c2, c3 in reverse order *)
  push c3;
  push c2;
  push c1
};

value cont4 c1 c2 c3 c4 = do
{
  (* push c1, c2, c3, c4 in reverse order *)
  push c4;
  push c3;
  push c2;
  push c1
};

