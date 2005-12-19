
open Runtime;
open Logging;
open Engine;
open ParseState;

(* |begin_paragraph| starts a new paragraph, |end_paragraph| adds the current paragraph to the page. *)

value begin_paragraph ps = do
{
  open_node_list ps `Paragraph
};

value end_paragraph ps = do
{
  match close_node_list ps `Paragraph with
  [ []    -> ()
  | nodes -> add_node ps (Node.Paragraph (location ps) nodes)
  ]
};

(* |begin_math| starts a math formula, |end_math| adds it to the current paragraph. *)

value begin_math ps = do
{
  open_node_list ps `Math
};

value end_math ps = do
{
  match close_node_list ps `Math with
  [ []    -> ()
  | nodes -> add_node ps (Node.Math (location ps) nodes)
  ]
};

value begin_hbox ps = do
{
  open_node_list ps `HBox
};

value end_hbox ps = do
{
  match close_node_list ps `HBox with
  [ []    -> ()
  | nodes -> add_node ps (Node.HBox (location ps) nodes)
  ]
};

(* |ensure_par_mode| enters paragraph mode if the system is in document mode. *)

value ensure_par_mode ps = match current_mode ps with
[ `Galley                    -> begin_paragraph ps
| `Paragraph | `Math | `HBox -> ()
| `VBox                      -> begin_hbox ps
| m                          -> log_error (location ps)
                                  ("You can't start a paragraph in "
                                 ^ ParseState.mode_to_string m
                                 ^ " mode!")
];

value leave_par_mode ps = match current_mode ps with
[ `Preamble
| `Galley
| `VBox      -> ()
| `Paragraph -> end_paragraph ps
| `Math      -> do
                {
                  end_math ps;
                  end_paragraph ps
                }
| `HBox      -> end_hbox ps
| m          -> log_error (location ps)
                  ("Mode "
                 ^ ParseState.mode_to_string m
                 ^ " at end of paragraph!")
];

(* |set_mode <mode>| sets the current mode and returns |True| on success. *)

value set_mode ps mode = do
{
  let set_mode_par ps = match current_mode ps with
  [ `Galley    -> do
                  {
                    begin_paragraph ps;
                    True
                  }
  | `Paragraph -> True
  | `Math      -> do
                  {
                    end_math ps;
                    True
                  }
  | _          -> False
  ]
  in

  let set_mode_math ps = match current_mode ps with
  [ `Galley    -> do
                  {
                    begin_paragraph ps;
                    begin_math ps;
                    True
                  }
  | `Paragraph -> do
                  {
                    begin_math ps;
                    True
                  }
  | `Math      -> True
  | `HBox      -> do
                  {
                    begin_math ps;
                    True
                  }
  | _          -> False
  ]
  in

  match mode with
  [ `Galley    -> do
                  {
                    leave_par_mode ps;
                    True
                  }
  | `Paragraph -> set_mode_par ps
  | `Math      -> set_mode_math ps
  | _          -> False
  ]
};

