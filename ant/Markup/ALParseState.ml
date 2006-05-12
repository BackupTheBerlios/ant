
open XNum;
open Runtime;
open Logging;
open VM;
open Typesetting;
open Engine;
open ParseState;
open ALCoding;
open ALDim;
open ALEnvironment;
open ALNodes;
open ALGraphics;

module UString     = Unicode.UString;
module SymbolTable = Unicode.SymbolTable;
module SymbolMap   = SymbolTable.SymbolMap;

(*
  Opaque type for parse-states: A parse-command is a function of type |parse_state -> unit|.
*)

value apply_ps _ _ = Types.runtime_error "application of non-function";

value cmp_ps p1 p2 = p1 == p2;

value (ps_wrapper, ps_unwrapper) = Opaque.declare_type "parse-command" apply_ps cmp_ps cmp_ps;

value wrap_ps ps = Types.Opaque (ps_wrapper ps);

value unwrap_ps = Machine.evaluate_opaque "parse-command" ps_unwrapper;

value decode_ps = decode_opaque "parse-command" ps_unwrapper;

value execute_ps_command_unkown name f ps = do
{
  try
    let result = Machine.decode_function name f [ref (wrap_ps (fun _ -> ()))] in
    let cmd    = decode_ps name result in

    cmd ps
  with
  [ VM.Types.Syntax_error loc msg -> log_warn loc (UString.to_string (Array.to_list msg))
  | VM.Types.Runtime_error msg    -> log_warn (location ps) (UString.to_string (Array.to_list msg))
  ]
};

value execute_ps_command name stream ps = do
{
  try
    let cmd = decode_ps name
                (ref (Machine.evaluate_monad_expr ps.al_scope stream (wrap_ps (fun _ -> ()))))
              in
    cmd ps
  with
  [ VM.Types.Syntax_error loc msg -> log_warn loc (UString.to_string (Array.to_list msg))
  | VM.Types.Runtime_error msg    -> log_warn (location ps) (UString.to_string (Array.to_list msg))
  ]
};

value ps_cmd name res parse_command f = do
{
  let cmd = ref (fun _ -> ()) in

  Machine.continue2
    (fun () -> unwrap_ps name cmd parse_command)
    (fun () -> !res := wrap_ps (fun ps -> do { !cmd ps; f ps }))
};

(* primitives *)

(* stream commands *)

value ps_next_char res c parse_command = do
{
  ps_cmd "ps_next_char" res parse_command
    (fun ps -> do
      {
        Machine.set_unknown c (Types.Char (UCStream.next_char ps.input_stream))
      })
};

value ps_get_char res args = match args with
[ [c; pos; parse_command] -> do
  {
    ps_cmd "ps_get_char" res parse_command
      (fun ps -> do
        {
          let n = decode_int "ps_get_char" pos in

          Machine.set_unknown c (Types.Char (UCStream.get_char ps.input_stream n))
        })
  }
| _ -> assert False
];

value ps_remove_chars res n parse_command = do
{
  ps_cmd "ps_remove_chars" res parse_command
    (fun ps -> do
      {
        let k = decode_int "ps_remove_chars" n in

        UCStream.remove ps.input_stream k
      })
};

value ps_insert_string res str parse_command = do
{
  ps_cmd "ps_insert_string" res parse_command
    (fun ps -> do
      {
        let s  = Machine.decode_string "ps_insert_string" str  in

        UCStream.insert_list ps.input_stream s
      })
};

value ps_location res loc parse_command = do
{
  ps_cmd "ps_location" res parse_command
    (fun ps -> do
      {
        Machine.set_unknown loc (encode_location (ParseState.location ps))
      })
};

value ps_arg_expanded res arg parse_command = do
{
  ps_cmd "ps_arg_expanded" res parse_command
    (fun ps -> do
      {
        let str = ParseArgs.arg_expanded ps in

        Machine.set_unknown arg (Machine.uc_list_to_char_list str)
      })
};

value ps_arg_execute res args = match args with
[ [result; mode; parse_command] -> do
  {
    ps_cmd "ps_arg_execute" res parse_command
      (fun ps -> do
        {
          let m = decode_mode "ps_arg_execute" mode in
          let n = ParseArgs.arg_execute ps m in

          Machine.set_unknown result (encode_node_list n)
        })
  }
| _ -> assert False
];

value ps_arg_num res arg parse_command = do
{
  ps_cmd "ps_arg_num" res parse_command
    (fun ps -> do
      {
        let n  = ParseArgs.arg_num ps in

        Machine.set_unknown arg (Types.Number n)
      })
};

value ps_arg_int res arg parse_command = do
{
  ps_cmd "ps_arg_int" res parse_command
    (fun ps -> do
      {
        let n  = ParseArgs.arg_int ps in

        Machine.set_unknown arg (Types.Number (num_of_int n))
      })
};

value ps_arg_skip res arg parse_command = do
{
  ps_cmd "ps_arg_skip" res parse_command
    (fun ps -> do
      {
        let s = ParseArgs.arg_skip ps in

        Machine.set_unknown arg (encode_skip_arg s)
      })
};

value ps_arg_dim res arg parse_command = do
{
  ps_cmd "ps_arg_dim" res parse_command
    (fun ps -> do
      {
        let d = ParseArgs.arg_dim ps in

        Machine.set_unknown arg (encode_dim_arg d)
      })
};

value ps_arg_key_val res arg parse_command = do
{
  ps_cmd "ps_arg_key_val" res parse_command
    (fun ps -> do
      {
        let kv = ParseArgs.arg_key_val ps in

        let code v = match v with
                     [ None   -> ref (Types.Symbol sym_None)
                     | Some x -> ref (Machine.uc_list_to_char_list x)
                     ]
        in

        Machine.set_unknown arg
          (Types.Dictionary
            (DynUCTrie.fold
              (fun k v m -> SymbolMap.add (SymbolTable.string_to_symbol k) (code v) m)
              kv
              SymbolMap.empty))
      })
};

value ps_opt_expanded res args = match args with
[ [arg; default; parse_command] -> do
  {
    ps_cmd "ps_opt_expanded" res parse_command
      (fun ps -> do
        {
          let d  = Machine.decode_string "ps_opt_expanded" default in

          Machine.set_unknown arg (Machine.uc_list_to_char_list (ParseArgs.opt_expanded ps d))
        })
  }
| _ -> assert False
];

value ps_opt_key_val res arg parse_command = do
{
  ps_cmd "ps_opt_key_val" res parse_command
    (fun ps -> do
      {
        let kv = ParseArgs.opt_key_val ps in

        let code v = match v with
                     [ None   -> ref (Types.Symbol sym_None)
                     | Some x -> ref (Machine.uc_list_to_char_list x)
                     ]
        in

        Machine.set_unknown arg
          (Types.Dictionary
            (DynUCTrie.fold
              (fun k v m -> SymbolMap.add (SymbolTable.string_to_symbol k) (code v) m)
              kv
              SymbolMap.empty))
      })
};

value ps_opt_int res args = match args with
[ [arg; default; parse_command] -> do
  {
    ps_cmd "ps_opt_int" res parse_command
      (fun ps -> do
        {
          let d = decode_int "ps_opt_int" default in

          Machine.set_unknown arg (Types.Number (num_of_int (ParseArgs.opt_int ps d)))
        })
  }
| _ -> assert False
];

value ps_arg_TeX_dim res arg parse_command = do
{
  ps_cmd "ps_arg_TeX_dim" res parse_command
    (fun ps -> do
      {
        let d = ParseArgs.arg_TeX_dim ps in

        Machine.set_unknown arg (encode_dim_arg d)
      })
};

(* modes and node-list *)

value ps_current_mode res m parse_command = do
{
  ps_cmd "ps_current_mode" res parse_command
    (fun ps -> do
      {
        Machine.set_unknown m (encode_mode (ParseState.current_mode ps))
      })
};

value ps_open_node_list res mode parse_command = do
{
  ps_cmd "ps_open_node_list" res parse_command
    (fun ps -> do
      {
        ParseState.open_node_list ps (decode_mode "ps_open_node_list" mode)
      })
};

value ps_close_node_list res args = match args with
[ [result; mode; parse_command] -> do
  {
    ps_cmd "ps_close_node_list" res parse_command
      (fun ps -> do
        {
          let nodes = ParseState.close_node_list ps (decode_mode "ps_close_node_list" mode) in

          Machine.set_unknown result (encode_node_list nodes)
        })
  }
| _ -> assert False
];

value ps_add_node res node parse_command = do
{
  ps_cmd "ps_add_node" res parse_command
    (fun ps -> do
      {
        ParseState.add_node ps (decode_node "ps_add_node" node)
      })
};

(* commands *)

value decode_command name execute expand = do
{
  let exe ps = execute_ps_command_unkown name execute ps in
  let exp ps tok = try do
    {
      let result  = ref Types.Unbound in
      let command = Machine.decode_function
                      name
                      expand
                      [result;
                       ref ((Machine.uc_list_to_char_list tok));
                       ref (wrap_ps (fun _ -> ()))]
      in

      let cmd = decode_ps name command in

      cmd ps;

      Machine.decode_string name result
    }
    with
    [ VM.Types.Syntax_error loc msg -> do
      {
        log_warn loc (UString.to_string (Array.to_list msg));
        []
      }
    | VM.Types.Runtime_error msg -> do
      {
        log_warn (location ps) (UString.to_string (Array.to_list msg));
        []
      }
    ]
  in

  { ParseState.execute = exe; ParseState.expand = exp }
};

value decode_unexpandable_command name execute = do
{
  { ParseState.execute = execute_ps_command_unkown name execute;
    ParseState.expand  = Macro.noexpand }
};

value ps_set_default_char_cmd res args = match args with
[ [execute; expand; parse_command] -> do
  {
    ps_cmd "ps_set_default_char_cmd" res parse_command
      (fun ps -> do
        {
          Machine.evaluate expand;

          match !expand with
          [ Types.Symbol s -> do
            {
              if s = sym_None then
                ParseState.set_default_char_cmd ps
                  (decode_unexpandable_command "default_char_cmd" execute)
              else
                log_warn (location ps) "ps_set_default_char_cmd: None or a function expected"
            }
          | _ -> do
            {
              ParseState.set_default_char_cmd ps
                (decode_command "default_char_cmd" execute expand)
            }
          ]
        })
  }
| _ -> assert False
];

value ps_define_command res args = match args with
[ [name; execute; expand; parse_command] -> do
  {
    ps_cmd "ps_define_command" res parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_define_command" name in
          let name_str = UString.to_string name_uc                      in

          Machine.evaluate expand;

          match !expand with
          [ Types.Symbol s -> do
            {
              if s = sym_None then
                ParseState.define_command ps name_uc
                  (decode_unexpandable_command name_str execute)
              else
                log_warn (location ps) "ps_define_command: None or a function expected"
            }
          | _ -> do
            {
              ParseState.define_command ps name_uc
                (decode_command name_str execute expand)
            }
          ]
        })
  }
| _ -> assert False
];

value ps_define_pattern res args = match args with
[ [name; execute; expand; parse_command] -> do
  {
    ps_cmd "ps_define_pattern" res parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_define_pattern" name in
          let name_str = UString.to_string name_uc                      in

          Machine.evaluate expand;

          match !expand with
          [ Types.Symbol s -> do
            {
              if s = sym_None then
                ParseState.define_pattern ps name_uc
                  (decode_unexpandable_command name_str execute)
              else
                log_warn (location ps) "ps_define_pattern: None or a function expected"
            }
          | _ -> do
            {
              ParseState.define_pattern ps name_uc
                (decode_command name_str execute expand)
            }
          ]
        })
  }
| _ -> assert False
];

value ps_save_command res name parse_command = do
{
  ps_cmd "ps_save_command" res parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_save_command" name in

        ParseState.save_command ps n
      })
};

value ps_restore_command res name parse_command = do
{
  ps_cmd "ps_restore_command" res parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_restore_command" name in

        ParseState.restore_command ps n
      })
};

value ps_save_pattern res name parse_command = do
{
  ps_cmd "ps_save_pattern" res parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_save_pattern" name in

        ParseState.save_pattern ps n
      })
};

value ps_restore_pattern res name parse_command = do
{
  ps_cmd "ps_restore_pattern" res parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_restore_pattern" name in

        ParseState.restore_pattern ps n
      })
};

value encode_command name command = do
{
  let execute res parse_command = do
    {
      let cmd = decode_ps name parse_command in

      !res := wrap_ps (fun ps -> do { cmd ps; command.execute ps })
    }
  in
  let expand res args = match args with
  [ [result; tok; parse_command] -> do
    {
      let cmd = decode_ps name parse_command in

      !res :=
        wrap_ps
          (fun ps -> do
            {
              cmd ps;

              let t = Machine.decode_string name tok in

              Machine.set_unknown result (Machine.uc_list_to_char_list (command.expand ps t))
            })
    }
  | _ -> assert False
  ]
  in

  Types.Tuple
    [|ref (Types.Primitive1 execute);
      ref (Types.PrimitiveN 3 expand)|]
};

value ps_lookup_command res args = match args with
[ [command; name; parse_command] -> do
  {
    ps_cmd "ps_lookup_command" res parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_lookup_command" name in
          let name_str = UString.to_string name_uc in

          match ParseState.lookup_command ps name_uc with
          [ None   -> Machine.set_unknown command (Types.Symbol sym_None)
          | Some c -> Machine.set_unknown command (encode_command name_str c)
          ]
        })
  }
| _ -> assert False
];

value ps_push_env res args = match args with
[ [name; arg; parse_command] -> do
  {
    ps_cmd "ps_push_env" res parse_command
      (fun ps -> do
        {
          let n    = Machine.decode_string "ps_push_env" name in
          let args = List.map
                       (Machine.decode_string "ps_push_env")
                       (Machine.decode_list "ps_push_env" arg)
                     in

          ParseState.push_env ps n args
        })
  }
| _ -> assert False
];

value ps_pop_env res args = match args with
[ [name; arg; parse_command] -> do
  {
    ps_cmd "ps_pop_env" res parse_command
      (fun ps -> do
        {
          let (n,args) = ParseState.pop_env ps in

          Machine.set_unknown name (Machine.uc_list_to_char_list n);
          Machine.set_unknown arg
            (List.fold_right
              (fun a l -> Types.List
                            (ref (Machine.uc_list_to_char_list a))
                            (ref l))
              args
              Types.Nil)
        })
  }
| _ -> assert False
];

value ps_set_env_args res args parse_command = do
{
  ps_cmd "ps_set_env_args" res parse_command
    (fun ps -> do
      {
        let a = List.map
                  (Machine.decode_string "ps_set_env_args")
                  (Machine.decode_list "ps_set_env_args" args)
                in

        ParseState.set_env_args ps a
      })
};

value ps_top_env res args = match args with
[ [name; arg; parse_command] -> do
  {
    ps_cmd "ps_top_env" res parse_command
      (fun ps -> do
        {
          let (n,args) = ParseState.top_env ps in

          Machine.set_unknown name (Machine.uc_list_to_char_list n);
          Machine.set_unknown arg
            (List.fold_right
              (fun a l -> Types.List
                            (ref (Machine.uc_list_to_char_list a))
                            (ref l))
              args
              Types.Nil)
        })
  }
| _ -> assert False
];

value ps_lookup_env res args = match args with
[ [commands; name; parse_command] -> do
  {
    ps_cmd "ps_lookup_env" res parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_lookup_env" name in
          let name_str = UString.to_string name_uc in

          match ParseState.lookup_env ps name_uc with
          [ None       -> Machine.set_unknown commands (Types.Symbol sym_None)
          | Some (b,e) -> Machine.set_unknown commands
                            (Types.Tuple [|ref (encode_command name_str b);
                                           ref (encode_command name_str e)|])
          ]
        })
  }
| _ -> assert False
];

value ps_define_env res args = match args with
[ [name; execute_begin; expand_begin; execute_end; expand_end; parse_command] -> do
  {
    ps_cmd "ps_define_env" res parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_define_env" name in
          let name_str = UString.to_string name_uc                  in

          Machine.evaluate expand_begin;
          Machine.evaluate expand_end;

          match (!expand_begin, !expand_end) with
          [ (Types.Symbol s1, Types.Symbol s2) -> do
            {
              if s1 = sym_None && s2 = sym_None then
                ParseState.define_env ps name_uc
                  (decode_unexpandable_command name_str execute_begin)
                  (decode_unexpandable_command name_str execute_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            }
          | (Types.Symbol s, _) -> do
            {
              if s = sym_None then
                ParseState.define_env ps name_uc
                  (decode_unexpandable_command name_str execute_begin)
                  (decode_command              name_str execute_end expand_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            }
          | (_, Types.Symbol s) -> do
            {
              if s = sym_None then
                ParseState.define_env ps name_uc
                  (decode_command              name_str execute_begin expand_begin)
                  (decode_unexpandable_command name_str execute_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            }
          | _ -> do
            {
              ParseState.define_env ps name_uc
                (decode_command name_str execute_begin expand_begin)
                (decode_command name_str execute_end   expand_end)
            }
          ]
        })
  }
| _ -> assert False
];

(* page layout *)

value ps_shipout_pages res args = match args with
[ [number; even; odd; parse_command] -> do
  {
    ps_cmd "ps_shipout_pages" res parse_command
      (fun ps -> do
        {
          let n        = decode_int "ps_shipout_pages" number in
          let even_str = Array.of_list (Machine.decode_string "ps_shipout_pages" even) in
          let odd_str  = Array.of_list (Machine.decode_string "ps_shipout_pages" odd)  in

          ParseState.add_node ps (Node.ShipOut (ParseState.location ps) even_str odd_str (max 0 n))
        })
  }
| _ -> assert False
];

value ps_new_page_layout res args = match args with
[ [name; width; height; parse_command] -> do
  {
    ps_cmd "ps_new_page_layout" res parse_command
      (fun ps -> do
        {
          let name_str = Array.of_list (Machine.decode_string "ps_new_page_layout" name) in
          let w        = Machine.decode_num "ps_new_page_layout" width  in
          let h        = Machine.decode_num "ps_new_page_layout" height in

          ParseState.add_node ps
            (Node.NewLayout (ParseState.location ps)
               name_str
               (fun _ -> w)
               (fun _ -> h))
        })
  }
| _ -> assert False
];

value encode_page_info pi = do
{
  Types.Dictionary
    (SymbolMap.add
      sym_Width    (ref (Types.Number pi.Box.pi_width))
    (SymbolMap.add
      sym_Height   (ref (Types.Number pi.Box.pi_height))
    (SymbolMap.add
      sym_PageNo   (ref (Types.Number (num_of_int pi.Box.pi_page_no)))
    (SymbolMap.add
      sym_OldMarks (ref (List.fold_right
                          (fun (a,b) c ->
                              Types.List
                                (ref (Types.Tuple
                                       [|ref (Machine.uc_string_to_char_list a);
                                         ref (Machine.uc_string_to_char_list b)|]))
                                (ref c))
                          pi.Box.pi_old_marks
                          Types.Nil))
    (SymbolMap.add
      sym_NewMarks (ref (List.fold_right
                          (fun (a,b) c ->
                              Types.List
                                (ref (Types.Tuple
                                       [|ref (Machine.uc_string_to_char_list a);
                                         ref (Machine.uc_string_to_char_list b)|]))
                                (ref c))
                          pi.Box.pi_new_marks
                          Types.Nil))
    SymbolMap.empty)))))
};

value ps_new_area res args = match args with
[ [name; pos_x; pos_y; width; height; max_top; max_bot; area_type; param; parse_command] -> do
  {
    ps_cmd "ps_new_area" res parse_command
      (fun ps -> do
      {
        let name_str = Array.of_list (Machine.decode_string "ps_new_area" name) in
        let x        = Machine.decode_num "ps_new_area" pos_x                   in
        let y        = Machine.decode_num "ps_new_area" pos_y                   in
        let w        = Machine.decode_num "ps_new_area" width                   in
        let h        = Machine.decode_num "ps_new_area" height                  in
        let t        = Machine.decode_num "ps_new_area" max_top                 in
        let b        = Machine.decode_num "ps_new_area" max_bot                 in
        let at       = decode_symbol      "ps_new_area" area_type               in

        if at = sym_Galley then do
        {
          let ap = decode_dict "ps_new_area" param in

          add_node ps
            (Node.NewArea (ParseState.location ps)
               name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
               (`Galley
                 (Option.from_option [||]
                    (lookup_string  "ps_new_area" ap sym_Name),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_em (num_of_int 5))
                     (lookup_skip   "ps_new_area" ap sym_MinSize),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_GridSize))))
        }
        else if at = sym_Float then do
        {
          let ap    = decode_dict "ps_new_area" param in
          let align = match lookup_symbol "ps_new_area" ap sym_Alignment with
            [ None   -> FloatVertical.Top
            | Some s -> if s = sym_Bottom then
                          FloatVertical.Bottom
                        else
                          FloatVertical.Top
            ]
          in

          add_node ps
            (Node.NewArea (ParseState.location ps)
               name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
               (`Float
                 (align,
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip    "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip    "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_fixed_dim (Evaluate.const_em num_one))
                    (lookup_dim     "ps_new_area" ap sym_FloatSep))))
        }
        else if at = sym_Footnote then do
        {
          let lookup_dict dict key = try
            SymbolMap.find key dict
          with
          [ Not_found -> ref (Types.Dictionary SymbolMap.empty) ]
          in

          let ap                = decode_dict "ps_new_area" param    in
          let line_params       = lookup_dict ap sym_LineParams      in
          let par_params        = lookup_dict ap sym_ParParams       in
          let line_break_params = lookup_dict ap sym_LineBreakParams in
          let hyphen_params     = lookup_dict ap sym_HyphenParams    in
          let space_params      = lookup_dict ap sym_SpaceParams     in
          let math_params       = lookup_dict ap sym_MathParams      in

          add_node ps
            (Node.NewArea (ParseState.location ps)
               name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
               (`Footnote
                 (Option.from_option []
                    (lookup (decode_node_list "ps_new_area") ap sym_Separator),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_GridSize),
                  decode_line_params "ps_new_area" line_params,
                  decode_par_params "ps_new_area" par_params,
                  decode_line_break_params "ps_new_area" line_break_params,
                  decode_hyphen_params "ps_new_area" hyphen_params,
                  decode_space_params "ps_new_area" space_params,
                  decode_math_params "ps_new_area" math_params)))
        }
        else if at = sym_Direct then do
        {
          Machine.evaluate param;

          match !param with
          [ Types.Nil
          | Types.List _ _ -> do
            {
              (* <param> is a string with ant code. *)

              let code       = Machine.decode_string "ps_new_area" param in
              let current_ps = ParseState.duplicate ps in
              let stream     = UCStream.of_list code   in

              let f pi _ = do
              {
                UCStream.assign current_ps.input_stream stream;

                ParseState.set_counter current_ps (UString.uc_string_of_ascii "page") pi.Box.pi_page_no;

                List.iter
                  (fun (m,v) -> do
                    {
                      ParseState.add_reference current_ps
                        (UString.of_ascii "old mark: " @ Array.to_list m)
                        v;
                      ParseState.add_reference current_ps
                        (UString.of_ascii "new mark: " @ Array.to_list m)
                        v
                    })
                  (List.rev pi.Box.pi_old_marks);
                List.iter
                  (fun (m,v) ->
                      ParseState.add_reference current_ps
                        (UString.of_ascii "new mark: " @ Array.to_list m)
                        v
                  )
                  (List.rev pi.Box.pi_new_marks);

                ParseState.run_parser current_ps `VBox;
              }
              in

              add_node ps
                (Node.NewArea (ParseState.location ps)
                   name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
                   (`Direct f))
            }
          | _ -> do
            {
              (* <param> is a function that returns a node list. *)

              let f pi (x,y) = do
              {
                decode_node_list
                  "<anonymous>"
                  (Machine.decode_function
                    "<anonymous>"
                    param
                    [ref (encode_page_info pi);
                     ref (Types.Tuple [|ref (Types.Number x); ref (Types.Number y)|])])
              }
              in

              add_node ps
                (Node.NewArea (ParseState.location ps)
                   name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
                   (`Direct f))
            }
          ]
        }
        else do
        {
          log_warn (ParseState.location ps) "unkown area type ";
          log_uc_string (SymbolTable.symbol_to_string at);
          log_string "!\n"
        }
        })
  }
| _ -> assert False
];

value ps_new_galley res args = match args with
[ [name; measure; parse_command] -> do
  {
    ps_cmd "ps_new_galley" res parse_command
      (fun ps -> do
        {
          let name_str = Array.of_list (Machine.decode_string "ps_new_galley" name) in
          let m        = Machine.decode_num "ps_new_galley" measure                 in

          ParseState.add_node ps
            (Node.NewGalley (ParseState.location ps) name_str (fun _ -> m))
        })
  }
| _ -> assert False
];

(* fonts *)

value ps_declare_font res args = match args with
[ [name; family; series; shape; sizes; params; parse_command] -> do
  {
    ps_cmd "ps_declare_font" res parse_command
      (fun ps -> do
        {
          let n       = decode_uc_string "ps_declare_font" name     in
          let fam     = decode_uc_string "ps_declare_font" family   in
          let ser     = decode_uc_string "ps_declare_font" series   in
          let sha     = decode_uc_string "ps_declare_font" shape    in
          let p       = decode_dict      "ps_declare_font" params   in
          let (s1,s2) = match decode_tuple "ps_declare_font" sizes with
            [ [| s1; s2 |] -> (s1,s2)
            | _ -> Types.runtime_error "ps_declare_font: pair expected"
            ]
          in

          let get_glyph g = if g < 0 then
                              Substitute.Undef
                            else
                              Substitute.Simple g
                            in

          let encoding      = Array.map
                                (decode_uc_string "ps_declare_font")
                                (Option.from_option [||]
                                  (lookup_tuple "ps_declare_font" p sym_Encoding))
                              in
          let hyphen        = Option.from_option (-1)
                               (lookup_int "ps_declare_font" p sym_HyphenGlyph)   in
          let skew          = Option.from_option (-1)
                               (lookup_int "ps_declare_font" p sym_SkewGlyph)     in
          let scale         = Option.from_option num_one
                               (lookup_num "ps_declare_font" p sym_Scale)         in
          let letterspacing = Option.from_option num_zero
                               (lookup_num "ps_declare_font" p sym_LetterSpacing) in

          add_node ps
            (Node.Command (location ps)
              (Environment.declare_font
                n fam ser sha
                (Machine.decode_num "ps_declare_font" s1,
                 Machine.decode_num "ps_declare_font" s2)
                {
                  (FontMetric.empty_load_params)

                  with

                  FontMetric.flp_encoding       = encoding;
                  FontMetric.flp_letter_spacing = letterspacing;
                  FontMetric.flp_size           = scale;
                  FontMetric.flp_hyphen_glyph   = get_glyph hyphen;
                  FontMetric.flp_skew_glyph     = get_glyph skew
                }))
        })
  }
| _ -> assert False
];

value ps_define_math_symbol res args = match args with
[ [name; math_code; font; glyph; parse_command] -> do
  {
    ps_cmd "ps_define_math_symbol" res parse_command
      (fun ps -> do
        {
          let name = Machine.decode_string "ps_define_math_symbol" name      in
          let mc   = decode_math_code      "ps_define_math_symbol" math_code in
          let f    = decode_int            "ps_define_math_symbol" font      in
          let g    = decode_int            "ps_define_math_symbol" glyph     in

          ParseState.define_command ps name
            { ParseState.execute = (fun ps -> ParseState.add_node ps
                                                (Node.MathChar (location ps) (mc, (f, f), (g, g))));
              ParseState.expand  = Macro.noexpand }
        })
  }
| _ -> assert False
];

value ps_define_root_symbol res args = match args with
[ [name; small_font; small_glyph; large_font; large_glyph; parse_command] -> do
  {
    ps_cmd "ps_define_root_symbol" res parse_command
      (fun ps -> do
        {
          let name = Machine.decode_string "ps_define_root_symbol" name        in
          let sf   = decode_int            "ps_define_root_symbol" small_font  in
          let sg   = decode_int            "ps_define_root_symbol" small_glyph in
          let lf   = decode_int            "ps_define_root_symbol" large_font  in
          let lg   = decode_int            "ps_define_root_symbol" large_glyph in

          ParseState.define_command ps name
            { ParseState.execute = (fun ps -> ParseState.add_node ps
                                                (Node.Root (location ps) sf sg lf lg (ParseArgs.arg_execute ps `Math)));
              ParseState.expand  = Macro.noexpand }
        })
  }
| _ -> assert False
];

value ps_define_math_accent res args = match args with
[ [name; font; glyph; parse_command] -> do
  {
    ps_cmd "ps_define_math_accent" res parse_command
      (fun ps -> do
        {
          let name = Machine.decode_string "ps_define_math_accent" name  in
          let f    = decode_int            "ps_define_math_accent" font  in
          let g    = decode_int            "ps_define_math_accent" glyph in

          ParseState.define_command ps name
            { ParseState.execute = (fun ps -> ParseState.add_node ps
                                                (Node.MathAccent (location ps) f g (ParseArgs.arg_execute ps `Math)));
              ParseState.expand  = Macro.noexpand }
        })
  }
| _ -> assert False
];

value ps_set_math_code res args = match args with
[ [char; math_code; font1; glyph1; font2; glyph2; parse_command] -> do
  {
    ps_cmd "ps_set_math_code" res parse_command
      (fun ps -> do
        {
          let c  = decode_char      "ps_set_math_code" char      in
          let mc = decode_math_code "ps_set_math_code" math_code in
          let f1 = decode_int       "ps_set_math_code" font1     in
          let g1 = decode_int       "ps_set_math_code" glyph1    in
          let f2 = decode_int       "ps_set_math_code" font2     in
          let g2 = decode_int       "ps_set_math_code" glyph2    in

          ParseState.set_math_code ps c mc f1 g1 f2 g2
        })
  }
| _ -> assert False
];

(* graphics *)

value decode_coord name z = do
{
  Machine.evaluate z;

  match !z with
  [ Types.Tuple [|x;y|] -> (Machine.decode_num name x,
                            Machine.decode_num name y)
  | _                   -> Types.runtime_error (name ^ ": pair expected but got " ^ Types.type_name !z)
  ]
};

value decode_bezier name z = do
{
  Machine.evaluate z;

  match !z with
  [ Types.Tuple [|a;b;c;d|] -> do
    {
      let (ax,ay) = decode_coord name a in
      let (bx,by) = decode_coord name b in
      let (cx,cy) = decode_coord name c in
      let (dx,dy) = decode_coord name d in

      (fun _ -> Dim.fixed_dim ax, fun _ -> Dim.fixed_dim ay,
       fun _ -> Dim.fixed_dim bx, fun _ -> Dim.fixed_dim by,
       fun _ -> Dim.fixed_dim cx, fun _ -> Dim.fixed_dim cy,
       fun _ -> Dim.fixed_dim dx, fun _ -> Dim.fixed_dim dy)
    }
  | _ -> Types.runtime_error (name ^ ": 4-tuple expected but got " ^ Types.type_name !z)
  ]
};

value decode_path name p = do
{
  List.map (decode_bezier name)
    (Machine.decode_list name p)
};

value ps_set_colour res colour parse_command = do
{
  ps_cmd "ps_set_colour" res parse_command
    (fun ps -> do
      {
        let c = decode_colour "ps_set_colour" colour in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetColour c))
      })
};

value ps_set_bg_colour res colour parse_command = do
{
  ps_cmd "ps_set_bg_colour" res parse_command
    (fun ps -> do
      {
        let c = decode_colour "ps_set_bg_colour" colour in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetBgColour c))
      })
};

value ps_set_alpha res alpha parse_command = do
{
  ps_cmd "ps_set_alpha" res parse_command
    (fun ps -> do
      {
        let a = Machine.decode_num "ps_set_alpha" alpha in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetAlpha a))
      })
};

value ps_draw name mode res path parse_command = do
{
  ps_cmd "ps_draw" res parse_command
    (fun ps -> do
      {
        let p = decode_path name path in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.Draw mode p))
      })
};

value ps_set_line_width res width parse_command = do
{
  ps_cmd "ps_set_line_width" res parse_command
    (fun ps -> do
      {
        let w = Machine.decode_num "ps_set_line_width" width in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetLineWidth w))
      })
};

value ps_set_line_cap res cap parse_command = do
{
  ps_cmd "ps_set_line_cap" res parse_command
    (fun ps -> do
      {
        let c = decode_line_cap "ps_set_line_cap" cap in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetLineCap c))
      })
};

value ps_set_line_join res join parse_command = do
{
  ps_cmd "ps_set_line_join" res parse_command
    (fun ps -> do
      {
        let j = decode_line_join "ps_set_line_join" join in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetLineJoin j))
      })
};

value ps_set_miter_limit res limit parse_command = do
{
  ps_cmd "ps_set_miter_limit" res parse_command
    (fun ps -> do
      {
        let l = Machine.decode_num "ps_set_miter_limit" limit in

        ParseState.add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetMiterLimit l))
      })
};

(* counters *)

value ps_new_counter res args = match args with
[ [name; val; super; parse_command] -> do
  {
    ps_cmd "ps_new_counter" res parse_command
      (fun ps -> do
        {
          let n = decode_uc_string "ps_new_counter" name in
          let v = decode_int       "ps_new_counter" val  in
          let s = decode_option    "ps_new_counter" decode_uc_string super in

          ParseState.new_counter ps n v s
        })
  }
| _ -> assert False
];

value ps_get_counter res args = match args with
[ [val; name; parse_command] -> do
  {
    ps_cmd "ps_get_counter" res parse_command
      (fun ps -> do
        {
          let n = decode_uc_string "ps_get_counter" name in

          Machine.set_unknown val (Types.Number (num_of_int (ParseState.get_counter ps n)))
        })
  }
| _ -> assert False
];

value ps_set_counter res args = match args with
[ [name; val; parse_command] -> do
  {
    ps_cmd "ps_set_counter" res parse_command
      (fun ps -> do
        {
          let n = decode_uc_string "ps_set_counter" name in
          let v = decode_int       "ps_set_counter" val  in

          ParseState.set_counter ps n v
        })
  }
| _ -> assert False
];

(* references *)

value ps_add_reference res args = match args with
[ [name; val; parse_command] -> do
  {
    ps_cmd "ps_add_reference" res parse_command
      (fun ps -> do
        {
          let n = Machine.decode_string "ps_add_reference" name in
          let v = decode_uc_string      "ps_add_reference" val  in

          ParseState.add_reference ps n v
        })
  }
| _ -> assert False
];

value ps_reference_exists res args = match args with
[ [ret; name; parse_command] -> do
  {
    ps_cmd "ps_reference_exists" res parse_command
      (fun ps -> do
        {
          let n = Machine.decode_string "ps_reference_exists" name in

          Machine.set_unknown ret (Types.Bool (ParseState.reference_exists ps n))
        })
  }
| _ -> assert False
];

value ps_lookup_reference res args = match args with
[ [ret; name; parse_command] -> do
  {
    ps_cmd "ps_lookup_reference" res parse_command
      (fun ps -> do
        {
          let n = Machine.decode_string "ps_lookup_reference" name in

          Machine.set_unknown ret (Machine.uc_string_to_char_list (ParseState.lookup_reference ps n))
        })
  }
| _ -> assert False
];

value ps_fold_references res args = match args with
[ [ret; f; e; parse_command] -> do
  {
    ps_cmd "ps_fold_references" res parse_command
      (fun ps -> do
        {
          let accum = ref (Types.TGlobal e) in

          ParseState.iter_references ps fold_fun

          where rec fold_fun name val = do
          {
            let new_val = Types.TApplication
                            (Types.TGlobal f)
                            [!accum;
                             Types.TConstant (Machine.uc_string_to_char_list name);
                             Types.TConstant (Machine.uc_string_to_char_list val)]
            in

            !accum := new_val
          };

          Machine.set_unknown ret (Types.UnevalT [] !accum)
        })
  }
| _ -> assert False
];

value ps_store_old_references res parse_command = do
{
  ps_cmd "ps_store_old_references" res parse_command
    (fun ps -> do
      {
        ParseState.store_old_references ps
      })
};

value ps_compare_references res ret parse_command = do
{
  ps_cmd "ps_compare_references" res parse_command
    (fun ps -> do
      {
        Machine.set_unknown ret (Types.Bool (ParseState.compare_references ps))
      })
};

value ps_write_references res file parse_command = do
{
  ps_cmd "ps_write_references" res parse_command
    (fun ps -> do
      {
        let f  = Machine.decode_string "ps_write_references" file in

        ParseState.write_references ps (UString.to_string f)
      })
};

(* running the parser *)

value ps_execute_next_char res finished parse_command = do
{
  ps_cmd "ps_execute_next_char" res parse_command
    (fun ps -> do
      {
        Machine.set_unknown finished (Types.Bool (ParseState.execute_next_char ps))
      })
};

value ps_execute_stream res string parse_command = do
{
  ps_cmd "ps_execute_stream" res parse_command
    (fun ps -> do
      {
        let str = Machine.decode_string "ps_execute_stream" string in

        ParseState.execute_stream ps (UCStream.of_list str)
      })
};

value ps_execute_argument res parse_command = do
{
  ps_cmd "ps_execute_argument" res parse_command
    (fun ps -> do
      {
        ParseState.execute_argument ps
      })
};

value ps_run_parser res args = match args with
[ [result; mode; parse_command] -> do
  {
    ps_cmd "ps_run_parser" res parse_command
      (fun ps -> do
        {
          let m = decode_mode "ps_run_parser" mode in

          Machine.set_unknown result (encode_node_list (ParseState.run_parser ps m))
        })
  }
| _ -> assert False
];

