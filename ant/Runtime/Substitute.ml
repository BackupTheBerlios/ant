
open XNum;
open Unicode.Types;
open Logging;
open Dim;

(* glyph descriptions *)

type border_glyph =
[ Space   (* for kerning with spaces                *)
| Margin  (* for margin kerning                     *)
| Foreign (* for kerning with glyphs of other fonts *)
];

type glyph_desc =
[ Undef
| Simple     of int
| Accent     of int and int
| Sequence   of list int
| Extendable of glyph_desc and glyph_desc and glyph_desc and glyph_desc
| Border     of border_glyph
];

module OrderedGlyphDesc =
struct

  type t = glyph_desc;

  value compare = (compare : glyph_desc -> glyph_desc -> int);

end;

module GlyphMap  = Map.Make(OrderedGlyphDesc);
module GlyphSet  = Set.Make(OrderedGlyphDesc);
module GlyphTrie = DynamicTrie.Make(OrderedGlyphDesc);

type glyph_item 'font 'box 'cmd =
[= `Glyph of (glyph_desc * 'font)
|  `Kern of (num * num)
|  `Box of 'box
|  `Command of 'cmd
|  `Break of (num * bool
                  * array (glyph_item 'font 'box 'cmd)
                  * array (glyph_item 'font 'box 'cmd)
                  * array (glyph_item 'font 'box 'cmd))
];

type replacement_cmd =
[ ConstGlyph of glyph_desc
| ConstKern of num and num
| CopyGlyph of int
| CopyCommands of int and int
];

type replacement = (list replacement_cmd * int);

type glyph_composer 'font 'box 'cmd =
  list (glyph_item 'font 'box 'cmd) ->
  list (glyph_item 'font 'box 'cmd);

type substitution 'font 'box 'cmd =
  list (glyph_item 'font 'box 'cmd) ->
  option (list (glyph_item 'font 'box 'cmd) * list (glyph_item 'font 'box 'cmd) * replacement);

(* auxilliary functions *)

(* |first_real_item <items> <from> <to>| and |last_real_item <items> <from> <to>|
   return the the first/last |`Glyph| or |`Box| between <from> and <to> in <items>.
   |first_real_item_list <items>| is the list version of |first_real_item|. *)

value rec first_real_item_list items = match items with
[ []      -> None
| [i::is] -> match i with
    [ `Glyph _
    | `Box _   -> Some i
    | _        -> first_real_item_list is
    ]
];

value first_real_item items from_pos to_pos = do
{
  iter from_pos

  where rec iter i = do
  {
    if i > to_pos then
      None
    else match items.(i) with
    [ `Glyph _
    | `Box _   -> Some items.(i)
    | _        -> iter (i+1)
    ]
  }
};

value last_real_item items from_pos to_pos = do
{
  iter to_pos

  where rec iter i = do
  {
    if i < from_pos then
      None
    else match items.(i) with
    [ `Glyph _
    | `Box _   -> Some items.(i)
    | _        -> iter (i-1)
    ]
  }
};

(* ligatures and substitutions *)

(*
type pattern =
[ Const of glyph_desc
| Class of GlyphSet.t
];

value match_pattern pattern glyph = match pattern with
[ Const g -> g = glyph
| Class c -> GlyphSet.mem glyph c
];
*)

value match_substitution_trie (is_empty, prefix_trie, root_value) subst_trie items = do
{
  let return_match found = match found with
  [ (_,      _,    None)      -> None
  | (prefix, rest, Some repl) -> Some (prefix, rest, repl)
  ]
  in

  iter ([], items, None) [] items subst_trie

  where rec iter found prefix items trie = match items with
  [ []                          -> return_match found
  | [(`Glyph (g,_) as i) :: is] -> do
    {
      if is_empty trie then
        return_match found
      else match g with
      [ Simple x -> do
        {
          let new_prefix = [i :: prefix]      in
          let next       = prefix_trie trie x in

          match root_value next with
          [ Some x as val -> iter (new_prefix, is, val) new_prefix is next
          | None          -> iter found                 new_prefix is next
          ]
        }
      | _ -> return_match found
      ]
    }
  | [i :: is] -> iter found [i :: prefix] is trie
  ]
};

value match_substitution_dyntrie subst_trie items = do
{
  match_substitution_trie (DynUCTrie.is_empty, DynUCTrie.prefix, DynUCTrie.root_value) subst_trie items
};

value apply_substitution font glyphs cmds rest replacement = do
{
  iter replacement

  where rec iter replacement = match replacement with
  [ []      -> rest
  | [r::rs] -> match r with
    [ ConstGlyph glyph       -> [`Glyph (glyph, font) :: iter rs]
    | ConstKern x y          -> [`Kern x y            :: iter rs]
    | CopyGlyph idx          -> [`Glyph glyphs.(idx)  :: iter rs]
    | CopyCommands idx1 idx2 -> do
      {
        (* FIX: also collect movable commands before and after the interval? *)

        copy idx2 (iter rs)

        where rec copy i lst = do
        {
          if i < idx1 then
            lst
          else
            copy (i-1) (cmds.(i) @ lst)
        }
      }
    ]
  ]
};

value separate_items items = do
{
  (* <items> are in reversed order *)

  let collect_commands items = do
  {
    iter [] items

    where rec iter cmds items = match items with
    [ [i :: is] -> match i with
      [ `Command _
      | `Break _
      | `Kern _  -> iter [i :: cmds] is
      | `Box _
      | `Glyph _ -> (cmds, items)
      ]
    | [] -> (cmds, items)
    ]
  }
  in

  iter 0 [] [] items

  where rec iter n glyphs cmds items = match items with
  [ [] -> do
    {
      let glyph_arr = Array.of_list glyphs in

      (* <cmds> might lack the last entry, so we build the array manually. *)

      let cmd_arr   = Array.make (n+1) []  in

      List.fold_left
        (fun i c -> do
          {
            cmd_arr.(i) := c;
            i+1
          })
        0
        cmds;

      (glyph_arr, cmd_arr)
    }
  | _  -> do
    {
      let (new_cmds, rest) = collect_commands items in

      match rest with
      [ [`Glyph gf :: is] -> iter (n+1) [gf :: glyphs] [new_cmds :: cmds] is
      | _                 -> iter n     glyphs         [new_cmds :: cmds] []
      ]
    }
  ]
};

value substitute font find_subst items = do
{
  let result = ListBuilder.make () in

  (* |skip_prefix <skip> <items>| moves the first <skip> glyphs from <items> to <result>. *)

  let rec skip_prefix skip items = do
  {
    if skip <= 0 then
      items
    else match items with
    [ []      -> []
    | [i::is] -> do
      {
        ListBuilder.add result i;

        match i with
        [ `Glyph _ -> skip_prefix (skip-1) is
        | _        -> skip_prefix skip     is
        ]
      }
    ]
  }
  in

  iter items

  where rec iter items = match items with
  [ []        -> ListBuilder.get result
  | [i :: is] -> do
    {
      match find_subst items with
      [ Some (prefix, rest, (replacement, skip)) -> do
        {
          let (glyphs, cmds) = separate_items prefix                                in
          let new_items      = apply_substitution font glyphs cmds rest replacement in

          iter (skip_prefix skip new_items)
        }
      | None -> iter (skip_prefix 1 items)
      ]
    }
  ]
};

