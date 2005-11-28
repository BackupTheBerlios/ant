
open XNum;
open Runtime;
open Dim;
open Box;

type table_entry 'a =
{
  te_left     : int;       (* first column of the entry *)
  te_right    : int;       (* its last column           *)
  te_top      : int;       (* its first row             *)
  te_baseline : int;       (* the row of the baseline   *)
  te_bottom   : int;       (* the last row              *)
  te_contents : 'a         (* the contents of the entry *)
};

value calc_column_sizes num_columns entries = do
{
  let widths = Array.make
                 num_columns
                 {
                   d_base = minus_num infinite;
                   d_stretch_factor = infinite;
                   d_stretch_order  = 10;
                   d_shrink_factor  = infinite;
                   d_shrink_order   = 10
                 }
               in

  let rec sum_widths f t = do
  {
    if f > t then
      xdim_zero
    else
      xdim_add_dim (sum_widths (f+1) t) widths.(f)
  }
  in

  calc_width 0 entries

  where rec calc_width col entr = match entr with
  [ [] -> do
    {
      if col < num_columns then
        calc_width (col+1) entries
      else
        widths
    }
  | [e::es] -> do
    {
      if e.te_right = col then do
      {
        let prev_width        = xdim_to_dim (sum_widths e.te_left (e.te_right-1)) in
        let (cur_width, _, _) = e.te_contents in

        widths.(col) := dim_max widths.(col) (dim_sub cur_width prev_width)
      }
      else ();

      calc_width col es
    }
  ]
};

value calc_row_sizes num_rows entries line_params = do
{
  let heights   = Array.make num_rows dim_zero in
  let depths    = Array.make num_rows dim_zero in
  let baselines = Array.make num_rows dim_zero in

  let rec sum_baselines f t = do
  {
    if f > t then
      xdim_zero
    else
      xdim_add_dim (sum_baselines (f+1) t) baselines.(f)
  }
  in

  calc_size 0 entries

  where rec calc_size row entr = match entr with
  [ [] -> do
    {
      if row < num_rows then do
      {
        if row > 0 then do
        {
          let leading = line_params.Galley.leading
                          (new_rule_box dim_zero dim_zero depths.(row-1))
                          (new_rule_box dim_zero heights.(row) dim_zero)
                          line_params
                        in
          baselines.(row-1) := dim_add (dim_add depths.(row-1) heights.(row)) leading
        }
        else ();

        calc_size (row+1) entries       (* start with the next row *)
      }
      else
        (heights, depths, baselines)
    }
  | [e::es] -> do
    {
      if e.te_baseline = row then do
      {
        (*
          This algorithm places the rows spanned by a multi-row entry at the top:

            ===========                      ===========
            ===== +---+                            +---+
            ===== |   |     insetead of:     ===== |   |
                  |   |                      ===== |   |
            ===== +---+                      ===== +---+
            ===========                      ===========

          The latter seems preferable but would be more complicated to implement.
        *)

        let prev_height = xdim_to_dim (sum_baselines e.te_top (e.te_baseline-1)) in
        let (_, cur_height, _) = e.te_contents in

        heights.(row) := dim_max heights.(row) (dim_sub cur_height prev_height);
      }
      else if e.te_bottom = row then do
      {
        let prev_depth = xdim_to_dim (sum_baselines e.te_baseline (e.te_bottom-1)) in
        let (_, _, cur_depth) = e.te_contents in

        depths.(row) := dim_max depths.(row) (dim_sub cur_depth prev_depth)
      }
      else ();

      calc_size row es
    }
  ]
};

value make num_columns num_rows entries line_params = do
{
  let dimensions = List.map
                     (fun e -> { (e) with te_contents = HBox.dimensions e.te_contents })
                     entries
                   in
  let widths                       = calc_column_sizes num_columns dimensions in
  let (heights, depths, baselines) = calc_row_sizes num_rows dimensions line_params in

  let col_start = Array.make (num_columns + 1) xdim_zero in
  let row_start = Array.make (num_rows    + 1) xdim_zero in

  for i = 0 to num_columns - 1 do
  {
    col_start.(i+1) := xdim_add_dim col_start.(i) widths.(i)
  };
  for i = 0 to num_rows - 2 do
  {
    row_start.(i+1) := xdim_add_dim row_start.(i) baselines.(i)
  };
  row_start.(num_rows) := xdim_add_dim row_start.(num_rows - 1) depths.(num_rows - 1);

  let sum_widths  first last = xdim_to_dim (xdim_sub col_start.(last + 1) col_start.(first)) in
  let sum_heights first last = xdim_to_dim (xdim_sub row_start.(last + 1) row_start.(first)) in

  new_compound_box
    (xdim_to_dim col_start.(num_columns))
    heights.(0)
    (xdim_to_dim row_start.(num_rows))
    (layout entries)

  where rec layout entries = match entries with
  [ []      -> []
  | [e::es] -> [Graphic.PutBox
                  (xdim_to_dim col_start.(e.te_left))
                  (dim_neg (xdim_to_dim row_start.(e.te_baseline)))
                  (HBox.make_to (sum_widths e.te_left e.te_right).d_base e.te_contents)
               :: layout es]
  ]
};

