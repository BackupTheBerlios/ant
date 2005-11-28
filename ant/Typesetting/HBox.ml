
open XNum;
open Runtime;
open Logging;
open Dim;
open Box;

(* The maximal badness tolerated by make. *)

value max_h_badness = ref (num_of_int 1000);


(* h-boxes *)

(* |dimensions <boxes>| returns the dimensions of an h-box containing <boxes>. *)

value dimensions boxes = do
{
  iter xdim_zero dim_zero dim_zero boxes

  where rec iter width height depth boxes = match boxes with
  [ []      -> (xdim_to_dim width, height, depth)
  | [b::bs] -> iter
                 (xdim_add_dim width  b.b_width)
                 (dim_max      height b.b_height)
                 (dim_max      depth  b.b_depth)
                 bs
  ]
};

value calc_xwidth boxes = do
{
  iter xdim_zero boxes

  where rec iter w boxes = match boxes with
  [ []      -> w
  | [b::bs] -> iter (xdim_add_dim w b.b_width) bs
  ]
};

value calc_width boxes = xdim_to_dim (calc_xwidth boxes);

(* |calc_width_and_glue <boxes>| calculates the total width
   of <boxes>. Additionally, it returns a list of all intervals
   (a,b) which contain glue. If these intervals are
   (a_1,b_1),...,(a_n,b_n) and the total width is w then the returned
   list contains the values

     w - b_n
     b_n - a_n
     a_n - b_n-1
     b_n-1 - a_n-1
     ...
     a_2 - b_1
     b_1 - a_1
     a_1
*)

value calc_width_and_glue boxes = do
{
  iter [] xdim_zero xdim_zero boxes

  where rec iter glue width delta boxes = match boxes with
  [ []      -> (width, [delta :: glue])
  | [b::bs] -> do
    {
      let new_width = xdim_add_dim width b.b_width in

      (* Check whether it is explicit glue. *)
      match b.b_contents with
      [ GlueBox False _ -> iter [dim_to_xdim b.b_width; delta :: glue]
                                new_width
                                xdim_zero
                                bs
      | _               -> iter glue new_width (xdim_add_dim delta b.b_width) bs
      ]
    }
  ]
};

value layout boxes = match boxes with
[ []  -> empty_box
| [b] -> match b.b_contents with
  [ CommandBox (`GfxCmd c) -> new_compound_box dim_zero dim_zero dim_zero [c]
  | _                      -> b
  ]
| _ -> do
  {
    let (width, height, depth) = dimensions boxes    in
    let result                 = ListBuilder.make () in

    iter xdim_zero boxes

    where rec iter x boxes = match boxes with
    [ []      -> new_compound_box width height depth (ListBuilder.get result)
    | [b::bs] -> match b.b_contents with
      [ CommandBox (`GfxCmd c) -> do
        {
          ListBuilder.add result c;
          iter x  bs
        }
      | _ -> do
        {
          ListBuilder.add result
            (Graphic.PutBox
              (xdim_select_order x width.d_stretch_order width.d_shrink_order)
              dim_zero
              b);
          iter (xdim_add_dim x b.b_width) bs
        }
      ]
    ]
  }
];

value layout_scaled (orig_width, height, depth) scaled_width boxes = do
{
  let (factor, order) = adjustment_ratio orig_width scaled_width in
  let ratio           = if factor </ num_of_int (-1) && order = 0 then
                          (num_of_int (-1), 0)
                        else
                          (factor, order)
                        in
  let bad             = dim_scale_badness (factor, order) in

  if bad >/ !max_h_badness then do
  {
    if factor </ num_zero then
      log_string "\nWarning: Overfull hbox (badness "
    else
      log_string "\nWarning: Underfull hbox (badness ";

    if bad </ infinite then
      log_num bad
    else
      log_string "infinite";

    log_string ")!\n";
    List.iter log_box boxes;
    log_string "\n";
  }
  else
    ();

  let result = ListBuilder.make () in

  iter xdim_zero boxes

  where rec iter x boxes = match boxes with
  [ []      -> new_compound_box (fixed_dim scaled_width) height depth (ListBuilder.get result)
  | [b::bs] -> match b.b_contents with
    [ CommandBox (`GfxCmd c) -> do
      {
        ListBuilder.add result c;
        iter x bs
      }
    | _                      -> do
      {
        let box = scale_box_horiz b ratio in

        ListBuilder.add result (Graphic.PutBox (fixed_dim x.xd_base) dim_zero box);

        iter (xdim_add_dim x box.b_width) bs
      }
    ]
  ]
};

value make boxes = layout boxes;

value make_to width boxes = do
{
  layout_scaled (dimensions boxes) width boxes
};

value make_scaled factor boxes = do
{
  let (w, h, d) = dimensions boxes in

  layout_scaled (w, h, d) (factor */ w.d_base) boxes
};

value make_spread amount boxes = do
{
  let (w, h, d) = dimensions boxes in

  layout_scaled (w, h, d) (w.d_base +/ amount) boxes
};

