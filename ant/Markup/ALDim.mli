
open Runtime.Dim;
open VM;
open Types;

value wrap_dim    : dim -> partial_value;
value unwrap_dim  : string -> unknown -> dim;

value prim_make_dim            : unknown -> list unknown -> unit;
value prim_fixed_dim           : unknown -> unknown -> unit;
value prim_dim_zero            : partial_value;
value prim_dim_1pt             : partial_value;
value prim_dim_12pt            : partial_value;
value prim_dim_fil             : partial_value;
value prim_dim_fill            : partial_value;
value prim_dim_ss              : partial_value;
value prim_dim_filneg          : partial_value;
value prim_dim_equal           : unknown -> unknown -> unknown -> unit;
value prim_dim_add             : unknown -> unknown -> unknown -> unit;
value prim_dim_neg             : unknown -> unknown -> unit;
value prim_dim_sub             : unknown -> unknown -> unknown -> unit;
value prim_dim_mult            : unknown -> unknown -> unknown -> unit;
value prim_dim_max             : unknown -> unknown -> unknown -> unit;
value prim_dim_min             : unknown -> unknown -> unknown -> unit;
value prim_dim_max_stretch     : unknown -> unknown -> unit;
value prim_dim_max_shrink      : unknown -> unknown -> unit;
value prim_dim_max_value       : unknown -> unknown -> unit;
value prim_dim_min_value       : unknown -> unknown -> unit;
value prim_dim_shift_base      : unknown -> unknown -> unknown -> unit;
value prim_dim_shift_base_upto : unknown -> unknown -> unknown -> unit;
value prim_dim_inc_upto        : unknown -> unknown -> unknown -> unit;
value prim_dim_dec_upto        : unknown -> unknown -> unknown -> unit;
value prim_dim_resize_upto     : unknown -> unknown -> unknown -> unit;
value prim_adjustment_ratio    : unknown -> unknown -> unknown -> unit;
value prim_dim_scale_badness   : unknown -> unknown -> unit;
value prim_dim_scale           : unknown -> unknown -> unknown -> unit;
value prim_dim_scale_upto      : unknown -> unknown -> unknown -> unit;

