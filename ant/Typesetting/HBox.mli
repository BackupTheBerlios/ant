
open XNum;
open Runtime;
open Dim;
open Box;

value dimensions          : list box -> (dim * dim * dim);
value calc_xwidth         : list box -> xdim;
value calc_width          : list box -> dim;
value calc_width_and_glue : list box -> (xdim * list xdim);

value make        : list box -> box;
value make_to     : num -> list box -> box;
value make_scaled : num -> list box -> box;
value make_spread : num -> list box -> box;

