open Pcaml;

EXTEND
  expr: LEVEL "+"
  [ [ x = expr; "+/"; y = expr  ->  <:expr< add_num  $x$ $y$ >>
    | x = expr; "-/"; y = expr  ->  <:expr< sub_num  $x$ $y$ >> ] ];
  expr: LEVEL "*"
  [ [ x = expr; "*/"; y = expr  ->  <:expr< mult_num $x$ $y$ >>
    | x = expr; "//"; y = expr  ->  <:expr< div_num  $x$ $y$ >> ] ];
  expr: LEVEL "unary minus"
  [ [ x = expr; "/:"; y = expr  ->  <:expr< num_of_ints $x$ $y$ >> ] ];
  expr: LEVEL "<"
  [ [ x = expr; "=/";  y = expr  ->  <:expr< eq_num $x$ $y$ >>
    | x = expr; "</";  y = expr  ->  <:expr< lt_num $x$ $y$ >>
    | x = expr; ">/";  y = expr  ->  <:expr< gt_num $x$ $y$ >>
    | x = expr; "<=/"; y = expr  ->  <:expr< le_num $x$ $y$ >>
    | x = expr; ">=/"; y = expr  ->  <:expr< ge_num $x$ $y$ >>
    | x = expr; "<>/"; y = expr  ->  <:expr< not (eq_num  $x$ $y$) >> ] ];
END;
