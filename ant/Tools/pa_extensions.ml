open Pcaml;

EXTEND
  expr: LEVEL "~-"
  [ [ "!"; x = expr -> <:expr< $x$.val >> ] ];
END;
