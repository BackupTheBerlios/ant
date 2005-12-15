
open Unicode.Types;
open Types;

type scope =
{
  symbol_table   : Hashtbl.t uc_string Lexer.token_class;
  global_symbols : Hashtbl.t int unknown;
  local_symbols  : Hashtbl.t int (int * int);
  depth          : int
};

value create () =
{
  symbol_table   = Lexer.initial_symbol_table ();
  global_symbols = Hashtbl.create 1000;
  local_symbols  = Hashtbl.create 100;
  depth          = 0
};

value symbol_table scope = scope.symbol_table;

value copy scope =
{
  symbol_table   = Hashtbl.copy scope.symbol_table;
  global_symbols = Hashtbl.copy scope.global_symbols;
  local_symbols  = Hashtbl.copy scope.local_symbols;
  depth          = scope.depth
};

(*
value print_scope s = do
{
  Runtime.Logging.log_string "globals:\n";
  Hashtbl.iter
    (fun n _ -> do
      {
        Runtime.Logging.log_uni_string (Unicode.SymbolTable.symbol_to_string n);
        Runtime.Logging.log_string "\n"
      })
    s.global_symbols;

  Hashtbl.iter
    (fun n _ -> do
      {
        Runtime.Logging.log_uni_string (Unicode.SymbolTable.symbol_to_string n);
        Runtime.Logging.log_string "\n"
      })
    s.local_symbols
};
*)

value add_global scope symbol val = do
{
  Hashtbl.replace scope.global_symbols symbol (create_unknown val)
};

value push scope symbols = do
{
  let local = Hashtbl.copy scope.local_symbols in
  let depth = scope.depth + 1                  in

  iter 0 symbols

  where rec iter n symbols = match symbols with
  [ [] ->
      ({
         symbol_table   = scope.symbol_table;
         global_symbols = scope.global_symbols;
         local_symbols  = local;
         depth          = depth
       },
       n)
  | [s::ss] -> do
    {
      if Hashtbl.mem local s then
        iter n ss
      else do
      {
        Hashtbl.add local s (depth, n);
        iter (n+1) ss
      }
    }
  ]
};

value lookup_local scope symbol = do
{
  let (level, index) = Hashtbl.find scope.local_symbols symbol in

  (scope.depth - level, index)
};

value lookup_global scope symbol = do
{
  Hashtbl.find scope.global_symbols symbol
};

value lookup scope symbol = do
{
  try
    let (depth, index) = lookup_local scope symbol in

    TVariable depth index
  with
  [ Not_found -> TGlobal (lookup_global scope symbol) ]
};
