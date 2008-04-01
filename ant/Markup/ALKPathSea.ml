
open XNum;
open Runtime;
open Logging;
open VM;
open ALCoding;

module UString     = Unicode.UString;
module SymbolTable = Unicode.SymbolTable;
module SymbolMap   = SymbolTable.SymbolMap;

value find_file args = match args with
[ [name; file_type; must_exist] -> do
  {
    let n = decode_uc_string "kpse_find_file" name;
    let t = decode_symbol    "kpse_find_file" file_type;
    let e = decode_bool      "kpse_find_file" must_exist;

    let tt = do
    {
      if t = sym_PK then
        `PK
      else if t = sym_TFM then
        `TFM
      else if t = sym_AFM then
        `AFM
      else if t = sym_TeX then
        `TeX
      else if t = sym_Type_1 then
        `Type1
      else if t = sym_TrueType then
        `TrueType
      else if t = sym_OpenType then
        `OpenType
      else if t = sym_Source then
        `Source
      else
        `Source
    };
    let s = KPathSea.find_file (UString.uc_string_to_ascii n) tt e;

    Machine.uc_string_to_char_list (UString.uc_string_of_ascii s)
  }
| _ -> assert False
];

value find_glyph name dpi = do
{
  let n = decode_uc_string   "kpse_find_glyph" name;
  let d = Machine.decode_num "kpse_find_glyph" dpi;

  let s = KPathSea.find_glyph (UString.uc_string_to_ascii n) (int_of_num (round_num d));

  Machine.uc_string_to_char_list (UString.uc_string_of_ascii s)
};

