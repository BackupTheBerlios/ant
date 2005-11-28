extern "C" {
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <memory.h>
}

#include "config.h"
#include "lcdf/string.hh"
#include "lcdf/error.hh"
#include "lcdf/permstr.hh"
#include "lcdf/transform.hh"
#include "efont/otf.hh"
#include "efont/cff.hh"
#include "efont/metrics.hh"
#include "efont/otfcmap.hh"
#include "efont/t1bounds.hh"
#include "efont/otfgpos.hh"

//static ErrorHandler *errh = ErrorHandler::silent_handler();
static ErrorHandler *errh = new FileErrorHandler(stderr, "lcdf: ");

#define Alloc_custom(ops,t)   (caml_alloc_custom(&(ops), sizeof(t), 0, 1))
#define Alloc_abstract(t)     (caml_alloc(sizeof(t)/sizeof(void *), Abstract_tag))
#define Custom_val(v,t)       (*(t *)Data_custom_val((v)))
#define Abstract_val(v,t)     (*(t *)&Field((v), 0))

#define Declare_Ops(id, name, type) \
  void finalise_##id(value x) \
  { \
    delete (Custom_val(x, type)); \
  }; \
  static struct custom_operations id##_ops = \
  { \
    name, \
    finalise_##id, \
    custom_compare_default, \
    custom_hash_default, \
    custom_serialize_default, \
    custom_deserialize_default \
  }

#define Alloc_otf()         (Alloc_custom(otf_ops,      Efont::OpenType::Font *))
#define Alloc_cff()         (Alloc_custom(cff_ops,      Efont::Cff *))
#define Alloc_cff_font()    (Alloc_custom(cff_font_ops, Efont::Cff::Font *))
#define Alloc_cmap()        (Alloc_custom(cmap_ops,     Efont::OpenType::Cmap *))
#define Alloc_gpos()        (Alloc_custom(gpos_ops,     Efont::OpenType::Gpos *))
#define Alloc_lookup()      (Alloc_abstract(Efont::OpenType::GposLookup))
#define Alloc_positioning() (Alloc_abstract(Efont::OpenType::Positioning))

#define Otf_val(v)         (Custom_val((v), Efont::OpenType::Font *))
#define Cff_val(v)         (Custom_val((v), Efont::Cff *))
#define Cff_Font_val(v)    (Custom_val((v), Efont::Cff::Font *))
#define CMap_val(v)        (Custom_val((v), Efont::OpenType::Cmap *))
#define GPos_val(v)        (Custom_val((v), Efont::OpenType::Gpos *))
#define Lookup_val(v)      (Abstract_val((v), Efont::OpenType::GposLookup))
#define Positioning_val(v) (Abstract_val((v), Efont::OpenType::Positioning))

Declare_Ops(otf,      "opentype font", Efont::OpenType::Font *);
Declare_Ops(cff,      "cff",           Efont::Cff *);
Declare_Ops(cff_font, "cff font",      Efont::Cff::Font *);
Declare_Ops(cmap,     "cmap",          Efont::OpenType::Cmap *);
Declare_Ops(gpos,     "gpos",          Efont::OpenType::Gpos *);

extern "C"
CAMLprim value Wrapper_initialise_otf (value x)
{
  CAMLparam1(x);

  ErrorHandler::static_initialize(errh);

  CAMLreturn(Val_unit);
};

extern "C"
CAMLprim value Wrapper_parse_otf_font (value data)
{
  CAMLparam1(data);
  CAMLlocal1(block);

  try
  {
    block = Alloc_otf();
    Otf_val(block) = NULL;

    String str(String_val(data), caml_string_length(data));

    if (str.out_of_memory())
      failwith("parse_otf_font: out of memory");

    Otf_val(block) = new Efont::OpenType::Font(str, errh);

    if (!Otf_val(block)->ok())
      failwith("parse_otf_font: font corrupt");

    CAMLreturn(block);
  }
  catch (...)
  {
    failwith("parse_otf_font: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_parse_cff_font (value data)
{
  CAMLparam1(data);
  CAMLlocal1(block);

  try
  {
    block = Alloc_cff();
    Cff_val(block) = NULL;

    String str(String_val(data), caml_string_length(data));

    if (str.out_of_memory())
      failwith("parse_cff_font: out of memory");

    Cff_val(block) = new Efont::Cff(str, errh);

    if (!Cff_val(block)->ok())
      failwith("parse_cff_font: font corrupt");

    CAMLreturn(block);
  }
  catch (...)
  {
    failwith("parse_cff_font: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_parse_cmap (value data)
{
  CAMLparam1(data);
  CAMLlocal1(block);

  try
  {
    block = Alloc_cmap();
    CMap_val(block) = NULL;

    String str(String_val(data), caml_string_length(data));

    if (str.out_of_memory())
      failwith("parse_cmap: out of memory");

    CMap_val(block) = new Efont::OpenType::Cmap(str, errh);

    if (!CMap_val(block)->ok())
      failwith("parse_cmap: cmap corrupt");

    CAMLreturn(block);
  }
  catch (...)
  {
    failwith("parse_cmap: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_parse_gpos (value data)
{
  CAMLparam1(data);
  CAMLlocal1(block);

  try
  {
    block = Alloc_gpos();
    GPos_val(block) = NULL;

    String str(String_val(data), caml_string_length(data));

    if (str.out_of_memory())
      failwith("parse_gpos: out of memory");

    GPos_val(block) = new Efont::OpenType::Gpos(str, errh);

    CAMLreturn(block);
  }
  catch (...)
  {
    failwith("parse_gpos: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_otf_table (value otf, value tag)
{
  CAMLparam2(otf, tag);
  CAMLlocal1(str);

  try
  {
    Efont::OpenType::Tag t(Int32_val(tag));

    const String &table = Otf_val(otf)->table(t);

    str = caml_alloc_string(table.length());

    memcpy(String_val(str), table.data(), table.length());

    CAMLreturn(str);
  }
  catch (...)
  {
    failwith("otf_table: runtime error");
  }
};


extern "C"
CAMLprim value Wrapper_get_cff_font (value cff)
{
  CAMLparam1(cff);
  CAMLlocal1(block);

  try
  {
    block = Alloc_cff_font();
    Cff_Font_val(block) = NULL;

    Efont::Cff::FontParent *fp = Cff_val(cff)->font(PermString(), errh);

    if (fp == NULL || !fp->ok())
      failwith("get_cff_font: runtime error");

    Cff_Font_val(block) = dynamic_cast<Efont::Cff::Font *>(fp);

    if (Cff_Font_val(block) == NULL)
      failwith("get_cff_font: CID-keyed fonts not supported");

    CAMLreturn(block);
  }
  catch (...)
  {
    failwith("get_cff_font: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_num_glyphs (value cff_font)
{
  CAMLparam1(cff_font);

  try
  {
    CAMLreturn(Val_int(Cff_Font_val(cff_font)->nglyphs()));
  }
  catch (...)
  {
    failwith("num_glyphs: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_get_glyph (value cmap, value chr)
{
  CAMLparam2(cmap, chr);

  try
  {
    CAMLreturn(Val_int(CMap_val(cmap)->map_uni(Int_val(chr))));
  }
  catch (...)
  {
    failwith("get_glyph: runtime error");
  }
};


extern "C"
CAMLprim value Wrapper_get_bounds (value font, value glyph)
{
  CAMLparam2(font, glyph);
  CAMLlocal1(block);
  int bounds[4];
  int width;

  try
  {
    Transform transform;

    Efont::CharstringBounds::bounds(
      transform,
      Cff_Font_val(font)->glyph_context(Int_val(glyph)),
      bounds,
      width);

    block = alloc_tuple(5);

    Store_field(block, 0, Val_int(width));
    Store_field(block, 1, Val_int(bounds[0]));
    Store_field(block, 2, Val_int(bounds[1]));
    Store_field(block, 3, Val_int(bounds[2]));
    Store_field(block, 4, Val_int(bounds[3]));

    CAMLreturn(block);
  }
  catch (...)
  {
    failwith("get_bounds: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_get_family_name (value font)
{
  CAMLparam1(font);
  CAMLlocal1(str);

  try
  {
    String name = Cff_Font_val(font)->dict_string(Efont::Cff::oFamilyName);

    str = caml_alloc_string(name.length());

    memcpy(String_val(str), name.data(), name.length());

    CAMLreturn(str);
  }
  catch (...)
  {
    failwith("get_family_name: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_get_full_name (value font)
{
  CAMLparam1(font);
  CAMLlocal1(str);

  try
  {
    String name = Cff_Font_val(font)->dict_string(Efont::Cff::oFullName);

    str = caml_alloc_string(name.length());

    memcpy(String_val(str), name.data(), name.length());

    CAMLreturn(str);
  }
  catch (...)
  {
    failwith("get_full_name: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_get_design_size (value gpos, value script, value lang)
{
  CAMLparam3(gpos, script, lang);

  try
  {
    int required_fid;
    Vector<int> fids;

    Efont::OpenType::Tag script_tag(Int32_val(script));
    Efont::OpenType::Tag lang_tag(Int32_val(lang));
    GPos_val(gpos)->script_list().features(script_tag, lang_tag, required_fid, fids, 0, false);

    int size_fid = GPos_val(gpos)->feature_list().find(Efont::OpenType::Tag("size"), fids);
    if (size_fid < 0)
      failwith("get_design_size: no size feature");

    Efont::OpenType::Data size_data = GPos_val(gpos)->feature_list().params(size_fid, 10, errh, true);
    if (!size_data.length())
      failwith("get_design_size: no size data");

    int size = size_data.u16(0);
    if (size < 10 || size > 10000)
      failwith("get_design_size: size corrupt");

    CAMLreturn(Int_val(size));
  }
  catch (...)
  {
    failwith("get_design_size: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_gpos_num_lookups (value gpos)
{
  CAMLparam1(gpos);

  try
  {
    CAMLreturn(Val_int(GPos_val(gpos)->nlookups()));
  }
  catch (...)
  {
    failwith("gpos_lookup: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_gpos_lookup (value gpos, value idx)
{
  CAMLparam2(gpos, idx);
  CAMLlocal1(block);

  try
  {
    if (Int_val(idx) < 0 || Int_val(idx) >= GPos_val(gpos)->nlookups())
      failwith("gpos_lookup: index out of range");

    block = Alloc_lookup();

    Lookup_val(block) = GPos_val(gpos)->lookup(Int_val(idx));

    CAMLreturn(block);
  }
  catch (...)
  {
    failwith("gpos_lookup: runtime error");
  }
};

extern "C"
CAMLprim value Wrapper_lookup_unparse_automatics (value lookup)
{
  CAMLparam1(lookup);
  CAMLlocal2(arr, block);

  try
  {
    Vector<Efont::OpenType::Positioning> pos;

    bool understood = Lookup_val(lookup).unparse_automatics(pos);

    if (!Lookup_val(lookup).unparse_automatics(pos))
      failwith("lookup_unparse_automatics: not understood");

    arr = alloc_tuple(pos.size());

    for (int i = 0; i < pos.size(); i++)
    {
      block = Alloc_positioning();

      Positioning_val(block) = pos[i];

      Store_field(arr, i, block);
    }

    CAMLreturn(arr);
  }
  catch (...)
  {
    failwith("lookup_unparse_automatics: runtime error");
  }
};

