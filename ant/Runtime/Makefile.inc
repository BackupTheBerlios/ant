
RUNTIME_MOD = Runtime/UCStream Runtime/Bitmap Runtime/KPathSea Runtime/FreeType \
              Runtime/DynamicTrie Runtime/DynUCTrie Runtime/Trie Runtime/PTable Runtime/SymbolSet \
              Runtime/Type1 Runtime/OpenType Runtime/Logging Runtime/Dim Runtime/Graphic \
              Runtime/Substitute Runtime/FontMetric Runtime/Encodings Runtime/Hyphenation Runtime/JustHyph \
              Runtime/Glyph Runtime/FontPK Runtime/FontTFM Runtime/FontFT Runtime/FontVirtual \
              Runtime/FontTable \
              Runtime/DVI Runtime/PDF Runtime/OutputPDF Runtime/PostScript Runtime/SVG
RUNTIME_ML  = $(addsuffix .ml,$(RUNTIME_MOD))
RUNTIME_MLI = $(addsuffix .mli,$(RUNTIME_MOD))
RUNTIME_CMO = $(addsuffix .$(CMO),$(RUNTIME_MOD))
RUNTIME_CMI = $(addsuffix .cmi,$(RUNTIME_MOD))
RUNTIME_C   = Runtime/kpathsea-stubs.c Runtime/freetype-stubs.c # Runtime/otf-stubs.cc
RUNTIME_O   = Runtime/kpathsea-stubs.o Runtime/freetype-stubs.o # Runtime/otf-stubs.o

ifdef NATIVE
  MLFLAGS_Runtime_ = #-for-pack Runtime
else
  MLFLAGS_Runtime_ =
endif

Runtime/kpathsea-stubs.o: Runtime/kpathsea-stubs.c
	$(MLC) -c $(KPATHSEA_CFLAGS) $(SELFAUTO_FLAGS) -o $@ $^
	mv kpathsea-stubs.o $@  # BUG: ocaml creates the object file in the current directory

Runtime/freetype-stubs.o: Runtime/freetype-stubs.c
	$(MLC) -c $(FREETYPE_FLAGS) -o $@ $^
	mv freetype-stubs.o $@  # BUG: ocaml creates the object file in the current directory

#Runtime/otf-stubs.o: Runtime/otf-stubs.cc
#	$(CXX) -c -I$(CAML_LIB_DIR) $(OTF_CFLAGS) -o $@ $^

Runtime/Runtime.$(CMO): $(RUNTIME_CMO) $(RUNTIME_O)
	$(MLC) -pack $(DEBUGFLAGS) -I Runtime -o $@ $(CUSTOM) $^

Runtime/Runtime.cmi: Runtime/Runtime.$(CMO)
	touch $@

Runtime/Runtime.$(CMA): Runtime/Runtime.$(CMO) $(RUNTIME_O)
	$(MLC) -a -o $@ $(CUSTOM) $^

Runtime/Runtime.a: Runtime/Runtime.$(CMA)
	touch $@


ifdef NATIVE
  RUNTIME_INSTALL = Runtime/Runtime.$(CMA) Runtime/Runtime.cmi Runtime/Runtime.a
else
  RUNTIME_INSTALL = Runtime/Runtime.$(CMA) Runtime/Runtime.cmi
endif

lib/Runtime.$(CMA) lib/Runtime.cmi: $(RUNTIME_INSTALL)
	cp -f $^ lib

-include Runtime/Makefile.depend

Runtime/depend:
	$(ML_DEPEND) $(RUNTIME_MLI) $(RUNTIME_ML) | sed -e 's!Images.cmx!Images.$(CMA)!' -e 's!Unicode.cmx!Unicode.$(CMA)!' >Runtime/Makefile.depend

# vim:set ft=make:
