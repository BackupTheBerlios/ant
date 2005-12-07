
TYPESETTING_MOD = Typesetting/Box Typesetting/Builder Typesetting/Compose \
                  Typesetting/HBox Typesetting/VBox \
                  Typesetting/Glyph Typesetting/MathLayout Typesetting/ParLayout Typesetting/Galley \
                  Typesetting/Page Typesetting/PageLayout Typesetting/AreaGalley \
                  Typesetting/FloatVertical Typesetting/Footnote \
                  Typesetting/Table
TYPESETTING_ML  = $(addsuffix .ml,$(TYPESETTING_MOD))
TYPESETTING_MLI = $(addsuffix .mli,$(TYPESETTING_MOD))
TYPESETTING_CMO = $(addsuffix .$(CMO),$(TYPESETTING_MOD))

ifdef NATIVE
  MLFLAGS_Typesetting_ = #-for-pack Typesetting
else
  MLFLAGS_Typesetting_ =
endif

Typesetting/Typesetting.$(CMO): $(TYPESETTING_CMO)
	$(MLC) -pack $(DEBUGFLAGS) -I Typesetting -o $@ $(CUSTOM) $^

Typesetting/Typesetting.cmi Typesetting/Typesetting.o: Typesetting/Typesetting.$(CMO)
	touch $@

ifdef NATIVE
  TYPESETTING_INSTALL = Typesetting/Typesetting.$(CMO) Typesetting/Typesetting.cmi Typesetting/Typesetting.o
else
  TYPESETTING_INSTALL = Typesetting/Typesetting.$(CMO) Typesetting/Typesetting.cmi
endif

lib/Typesetting.$(CMO) lib/Typesetting.cmi: $(TYPESETTING_INSTALL)
	cp -f $^ lib

-include Typesetting/Makefile.depend

Typesetting/depend:
	$(ML_DEPEND) $(TYPESETTING_MLI) $(TYPESETTING_ML) | sed -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >Typesetting/Makefile.depend

# vim:set ft=make:
