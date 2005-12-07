
MARKUP_MOD = Markup/Counter Markup/CharCode Markup/Parser Markup/ParseState Markup/Group Markup/Macro \
             Markup/Mode Markup/ParseArgs Markup/ALCoding Markup/ALDim Markup/ALEnvironment \
             Markup/ALGraphics Markup/ALNodes Markup/ALParseState Markup/ALBindings \
             Markup/Primitives Markup/Run
MARKUP_ML  = $(addsuffix .ml,$(MARKUP_MOD))
MARKUP_MLI = $(addsuffix .mli,$(MARKUP_MOD))
MARKUP_CMO = $(addsuffix .$(CMO),$(MARKUP_MOD))

ifdef NATIVE
  MLFLAGS_Markup_ = #-for-pack Markup
else
  MLFLAGS_Markup_ =
endif

Markup/Markup.$(CMO): $(MARKUP_CMO)
	$(MLC) -pack $(DEBUGFLAGS) -I Markup -o $@ $(CUSTOM) $^

Markup/Markup.cmi Markup/Markup.o: Markup/Markup.$(CMO)
	touch $@

ifdef NATIVE
  MARKUP_INSTALL = Markup/Markup.$(CMO) Markup/Markup.cmi Markup/Markup.o
else
  MARKUP_INSTALL = Markup/Markup.$(CMO) Markup/Markup.cmi
endif

lib/Markup.$(CMO) lib/Markup.cmi: $(MARKUP_INSTALL)
	cp -f $^ lib

-include Markup/Makefile.depend

Markup/depend:
	$(ML_DEPEND) $(MARKUP_MLI) $(MARKUP_ML) | sed -e 's!Images.cmx!Images.$(CMA)!' -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >Markup/Makefile.depend

# vim:set ft=make:
