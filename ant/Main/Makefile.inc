
MAIN_MOD = Main/Main
MAIN_ML  = $(addsuffix .ml,$(MAIN_MOD))
MAIN_MLI = $(addsuffix .mli,$(MAIN_MOD))
MAIN_CMO = $(addsuffix .$(CMO),$(MAIN_MOD))

-include Main/Makefile.depend

Main/Main.cmi:    Main/Main.mli lib/Runtime.$(CMA) lib/Typesetting.$(CMO) lib/Engine.$(CMO) lib/Markup.$(CMO)
Main/Main.$(CMO): Main/Main.ml lib/Runtime.$(CMA) lib/Typesetting.$(CMO) lib/Engine.$(CMO) lib/Markup.$(CMO)

Main/depend:
	$(ML_DEPEND) $(MAIN_MLI) $(MAIN_ML) | sed -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >Main/Makefile.depend

# vim:set ft=make:
