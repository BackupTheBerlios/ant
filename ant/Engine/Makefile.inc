
ENGINE_MOD = Engine/Fonts Engine/Environment Engine/Evaluate Engine/Job Engine/Output
ENGINE_ML  = $(addsuffix .ml,$(ENGINE_MOD))
ENGINE_MLI = $(addsuffix .mli,$(ENGINE_MOD))
ENGINE_CMO = $(addsuffix .$(CMO),$(ENGINE_MOD))

HYPHEN_FILES = $(addprefix data/hyphen/, $(addsuffix .hyphen, $(HYPHEN_PATTERNS)))

Engine/HyphenTable.ml: Util/GenHyphTable
	./Util/GenHyphTable $(HYPHEN_FILES) >$@

Engine/HyphenTable.cmi: Engine/HyphenTable.mli lib/Runtime.cmi

Engine/HyphenTable.cmo: Engine/HyphenTable.ml Engine/HyphenTable.cmi
	$(MLC_BIN) $(MLFLAGS) -c $<

Engine/HyphenTable.cmx: Engine/HyphenTable.ml Engine/HyphenTable.cmi
	$(MLC_NAT) $(MLFLAGS) -c $<

Engine/Engine.$(CMO): Engine/HyphenTable.$(CMO) $(ENGINE_CMO)
	$(MLC) -pack $(DEBUGFLAGS) -o $@ $(CUSTOM) $^

Engine/Engine.cmi Engine/Engine.o: Engine/Engine.$(CMO)
	touch $@

ifdef NATIVE
  ENGINE_INSTALL = Engine/Engine.$(CMO) Engine/Engine.cmi Engine/Engine.o
else
  ENGINE_INSTALL = Engine/Engine.$(CMO) Engine/Engine.cmi
endif

lib/Engine.$(CMO) lib/Engine.cmi: $(ENGINE_INSTALL)
	cp -f $^ lib

-include Engine/Makefile.depend

Engine/depend:
	$(ML_DEPEND) $(ENGINE_MLI) $(ENGINE_ML) | sed -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >Engine/Makefile.depend

# vim:set ft=make:
