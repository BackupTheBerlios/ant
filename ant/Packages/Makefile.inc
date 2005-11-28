
PACKAGES_MOD = Packages/german Packages/advi
PACKAGES_ML  = $(addsuffix .ml,$(PACKAGES_MOD))
PACKAGES_MLI = $(addsuffix .mli,$(PACKAGES_MOD))
PACKAGES_CMO = $(addsuffix .$(CMO),$(PACKAGES_MOD))

-include Packages/Makefile.depend

Packages/depend:
	$(ML_DEPEND) $(PACKAGES_MLI) $(PACKAGES_ML) | sed  -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >Packages/Makefile.depend

# vim:set ft=make:
