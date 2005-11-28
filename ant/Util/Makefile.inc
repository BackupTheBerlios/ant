
UTILS = Util/GenHyphTable #Util/Kerning

Util/GenHyphTable: Tools/Tools.$(CMA) lib/CamlImages.$(CMA) lib/Unicode.$(CMA) lib/Runtime.$(CMA) \
                   Util/GenHyphTable.$(CMO)
	$(LINK_ML)

Util/Kerning: Tools/Tools.$(CMA) lib/CamlImages.$(CMA) lib/Runtime.$(CMA) Engine/Job.$(CMO) Util/Kerning.$(CMO)
	$(LINK_ML)

Util/GenHyphTable.$(CMO): Util/GenHyphTable.ml lib/CamlImages.$(CMA) lib/Runtime.$(CMA)
Util/Kerning.$(CMO):      Util/Kerning.ml lib/CamlImages.$(CMA) lib/Runtime.$(CMA)

Util/depend:

# vim:set ft=make:
