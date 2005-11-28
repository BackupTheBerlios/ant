
VM_MOD         = VM/Opaque VM/Types VM/VMPrivate VM/Machine
VM_ML          = $(addsuffix .ml,$(VM_MOD))
VM_MLI         = $(addsuffix .mli,$(VM_MOD))
VM_CMO         = $(addsuffix .$(CMO),$(VM_MOD))
VM_PRIVATE_MOD = VM/Lexer VM/Parser VM/Scope VM/Compile VM/Evaluate VM/Primitives
VM_PRIVATE_ML  = $(addsuffix .ml,$(VM_PRIVATE_MOD))
VM_PRIVATE_MLI = $(addsuffix .mli,$(VM_PRIVATE_MOD))
VM_PRIVATE_CMO = $(addsuffix .$(CMO),$(VM_PRIVATE_MOD))

VM/VMPrivate.$(CMO): $(VM_PRIVATE_CMO)
	$(MLC) -pack $(DEBUGFLAGS) -o $@ $(CUSTOM) $^

VM/VMPrivate.cmi VM/VMPrivate.o: VM/VMPrivate.$(CMO)
	touch $@

VM/VM.$(CMO): $(VM_CMO)
	$(MLC) -pack $(DEBUGFLAGS) -o $@ $(CUSTOM) $^

VM/VM.cmi VM/VM.o: VM/VM.$(CMO)
	touch $@

ifdef NATIVE
  VM_INSTALL = VM/VM.$(CMO) VM/VM.cmi VM/VM.o
else
  VM_INSTALL = VM/VM.$(CMO) VM/VM.cmi
endif

VM/Ali.$(CMO): VM/Ali.ml VM/VM.cmi

lib/VM.$(CMO) lib/VM.cmi: $(VM_INSTALL)
	cp -f $^ lib

-include VM/Makefile.depend

VM/depend:
	touch VM/VMPrivate.ml VM/VMPrivate.mli
	$(ML_DEPEND) $(filter-out VM/VMPrivate.%,$(VM_MLI) $(VM_PRIVATE_MLI) $(VM_ML) $(VM_PRIVATE_ML)) | sed -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >VM/Makefile.depend
	rm VM/VMPrivate.ml VM/VMPrivate.mli

VM/ali: Tools/Tools.$(CMA) lib/CamlImages.$(CMA) lib/Unicode.$(CMA) lib/Runtime.$(CMA) $(VM_CMO) VM/Ali.$(CMO)
	$(LINK_ML)

# vim:set ft=make:
