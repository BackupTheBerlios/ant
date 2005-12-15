
VM_MOD         = VM/Opaque VM/Types VM/Private/VMPrivate VM/Machine
VM_ML          = $(addsuffix .ml,$(VM_MOD))
VM_MLI         = $(addsuffix .mli,$(VM_MOD))
VM_CMO         = $(addsuffix .$(CMO),$(VM_MOD))
VM_PRIVATE_MOD = VM/Private/Lexer VM/Private/Parser VM/Private/Scope VM/Private/Compile VM/Private/CStack VM/Private/Evaluate \
                 VM/Private/Primitives
VM_PRIVATE_ML  = $(addsuffix .ml,$(VM_PRIVATE_MOD))
VM_PRIVATE_MLI = $(addsuffix .mli,$(VM_PRIVATE_MOD))
VM_PRIVATE_CMO = $(addsuffix .$(CMO),$(VM_PRIVATE_MOD))

ifdef NATIVE
  MLFLAGS_VM_         = -I VM/Private #-for-pack VM
  MLFLAGS_VM_Private_ = -I VM #-for-pack VM.VMPrivate
else
  MLFLAGS_VM_         = -I VM/Private
  MLFLAGS_VM_Private_ = -I VM
endif

VM/Private/VMPrivate.$(CMO): $(VM_PRIVATE_CMO)
	$(MLC) -pack $(DEBUGFLAGS) -I VM/Private -for-pack VM -o $@ $(CUSTOM) $^

VM/Private/VMPrivate.cmi VM/Private/VMPrivate.o: VM/Private/VMPrivate.$(CMO)
	touch $@

VM/VM.$(CMO): $(VM_CMO)
	$(MLC) -pack $(DEBUGFLAGS) -I VM -I VM/Private -o $@ $(CUSTOM) $^

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
-include VM/Private/Makefile.depend

VM/depend:
	touch VM/Private/VMPrivate.ml VM/Private/VMPrivate.mli
	$(ML_DEPEND) -I VM/Private $(filter-out VM/Private/VMPrivate.%,$(VM_MLI) $(VM_ML)) | sed -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >VM/Makefile.depend
	rm VM/Private/VMPrivate.ml VM/Private/VMPrivate.mli

VM/Private/depend:
	$(ML_DEPEND) $(VM_PRIVATE_MLI) $(VM_PRIVATE_ML) | sed -e 's!Unicode.cmx!Unicode.$(CMA)!' -e 's!Runtime.cmx!Runtime.$(CMA)!' >VM/Private/Makefile.depend

VM/ali: Tools/Tools.$(CMA) lib/CamlImages.$(CMA) lib/Unicode.$(CMA) lib/Runtime.$(CMA) $(VM_CMO) VM/Ali.$(CMO)
	$(LINK_ML)

# vim:set ft=make:
