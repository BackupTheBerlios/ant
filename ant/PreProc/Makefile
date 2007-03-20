
#NATIVE=1
#NATIVE_CAMLP4=

OPT = .opt

MLC_BIN = ocamlc$(OPT)
MLC_NAT = ocamlopt$(OPT)

ifdef NATIVE
  MLC = $(MLC_NAT)
  CMO = cmx
  CMA = cmxa
  CUSTOM =
  DEBUGFLAGS =
  DEPFLAGS = -native
else
  MLC = $(MLC_BIN)
  CMO = cmo
  CMA = cma
  CUSTOM = -custom
  DEBUGFLAGS = -g# -DDEBUG
  DEPFLAGS =
endif

ifdef NATIVE_CAMLP4
  CAMLP4         = camlp4rf     # preprocessor for camlp4 syntax extensions
  MLPP           = ./camlp4opt  # preprocessor for normal ml files
  MLPP_PA        =
  CAMLP4_TARGETS = camlp4opt    # additional targets to make
else
  CAMLP4         = camlp4rf     # preprocessor for camlp4 syntax extensions
  MLPP           = camlp4r      # preprocessor for normal ml files
  MLPP_PA        = ./pa_Num.cmo ./pa_extensions.cmo
  CAMLP4_TARGETS = pa_Num.cmo pa_extensions.cmo      # additional targets to make
endif

MLCPPLIB  := $(shell $(CAMLP4) -where)

MLFLAGS  = -rectypes -pp "$(MLPP) $(MLPP_PA)" $(DEBUGFLAGS)

%.cmi: %.mli
	$(MLC) $(MLFLAGS) -c $<

%.cmo: %.ml
	$(MLC_BIN) $(MLFLAGS) -c $<

%.cmx: %.ml
	$(MLC_NAT) $(MLFLAGS) -c $<

%.$(CMO) %.cmi: %.ml
	$(MLC) $(MLFLAGS) -c $<

%: %.$(CMO)
	$(MLC) -o $@ $(wordlist 3,$(words $+),$+)

empty:=
space:= $(empty) $(empty)
PWD := $(subst $(space),\$(space),$(shell pwd))# fix bug with ocamlc -a

all: $(CAMLP4_TARGETS)

pa_Num.cmo: pa_Num.ml
	$(MLC_BIN) -pp "$(CAMLP4)" -I Tools -I $(MLCPPLIB) -c $<
pa_Num.cmx: pa_Num.ml
	$(MLC_NAT) -pp "$(CAMLP4)" -I Tools -I $(MLCPPLIB) -c $<

pa_extensions.cmo: pa_extensions.ml
	$(MLC_BIN) -pp "$(CAMLP4)" -I Tools -I $(MLCPPLIB) -c $<
pa_extensions.cmx: pa_extensions.ml
	$(MLC_NAT) -pp "$(CAMLP4)" -I Tools -I $(MLCPPLIB) -c $<

camlp4opt: pa_Num.cmx pa_extensions.cmx
	ocamlopt -linkall -I $(MLCPPLIB) -o camlp4opt odyl.cmxa camlp4.cmxa pa_r.cmx pa_rp.cmx pr_dump.cmx \
          pa_Num.cmx pa_extensions.cmx odyl.cmx

depend:

clean:
	rm -f *.cm[aixo] *.cmxa *.[ao] camlp4opt

