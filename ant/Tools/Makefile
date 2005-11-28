
#NATIVE=1
#NATIVE_CAMLP4=

#MATH_LIB=Gmp
#MATH_LIB=Num
#MATH_LIB=Float

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
  CAMLP4         = camlp4r      # preprocessor for camlp4 syntax extensions
  MLPP           = camlp4opt    # preprocessor for normal ml files
  MLPP_PA        =
  CAMLP4_TARGETS = camlp4opt    # additional targets to make
else
  CAMLP4         = camlp4r      # preprocessor for camlp4 syntax extensions
  MLPP           = camlp4r      # preprocessor for normal ml files
  MLPP_PA        = ./pa_Num.cmo ./pa_extensions.cmo
  CAMLP4_TARGETS = pa_Num.cmo pa_extensions.cmo      # additional targets to make
endif

ifndef MATH_LIB
  MATH_LIB=Float
endif

MLCPPLIB  := $(shell $(CAMLP4) -where)
ML_DEPEND = ocamldep -pp "$(MLPP) $(MLPP_PA)" $(DEPFLAGS)

GMP_LIB   := $(shell $(MLC) -where)/gmp

MLFLAGS  = -rectypes -pp "$(MLPP) $(MLPP_PA)" -I $(GMP_LIB) $(DEBUGFLAGS)

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

PWD := $(shell pwd)# fix bug with ocamlc -a

TOOLS     = NumGmp NumNum NumFloat Option XList XNum XString Zlib IO LinForm Constraint PlanePartition ListBuilder ShortestPath
TOOLS_ML  = $(addsuffix .ml,$(TOOLS))
TOOLS_MLI = $(addsuffix .mli,$(TOOLS))
TOOLS_CMI = $(addsuffix .cmi,$(TOOLS))
TOOLS_CMO = $(addsuffix .$(CMO),$(TOOLS))
TOOLS_C   = zlib-stubs.c
TOOLS_O   = $(PWD)/zlib-stubs.o  # fix bug with ocamlc -a

all: XNum.ml $(CAMLP4_TARGETS) $(TOOLS_CMI) Tools.$(CMA)

$(PWD)/zlib-stubs.o: zlib-stubs.c
	$(MLC) -o $@ $^

Tools.$(CMA): $(TOOLS_O) $(TOOLS_CMO)
	$(MLC) -a -o $@ $(CUSTOM) $^

pa_Num.cmo: pa_Num.ml
	$(MLC_BIN) -pp "$(CAMLP4) pa_extend.cmo q_MLast.cmo" -I Tools -I $(MLCPPLIB) -c $<
pa_Num.cmx: pa_Num.ml
	$(MLC_NAT) -pp "$(CAMLP4) pa_extend.cmo q_MLast.cmo" -I Tools -I $(MLCPPLIB) -c $<

pa_extensions.cmo: pa_extensions.ml
	$(MLC_BIN) -pp "$(CAMLP4) pa_extend.cmo q_MLast.cmo" -I Tools -I $(MLCPPLIB) -c $<
pa_extensions.cmx: pa_extensions.ml
	$(MLC_NAT) -pp "$(CAMLP4) pa_extend.cmo q_MLast.cmo" -I Tools -I $(MLCPPLIB) -c $<

camlp4opt: pa_Num.cmx pa_extensions.cmx
	ocamlopt -linkall -I $(MLCPPLIB) -o camlp4opt odyl.cmxa camlp4.cmxa pa_r.cmx pa_rp.cmx pr_dump.cmx \
          pa_Num.cmx pa_extensions.cmx odyl.cmx

XNum.ml:
	echo "include Num$(MATH_LIB);" >$@

XList.mli: XList.ml
	$(MLC) $(MLFLAGS) -i $< >xList.tmp.mli
	camlp4o pr_r.cmo -I $(MLCPPLIB) xList.tmp.mli >$@
	rm xList.tmp.mli
XNum.mli: XNum.ml Num$(MATH_LIB).mli
	$(MLC) $(MLFLAGS) -i $< >xNum.tmp.mli
	camlp4o pr_r.cmo -I $(MLCPPLIB) xNum.tmp.mli >$@
	rm xNum.tmp.mli
XString.mli: XString.ml
	$(MLC) $(MLFLAGS) -i $< >xString.tmp.mli
	camlp4o pr_r.cmo -I $(MLCPPLIB) xString.tmp.mli >$@
	rm xString.tmp.mli

XNum.$(CMO): XNum.ml XNum.cmi Num$(MATH_LIB).cmi Num$(MATH_LIB).ml Num$(MATH_LIB).mli

-include Makefile.depend

depend: $(CAMLP4_TARGETS)
	$(ML_DEPEND) $(TOOLS_MLI) $(TOOLS_ML) >Makefile.depend

clean:
	rm -f XNum.ml XList.mli XNum.mli XString.mli *.cm[aixo] *.cmxa *.[ao] Makefile.depend camlp4opt

