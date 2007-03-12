
NATIVE=1
NATIVE_CAMLP4=1

#MATH_LIB=Gmp
#MATH_LIB=Num
MATH_LIB=Float

OPT = .opt

# You may need to change the following parameters.

# paths to various parts of the TeX tree
HYPHENDIR      := $(shell kpsewhich '--expand-path=$$TEXMF/tex/generic/hyphen')
SELFAUTOLOC    := $(shell kpsewhich '--expand-var=$$SELFAUTOLOC')
SELFAUTODIR    := $(shell kpsewhich '--expand-var=$$SELFAUTODIR')
SELFAUTOPARENT := $(shell kpsewhich '--expand-var=$$SELFAUTOPARENT')

# compile flags for C files
KPATHSEA_LFLAGS = -cclib -L$(SELFAUTOPARENT)/lib
KPATHSEA_CFLAGS = -ccopt -I$(SELFAUTOPARENT)/include
SELFAUTO_FLAGS  = -ccopt -DSELFAUTOLOC='\"$(SELFAUTOLOC)\"' \
                  -ccopt -DSELFAUTODIR='\"$(SELFAUTODIR)\"' \
                  -ccopt -DSELFAUTOPARENT='\"$(SELFAUTOPARENT)\"'
FREETYPE_FLAGS := $(addprefix -ccopt ,$(shell freetype-config --cflags))
X11_LFLAGS      = -cclib -L/usr/X11/lib

# select compiler

MLC_BIN = ocamlc$(OPT)
MLC_NAT = ocamlopt$(OPT)

ifdef NATIVE
  MLC = $(MLC_NAT)# -inline 0 -p # for profiling
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

# select preprocessor

ifdef NATIVE_CAMLP4
  PARSERS =
  MLPP    = ./Tools/camlp4opt
else
  PARSERS = ./Tools/pa_Num.cmo ./Tools/pa_extensions.cmo
  MLPP    = "camlp4r $(PARSERS)"
endif

CAML_LIB_DIR := $(shell $(MLC) -where)

CAMLIMAGES_DIR = $(CAML_LIB_DIR)/camlimages

ifndef MATH_LIB
  MATH_LIB=Float
endif

NUMLIB =

ifeq "$(MATH_LIB)" "Gmp"
  NUMLIB = gmp/gmp.$(CMA)
endif
ifeq "$(MATH_LIB)" "Num"
  NUMLIB = nums.$(CMA)
endif

INCFLAGS   = -I . -I Tools $(addprefix -I ,$(dir $@)) -I lib #$(addprefix -I ,$(DIRECTORIES))
WARNFLAGS  = -w Aes -warn-error P
MLFLAGS    = -rectypes $(INCFLAGS) $(WARNFLAGS) $(DEBUGFLAGS)
MLLIBS     = $(NUMLIB) unix.$(CMA) -I $(CAMLIMAGES_DIR) $(CAMLIMAGES_MLLIBS) $(KPATHSEA_LFLAGS) $(X11_LFLAGS) \
             -cclib -lkpathsea -cclib -lfreetype -cclib -lz $(addprefix -cclib , $(CAMLIMAGES_CLIBS)) -cclib -lstdc++

ML_DEPEND = ocamldep -pp $(MLPP) $(INCFLAGS) $(DEPFLAGS)

LINK_ML  = $(MLC) -o $@ $(DEBUGFLAGS) $(MLLIBS) $^
LINK_LIB = $(MLC) -a -o $@ $^

OTAGS = otags -r -vi -o $@ -pa pa_r.cmo -pa ./Tools/pa_Num.cmo -pa ./Tools/pa_extensions.cmo

# rules

%.cmi: %.mli $(PARSERS)
	$(MLC) -pp $(MLPP) $(MLFLAGS) $(value MLFLAGS_$(subst /,_,$(dir $@))) -c $<

%.cmo: %.ml $(PARSERS)
	$(MLC_BIN) -pp $(MLPP) $(MLFLAGS) $(value MLFLAGS_$(subst /,_,$(dir $@))) -c $<

%.cmx: %.ml $(PARSERS)
	$(MLC_NAT) -pp $(MLPP) $(MLFLAGS) $(value MLFLAGS_$(subst /,_,$(dir $@))) -c $<

#%.$(CMO) %.cmi: %.ml $(PARSERS)
#	$(MLC) -pp $(MLPP) $(MLFLAGS) -c $<

%: %.$(CMO)
	$(MLC) -o $@ $(MLLIBS) $(wordlist 3,$(words $+),$+)

