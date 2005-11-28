
# packages to be compiled into ant:
PACKAGES_USED = #german advi

DIRECTORIES   = Images Unicode Runtime Typesetting Engine VM Markup Main Packages Util Examples

HYPHEN_PATTERNS = british us #deutsch-traditional deutsch-new français

include Config.mk

all: Tools/all make-utils VM/ali ant

-include $(addsuffix /Makefile.inc,$(DIRECTORIES))

PACKAGES_USED_MOD = $(addprefix Packages/,$(PACKAGES_USED))

MODULES    = $(PACKAGES_USED_MOD) $(MAIN_MOD)
SOURCES    = $(addsuffix .ml,$(MODULES) $(RUNTIME_MOD))
ISOURCES   = $(addsuffix .mli,$(MODULES) $(RUNTIME_MOD))
INTERFACES = $(addsuffix .cmi,$(MODULES))
OBJECTS    = $(addsuffix .$(CMO),$(MODULES))
LIBRARIES  = Tools/Tools.$(CMA) lib/CamlImages.$(CMA) lib/Unicode.$(CMA) lib/Runtime.$(CMA) \
             lib/VM.$(CMO) lib/Typesetting.$(CMO) lib/Engine.$(CMO) lib/Markup.$(CMO)

ant: $(LIBRARIES) $(OBJECTS)
	$(LINK_ML)

make-utils: $(UTILS)

MAKE_TOOLS = $(MAKE) "NATIVE=$(NATIVE)" "NATIVE_CAMLP4=$(NATIVE_CAMLP4)" "MATH_LIB=$(MATH_LIB)"
Tools/all Tools/Tools.$(CMA) $(PARSERS):
	cd Tools; $(MAKE_TOOLS)

depend:
	cd Tools; $(MAKE_TOOLS) depend
	touch lib/Images.ml lib/Images.mli lib/Unicode.ml lib/Unicode.mli lib/Runtime.ml lib/Runtime.mli \
	      lib/Typesetting.ml lib/Typesetting.mli lib/VM.ml lib/VM.mli lib/Engine.ml lib/Engine.mli \
	      lib/Markup.ml lib/Markup.mli
	$(MAKE) $(addsuffix /depend,$(DIRECTORIES))
	rm lib/Images.ml lib/Images.mli lib/Unicode.ml lib/Unicode.mli lib/Runtime.ml lib/Runtime.mli \
	   lib/Typesetting.ml lib/Typesetting.mli lib/VM.ml lib/VM.mli lib/Engine.ml lib/Engine.mli \
	   lib/Markup.ml lib/Markup.mli

tags: $(SOURCES)
	$(OTAGS) $(DIRECTORIES)
#	sort tags -o tags

clean: Examples/clean
	cd Tools; $(MAKE_TOOLS) clean
	rm -f Makefile.depend lib/*
	for d in . $(DIRECTORIES); \
	do \
	  rm -f $$d/*.{o,a,cm[aiox],cmxa} $$d/Makefile.depend; \
	done

distclean: clean Examples/distclean
	rm -f ant Unicode/GenUnicodeTable Util/GenHyphTable Engine/HyphenTable.ml VM/ali */.viminfo

DATE := $(shell date +"%Y%m%d")

ARCHIVE_FILES =  ant/Config.mk ant/Makefile ant/README ant/data/hyphen ant/Engine \
                 ant/Examples/*.{ant,sty,tex} ant/Examples/Makefile.inc \
                 ant/lib ant/Images ant/Main ant/Markup ant/Packages \
                 ant/Unicode ant/Runtime ant/Tools ant/Typesetting ant/Util ant/VM
archive: distclean
	cd ..; tar -cvf ant-$(DATE).tar $(ARCHIVE_FILES)
	bzip2 -vf9 ../ant-$(DATE).tar

# vim:set fenc=utf8:
