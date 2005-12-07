
UNICODE_MOD = Unicode/Types Unicode/Charmap Unicode/UChar Unicode/UString Unicode/SymbolTable
UNICODE_ML  = $(addsuffix .ml,$(UNICODE_MOD))
UNICODE_MLI = $(addsuffix .mli,$(UNICODE_MOD))
UNICODE_CMO = $(addsuffix .$(CMO),$(UNICODE_MOD))
UNICODE_C   = Unicode/Tables.c
UNICODE_O   = Unicode/Tables.o

ifdef NATIVE
  MLFLAGS_Unicode_ = #-for-pack Unicode
else
  MLFLAGS_Unicode_ =
endif

DATAFILE = data/UnicodeData.txt

Unicode/GenUnicodeTable.$(CMO): Unicode/GenUnicodeTable.ml Tools/XNum.cmi Tools/ListBuilder.cmi

Unicode/GenUnicodeTable: Tools/Tools.$(CMA) Unicode/GenUnicodeTable.$(CMO)
	$(LINK_ML)

Unicode/Tables.c: Unicode/Tables.c.top Unicode/Tables.c.bot Unicode/GenUnicodeTable
	@if [ -e $(DATAFILE) ]; then \
	echo "Generating unicode table ..."; \
	(cat Unicode/Tables.c.top; \
	 Unicode/GenUnicodeTable <$(DATAFILE); \
	 cat Unicode/Tables.c.bot) >Unicode/Tables.c; \
	else \
	  if [ -e $@ ]; then \
	    touch $@; \
	  else \
	    echo "$(DATAFILE) not found. Please download it from ftp.unicode.org."; \
	    exit 1; \
	  fi; \
	fi

Unicode/Tables.o: Unicode/Tables.c
	$(MLC) -c -o $@ $^
	mv Tables.o $@  # BUG: ocaml creates the object file in the current directory

Unicode/Unicode.$(CMO): $(UNICODE_CMO) $(UNICODE_O)
	$(MLC) -pack $(DEBUGFLAGS) -I Unicode -o $@ $(CUSTOM) $^

Unicode/Unicode.cmi Unicode/Unicode.o: Unicode/Unicode.$(CMO)
	touch $@

Unicode/Unicode.$(CMA): Unicode/Unicode.$(CMO) $(UNICODE_O)
	$(MLC) -a -o $@ $(CUSTOM) $^

Unicode/Unicode.a: Unicode/Unicode.$(CMA)
	touch $@

ifdef NATIVE
  UNICODE_INSTALL = Unicode/Unicode.$(CMA) Unicode/Unicode.cmi Unicode/Unicode.a
else
  UNICODE_INSTALL = Unicode/Unicode.$(CMA) Unicode/Unicode.cmi
endif

lib/Unicode.$(CMA) lib/Unicode.cmi: $(UNICODE_INSTALL)
	cp -f $^ lib

-include Unicode/Makefile.depend

Unicode/depend:
	$(ML_DEPEND) $(UNICODE_MLI) $(UNICODE_ML) >Unicode/Makefile.depend

# vim:set ft=make:
