
manual: Examples/manual.dvi

Examples/manual.dvi: ant Examples/manual.ant Examples/manual.sty
	cd Examples; ../ant $(ANTFLAGS) manual.ant

Examples/manual.ps: Examples/manual.dvi
	cd Examples; dvips -o manual.ps manual.dvi

Examples/manual.pdf: ant Examples/manual.ant Examples/manual.sty
	cd Examples; ../ant --format=pdf $(ANTFLAGS) manual.ant

debug-manual: ant Examples/manual.ant Examples/manual.sty
	cd Examples; ocamldebug $(addprefix -I ../,Tools $(DIRECTORIES)) ../ant $(ANTFLAGS) manual.ant

debug-minimal: ant
	cd Examples; ocamldebug $(addprefix -I ../,Tools $(DIRECTORIES)) ../ant $(ANTFLAGS) minimal.ant

Examples/test.dvi: Examples/test.ant
	./ant Examples/test.ant

Examples/test-TeX.dvi: Examples/test-TeX.tex
	cd Examples; tex test-TeX.tex

Examples/clean:
	rm -f Examples/*.{log,aux,refdb}

Examples/distclean: Examples/clean
	rm -f Examples/*.{dvi,pdf,ps,svg}

Examples/depend:

# vim:set ft=make:
