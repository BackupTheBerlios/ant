
CAMLIMAGES_CLIBS  = -lgif -ljpeg -ltiff -lfreetype -lpng -lz -lXpm
CAMLIMAGES_MLLIBS = $(addprefix camlimages/, ci_core.$(CMA) ci_freetype.$(CMA) ci_gif.$(CMA) ci_jpeg.$(CMA) \
                    ci_tiff.$(CMA) ci_bmp.$(CMA) ci_ppm.$(CMA) ci_png.$(CMA) ci_xvthumb.$(CMA) ci_xpm.$(CMA) ci_ps.$(CMA))
MLFLAGS_Images_ = -I +camlimages

Images/CamlImages.$(CMO): Images/CamlImages.ml

Images/CamlImages.cmi: Images/CamlImages.$(CMO)
	touch $@

Images/CamlImages.$(CMA): Images/CamlImages.$(CMO)
	$(MLC) -a -o $@ $(CUSTOM) $^

Images/CamlImages.a: Images/CamlImages.$(CMA)
	touch $@

ifdef NATIVE
  IMAGES_INSTALL = Images/CamlImages.$(CMA) Images/CamlImages.cmi Images/CamlImages.a
else
  IMAGES_INSTALL = Images/CamlImages.$(CMA) Images/CamlImages.cmi
endif

lib/CamlImages.$(CMA) lib/CamlImages.cmi: $(IMAGES_INSTALL)
	cp -f $^ lib

Images/depend:

# vim:set ft=make:
