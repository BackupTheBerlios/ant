
CAMLIMAGES_CMO  = camlimages.$(CMO) region.$(CMO) tmpfile.$(CMO) bitmap.$(CMO) blend.$(CMO) genimage.$(CMO) \
                  cmyk32.$(CMO) mstring.$(CMO) color.$(CMO) rgba32.$(CMO) rgb24.$(CMO) index16.$(CMO) \
                  index8.$(CMO) images.$(CMO) ftlow.$(CMO) freetype.$(CMO) jis_table.$(CMO) \
                  jis_unicode.$(CMO) fttext.$(CMO) geometry.$(CMO) gif.$(CMO) \
                  bmp.$(CMO) jpeg.$(CMO) png.$(CMO) ppm.$(CMO) ps.$(CMO) reduce.$(CMO) tiff.$(CMO) \
                  xpm.$(CMO) xvthumb.$(CMO)
CAMLIMAGES_O    = tiffread.o tiffwrite.o xpmread.o xpmwrite.o ftintf.o gifread.o gifwrite.o jpegread.o \
                  jpegwrite.o pngread.o pngwrite.o
CAMLIMAGES_LIBS = -lgif -ljpeg -ltiff -lfreetype -lpng -lz -lXpm

Images/CamlImages.$(CMO):
	$(MLC) -pack -o $@ $(CUSTOM) $(addprefix camlimages/, $(CAMLIMAGES_CMO))

Images/CamlImages.cmi: Images/CamlImages.$(CMO)
	touch $@

Images/CamlImages.$(CMA): Images/CamlImages.$(CMO) \
                                      $(addprefix $(CAML_LIB_DIR)/camlimages/, $(CAMLIMAGES_O))
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
