.PHONY: all clean

ALL_UNITS=../embeddedimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../embeddedimages.pas: invalid.png welcome.png
	image-to-pascal --output .. EmbeddedImages $^
