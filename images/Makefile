.PHONY: all clean

ALL_UNITS=../embeddedimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../embeddedimages.pas: invalid.png welcome.png arrow-left-circle.png arrow-right-circle.png arrow-down-circle.png arrow-up-circle.png
	image-to-pascal --output .. EmbeddedImages $^
