.PHONY: all clean

ALL_UNITS=../gviimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../gviimages.pas: invalid.png welcome.png
	image2pascal --output .. GVIImages $^
