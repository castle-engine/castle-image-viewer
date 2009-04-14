.PHONY: all clean

ALL_UNITS=../imageinvalid.pas ../imagewelcome.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../imageinvalid.pas: invalid.png
	image_to_pas invalid.png > ../imageinvalid.pas

../imagewelcome.pas: welcome.png
	image_to_pas welcome.png > ../imagewelcome.pas
