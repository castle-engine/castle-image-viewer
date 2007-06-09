.PHONY: all clean

ALL_UNITS=../imageinvalid.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../imageinvalid.pas: invalid.png
	imageToPas invalid.png > ../imageinvalid.pas
