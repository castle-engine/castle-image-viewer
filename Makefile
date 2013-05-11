.PHONY: compile
compile:
	./compile.sh

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	rm -f glViewImage glViewImage.exe
	rm -Rf glViewImage.app
