.PHONY: compile
compile:
	./compile.sh

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	castle-engine clean
	cd macosx/utils && castle-engine clean
	rm -f glViewImage glViewImage.exe
	rm -Rf glViewImage.app macosx/glViewImage.app .DS_Store backup
