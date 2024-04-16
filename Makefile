.PHONY: compile
compile:
	./compile.sh

.PHONY: clean
clean:
	castle-engine clean
	cd macosx/utils && castle-engine clean
	rm -f castle-image-viewer castle-image-viewer.exe
	rm -Rf castle-image-viewer.app macosx/castle-image-viewer.app .DS_Store backup
