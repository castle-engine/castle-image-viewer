.PHONY: compile
compile:
	./compile.sh

.PHONY: clean
clean:
	castle-engine clean
	cd macosx/utils && castle-engine clean
	rm -f castle-view-image castle-view-image.exe
	rm -Rf castle-view-image.app macosx/castle-view-image.app .DS_Store backup
