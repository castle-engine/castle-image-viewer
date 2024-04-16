APP_NAME := castle-image-viewer

ALL_IMAGES := $(APP_NAME).icns

.PHONY: all_images
all_images: $(ALL_IMAGES)

$(APP_NAME).icns: $(APP_NAME).svg
	../../scripts/svg_to_icns.sh $< $@
