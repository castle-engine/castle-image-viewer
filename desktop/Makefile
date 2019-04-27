APP_NAME := castle-view-image

ALL_IMAGES := $(APP_NAME).icns

.PHONY: all_images
all_images: $(ALL_IMAGES)

$(APP_NAME).icns: $(APP_NAME).svg
	../../scripts/svg_to_icns.sh $< $@
