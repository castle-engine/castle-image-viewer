#!/bin/bash
set -eu

# Create Mac OS X bundle, and then dmg (disk image) file to distribute glViewImage.
# Compile the glViewImage binary before calling this.

. ../../cge-scripts/create_macosx_bundle.sh

create_bundle glViewImage ../glViewImage ../desktop/glViewImage.icns \
  "`../utils/output_macosx_document_types`"

# add libraries from fink
cd glViewImage.app/Contents/MacOS/

# TODO: for now, I don't have fink available.
# Fortunately, lack of png is harmless now, we read PNG without it.
#
# cp_fink_lib libpng14.14.dylib

check_libs_not_depending_on_fink glViewImage
cd ../../../

make -f ../../cge-scripts/macosx_dmg.makefile NAME=glViewImage

echo 'Done OK'
