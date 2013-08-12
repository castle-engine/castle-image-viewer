#!/bin/bash
set -eu

. ../../scripts/create_macosx_bundle.sh

create_bundle glViewImage ../glViewImage ../desktop/glViewImage.icns \
  "`output_macosx_document_types`"

# add libraries from fink
cd glViewImage.app/Contents/MacOS/
cp_fink_lib libpng14.14.dylib
check_libs_not_depending_on_fink glViewImage
