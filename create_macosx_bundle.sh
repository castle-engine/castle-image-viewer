#!/bin/bash

../scripts/create_macosx_bundle.sh glViewImage glViewImage desktop/glViewImage.icns \
  "`output_macosx_document_types`"

cp /sw/lib/libpng14.14.dylib glViewImage.app/Contents/MacOS/libpng.dylib
