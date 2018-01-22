#!/bin/bash
set -euo pipefail

# Compile for Mac OS X (using CGE alternative_castle_window_based_on_lcl package),
# create Mac OS X bundle (.app),
# create dmg (disk image) file to distribute.


. ../../cge-scripts/create_macosx_bundle.sh

make -C ../ clean

# compile utils/output_macosx_document_types
echo '--------------------- Compiling utils  --------------------'
pushd .
cd utils/
castle-engine compile
popd

# compile glViewImage for Mac OS X
echo '--------------------- Compiling glViewImage  --------------------'
temporary_change_lpi_to_alternative_castle_window_based_on_lcl ../glViewImage.lpi
lazbuild ../glViewImage.lpi

# Fail if utils/output_macosx_document_types is missing
utils/output_macosx_document_types > /tmp/document_types.txt

create_bundle glViewImage ../glViewImage ../desktop/glViewImage.icns \
  "`cat /tmp/document_types.txt`"

# We used to add now libpng from Fink, but
# - we don't use Fink anymore
# - we don't need libpng anymore, FpImage has internal png reader

create_dmg glViewImage

finish_macosx_pack
