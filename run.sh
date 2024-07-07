#!/bin/bash
set -e

# Include additional directory in LD_LIBRARY_PATH, to find libtiff
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:libraries/x86_64-linux/"
./castle-image-viewer "$@"
