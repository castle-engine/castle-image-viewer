#!/bin/sh
set -eu

cd ../castle_game_engine/
# Force rebuilding CastleWindow unit with proper backend.
make --quiet clean-window

# Compile binary using castle-engine,
# this is good for Windows to include the icon/versioninfo/manfest in resources.
cd ../glviewimage/
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
