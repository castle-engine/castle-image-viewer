#!/bin/sh
set -eu

if [ -d ../castle_game_engine/ ]; then
  cd ../castle_game_engine/
  # Force rebuilding CastleWindow unit with proper backend.
  make --quiet clean-window
  cd ../glviewimage/
fi

# Compile binary using castle-engine,
# this is good for Windows to include the icon/versioninfo/manfest in resources.
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
