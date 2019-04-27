#!/bin/sh
set -eu

# Compile binary using castle-engine build tool.
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
