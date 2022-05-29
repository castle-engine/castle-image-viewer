#!/bin/bash
set -euo pipefail

# -----------------------------------------------------------------------------
# Compile for macOS, create macOS app bundle (.app).
#
# Note: you need to manually synchronize CastleEngineManifest.xml to reflect available
# image file formats for CGE.
# Use utils/output_document_types for this.
# This is actualy a cross-platform need, but for now macOS is the only desktop that
# actually uses <associate_document_types> in manifest.
# -----------------------------------------------------------------------------

cd ../
castle-engine package ${CASTLE_ENGINE_TOOL_OPTIONS:-}
