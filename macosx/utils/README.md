# Generate `<associate_document_types>` to associate castle-image-viewer with all supported image types

When we add / remove supported image formats in _Castle Game Engine_,
run this utility and insert the resulting `<associate_document_types>`
into the `../../CastleEngineManifest.xml`.

This way _Castle Image Viewer_ will be associated with all supported image types,
so that double-clicking such files will open them in _Castle Image Viewer_.

This is theoretically useful for all platforms (`<associate_document_types>`
is parsed on all platforms), but (for now) macOS is the only targer that
actually uses `<associate_document_types>` in manifest to associate applications
with document types.
