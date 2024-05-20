# Castle Image Viewer (formerly castle-view-image, glViewImage)

Image viewer, converter and even a very limited image editor. Using _Castle Game Engine_.

- Supports many common image formats: PNG, JPEG, GIF, TGA, XPM, PSD, PCX, PNM, PBM, PGM, PPM, BMP.

- Supports also some "exotic" file formats that are useful for game developers: [KTX](https://castle-engine.io/ktx) and [DDS](https://castle-engine.io/dds) (advanced formats for textures) and RGBE (simple HDR format).

- Browse all images in a directory by easy key shortcuts.

- Scale image, test is it tileable, see alpha channel, other image "investigation" features.

- Perform [alpha bleeding](https://castle-engine.io/manual_alpha_bleeding.php) and other edits (grayscale, mirror, resize).

Complete documentation on https://castle-engine.io/castle-image-viewer .

Using Castle Game Engine, see https://castle-engine.io/ .

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle_image_viewer.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

## License

GNU GPL >= 2.
