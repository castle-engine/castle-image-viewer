{ -*- buffer-read-only: t -*- }

{ Unit automatically generated by image_to_pas tool,
  to embed images in Pascal source code. }
unit GVIImages;

interface

uses Images;

var
  Invalid: TRGBImage;

var
  Welcome: TRGBAlphaImage;

implementation

uses SysUtils;

{ Actual image data is included from another file, with a deliberately
  non-Pascal file extension ".image_data". This way ohloh.net will
  not recognize this source code as (uncommented) Pascal source code. }
{$I gviimages.image_data}

initialization
  Invalid := TRGBImage.Create(InvalidWidth, InvalidHeight);
  Move(InvalidPixels, Invalid.RawPixels^, SizeOf(InvalidPixels));
  Welcome := TRGBAlphaImage.Create(WelcomeWidth, WelcomeHeight);
  Move(WelcomePixels, Welcome.RawPixels^, SizeOf(WelcomePixels));
finalization
  FreeAndNil(Invalid);
  FreeAndNil(Welcome);
end.