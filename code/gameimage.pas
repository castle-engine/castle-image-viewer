{
  Copyright 2003-2024 Michalis Kamburelis.

  This file is part of "castle-image-viewer".

  "castle-image-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-image-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-image-viewer"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Manage the loaded image of castle-image-viewer. }
unit GameImage;

interface

uses CastleGLUtils, SysUtils, CastleUtils, CastleImages, Classes,
  CastleClassUtils, CastleMessages, CastleWindow, CastleGLImages,
  CastleInternalCompositeImage, CastleWindowRecentFiles;

var
  { The currently loaded image, as TCastleImage and TDrawableImage.
    The TDrawableImage is always connected with the current TCastleImage through
    @link(TDrawableImage.Image).

    This is always initialized (even when not @link(IsImageValid)),
    because we show ImageInvalid in case loading failed.
  }
  Image: TCastleImage;
  DrawableImage: TDrawableImage;

  { Currently loaded image URL. This is not valid when not IsImageValid. }
  ImageURL: string;

  { Whether we have successfully loaded the image.
    When this is false, ImageURL should not be read (it has undefined value). }
  IsImageValid: boolean = false;

  { If you loaded Composite image, then CompositeImage <> nil and
    CompositeImageIndex is > 0 and Image is just = CompositeImage.Images[CompositeImageIndex]. }
  CompositeImage: TCompositeImage;
  CompositeImageIndex: Integer;

  SmoothScaling: boolean = true;

{ Load image from URL. The image may be composite, in which case CompositeImage
  will be loaded too, otherwise CompositeImage is left @nil.

  If (for any reason) loading of image from URL fails,
  then we will
  - replace current image with special InvalidImage,
  - show failure message using MessageOK,
  - and no exception will be raised outside of this procedure. }
procedure CreateImage(const URL: string); overload;

{ Load a ready TCastleImage instance. It becomes owned by this unit
  (will be freed by this unit automatically). }
procedure CreateImage(const NewImage: TCastleImage;
  const NewImageURL: string); overload;

{ Load a special "invalid" image.
  ErrorURL is used for Application.MainWindow.Caption suffix,
  so pass here an image URL that couldn't be loaded. }
procedure CreateImageInvalid(const ErrorURL: string);

{ Change CompositeImageIndex.
  Call this only if current image is composite (CompositeImage <> nil) and NewIndex
  is valid (NewIndex < CompositeImages.ImagesCount). }
procedure ChangeCompositeImageIndex(NewIndex: Cardinal);

{ Call after directly changing the Image contents. }
procedure ImageChanged;

var
  { CreateImage will add to this. }
  RecentMenu: TWindowRecentFiles;

const
  SWelcomeImageUrl = '<Welcome image>';

implementation

uses GameEmbeddedImages, CastleURIUtils;

{ Update Application.MainWindow.Caption,
  reflecting IsImageValid, ImageURL, CompositeImageIndex. }
procedure UpdateCaption;
var
  S: string;
begin
  if IsImageValid then
  begin
    S := URICaption(ImageURL);
    if CompositeImage <> nil then
      S := S + Format(' (Composite subimage: %d)', [CompositeImageIndex]);
  end else
    S := '<error: ' + URIDisplay(ImageURL) + '>';

  S := S + ' - castle-image-viewer';

  Assert(Application.MainWindow <> nil);
  Application.MainWindow.Caption := S;
end;

{ Destroy the loaded image.
  It is valid, and silently ignored, to call DestroyImage when the image is already destroyed.
  This is automatically called in finalization of this unit. }
procedure DestroyImage;
begin
  FreeAndNil(DrawableImage);

  if CompositeImage <> nil then
  begin
    Image := nil; { it will be freed as part of CompositeImage }
    FreeAndNil(CompositeImage);
  end else
    FreeAndNil(Image);

  IsImageValid := false;
end;

procedure CreateImage(const URL: string);
var
  NewComposite: TCompositeImage;
begin
  DestroyImage;

  try
    if TCompositeImage.MatchesURL(URL) then
    begin
      NewComposite := TCompositeImage.Create;
      try
        NewComposite.LoadFromFile(URL);
        NewComposite.Flatten3d;
        NewComposite.DecompressTexture;
      except
        FreeAndNil(NewComposite);
        raise;
      end;

      CompositeImage := NewComposite;
      CompositeImageIndex := 0;
      if CompositeImage.Images[0] is TCastleImage then
        Image := TCastleImage(CompositeImage.Images[0]) else
        raise Exception.Create('castle-image-viewer cannot display compressed textures from composite (DDS, KTX..) image');
    end else
    begin
      CompositeImage := nil;
      CompositeImageIndex := -1;
      Image := LoadImage(URL, PixelsImageClasses);
    end;

    { If above went without exceptions, finish loading and add to RecentMenu. }
    ImageURL := URL;
    IsImageValid := true;
    UpdateCaption;
    RecentMenu.Add(URL);
  except
    on E: Exception do
    begin
      CreateImageInvalid(URL);
      MessageOK(Application.MainWindow, ExceptMessage(E));
    end;
  end;

  DrawableImage := TDrawableImage.Create(Image, SmoothScaling, false);
end;

procedure CreateImage(const NewImage: TCastleImage; const NewImageURL: string);
begin
  DestroyImage;

  CompositeImage := nil;
  CompositeImageIndex := -1;
  Image := NewImage;
  ImageURL := NewImageURL;
  IsImageValid := true;
  UpdateCaption;

  DrawableImage := TDrawableImage.Create(Image, SmoothScaling, false);
end;

procedure CreateImageInvalid(const ErrorURL: string);
begin
  DestroyImage;

  CompositeImage := nil;
  CompositeImageIndex := -1;
  Image := Invalid.MakeCopy;
  ImageURL := ErrorURL;
  IsImageValid := false;
  UpdateCaption;

  DrawableImage := TDrawableImage.Create(Image, SmoothScaling, false);
end;

procedure ChangeCompositeImageIndex(NewIndex: Cardinal);
begin
  Assert(CompositeImage <> nil);
  Assert(NewIndex < Cardinal(CompositeImage.Images.Count));
  Assert(IsImageValid); { IsImageValid = always true when CompositeImage <> nil }

  CompositeImageIndex := NewIndex;
  Image := CompositeImage.Images[NewIndex] as TCastleImage;

  { usually, one should destroy DrawableImage before changing Image reference,
    because DrawableImage may exist only as long as Image reference is valid.
    But in this case this is simpler and will work too, since previous Image reference
    remains valid, as long as CompositeImage is not freed.
    So it's enough to call ImageChanged after changing Image reference. }

  ImageChanged;

  UpdateCaption;
end;

procedure ImageChanged;
begin
  DrawableImage.Load(Image);
end;

initialization
finalization
  DestroyImage;
end.
