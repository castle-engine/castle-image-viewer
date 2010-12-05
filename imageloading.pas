{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "glViewImage".

  "glViewImage" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "glViewImage" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "glViewImage"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

unit ImageLoading;

{ Basic image loading funcs for glViewImage.

  Every variable below is read-only from outside of this module.

  Cala komplikacja podzialu na NonGL i GL powstala dlatego ze na poczatku
  programu glViewImage potrzebujemy jednak zaladowac Image przed utworzeniem
  sobie kontekstu OpenGL'a (bo chcemy rozmiar okienka uzaleznic od
  zaladowanego Image).
}

interface

uses GL, GLU, GLExt, KambiGLUtils, SysUtils, KambiUtils, Images, Math, Classes,
  KambiClassUtils, GLWinMessages, GLWindow, GLImages, DDS,
  GLWindowRecentFiles;

{ Below is "image state". The idea is that for the whole time of a program
  this module manages one image. An image is:

  ------------------------------------------------------------
  1. Things related to image not connected with the OpenGL context.
     This means that such things can be created/destroyed without OpenGL context
     and you can freely switch from one gl context to another without
     having to change these things. }
var
  Image: TImage;

  { ImageExpand to ten sam image co Image ale rozszerzony o jedna kolumne wiecej
    i jeden wiersz wiecej (ostatnia kolumna i ostatni wiersz sa powielone na koncu).
    Przydaje sie gdy chcemy narysowac image tiled jeden obok drugiego.
    Wtedy gdy rysujemy OpenGL'em Image o rozmiarze Width moze byc niekiedy
    narysowany na Width-1 pixlach gdy mamy pixel zoom <> 1 -> wtedy
    czasem
    Round(image.size*zoomX) > Round((moveX+Width)*zoomX) - Round(moveX*zoomX)
    (bez tych Roundow, matematycznie, powinna byc zawsze rownosc;
    ale wlasnie tak nie zawsze jest na skutek tych Roundow). W rezultacie
    obrazek zajmuje o kolumne/wiersz mniej i w rezultacie pojawia sie czarna
    rysa na ekranie pomiedzy dwoma kolejnymi rysunkami.

    Poniewaz rysa ma zawsze szerokosc 1 pixla (tylko taka roznica moze
    wystapic w powyzszej nierownosci) wiec rozwiazaniem jakiego uzywamy
    jest renderowac obrazek o jedna kolumne i wiersz szerszy - oczywiscie
    o ostatnia kolumne. Jezeli rysy by tam nie bylo to ta ostatnia kolumna
    zostanie przykrywa przez poczatek rysunku obok, wpp. ostatnia kolumna
    zamaluje nam miejsce gdzie bylaby rysa. }
  ImageExpand: TImage;

  { This is not valid when not IsImageValid }
  ImageFileName: string;

  { Even when this is false, if we are after some successful
    CreateNonGLImage* and before DestroyNonGLImage,
    Image and ImageExpand are initialized (to copies of ImageInvalid).
    When this is false, ImageFileName should not be read (it has undefined value) }
  IsImageValid: boolean = false;

  { If you loaded DDS image, then DDSImage <> nil and
    DDSImageIndex is > 0 and Image is just = DDSImage.Images[DDSImageIndex]. }
  DDSImage: TDDSImage;
  DDSImageIndex: Integer;

{ Note: CreateNonGLImage first automatically calls DestroyNonGLImage }
procedure CreateNonGLImage(Window: TGLWindow; const fname: string); overload;

{ About this version of CreateNonGLImage: you can give already loaded
  image. After calling this CreateNonGLImage you must STOP managing
  this NewImage - it will be managed (and freed) by this unit. }
procedure CreateNonGLImage(Window: TGLWindow; const NewImage: TImage;
  const NewImageFileName: string); overload;

{ ErrorFileName is used for Window.Caption suffix,
  give here image name that can't be loaded. }
procedure CreateNonGLImageInvalid(Window: TGLWindow;
  const ErrorFileName: string);

{ It is valid NOP to call DestroyNonGLImage on already destroyed image.
  Note: DestroyNonGLImage is automatically called in finalization of this unit. }
procedure DestroyNonGLImage;

procedure RemakeImageExpand;

{ ------------------------------------------------------------
  2. Things related to image connected with OpenGL context.
     You can manipulate them (create/destroy) only when you have a valid OpenGL
     context. Moreover, between creating and destroying you MUST stay
     in the SAME OpenGL context.

     Moreover, these things may depend on other things related to image,
     those not connected with OpenGL's context.
     This means that these things MUST NOT be
     created BEFORE creating things in point 1 and MUST be freed BEFORE
     freeing things in point 1.  }
var
  dlDrawImage: TGLuint = 0;
  dlDrawImageExpand: TGLuint = 0;

{ Note: CreateGLImage first automatically calls DestroyGLImage }
procedure CreateGLImage;
{ It is valid NOP to call DestroyGLImage on already destroyed image. }
procedure DestroyGLImage;

{ ------------------------------------------------------------ }
{ Stating it shortly, CreateImage is something like calling
    DestroyGLImage;
    DestroyNonGLImage;
    CreateNonGLImage(...);
    CreateGLImage;
  so it replaces current image with given.

  But if (for any reason) loading of image from fname fails, then
  it will replace current image with special InvalidImage.
  Message about failing to load an image will be shown using
  MessageOK(Window,...) and no exception will be raised outside of this
  procedure CreateImage. }
procedure CreateImage(Window: TGLWindow; const fname: string);

{ Takes the already created Image instance, and makes it loaded.

  Just like regular CreateImage(glwindow, filename),
  only it doesn't load image from file, but takes ready
  Image instance (you should leave further freeing of this Image
  to this unit, don't mess with it yourself). }
procedure CreateImage(Window: TGLWindow; Image: TImage; const Name: string);

{ Change DDSImageIndex.

  This frees GL image, then changes NonGL image portions to point
  to new DDSImageIndex, then loads again GL image.

  When calling this, always make sure that NonGL image is already loaded,
  and it's a DDSImage (DDSImage <> nil) and NewIndex is allowed
  (NewIndex < DDSImages.ImagesCount). }
procedure ChangeDDSImageIndex(Window: TGLWindow; NewIndex: Cardinal);

var
  { CreateImage will add to this. }
  RecentMenu: TGLRecentFiles;

implementation

uses GVIImages;

{ zwraca Image ktory ma ostatnia (prawa) kolumne powtorzona i
  powtorzony ostatni (gorny) wiersz. Wiec wynik ma o jeden wieksze
  Width i Height. (prawy-gorny pixel tez jest powtorzony, konsekwentnie). }
function ImageDuplicatedLastRowCol(const Image: TImage): TImage;
var y: integer;
begin
 Result := TImageClass(Image.ClassType).Create(Image.Width+1, Image.Height+1);
 try
  for y := 0 to Image.Height-1 do
  begin
   {kopiuj wiersz + duplikuj ostatni pixel w tym wierszu}
   Move(Image.PixelPtr(0, y)^,
        Result.PixelPtr( 0, y)^,
        Result.PixelSize * Image.Width );
   Move(Image.PixelPtr(Image.Width-1, y)^,
        Result.PixelPtr( result.Width-1, y)^,
        Result.PixelSize );
  end;

  {ostatni wiersz}
  Move(Image.PixelPtr(0, Image.Height-1)^,
       Result.PixelPtr( 0, result.Height-1)^,
       Result.PixelSize * Image.Width );
  Move(Image.PixelPtr(Image.Width-1, Image.Height-1)^,
       Result.PixelPtr( result.Width-1, result.Height-1)^,
       Result.PixelSize );

 except Result.Free; raise end;
end;

procedure RemakeImageExpand;
begin
  FreeAndNil(ImageExpand);
  ImageExpand := ImageDuplicatedLastRowCol(Image);
end;

procedure UpdateCaption(Window: TGLWindow);
var
  S: string;
begin
  if IsImageValid then
  begin
    S := ExtractFileName(ImageFileName);
    if DDSImage <> nil then
      S += Format(' (DDS subimage: %d)', [DDSImageIndex]);
  end else
    S := '<error: ' + ExtractFileName(ImageFileName) + '>';

  S += ' - glViewImage';

  Window.Caption := S;
end;

procedure InternalCreateNonGLImageDDS(Window: TGLWindow; const NewImage: TDDSImage;
  const NewImageFileName: string);
begin
  DestroyNonGLImage;
  DDSImage := NewImage;
  DDSImageIndex := 0;
  if DDSImage.Images[0] is TImage then
    Image := TImage(DDSImage.Images[0]) else
    raise Exception.Create('glViewImage cannot display S3TC compressed textures from DDS');
  ImageExpand := ImageDuplicatedLastRowCol(Image);
  ImageFileName := NewImageFileName;
  IsImageValid := true;
  UpdateCaption(Window);
end;

procedure InternalCreateNonGLImage(Window: TGLWindow; const NewImage: TImage;
  const NewImageFileName: string; NewIsImageValid: boolean);
begin
  DestroyNonGLImage;
  DDSImage := nil;
  DDSImageIndex := -1;
  Image := NewImage;
  ImageExpand := ImageDuplicatedLastRowCol(Image);
  ImageFileName := NewImageFileName;
  IsImageValid := NewIsImageValid;
  UpdateCaption(Window);
end;

procedure CreateNonGLImage(Window: TGLWindow; const fname: string);
var
  NewDDS: TDDSImage;
begin
  if FileExtToImageFormatDef(ExtractFileExt(FName),
    false, false, ifBMP) = ifDDS then
  begin
    NewDDS := TDDSImage.Create;
    try
      NewDDS.LoadFromFile(FName);
      NewDDS.Flatten3d;
      NewDDS.DecompressS3TC;
    except
      FreeAndNil(NewDDS);
      raise;
    end;
    InternalCreateNonGLImageDDS(Window, NewDDS, FName);
  end else
  begin
    InternalCreateNonGLImage(Window,
      LoadImage(fname, PixelsImageClasses, []), fname, true);
  end;
  { If InternalCreateNonGLImage went without exceptions,
    add to RecentMenu. }
  RecentMenu.Add(FName);
end;

procedure CreateNonGLImage(Window: TGLWindow; const NewImage: TImage;
  const NewImageFileName: string);
begin
 InternalCreateNonGLImage(Window, NewImage, NewImageFileName, true);
end;

procedure CreateNonGLImageInvalid(Window: TGLWindow;
  const ErrorFileName: string);
begin
 InternalCreateNonGLImage(Window, Invalid.MakeCopy, ErrorFileName, false);
end;

{ Zwolnij rzeczy obrazka ktore nie zaleza od kontekstu OpenGLa.
  Czyli zwolnij rzeczy inicjowane przez InternalCreateNonGLImage*. }
procedure DestroyNonGLImage;
begin
  if DDSImage <> nil then
  begin
    Image := nil; { it will be freed as part of DDSImage }
    FreeAndNil(DDSImage);
  end else
    FreeAndNil(Image);

  FreeAndNil(ImageExpand);
  IsImageValid := false;
end;

{ wywolaj to ZAWSZE po udanym (bez wyjatkow) InternalCreateNonGLImage*. }
procedure CreateGLImage;
begin
 DestroyGLImage;
 dlDrawImage := ImageDrawToDisplayList(Image);
 dlDrawImageExpand := ImageDrawToDisplayList(ImageExpand);
end;

{ Zwolnij rzeczy inicjowane przez CreateGLImage.
  To zwalnianie moze wymagac kontekstu OpenGLa. }
procedure DestroyGLImage;
begin
 glFreeDisplayList(dlDrawImage);
 glFreeDisplayList(dlDrawImageExpand);
end;

procedure CreateImage(Window: TGLWindow; const fname: string);
begin
  DestroyGLImage;
  DestroyNonGLImage;

  try
    CreateNonGLImage(Window, FName);
  except
    on E: Exception do
    begin
      CreateNonGLImageInvalid(Window, fname);
      CreateGLImage;
      MessageOK(Window, ExceptMessage(E, nil), taLeft);
      Exit;
    end;
  end;

  CreateGLImage;
end;

procedure CreateImage(Window: TGLWindow; Image: TImage; const Name: string);
begin
  DestroyGLImage;
  DestroyNonGLImage;
  CreateNonGLImage(Window, Image, Name);
  CreateGLImage;
end;

procedure ChangeDDSImageIndex(Window: TGLWindow; NewIndex: Cardinal);
begin
  Assert(DDSImage <> nil);
  Assert(NewIndex < Cardinal(DDSImage.Images.Count));
  Assert(IsImageValid); { IsImageValid = always true when DDSImage <> nil }

  DestroyGLImage;

  DDSImageIndex := NewIndex;
  Image := DDSImage.Images[NewIndex] as TImage;

  RemakeImageExpand;

  CreateGLImage;

  UpdateCaption(Window);
end;

initialization
finalization
  DestroyNonGLImage;
end.
