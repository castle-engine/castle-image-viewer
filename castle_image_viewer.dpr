{
  Copyright 2001-2024 Michalis Kamburelis.

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

{ Image viewer using Castle Game Engine.
  See README.md and https://castle-engine.io/castle-image-viewer . }
program castle_image_viewer;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

uses SysUtils, Math, Classes, TypInfo,
  CastleWindow, CastleGLUtils, CastleUtils, CastleImages,
  CastleClassUtils, CastleMessages, CastleParameters, CastleControls,
  CastleFindFiles, CastleVectors, CastleStringUtils,
  CastleGLImages, CastleWindowRecentFiles, CastleInternalCompositeImage, CastleFilesUtils,
  CastleColors, CastleConfig, CastleKeysMouse, CastleURIUtils, CastleRectangles,
  CastleApplicationProperties, CastleOpenDocument,
  CastleDownload, CastleLog, CastleRenderContext, CastleUIControls,
  ImageLoading, EmbeddedImages;

var
  Window: TCastleWindow;
  MoveX: Single = 0;
  MoveY: Single = 0;
  DrawTiled: boolean = false;
  BackgroundColor: TCastleColor = (X: 0.1; Y: 0.1; Z: 0.1; W: 1);
  UseImageAlpha: boolean = true;
  ImageArrowLeft, ImageArrowRight, ImageArrowUp, ImageArrowDown: TCastleImageControl;

var
  { A list of images to browse by previous/next keys/menu items.
    May be relative URLs, so the current dir of this program must never
    change during execution.

    A special case is an item with Object[] property <> @nil:
    this means that an internal image, which should be loaded from
    TImage(Object[]), should be loaded. Display name for user
    should be the value of given string.
    For now, this is only used to load Welcome image, possibly will
    be used for other purposes at some time.

    CurrentImageIndex is always in [0..Images.Count - 1].
    Images.Count is always > 0.

    After changing this call ImagesChanged; }
  Images: TStringList;
  CurrentImageIndex: Integer = 0; { change this only with SetCurrentImageIndex }

{ Create image, based on list index ListPos. }
procedure CreateImageFromList(const ListPos: Integer; const InvalidImage: boolean = false);
begin
  if InvalidImage then
    CreateImageInvalid(Window, Images[ListPos]) else
  if Images.Objects[ListPos] = nil then
    CreateImage(Window, Images[ListPos]) else
    CreateImage(Window, TCastleImage(Images.Objects[ListPos]).MakeCopy,
      Images[ListPos]);
end;

{ Change CurrentImageIndex and try to load newly choosen image.
  Uses CreateImage(Window, Images[CurrentImageIndex]) if it's a normal
  (not internal) image, or something else as appropriate. }
procedure SetCurrentImageIndex(NewValue: Integer);
begin
  CurrentImageIndex := NewValue;
  CreateImageFromList(CurrentImageIndex);
  Window.Invalidate;
end;

procedure ChangeCurrentImageIndex(Change: Integer);
begin
  SetCurrentImageIndex(ChangeIntCycle(
    CurrentImageIndex, Change, Images.Count - 1));
end;

{ operations on Images ---------------------------------------------
  They all do NOT call ImagesChanged automatically. }

procedure AddImageNamesFromFile(const URL: string);

  procedure AddImageNamesFromTextReader(Reader: TTextReader);
  var
    s: string;
  begin
    while not Reader.Eof do
    begin
      s := Trim(Reader.Readln);
      if s <> '' then Images.Append(s);
    end;
  end;

var
  F: TTextReader;
begin
  if URL = '-' then
    F := TTextReader.Create(StdInStream, false)
  else
    F := TTextReader.Create(URL);

  try
    AddImageNamesFromTextReader(F);
  finally FreeAndNil(F) end;
end;

procedure AddToList(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  { avoid adding dups, otherwise AddImageNamesFromURL always adds dups.
    Note: we cannot use Images.Duplicates := dupIgnore,
    because of http://bugs.freepascal.org/view.php?id=28774 }
  if Images.IndexOf(FileInfo.AbsoluteName) = -1 then
    Images.Append(FileInfo.AbsoluteName);
end;

procedure AddImageNamesFromMask(const PathAndMask: string);
begin
  FindFiles(PathAndMask, false, @AddToList, nil, []);
end;

{ Path can be '' or must end with '/' }
procedure AddImageNamesAllLoadable(const Path: string);
var
  I, J: Integer;
  Pattern: string;
begin
  for I := 2 to LoadImage_FileFilters.Count - 1 do
    for J := 0 to LoadImage_FileFilters[I].Patterns.Count - 1 do
    begin
      Pattern := LoadImage_FileFilters[I].Patterns[J];
      AddImageNamesFromMask(Path + Pattern);
    end;
end;

procedure AddImageNamesFromURL(const URL: string);
begin
  // URL will match exactly 1 file
  AddImageNamesFromMask(URL);

  { Add also other filenames within this directory.
    This way if you open a file by double-clicking,
    you can still browse other images in this dir. }
  AddImageNamesAllLoadable(ExtractURIPath(URL));
end;

{ open file --------------------------------------------------------------- }

procedure ImagesChanged; forward;

type
  THelper = class
    class procedure FileOpen(const URL: string);
  end;

class procedure THelper.FileOpen(const URL: string);
begin
  Images.Insert(CurrentImageIndex + 1, URL);
  ImagesChanged;
  ChangeCurrentImageIndex(+1);

  // unused: this is how to open an image without adding it to Images list:
  //CreateImage(Window, URL);
end;

{ operacje na zoom --------------------------------------------------------- }

const
  { musimy miec jakies min i max Zoom - gdyby nie, to rozne integery po drodze
    moga sie latwo przewinac i dzielenie cos / ZoomX (lub ZoomY) tez moze
    powodowac roznorakie przewiniecia. }
  MinZoom: Single = 0.01;
  MaxZoom: Single = 300;

var
  { Zawsze bedzie 0 < MinZoom <= Zoom* <=MaxZoom.
    Uzywaj tylko funkcji ponizej, Set/MultZoom aby zmieniac zmienne Zoom* -
    poza tym procedurami, te zmienne sa read only. }
  ZoomX: Single = 1.0;
  ZoomY: Single = 1.0;

procedure SetZoom(var Zoom: Single; const NewZoom: Single);
begin
  Zoom := Clamped(NewZoom, MinZoom, MaxZoom);
  Window.Invalidate;
end;

procedure MultZoom(var Zoom: Single; const Multiplier: Single);
begin
  Zoom := Zoom * Multiplier;
  ClampVar(Zoom, MinZoom, MaxZoom);
  Window.Invalidate;
end;

{ window callbacks ---------------------------------------------------------- }

procedure Render(Container: TCastleContainer);

  { Draw image with current (ZoomX, ZoomY) and (MoveX, MoveY). }
  procedure DrawImage(const MoveX, MoveY: Single);
  begin
    if Image.HasAlpha and UseImageAlpha then
      DrawableImage.Alpha := acBlending else
      DrawableImage.Alpha := acNone;

    DrawableImage.Draw(
      MoveX, MoveY,
      DrawableImage.Width * ZoomX, DrawableImage.Height * ZoomY,
      0, 0, DrawableImage.Width, DrawableImage.Height);
  end;

var
  VisibleXStart, VisibleXEnd, VisibleYStart, VisibleYEnd: Single;
  Width, Height, i, j, i0, i1, j0, j1: integer;
  HorizScrollBar, VertScrollBar: boolean;
const
  ScrollbarSize = 10; {< scrollbar thickness (with frames around) }
  ScrollbarMargin = 2; {< margin between scrollbar inside and frame }
begin
  Assert(Image <> nil, 'Image not loaded yet');

  { Make sure to treat Width and Height as signed values for compurations
    and comparisons below. }
  Width := Image.Width;
  Height := Image.Height;

  ImageArrowLeft.Exists := false;
  ImageArrowRight.Exists := false;
  ImageArrowUp.Exists := false;
  ImageArrowDown.Exists := false;

  if DrawTiled then
  begin
    { MoveX to wspolrzedna X zasadniczego obrazka (tego ktory bylby
      wyswietlany z not DrawTiled). Znajdujemy i0 takie ze jest to najwieksze
      i dla ktorego (MoveX + i * Width) * ZoomX <= 0 a wiec jest to
      i dla lewej kolumny kafelkow. Podobnie, i1 dla prawej kolumny
      kafelkow to najmniejsze i t.ze (MoveX + (i+1) * Width) * ZoomX >= Window.Width.
      Podobnie znajdujemy j0, j1.

      (operujemy przesuwajac MoveX o Width ale pamietamy ze
      faktyczna pozycja obrazka w pixelach to pozycja*zoom).

      (MoveX + i0 * Width) * ZoomX <= 0 wiec
        i0 <= -MoveX / Width wiec
        i0 = Floor(-MoveX / Width)
      (MoveX + (i1 + 1) * Width) * ZoomX >= Window.Width wiec
        i1 >= (Window.Width / ZoomX - MoveX) / Width - 1 wiec
        i1 = Ceil((Window.Width / ZoomX - MoveX) / Width - 1)
    }

    i0 := Floor(-MoveX / Width);
    i1 := Ceil((Window.Width / ZoomX - MoveX) / Width - 1);
    j0 := Floor(-MoveY / Height);
    j1 := Ceil((Window.Height / ZoomY - MoveY) / Height - 1);

    for i := i0 to i1 do
      for j := j0 to j1 do
        DrawImage(
          MoveX * ZoomX + i * Integer(DrawableImage.Width)  * ZoomX,
          MoveY * ZoomY + j * Integer(DrawableImage.Height) * ZoomY);
  end else
  begin
    VisibleXStart := -MoveX;
    VisibleXEnd   := Window.Width / ZoomX - MoveX;
    VisibleYStart := -MoveY;
    VisibleYEnd   := Window.Height/ZoomY - MoveY;

    if VisibleXStart > Width  then ImageArrowLeft.Exists := true else
    if VisibleXEnd < 0        then ImageArrowRight.Exists := true else
    if VisibleYStart > Height then ImageArrowDown.Exists := true else
    if VisibleYEnd < 0        then ImageArrowUp.Exists := true else
    begin
      ClampVar(VisibleXStart, 0, Width);
      ClampVar(VisibleXEnd  , 0, Width);
      ClampVar(VisibleYStart, 0, Height);
      ClampVar(VisibleYEnd  , 0, Height);

      HorizScrollBar := (VisibleXStart > 0) or (VisibleXEnd < Width);
      VertScrollBar := (VisibleYStart > 0) or (VisibleYEnd < Height);

      if HorizScrollBar and VertScrollBar then
      begin
        RenderContext.ScissorEnable(Window.Rect.RemoveLeft(ScrollbarSize + 1).RemoveBottom(ScrollbarSize + 1));
      end else
      if HorizScrollBar then
      begin
        RenderContext.ScissorEnable(Window.Rect.RemoveBottom(ScrollbarSize + 1));
      end else
      if VertScrollBar then
      begin
        RenderContext.ScissorEnable(Window.Rect.RemoveLeft(ScrollbarSize + 1));
      end;

      DrawImage(MoveX * ZoomX, MoveY * ZoomY);

      RenderContext.ScissorDisable;

      if HorizScrollBar then
      begin
        DrawPrimitive2D(pmLines, [
          Vector2(ScrollbarSize, ScrollbarSize),
          Vector2(Window.Width, ScrollbarSize),
          Vector2(ScrollbarSize, 0),
          Vector2(ScrollbarSize, ScrollbarSize)
        ], Yellow);

        VisibleXStart := MapRange(VisibleXStart, 0, Width, ScrollbarSize+ScrollbarMargin, Window.Width-ScrollbarMargin);
        VisibleXEnd   := MapRange(VisibleXEnd,   0, Width, ScrollbarSize+ScrollbarMargin, Window.Width-ScrollbarMargin);
        DrawRectangle(FloatRectangle(VisibleXStart, ScrollbarMargin,
          VisibleXEnd - VisibleXStart, ScrollbarSize - 2 * ScrollbarMargin), Gray);
      end;

      if VertScrollBar then
      begin
        DrawPrimitive2D(pmLines, [
          Vector2(ScrollbarSize, ScrollbarSize),
          Vector2(ScrollbarSize, Window.Height),
          Vector2(0, ScrollbarSize),
          Vector2(ScrollbarSize, ScrollbarSize)
        ], Yellow);

        VisibleYStart := MapRange(VisibleYStart, 0, Height, ScrollbarSize + ScrollbarMargin, Window.Height - ScrollbarMargin);
        VisibleYEnd   := MapRange(VisibleYEnd,   0, Height, ScrollbarSize + ScrollbarMargin, Window.Height - ScrollbarMargin);
        DrawRectangle(FloatRectangle(ScrollbarMargin, VisibleYStart,
          ScrollbarSize - 2 * ScrollbarMargin, VisibleYEnd - VisibleYStart), Gray);
      end;
    end;
  end;
end;

procedure Update(Container: TCastleContainer);

  procedure Move(var value: Single; Change: Single);
  begin
    Change := Change * (8 * 50 * Window.Fps.SecondsPassed);
    if Window.Pressed[keyCtrl] then Change := Change * 10;
    value := value + Change;
    Window.Invalidate;
  end;

const
  ScaleFactor = 5;
var
  ScaleUp, ScaleDown: Single;
begin
  if Window.Pressed[keyArrowUp] then Move(MoveY, -1 / ZoomY);
  if Window.Pressed[keyArrowDown] then Move(MoveY, 1 / ZoomY);
  if Window.Pressed[keyArrowRight] then Move(MoveX, -1 / ZoomX);
  if Window.Pressed[keyArrowLeft] then Move(MoveX, 1 / ZoomX);

  ScaleUp := Power(ScaleFactor, Window.Fps.SecondsPassed);
  ScaleDown := Power(1 / ScaleFactor, Window.Fps.SecondsPassed);

  if Window.Pressed[keyNumpadPlus] or
     Window.Pressed[keyPlus] or
     Window.Pressed.Characters['+'] then
  begin
    MultZoom(ZoomX, ScaleUp);
    MultZoom(ZoomY, ScaleUp);
  end;

  if Window.Pressed[keyNumpadMinus] or
     Window.Pressed[keyMinus] or
     Window.Pressed.Characters['-'] then
  begin
    MultZoom(ZoomX, ScaleDown);
    MultZoom(ZoomY, ScaleDown);
  end;

  if Window.Pressed[keyX] then
    if Window.Pressed[keyShift] then
      MultZoom(ZoomX, ScaleUp)
    else
      MultZoom(ZoomX, ScaleDown);

  if Window.Pressed[keyY] then
    if Window.Pressed[keyShift] then
      MultZoom(ZoomY, ScaleUp)
    else
      MultZoom(ZoomY, ScaleDown);
end;

procedure Motion(Container: TCastleContainer; const Event: TInputMotion);
begin
  if buttonLeft in Event.Pressed then
  begin
    MoveX := MoveX + ((Event.Position[0] - Event.OldPosition[0]) / ZoomX);
    MoveY := MoveY + ((Event.Position[1] - Event.OldPosition[1]) / ZoomY);
    Window.Invalidate;
  end;
end;

procedure Press(Container: TCastleContainer; const Event: TInputPressRelease);
const
  ScaleFactor = 1.1;
var
  ScaleUp, ScaleDown: Single;
begin
  if Event.IsMouseWheel(mwUp) then
  begin
    ScaleUp := ScaleFactor;
    MultZoom(ZoomX, ScaleUp);
    MultZoom(ZoomY, ScaleUp);
  end;

  if Event.IsMouseWheel(mwDown) then
  begin
    ScaleDown := 1 / ScaleFactor;
    MultZoom(ZoomX, ScaleDown);
    MultZoom(ZoomY, ScaleDown);
  end;
end;

procedure CreateUserInterface;

  function CreateArrowImage(const Image: TCastleImage): TCastleImageControl;
  begin
    Result := TCastleImageControl.Create(Application);
    Result.Anchor(hpMiddle);
    Result.Anchor(vpMiddle);
    Result.Content.OwnsImage := false;
    Result.Content.Image := Image;
    Result.Color := Yellow;
    { Intial Exists = false matters in case we have an error (and make MessageOK)
      at opening 1st image. }
    Result.Exists := false;
    Window.Controls.InsertFront(Result);
  end;

begin
  ImageArrowLeft := CreateArrowImage(Arrow_left_circle);
  ImageArrowRight := CreateArrowImage(Arrow_right_circle);
  ImageArrowUp := CreateArrowImage(Arrow_up_circle);
  ImageArrowDown := CreateArrowImage(Arrow_down_circle);
end;

{ menu ------------------------------------------------------------ }

var
  { initialized in CreateMainMenu, then updated in each ImagesNamesListChanged. }
  ImagesMenu: TMenu;

{ After changing Images contents always call this.
  This ensures that ImagesMenu is properly updated. }
procedure ImagesChanged;
const
  NormalImageMenuCount = 6;
var
  i: Integer;
begin
  while ImagesMenu.Count > NormalImageMenuCount do
    ImagesMenu.Delete(NormalImageMenuCount);

  { Do not load all Images.Count to the menu,
    as this makes menu very large, making updating it (e.g. when user
    loads new image) time-consuming, and it's not possible to navigate
    it anyway. }

  for i := 0 to Min(Images.Count, 20) - 1 do
    ImagesMenu.Append(TMenuItem.Create(
      SQuoteMenuEntryCaption(URICaption(Images[i])), 10000 + i));
end;

procedure DropFiles(Container: TCastleContainer; const FileNames: array of string);
var
  URL: string;
begin
  if High(FileNames) >= 0 then
  begin
    URL := FilenameToURISafe(FileNames[0]);
    if URL <> '' then
      THelper.FileOpen(URL);
  end;
end;

procedure MenuClick(Container: TCastleContainer; Item: TMenuItem);

  procedure ImageSave;
  var
    URL: string;
  begin
    if IsImageValid then
    begin
      URL := ImageURL;
      if Window.FileDialog('Save image to file', URL, false, SaveImage_FileFilters) then
      try
        if CompositeImage <> nil then
        begin
          if TCompositeImage.MatchesURL(URL) then
          begin
            CompositeImage.SaveToFile(URL);
          end else
          begin
            if MessageYesNo(Window, 'Composite (DDS, KTX...) image is composed from many single (normal, simple, 2D) images. Saving from composite image to other file format will only save the current single image layer. Continue?') then
              SaveImage(Image, URL);
          end;
        end else
          SaveImage(Image, URL);
      except
        on E: EImageSaveError do
          MessageOk(Window, Format('Saving image "%s" failed: %s', [URL, E.Message]));
      end;
    end else
      MessageOK(Window, 'No valid image loaded');
  end;

  procedure ImageOpen;
  var
    URL: string;
  begin
    if IsImageValid then
      URL := ImageURL
    else
      URL := '';
    if Window.FileDialog('Load image from file', URL, true, LoadImage_FileFilters) then
      THelper.FileOpen(URL);
  end;

  procedure ImageOpenWithDirectory;
  var
    URL: string;
  begin
    if IsImageValid then
      URL := ImageURL
    else
      URL := '';
    if Window.FileDialog('Load image from file', URL, true, LoadImage_FileFilters) then
    begin
      Images.Clear;
      AddImageNamesFromURL(URL);
      SetCurrentImageIndex(0);
    end;
  end;

  procedure ShowImageInfo;

    function CompositeImageInfo(DImg: TCompositeImage): string;
    begin
      Result := Format(
        'Containing Composite image info:' +NL+
        '  Width x Height x Depth: %d x %d %d'  +NL+
        '  Type: %s' +NL+
        '  Mipmaps: %s' +NL+
        '  Simple 2D images inside: %d',
        [ DImg.Width, DImg.Height, DImg.Depth,
          CompositeTypeToString[DImg.CompositeType],
          BoolToStr(DImg.Mipmaps, true),
          DImg.Images.Count ]);
    end;

  var
    S: string;
  begin
    if IsImageValid then
      S := Format(
        'Image URL: %s' + NL +
        'Class: %s' +NL +
        'Width x height: %d x %d',
        [ImageURL, Image.ClassName, Image.Width, Image.Height]) else
      S := '<invalid image file>';
    if CompositeImage <> nil then
      S := S + (NL + Format('Composite subimage: %d', [CompositeImageIndex]) + NL +
        CompositeImageInfo(CompositeImage));
    MessageOK(Window, S);
  end;

  procedure ShowAbout;
  begin
    MessageOK(Window,
      'Keys (the ones not documented already in the menu):' + NL +
      'Arrows: move image,' + NL +
      'Arrows + Ctrl: move image 10 x faster,' + NL +
      '- / +: scale image,' + NL +
      'x / X: scale only horizontally,' + NL +
      'y / Y: scale only vertically.' + NL +
      NL +
      ApplicationProperties.Description
    );
  end;

  function CheckNotGrayscale: boolean;
  begin
    Result := (Image is TRGBImage) or (Image is TRGBAlphaImage);
    if not Result then
      MessageOk(Window, 'This function is not available for grayscale images.');
  end;

  procedure ImageClear;
  var
    Color: TCastleColor;
  begin
    if CheckNotGrayscale then
    begin
      Color := White;
      if Window.ColorDialog(Color) then
      begin
        Image.Clear(Color);
        ImageChanged;
      end;
    end;
  end;

  procedure EasyChangeCompositeImageIndex(Change: Integer);
  begin
    if CompositeImage <> nil then
      ChangeCompositeImageIndex(Window, ChangeIntCycle(
        CompositeImageIndex, Change, CompositeImage.Images.Count - 1)) else
      MessageOk(Window, 'Available only for Composite images.');
  end;

  procedure DoResize(const Interpolation: TResizeInterpolation);
  var
    ResizeToX, ResizeToY: Cardinal;
  begin
    ResizeToX := 0;
    ResizeToY := 0;
    if MessageInputQueryCardinal(Window,
      'Resize to given Width (leave 0 to keep current)', ResizeToX) then
    if MessageInputQueryCardinal(Window,
      'Resize to given Height (leave 0 to keep current)', ResizeToY) then
    begin
      Image.Resize(ResizeToX, ResizeToY, Interpolation);
      ImageChanged;
    end;
  end;

var
  change: Single;
begin
  case Item.IntData of
    110: ImageOpen;
    115: ImageOpenWithDirectory;
    120: ImageSave;
    140: Window.Close;

    205: begin
           SmoothScaling := not SmoothScaling;
           if DrawableImage <> nil then
             DrawableImage.SmoothScaling := SmoothScaling;
         end;
    210: begin
           Change := (Window.Width / Image.Width) / ZoomX;
           MultZoom(ZoomX, change);
           MultZoom(ZoomY, change);
         end;
    211: begin
           Change := (Window.Height / Image.Height) / ZoomY;
           MultZoom(ZoomX, change);
           MultZoom(ZoomY, change);
         end;
    220: SetZoom(ZoomX, Window.Width / Image.Width);
    221: SetZoom(ZoomY, Window.Height / Image.Height);
    230: DrawTiled := not DrawTiled;
    240: begin
           SetZoom(ZoomX, 1.0);
           SetZoom(ZoomY, 1.0);
           MoveX := 0;
           MoveY := 0;
         end;

    260: begin
           if Window.ColorDialog(BackgroundColor) then
             Window.Container.BackgroundColor := BackgroundColor;
         end;
    270: UseImageAlpha := not UseImageAlpha;

    310: ChangeCurrentImageIndex(-1);
    311: ChangeCurrentImageIndex(+1);

    320: EasyChangeCompositeImageIndex(-1);
    321: EasyChangeCompositeImageIndex(+1);

    410: if CheckNotGrayscale then
         begin
           Image.Grayscale;
           ImageChanged;
         end;
    415: begin
           Image.AlphaBleed('Alpha Bleeding');
           ImageChanged;
         end;
    420..422:
         if CheckNotGrayscale then
         begin
           Image.ConvertToChannelRGB(Item.IntData - 420);
           ImageChanged;
         end;
    430..432:
         if CheckNotGrayscale then
         begin
           Image.StripToChannelRGB(Item.IntData - 430);
           ImageChanged;
         end;
    440: begin
           Image.FlipHorizontal;
           ImageChanged;
         end;
    441: begin
           Image.FlipVertical;
           ImageChanged;
         end;

    450: begin
           Image.Rotate(1);
           ImageChanged;
         end;
    451: begin
           Image.Rotate(2);
           ImageChanged;
         end;
    452: begin
           Image.Rotate(3);
           ImageChanged;
         end;
    460: ImageClear;
    510: ShowImageInfo;
    520: ShowAbout;
    1600..1700: DoResize(TResizeInterpolation(Item.IntData - 1600));
    2010: OpenUrl(ImageURL);
    else SetCurrentImageIndex(Item.IntData - 10000);
  end;
  Window.Invalidate;
end;

function InterpolationToStr(const Interpolation: TResizeInterpolation): string;
begin
  Result := SEnding(GetEnumName(TypeInfo(TResizeInterpolation), Ord(Interpolation)), 3);
end;

{ This assumes that Images is empty, so be sure to call this before
  adding something to Images. For simplicity of "control flow" we do NOT
  call here ImagesChanged. }
function CreateMainMenu: TMenu;
var
  M: TMenu;
  NextRecentMenuItem: TMenuEntry;
  Interpolation: TResizeInterpolation;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('_Open ...',                 110, CtrlO));
    M.Append(TMenuItem.Create('_Open (and place other images in the same directory on a list) ...', 115, 'o'));
    M.Append(TMenuItem.Create('_Save ...',                 120, CtrlS));
    NextRecentMenuItem := TMenuSeparator.Create;
    M.Append(NextRecentMenuItem);
    RecentMenu.NextMenuItem := NextRecentMenuItem;
    M.Append(TMenuItem.Create('_Exit',                     140, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItemChecked.Create('Smooth Scaling', 205, SmoothScaling, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Fit Image to Window _Width',         210, 'w'));
    M.Append(TMenuItem.Create('Fit Image to Window _Height',        211, 'h'));
    M.Append(TMenuItem.Create('Fit Image Width to Window Width',    220, 'W'));
    M.Append(TMenuItem.Create('Fit Image Height to Window Height',  221, 'H'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Test Is _Tileable', 230, 't', DrawTiled, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Reset _Zoom and Translation',         240, keyHome));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Use Image Alpha Channel', 270,  UseImageAlpha, true));
    M.Append(TMenuItem.Create('Background Color ...',                260));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemToggleFullScreen.Create(Window.FullScreen));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Open In External Application', 2010));
    Result.Append(M);
  M := TMenu.Create('_Resize');
    for Interpolation := Low(Interpolation) to High(Interpolation) do
      M.Append(TMenuItem.Create('Resize (' + InterpolationToStr(Interpolation) + ') ...', 1600 + Ord(Interpolation)));
    Result.Append(M);
  M := TMenu.Create('_Edit');
    M.Append(TMenuItem.Create('_Grayscale',                          410));
    M.Append(TMenuItem.Create('_Alpha Bleed (Slow but Correct Algorithm)', 415));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Convert to Red Channel',             420));
    M.Append(TMenuItem.Create('Convert to Green Channel',           421));
    M.Append(TMenuItem.Create('Convert to Blue Channel',            422));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Strip to Red Channel',                430));
    M.Append(TMenuItem.Create('Strip to Green Channel',              431));
    M.Append(TMenuItem.Create('Strip to Blue Channel',               432));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Mirror Horizontally',                440));
    M.Append(TMenuItem.Create('Mirror Vertically',                   441));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Rotate 90 Degrees (Clockwise)', 450));
    M.Append(TMenuItem.Create('Rotate 180 Degrees', 451));
    M.Append(TMenuItem.Create('Rotate 90 Degrees (Counter-Clockwise)', 452));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Clear ...', 460));
    Result.Append(M);
  M := TMenu.Create('_Images');
  ImagesMenu := M;
    // TODO: Ctrl + PageUp/Down would be more consistent
    M.Append(TMenuItem.Create('_Previous Subimage in Composite (DDS, KTX)', 320, CtrlP));
    M.Append(TMenuItem.Create('_Next Subimage in Composite (DDS, KTX)',     321, CtrlN));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Previous Image', 310, keyPageUp));
    M.Append(TMenuItem.Create('_Next Image',     311, keyPageDown));
    M.Append(TMenuSeparator.Create);
    Result.Append(M);
  M := TMenu.Create('_Help');
    M.Append(TMenuItem.Create('Image Information', 510, CtrlI));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('About castle-image-viewer', 520));
    Result.Append(M);
end;

{ params ------------------------------------------------------------------- }

const
  Options: array [0..2] of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0 ; Long: 'capabilities'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  Pattern, RecognizedPatterns: string;
  I, J: integer;
begin
  case OptionNum of
    0:begin
        RecognizedPatterns := '';
        for I := 2 to LoadImage_FileFilters.Count - 1 do
          for J := 0 to LoadImage_FileFilters[I].Patterns.Count - 1 do
          begin
            Pattern := LoadImage_FileFilters[I].Patterns[J];
            RecognizedPatterns := RecognizedPatterns + (' ' + Pattern);
          end;
        InfoWrite(
          'castle-image-viewer: image viewer using Castle Game Engine.' +nl+
          '  Support various common image formats (PNG, JPG...)' +nl+
          '  and some exotic ones useful in game development (DDS, KTX...).' +nl+
          '  Allows browsing images list,' +nl+
          '  allows to scale and move viewed image, allows to test visually' +nl+
          '  is image "tileable".' +nl+
          nl+
          'Usage:' +nl+
          '  castle-image-viewer [OPTIONS]... [IMAGES]...' +nl+
          nl+
          'You can give as many image names on the command line as you like' +nl+
          '(you will be able to switch between them using n/p (next/previous)' +nl+
          'keys). Each image name will be treated as a mask with special chars' +nl+
          '* (any number of any chars) and ? (any char), e.g.' +nl+
          '  castle-image-viewer *.png' +nl+
          'will open any png images (i.e., even if the shell itself will not expand' +nl+
          '*.png). Non-existing image names (so, also filename masks not matching any'+nl+
          'existing filename) will be ignored.' +nl+
          nl+
          'Instead of image name, you can give parameter starting with "@" :' +nl+
          'parameter "@file_list.txt" means "read image names to load' +nl+
          'from the file file_list.txt - one image name per line".' +nl+
          nl+
          'Not giving any image names for castle-image-viewer to load will have the same' +nl+
          'effect as calling' +nl+
          '  castle-image-viewer' +RecognizedPatterns +nl+
          'so all images in known format (in the current directory) will be loaded.' +nl+
          nl+
          'Accepted command-line options:' +nl+
          OptionDescription('-h / --help', 'Print this help message and exit.') + NL +
          OptionDescription('-v / --version', 'Print the version number and exit.') + NL +
          OptionDescription('--capabilities automatic|force-fixed-function|force-modern', 'Force OpenGL context to have specific capabilities, to test rendering on modern or ancient GPUs.') + NL +
          nl+
          TCastleWindow.ParseParametersHelp +nl+
          nl+
          'By default, window size will be the same as of the first loaded image.'+nl+
          nl+
          ApplicationProperties.Description);
        Halt;
      end;
    1:begin
        WritelnStr(ApplicationProperties.Version);
        Halt;
      end;
    2:TGLFeatures.RequestCapabilities := StrToCapabilities(Argument);
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ main ----------------------------------------------------------------------- }

procedure Run;
var
  i: Integer;
  { error messages gathered }
  SavedErrorMessages: string;
begin
  ApplicationProperties.ApplicationName := 'castle-image-viewer';
  ApplicationProperties.Version := '2.2.0';
  { Do not show warnings,
    as on Windows GUI this would result in new modal window for each warning.
    Worse, font rendering warnings (about missing glyphs) would prevent
    us showing MessageOK with errors about the image. }
  // ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  // Initialize log as early as possible, but avoid messing --help/--version output
  if not Parameters.IsPresent([
        '-h',
        '--help',
        '-v',
        '--version'
      ]) then
    InitializeLog;

  Window := TCastleWindow.Create(Application);
  Theme.DialogsLight;

  // Assigning Application.MainWindow is probably not necessary now in this application
  Application.MainWindow := Window;

  Images := TStringList.Create;
  try
    { init menu things. We must do it before we add something to Images,
      this is required by CreateMainMenu. }
    RecentMenu := TWindowRecentFiles.Create(nil);
    RecentMenu.OnOpenRecent := {$ifdef FPC}@{$endif}THelper{$ifdef FPC}(nil){$endif}.FileOpen;
    Window.MainMenu := CreateMainMenu;
    Window.OnMenuClick := @MenuClick;

    { parse options }
    Window.ParseParameters;
    { parse our options }
    Parameters.Parse(Options, @OptionProc, nil);

    Window.DepthBits := 0; { depth buffer not needed here }
    Window.OnUpdate := @Update;
    Window.OnRender := @Render;
    Window.OnDropFiles := @DropFiles;
    Window.OnMotion := @Motion;
    Window.OnPress := @Press;
    Window.Open;

    CreateUserInterface;

    UserConfig.Load;
    RecentMenu.LoadFromConfig(UserConfig);

    { calculate Images = parse the list of image files to open }
    if Parameters.High = 0 then
    begin
      AddImageNamesAllLoadable('');
      Images.Sort;
    end else
    begin
      SavedErrorMessages := '';

      for i := 1 to Parameters.High do
      begin
        if SCharIs(Parameters[i], 1, '@') then
          AddImageNamesFromFile(SEnding(Parameters[i], 2))
        else
        case URIExists(Parameters[i]) of
          ueFile, ueUnknown: AddImageNamesFromURL(Parameters[i]);
          ueDirectory: AddImageNamesAllLoadable(InclPathDelim(Parameters[i]));
          else SavedErrorMessages := SavedErrorMessages + 'File "' + Parameters[i] + '" does not exist.' + NL;
        end;
      end;

      if SavedErrorMessages <> '' then
      begin
        { make sure to create some Image, to be able to Render }
        if Image = nil then
          CreateImageInvalid(Window, '');
        MessageOk(Window, Trim(SavedErrorMessages));
        SavedErrorMessages := '';
      end;
    end;

    { Always insert welcome image }
    Images.InsertObject(0, '<Welcome image>', Welcome);
    Assert(Images.Count > 0);

    { By default, view the first loaded image, or welcome image if nothing loaded. }
    if Images.Count > 1 then
      CurrentImageIndex := 1 else
      CurrentImageIndex := 0;

    ImagesChanged;

    { Initialize Image. Window is open now (it has to, otherwise we get
      warnings that opening files before Application.OnInitialize is not portable),
      DecompressTexture is assigned, so we load it in a straightforward way. }
    CreateImageFromList(CurrentImageIndex);

    Application.Run;

    RecentMenu.SaveToConfig(UserConfig);
    UserConfig.Save;
  finally
    FreeAndNil(Images);
    FreeAndNil(RecentMenu);
  end;
end;

begin
  try
    Run;
  except
    on E: TObject do
    begin
      { In case of exception, write nice message and exit with non-zero status,
        without dumping any stack trace (because it's normal to
        exit with exception in case of project/environment error, not a bug,
        and the stack trace is mostly useless for end-users in -dRELEASE mode). }
      ErrorWrite(ExceptMessage(E));
      Halt(1);
    end;
  end;
end.
