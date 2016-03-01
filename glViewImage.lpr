{
  Copyright 2001-2014 Michalis Kamburelis.

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

program glViewImage;

{$I castleconf.inc}

{$ifdef MSWINDOWS}
  {$R automatic-windows-resources.res}
{$endif MSWINDOWS}

{$apptype GUI}

uses SysUtils, Math, Classes, TypInfo,
  CastleWindow, CastleGL, CastleGLUtils, CastleUtils, CastleImages,
  CastleClassUtils, CastleMessages, CastleParameters, CastleControls,
  CastleFindFiles, CastleVectors, CastleStringUtils, CastleWarnings,
  CastleGLImages, CastleWindowRecentFiles, CastleCompositeImage, CastleFilesUtils,
  CastleColors, CastleConfig, CastleKeysMouse, CastleURIUtils, CastleRectangles,
  ImageLoading, GVIImages;

var
  Window: TCastleWindowCustom;
  MoveX: TGLfloat = 0;
  MoveY: TGLfloat = 0;
  DrawTiled: boolean = false;
  BackgroundColor: TCastleColor = (0, 0, 0, 1);
  { error messages gathered before Window was open }
  SavedErrorMessages: string = '';
  UseImageAlpha: boolean = true;

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

{ Create non-GL image part, based on list index ListPos. }
procedure CreateNonGLImageFromList(const ListPos: Integer;
  const InvalidImage: boolean);
begin
  if InvalidImage then
    CreateNonGLImageInvalid(Window, Images[ListPos]) else
  if Images.Objects[ListPos] = nil then
    CreateNonGLImage(Window, Images[ListPos]) else
    CreateNonGLImage(Window,
      TCastleImage(Images.Objects[ListPos]).MakeCopy,
      Images[ListPos]);
end;

{ Create both non-GL and GL image parts, based on list index ListPos. }
procedure CreateImageFromList(const ListPos: Integer);
begin
  if Images.Objects[ListPos] = nil then
    CreateImage(Window, Images[ListPos]) else
    CreateImage(Window,
      TCastleImage(Images.Objects[ListPos]).MakeCopy,
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
  f: TTextReader;
begin
  if URL = '-' then
    AddImageNamesFromTextReader(StdinReader) else
  begin
    f := TTextReader.Create(URL);
    try
      AddImageNamesFromTextReader(f);
    finally f.Free end;
  end;
end;

procedure AddToList(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  { avoid adding dups, otherwise AddImageNamesFromFileName always adds dups.
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

procedure AddImageNamesFromFileName(const FileName: string);
begin
  // FileName will match exactly 1 file
  AddImageNamesFromMask(ExpandFileName(FileName));

  { Add also other filenames within this directory.
    This way if you open a file by double-clicking,
    you can still browse other images in this dir. }
  AddImageNamesAllLoadable(ExtractFilePath(FileName));
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
    moga sie latwo przewinac i dzielenie cos / zoomx (lub zoomy) tez moze
    powodowac roznorakie przewiniecia. }
  MinZoom: Single = 0.01;
  MaxZoom: Single = 300;

var
  { Zawsze bedzie 0 < MinZoom <= Zoom* <=MaxZoom.
    Uzywaj tylko funkcji ponizej, Set/MultZoomGL aby zmieniac zmienne Zoom* -
    poza tym procedurami, te zmienne sa read only. }
  ZoomX: Single = 1.0;
  ZoomY: Single = 1.0;

procedure SetZoomGL(var Zoom: Single; const NewZoom: Single);
begin
  Zoom := Clamped(NewZoom, MinZoom, MaxZoom);
  Window.Invalidate;
end;

procedure MultZoomGL(var Zoom: Single; const Multiplier: Single);
begin
  Zoom *= Multiplier;
  ClampVar(Zoom, MinZoom, MaxZoom);
  Window.Invalidate;
end;

{ glw callbacks ---------------------------------------------------------- }

procedure Render(Container: TUIContainer);

  procedure Arrow(const Angle: TGLfloat);
  begin
    glTranslatef(Window.width div 2, Window.height div 2, 0);
    glRotatef(Angle, 0, 0, 1);
    glColorv(Yellow);
    glScalef(50, 50, 0);
    GLDrawArrow;
  end;

  { Draw image with current (ZoomX, ZoomY) and (MoveX, MoveY). }
  procedure DrawImage(MoveX, MoveY: Integer);
  begin
    if Image.HasAlpha and UseImageAlpha then
      GLImage.Alpha := acFullRange else
      GLImage.Alpha := acNone;

    GLImage.Draw(
      MoveX, MoveY,
      Round(GLImage.Width * ZoomX), Round(GLImage.Height * ZoomX),
      0, 0, GLImage.Width, GLImage.Height);
  end;

var
  visibleXStart, visibleXEnd,
  visibleYStart, visibleYEnd,
  Width, Height, i, j, i0, i1, j0, j1: integer;
  horizScrollbar, vertScrollbar: boolean;
const
  ScrollbarSize = 10; {< scrollbar thickness (with frames around) }
  ScrollbarMargin = 2; {< margin between scrollbar inside and frame }
begin
  glLoadIdentity;

  { fpc zle sobie radzi gdy przychodzi do porownywania integerow i cardinali :
    wybiera wspolny typ jako cardinal - tragiczny blad. Przydadza nam sie tutaj
    Width i Height jako int. }
  Width := Image.Width;
  Height := Image.Height;

  { Clear color buffer bit, even when DrawTiled --- because image may
    have alpha channel, and then background is visible. }
  GLClear([cbColor], BackgroundColor);

  if DrawTiled then
  begin
    { MoveX to wspolrzedna X zasadniczego obrazka (tego ktory bylby
      wyswietlany z not DrawTiled). Znajdujemy i0 takie ze jest to najwieksze
      i dla ktorego (MoveX + i*Width)*zoomX <= 0 a wiec jest to
      i dla lewej kolumny kafelkow. Podobnie, i1 dla prawej kolumny
      kafelkow to najmniejsze i t.ze (MoveX + (i+1)*Width)*zoomx >= Window.width.
      Podobnie znajdujemy j0, j1.

      (operujemy przesuwajac MoveX o Width ale pamietamy ze
      faktyczna pozycja obrazka w pixelach to pozycja*zoom).

      (MoveX + i0*Width)*zoomX <= 0 wiec
        i0 <= -MoveX/Width wiec
        i0 = Floor(-MoveX/Width)
      (MoveX + (i1 + 1)*Width)*zoomx >= Window.Width wiec
        i1 >= (Window.Width/zoomx - MoveX)/Width - 1 wiec
        i1 = Ceil((Window.width/zoomx - MoveX)/Width - 1)
    }

    i0 := Floor(-MoveX/Width);
    i1 := Ceil((Window.Width/zoomX - MoveX)/Width - 1);
    j0 := Floor(-MoveY/Height);
    j1 := Ceil((Window.Height/zoomY - MoveY)/Height - 1);

    for i := i0 to i1 do
      for j := j0 to j1 do
        DrawImage(
          { We have to round this way, to make drawn rectangles fit perfectly
            next to each other, without any 1-pixel border because of different
            rounding. }
          Round(MoveX * ZoomX) + i * Round(GLImage.Width  * ZoomX),
          Round(MoveY * ZoomY) + j * Round(GLImage.Height * ZoomY));
  end else
  begin
    visibleXStart := -Round(MoveX);
    visibleXEnd   := Round(Window.width/zoomX - MoveX);
    visibleYStart := -Round(MoveY);
    visibleYEnd   := Round(Window.height/zoomY - MoveY);

    if visibleXStart > Width  then Arrow(90) else
    if visibleXEnd < 0        then Arrow(-90) else
    if visibleYStart > Height then Arrow(180) else
    if visibleYEnd < 0        then Arrow(0) else
    begin
      ClampVar(visibleXStart, 0, Width);
      ClampVar(visibleXEnd  , 0, Width);
      ClampVar(visibleYStart, 0, Height);
      ClampVar(visibleYEnd  , 0, Height);

      horizScrollbar := (visibleXStart > 0) or (visibleXEnd < Width);
      vertScrollbar := (visibleYStart > 0) or (visibleYEnd < Height);

      if horizScrollbar and vertScrollbar then
      begin
        ScissorEnable(Window.Rect.RemoveLeft(ScrollbarSize + 1).RemoveBottom(ScrollbarSize + 1));
      end else
      if horizScrollbar then
      begin
        ScissorEnable(Window.Rect.RemoveBottom(ScrollbarSize + 1));
      end else
      if vertScrollbar then
      begin
        ScissorEnable(Window.Rect.RemoveLeft(ScrollbarSize + 1));
      end;

      DrawImage(Round(MoveX * ZoomX), Round(MoveY * ZoomY));

      ScissorDisable;

      if horizScrollbar then
      begin
        glColorv(Yellow);
        glBegin(GL_LINES);
          glVertex2f(ScrollbarSize, ScrollbarSize);
          glVertex2f(Window.width, ScrollbarSize);
          glVertex2f(ScrollbarSize, 0);
          glVertex2f(ScrollbarSize, ScrollbarSize);
        glEnd;

        visibleXStart := Round(MapRange(visibleXStart, 0, Width, ScrollbarSize+ScrollbarMargin, Window.width-ScrollbarMargin));
        visibleXEnd   := Round(MapRange(visibleXEnd,   0, Width, ScrollbarSize+ScrollbarMargin, Window.width-ScrollbarMargin));
        DrawRectangle(Rectangle(visibleXStart, ScrollbarMargin,
          visibleXEnd - visibleXStart, ScrollbarSize - 2 * ScrollbarMargin), Gray);
      end;

      if vertScrollbar then
      begin
        glColorv(Yellow);
        glBegin(GL_LINES);
          glVertex2f(ScrollbarSize, ScrollbarSize);
          glVertex2f(ScrollbarSize, Window.height);
          glVertex2f(0, ScrollbarSize);
          glVertex2f(ScrollbarSize, ScrollbarSize);
        glEnd;

        visibleYStart := Round(MapRange(visibleYStart, 0, Height, ScrollbarSize+ScrollbarMargin, Window.height-ScrollbarMargin));
        visibleYEnd    :=Round(MapRange(visibleYEnd,   0, Height, ScrollbarSize+ScrollbarMargin, Window.height-ScrollbarMargin));
        DrawRectangle(Rectangle(ScrollbarMargin, visibleYStart,
          ScrollbarSize - 2 * ScrollbarMargin, visibleYEnd - visibleYStart), Gray);
      end;
    end;
  end;
end;

procedure Update(Container: TUIContainer);

  procedure Move(var value: TGLfloat; change: TGLfloat);
  begin
   change *= 8*Window.Fps.UpdateSecondsPassed * 50;
   if Window.Pressed[k_Ctrl] then change *= 10;
   value += change;
   Window.Invalidate;
  end;

const SCALE_FACTOR = 0.1;
var scale_up, scale_down: Single;
begin
 with Window do begin
  if Pressed[K_Up] then Move(MoveY, -1 / zoomY);
  if Pressed[K_Down] then Move(MoveY, 1 / zoomY);
  if Pressed[K_Right] then Move(MoveX, -1 / zoomX);
  if Pressed[K_Left] then Move(MoveX, 1 / zoomX);

  scale_up := 1 + SCALE_FACTOR * Window.Fps.UpdateSecondsPassed * 50;
  scale_down := 1 / scale_up;

  if Pressed[K_Numpad_Plus ] or Pressed[K_Plus ] or Pressed.Characters['+'] then
  begin
    MultZoomGL(ZoomX, scale_up);
    MultZoomGL(ZoomY, scale_up);
  end;

  if Pressed[K_Numpad_Minus] or Pressed[K_Minus] or Pressed.Characters['-'] then
  begin
    MultZoomGL(ZoomX, scale_down);
    MultZoomGL(ZoomY, scale_down);
  end;

  if Pressed[K_x] then
   if Pressed[K_Shift] then
    MultZoomGL(ZoomX, scale_up) else
    MultZoomGL(ZoomX, scale_down);
  if Pressed[K_y] then
   if Pressed[K_Shift] then
    MultZoomGL(ZoomY, scale_up) else
    MultZoomGL(ZoomY, scale_down);
 end;
end;

procedure Open(Container: TUIContainer);
begin
  DecompressTexture := @GLDecompressTexture;

  if Image <> nil then
    CreateGLImage else
    { Image failed to initialize before (because texture could
      not be decompressed), initialize it fully now. }
    CreateImageFromList(CurrentImageIndex);

  if Trim(SavedErrorMessages) <> '' then
  begin
    MessageOk(Window, Trim(SavedErrorMessages));
    SavedErrorMessages := '';
  end;
end;

procedure Close(Container: TUIContainer);
begin
  DestroyGLImage;
end;

{ menu ------------------------------------------------------------ }

const
  Version = '1.5.0';
  DisplayApplicationName = 'glViewImage';

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

procedure DropFiles(Container: TUIContainer; const FileNames: array of string);
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

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);

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
    URL := ImageURL else
    URL := '';
   if Window.FileDialog('Load image from file', URL, true, LoadImage_FileFilters) then
   begin
     THelper.FileOpen(URL);
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
          BoolToStr[DImg.Mipmaps],
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
      S += NL + Format('Composite subimage: %d', [CompositeImageIndex]) + NL +
        CompositeImageInfo(CompositeImage);
    MessageOK(Window, S);
   end;

  procedure ShowAbout;
  var
    SList: TStringList;
  begin
    SList := TStringList.Create;
    try
      AddStrArrayToStrings([
        'Keys (the ones not documented already in the menu):',
        'Arrows: move image,',
        'Arrows + Ctrl: move image 10 x faster,',
        '- / +: scale image,',
        'x / X: scale only horizontally,',
        'y / Y: scale only vertically.',
        ''], SList);
      Strings_AddCastleEngineProgramHelpSuffix(SList,
        DisplayApplicationName, Version, false);

      { Don't show this, long and useless for normal user:
      AddStrArrayToStrings([
        '',
        Format('Image list (%d images) :', [Images.Count])], SList);
      SList.AddStrings(Images); }

      MessageOK(Window, SList);
    finally SList.Free end;
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
        DestroyGLImage;
        Image.Clear(Color);
        CreateGLImage;
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
      DestroyGLImage;
      Image.Resize(ResizeToX, ResizeToY, Interpolation);
      CreateGLImage;
    end;
  end;

  procedure DoResize(const Interpolation: TResizeNiceInterpolation);
  var
    ResizeToX, ResizeToY: Cardinal;
    NewImage: TCastleImage;
  begin
    ResizeToX := 0;
    ResizeToY := 0;
    if MessageInputQueryCardinal(Window,
      'Resize to given Width (leave 0 to keep current)', ResizeToX) then
    if MessageInputQueryCardinal(Window,
      'Resize to given Height (leave 0 to keep current)', ResizeToY) then
    begin
      try
        NewImage := Image.MakeResized(ResizeToX, ResizeToY, Interpolation);
      except
        on E: Exception do
        begin
          MessageOK(Window, 'Resizing failed: ' + ExceptMessage(E));
          Exit;
        end;
      end;

      CreateImage(Window, NewImage, ImageURL);
    end;
  end;

var
  change: TGLfloat;
begin
  case Item.IntData of
    110: ImageOpen;
    120: ImageSave;
    140: Window.Close;

    210: begin
          change:=(Window.Width / Image.Width) / zoomx;
          MultZoomGL(ZoomX, change);
          MultZoomGL(ZoomY, change);
         end;
    211: begin
          change:=(Window.Height / Image.Height) / zoomy;
          MultZoomGL(ZoomX, change);
          MultZoomGL(ZoomY, change);
         end;
    220: SetZoomGL(ZoomX, Window.Width / Image.Width);
    221: SetZoomGL(ZoomY, Window.Height / Image.Height);
    230: DrawTiled := not DrawTiled;
    240: begin
          SetZoomGL(ZoomX, 1.0);
          SetZoomGL(ZoomY, 1.0);
          MoveX := 0;
          MoveY := 0;
         end;

    260: Window.ColorDialog(BackgroundColor);
    270: UseImageAlpha := not UseImageAlpha;

    310: ChangeCurrentImageIndex(-1);
    311: ChangeCurrentImageIndex(+1);

    320: EasyChangeCompositeImageIndex(-1);
    321: EasyChangeCompositeImageIndex(+1);

    410: if CheckNotGrayscale then
         begin
          DestroyGLImage;
          Image.Grayscale;
          CreateGLImage;
         end;
    420..422:
         if CheckNotGrayscale then
         begin
          DestroyGLImage;
          Image.ConvertToChannelRGB(Item.IntData - 420);
          CreateGLImage;
         end;
    430..432:
         if CheckNotGrayscale then
         begin
          DestroyGLImage;
          Image.StripToChannelRGB(Item.IntData - 430);
          CreateGLImage;
         end;
    440: begin
           DestroyGLImage;
           Image.FlipHorizontal;
           CreateGLImage;
         end;
    441: begin
           DestroyGLImage;
           Image.FlipVertical;
           CreateGLImage;
         end;

    450: begin
           DestroyGLImage;
           Image.Rotate(1);
           CreateGLImage;
         end;
    451: begin
           DestroyGLImage;
           Image.Rotate(2);
           CreateGLImage;
         end;
    452: begin
           DestroyGLImage;
           Image.Rotate(3);
           CreateGLImage;
         end;
    460: ImageClear;
    510: ShowImageInfo;
    520: ShowAbout;
    600: DoResize(riNearest);
    610: DoResize(riBilinear);
    1600..1700: DoResize(TResizeNiceInterpolation(Item.IntData - 1600));
    else SetCurrentImageIndex(Item.IntData - 10000);
  end;
  Window.Invalidate;
end;

{ This assumes that Images is empty, so be sure to call this before
  adding something to Images. For simplicity of "control flow" we do NOT
  call here ImagesChanged. }
function CreateMainMenu: TMenu;
var
  M, M2: TMenu;
  NextRecentMenuItem: TMenuEntry;
  NiceInterpolation: TResizeNiceInterpolation;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('_Open ...',                 110, CtrlO));
    M.Append(TMenuItem.Create('_Save ...',                 120, CtrlS));
    NextRecentMenuItem := TMenuSeparator.Create;
    M.Append(NextRecentMenuItem);
    RecentMenu.NextMenuItem := NextRecentMenuItem;
    M.Append(TMenuItem.Create('_Exit',                     140, CharEscape));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItem.Create('Fit image to window _width',          210, 'w'));
    M.Append(TMenuItem.Create('Fit image to window _height',         211, 'h'));
    M.Append(TMenuItem.Create('Fit image width to window width',    220, 'W'));
    M.Append(TMenuItem.Create('Fit image height to window height',  221, 'H'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create(
      'Testing is image "_tileable" on/off',                         230, 't',
      DrawTiled, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('No _zoom and no translation',         240, K_Home));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Use Image Alpha Channel', 270,  UseImageAlpha, true));
    M.Append(TMenuItem.Create('Background color ...',                260));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemToggleFullScreen.Create(Window.FullScreen));
    Result.Append(M);
  M := TMenu.Create('_Edit');
    M.Append(TMenuItem.Create('Resize (Nearest) ...',                600));
    M.Append(TMenuItem.Create('Resize (Bilinear) ...',               610));
    M2 := TMenu.Create('Resize (Slower but Nicer Algorithms)');
      for NiceInterpolation := Low(NiceInterpolation) to High(NiceInterpolation) do
        M2.Append(TMenuItem.Create('Resize (' +
          GetEnumName(TypeInfo(TResizeNiceInterpolation), Ord(NiceInterpolation)) +
          ') ...',  1600 + Ord(NiceInterpolation)));
      M.Append(M2);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Grayscale',                          410));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Convert to _red channel',             420));
    M.Append(TMenuItem.Create('Convert to gr_een channel',           421));
    M.Append(TMenuItem.Create('Convert to _blue channel',            422));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Strip to red channel',                430));
    M.Append(TMenuItem.Create('Strip to green channel',              431));
    M.Append(TMenuItem.Create('Strip to blue channel',               432));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Mirror horizontally',                440));
    M.Append(TMenuItem.Create('Mirror vertically',                   441));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Rotate 90 Degrees (Clockwise)', 450));
    M.Append(TMenuItem.Create('Rotate 180 Degrees', 451));
    M.Append(TMenuItem.Create('Rotate 90 Degrees (Counter-Clockwise)', 452));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Clear ...', 460));
    Result.Append(M);
  M := TMenu.Create('_Images');
  ImagesMenu := M;
    M.Append(TMenuItem.Create('_Previous subimage in Composite (DDS, KTX)', 320, CtrlP));
    M.Append(TMenuItem.Create('_Next subimage in Composite (DDS, KTX)',     321, CtrlN));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Previous image', 310, 'p'));
    M.Append(TMenuItem.Create('_Next image',     311, 'n'));
    M.Append(TMenuSeparator.Create);
    Result.Append(M);
  M := TMenu.Create('_Help');
    M.Append(TMenuItem.Create('Image Information', 510, CtrlI));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('About glViewImage', 520));
    Result.Append(M);
end;

{ params ------------------------------------------------------------------- }

const
  Options: array[0..1]of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone)
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
            RecognizedPatterns += ' ' + Pattern;
          end;
        InfoWrite(
          'glViewImage: simple image viewer. Allows browsing images list,' +nl+
          '  allows to scale and move viewed image, allows to test visually' +nl+
          '  is image "tileable".' +nl+
          nl+
          'Usage:' +nl+
          '  glViewImage [OPTIONS]... [IMAGES]...' +nl+
          nl+
          'You can give as many image names on the command line as you like' +nl+
          '(you will be able to switch between them using n/p (next/previous)' +nl+
          'keys). Each image name will be treated as a mask with special chars' +nl+
          '* (any number of any chars) and ? (any char), e.g.' +nl+
          '  glViewImage *.png' +nl+
          'will open any png images (i.e., even if the shell itself will not expand' +nl+
          '*.png). Non-existing image names (so, also filename masks not matching any'+nl+
          'existing filename) will be ignored.' +nl+
          nl+
          'Instead of image name, you can give parameter starting with "@" :' +nl+
          'parameter "@file_list.txt" means "read image names to load' +nl+
          'from the file file_list.txt - one image name per line".' +nl+
          nl+
          'Not giving any image names for glViewImage to load will have the same' +nl+
          'effect as calling' +nl+
          '  glViewImage' +RecognizedPatterns +nl+
          'so all images in known format (in the current directory) will be loaded.' +nl+
          nl+
          'Accepted command-line options:' +nl+
          HelpOptionHelp+ nl+
          VersionOptionHelp +nl+
          nl+
          TCastleWindowCustom.ParseParametersHelp(StandardParseOptions, true) +nl+
          nl+
          'By default, window size will be the same as of the first loaded image.'+nl+
          nl+
          SCastleEngineProgramHelpSuffix(DisplayApplicationName, Version, true));
        Halt;
      end;
    1:begin
        WritelnStr(Version);
        Halt;
      end;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ main part ----------------------------------------------------------------- }

var
  i: Integer;
  SpecifiedOptions: TWindowParseOptions;
begin
  Window := TCastleWindowCustom.Create(Application);
  Theme.DialogsLight;

  OnWarning := @OnWarningWrite;

  Images := TStringList.Create;
  try
    { init menu things. We must do it before we add something to Images,
      this is required by CreateMainMenu. }
    RecentMenu := TWindowRecentFiles.Create(nil);
    RecentMenu.OnOpenRecent := @THelper(nil).FileOpen;
    Window.MainMenu := CreateMainMenu;
    Window.OnMenuClick := @MenuClick;

    UserConfig.Load;
    RecentMenu.LoadFromConfig(UserConfig);

    { parse glw options }
    Window.ParseParameters(StandardParseOptions, SpecifiedOptions);
    { parse our options }
    Parameters.Parse(Options, @OptionProc, nil);

    { calculate Images = parse the list of image files to open }
    if Parameters.High = 0 then
    begin
      AddImageNamesAllLoadable('');
      Images.Sort;
    end else
    begin
      for i := 1 to Parameters.High do
      begin
        if SCharIs(Parameters[i], 1, '@') then
          AddImageNamesFromFile(SEnding(Parameters[i], 2)) else
        if DirectoryExists(Parameters[i]) then
          AddImageNamesAllLoadable(InclPathDelim(Parameters[i])) else
        if FileExists(Parameters[i]) then
          AddImageNamesFromFileName(Parameters[i]) else
          SavedErrorMessages += 'File "' + Parameters[i] + '" does not exist.' + NL;
      end;
    end;

    { Always insert wecome image }
    Images.InsertObject(0, '<Welcome image>', Welcome);
    Assert(Images.Count > 0);

    { By default, view the first loaded image, or welcome image if nothing loaded. }
    if Images.Count > 1 then
      CurrentImageIndex := 1 else
      CurrentImageIndex := 0;

    ImagesChanged;

    { initialize Image. This must be done before window is created,
      as we want to use Image size for initial window size. }
    try
      CreateNonGLImageFromList(CurrentImageIndex, false);
    except
      on E: ECannotDecompressTexture do
      begin
        { Silence warning in this case, image size cannot be known
          before we initialize OpenGL context.
          Leave Image = nil, Open callback will initialize it. }
      end;
      on E: Exception do
      begin
        SavedErrorMessages += ExceptMessage(E, nil) + NL;
        CreateNonGLImageFromList(CurrentImageIndex, true);
      end;
    end;

    { set window size, if we managed to load image before Open callback,
      and user did not already request some size }
    if (Image <> nil) and not (poGeometry in SpecifiedOptions) then
    begin
      { Clamp to:
        - not make window too large (some window managers accept it
          and show window that's uncomfortable for user, covers top/bottom
          panels and such),
        - not make window too small (as then messages ("about", "image info",
          errors when reading images etc.) and other things are uncomfortable.
          Remember that Image.Width/Height may come from ImageInvalid
          if command-line image is invalid.)
          Under Windows, this also should avoid menu bar wrapping (at least with
          typical themes), which avoids accidentaly creating too small OpenGL area
          and having to display scrollbars. (see CastleWindow WinAPI comments about
          AdjustWindowRectEx, this is documented WinAPI bug without any sensible
          workaround.) }
      Window.width  := Clamped(Image.Width , 400, Application.ScreenWidth -50);
      Window.height := Clamped(Image.Height, 400, Application.ScreenHeight-50);
    end;

    Window.OnUpdate := @Update;
    Window.OnRender := @Render;
    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    Window.OnResize := @Resize2D;
    Window.OnDropFiles := @DropFiles;

    Window.DepthBits := 0; { depth buffer not needed here }
    Window.OpenAndRun;

    RecentMenu.SaveToConfig(UserConfig);
    UserConfig.Save;
  finally
    FreeAndNil(Images);
    FreeAndNil(RecentMenu);
  end;
end.
