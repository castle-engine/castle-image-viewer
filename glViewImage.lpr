{
  Copyright 2001-2010 Michalis Kamburelis.

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

{$apptype GUI}

uses GLWindow, GL, GLU, GLExt, KambiGLUtils, SysUtils, KambiUtils, Images,
  OpenGLBmpFonts, OpenGLFonts, Math, Classes, KambiClassUtils,
  GLWinMessages, ImageLoading, ParseParametersUnit, GVIImages, EnumerateFiles,
  VectorMath, KambiStringUtils, DataErrors, GLImages, FileFilters,
  GLWindowRecentFiles, GVIConfig, DDS;

var
  Glw: TGLWindowDemo;
  MoveX: TGLfloat = 0;
  MoveY: TGLfloat = 0;
  DrawTiled: boolean = false;
  BackgroundColor: TVector3Single;
  SavedErrorMessage: string = '';

{ Lista nazw obrazkow --------------------------------------------------------

  Items on ImageNamesList may be relative filenames, so current dir of this
  program must stay constant for all time.

  A special case is an item with Object[] property <> @nil:
  this means that an internal image, which should be loaded from
  TImage(Object[]), should be loaded. Display name for user
  should be the value of given string.
  For now, this is only used to load Welcome image, possibly will
  be used for other purposes at some time.

  ImageNamesListPos zawsze wskazuje na jakis istniejacy
  indeks 0..ImageNamesList.Count-1. Lista ImageNamesList tym samym zawsze
  musi miec Count > 0.

  Tym niemniej, nie jest gwarantowane ze aktualny ImageFileName to
  ImageNamesList[ImageNamesListPos]. Byc moze np. kiedys zrobie
  ImageOpen w wersji "bez dodawania do listy obrazkow ?".
  Poza tym ImageFileName jest undefined gdy not IsImageValid.

  After changing this call ImageNamesListChanged; }
var
  ImageNamesList: TStringList;
  ImageNamesListPos: Integer = 0; { change this only with SetImageNameListPos }

{ This changes ImageNamesListPos and tries to load newly choosen image.
  Uses CreateImage(glw, ImageNamesList[ImageNamesListPos]) if it's a normal
  (not internal) image, or something else as appropriate.
  Then PostRedisplay; }
procedure SetImageNamesListPos(NewValue: Integer);
begin
 ImageNamesListPos := NewValue;

 if ImageNamesList.Objects[ImageNamesListPos] = nil then
   CreateImage(Glw, ImageNamesList[ImageNamesListPos]) else
   CreateImage(Glw,
     TImage(ImageNamesList.Objects[ImageNamesListPos]).MakeCopy,
     ImageNamesList[ImageNamesListPos]);

 glw.PostRedisplay;
end;

procedure ChangeImageNamesListPos(Change: Integer);
begin
 SetImageNamesListPos(ChangeIntCycle(
   ImageNamesListPos, Change, ImageNamesList.Count-1));
end;

{ operations on ImageNamesList ---------------------------------------------
  They all do NOT call ImageNamesListChanged automatically. }

procedure AddImageNamesFromTextReader(Reader: TTextReader);
var s: string;
begin
 while not Reader.Eof do
 begin
  s := Trim( Reader.Readln );
  if s <> '' then ImageNamesList.Append(s);
 end;
end;

procedure AddImageNamesFromFile(const fname: string);
var f: TTextReader;
begin
 if fname = '-' then
  AddImageNamesFromTextReader(StdinReader) else
 begin
  f := TTextReader.CreateFromFileStream(fname);
  try
   AddImageNamesFromTextReader(f);
  finally f.Free end;
 end;
end;

procedure AddToList(const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
 { add NiceFileName(fullname) instead of FullName, because that's
   more useful (shorter text) for user. }
 ImageNamesList.Append(NiceFileName(FileInfo.FullFileName));
end;

procedure AddImageNamesFromMask(const mask: string);
begin
 EnumFiles(Mask, RegularFileAttr, @AddToList, nil, [eoSymlinks]);
end;

{ Path can be '' or must end with '/' }
procedure AddImageNamesAllLoadable(const Path: string);
var iff: TImageformat;
    i: integer;
begin
 for iff := Low(iff) to High(iff) do
  for i := 1 to ImageformatInfos[iff].extsCount do
   AddImageNamesFromMask(Path + '*.' + ImageFormatInfos[iff].exts[i]);
end;

{ recent files --------------------------------------------------------------- }

procedure ImageNamesListChanged; forward;

type
  THelper = class
    class procedure FileOpen(const FileName: string);
  end;

class procedure THelper.FileOpen(const FileName: string);
begin
  ImageNamesList.Insert(ImageNamesListPos+1, FileName);
  ImageNamesListChanged;
  ChangeImageNamesListPos(+1);

  { temporary unused code: this is how to open an image without adding it to
    ImageNamesList: CreateImage(glwin, FileName); }
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
 glw.PostRedisplay;
end;

procedure MultZoomGL(var Zoom: Single; const Multiplier: Single);
begin
 Zoom *= Multiplier;
 Clamp(Zoom, MinZoom, MaxZoom);
 glw.PostRedisplay;
end;

{ glw callbacks ---------------------------------------------------------- }

procedure DrawGL(glwin: TGLWindow);

  procedure Arrow(const Angle: TGLfloat);
  begin
   glTranslatef(glwin.width div 2, glwin.height div 2, 0);
   glRotatef(Angle, 0, 0, 1);
   glColorv(Yellow3Single);
   glScalef(50, 50, 0);
   drawArrow;
  end;

  procedure drawImage(MoveX, MoveY: Single);
  {rysuje obrazek Image zgodnie z (zoomx, zomy) i zadanym (MoveX, MoveY).
   (a wiec mozesz zadac move inne niz globalne).
   Zaklada ze glPixelZoom(zoomx, zoomy) zostalo juz wykonane !}
  var rx, ry: Single;
      cutx, cuty: Cardinal;
  begin
   { na potrzeby przesuwania sie pod ekranie MoveX i MoveY powinny byc float.
     Ale gdy przychodzi do wyswietlania - wygodnie jest jesli move jest integerem.
     Poza tym i tak mnozymy move razy zoom. }
   rx := MoveX*zoomX;
   ry := MoveY*zoomY;

   { jezeli rX jest ujemne nie mozemy go przekazac glRasterPos bo wtedy sprawimy
     ze raster position bedzie invalid i glDrawPixels nic nie zrobi. Zamiast
     tego jesli rX bedzie < 0 to po prostu pomijamy pierwsze -MoveX kolumn
     z obrazka i ustawiamy rx na 0.
     Podobnie robimy z MoveY. }
   if rx < 0 then begin cutx := -Round(MoveX); rx := 0 end else cutx := 0;
   if ry < 0 then begin cuty := -Round(MoveY); ry := 0 end else cuty := 0;

   glRasterPos2f(rx, ry);

   if Image.HasAlpha then
   begin
     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
     glEnable(GL_BLEND);
   end;

   { uzywanie display listy kiedy tylko mozemy pozwala nam znacznie przyspieszyc
     rysowanie image'ow (zwlaszcza gdy mamy male zoom i widac bardzo wiele
     obrazkow w okienku (zdecydowana wiekszosc z nich bedzie mogla byc rysowana
     display-lista).}
   if DrawTiled then
   begin
    if (cutx = 0) and (cuty = 0) then
     glCallList(dlDrawImageExpand) else
     ImageDrawPart(ImageExpand, cutX, cutY);
   end else
   begin
    if (cutx = 0) and (cuty = 0) then
     glCallList(dlDrawImage) else
     ImageDrawPart(Image, cutX, cutY);
   end;

   if Image.HasAlpha then
     glDisable(GL_BLEND);
  end;

var visibleXStart, visibleXEnd,
    visibleYStart, visibleYEnd,
    Width, Height, i, j, i0, i1, j0, j1: integer;
    horizPrzewijak, vertPrzewijak: boolean;
const przewThick = 10; { na ile pixeli gruby jest przewijak (tzn. cale obramowanie przewijaka) }
      przewMarg = 2; { margines miedzy obramowaniem przewijaka a paskiem we wnetrzu }
begin
 glLoadIdentity;

 { fpc zle sobie radzi gdy przychodzi do porownywania integerow i cardinali :
   wybiera wspolny typ jako cardinal - tragiczny blad. Przydadza nam sie tutaj
   Width i Height jako int. }
 Width := Image.Width;
 Height := Image.Height;

 { Clear color buffer bit, even when DrawTiled --- because image may
   have alpha channel, and then background is visible. }
 glClear(GL_COLOR_BUFFER_BIT);

 if DrawTiled then
 begin
  { MoveX to wspolrzedna X zasadniczego obrazka (tego ktory bylby
    wyswietlany z not DrawTiled). Znajdujemy i0 takie ze jest to najwieksze
    i dla ktorego (MoveX + i*Width)*zoomX <= 0 a wiec jest to
    i dla lewej kolumny kafelkow. Podobnie, i1 dla prawej kolumny
    kafelkow to najmniejsze i t.ze (MoveX + (i+1)*Width)*zoomx >= glwin.width.
    Podobnie znajdujemy j0, j1.

    (operujemy przesuwajac MoveX o Width ale pamietamy ze
    faktyczna pozycja obrazka w pixelach to pozycja*zoom).

    (MoveX + i0*Width)*zoomX <= 0 wiec
      i0 <= -MoveX/Width wiec
      i0 = Floor(-MoveX/Width)
    (MoveX + (i1 + 1)*Width)*zoomx >= glwin.Width wiec
      i1 >= (glwin.Width/zoomx - MoveX)/Width - 1 wiec
      i1 = Ceil((glwin.width/zoomx - MoveX)/Width - 1)
  }

  i0 := Floor(-MoveX/Width);
  i1 := Ceil((glwin.Width/zoomX - MoveX)/Width - 1);
  j0 := Floor(-MoveY/Height);
  j1 := Ceil((glwin.Height/zoomY - MoveY)/Height - 1);

  { As strange as it seems, some graphic cards (mine NVidia GForce2 MX 100/200
    with drivers Linux-x86-1.0-6629) require that in case the image is large
    and it's top-right corner is outside the window, I must explicitly
    use scissor to cut it down, otherwise strange artifacts (black area
    at the bottom of the window) appear. }
  glScissor(0, 0, glwin.width, glwin.height);
  glEnable(GL_SCISSOR_TEST);

  glPixelZoom(zoomX, zoomY);
  for i := i0 to i1 do
   for j := j0 to j1 do
    drawImage(MoveX + i*Width, MoveY + j*Height);
  glPixelZoom(1, 1);

  glDisable(GL_SCISSOR_TEST);
 end else
 begin
  visibleXStart := -Round(MoveX);
  visibleXEnd   := Round(glwin.width/zoomX - MoveX);
  visibleYStart := -Round(MoveY);
  visibleYEnd   := Round(glwin.height/zoomY - MoveY);

  if visibleXStart > Width  then Arrow(90) else
  if visibleXEnd < 0        then Arrow(-90) else
  if visibleYStart > Height then Arrow(180) else
  if visibleYEnd < 0        then Arrow(0) else
  begin
   Clamp(visibleXStart, 0, Width);
   Clamp(visibleXEnd  , 0, Width);
   Clamp(visibleYStart, 0, Height);
   Clamp(visibleYEnd  , 0, Height);

   horizPrzewijak := (visibleXStart > 0) or (visibleXEnd < Width);
   vertPrzewijak := (visibleYStart > 0) or (visibleYEnd < Height);

   if horizPrzewijak and vertPrzewijak then
   begin
    glScissor(przewThick+1, przewThick+1, glwin.width, glwin.height);
    glEnable(GL_SCISSOR_TEST);
   end else
   if horizPrzewijak then
   begin
    glScissor(0, przewThick+1, glwin.width, glwin.height);
    glEnable(GL_SCISSOR_TEST);
   end else
   if vertPrzewijak then
   begin
    glScissor(przewThick+1, 0, glwin.width, glwin.height);
    glEnable(GL_SCISSOR_TEST);
   end else
   begin
    { This seems useless, but see comments for analogous code for the case
      DrawTiled = true. }
    glScissor(0, 0, glwin.width, glwin.height);
    glEnable(GL_SCISSOR_TEST);
   end;

   glPixelZoom(zoomX, zoomY);
   drawImage(MoveX, MoveY);
   glPixelZoom(1, 1);

   glDisable(GL_SCISSOR_TEST);

   if horizPrzewijak then
   begin
    glColorv(Yellow3Single);
    glBegin(GL_LINES);
     glVertex2f(przewThick, przewThick);
     glVertex2f(glwin.width, przewThick);
     glVertex2f(przewThick, 0);
     glVertex2f(przewThick, przewThick);
    glEnd;

    visibleXStart := Round(MapRange(visibleXStart, 0, Width, przewThick+przewMarg, glwin.width-przewMarg));
    visibleXEnd   := Round(MapRange(visibleXEnd,   0, Width, przewThick+przewMarg, glwin.width-przewMarg));
    glColorv(Gray3Single);
    glRecti(visibleXStart, przewMarg, visibleXEnd, przewThick-przewMarg);
   end;

   if vertPrzewijak then
   begin
    glColorv(Yellow3Single);
    glBegin(GL_LINES);
     glVertex2f(przewThick, przewThick);
     glVertex2f(przewThick, glwin.height);
     glVertex2f(0, przewThick);
     glVertex2f(przewThick, przewThick);
    glEnd;

    visibleYStart := Round(MapRange(visibleYStart, 0, Height, przewThick+przewMarg, glwin.height-przewMarg));
    visibleYEnd  :=Round(MapRange(visibleYEnd,   0, Height, przewThick+przewMarg, glwin.height-przewMarg));
    glColorv(Gray3Single);
    glRecti(przewMarg, visibleYStart, przewThick-przewMarg, visibleYEnd);
   end;
  end;
 end;
end;

procedure IdleGL(glwin: TGLWindow);

  procedure MoveGL(var value: TGLfloat; change: TGLfloat);
  begin
   change *= 8*glwin.Fps.IdleSpeed * 50;
   if glw.Pressed[k_Ctrl] then change *= 10;
   value += change;
   glw.PostRedisplay;
  end;

const SCALE_FACTOR = 0.1;
var scale_up, scale_down: Single;
begin
 with glw do begin
  if Pressed[K_Up] then moveGL(MoveY, -1 / zoomY);
  if Pressed[K_Down] then moveGL(MoveY, 1 / zoomY);
  if Pressed[K_Right] then moveGL(MoveX, -1 / zoomX);
  if Pressed[K_Left] then moveGL(MoveX, 1 / zoomX);

  scale_up := 1 + SCALE_FACTOR * glw.Fps.IdleSpeed * 50;
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

procedure InitGL(glwin: TGLWindow);
begin
 DecompressS3TC := @GLDecompressS3TC;

 CreateGLImage;

 if SavedErrorMessage <> '' then
 begin
   MessageOk(Glwin, SavedErrorMessage, taLeft);
   SavedErrorMessage := '';
 end;
end;

procedure CloseGL(glwin: TGLWindow);
begin
 DestroyGLImage;
end;

{ menu ------------------------------------------------------------ }

const
  Version = '1.3.1';
  DisplayProgramName = 'glViewImage';

var
  { initialized in CreateMainMenu, then updated in each ImagesNamesListChanged. }
  ImageListMenu: TMenu;

{ After changing ImageNamesList contents always call this.
  This ensures that ImageListMenu is properly updated. }
procedure ImageNamesListChanged;
var i: Integer;
begin
 while ImageListMenu.EntriesCount > 6 do
  ImageListMenu.EntryDelete(6);

 { Do not load all ImageNamesList.Count to the menu,
   as this makes menu very large, making updating it (e.g. when user
   loads new image) time-consuming, and it's not possible to navigate
   it anyway. }

 for i := 0 to Min(ImageNamesList.Count, 20) - 1 do
  ImageListMenu.Append(TMenuItem.Create(
    SQuoteMenuEntryCaption(ImageNamesList[i]), 10000 + i));
end;

procedure MenuCommand(glwin: TGLWindow; Item: TMenuItem);

  procedure ImageSave;
  var FileName: string;
  begin
   if IsImageValid then
   begin
    FileName := ImageFileName;
    if glwin.FileDialog('Save image to file', FileName, false, SaveImage_FileFilters) then
    try
      if DDSImage <> nil then
      begin
        if FileExtToImageFormatDef(ExtractFileExt(FileName), false, false,
          ifBMP) = ifDDS then
        begin
          DDSImage.SaveToFile(FileName);
        end else
        begin
          if MessageYesNo(Glwin, 'DDS image is composed from many single (normal, simple, 2D) images. Saving from DDS image to other file format will only save the current single image layer. Continue?', taLeft) then
            SaveImage(Image, FileName);
        end;
      end else
        SaveImage(Image, FileName);
    except
      on E: EUnableToSaveImage do
        MessageOk(Glwin, Format('Saving image "%s" failed: %s', [FileName, E.Message]), taLeft);
    end;
   end else
    MessageOK(glwin, 'No valid image loaded', taMiddle);
  end;

  procedure ImageOpen;
  var FileName: string;
  begin
   if IsImageValid then
    FileName := ExtractFilePath(ImageFileName) else
    FileName := '';
   if glwin.FileDialog('Load image from file', FileName, true, LoadImage_FileFilters) then
   begin
     THelper.FileOpen(FileName);
   end;
  end;

  procedure ShowImageInfo;

    function DDSImageInfo(DImg: TDDSImage): string;
    begin
      Result := Format(
        'Containing DDS image info:' +NL+
        '  Width x Height x Depth: %d x %d %d'  +NL+
        '  Type: %s' +NL+
        '  Mipmaps: %s' +NL+
        '  Simple 2D images inside: %d',
        [ DImg.Width, DImg.Height, DImg.Depth,
          DDSTypeToString[DImg.DDSType],
          BoolToStr[DImg.Mipmaps],
          DImg.Images.Count ]);
    end;

  var
    S: string;
  begin
    if IsImageValid then
      S := Format(
        'File name: %s' + NL +
        'Class: %s' +NL +
        'Width x height: %d x %d',
        [ImageFileName, Image.ClassName, Image.Width, Image.Height]) else
      S := '<invalid image file>';
    if DDSImage <> nil then
      S += NL + Format('DDS subimage: %d', [DDSImageIndex]) + NL +
        DDSImageInfo(DDSImage);
    MessageOK(Glwin, S, taLeft);
   end;

  procedure ShowAbout;
  var SList: TStringList;
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
    Strings_AddVrmlEngineProgramHelpSuffix(SList,
      DisplayProgramName, Version, false);

    { Don't show this, long and useless for normal user:
    AddStrArrayToStrings([
      '',
      Format('Image list (%d images) :', [ImageNamesList.Count])], SList);
    SList.AddStrings(ImageNamesList); }

    MessageOK(glwin, SList, taLeft);
   finally SList.Free end;
  end;

  function CheckNotGrayscale: boolean;
  begin
    Result := (Image is TRGBImage) or (Image is TRGBAlphaImage);
    if not Result then
      MessageOk(Glwin, 'This function is not available for grayscale images.');
  end;

  procedure ImageClear;
  var
    Color4: TVector4Byte;
    Color3: TVector3Byte absolute Color4;
  begin
    if CheckNotGrayscale then
    begin
      Color3 := Vector3Byte(255, 255, 255);
      if glwin.ColorDialog(Color3) then
      begin
        Color4[3] := 255;
        DestroyGLImage;
        Image.Clear(Color4);
        ImageExpand.Clear(Color4);
        CreateGLImage;
      end;
    end;
  end;

  procedure EasyChangeDDSImageIndex(Change: Integer);
  begin
    if DDSImage <> nil then
      ChangeDDSImageIndex(Glwin, ChangeIntCycle(
        DDSImageIndex, Change, DDSImage.Images.Count - 1)) else
      MessageOk(Glwin, 'Available only for DDS images.');
  end;

var change: TGLfloat;
begin
 case Item.IntData of
  110: ImageOpen;
  120: ImageSave;
  140: glwin.Close;

  210: begin
        change:=(glwin.Width / Image.Width) / zoomx;
        MultZoomGL(ZoomX, change);
        MultZoomGL(ZoomY, change);
       end;
  211: begin
        change:=(glwin.Height / Image.Height) / zoomy;
        MultZoomGL(ZoomX, change);
        MultZoomGL(ZoomY, change);
       end;
  220: SetZoomGL(ZoomX, glwin.Width / Image.Width);
  221: SetZoomGL(ZoomY, glwin.Height / Image.Height);
  230: DrawTiled := not DrawTiled;
  240: begin
        SetZoomGL(ZoomX, 1.0);
        SetZoomGL(ZoomY, 1.0);
        MoveX := 0;
        MoveY := 0;
       end;
  250: glw.SwapFullScreen;

  260: if glwin.ColorDialog(BackgroundColor) then
         glClearColor(BackgroundColor[0], BackgroundColor[1], BackgroundColor[2], 1);

  310: ChangeImageNamesListPos(-1);
  311: ChangeImageNamesListPos(+1);

  320: EasyChangeDDSImageIndex(-1);
  321: EasyChangeDDSImageIndex(+1);

  410: if CheckNotGrayscale then
       begin
        DestroyGLImage;
        Image.Grayscale;
        ImageExpand.Grayscale;
        CreateGLImage;
       end;
  420..422:
       if CheckNotGrayscale then
       begin
        DestroyGLImage;
        Image.ConvertToChannelRGB(Item.IntData - 420);
        ImageExpand.ConvertToChannelRGB(Item.IntData - 420);
        CreateGLImage;
       end;
  430..432:
       if CheckNotGrayscale then
       begin
        DestroyGLImage;
        Image.StripToChannelRGB(Item.IntData - 430);
        ImageExpand.StripToChannelRGB(Item.IntData - 430);
        CreateGLImage;
       end;
  440: begin
         DestroyGLImage;
         Image.FlipHorizontal;
         RemakeImageExpand;
         CreateGLImage;
       end;
{  441: begin
         DestroyGLImage;
         Image.FlipVertical;
         RemakeImageExpand;
         CreateGLImage;
       end;}

  450: begin
         DestroyGLImage;
         Image.Rotate(1);
         RemakeImageExpand;
         CreateGLImage;
       end;
  451: begin
         DestroyGLImage;
         Image.Rotate(2);
         RemakeImageExpand;
         CreateGLImage;
       end;
  452: begin
         DestroyGLImage;
         Image.Rotate(3);
         RemakeImageExpand;
         CreateGLImage;
       end;
  460: ImageClear;
  510: ShowImageInfo;
  520: ShowAbout;
  else
   SetImageNamesListPos(Item.IntData - 10000);
 end;
 glwin.PostRedisplay;
end;

{ This assumes that ImageNamesList is empty, so be sure to call this before
  adding something to ImageNamesList. For simplicity of "control flow" we do NOT
  call here ImageNamesListChanged. }
function CreateMainMenu: TMenu;
var
  M: TMenu;
  NextRecentMenuItem: TMenuEntry;
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
   M.Append(TMenuItem.Create('Background color ...',                260));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('_FullScreen on/off',           250, K_F11,
     glw.FullScreen, true));
   Result.Append(M);
 M := TMenu.Create('_Edit');
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
   M.Append(TMenuItem.Create('_Mirror horizontally',                 440));
   { TODO: too lazy to implement it right now.
   M.Append(TMenuItem.Create('Mirror vertically',                   441));
   }
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Rotate 90 Degrees (Clockwise)', 450));
   M.Append(TMenuItem.Create('Rotate 180 Degrees', 451));
   M.Append(TMenuItem.Create('Rotate 90 Degrees (Counter-Clockwise)', 452));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Clear ...', 460));
   Result.Append(M);
 M := TMenu.Create('_Images');
 ImageListMenu := M;
   M.Append(TMenuItem.Create('_Previous subimage in DDS', 320, CtrlP));
   M.Append(TMenuItem.Create('_Next subimage in DDS',     321, CtrlN));
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
var RecognizedExts: string;
    iff: TImageformat;
    i: integer;
begin
 case OptionNum of
  0: begin
      RecognizedExts := '';
      for iff := Low(iff) to High(iff) do
       for i := 1 to ImageformatInfos[iff].extsCount do
        RecognizedExts += ' *.'+ImageFormatInfos[iff].exts[i];

      InfoWriteParts('glViewImage help - part %d / %d',
       ['glViewImage: simple image viewer. Allows browsing images list,' +nl+
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
        'from the file file_list.txt - one image name per line".' +nl,

        'Not giving any image names for glViewImage to load will have the same' +nl+
        'effect as calling' +nl+
        '  glViewImage' +RecognizedExts +nl+
        'so all images in known format (in the current directory) will be loaded.' +nl+
        nl+
        'Accepted command-line options:' +nl+
        HelpOptionHelp+ nl+
        VersionOptionHelp +nl+
        nl+
        TGLWindow.ParseParametersHelp(StandardParseOptions, true) +nl+
        nl+
        'By default, window size will be the same as of the first loaded image.'+nl+
        nl+
        SVrmlEngineProgramHelpSuffix(DisplayProgramName, Version, true)]);

      ProgramBreak;
     end;
  1: begin
      WritelnStr(Version);
      ProgramBreak;
     end;
  else raise EInternalError.Create('OptionProc');
 end;
end;

{ main part ----------------------------------------------------------------- }

var
  i: Integer;
  SpecifiedOptions: TGLWindowParseOptions;
begin
 Glw := TGLWindowDemo.Create(Application);

 DataWarning := @DataWarning_Write;

 ImageNamesList := TStringList.Create;
 try
  { init menu things. We must do it before we add something to ImageNamesList,
    this is required by CreateMainMenu. }
  RecentMenu := TGLRecentFiles.Create(nil);
  RecentMenu.LoadFromConfig(ConfigFile, 'recent_files');
  RecentMenu.OnOpenRecent := @THelper(nil).FileOpen;
  glw.MainMenu := CreateMainMenu;
  glw.OnMenuCommand := @MenuCommand;

  { parse glw options }
  glw.ParseParameters(StandardParseOptions, SpecifiedOptions);
  { parse our options }
  ParseParameters(Options, @OptionProc, nil);

  { evaluate ImageNamesList = parse the list of image files to open }
  if Parameters.High = 0 then
  begin
   AddImageNamesAllLoadable('');
   ImageNamesList.Sort;
  end else
  begin
   for i := 1 to Parameters.High do
   begin
    if SCharIs(Parameters[i], 1, '@') then
     AddImageNamesFromFile(SEnding(Parameters[i], 2)) else
    if DirectoryExists(Parameters[i]) then
     AddImageNamesAllLoadable(InclPathDelim(Parameters[i])) else
     AddImageNamesFromMask(Parameters[i]);
   end;
  end;

  { Always insert wecome image }
  ImageNamesList.InsertObject(0, '<Welcome image>', Welcome);
  Assert(ImageNamesList.Count > 0);

  { By default, view the first loaded image, or welcome image if nothing loaded. }
  if ImageNamesList.Count > 1 then
    ImageNamesListPos := 1 else
    ImageNamesListPos := 0;

  ImageNamesListChanged;

  { initialize Image. This must be done before window is created,
    as we want to use Image size for initial window size. }
  try
    if ImageNamesList.Objects[ImageNamesListPos] = nil then
      CreateNonGLImage(glw, ImageNamesList[ImageNamesListPos]) else
      CreateNonGLImage(glw,
        TImage(ImageNamesList.Objects[ImageNamesListPos]).MakeCopy,
        ImageNamesList[ImageNamesListPos]);
  except
   on E: Exception do
   begin
    SavedErrorMessage := ExceptMessage(E, nil);
    CreateNonGLImageInvalid(glw, ImageNamesList[ImageNamesListPos]);
   end;
  end;

  {inicjuj GLWinMessages}
  GLWinMessagesTheme.TextCol := Green3Single;

  {set size, unless already requested some size}
  if not (poGeometry in SpecifiedOptions) then
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
       and having to display scrollbars. (see GLWindow WinAPI comments about
       AdjustWindowRectEx, this is documented WinAPI bug without any sensible
       workaround.) }
   glw.width  := Clamped(Image.Width , 400, Application.ScreenWidth -50);
   glw.height := Clamped(Image.Height, 400, Application.ScreenHeight-50);
  end;

  {go for it}
  glw.OnIdle := @idleGL;
  glw.OnDraw := @DrawGL;
  glw.OnInit := @InitGL;
  glw.OnClose := @CloseGL;
  glw.OnResize := @Resize2D;

  glw.Fps.Active := true;
  glw.DepthBufferBits := 0; { depth buffer not needed here }
  glw.SetDemoOptions(K_None, #0, false);
  glw.InitAndRun;

  RecentMenu.SaveToConfig(ConfigFile, 'recent_files');
 finally
  FreeAndNil(ImageNamesList);
  FreeAndNil(RecentMenu);
 end;
end.

{
  Local Variables:
  kam-compile-release-command-unix:    "./compile.sh && mv -fv glViewImage      ~/bin/"
  kam-compile-release-command-windows: "./compile.sh && mv -fv glViewImage.exe c:\\\\bin\\\\"
  End:
}
