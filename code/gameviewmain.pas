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

{ Main view, presenting UI to display everything and initialize all operations. }
unit GameViewMain;

interface

uses SysUtils, Math, Classes, TypInfo,
  CastleWindow, CastleGLUtils, CastleUtils, CastleImages,
  CastleClassUtils, CastleMessages, CastleParameters, CastleControls,
  CastleFindFiles, CastleVectors, CastleStringUtils,
  CastleGLImages, CastleWindowRecentFiles, CastleInternalCompositeImage, CastleFilesUtils,
  CastleColors, CastleConfig, CastleKeysMouse, CastleURIUtils, CastleRectangles,
  CastleApplicationProperties, CastleOpenDocument,
  CastleDownload, CastleLog, CastleRenderContext, CastleUIControls,
  GameImage, GameEmbeddedImages;

type
  TViewMain = class(TCastleView)
  private
    const
      { We must have soom constraints on Zoom* values,
        otherwise various integers on the way may overflow,
        and dividing something / ZoomX (or ZoomY) may result in weirdly huge
        error or division by zero. }
      MinZoom: Single = 0.01;
      MaxZoom: Single = 300;
    var
      MoveX: Single;
      MoveY: Single;
      DrawTiled: Boolean;
      UseImageAlpha: Boolean;
      ImageArrowLeft, ImageArrowRight, ImageArrowUp, ImageArrowDown: TCastleImageControl;

      { Zoom.
        Always 0 < MinZoom <= Zoom* <=MaxZoom.
        Set only by SetZoom/MultZoom. }
      ZoomX: Single;
      ZoomY: Single;

      RectStatus: TCastleRectangleControl;
      LabelStatus: TCastleLabel;

    procedure SetZoom(var Zoom: Single; const NewZoom: Single);
    procedure MultZoom(var Zoom: Single; const Multiplier: Single);
  public
    { Window that contains everything, including ViewMain.
      Set by main program.
      The TViewMain implementation should try to use Container,
      not Window (to be more flexible in theory, e.g. for TCastleControl) ...
      but for some things, you just need Window, go ahead. }
    Window: TCastleWindow;

    procedure Start; override;
    procedure Stop; override;
    procedure Render; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Motion(const Event: TInputMotion): Boolean; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

{ Create main menu, to be assigned to Window.MainMenu by the caller.
  Also initializes Images.Menu. }
function CreateMainMenu: TMenu;

implementation

uses GameImageList;

{ declare menu routines ------------------------------------------------------ }

procedure DropFiles(Container: TCastleContainer; const FileNames: array of string); forward;
procedure MenuClick(Container: TCastleContainer; Item: TMenuItem); forward;

{ TViewMain ------------------------------------------------------------------ }

procedure TViewMain.Start;

  procedure CreateUserInterface;

    function CreateArrowImage(const Image: TCastleImage): TCastleImageControl;
    begin
      Result := TCastleImageControl.Create(FreeAtStop);
      Result.Anchor(hpMiddle);
      Result.Anchor(vpMiddle);
      Result.Content.OwnsImage := false;
      Result.Content.Image := Image;
      Result.Color := Yellow;
      { Intial Exists = false matters in case we have an error (and make MessageOK)
        at opening 1st image. }
      Result.Exists := false;
      InsertFront(Result);
    end;

  begin
    ImageArrowLeft := CreateArrowImage(Arrow_left_circle);
    ImageArrowRight := CreateArrowImage(Arrow_right_circle);
    ImageArrowUp := CreateArrowImage(Arrow_up_circle);
    ImageArrowDown := CreateArrowImage(Arrow_down_circle);

    RectStatus := TCastleRectangleControl.Create(FreeAtStop);
    RectStatus.AutoSizeToChildren := true;
    RectStatus.Color := Vector4(0.25, 0.25, 0.25, 0.5);
    RectStatus.Border.AllSides := 1;
    RectStatus.BorderColor := Yellow;
    RectStatus.Anchor(hpRight, -10);
    RectStatus.Anchor(vpTop, -10);
    InsertFront(RectStatus);

    LabelStatus := TCastleLabel.Create(FreeAtStop);
    LabelStatus.Color := Yellow;
    LabelStatus.Padding := 10;
    LabelStatus.Html := true;
    RectStatus.InsertFront(LabelStatus);
  end;

begin
  inherited;

  // nice defaults
  UseImageAlpha := true;
  ZoomX := 1;
  ZoomY := 1;

  CreateUserInterface;

  Window.OnMenuClick := {$ifdef FPC}@{$endif} MenuClick;
  Window.OnDropFiles := {$ifdef FPC}@{$endif} DropFiles;
end;

procedure TViewMain.Stop;
begin
  Window.OnMenuClick := nil;
  Window.OnDropFiles := nil;
  inherited;
end;

procedure TViewMain.SetZoom(var Zoom: Single; const NewZoom: Single);
begin
  Zoom := Clamped(NewZoom, MinZoom, MaxZoom);
  VisibleChange([chRender]);
end;

procedure TViewMain.MultZoom(var Zoom: Single; const Multiplier: Single);
begin
  Zoom := Zoom * Multiplier;
  ClampVar(Zoom, MinZoom, MaxZoom);
  VisibleChange([chRender]);
end;

procedure TViewMain.Render;

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
  ImgWidth, ImgHeight, i, j, i0, i1, j0, j1: integer;
  HorizScrollBar, VertScrollBar: boolean;
const
  ScrollbarSize = 10; {< scrollbar thickness (with frames around) }
  ScrollbarMargin = 2; {< margin between scrollbar inside and frame }
begin
  inherited;

  Assert(Image <> nil, 'Image not loaded yet');

  { Make sure to treat ImgWidth and ImgHeight as signed values for compurations
    and comparisons below. }
  ImgWidth := Image.Width;
  ImgHeight := Image.Height;

  ImageArrowLeft.Exists := false;
  ImageArrowRight.Exists := false;
  ImageArrowUp.Exists := false;
  ImageArrowDown.Exists := false;

  if DrawTiled then
  begin
    { MoveX to wspolrzedna X zasadniczego obrazka (tego ktory bylby
      wyswietlany z not DrawTiled). Znajdujemy i0 takie ze jest to najwieksze
      i dla ktorego (MoveX + i * ImgWidth) * ZoomX <= 0 a wiec jest to
      i dla lewej kolumny kafelkow. Podobnie, i1 dla prawej kolumny
      kafelkow to najmniejsze i t.ze (MoveX + (i+1) * ImgWidth) * ZoomX >= Container.PixelsWidth.
      Podobnie znajdujemy j0, j1.

      (operujemy przesuwajac MoveX o ImgWidth ale pamietamy ze
      faktyczna pozycja obrazka w pixelach to pozycja*zoom).

      (MoveX + i0 * ImgWidth) * ZoomX <= 0 wiec
        i0 <= -MoveX / ImgWidth wiec
        i0 = Floor(-MoveX / ImgWidth)
      (MoveX + (i1 + 1) * ImgWidth) * ZoomX >= Container.PixelsWidth wiec
        i1 >= (Container.PixelsWidth / ZoomX - MoveX) / ImgWidth - 1 wiec
        i1 = Ceil((Container.PixelsWidth / ZoomX - MoveX) / ImgWidth - 1)
    }

    i0 := Floor(-MoveX / ImgWidth);
    i1 := Ceil((Container.PixelsWidth / ZoomX - MoveX) / ImgWidth - 1);
    j0 := Floor(-MoveY / ImgHeight);
    j1 := Ceil((Container.PixelsHeight / ZoomY - MoveY) / ImgHeight - 1);

    for i := i0 to i1 do
      for j := j0 to j1 do
        DrawImage(
          MoveX * ZoomX + i * Integer(DrawableImage.Width)  * ZoomX,
          MoveY * ZoomY + j * Integer(DrawableImage.Height) * ZoomY);
  end else
  begin
    VisibleXStart := -MoveX;
    VisibleXEnd   := Container.PixelsWidth  / ZoomX - MoveX;
    VisibleYStart := -MoveY;
    VisibleYEnd   := Container.PixelsHeight / ZoomY - MoveY;

    if VisibleXStart > ImgWidth  then ImageArrowLeft.Exists := true else
    if VisibleXEnd < 0           then ImageArrowRight.Exists := true else
    if VisibleYStart > ImgHeight then ImageArrowDown.Exists := true else
    if VisibleYEnd < 0           then ImageArrowUp.Exists := true else
    begin
      ClampVar(VisibleXStart, 0, ImgWidth);
      ClampVar(VisibleXEnd  , 0, ImgWidth);
      ClampVar(VisibleYStart, 0, ImgHeight);
      ClampVar(VisibleYEnd  , 0, ImgHeight);

      HorizScrollBar := (VisibleXStart > 0) or (VisibleXEnd < ImgWidth);
      VertScrollBar := (VisibleYStart > 0) or (VisibleYEnd < ImgHeight);

      if HorizScrollBar and VertScrollBar then
      begin
        RenderContext.ScissorEnable(Container.PixelsRect.RemoveLeft(ScrollbarSize + 1).RemoveBottom(ScrollbarSize + 1));
      end else
      if HorizScrollBar then
      begin
        RenderContext.ScissorEnable(Container.PixelsRect.RemoveBottom(ScrollbarSize + 1));
      end else
      if VertScrollBar then
      begin
        RenderContext.ScissorEnable(Container.PixelsRect.RemoveLeft(ScrollbarSize + 1));
      end;

      DrawImage(MoveX * ZoomX, MoveY * ZoomY);

      RenderContext.ScissorDisable;

      if HorizScrollBar then
      begin
        DrawPrimitive2D(pmLines, [
          Vector2(ScrollbarSize, ScrollbarSize),
          Vector2(Container.PixelsWidth, ScrollbarSize),
          Vector2(ScrollbarSize, 0),
          Vector2(ScrollbarSize, ScrollbarSize)
        ], Yellow);

        VisibleXStart := MapRange(VisibleXStart, 0, ImgWidth, ScrollbarSize+ScrollbarMargin, Container.PixelsWidth-ScrollbarMargin);
        VisibleXEnd   := MapRange(VisibleXEnd,   0, ImgWidth, ScrollbarSize+ScrollbarMargin, Container.PixelsWidth-ScrollbarMargin);
        DrawRectangle(FloatRectangle(VisibleXStart, ScrollbarMargin,
          VisibleXEnd - VisibleXStart, ScrollbarSize - 2 * ScrollbarMargin), Gray);
      end;

      if VertScrollBar then
      begin
        DrawPrimitive2D(pmLines, [
          Vector2(ScrollbarSize, ScrollbarSize),
          Vector2(ScrollbarSize, Container.PixelsHeight),
          Vector2(0, ScrollbarSize),
          Vector2(ScrollbarSize, ScrollbarSize)
        ], Yellow);

        VisibleYStart := MapRange(VisibleYStart, 0, ImgHeight, ScrollbarSize + ScrollbarMargin, Container.PixelsHeight - ScrollbarMargin);
        VisibleYEnd   := MapRange(VisibleYEnd,   0, ImgHeight, ScrollbarSize + ScrollbarMargin, Container.PixelsHeight - ScrollbarMargin);
        DrawRectangle(FloatRectangle(ScrollbarMargin, VisibleYStart,
          ScrollbarSize - 2 * ScrollbarMargin, VisibleYEnd - VisibleYStart), Gray);
      end;
    end;
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  procedure Move(var value: Single; Change: Single);
  begin
    Change := Change * (8 * 50 * Container.Fps.SecondsPassed);
    if Container.Pressed[keyCtrl] then Change := Change * 10;
    value := value + Change;
    Container.Invalidate;
  end;

  procedure UpdateLabelStatus;

    function ImageSizeToStr(const Width, Height, Depth: Cardinal): String;
    begin
      // do not show Depth for 2D image, to not confuse users
      if Depth <> 1 then
        Result := Format('%d x %d x %d', [Width, Height, Depth])
      else
        Result := Format('%d x %d', [Width, Height]);
    end;

    function CompositeImageInfo(DImg: TCompositeImage): String;
    begin
      Result := Format(
        '  Whole composite image:' +NL+
        '    Size: <font color="#ffffff">%s</font>'  +NL+
        '    Type: <font color="#ffffff">%s</font>' +NL+
        '    Mipmaps: <font color="#ffffff">%s</font>' +NL+
        '    Subimages count: <font color="#ffffff">%d</font>',[
        ImageSizeToStr(DImg.Width, DImg.Height, DImg.Depth),
        CompositeTypeToString[DImg.CompositeType],
        BoolToStr(DImg.Mipmaps, true),
        DImg.Images.Count
      ]);
    end;

  var
    S: String;
  begin
    if IsImageValid then
      S := Format(
        'Image (URL): <font color="#ffffff">%s</font>' + NL +
        'Class: <font color="#ffffff">%s</font>' +NL +
        'Size: <font color="#ffffff">%s</font>', [
        ImageURL,
        Image.ClassName,
        ImageSizeToStr(Image.Width, Image.Height, Image.Depth)
      ])
    else
      S := '<invalid image file>';

    if CompositeImage <> nil then
      S := S + NL + NL +
        'Part of composite image (KTX, DDS):' + NL +
        Format('  Subimage: <font color="#ffffff">%d</font>', [CompositeImageIndex]) + NL +
        CompositeImageInfo(CompositeImage);

    LabelStatus.Caption := S;
  end;

const
  ScaleFactor = 5;
var
  ScaleUp, ScaleDown: Single;
begin
  inherited;

  if Container.Pressed[keyArrowUp] then Move(MoveY, -1 / ZoomY);
  if Container.Pressed[keyArrowDown] then Move(MoveY, 1 / ZoomY);
  if Container.Pressed[keyArrowRight] then Move(MoveX, -1 / ZoomX);
  if Container.Pressed[keyArrowLeft] then Move(MoveX, 1 / ZoomX);

  ScaleUp := Power(ScaleFactor, Container.Fps.SecondsPassed);
  ScaleDown := Power(1 / ScaleFactor, Container.Fps.SecondsPassed);

  if Container.Pressed[keyNumpadPlus] or
     Container.Pressed[keyPlus] or
     Container.Pressed.Characters['+'] then
  begin
    MultZoom(ZoomX, ScaleUp);
    MultZoom(ZoomY, ScaleUp);
  end;

  if Container.Pressed[keyNumpadMinus] or
     Container.Pressed[keyMinus] or
     Container.Pressed.Characters['-'] then
  begin
    MultZoom(ZoomX, ScaleDown);
    MultZoom(ZoomY, ScaleDown);
  end;

  if Container.Pressed[keyX] then
    if Container.Pressed[keyShift] then
      MultZoom(ZoomX, ScaleUp)
    else
      MultZoom(ZoomX, ScaleDown);

  if Container.Pressed[keyY] then
    if Container.Pressed[keyShift] then
      MultZoom(ZoomY, ScaleUp)
    else
      MultZoom(ZoomY, ScaleDown);

  UpdateLabelStatus;
end;

function TViewMain.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if buttonLeft in Event.Pressed then
  begin
    MoveX := MoveX + ((Event.Position[0] - Event.OldPosition[0]) / ZoomX);
    MoveY := MoveY + ((Event.Position[1] - Event.OldPosition[1]) / ZoomY);
    Container.Invalidate;
    Exit(true);
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
const
  ScaleFactor = 1.1;
var
  ScaleUp, ScaleDown: Single;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseWheel(mwUp) then
  begin
    ScaleUp := ScaleFactor;
    MultZoom(ZoomX, ScaleUp);
    MultZoom(ZoomY, ScaleUp);
    Exit(true);
  end;

  if Event.IsMouseWheel(mwDown) then
  begin
    ScaleDown := 1 / ScaleFactor;
    MultZoom(ZoomX, ScaleDown);
    MultZoom(ZoomY, ScaleDown);
    Exit(true);
  end;
end;

{ menu ------------------------------------------------------------ }

procedure DropFiles(Container: TCastleContainer; const FileNames: array of string);
var
  URL: string;
begin
  if High(FileNames) >= 0 then
  begin
    URL := FilenameToURISafe(FileNames[0]);
    if URL <> '' then
      Images.FileOpen(URL);
  end;
end;

procedure MenuClick(Container: TCastleContainer; Item: TMenuItem);

  function Window: TCastleWindow;
  begin
    Result := Application.MainWindow;
  end;

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
      Images.FileOpen(URL);
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
      Images.Urls.Clear;
      Images.AddImageNamesFromURL(URL);
      Images.Current := 0;
    end;
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
      ChangeCompositeImageIndex(ChangeIntCycle(
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

  procedure ChangeBackgroundColor;
  var
    BgCol: TCastleColor;
  begin
    BgCol := Window.Container.BackgroundColor;
    if Window.ColorDialog(BgCol) then
      Window.Container.BackgroundColor := BgCol
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
           Change := (Window.Width / Image.Width) / ViewMain.ZoomX;
           ViewMain.MultZoom(ViewMain.ZoomX, change);
           ViewMain.MultZoom(ViewMain.ZoomY, change);
         end;
    211: begin
           Change := (Window.Height / Image.Height) / ViewMain.ZoomY;
           ViewMain.MultZoom(ViewMain.ZoomX, change);
           ViewMain.MultZoom(ViewMain.ZoomY, change);
         end;
    220: ViewMain.SetZoom(ViewMain.ZoomX, Window.Width / Image.Width);
    221: ViewMain.SetZoom(ViewMain.ZoomY, Window.Height / Image.Height);
    230: ViewMain.DrawTiled := not ViewMain.DrawTiled;
    240: begin
           ViewMain.SetZoom(ViewMain.ZoomX, 1.0);
           ViewMain.SetZoom(ViewMain.ZoomY, 1.0);
           ViewMain.MoveX := 0;
           ViewMain.MoveY := 0;
         end;

    260: ChangeBackgroundColor;
    270: ViewMain.UseImageAlpha := not ViewMain.UseImageAlpha;

    310: Images.ChangeCurrent(-1);
    311: Images.ChangeCurrent(+1);

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
    510: ViewMain.RectStatus.Exists := not ViewMain.RectStatus.Exists;
    520: ShowAbout;
    1600..1700: DoResize(TResizeInterpolation(Item.IntData - 1600));
    2010: OpenUrl(ImageURL);
    else
      Images.Current := Item.IntData - 10000;
  end;
  Window.Invalidate;
end;

function InterpolationToStr(const Interpolation: TResizeInterpolation): string;
begin
  Result := SEnding(GetEnumName(TypeInfo(TResizeInterpolation), Ord(Interpolation)), 3);
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
  NextRecentMenuItem: TMenuEntry;
  Interpolation: TResizeInterpolation;
begin
  Assert(ViewMain <> nil);

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
    M.Append(TMenuItemChecked.Create('Test Is _Tileable', 230, 't', ViewMain.DrawTiled, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Reset _Zoom and Translation',         240, keyHome));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Use Image Alpha Channel', 270,  ViewMain.UseImageAlpha, true));
    M.Append(TMenuItem.Create('Background Color ...',                260));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemToggleFullScreen.Create(ViewMain.Window.FullScreen));
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
  Images.Menu := M;
    // TODO: Ctrl + PageUp/Down would be more consistent
    M.Append(TMenuItem.Create('_Previous Subimage in Composite (DDS, KTX)', 320, CtrlP));
    M.Append(TMenuItem.Create('_Next Subimage in Composite (DDS, KTX)',     321, CtrlN));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Previous Image', 310, keyPageUp));
    M.Append(TMenuItem.Create('_Next Image',     311, keyPageDown));
    M.Append(TMenuSeparator.Create);
    Result.Append(M);
  M := TMenu.Create('_Help');
    M.Append(TMenuItemChecked.Create('Image Information', 510, keyF1, ViewMain.RectStatus.Exists, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('About castle-image-viewer', 520));
    Result.Append(M);
end;

end.
