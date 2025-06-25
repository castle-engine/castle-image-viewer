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

{ List of images to browse by previous/next keys/menu items. }
unit GameImageList;

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
  TImagesList = class
  private
    InitializeDone: Boolean;
    FCurrent: Integer;
    { Change Current and try to load newly choosen image.
      Uses CreateImage(Urls[Current]) if it's a normal
      (not internal like SWelcomeImageUrl) image. }
    procedure SetCurrent(const Value: Integer);

    { After changing Images contents always call this.
      This ensures that Menu is properly updated. }
    procedure ImagesChanged;

    { Create image, based on list index ListPos. }
    procedure CreateImageFromList(const ListPos: Integer; const InvalidImage: boolean = false);
  public
    { A list of images to browse by previous/next keys/menu items.
      May be relative URLs, so the current dir of this program must never
      change during execution.

      Current is always in [0..Url.Count - 1].
      Urls.Count is always > 0.

      After changing this call ImagesChanged; }
    Urls: TStringList;

    { Initialized in CreateMainMenu, then contents updated by this class. }
    Menu: TMenu;

    constructor Create;
    destructor Destroy; override;

    { Current index in Urls list. }
    property Current: Integer read FCurrent write SetCurrent;

    procedure FileOpen(const URL: string);

    { Add image names to list.

      If calling after @link(Initialize),
      be sure to set @link(Current) to reload image (for now, @link(Current)
      always reloads, even if index is equal, which is helpful here).

      We automatically update menu (in Images.Menu) if these are called
      after @link(Initialize). Otherwise, @link(Initialize) will do it.
      Pass @code(UpdateMenu) = @false to avoid it,
      but this only makes sense if you call many AddImage* methods,
      and you let the last one update the menu.

      @groupBegin }
    procedure AddImageNamesFromFile(const URL: string; const UpdateMenu: Boolean = true);
    procedure AddImageNamesFromMask(const PathAndMask: string; const UpdateMenu: Boolean = true);
    { Path can be '' or must end with '/' }
    procedure AddImageNamesAllLoadable(const Path: string; const UpdateMenu: Boolean = true);
    procedure AddImageNamesFromURL(const URL: string; const UpdateMenu: Boolean = true);
    { @groupEnd }

    { Add welcome image, initialize @link(Current), load initial image. }
    procedure Initialize;

    { Change @link(Current) by Change, to advance to next or previous image. }
    procedure ChangeCurrent(const Change: Integer);
  end;

var
  Images: TImagesList;

implementation

constructor TImagesList.Create;
begin
  inherited;
  Urls := TStringList.Create;
end;

destructor TImagesList.Destroy;
begin
  FreeAndNil(Urls);
  inherited;
end;

procedure TImagesList.CreateImageFromList(const ListPos: Integer; const InvalidImage: boolean = false);
begin
  if InvalidImage then
    CreateImageInvalid(Urls[ListPos])
  else
  if Urls[ListPos] = SWelcomeImageUrl then
    CreateImage(Welcome.MakeCopy, Urls[ListPos])
  else
    CreateImage(Urls[ListPos]);
end;

procedure TImagesList.SetCurrent(const Value: Integer);
begin
  FCurrent := Value;
  CreateImageFromList(Current);
  Application.MainWindow.Invalidate;
end;

procedure TImagesList.ChangeCurrent(const Change: Integer);
begin
  Current := ChangeIntCycle(Current, Change, Urls.Count - 1);
end;

procedure TImagesList.AddImageNamesFromFile(const URL: string; const UpdateMenu: Boolean);

  procedure AddImageNamesFromTextReader(Reader: TCastleTextReader);
  var
    s: string;
  begin
    while not Reader.Eof do
    begin
      s := Trim(Reader.Readln);
      if s <> '' then Urls.Append(s);
    end;
  end;

var
  F: TCastleTextReader;
begin
  if URL = '-' then
    F := TCastleTextReader.Create(StdInStream, false)
  else
    F := TCastleTextReader.Create(URL);
  try
    AddImageNamesFromTextReader(F);
  finally FreeAndNil(F) end;

  if InitializeDone and UpdateMenu then
    ImagesChanged;
end;

procedure TImagesList.AddImageNamesFromMask(const PathAndMask: string; const UpdateMenu: Boolean);
var
  List: TFileInfoList;
  FileInfo: TFileInfo;
  Path, Mask: String;
begin
  Path := ExtractURIPath(PathAndMask);
  Mask := ExtractURIName(PathAndMask);
  List := FindFilesList(Path, Mask, false, []);
  try
    { Sort results, this is nice when the order is meaningful,
      e.g. when comparing castle-image-viewer order with order in file manager
      when browsing a directory, e.g. for PNG testcase http://www.schaik.com/pngsuite/ . }
    List.SortUrls;
    for FileInfo in List do
    begin
      { Avoid adding duplicates, otherwise AddImageNamesFromURL always adds duplicates.
        Note: we cannot use Urls.Duplicates := dupIgnore,
        because it doesn't work when the list is not sorted (see
        https://gitlab.com/freepascal.org/fpc/source/-/issues/28774 )
        and we cannot set Url.Sorted := true as it would change the order
        of other entries. }
      if Urls.IndexOf(FileInfo.AbsoluteName) = -1 then
        Urls.Append(FileInfo.AbsoluteName);
    end;
  finally FreeAndNil(List) end;

  if InitializeDone and UpdateMenu then
    ImagesChanged;
end;

procedure TImagesList.AddImageNamesAllLoadable(const Path: string; const UpdateMenu: Boolean);
var
  I, J: Integer;
  Pattern: string;
begin
  for I := 2 to LoadImage_FileFilters.Count - 1 do
    for J := 0 to LoadImage_FileFilters[I].Patterns.Count - 1 do
    begin
      Pattern := LoadImage_FileFilters[I].Patterns[J];
      AddImageNamesFromMask(Path + Pattern, false);
    end;

  if InitializeDone and UpdateMenu then
    ImagesChanged;
end;

procedure TImagesList.AddImageNamesFromURL(const URL: string; const UpdateMenu: Boolean);
begin
  // URL will match exactly 1 file
  AddImageNamesFromMask(URL, false);

  { Add also other filenames within this directory.
    This way if you open a file by double-clicking,
    you can still browse other images in this dir. }
  AddImageNamesAllLoadable(ExtractURIPath(URL), false);

  if InitializeDone and UpdateMenu then
    ImagesChanged;
end;

{ open file --------------------------------------------------------------- }

procedure TImagesList.FileOpen(const URL: string);
begin
  Urls.Insert(Current + 1, URL);
  ImagesChanged;
  ChangeCurrent(+1);

  // unused: this is how to open an image without adding it to Images list:
  //CreateImage(URL);
end;

procedure TImagesList.ImagesChanged;
const
  NormalImageMenuCount = 6;
var
  i: Integer;
begin
  Assert(Menu <> nil);

  while Menu.Count > NormalImageMenuCount do
    Menu.Delete(NormalImageMenuCount);

  { Do not load all Urls.Count to the menu,
    as this makes menu very large, making updating it (e.g. when user
    loads new image) time-consuming, and it's not possible to navigate
    it anyway. }

  for i := 0 to Min(Urls.Count, 20) - 1 do
    Menu.Append(TMenuItem.Create(
      SQuoteMenuEntryCaption(URICaption(Urls[i])), 10000 + i));
end;

procedure TImagesList.Initialize;
begin
  Assert(Application.MainWindow <> nil);
  Assert(not Application.MainWindow.Closed);

  { Always insert welcome image }
  Urls.Insert(0, SWelcomeImageUrl);
  Assert(Urls.Count > 0);

  { By default, view the first loaded image, or welcome image if nothing loaded. }
  if Urls.Count > 1 then
    FCurrent := 1
  else
    FCurrent := 0;

  ImagesChanged;

  { Initialize Image. Window is open now (it has to, otherwise we get
    warnings that opening files before Application.OnInitialize is not portable),
    DecompressTexture is assigned, so we load it in a straightforward way. }
  CreateImageFromList(FCurrent);

  InitializeDone := true;
end;

initialization
  Images := TImagesList.Create;
finalization
  FreeAndNil(Images);
end.
