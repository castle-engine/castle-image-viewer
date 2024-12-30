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
  GameImage, GameEmbeddedImages, GameImageList, GameViewMain;

var
  Window: TCastleWindow;

{ command-line params --------------------------------------------------------- }

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
  ApplicationProperties.Version := '2.3.0';
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

  { Application.MainWindow is used e.g. by GameImage,
    to update window Caption or make MessageOK. }
  Application.MainWindow := Window;

  try
    { init menu things. We must do it before we add something to Images,
      this is required by CreateMainMenu. }
    RecentMenu := TWindowRecentFiles.Create(nil);
    RecentMenu.OnOpenRecent := {$ifdef FPC}@{$endif} Images.FileOpen;

    { parse options }
    Window.ParseParameters;
    { parse our options }
    Parameters.Parse(Options, @OptionProc, nil);

    ViewMain := TViewMain.Create(Application);
    ViewMain.Window := Window;
    Window.Container.View := ViewMain;

    Window.DepthBits := 0; { depth buffer not needed here }
    { We have to set Window.MainMenu before Window is open.
      Although this menu only makes sense with ViewMain,
      but we cannot change it from view's Start / Stop to nil/non-nil. }
    Window.MainMenu := ViewMain.CreateMainMenu;
    Window.Open;

    UserConfig.Load;
    RecentMenu.LoadFromConfig(UserConfig);

    { calculate Images = parse the list of image files to open }
    if Parameters.High = 0 then
    begin
      Images.AddImageNamesAllLoadable('');
      Images.Urls.Sort;
    end else
    begin
      SavedErrorMessages := '';

      for i := 1 to Parameters.High do
      begin
        if SCharIs(Parameters[i], 1, '@') then
          Images.AddImageNamesFromFile(SEnding(Parameters[i], 2))
        else
        case URIExists(Parameters[i]) of
          ueFile, ueUnknown:
            Images.AddImageNamesFromURL(Parameters[i]);
          ueDirectory:
            Images.AddImageNamesAllLoadable(InclPathDelim(Parameters[i]));
          else SavedErrorMessages := SavedErrorMessages + 'File "' + Parameters[i] + '" does not exist.' + NL;
        end;
      end;

      if SavedErrorMessages <> '' then
      begin
        { make sure to create some Image, to be able to Render }
        if Image = nil then
          CreateImageInvalid('');
        MessageOk(Window, Trim(SavedErrorMessages));
        SavedErrorMessages := '';
      end;
    end;

    Images.Initialize;

    Application.Run;

    RecentMenu.SaveToConfig(UserConfig);
    UserConfig.Save;
  finally
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
