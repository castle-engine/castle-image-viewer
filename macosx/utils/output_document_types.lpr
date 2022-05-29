{
  Copyright 2013-2022 Michalis Kamburelis.

  This file is part of "castle-view-image".

  "castle-view-image" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-view-image" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-view-image"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Output images types supported by CastleImages in XML format,
  suitable to use CastleEngineManifest.xml .
  Use this to associate castle-view-image with all supported image types. }

uses SysUtils,
  CastleUtils, CastleImagesFormats;
var
  FormatInfo: TImageFormatInfo;
  Output: string = '';
  I: Integer;
begin
  Output := '  <associate_document_types>' + NL;
  for FormatInfo in ImageFormatInfos do
  begin
    Output += Format('    <document_type caption="%s" name="%s">' + NL, [
      FormatInfo.FormatName,
      FormatInfo.Exts[1]
    ]);
    for I := 1 to FormatInfo.ExtsCount do
      Output +=
        '      <extension>' + FormatInfo.Exts[I] + '</extension>' +NL;
    for I := 1 to FormatInfo.MimeTypesCount do
      Output +=
        '      <mime>' + FormatInfo.MimeTypes[I] + '</mime>' +NL;
    Output += '    </document_type>' + NL;
  end;
  Output += '  </associate_document_types>' + NL;
  Writeln(Output);
end.
