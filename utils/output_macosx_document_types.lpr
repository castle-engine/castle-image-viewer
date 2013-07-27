{
  Copyright 2001-2013 Michalis Kamburelis.

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

{ Output images types supported by CastleImages in XML format,
  suitable to use in an Mac OS X bundle info.
  Use this to associate glViewImage with all supported image types on Mac OS X. }

uses CastleUtils, CastleImages;
var
  FormatInfo: TImageFormatInfo;
  Output: string = '';
  Ext: TImageFormatInfoExtsCount;
begin
  for FormatInfo in ImageFormatInfos do
  begin
    Output +=
      '  <dict>' +NL+
      '    <key>CFBundleTypeExtensions</key>' +NL+
      '    <array>' +NL;
    for Ext := 1 to FormatInfo.ExtsCount do
      Output +=
        '      <string>' + FormatInfo.Exts[Ext] + '</string>' +NL;
    Output +=
      '    </array>' +NL+
      '    <key>CFBundleTypeMIMETypes</key>' +NL +
      '    <string>' + FormatInfo.MimeTypes[1] + '</string>' +NL +
      '    <key>CFBundleTypeName</key>' +NL+
      '    <string>' + FormatInfo.FormatName + '</string>' +NL+
      '    <key>CFBundleTypeIconFile</key>' +NL+
      '    <string>glViewImage</string>' +NL+
      '    <key>CFBundleTypeOSTypes</key>' +NL+
      '    <array>' +NL+
      '      <string>****</string>' +NL+
      '    </array>' +NL+
      '    <key>CFBundleTypeRole</key>' +NL;
    if Assigned(FormatInfo.Save) then
      Output += '    <string>Editor</string>' +NL else
      Output += '    <string>Viewer</string>' +NL;
    Output +=
      '  </dict>' +NL;
  end;
  Writeln(Output);
end.
