{ FPDocMover - common utils for joining and splitting

  Copyright (C) 2014, Simon Ameis <simon.ameis@web.de>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit commonutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dom, XMLRead, XmlReader;

type
  TLogCallback = procedure(IsError: Boolean; const aMessgage: String) of object;

  { TFPDocModule }

  TFPDocModule = class(TObject)
  private
    fLogCallback: TLogCallback;
  protected
    procedure ReadXML(out doc: TXMLDocument; const aFileName: UnicodeString);
    procedure WriteLogCallback(const aMessage: String);
     procedure WriteLogCallback(const aMessage: String; aFormatArgs: array of const);
    procedure WriteLogCallback(IsError: Boolean; const aMessage: String);
  public
    procedure Process; virtual; abstract;
    property LogCallback: TLogCallback read fLogCallback write fLogCallback;
  end;

implementation

{ TFPDocModule }

procedure TFPDocModule.ReadXML(out doc: TXMLDocument;
  const aFileName: UnicodeString);
var
  fh: THandle;
  fs: THandleStream;
  parser: TDOMParser;
  xmlsrc: XmlReader.TXMLInputSource;
begin
  fs := nil;
  parser := nil;
  xmlsrc := nil;
  fh := FileOpen(aFileName, fmOpenRead or fmShareDenyWrite);
  if fh = -1 then
    RaiseLastOSError;
  try
    fs := THandleStream.Create(fh);
    parser := TDOMParser.Create;
    xmlsrc := TXMLInputSource.Create(fs);
    parser.Options.PreserveWhitespace := True;
    parser.Parse(xmlsrc, doc);
  finally
    xmlsrc.Free;
    parser.Free;
    fs.Free;
    FileClose(fh);
  end;
end;

procedure TFPDocModule.WriteLogCallback(const aMessage: String);
begin
  WriteLogCallback(False, aMessage);
end;

procedure TFPDocModule.WriteLogCallback(const aMessage: String;
  aFormatArgs: array of const);
begin
  WriteLogCallback(Format(aMessage, aFormatArgs));
end;

procedure TFPDocModule.WriteLogCallback(IsError: Boolean; const aMessage: String
  );
begin
  if Assigned(fLogCallback) then
    fLogCallback(IsError, aMessage);
end;

end.

