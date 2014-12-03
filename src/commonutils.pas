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

