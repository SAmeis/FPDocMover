unit commonutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogCallback = procedure(IsError: Boolean; const aMessgage: String) of object;

  { TFPDocModule }

  TFPDocModule = class(TObject)
  private
    fLogCallback: TLogCallback;
  protected
    procedure WriteLogCallback(const aMessage: String);
     procedure WriteLogCallback(const aMessage: String; aFormatArgs: array of const);
    procedure WriteLogCallback(IsError: Boolean; const aMessage: String);
  public
    procedure Process; virtual; abstract;
    property LogCallback: TLogCallback read fLogCallback write fLogCallback;
  end;

implementation

{ TFPDocModule }

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

