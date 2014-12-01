program fpdocmover;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, fpdocsplitter, fpdocjoiner, commonutils;

type

  { TFPDocMover }

  TFPDocMover = class(TCustomApplication)
  private // Options
    fOptHelp  : Boolean;
    fOptFiles : UnicodeString;
    fOptOutput: UnicodeString;
    fOptInput : UnicodeString;
    fOptJoin  : Boolean;
    fOptSplit : Boolean;
    fOptSplitModule   : Boolean;
    fOptSplitPackage  : Boolean;
    fOptMergeTextNodes: Boolean;
    fOptComment       : Boolean;
    fOptOverwrite     : Boolean;
  protected
    procedure WriteStdErr(const AString: String; aTerminate: Boolean);
    procedure WriteStdErr(const AString: String);
    procedure DoRun; override;
    procedure DoOptionConsitenceCheck;
    procedure DoSplit;
    procedure DoJoin;
    procedure ModuleLogCallback(IsError: Boolean; const aMessgage: String);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

{ TFPDocMover }

procedure TFPDocMover.WriteStdErr(const AString: String; aTerminate: Boolean);
begin
  WriteLn(StdErr, AString);
  if aTerminate then
  begin
    Terminate;
    Halt;
  end;
end;

procedure TFPDocMover.WriteStdErr(const AString: String);
begin
  WriteStdErr(AString, False);
end;

procedure TFPDocMover.DoRun;
var
  LongOpts: TStringList;
  ErrorMsg: String;
begin
  // quick check parameters
  LongOpts := TStringList.Create;
  LongOpts.Add('help');
  LongOpts.Add('files');
  LongOpts.Add('input');
  LongOpts.Add('output');
  LongOpts.Add('join');
  LongOpts.Add('split');
  LongOpts.Add('module');
  LongOpts.Add('package');
  LongOpts.Add('text-nodes');
  LongOpts.Add('comment');
  LongOpts.Add('overwrite');

  ErrorMsg:=CheckOptions('hf:i:o:jsmptco',LongOpts, True);
  LongOpts.Destroy;
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  fOptHelp           := HasOption('h','help');
  fOptFiles          := GetOptionValue('f', 'files');
  fOptOutput         := GetOptionValue('o', 'output');
  fOptInput          := GetOptionValue('i', 'input');
  fOptJoin           := HasOption('j', 'join');
  fOptSplit          := HasOption('s', 'split');
  fOptSplitModule    := HasOption('m', 'module');
  fOptSplitPackage   := HasOption('p', 'package');
  fOptMergeTextNodes := HasOption('t', 'text-nodes');
  fOptComment        := HasOption('c', 'comment');
  fOptOverwrite      := HasOption('o', 'overwrite');

  if fOptHelp or (ParamCount = 0) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  DoOptionConsitenceCheck;
  if fOptJoin then
    DoJoin
  else if fOptSplit then
    DoSplit;

  // stop program loop
  Terminate;
end;

procedure TFPDocMover.DoOptionConsitenceCheck;
begin
  // check parameter consitence
  if fOptJoin and fOptSplit then
    WriteStdErr('Only one mode allowed (--split or --join).', True);

  if fOptSplitModule and fOptJoin then
    WriteStdErr('Option --module -m only allowed for spliiting files.', True);
  if fOptSplitPackage and fOptJoin then
    WriteStdErr('Option --package -p only allowed for spliiting files.', True);
  if (fOptInput <> '') and fOptJoin then
    WriteStdErr('Option --input -i only allowed for spliiting files.', True);
  if (fOptFiles = '') and fOptJoin then
    WriteStdErr('Option --files -f must be specified for joining.', True);
  if (fOptOutput = '') and fOptJoin then
    WriteStdErr('Option --output -o required for joining.', True);

  if fOptComment and fOptSplit then
    WriteStdErr('Option --comment -c only allowed for joining files.', True);
  if (fOptInput = '') and fOptSplit then
    WriteStdErr('Option --input -i required for splitting.', True);
  if (fOptFiles <> '') and fOptSplit then
    WriteStdErr('Option --files -f only allowed for joining files.', True);

  if (fOptOutput = '') and fOptSplit then
  begin
    WriteStdErr('No output direcotry given, set to '+GetCurrentDir);
    fOptOutput := GetCurrentDir;
  end;

  if fOptSplit then
  begin
    if FileExists(fOptOutput) then
    begin
      WriteStdErr('Output directory is a file.', True);
    end;
    if not DirectoryExists(fOptOutput) then
    begin
      WriteStdErr(Format('Output directory %s doesn''t exist.', [fOptOutput]), True);
    end;
  end;
  if fOptJoin then
  begin
    if FileExists(fOptOutput) and not fOptOverwrite then
    begin
      WriteStdErr('Output file already exist.', True);
    end;
    if DirectoryExists(fOptOutput) then
    begin
      WriteStdErr('Output file is a directory but needs to be a file name.', True);
    end;
  end;
end;

procedure TFPDocMover.DoSplit;
var
  SplitMode: TFPDocSplitMode;
  splitter: TFPDocSplitter;
begin
  if fOptSplitPackage then
    SplitMode := smPackage
  else
    SplitMode := smModule;

  splitter := TFPDocSplitter.Create(fOptInput, fOptOutput, SplitMode);
  try
    splitter.LogCallback := @ModuleLogCallback;
    splitter.Process;
  finally
    splitter.Destroy;
  end;
end;

procedure TFPDocMover.DoJoin;
var
  joiner: TFPDocJoiner;
begin
  joiner := TFPDocJoiner.Create(fOptFiles, fOptOutput, fOptMergeTextNodes,
    fOptComment);
  try
    joiner.LogCallback := @ModuleLogCallback;
    joiner.Process;
  finally
    joiner.Destroy;
  end;
end;

procedure TFPDocMover.ModuleLogCallback(IsError: Boolean;
  const aMessgage: String);
begin
  if IsError then
    WriteLn(StdErr, aMessgage)
  else
    WriteLn(aMessgage);
end;

constructor TFPDocMover.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

procedure TFPDocMover.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('Split or join FPDoc documentation XML files.');
  writeln();
  writeln('GENERAL');
  writeln('  --help        -h    Shows this help and exits');
  writeln('  --output      -o    Output file name for joins or directory for');
  writeln('                      splitting (default: current directory)');
  writeln('  --overwrite   -w    Overwrite existing files');
  Writeln();
  writeln('JOINING FILES');
  writeln('  --join        -j    Join files');
  writeln('  --text-nodes  -t    Join text nodes instead of element nodes');
  writeln('  --files       -f    Specify file with list of input file names for joining');
  writeln('  --comment     -c    Allow characters # and ; to start a comment line in file list');
  Writeln();
  writeln('SPLITTING FILES');
  writeln('  --split   -s    Split files');
  writeln('  --module  -m    Split files by module (default for splitting)');
  writeln('  --package -p    Split files by package');
  writeln('  --input   -i    Input file name for splitting');
end;

var
  Application: TFPDocMover;
begin
  Application:=TFPDocMover.Create(nil);
  Application.Run;
  Application.Free;
end.

