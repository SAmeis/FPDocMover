unit fpdocsplitter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, commonutils, fgl, dom, XMLRead, XMLWrite, xpath;
type
  TFPDocSplitMode = (smPackage, smModule);

  { TFPDocModuleStructure }

  TFPDocModuleStructure = class(TObject)
  private
    fModules: TStringList;
    fPackageNode: TDOMElement;
    function GetModuleNode(Index: Integer): TDOMElement;
    function GetModules: TStrings;
    function GetPackageName: UnicodeString;
  protected
    procedure UpdateModules;
  public
    constructor Create(const aPackageNode: TDOMElement);
    destructor Destroy; override;
    property PackageName: UnicodeString read GetPackageName;
    property PackageNode: TDOMElement read fPackageNode;
    property Modules    : TStrings read GetModules;
    property ModuleNodes[Index: Integer]: TDOMElement read GetModuleNode;
  end;

  TFPDocModuleStructureList = specialize TFPGObjectList<TFPDocModuleStructure>;

  { TFPDocSplitter }

  TFPDocSplitter = class(TFPDocModule)
  private
    fInputFile: UnicodeString;
    fOutputDirectory: UnicodeString;
    fSplitMode: TFPDocSplitMode;
  protected
    function GetPackageList(doc: TXMLDocument): TFPDocModuleStructureList;
    function GetOutputFileNames(aStructure: TFPDocModuleStructureList): TStrings;
    function GetPackgeOutputFileName(Pkg: TFPDocModuleStructure): UnicodeString;
    function GetModuleOutputFileName(const aModuleName: UnicodeString): UnicodeString;
    procedure ProcessPackage(aPackage: TFPDocModuleStructure);
  public
    constructor Create(const aInputFile, aOutputDirectory: UnicodeString;
      aSplitMode: TFPDocSplitMode);
    procedure Process; override;
    property InputFile: UnicodeString read fInputFile write fInputFile;
    property OutputDirectory: UnicodeString read fOutputDirectory
      write fOutputDirectory;
    property SplitMode: TFPDocSplitMode read fSplitMode write fSplitMode;
  end;

implementation

{ TFPDocModuleStructure }

function TFPDocModuleStructure.GetPackageName: UnicodeString;
begin
  Result := fPackageNode.GetAttribute('name');
end;

function TFPDocModuleStructure.GetModuleNode(Index: Integer): TDOMElement;
begin
  Result := TDOMElement(fModules.Objects[Index]);
end;

function TFPDocModuleStructure.GetModules: TStrings;
begin
  Result := fModules;
end;

procedure TFPDocModuleStructure.UpdateModules;
var
  ModuleNode: TDOMNode;
  i: Integer;
begin
  fModules.Clear;

  for i := 0 to fPackageNode.ChildNodes.Count -1 do
  begin
    ModuleNode := fPackageNode.ChildNodes.Item[i];
    if ModuleNode.NodeType = ELEMENT_NODE then
    begin
      if ModuleNode.NodeName = 'module' then
        fModules.AddObject(TDOMElement(ModuleNode).GetAttribute('name'),
          ModuleNode);
    end;
  end;
end;

constructor TFPDocModuleStructure.Create(const aPackageNode: TDOMElement);
begin
  inherited Create;
  fModules     := TStringList.Create;
  fModules.OwnsObjects := False;
  fModules.Duplicates := dupError;
  fPackageNode := aPackageNode;
  UpdateModules;
end;

destructor TFPDocModuleStructure.Destroy;
begin
  fModules.Free;
  inherited Destroy;
end;

{ TFPDocSplitter }

function TFPDocSplitter.GetPackageList(doc: TXMLDocument
  ): TFPDocModuleStructureList;
const
  PACKAGE_NODES = '/fpdoc-descriptions/package';
var
  FPDocRootNode: TDOMNode;
  PackageNode: TDOMNode;
  i: Integer;
  pkg: TFPDocModuleStructure;
  res: TXPathVariable;
begin
  Result := TFPDocModuleStructureList.Create(True);
  try
    res := EvaluateXPathExpression(PACKAGE_NODES, doc.DocumentElement);
    Assert(res is TXPathNodeSetVariable, 'Unexpected xpath result type.');

    for i := 0 to res.AsNodeSet.Count -1 do
    begin
      PackageNode := TDOMNode(res.AsNodeSet.Items[i]);
      Assert(PackageNode.NodeType = ELEMENT_NODE, 'Error in XPath expression - element node expected.');
      pkg := TFPDocModuleStructure.Create(TDOMElement(PackageNode));
      Result.Add(pkg);
    end;
  except
    Result.Destroy;
    raise;
  end;
end;

function TFPDocSplitter.GetOutputFileNames(aStructure: TFPDocModuleStructureList
  ): TStrings;
var
  pkg: TFPDocModuleStructure;
  i: Integer;
  ModuleNode: TDOMElement;
begin
  ModuleNode := nil;
  Result := TStringList.Create;
  TStringList(Result).Duplicates := dupError;
  try
    for pkg in aStructure do
    begin
      if SplitMode = smPackage then
        Result.Add(Format('%s.xml', [pkg.PackageName]))
      else if SplitMode = smModule then
      begin
        for i := 0 to pkg.Modules.Count -1 do
        begin
          ModuleNode := pkg.ModuleNodes[i];
          Result.Add(Format('%s.xml', [pkg.Modules.Strings[i]]));
        end;
      end;
    end;
  except
    on e: TObject do
    begin
      Result.Destroy;
      if not (e is EStringListError) then raise;

      if not assigned(pkg) then
        raise Exception.Create('Duplicate package or module.');
      if not Assigned(ModuleNode) then
        raise Exception.CreateFmt('Duplicate package: %s', [pkg.PackageName])
      else
        raise Exception.CreateFmt(
          'Duplicate module %s in one ore more packages.',
          [ModuleNode.GetAttribute('name')]);
    end;
  end;
end;

function TFPDocSplitter.GetPackgeOutputFileName(Pkg: TFPDocModuleStructure
  ): UnicodeString;
var
  PkgUniName: UnicodeString;
begin
  PkgUniName := UnicodeString(pkg.PackageName);
  Result := IncludeTrailingPathDelimiter(OutputDirectory)+'%s.xml';
  Result := Format(Result, [PkgUniName]);
end;

function TFPDocSplitter.GetModuleOutputFileName(const aModuleName: UnicodeString
  ): UnicodeString;
begin
  Result := IncludeTrailingPathDelimiter(OutputDirectory)+'%s.xml';
  Result := Format(Result, [aModuleName]);
end;

procedure TFPDocSplitter.ProcessPackage(aPackage: TFPDocModuleStructure);
  procedure WriteFullPackage(aPackage: TFPDocModuleStructure);
  var
    Doc: TXMLDocument;
    RootNode: TDOMElement;
    pkgNode: TDOMElement;
    OutputFileName: UnicodeString;
  begin
    Doc := TXMLDocument.Create;
    RootNode := Doc.CreateElement('fpdoc-descriptions');
    Doc.AppendChild(RootNode);
    pkgNode := aPackage.PackageNode;
    RootNode.AppendChild(pkgNode.CloneNode(True, Doc));
    OutputFileName := GetPackgeOutputFileName(aPackage);
    WriteLogCallback('Writing package "%s" to file "%s".',
      [aPackage.PackageName, OutputFileName]);
    WriteXML(doc, OutputFileName);
  end;
  procedure WriteModule(aPackageName: UnicodeString; aModuleName: UnicodeString;
    aModuleNode: TDOMElement);
  var
    Doc: TXMLDocument;
    RootNode: TDOMElement;
    pkgNode: TDOMElement;
    OutputFileName: UnicodeString;
  begin
    Doc := TXMLDocument.Create;
    RootNode := Doc.CreateElement('fpdoc-descriptions');
    Doc.AppendChild(RootNode);
    pkgNode := Doc.CreateElement('package');
    pkgNode.SetAttribute('name', aPackageName);
    RootNode.AppendChild(pkgNode);
    pkgNode.AppendChild(aModuleNode.CloneNode(True, Doc));
    OutputFileName := GetModuleOutputFileName(aModuleName);
    WriteLogCallback('Writing module "%s" to file "%s".',
      [aModuleName, OutputFileName]);
    WriteXML(doc, OutputFileName);
  end;
var
  i: Integer;
  ModuleName: UnicodeString;
  ModuleNode: TDOMElement;
begin
  if SplitMode = smPackage then
  begin
    WriteFullPackage(aPackage);
  end else
  if SplitMode = smModule then
  begin
    for i := 0 to aPackage.Modules.Count - 1 do
    begin
      ModuleName := aPackage.Modules.Strings[i];
      ModuleNode := aPackage.ModuleNodes[i];
      WriteModule(aPackage.PackageName, ModuleName, ModuleNode);
    end;
  end;
end;

constructor TFPDocSplitter.Create(const aInputFile,
  aOutputDirectory: UnicodeString; aSplitMode: TFPDocSplitMode);
begin
  inherited Create;
  fInputFile := aInputFile;
  fOutputDirectory := aOutputDirectory;
  fSplitMode := aSplitMode;
end;

procedure TFPDocSplitter.Process;
var
  Doc: TXMLDocument;
  Packages: TFPDocModuleStructureList;
  pkg: TFPDocModuleStructure;
begin
  Self.ReadXMLFile(Doc, InputFile);
  try
    Packages := GetPackageList(Doc);
    if Packages.Count = 0 then
      raise Exception.CreateFmt('No packages in source file "%s".', [InputFile]);
    for pkg in Packages do
      ProcessPackage(pkg);
  finally
    Packages.Free;
    Doc.Free;
  end;
end;

end.

