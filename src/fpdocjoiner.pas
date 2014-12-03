{ FPDocMover - program module for joining files

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
unit fpdocjoiner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, commonutils, DOM, XMLWrite, XPath;

type
  TUnicodeStringArray = array of UnicodeString;

  { TFPDocJoiner }

  TFPDocJoiner = class(TFPDocModule)
  private
    fFileListName: UnicodeString;
    fOutputFile: UnicodeString;
    fInputFiles: TStringList;
    fMergeTextNodes: Boolean;
    fAllowCommentInFileList: Boolean;
    fTreeEndElements: TUnicodeStringArray;  // element nodes at which merging is switched to text merging
  protected
    procedure MergeFileIntoDocument(const aFile: UnicodeString;
      aTargetDoc: TXMLDocument);
    procedure MergeFPDocIntoParent(aSourceNode, aTargetParentNode: TDOMElement);
    function GetPackageNode(aDoc: TXMLDocument;
      const aPackageName: UnicodeString): TDOMElement;
    function GetXPathForNode(aNode: TDOMNode): UnicodeString;
    procedure CheckInputFiles;
  public
    constructor Create(const aFileList, aOutputFile: UnicodeString;
      aMergeTextNodes: Boolean; aAllowCommentInFileList: Boolean);
    destructor Destroy; override;
    procedure Process; override;
    property FileListName: UnicodeString read fFileListName;
    property OutputFile  : UnicodeString read fOutputFile;
    property MergeTextNodes: Boolean read fMergeTextNodes;
    property AllowCommentInFileList: Boolean read fAllowCommentInFileList;
  end;

operator in(const aStr: UnicodeString; arr: TUnicodeStringArray): Boolean;

implementation

operator in(const aStr: UnicodeString; arr: TUnicodeStringArray): Boolean;
var
  i: SizeInt;
begin
  for i := low(arr) to high(arr) do
    if arr[i] = aStr then
      exit(True);
  exit(False);
end;

{ TFPDocJoiner }

procedure TFPDocJoiner.MergeFileIntoDocument(const aFile: UnicodeString;
  aTargetDoc: TXMLDocument);
var
  InputDoc: TXMLDocument;
  i: Integer;
  Node: TDOMNode;
  ElementNode: TDOMElement absolute Node;
begin
  Self.ReadXML(InputDoc, aFile);
  try
    if not (InputDoc.DocumentElement.NodeName = 'fpdoc-descriptions') then
      raise Exception.CreateFmt('FPDoc root node %s not found in file %s.',
        ['fpdoc-descriptions', aFile]);
    for i := 0 to InputDoc.DocumentElement.ChildNodes.Count - 1 do
    begin
      Node := InputDoc.DocumentElement.ChildNodes.Item[i];
      if Node.NodeType = ELEMENT_NODE then
      begin
        if ElementNode.NodeName = 'package' then
          MergeFPDocIntoParent(ElementNode, aTargetDoc.DocumentElement);
      end;
    end;
  finally
    InputDoc.Destroy;
  end;
end;

// aSourceNode should be merged as child into aTargetParentNode
procedure TFPDocJoiner.MergeFPDocIntoParent(aSourceNode,
  aTargetParentNode: TDOMElement);
var
  xpSource: UnicodeString;
  res: TXPathVariable;
  i: Integer;
  childNode: TDOMNode;
  sourceStream, DestStream: TStringStream;
begin
  xpSource := GetXPathForNode(aSourceNode);
  res := EvaluateXPathExpression(xpSource, aTargetParentNode);
  try
    // element not in target, just clone it
    if res.AsNodeSet.Count = 0 then
      aTargetParentNode.AppendChild(
        aSourceNode.CloneNode(True, aTargetParentNode.OwnerDocument)
      )
    else
    begin
      Assert(res.AsNodeSet.Count = 1, 'Target document elements not unique.');
      if aSourceNode.NodeName in fTreeEndElements then
      begin

        // merge all contents of aSourceNode into
        sourceStream := TStringStream.Create('');
        DestStream   := TStringStream.Create('');
        try
          WriteXML(aSourceNode, sourceStream);
          WriteXML(TDOMNode(res.AsNodeSet.Items[0]), DestStream);
          if sourceStream.DataString = DestStream.DataString then
            exit;
          raise Exception.CreateFmt('Duplicate node %s.', [xpSource]);
        finally
          sourceStream.Destroy;
          DestStream.Destroy;
        end;
        exit;
      end;
      for i := 0 to aSourceNode.ChildNodes.Count - 1 do
      begin
        childNode := aSourceNode.ChildNodes.Item[i];
        if childNode.NodeType = ELEMENT_NODE then
          MergeFPDocIntoParent(TDOMElement(childNode),
            TDOMElement(res.AsNodeSet.Items[0])
          );
      end;
    end;
  finally
    res.Destroy;
  end;
end;

function TFPDocJoiner.GetPackageNode(aDoc: TXMLDocument;
  const aPackageName: UnicodeString): TDOMElement;
const
  PACKAGE_XPATH = '/fpdoc-descriptions/package[@name=''%s''][1]';
var
  xpExpr: UnicodeString;
  res: TXPathVariable;
  NodeSet: TNodeSet;
begin
  xpExpr := Format(PACKAGE_XPATH, [aPackageName]);
  res := EvaluateXPathExpression(xpExpr, aDoc.DocumentElement);
  Assert(res is TXPathNodeSetVariable, 'Invalid XPath result type.');
  NodeSet := res.AsNodeSet;
  Assert(NodeSet.Count <= 1, 'XPath returned too much package nodes.');
  if NodeSet.Count = 0 then
    Result := nil
  else
  begin
    Assert(TObject(NodeSet.Items[0]) is TDOMElement,
      'Unexpected node type from XPath.');
    Result := TDOMElement(NodeSet.Items[0]);
  end;
  Res.Destroy;
end;

function TFPDocJoiner.GetXPathForNode(aNode: TDOMNode): UnicodeString;
begin
  if aNode = aNode.OwnerDocument.DocumentElement then
    Result := '/'+aNode.NodeName
  else
  if aNode.NodeType = ELEMENT_NODE then
  begin
    if TDOMElement(aNode).hasAttribute('name') then
    begin
      Result := '[@name=''%s'']';
      Result := Format(Result, [TDOMElement(aNode).GetAttribute('name')]);
    end else
    if TDOMElement(aNode).NodeName = 'link' then
    begin
      Result := '[@id=''%s'']';
      Result := Format(Result, [TDOMElement(aNode).GetAttribute('id')]);
    end else
    if TDOMElement(aNode).NodeName = 'link' then
    begin
      Result := '[@file=''%s'']';
      Result := Format(Result, [TDOMElement(aNode).GetAttribute('file')]);
    end else
      Result := '';

    Result := GetXPathForNode(aNode.ParentNode)+'/'+aNode.NodeName+Result;
  end else
  if aNode.NodeType = TEXT_NODE then
    Result := GetXPathForNode(aNode.ParentNode)+'/text()';
end;

procedure TFPDocJoiner.CheckInputFiles;
var
  s: String;
  i: Integer;
begin
  for i := fInputFiles.Count - 1 downto 0 do
  begin
    s := Trim(fInputFiles.Strings[i]);
    if s = '' then
      fInputFiles.Delete(i)
    else
    begin
      Assert(Length(s) >= 1, 'Invalid string length.');
      if AllowCommentInFileList and (s[1] in [';','#'])then
      begin
        fInputFiles.Delete(i);
        continue;
      end else
      if not FileExists(s) then
        raise EFOpenError.CreateFmt('Input file "%s" doesnot exist.', [s]);
    end;
  end;
end;

constructor TFPDocJoiner.Create(const aFileList, aOutputFile: UnicodeString;
  aMergeTextNodes: Boolean; aAllowCommentInFileList: Boolean);
begin
  inherited Create;
  fFileListName := aFileList;
  fOutputFile   := aOutputFile;
  fInputFiles := TStringList.Create;
  fInputFiles.Duplicates := dupIgnore;
  fMergeTextNodes := aMergeTextNodes;
  fAllowCommentInFileList := aAllowCommentInFileList;

  SetLength(fTreeEndElements, 6);
  fTreeEndElements[0] := 'descr';
  fTreeEndElements[1] := 'short';
  fTreeEndElements[2] := 'version';
  fTreeEndElements[3] := 'errors';
  fTreeEndElements[4] := 'link';
  fTreeEndElements[5] := 'example';
end;

destructor TFPDocJoiner.Destroy;
begin
  fInputFiles.Free;
  inherited Destroy;
end;

procedure TFPDocJoiner.Process;
var
  f: String;
  Doc: TXMLDocument;
  RootNode: TDOMElement;
begin
  fInputFiles.LoadFromFile(FileListName);
  CheckInputFiles;
  Doc := TXMLDocument.Create;
  RootNode := Doc.CreateElement('fpdoc-descriptions');
  Doc.AppendChild(RootNode);
  try
    for f in fInputFiles do
    begin
      WriteLogCallback('Merging file "%s".', [f]);
      MergeFileIntoDocument(f, Doc);
    end;
    WriteLogCallback('Writing joined FPDoc file to: %s', [OutputFile]);
    WriteXML(Doc, OutputFile);
  finally
    Doc.Destroy;
  end;
end;

end.

