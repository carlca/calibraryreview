unit caNodes;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  SysUtils,
  Classes,
  Contnrs,
  Math,

  // ca units 
  caClasses,
  caCell,
  caUtils,
  caConsts,
  caTypes,
  caXMLData;

type

  //---------------------------------------------------------------------------
  // TcaNode                                                                   
  //---------------------------------------------------------------------------

  TcaNodeList = class;

  TcaNodes = class;

  TcaNode = class(TcaXMLDataItem)
  private
    FChecked: Boolean;
    FData: TcaCell;
    FDeleteOrigin: Boolean;
    FExpanded: Boolean;
    FFullDelete: Boolean;
    FFirstSub: TcaNode;
    FIndex: Integer;
    FLastSub: TcaNode;
    FLevel: Integer;
    FNextIso: TcaNode;
    FNodes: TcaNodes;
    FParent: TcaNode;
    FPriorIso: TcaNode;
    FTag: Integer;
    FText: string;
    // Property methods 
    function GetDataExt: Extended;
    function GetDataInt: Integer;
    function GetDataObj: TObject;
    function GetDataStr: string;
    function GetNext: TcaNode;
    function GetPrior: TcaNode;
    function GetRoot: TcaNode;
    function GetSubIndex: Integer;
    procedure SetDataExt(const Value: Extended);
    procedure SetDataInt(const Value: Integer);
    procedure SetDataObj(const Value: TObject);
    procedure SetDataStr(const Value: string);
    procedure SetExpanded(const Value: Boolean);
    procedure SetParent(const Value: TcaNode);
    // Private methods 
    function HasAsParent(Value: TcaNode): Boolean;
    procedure CleanUpSurroundingLinks;
    procedure Delete;
    procedure ExpandItem(IsExpanded: Boolean; Recurse: Boolean);
    procedure FreeSubNodes(ANode: TcaNode);
    procedure WriteNode(Buffer: PChar; Stream: TStream; StartLevel: Integer);
  protected
    // Protected methods 
    function LoadFloatValue(var AText: string; var FloatValue: Double): Boolean;
    function LoadIntegerValue(var AText: string; var IntValue: Integer): Boolean;
    function LoadStringValue(var AText: string; var StrValue: string): Boolean;
    procedure DoGetWriteNodeText(var AText: string); virtual;
    procedure DoParentChanged; virtual;
    procedure DoSetLoadNodeText(var AText: string); virtual;
    // Protected properties 
    property Data: TcaCell read FData;
    property DeleteOrigin: Boolean read FDeleteOrigin write FDeleteOrigin;
    property Nodes: TcaNodes read FNodes write FNodes;
  public
    // Create/Destroy 
    constructor Create; reintroduce; overload; virtual; 
    constructor Create(Collection: TCollection); overload; override;
    destructor Destroy; override;
    // Public methods 
    function AllParentsExpanded: Boolean;
    function HasParent: Boolean;
    function HasSubs: Boolean;
    function HasIsos: Boolean;
    function SubsCheckedState: TcaNodeCheckState;
    procedure Assign(Source: TPersistent); override;
    procedure Collapse(Recurse: Boolean);
    procedure Expand(Recurse: Boolean);
    procedure GetAllSubs(ANodeList: TcaNodeList);
    // Properties 
    property Checked: Boolean read FChecked write FChecked;
    property DataExt: Extended read GetDataExt write SetDataExt;
    property DataInt: Integer read GetDataInt write SetDataInt;
    property DataObj: TObject read GetDataObj write SetDataObj;
    property DataStr: string read GetDataStr write SetDataStr;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property FirstSub: TcaNode read FFirstSub write FFirstSub;
    property FullDelete: Boolean read FFullDelete write FFullDelete;
    property Index: Integer read FIndex write FIndex;
    property LastSub: TcaNode read FLastSub write FLastSub;
    property Level: Integer read FLevel write FLevel;
    property Next: TcaNode read GetNext;
    property NextIso: TcaNode read FNextIso write FNextIso;
    property Parent: TcaNode read FParent write SetParent;
    property Prior: TcaNode read GetPrior;
    property PriorIso: TcaNode read FPriorIso write FPriorIso;
    property Root: TcaNode read GetRoot;
    property SubIndex: Integer read GetSubIndex;
    property Tag: Integer read FTag write FTag;
  published
    // Published properties 
    property Text: string read FText write FText;
  end;

  //---------------------------------------------------------------------------
  // TcaNodeEnumerator
  //---------------------------------------------------------------------------

  TcaNodeEnumerator = class
  private
    // private members...
    FIndex: Integer;
    FNodeList: TcaNodeList;
    // property methods...
    function GetCurrent: TcaNode;
  public
    // lifetime...
    constructor Create(ANodeList: TcaNodeList);
    // public methods...
    function MoveNext: Boolean;
    property Current: TcaNode read GetCurrent;
  end;

  //---------------------------------------------------------------------------
  // TcaNodeList
  //---------------------------------------------------------------------------

  TcaNodeList = class(TObjectList)
  private
    // property methods...
    function GetNode(Index: Integer): TcaNode;
    procedure SetNode(Index: Integer; const Value: TcaNode);
  public
    // lifetime...
    constructor Create;
    // public methods...
    function GetEnumerator: TcaNodeEnumerator;
    function IndexOf(Value: TcaNode): Integer;
    procedure Add(Value: TcaNode);
    // properties...
    property Items[Index: Integer]: TcaNode read GetNode write SetNode; default;
  end;

  //---------------------------------------------------------------------------
  // IcaNodes                                                                  
  //---------------------------------------------------------------------------

  IcaNodes = interface
  ['{C24B3400-8A31-456B-B684-062FB7CA716A}']
    // Property methods 
    function GetLevelCount: Integer;
    function GetLoadNode: TcaNode;
    function GetNodeCount: Integer;
    function GetRoot(Index: Integer): TcaNode;
    function GetRootCount: Integer;
    function GetSaveNode: TcaNode;
    function GetVisibleNodeCount: Integer;
    function GetXML: string;
    procedure SetLoadNode(const Value: TcaNode);
    procedure SetSaveNode(const Value: TcaNode);
    procedure SetXML(const Value: string);
    // Event property methods 
    function GetOnChanged: TNotifyEvent;
    procedure SetOnChanged(const Value: TNotifyEvent);
    // Public methods 
    function AddIso(APriorIso: TcaNode; AText: string): TcaNode;
    function AddRoot(AText: string): TcaNode;
    function AddSub(AParent: TcaNode; AText: string): TcaNode;
    function AddSubAndCopy(AParent: TcaNode; AText: string): TcaNode;
    function InsertSub(AParent: TcaNode; AText: string): TcaNode;
    function IsBottomLevel(ANode: TcaNode): Boolean;
    procedure BeginUpdate;
    procedure GetAllNodes(ANodeList: TcaNodeList);
    procedure GetNodesForLevel(ANodeList: TcaNodeList; ALevel: Integer);
    procedure Clear;
    procedure Delete(Node: TcaNode);
    procedure EndUpdate;
    procedure FullCollapse;
    procedure FullExpand;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    // Properties 
    property LevelCount: Integer read GetLevelCount;
    property LoadNode: TcaNode read GetLoadNode write SetLoadNode;
    property NodeCount: Integer read GetNodeCount;
    property RootCount: Integer read GetRootCount;
    property Roots[Index: Integer]: TcaNode read GetRoot;
    property SaveNode: TcaNode read GetSaveNode write SetSaveNode;
    property VisibleNodeCount: Integer read GetVisibleNodeCount;
    property XML: string read GetXML write SetXML;
    // Event properties 
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  //---------------------------------------------------------------------------
  // TcaNodes                                                                  
  //---------------------------------------------------------------------------

  TcaNodesXMLData = class;

  TcaNodesXMLDataItem = class;

  TcaNodes = class(TInterfacedObject, IcaNodes)
  private
    // Private fields 
    FDestroying: Boolean;
    FLoadNode: TcaNode;
    FRoots: TList;
    FSaveNode: TcaNode;
    FUpdateCount: Integer;
    // Event property fields 
    FOnChanged: TNotifyEvent;
    // Private methods 
    procedure AddNode_Recurse(AXMLData: TcaNodesXMLData; AXMLDataItem: TcaNodesXMLDataItem; ANode: TcaNode);
    procedure AddXMLDataItem_Recurse(AXMLData: TcaNodesXMLData; ANode: TcaNode; AXMLDataItem: TcaNodesXMLDataItem);
    procedure Changed;
    procedure CheckRoots(Node: TcaNode);
    procedure SetExpandedAll(IsExpanded: Boolean);
    procedure UpdateIsoLinks(ANode, APriorIso: TcaNode);
    procedure UpdateRootLinks(ANode: TcaNode);
    procedure UpdateSubLinks(ANode, AParent: TcaNode; AInserting: Boolean);
  protected
    // Property methods 
    function GetLevelCount: Integer;
    function GetLoadNode: TcaNode;
    function GetNodeCount: Integer;
    function GetRoot(Index: Integer): TcaNode;
    function GetRootCount: Integer;
    function GetSaveNode: TcaNode;
    function GetVisibleNodeCount: Integer;
    function GetXML: string;
    procedure SetLoadNode(const Value: TcaNode);
    procedure SetSaveNode(const Value: TcaNode);
    procedure SetXML(const Value: string);
    // Event property methods 
    function GetOnChanged: TNotifyEvent;
    procedure SetOnChanged(const Value: TNotifyEvent);
    // Protected methods 
    function CreateNode: TcaNode; virtual;
    procedure DoAddIso(ANode, APriorIso: TcaNode; AText: string); virtual;
    procedure DoAddRoot(ANode: TcaNode; AText: string); virtual;
    procedure DoAddSub(ANode, AParent: TcaNode; AText: string); virtual;
    procedure DoChanged; virtual;
    procedure DoLoadFromStream; virtual;
    procedure NodeCollapsed(Node: TcaNode; Recurse: Boolean); virtual;
    procedure NodeExpanded(Node: TcaNode; Recurse: Boolean); virtual;
  public
    // lifetime...
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    function AddIso(APriorIso: TcaNode; AText: string): TcaNode;
    function AddRoot(AText: string): TcaNode;
    function AddSub(AParent: TcaNode; AText: string): TcaNode;
    function AddSubAndCopy(AParent: TcaNode; AText: string): TcaNode;
    function InsertSub(AParent: TcaNode; AText: string): TcaNode;
    function IsBottomLevel(ANode: TcaNode): Boolean;
    procedure BeginUpdate;
    procedure GetAllNodes(ANodeList: TcaNodeList);
    procedure GetNodesForLevel(ANodeList: TcaNodeList; ALevel: Integer);
    procedure Clear;
    procedure Delete(Node: TcaNode);
    procedure EndUpdate;
    procedure FullCollapse; virtual;
    procedure FullExpand; virtual;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromXML(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToXML(const AFileName: string);
    // Properties 
    property LevelCount: Integer read GetLevelCount;
    property LoadNode: TcaNode read GetLoadNode write SetLoadNode;
    property NodeCount: Integer read GetNodeCount;
    property RootCount: Integer read GetRootCount;
    property Roots[Index: Integer]: TcaNode read GetRoot;
    property SaveNode: TcaNode read GetSaveNode write SetSaveNode;
    property VisibleNodeCount: Integer read GetVisibleNodeCount;
    property XML: string read GetXML write SetXML;
    // Event properties 
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  //---------------------------------------------------------------------------
  // TcaNodesXMLData                                                           
  //---------------------------------------------------------------------------

  TcaNodesXMLData = class(TcaXMLData)
  public
    // Class methods 
    class function GetTagName: string; override;
  end;

  //---------------------------------------------------------------------------
  // TcaNodesXMLDataItem                                                       
  //---------------------------------------------------------------------------

  TcaNodesXMLDataItem = class(TcaNode)
  private
    // Property fields 
    FNodes: TcaNodesXMLData;
  public
    // Create/Destroy 
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // Class methods 
    class function GetTagName: string; override;
  published
    // Published properties 
    property Nodes: TcaNodesXMLData read FNodes write FNodes;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaNode                                                                   
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaNode.Create;
begin
  inherited Create(nil);
end;

constructor TcaNode.Create(Collection: TCollection);
begin
  inherited;
end;

destructor TcaNode.Destroy;
begin
  CleanUpSurroundingLinks;
  inherited;
end;

  // Public methods 

function TcaNode.AllParentsExpanded: Boolean;
var
  Node: TcaNode;
begin
  Result := True;
  Node := Parent;
  while Node <> nil do
    begin
      if not Node.Expanded then
        begin
          Result := False;
          Break;
        end;
      Node := Node.Parent;
    end;
end;

procedure TcaNode.Assign(Source: TPersistent);
begin
  if Source is TcaNode then
    Utils.CopyPublishedProperties(Source, Self, False, False)
  else
    inherited;
end;

procedure TcaNode.Collapse(Recurse: Boolean);
begin
  ExpandItem(False, Recurse);
  FNodes.NodeCollapsed(Self, Recurse);
end;

procedure TcaNode.Expand(Recurse: Boolean);
begin
  ExpandItem(True, Recurse);
  FNodes.NodeExpanded(Self, Recurse);
end;

procedure TcaNode.GetAllSubs(ANodeList: TcaNodeList);
var
  Node: TcaNode;
begin
  ANodeList.Clear;
  Node := FirstSub;
  while (Node <> nil) and (Node.HasAsParent(Self)) do
    begin
      ANodeList.Add(Node);
      Node := Node.Next;
    end;
end;

function TcaNode.HasIsos: Boolean;
begin
  Result := FNextIso <> nil;
end;

function TcaNode.HasParent: Boolean;
begin
  Result := FParent <> nil;
end;

function TcaNode.HasSubs: Boolean;
begin
  Result := FFirstSub <> nil;
end;

function TcaNode.SubsCheckedState: TcaNodeCheckState;
var
  NodeList: TcaNodeList;
  Index: Integer;
  CheckCount: Integer;
begin
  NodeList := TcaNodeList.Create;
  try
    GetAllSubs(NodeList);
    CheckCount := 0;
    for Index := 0 to NodeList.Count - 1 do
      if NodeList[Index].Checked then Inc(CheckCount);
    if CheckCount = 0 then
      Result := csFullyUnChecked
    else
      begin
        if CheckCount < NodeList.Count then
          Result := csSemiChecked
        else
          Result := csFullyChecked;
      end;
  finally
    NodeList.Free;
  end;
end;

  // Protected methods 

procedure TcaNode.DoGetWriteNodeText(var AText: string);
begin
  // Virtual 
end;

procedure TcaNode.DoParentChanged;
begin
  // Virtual 
end;

procedure TcaNode.DoSetLoadNodeText(var AText: string);
var
  P: Integer;
begin
  P := Pos('|', AText);
  if P > 0 then
    begin
      Text := Copy(AText, 1, P - 1);
      System.Delete(AText, 1, P);
    end
  else
    Text := AText;
end;

function TcaNode.LoadFloatValue(var AText: string; var FloatValue: Double): Boolean;
var
  P: Integer;
  ValStr: string;
begin
  P := Pos('|', AText);
  if P > 0 then
    begin
      ValStr := Copy(AText, 1, P - 1);
      FloatValue := Utils.StringToDouble(ValStr);
      System.Delete(AText, 1, P);
      Result := True;
    end
  else
    Result := False;
end;

function TcaNode.LoadIntegerValue(var AText: string; var IntValue: Integer): Boolean;
var
  P: Integer;
  ValStr: string;
begin
  P := Pos('|', AText);
  if P > 0 then
    begin
      ValStr := Copy(AText, 1, P - 1);
      IntValue := Utils.StringToInteger(ValStr);
      System.Delete(AText, 1, P);
      Result := True;
    end
  else
    Result := False;
end;

function TcaNode.LoadStringValue(var AText: string; var StrValue: string): Boolean;
var
  P: Integer;
begin
  P := Pos('|', AText);
  if P > 0 then
    begin
      StrValue := Copy(AText, 1, P - 1);
      System.Delete(AText, 1, P);
      Result := True;
    end
  else
    Result := False;
end;

  // Private methods 

procedure TcaNode.CleanUpSurroundingLinks;
var
  SubNode: TcaNode;
begin
  if FDeleteOrigin then FreeSubNodes(Self);
  FData.Free;
  if not FFullDelete then
    begin
      // Ensure that prior node points to next node 
      if FPriorIso <> nil then
        FPriorIso.NextIso := FNextIso;
      // Ensure that next node points to prior node 
      if FNextIso <> nil then
        FNextIso.PriorIso := FPriorIso;
      // If node is the first or last node of a parent, update the parents pointers 
      if HasParent then
        begin
          if (FParent.FirstSub = Self) and (FParent.LastSub = Self) then
            begin
              FParent.FirstSub := nil;
              FParent.LastSub := nil;
            end
          else
            begin
              if FParent.FirstSub = Self then
                FParent.FirstSub := FNextIso;
              if FParent.LastSub = Self then
                FParent.LastSub := FPriorIso;
            end;
        end;
      // Set the parent pointer of any sub-nodes to nil 
      if FFirstSub <> nil then
        begin
          SubNode := FFirstSub;
          while SubNode <> nil do
            begin
              SubNode.Parent := nil;
              SubNode := SubNode.NextIso;
            end;
        end;
    end;
end;

procedure TcaNode.Delete;
begin
  FDeleteOrigin := True;
  Free;
end;

procedure TcaNode.ExpandItem(IsExpanded, Recurse: Boolean);
var
  Node: TcaNode;
begin
  if Recurse then
    begin
      Node := Self;
      repeat
        Node.ExpandItem(IsExpanded, False);
        Node := Node.Next;
      until (Node = nil) or (not Node.HasAsParent(Self));
    end
  else
    FExpanded := IsExpanded;
end;

procedure TcaNode.FreeSubNodes(ANode: TcaNode);
var
  Index: Integer;
  SubList: TList;
  Node: TcaNode;
begin
  SubList := TList.Create;
  try
    Node := ANode.FirstSub;
    while (Node <> nil) and Node.HasAsParent(Self) do
      begin
        SubList.Add(Node);
        Node := Node.Next;
      end;
    for Index := 0 to SubList.Count - 1 do
      begin
        Node := TcaNode(SubList[Index]);
        Node.DeleteOrigin := False;
        Node.Free;
      end;
  finally
    SubList.Free;
  end;
end;

function TcaNode.HasAsParent(Value: TcaNode): Boolean;
begin
  if Value <> nil then
    begin
      if Parent = nil then
        Result := False
      else
        if Parent = Value then
          Result := True
        else
          Result := Parent.HasAsParent(Value);
    end
  else
    Result := True;
end;

procedure TcaNode.WriteNode(Buffer: PChar; Stream: TStream; StartLevel: Integer);
var
  BufPtr: PChar;
  NodeLevel: Word;
  SubNode: TcaNode;
  AText: string;
begin
  BufPtr := Buffer;
  NodeLevel := Level - StartLevel;
  while NodeLevel > 0 do
    begin
      BufPtr^ := #9;
      Dec(NodeLevel);
      Inc(BufPtr);
    end;
  AText := Text;
  DoGetWriteNodeText(AText);
  Utils.WriteStringToBuffer(AText, BufPtr);
  Stream.WriteBuffer(Buffer[0], BufPtr - Buffer);
  SubNode := FirstSub;
  while SubNode <> nil do
    begin
      SubNode.WriteNode(Buffer, Stream, StartLevel);
      SubNode := SubNode.NextIso;
    end;
end;

  // Property methods 

function TcaNode.GetDataExt: Extended;
begin
  if FData = nil then FData := TcaExtendedCell.Create;
  Result := TcaExtendedCell(FData).Value;
end;

function TcaNode.GetDataInt: Integer;
begin
  if FData = nil then FData := TcaIntegerCell.Create;
  Result := TcaIntegerCell(FData).Value;
end;

function TcaNode.GetDataObj: TObject;
begin
  if FData = nil then FData := TcaObjectCell.Create;
  Result := TcaObjectCell(FData).Value;
end;

function TcaNode.GetDataStr: string;
begin
  if FData = nil then FData := TcaStringCell.Create;
  Result := TcaStringCell(FData).Value;
end;

function TcaNode.GetNext: TcaNode;
var
  Node: TcaNode;
begin
  if HasSubs then
    Result := FFirstSub
  else
    if HasIsos then
      Result := FNextIso
    else
      begin
        Node := Parent;
        while (Node <> nil) and (Node.NextIso = nil) do
          Node := Node.Parent;
        if Node <> nil then
          Result := Node.NextIso
        else
          Result := nil;
      end;
end;

function TcaNode.GetPrior: TcaNode;
var
  Node: TcaNode;
begin
  if FPriorIso <> nil then
    begin
      if FPriorIso.HasSubs then
        begin
          Node := FPriorIso.LastSub;
          Result := nil;
          while Node <> nil do
            begin
              Result := Node;
              Node := Node.LastSub;
            end;
        end
      else
        Result := FPriorIso
    end
  else
    begin
      if HasParent then
        Result := Parent
      else
        Result := nil;
    end;
end;

function TcaNode.GetRoot: TcaNode;
begin
  Result := Self;
  while Result.HasParent do
    Result := Result.Parent;
end;

function TcaNode.GetSubIndex: Integer;
var
  Node: TcaNode;
begin
  if Parent = nil then
    Result := 0
  else
    begin
      Node := Parent.FirstSub;
      Result := 0;
      while Node <> nil do
        begin
          if Node = Self then Break;
          Node := Node.NextIso;
          Inc(Result);
        end;
    end;
end;

procedure TcaNode.SetDataExt(const Value: Extended);
begin
  if FData = nil then FData := TcaExtendedCell.Create;
  TcaExtendedCell(FData).Value := Value;
end;

procedure TcaNode.SetDataInt(const Value: Integer);
begin
  if FData = nil then FData := TcaIntegerCell.Create;
  TcaIntegerCell(FData).Value := Value;
end;

procedure TcaNode.SetDataObj(const Value: TObject);
begin
  if FData = nil then FData := TcaObjectCell.Create;
  TcaObjectCell(FData).Value := Value;
end;

procedure TcaNode.SetDataStr(const Value: string);
begin
  if FData = nil then FData := TcaStringCell.Create;
  TcaStringCell(FData).Value := Value;
end;

procedure TcaNode.SetExpanded(const Value: Boolean);
begin
  if Value <> FExpanded then
    begin
      FExpanded := Value;
      if FExpanded then
        Expand(False)
      else
        Collapse(False);
    end;
end;

procedure TcaNode.SetParent(const Value: TcaNode);
begin
  if Value <> FParent then
    begin
      FParent := Value;
      DoParentChanged;
    end;
end;

//---------------------------------------------------------------------------
// TcaNodeList                                                               
//---------------------------------------------------------------------------

// lifetime...

constructor TcaNodeEnumerator.Create(ANodeList: TcaNodeList);
begin
  inherited Create;
  FNodeList := ANodeList;
  FIndex := -1;
end;

// public methods...

function TcaNodeEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < Pred(FNodeList.Count);
  if Result then
    Inc(FIndex);
end;

// property methods...

function TcaNodeEnumerator.GetCurrent: TcaNode;
begin
  Result := FNodeList[FIndex];
end;

//---------------------------------------------------------------------------
// TcaNodeList                                                               
//---------------------------------------------------------------------------

// lifetime...

constructor TcaNodeList.Create;
begin
  inherited Create(False);
end;

// public methods...

function TcaNodeList.GetEnumerator: TcaNodeEnumerator;
begin
  Result := TcaNodeEnumerator.Create(Self);
end;

function TcaNodeList.IndexOf(Value: TcaNode): Integer;
begin
  Result := inherited IndexOf(Value);
end;

procedure TcaNodeList.Add(Value: TcaNode);
begin
  inherited Add(Value);
end;

// property methods...

function TcaNodeList.GetNode(Index: Integer): TcaNode;
begin
  Result := TcaNode(inherited Items[Index]);
end;

procedure TcaNodeList.SetNode(Index: Integer; const Value: TcaNode);
begin
  inherited Items[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaNodes                                                                  
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaNodes.Create;
begin
  inherited;
  FRoots := TList.Create;
end;

destructor TcaNodes.Destroy;
begin
  FDestroying := True;
  Clear;
  FRoots.Free;
  inherited;
end;

  // Public methods 

function TcaNodes.AddIso(APriorIso: TcaNode; AText: string): TcaNode;
begin
  Result := CreateNode;
  Result.Nodes := Self;
  Result.Text := AText;
  UpdateIsoLinks(Result, APriorIso);
  DoAddIso(Result, APriorIso, AText);
  Changed;
end;

function TcaNodes.AddRoot(AText: string): TcaNode;
begin
  Result := CreateNode;
  Result.Nodes := Self;
  Result.Text := AText;
  UpdateRootLinks(Result);
  DoAddRoot(Result, AText);
  Changed;
end;

function TcaNodes.AddSub(AParent: TcaNode; AText: string): TcaNode;
begin
  Result := CreateNode;
  Result.Nodes := Self;
  Result.Text := AText;
  UpdateSubLinks(Result, AParent, False);
  DoAddSub(Result, AParent, AText);
  Changed;
end;

function TcaNodes.AddSubAndCopy(AParent: TcaNode; AText: string): TcaNode;
var
  NodeList: TcaNodeList;
  Node: TcaNode;
  Index: Integer;
begin
  Result := AddSub(AParent, AText);
  NodeList := TcaNodeList.Create;
  try
    GetNodesForLevel(NodeList, AParent.Level);
    for Index := 0 to NodeList.Count - 1 do
      begin
        Node := NodeList[Index];
        if Node <> AParent then AddSub(Node, AText);
      end;
  finally
    NodeList.Free;
  end;
end;

function TcaNodes.InsertSub(AParent: TcaNode; AText: string): TcaNode;
begin
  Result := CreateNode;
  Result.Nodes := Self;
  Result.Text := AText;
  UpdateSubLinks(Result, AParent, True);
  Changed;
end;

procedure TcaNodes.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcaNodes.Clear;
var
  Index: Integer;
begin
  for Index := 0 to RootCount - 1 do
    Roots[Index].Delete;
  FRoots.Clear;
  if not FDestroying then Changed;
end;

procedure TcaNodes.Delete(Node: TcaNode);
begin
  if Node <> nil then
    begin
      if not FDestroying then CheckRoots(Node);
      Node.Delete;
      if not FDestroying then Changed;
    end;
end;

procedure TcaNodes.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TcaNodes.FullCollapse;
begin
  SetExpandedAll(False);
end;

procedure TcaNodes.FullExpand;
begin
  SetExpandedAll(True);
end;

procedure TcaNodes.GetAllNodes(ANodeList: TcaNodeList);
var
  Node: TcaNode;
begin
  ANodeList.Clear;
  Node := Roots[0];
  while Node <> nil do
    begin
      ANodeList.Add(Node);
      Node := Node.Next;
    end;
end;

procedure TcaNodes.GetNodesForLevel(ANodeList: TcaNodeList; ALevel: Integer);
var
  Node: TcaNode;
begin
  ANodeList.Clear;
  Node := Roots[0];
  while Node <> nil do
    begin
      if Node.Level = ALevel then ANodeList.Add(Node);
      Node := Node.Next;
    end;
end;

function TcaNodes.IsBottomLevel(ANode: TcaNode): Boolean;
begin
  Result := ANode.FirstSub = nil;
end;

procedure TcaNodes.LoadFromFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TcaNodes.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  XMLData: TcaNodesXMLData;
  XMLDataItem: TcaNodesXMLDataItem;
begin
  BeginUpdate;
  Clear;
  XMLData := Auto(TcaNodesXMLData.Create(TcaNodesXMLDataItem)).Instance;
  XMLData.LoadFromStream(Stream);
  for Index := 0 to Pred(XMLData.Count) do
    begin
      XMLDataItem := TcaNodesXMLDataItem(XMLData[Index]);
      AddNode_Recurse(XMLData, XMLDataItem, nil);
    end;
  EndUpdate;
end;

procedure TcaNodes.LoadFromXML(const AFileName: string);
begin
  LoadFromFile(AFileName);
end;

procedure TcaNodes.SaveToFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TcaNodes.SaveToStream(Stream: TStream);
var
  Index: Integer;
  Node: TcaNode;
  XMLData: TcaNodesXMLData;
begin
  XMLData := Auto(TcaNodesXMLData.Create(TcaNodesXMLDataItem)).Instance;
  for Index := 0 to Pred(GetRootCount) do
    begin
      Node := FRoots[Index];
      AddXMLDataItem_Recurse(XMLData, Node, nil);
    end;
  XMLData.SaveToStream(Stream);
end;

procedure TcaNodes.SaveToXML(const AFileName: string);
begin
  SaveToFile(AFileName);
end;

  // Protected methods 

function TcaNodes.CreateNode: TcaNode;
begin
  Result := TcaNode.Create(nil);
end;

procedure TcaNodes.DoAddIso(ANode, APriorIso: TcaNode; AText: string);
begin
  // Virtual 
end;

procedure TcaNodes.DoAddRoot(ANode: TcaNode; AText: string);
begin
  // Virtual 
end;

procedure TcaNodes.DoAddSub(ANode, AParent: TcaNode; AText: string);
begin
  // Virtual 
end;

procedure TcaNodes.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TcaNodes.DoLoadFromStream;
begin
  // Virtual 
end;

procedure TcaNodes.NodeCollapsed(Node: TcaNode; Recurse: Boolean);
begin
  Changed;
end;

procedure TcaNodes.NodeExpanded(Node: TcaNode; Recurse: Boolean);
begin
  Changed;
end;

  // Private methods 

procedure TcaNodes.AddNode_Recurse(AXMLData: TcaNodesXMLData; AXMLDataItem: TcaNodesXMLDataItem; ANode: TcaNode);
var
  Index: Integer;
  Node: TcaNode;
  SubXMLDataItem: TcaNodesXMLDataItem;
begin
  if not Assigned(ANode) then
    Node := AddRoot('')
  else
    Node := AddSub(ANode, '');
  Node.Assign(AXMLDataItem);
  for Index := 0 to Pred(AXMLDataItem.Nodes.Count) do
    begin
      SubXMLDataItem := TcaNodesXMLDataItem(AXMLDataItem.Nodes[Index]);
      AddNode_Recurse(AXMLData, SubXMLDataItem, Node);
    end;
end;

procedure TcaNodes.AddXMLDataItem_Recurse(AXMLData: TcaNodesXMLData; ANode: TcaNode; AXMLDataItem: TcaNodesXMLDataItem);
var
  SubNode: TcaNode;
  XMLDataItem: TcaNodesXMLDataItem;
begin
  if not Assigned(AXMLDataItem) then
    XMLDataItem := TcaNodesXMLDataItem(AXMLData.Add)
  else
    XMLDataItem := TcaNodesXMLDataItem(AXMLDataItem.Nodes.Add);
  XMLDataItem.Assign(ANode);
  SubNode := ANode.FirstSub;
  while Assigned(SubNode) do
    begin
      AddXMLDataItem_Recurse(AXMLData, SubNode, XMLDataItem);
      SubNode := SubNode.NextIso;
    end;
end;

procedure TcaNodes.Changed;
begin
  if FUpdateCount = 0 then DoChanged;
end;

procedure TcaNodes.CheckRoots(Node: TcaNode);
var
  Index: Integer;
begin
  for Index := 0 to FRoots.Count - 1 do
    if FRoots[Index] = Node then
      begin
        FRoots.Delete(Index);
        Break;
      end;
end;

procedure TcaNodes.SetExpandedAll(IsExpanded: Boolean);
var
  Node: TcaNode;
begin
  BeginUpdate;
  Node := Roots[0];
  while Node <> nil do
    begin
      Node.Expanded := IsExpanded;
      Node := Node.Next;
    end;
  EndUpdate;
end;

procedure TcaNodes.UpdateIsoLinks(ANode, APriorIso: TcaNode);
var
  NextIso: TcaNode;
begin
  NextIso := APriorIso.NextIso;
  ANode.Parent := APriorIso.Parent;
  ANode.PriorIso := APriorIso;
  ANode.Level := APriorIso.Level;
  APriorIso.NextIso := ANode;
  ANode.NextIso := NextIso;
end;

procedure TcaNodes.UpdateRootLinks(ANode: TcaNode);
begin
  FRoots.Add(ANode);
  if FRoots.Count > 1 then
    begin
      ANode.PriorIso := TcaNode(FRoots[FRoots.Count - 2]);
      ANode.PriorIso.NextIso := ANode;
    end;
end;

procedure TcaNodes.UpdateSubLinks(ANode, AParent: TcaNode; AInserting: Boolean);
begin
  ANode.Parent := AParent;
  ANode.Level := AParent.Level + 1;
  if AParent.FirstSub = nil then
    begin
      AParent.FirstSub := ANode;
      AParent.LastSub := ANode;
    end
  else
    begin
      if AInserting then
        begin
          ANode.PriorIso := nil;
          ANode.NextIso := AParent.FirstSub;
          AParent.FirstSub.PriorIso := ANode;
          AParent.FirstSub := ANode;
        end
      else
        begin
          ANode.PriorIso := AParent.LastSub;
          ANode.PriorIso.NextIso := ANode;
          AParent.LastSub := ANode;
        end;
    end;
end;

  // Property methods 

function TcaNodes.GetLevelCount: Integer;
var
  Node: TcaNode;
begin
  Result := 0;
  Node := Roots[0];
  while Node <> nil do
    begin
      Result := Max(Result, Node.Level);
      Node := Node.Next;
    end;
  Inc(Result);
end;

function TcaNodes.GetLoadNode: TcaNode;
begin
  Result := FLoadNode;
end;

function TcaNodes.GetNodeCount: Integer;
var
  Node: TcaNode;
begin
  Result := 0;
  Node := Roots[0];
  while Node <> nil do
    begin
      Inc(Result);
      Node := Node.Next;
    end;
end;

function TcaNodes.GetRoot(Index: Integer): TcaNode;
begin
  Result := nil;
  if Index < GetRootCount then
    Result := TcaNode(FRoots[Index]);
end;

function TcaNodes.GetRootCount: Integer;
begin
  Result := FRoots.Count;
end;

function TcaNodes.GetSaveNode: TcaNode;
begin
  Result := FSaveNode;
end;

function TcaNodes.GetVisibleNodeCount: Integer;
var
  Node: TcaNode;
  IncCount: Boolean;
begin
  Result := 0;
  Node := Roots[0];
  while Node <> nil do
    begin
      if Node.Parent = nil then
        IncCount := True
      else
        IncCount := Node.Parent.Expanded;
      if IncCount then Inc(Result);
      Node := Node.Next;
    end;
end;

function TcaNodes.GetXML: string;
var
  Stream: TStringStream;
begin
  Stream := Auto(TStringStream.Create('')).Instance;
  SaveToStream(Stream);
  Stream.Position := 0;
  Result := Stream.DataString;
end;

procedure TcaNodes.SetLoadNode(const Value: TcaNode);
begin
  FLoadNode := Value;
end;

procedure TcaNodes.SetSaveNode(const Value: TcaNode);
begin
  FSaveNode := Value;
end;

procedure TcaNodes.SetXML(const Value: string);
var
  Stream: TStringStream;
begin
  Stream := Auto(TStringStream.Create(Value)).Instance;
  Stream.Position := 0;
  LoadFromStream(Stream);
end;

  // Event property methods 

function TcaNodes.GetOnChanged: TNotifyEvent;
begin
  Result := FOnChanged;
end;

procedure TcaNodes.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

  //---------------------------------------------------------------------------
  // TcaNodesXMLData                                                           
  //---------------------------------------------------------------------------

  // Class methods 

class function TcaNodesXMLData.GetTagName: string;
begin
  Result := cNodes;
end;

  //---------------------------------------------------------------------------
  // TcaNodesXMLDataItem                                                       
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaNodesXMLDataItem.Create(Collection: TCollection);
begin
  inherited;
  FNodes := TcaNodesXMLData.Create(TcaNodesXMLDataItem);
end;

destructor TcaNodesXMLDataItem.Destroy;
begin
  FNodes.Free;
  inherited;
end;

  // Class methods 

class function TcaNodesXMLDataItem.GetTagName: string;
begin
  Result := cNode;
end;

end.
