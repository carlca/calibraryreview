unit caXMLData;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  SysUtils,
  TypInfo,
  DB,
  {$IFDEF D5}
  FileCtrl,
  {$ENDIF}
  {$IFDEF D7_UP}
  Variants,
  {$ENDIF}

  // ca units 
  caClasses,
  caBaseDataset,
  caRTTI,
  caTypes,
  caUtils,
  caXML,
  caLog;

type

  TcaXMLDataItem = class;

  XMLString = type AnsiString;

  TcaXMLDataItemValueEvent = procedure(Sender: TObject; const AColumnName: string; AItem: TcaXMLDataItem; var AValue: Variant) of object;

  TcaXMLDataLoadMode = (lmLocal, lmShared, lmBoth);

  //---------------------------------------------------------------------------
  // IcaXMLDataObject                                                          
  //---------------------------------------------------------------------------

  IcaXMLDataObject = interface(IUnknown)
    ['{FF809F5B-0856-4BFB-A1D0-7271DFD306A5}']
    // Property methods 
    function GetElementText: string;
    function GetXML: string;
    procedure SetElementText(const AXML: string);
    procedure SetXML(const AXML: string);
    // Interface methods 
    function GetDTDElements: string;
    function GetDTDSequence: string;
    // Properties 
    property ElementText: string read GetElementText write SetElementText;
    property XML: string read GetXML write SetXML;
  end;

  //---------------------------------------------------------------------------
  // TcaInterfacedXMLDataObject                                                
  //---------------------------------------------------------------------------

  TcaInterfacedXMLDataObject = class(TInterfacedObject, IcaXMLDataObject)
  protected
    // Property methods 
    function GetElementText: string; virtual;
    function GetXML: string;
    procedure SetElementText(const AXML: string); virtual;
    procedure SetXML(const AXML: string);
  public
    constructor Create(const AXML: string = ''); overload; virtual;
    // Class methods 
    class function GetTagName: string; virtual;
    // Interface methods 
    function GetDTDElements: string;
    function GetDTDSequence: string;
    // Properties 
    property ElementText: string read GetElementText write SetElementText;
    property XML: string read GetXML write SetXML;
  end;

  //---------------------------------------------------------------------------
  //  TcaXMLObject                                                             
  //---------------------------------------------------------------------------

  TcaXMLObject = class(TPersistent)
  protected
    // Property methods 
    function GetElementText: string; virtual;
    function GetXML: string;
    procedure SetElementText(const AXML: string); virtual;
    procedure SetXML(const AXML: string);
  public
    constructor Create(const AXML: string = ''); overload; virtual;
    // Class methods 
    class function GetTagName: string; virtual;
    // Public methods 
    function GetDTD: string;
    function GetDTDElements: string;
    function GetDTDSequence: string;
    // Properties 
    property ElementText: string read GetElementText write SetElementText;
    property XML: string read GetXML write SetXML;
  end;

  //---------------------------------------------------------------------------
  // TcaXMLList                                                                
  //---------------------------------------------------------------------------

  TcaXMLList = class(TStringList)
  protected
    // Property methods 
    function GetElementText: string; virtual;
    function GetXML: string;
    procedure SetElementText(const AXML: string); virtual;
    procedure SetXML(const AXML: string);
  public
    // Class methods 
    class function GetItemTagName: string; virtual;
    class function GetTagName: string; virtual;
    // Public methods 
    function GetDTDElements: string;
    function GetDTDSequence: string;
    // Properties 
    property ElementText: string read GetElementText write SetElementText;
    property XML: string read GetXML write SetXML;
  end;

  {$IFDEF D5}

  //---------------------------------------------------------------------------
  // TcaNotifyCollection                                                       
  //---------------------------------------------------------------------------

  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);

  TcaNotifyCollection = class(TCollection)
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); virtual;
    procedure SetItemName(Item: TCollectionItem); override;
  end;

  {$ELSE}

  TcaNotifyCollection = class(TCollection);

  {$ENDIF}

  //---------------------------------------------------------------------------
  // TcaXMLDataItem                                                            
  //---------------------------------------------------------------------------

  TcaXMLDataItem = class(TCollectionItem)
  private
    // Private fields 
    FID: TcaUniqueID;
    FIsNew: Boolean;
  protected
    // Protected methods 
    function GetElementText: string; virtual;
    function GetXML: string;
    function GetXMLAsProperties: string;
    procedure SetElementText(const AXML: string); virtual;
    procedure SetXML(const AXML: string);
  public
    // Class methods 
    class function GetTagName: string; virtual;
    // Public methods 
    function GetDTDElements: string;
    function GetDTDSequence: string;
    // Properties 
    property ElementText: string read GetElementText write SetElementText;
    property IsNew: Boolean read FIsNew write FIsNew;
    property XML: string read GetXML write SetXML;
  published
    // Published fields 
    property ID: TcaUniqueID read FID write FID;
  end;

  TcaXMLDataItemClass = class of TcaXMLDataItem;

  //---------------------------------------------------------------------------
  // TcaXMLData                                                                
  //---------------------------------------------------------------------------

  TcaXMLData = class(TcaNotifyCollection)
  private
    // Private fields 
    FFileName: string;
    FItemClass: TcaXMLDataItemClass;
    FLoaded: Boolean;
    FLoadedXML: TStringList;
    FLoadMode: TcaXMLDataLoadMode;
    FName: string;
    FLocalPath: string;
    FSharedPath: string;
    // Private methods 
    function GetFileName: string;
    function GetLocalFilePath: string;
    function GetSharedFilePath: string;
    function InternalLoadFromFile(const AFileName: string; IsCustom: Boolean): Boolean;
    function InternalLoadFromStream(AStream: TStream; IsCustom: Boolean): Boolean;
    function UTF8Header: string;
    procedure CustomParseXML(const AXML: string);
    procedure UpdateLocalFromShared;
    procedure UpdateSharedFromLocal;
    // Event handlers 
    procedure CustomReadDataEvent(Sender: TObject; const ATag, AData, AAttributes: string; ALevel: Integer);
    procedure CustomReadEndTagEvent(Sender: TObject; const ATag: string; ALevel: Integer);
    procedure CustomReadTagEvent(Sender: TObject; const ATag, AAttributes: string; ALevel: Integer);
  protected
    // Property methods 
    function GetElementText: string; virtual;
    function GetXML: string;
    function GetXMLCollectionItem(Index: Integer): TcaXMLDataItem;
    procedure DoCustomReadData(const ATag, AData, AAttributes: string; ALevel: Integer); virtual;
    procedure DoCustomReadEndTag(const ATag: string; ALevel: Integer); virtual;
    procedure DoCustomReadTag(const ATag, AAttributes: string; ALevel: Integer); virtual;
    procedure SetElementText(const AXML: string); virtual;
    procedure SetFileName(const Value: string);
    procedure SetXML(const AXML: string);
    procedure SetXMLCollectionItem(Index: Integer; Value: TcaXMLDataItem);
    // Virtual methods 
    procedure DoGetName(var AName: string); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    // Properties 
    property Loaded: Boolean read FLoaded;
  public
    // Create/Destroy 
    constructor Create(ItemClass: TcaXMLDataItemClass); virtual;
    destructor Destroy; override;
    // Class methods 
    class function GetTagName: string; virtual;
    // Public methods 
    function Add: TcaXMLDataItem;
    function Clone(APopulate: Boolean = True): TcaXMLData;
    function CustomLoadFromFile(const AFileName: string): Boolean;
    function CustomLoadFromStream(AStream: TStream): Boolean;
    function GetDTDElements: string;
    function GetDTDSequence: string;
    function GetItemTagName: string; virtual;
    function GetPropertiesXML: string;
    function GetSubsetElementText(Start, Ct: Integer): string;
    function IndexOfID(const ID: string): Integer;
    function IndexOfItem(AItem: TObject): Integer;
    function Load: Boolean;
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    procedure Discard;
    procedure Save;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure Sort(Compare: TListSortCompare);
    // Properties 
    property ElementText: string read GetElementText write SetElementText;
    property FileName: string read FFileName write SetFileName;
    property Items[Index: Integer]: TcaXMLDataItem read GetXMLCollectionItem write SetXMLCollectionItem; default;
    property LoadMode: TcaXMLDataLoadMode read FLoadMode write FLoadMode;
    property LocalPath: string read FLocalPath write FLocalPath;
    property Name: string read FName write FName;
    property SharedPath: string read FSharedPath write FSharedPath;
    property XML: string read GetXML write SetXML;
  end;

  TcaXMLDataClass = class of TcaXMLData;

  //---------------------------------------------------------------------------
  // TcaOwnedXMLCollection                                                     
  //---------------------------------------------------------------------------

  TcaOwnedXMLCollection = class(TcaXMLData)
  private
    // Private fields 
    FOwner: TPersistent;
  protected
    // Protected methods 
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TcaXMLDataItemClass); reintroduce;
  end;

  //---------------------------------------------------------------------------
  // TcaXMLDataUtils                                                           
  //---------------------------------------------------------------------------

  TcaXMLDataUtils = class(TObject)
  private
    // Private fields 
    FXMLDataObjects: TList;
    // Private methods 
    function GetObjectDTDElements(const AObject: TObject): string;
    function GetObjectDTDSequence(const AObject: TObject): string;
    function GetPropAsString(const Instance: TObject; const PropInfo: PPropInfo): string;
    function GetPropertyDTD(Instance: TObject; PropInfo: PPropInfo): string;
    function GetPropertyDTDElements(Instance: TObject; PropInfo: PPropInfo): string;
    function GetPropertyList(const ClassInfo: Pointer): IcaObjectList;
    function GetTagContent(const AXML, TargetTagName: string): string;
    procedure SetPropAsString(Instance: TObject; PropInfo: PPropInfo; value: string);
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    function ObjectToXMLElements(const AObject: TObject): string;
    function ObjectToXMLProperties(const AObject: TObject): string;
    function ParseTag(const Source, StartTag, EndTag: string; var Index: Integer): string;
    function StrToXML(const AXML: string): string;
    function XMLtoStr(const AXML: string): string;
    procedure FormatXML(var AXML: string);
    procedure InsertLineBreaks(var AXML: string);
    procedure XMLToObject(Instance: TObject; AXML: AnsiString);
  end;

  //---------------------------------------------------------------------------
  // TcaXMLDataset                                                             
  //---------------------------------------------------------------------------

  TcaXMLDataset = class(TcaBaseDataset)
  private
    // Private fields 
    FXMLData: TcaXMLData;
    FItemIndex: Integer;
    FNonUserChangedCols: TBits;
    FRTTIDummyItem: TcaXMLDataItem;
    FRTTIList: TcaRTTIList;
    // Event property fields 
    FOnGetItemValue: TcaXMLDataItemValueEvent;
    FOnSetItemValue: TcaXMLDataItemValueEvent;
    // Property methods 
    function GetActiveItem: TcaXMLDataItem;
    function GetRTTIList: TcaRTTIList;
    function GetXMLData: TcaXMLData;
    procedure SetXMLData(const Value: TcaXMLData);
    // Event property methods 
    function GetOnGetItemValue: TcaXMLDataItemValueEvent;
    function GetOnSetItemValue: TcaXMLDataItemValueEvent;
    procedure SetOnGetItemValue(const Value: TcaXMLDataItemValueEvent);
    procedure SetOnSetItemValue(const Value: TcaXMLDataItemValueEvent);
    // Private methods 
    function TypeKindToDataType(ATypeKind: TTypeKind): TFieldType;
    procedure UpdateRTTI;
  protected
    //---------------------------------------------------------------------------
    // Overridden abstract methods from TcaBaseDataset                           
    //---------------------------------------------------------------------------
    // Basic DB methods                                                          
    function CanOpen: Boolean; override;
    function GetFieldValue(Field: TField): Variant; override;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); override;
    procedure DoClose; override;
    procedure DoCreateFieldDefs; override;
    procedure DoDeleteRecord; override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
    procedure SetFieldValue(Field: TField; Value: Variant); override;
    // Buffer ID methods 
    function AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;
    procedure GotoRecordID(Value: Pointer); override;
    // BookMark functions 
    function GetBookMarkSize: Integer; override;
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); override;
    procedure DoGotoBookmark(Bookmark: Pointer); override;
    // Navigation methods 
    function Navigate(GetMode: TGetMode): TGetResult; override;
    procedure DoFirst; override;
    procedure DoLast; override;
    //---------------------------------------------------------------------------
    // Overridden methods from TDataset                                          
    //---------------------------------------------------------------------------
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure CreateFields; override;
    procedure DoAfterPost; override;
    procedure DoBeforePost; override;
    procedure SetActive(Value: Boolean); override;
    procedure SetRecNo(Value: Integer); override;
    //---------------------------------------------------------------------------
    // Other protected methods                                                   
    //---------------------------------------------------------------------------
    procedure DoGetItemValue(const AColumnName: string; AItem: TcaXMLDataItem; var AValue: Variant); virtual;
    procedure DoSetItemValue(const AColumnName: string; AItem: TcaXMLDataItem; var AValue: Variant); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public properties 
    property RTTIList: TcaRTTIList read GetRTTIList;
    property XMLData: TcaXMLData read GetXMLData write SetXMLData;
  published
    // Promoted properties 
    property Active;
    property ActiveItem: TcaXMLDataItem read GetActiveItem;
    property ReadOnly;
    // Event properties 
    property OnGetItemValue: TcaXMLDataItemValueEvent read GetOnGetItemValue write SetOnGetItemValue;
    property OnSetItemValue: TcaXMLDataItemValueEvent read GetOnSetItemValue write SetOnSetItemValue;
    // Promoted event properties 
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;

  //---------------------------------------------------------------------------
  // EcaXMLException                                                           
  //---------------------------------------------------------------------------

  EcaXMLException = class(Exception)
  end;

var
  XMLUtils: TcaXMLDataUtils;

implementation

const
  cXMLPrologTag = '?xml version = "1.0"?';
  cXMLProlog = '<' + cXMLPrologTag + '>';
  cXMLListItemTagName = 'value';
  cCDATADescriptor = '<![CDATA[';
  cCDATADescriptorClose = ']]>';
  cDocTypeHeader = '<!DOCTYPE ';
  cElementHeader = '<!ELEMENT ';

  //---------------------------------------------------------------------------
  // TcaInterfacedXMLDataObject                                                
  //---------------------------------------------------------------------------

constructor TcaInterfacedXMLDataObject.Create(const AXML: string = '');
begin
  inherited Create;
  if AXML <> '' then SetXML(AXML);
end;

  // Class methods 

class function TcaInterfacedXMLDataObject.GetTagName: string;
begin
  Result := Copy(ClassName, 2, Length(ClassName) - 1);
end;

function TcaInterfacedXMLDataObject.GetDTDElements: string;
begin
  Result := XMLUtils.GetObjectDTDElements(Self);
end;

function TcaInterfacedXMLDataObject.GetDTDSequence: string;
begin
  Result := XMLUtils.GetObjectDTDSequence(Self);
end;

  // Property methods 

function TcaInterfacedXMLDataObject.GetElementText: string;
begin
  Result := XMLUtils.ObjectToXMLElements(Self);
end;

function TcaInterfacedXMLDataObject.GetXML: string;
begin
  Result := '<' + GetTagName + '>' + GetElementText + '</' + GetTagName + '>';
end;

procedure TcaInterfacedXMLDataObject.SetElementText(const AXML: string);
begin
  XMLUtils.XMLToObject(Self, AXML);
end;

procedure TcaInterfacedXMLDataObject.SetXML(const AXML: string);
begin
  SetElementText(XMLUtils.GetTagContent(AXML, GetTagName));
end;

  //---------------------------------------------------------------------------
  // TcaXMLObject                                                              
  //---------------------------------------------------------------------------

constructor TcaXMLObject.Create(const AXML: string = '');
begin
  inherited Create;
  if AXML <> '' then SetXML(AXML);
end;

  // Class methods 

class function TcaXMLObject.GetTagName: string;
begin
  Result := Copy(ClassName, 2, Length(ClassName) - 1);
end;

  // Public methods 

function TcaXMLObject.GetDTD: string;
begin
  Result := cDocTypeHeader + GetTagName + ' [' + #13#10 +
    cElementHeader + GetTagName + ' (' + GetDTDSequence + ') >' + #13#10 +
    GetDTDElements + ']>'
end;

function TcaXMLObject.GetDTDElements: string;
begin
  Result := XMLUtils.GetObjectDTDElements(Self);
end;

function TcaXMLObject.GetDTDSequence: string;
begin
  Result := XMLUtils.GetObjectDTDSequence(Self);
end;

  // Property methods 

function TcaXMLObject.GetElementText: string;
begin
  Result := XMLUtils.ObjectToXMLElements(Self);
end;

function TcaXMLObject.GetXML: string;
begin
  Result := '<' + GetTagName + '>' + GetElementText + '</' + GetTagName + '>';
end;

procedure TcaXMLObject.SetElementText(const AXML: string);
begin
  XMLUtils.XMLToObject(Self, AXML);
end;

procedure TcaXMLObject.SetXML(const AXML: string);
begin
  SetElementText(XMLUtils.GetTagContent(AXML, GetTagName));
end;

  //---------------------------------------------------------------------------
  // TcaXMLList                                                                
  //---------------------------------------------------------------------------

  // Class methods 

class function TcaXMLList.GetItemTagName: string;
begin
  Result := cXMLListItemTagName;
end;

class function TcaXMLList.GetTagName: string;
begin
  Result := Copy(ClassName, 2, Length(ClassName) - 2);
end;

  // Public methods 

function TcaXMLList.GetDTDElements: string;
begin
  Result := XMLUtils.GetObjectDTDElements(Self);
end;

function TcaXMLList.GetDTDSequence: string;
begin
  Result := XMLUtils.GetObjectDTDSequence(Self);
  if Result = '' then
    Result := GetItemTagName + '*'
  else
    Result := GetItemTagName + '*, ' + Result;
end;

  // Property methods 

function TcaXMLList.GetElementText: string;
var
  Index: Integer;
  ItemTagName: string;
  List: TStringList;
begin
  Result := '';
  ItemTagName := GetItemTagName;
  List := TStringList.Create;
  try
    List.Text := XMLUtils.ObjectToXMLElements(Self);
    for Index := 0 to Pred(Count) do
      List.Add('<' + ItemTagName + '>' + Strings[Index] + '</' + ItemTagName + '>');
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TcaXMLList.GetXML: string;
begin
  Result := '<' + GetTagName + '>' + GetElementText + '</' + GetTagName + '>';
end;

procedure TcaXMLList.SetElementText(const AXML: string);
var
  CurrentTag: string;
  CurrentTagContent: string;
  CurrentTagIndex: Integer;
  ItemTagName: string;
  OverallIndex: Integer;
begin
  ItemTagName := GetItemTagName;
  CurrentTagIndex := 1;
  OverallIndex := 1;
  repeat
    CurrentTag := XMLUtils.ParseTag(AXML, '<', '>', OverallIndex);
    CurrentTagContent := XMLUtils.ParseTag(AXML, '<' + CurrentTag + '>', '</' + CurrentTag + '>', CurrentTagIndex);
    if (Length(CurrentTag) > 0) then
      begin
        if (CurrentTag = ItemTagName) then
          Add(CurrentTagContent)
        else
          XMLUtils.SetPropAsString(Self, GetPropInfo(ClassInfo, CurrentTag), CurrentTagContent);
      end;
    OverallIndex := CurrentTagIndex;
  until (OverallIndex < 1) or (OverallIndex > Length(AXML));
end;

procedure TcaXMLList.SetXML(const AXML: string);
begin
  SetElementText(XMLUtils.GetTagContent(AXML, GetTagName));
end;

{$IFDEF D5}

  //---------------------------------------------------------------------------
  // TcaNotifyCollection                                                       
  //---------------------------------------------------------------------------

procedure TcaNotifyCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  // Virtual 
end;

  // A crude mechanism to catch inserted items and fire the notify virtual method. 
  // This is to simulate the Notify mechanism in Delphi 6. It only works for Adding. 

procedure TcaNotifyCollection.SetItemName(Item: TCollectionItem);
begin
  if Item = Items[Pred(Count)] then
    Notify(Item, cnAdded);
end;

{$ENDIF}

  //---------------------------------------------------------------------------
  // TcaXMLDataItem                                                            
  //---------------------------------------------------------------------------

  // Class methods 

class function TcaXMLDataItem.GetTagName: string;
begin
  Result := Copy(ClassName, 2, Pos('Data', ClassName) - 2);
end;

  // Public methods 

function TcaXMLDataItem.GetDTDElements: string;
begin
  Result := XMLUtils.GetObjectDTDElements(Self);
end;

function TcaXMLDataItem.GetDTDSequence: string;
begin
  Result := XMLUtils.GetObjectDTDSequence(Self);
end;

  // Protected methods 

function TcaXMLDataItem.GetElementText: string;
begin
  Result := XMLUtils.ObjectToXMLElements(Self);
end;

function TcaXMLDataItem.GetXML: string;
begin
  Result := '<' + GetTagName + '>' + GetElementText + '</' + GetTagName + '>';
end;

function TcaXMLDataItem.GetXMLAsProperties: string;
begin
  Result := '<' + GetTagName + ' ' + XMLUtils.ObjectToXMLProperties(Self) + ' />'
end;

procedure TcaXMLDataItem.SetElementText(const AXML: string);
begin
  XMLUtils.XMLToObject(Self, AXML);
end;

procedure TcaXMLDataItem.SetXML(const AXML: string);
begin
  ElementText := XMLUtils.GetTagContent(AXML, GetTagName);
end;

  //---------------------------------------------------------------------------
  // TcaXMLData                                                                
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaXMLData.Create(ItemClass: TcaXMLDataItemClass);
begin
  inherited Create(ItemClass);
  FItemClass := ItemClass;
  FLoadedXML := TStringList.Create;
  DoGetName(FName);
end;

destructor TcaXMLData.Destroy;
begin
  FLoadedXML.Free;
  inherited;
end;

  // Class methods 

class function TcaXMLData.GetTagName: string;
begin
  Result := Copy(ClassName, 2, Pos('Data', ClassName) - 2) + 'List';
end;

  // Public methods 

function TcaXMLData.Add: TcaXMLDataItem;
begin
  Result := inherited Add as TcaXMLDataItem;
end;

function TcaXMLData.Clone(APopulate: Boolean = True): TcaXMLData;
var
  CloneClass: TcaXMLDataClass;
begin
  CloneClass := TcaXMLDataClass(ClassType);
  Result := CloneClass.Create(FItemClass);
  if APopulate then
    Result.XML := GetXML;
end;

function TcaXMLData.CustomLoadFromFile(const AFileName: string): Boolean;
begin
  Result := InternalLoadFromFile(AFileName, True);
end;

function TcaXMLData.CustomLoadFromStream(AStream: TStream): Boolean;
begin
  Result := InternalLoadFromStream(AStream, True);
end;

function TcaXMLData.GetDTDElements: string;
begin
  Result := XMLUtils.GetObjectDTDElements(Self);
end;

function TcaXMLData.GetDTDSequence: string;
begin
  Result := XMLUtils.GetObjectDTDSequence(Self);
  if Result = '' then
    Result := GetItemTagName + '*'
  else
    Result := GetItemTagName + '*, ' + Result;
end;

function TcaXMLData.GetItemTagName: string;
begin
  Result := TcaXMLDataItemClass(ItemClass).GetTagName;
end;

function TcaXMLData.GetPropertiesXML: string;
begin
  Result := XMLUtils.ObjectToXMLElements(Self)
end;

function TcaXMLData.GetSubsetElementText(Start, Ct: Integer): string;
var
  Index: Integer;
  List: TStringList;
begin
  if (Start < 0) or (Start >= Count) then
    raise EcaXMLException.Create('Invalid Start (' + IntToStr(Start) + ')');
  if (Ct < 0) or ((Start + Ct) >= Count) then
    raise EcaXMLException.Create('Invalid Ct (' + IntToStr(Ct) + ')');
  Result := '';
  List := TStringList.Create;
  try
    List.Text := XMLUtils.ObjectToXMLElements(Self);
    for Index := Start to Start + Ct - 1 do
      List.Add(Items[Index].XML);
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TcaXMLData.IndexOfID(const ID: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if Items[Index].ID = ID then
      begin
        Result := Index;
        Break;
      end;
end;

function TcaXMLData.IndexOfItem(AItem: TObject): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    begin
      if Items[Index] = AItem then
        begin
          Result := Index;
          Break;
        end;
    end;
end;

function TcaXMLData.Load: Boolean;
begin
  Result := False;
  case FLoadMode of
    lmLocal: Result := LoadFromFile(GetLocalFilePath);
    lmShared: Result := LoadFromFile(GetSharedFilePath);
    lmBoth:
      begin
        UpdateLocalFromShared;
        Result := LoadFromFile(GetLocalFilePath);
      end;
  end;
end;

function TcaXMLData.LoadFromFile(const AFileName: string): Boolean;
begin
  Result := InternalLoadFromFile(AFileName, False);
end;

function TcaXMLData.LoadFromStream(AStream: TStream): Boolean;
begin
  Result := InternalLoadFromStream(AStream, False);
end;

procedure TcaXMLData.Discard;
begin
  Clear;
  Load;
end;

procedure TcaXMLData.Save;
begin
  case FLoadMode of
    lmLocal: SaveToFile(GetLocalFilePath);
    lmShared: SaveToFile(GetSharedFilePath);
    lmBoth:
      begin
        SaveToFile(GetLocalFilePath);
        UpdateSharedFromLocal;
      end;
  end;
end;

procedure TcaXMLData.SaveToFile(const AFileName: string);
var
  List: TStringList;
  Path: string;
begin
  Path := ExtractFilePath(AFileName);
  if DirectoryExists(Path) then
    begin
      List := TStringList.Create;
      try
        List.Text := GetXML;
        List.SaveToFile(AFileName);
      finally
        List.Free;
      end;
    end;
end;

procedure TcaXMLData.SaveToStream(AStream: TStream);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := GetXML;
    List.SaveToStream(AStream);
  finally
    List.Free;
  end;
end;

procedure TcaXMLData.Sort(Compare: TListSortCompare);
var
  Anchor: Integer;
  Index: Integer;
  Item: TcaXMLDataItem;
begin
  for Index := 1 to Pred(Count) do
    begin
      Anchor := Index;
      while Compare(Items[Anchor - 1], Items[Anchor]) > 0 do
        begin
          Item := GetXMLCollectionItem(Anchor);
          Item.Index := Anchor - 1;
          Dec(Anchor);
          if Anchor < 1 then Break;
        end;
    end;
end;

  // Virtual methods 

procedure TcaXMLData.DoGetName(var AName: string);
begin
  // Virtual 
end;

procedure TcaXMLData.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if Action = cnAdded then
    TcaXMLDataItem(Item).ID := Utils.GetGUIDAsString;
end;

  // Private methods 

function TcaXMLData.GetFileName: string;
begin
  if FFileName <> '' then
    Result := FFileName
  else
    begin
      Result := ClassName;
      Utils.DeleteFromStart(Result, 1);
      while (Result <> '') and (Result[1] in ['a'..'z']) do
        Utils.DeleteFromStart(Result, 1);
      Result := Result + '.xml';
    end;
end;

function TcaXMLData.GetLocalFilePath: string;
var
  APath: string;
begin
  APath := FLocalPath;
  Utils.EnsureLastChar(APath, '\');
  Result := APath + GetFileName;
end;

function TcaXMLData.GetSharedFilePath: string;
var
  APath: string;
begin
  APath := FSharedPath;
  Utils.EnsureLastChar(APath, '\');
  Result := APath + GetFileName;
end;

function TcaXMLData.InternalLoadFromFile(const AFileName: string; IsCustom: Boolean): Boolean;
var
  List: TStringList;
  Path: string;
begin
  Result := False;
  Path := ExtractFilePath(AFileName);
  if DirectoryExists(Path) then
    begin
      Clear;
      if FileExists(AFileName) then
        begin
          List := TStringList.Create;
          try
            List.LoadFromFile(AFileName);
            if IsCustom then
              CustomParseXML(List.Text)
            else
              SetXML(List.Text);
            Result := True;
          finally
            List.Free;
          end;
        end;
      FLoaded := Result;
    end;
end;

function TcaXMLData.InternalLoadFromStream(AStream: TStream; IsCustom: Boolean): Boolean;
var
  List: TStringList;
begin
  Clear;
  List := TStringList.Create;
  try
    List.LoadFromStream(AStream);
    if IsCustom then
      CustomParseXML(List.Text)
    else
      SetXML(List.Text);
    Result := True;
  finally
    List.Free;
  end;
  FLoaded := Result;
end;

function TcaXMLData.UTF8Header: string;
begin
  Result := '<?xml version="1.0" encoding="utf-8" ?>';
end;

procedure TcaXMLData.CustomParseXML(const AXML: string);
var
  XMLReader: IcaXMLReader;
begin
  XMLReader := TcaXMLReader.Create;
  XMLReader.OnTag := CustomReadTagEvent;
  XMLReader.OnData := CustomReadDataEvent;
  XMLReader.OnEndTag := CustomReadEndTagEvent;
  XMLReader.AsText := AXML;
  XMLReader.Parse;
end;

procedure TcaXMLData.UpdateLocalFromShared;
begin
  if DirectoryExists(FSharedPath) then
    begin
      LoadFromFile(GetSharedFilePath);
      SaveToFile(GetLocalFilePath);
    end;
end;

procedure TcaXMLData.UpdateSharedFromLocal;
begin
  if DirectoryExists(FSharedPath) then
    begin
      LoadFromFile(GetLocalFilePath);
      SaveToFile(GetSharedFilePath);
    end;
end;

// Event handlers 

procedure TcaXMLData.CustomReadDataEvent(Sender: TObject; const ATag, AData, AAttributes: string; ALevel: Integer);
begin
  DoCustomReadData(ATag, AData, AAttributes, ALevel);
end;

procedure TcaXMLData.CustomReadEndTagEvent(Sender: TObject; const ATag: string; ALevel: Integer);
begin
  DoCustomReadEndTag(ATag, ALevel);
end;

procedure TcaXMLData.CustomReadTagEvent(Sender: TObject; const ATag, AAttributes: string; ALevel: Integer);
begin
  DoCustomReadTag(ATag, AAttributes, ALevel);
end;

  // Property methods 

function TcaXMLData.GetElementText: string;
var
  ItemName: string;
  Index: Integer;
  Item: TcaXMLDataItem;
  List: TStringList;
begin
  Result := '';
  List := TStringList.Create;
  try
    List.Text := GetPropertiesXML;
    ItemName := TcaXMLDataItemClass(ItemClass).GetTagName;
    for Index := 0 to Pred(Count) do
      begin
        Item := GetXMLCollectionItem(Index);
        List.Add('<' + ItemName + '>' + Item.ElementText + '</' + ItemName + '>');
      end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TcaXMLData.GetXML: string;
begin
  Result := '<' + GetTagName + '>' + GetElementText + '</' + GetTagName + '>';
  XMLUtils.FormatXML(Result);
  Result := UTF8Header + #13#10 + Result;
end;

function TcaXMLData.GetXMLCollectionItem(Index: Integer): TcaXMLDataItem;
begin
  Result := inherited Items[Index] as TcaXMLDataItem;
end;

procedure TcaXMLData.DoCustomReadData(const ATag, AData, AAttributes: string; ALevel: Integer);
begin
  // Virtual 
end;

procedure TcaXMLData.DoCustomReadEndTag(const ATag: string; ALevel: Integer);
begin
  // Virtual 
end;

procedure TcaXMLData.DoCustomReadTag(const ATag, AAttributes: string; ALevel: Integer);
begin
  // Virtual 
end;

procedure TcaXMLData.SetElementText(const AXML: string);
var
  CurrentTagIndex: Integer;
  OverallIndex: Integer;
  ItemName: string;
  CurrentTag: string;
  CurrentTagContent: string;
begin
  ItemName := TcaXMLDataItemClass(ItemClass).GetTagName;
  CurrentTagIndex := 1;
  OverallIndex := 1;
  repeat
    CurrentTag := XMLUtils.ParseTag(AXML, '<', '>', OverallIndex);
    CurrentTagContent := XMLUtils.ParseTag(AXML, '<' + CurrentTag + '>', '</' + CurrentTag + '>', CurrentTagIndex);
    if (Length(CurrentTag) > 0) then
      begin
        if (CurrentTag = ItemName) then
          Add.ElementText := CurrentTagContent
        else
          XMLUtils.SetPropAsString(Self, GetPropInfo(ClassInfo, CurrentTag), CurrentTagContent);
      end;
    OverallIndex := CurrentTagIndex;
  until (OverallIndex < 1) or (OverallIndex > Length(AXML));
end;

procedure TcaXMLData.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TcaXMLData.SetXML(const AXML: string);
var
  Xml: string;
begin
  Xml := StringReplace(AXML, UTF8Header + #13#10, '', [rfIgnoreCase]);
  SetElementText(XMLUtils.GetTagContent(XML, GetTagName));
end;

procedure TcaXMLData.SetXMLCollectionItem(Index: Integer; Value: TcaXMLDataItem);
begin
  inherited SetItem(Index, Value);
end;

  //---------------------------------------------------------------------------
  // TcaOwnedXMLCollection                                                     
  //---------------------------------------------------------------------------

constructor TcaOwnedXMLCollection.Create(AOwner: TPersistent; ItemClass: TcaXMLDataItemClass);
begin
  FOwner := AOwner;
  inherited Create(ItemClass);
end;

  // Protected methods 

function TcaOwnedXMLCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

  //---------------------------------------------------------------------------
  // TcaXMLDataUtils                                                           
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaXMLDataUtils.Create;
begin
  inherited;
  FXMLDataObjects := TList.Create;
end;

destructor TcaXMLDataUtils.Destroy;
begin
  FXMLDataObjects.Free;
  inherited;
end;

  // Public methods 

function TcaXMLDataUtils.ObjectToXMLElements(const AObject: TObject): string;
var
  EndTag: string;
  Index: Integer;
  List: TStringList;
  Prop: string;
  PropList: IcaObjectList;
  StartTag: string;
begin
  Result := '';
  List := TStringList.Create;
  try
    PropList := GetPropertyList(AObject.ClassInfo);
    for Index := 0 to Pred((PropList as IcaList).Count) do
      begin
        Prop := Trim(GetPropAsString(AObject, PPropInfo(PropList.Items[Index])));
        StartTag := '<' + PPropInfo(PropList.Items[Index]).Name + '>';
        EndTag := '</' + PPropInfo(PropList.Items[Index]).Name + '>';
        List.Add(StartTag + Prop + EndTag);
      end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TcaXMLDataUtils.ObjectToXMLProperties(const AObject: TObject): string;
var
  Index: Integer;
  PropList: IcaObjectList;
begin
  Result := #32;
  PropList := GetPropertyList(AObject.ClassInfo);
  for Index := 0 to Pred((PropList as IcaList).Count) do
    Result := Result + PPropInfo(PropList[Index]).Name + '="' + GetPropAsString(AObject, PPropInfo(PropList[Index])) + '" ';
end;

function TcaXMLDataUtils.ParseTag(const Source, StartTag, EndTag: string; var Index: Integer): string;
var
  EndPChar: PChar;
  StartPChar: PChar;
  TempStartPChar: PChar;
  Weight: Integer;
begin
  StartPChar := AnsiStrPos(PChar(@Source[Index]), PChar(StartTag));
  if StartPChar <> nil then
    begin
      Weight := 1;
      Inc(StartPChar, Length(StartTag));
      TempStartPChar := StartPChar;
      while Weight > 0 do
        begin
          EndPChar := AnsiStrPos(TempStartPChar, PChar(EndTag));
          if EndPChar <> nil then
            begin
              TempStartPChar := AnsiStrPos(TempStartPChar, PChar(StartTag));
              if (Weight = 1) and ((TempStartPChar = nil) or (Integer(TempStartPChar) > Integer(EndPChar))) then
                begin
                  Result := Copy(Source, Integer(StartPChar) - Integer(PChar(Source)) + 1, Integer(EndPChar) - Integer(StartPChar));
                  Index := Integer(EndPChar) - Integer(PChar(Source)) + Length(EndTag) + 1;
                  Break;
                end;
              if (TempStartPChar = nil) or (Integer(TempStartPChar) > Integer(EndPChar)) then
                begin
                  Dec(Weight);
                  TempStartPChar := EndPChar + 1;
                end
              else
                begin
                  Inc(Weight);
                  TempStartPChar := TempStartPChar + 1;
                end;
            end
          else
            begin
              Result := '';
              Index := -1;
              Break;
            end
        end
    end
  else
    begin
      Result := '';
      Index := -1;
    end;
end;

function TcaXMLDataUtils.StrToXML(const AXML: string): string;
const
  ValidCharTable = (['a'..'z', 'A'..'Z', '0'..'9', '-', ' ', ',', '.', '\', '/', '_', '[', ']', '!', ';', ':', '=']);
var
  Ch: Char;
  Index: Integer;
begin
  Result := AXML;
  for Index := Length(Result) downto 1 do
    begin
      Ch := Result[Index];
      if not (Ch in ValidCharTable) then
        begin
          Result[Index] := '&';
          Insert('#x' + IntToHex(Ord(Ch), 2) + ';', Result, Index + 1);
        end;
    end;
end;

function TcaXMLDataUtils.XMLToStr(const AXML: string): string;
var
  HexNum: Integer;
  HexStr: string;
  Index: Integer;
begin
  Result := AXML;
  if AnsiSameText(Copy(Result, 0, Length(cCDATADescriptor)), cCDATADescriptor) then
    begin
      Delete(Result, 1, Length(cCDATADescriptor));
      Delete(Result, Length(Result) - 2, 3);
    end
  else
    begin
      for Index := Length(Result) - 5 downto 1 do
        begin
          if (Result[Index] = '&') and (Length(Result) >= Index + 5) then
            begin
              if (Result[Index + 1] = '#') and (Result[Index + 2] = 'x') then
                begin
                  HexStr := Result[Index + 3] + Result[Index + 4];
                  HexNum := StrToInt('0x' + HexStr);
                  Delete(Result, Index, 5);
                  Result[Index] := Chr(HexNum);
                end;
            end;
        end;
      Result := Trim(Result);
    end;
end;

procedure TcaXMLDataUtils.FormatXML(var AXML: string);
var
  Index: Integer;
  Line: string;
  Lines: IcaStringList;
  TagName: string;
  XMLOut: IcaXmlBuilder;
begin
  // Convert to one line per element, no indent 
  InsertLineBreaks(AXML);
  // Generate "pretty" XML 
  Lines := TcaStringList.Create(AXML);
  XMLOut := TcaXmlBuilder.Create;
  XMLOut.IndentSpaces := 4;
  for Index := 0 to Pred(Lines.Count) do
    begin
      Line := Trim(Lines[Index]);
      if Line <> '' then
        begin
          if Copy(Line, 1, 2) = '</' then
            XMLOut.EndTag
          else
            begin
              if Line[1] = '<' then
                begin
                  TagName := Line;
                  Utils.DeleteFromStart(TagName, 1);
                  Utils.DeleteFromEnd(TagName, 1);
                  XMLOut.AddTag(TagName);
                end
              else
                XMLOut.Add(Line);
            end;
        end;
    end;
  AXML := XMLOut.AsText;
end;

procedure TcaXMLDataUtils.InsertLineBreaks(var AXML: string);
begin
  Utils.Replace(AXML, #13#10, #32);
  Utils.Replace(AXML, '>', '>'#13#10);
  Utils.Replace(AXML, '<', #13#10'<');
  Utils.Replace(AXML, #13#10#32, #13#10);
  Utils.Replace(AXML, #13#10#13#10, #13#10);
end;

  // Private methods 

function TcaXMLDataUtils.GetObjectDTDElements(const AObject: TObject): string;
var
  DTD: string;
  Index: Integer;
  Elements: string;
  List: TStringList;
  PropList: IcaObjectList;
begin
  Result := '';
  List := TStringList.Create;
  try
    PropList := GetPropertyList(AObject.ClassInfo);
    for Index := 0 to Pred((PropList as IcaList).Count) do
      begin
        DTD := GetPropertyDTD(AObject, PPropInfo(PropList.Items[Index]));
        List.Add(cElementHeader + PPropInfo(PropList.Items[Index]).Name + '(' + DTD + ')>');
        Elements := GetPropertyDTDElements(AObject, PPropInfo(PropList.Items[Index]));
        if Elements <> '' then List.Add(Elements);
      end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TcaXMLDataUtils.GetObjectDTDSequence(const AObject: TObject): string;
var
  Index: Integer;
  PropCount: Integer;
  PropList: IcaObjectList;
begin
  Result := '';
  PropList := GetPropertyList(AObject.ClassInfo);
  PropCount := (PropList as IcaList).Count;
  if PropCount >= 1 then
    begin
      for Index := 0 to PropCount - 2 do
        Result := Result + PPropInfo(PropList[Index]).Name + ', ';
      Result := Result + PPropInfo(PropList[Pred(PropCount)]).Name;
    end;
end;

function TcaXMLDataUtils.GetPropAsString(const Instance: TObject; const PropInfo: PPropInfo): string;
var
  Intf: IcaXMLDataObject;
  ObjectProp: TObject;
begin
  case PropInfo^.PropType^.Kind of
    tkString,
      tkLString,
      tkWString:
      if AnsiSameText(PropInfo^.PropType^.Name, 'XMLString') then
        Result := GetStrProp(Instance, PropInfo)
      else
        Result := StrToXML(GetStrProp(Instance, PropInfo));
    tkInt64: Result := IntToStr(GetInt64Prop(Instance, PropInfo));
    tkInteger: Result := IntToStr(GetOrdProp(Instance, PropInfo));
    tkFloat:
      if AnsiSameText(PropInfo^.PropType^.Name, 'TDateTime') then
        Result := DateTimeToStr(GetFloatProp(Instance, PropInfo))
      else if AnsiSameText(PropInfo^.PropType^.Name, 'TTime') then
        Result := TimeToStr(GetFloatProp(Instance, PropInfo))
      else if AnsiSameText(PropInfo^.PropType^.Name, 'TDate') then
        Result := DateToStr(GetFloatProp(Instance, PropInfo))
      else
        Result := FloatToStr(GetFloatProp(Instance, PropInfo));
    tkVariant: Result := StrToXML(GetVariantProp(Instance, PropInfo));
    tkChar,
      tkWChar: Result := StrToXML(Chr(GetOrdProp(Instance, PropInfo)));
    tkEnumeration: Result := StrToXML(GetEnumName(PropInfo^.PropType^, GetOrdProp(Instance, PropInfo)));
    tkClass:
      begin
        ObjectProp := TObject(GetOrdProp(Instance, PropInfo));
        if Assigned(ObjectProp) then
          begin
            if ObjectProp.GetInterface(IcaXMLDataObject, Intf) then
              Result := Intf.ElementText
            else if (ObjectProp is TcaXMLData) then
              Result := TcaXMLData(ObjectProp).ElementText
            else if (ObjectProp is TcaXMLDataItem) then
              Result := TcaXMLDataItem(ObjectProp).ElementText
            else if (ObjectProp is TcaXMLObject) then
              Result := TcaXMLObject(ObjectProp).ElementText
            else if (ObjectProp is TcaXMLList) then
              Result := TcaXMLList(ObjectProp).ElementText;
          end;
      end;
  else
    Result := '';
  end;
end;

function TcaXMLDataUtils.GetPropertyDTD(Instance: TObject; PropInfo: PPropInfo): string;
var
  ObjectProp: TObject;
  Intf: IcaXMLDataObject;
begin
  case PropInfo^.PropType^.Kind of
    tkString,
      tkLString,
      tkWString,
      tkInt64,
      tkInteger,
      tkFloat,
      tkVariant,
      tkChar,
      tkWChar,
      tkEnumeration: Result := '#PCDATA';
    tkClass:
      begin
        ObjectProp := TObject(GetOrdProp(Instance, PropInfo));
        if Assigned(ObjectProp) then
          begin
            if ObjectProp.GetInterface(IcaXMLDataObject, Intf) then
              Result := Intf.GetDTDSequence
            else if (ObjectProp is TcaXMLData) then
              Result := TcaXMLData(ObjectProp).GetDTDSequence
            else if (ObjectProp is TcaXMLDataItem) then
              Result := TcaXMLDataItem(ObjectProp).GetDTDSequence
            else if (ObjectProp is TcaXMLObject) then
              Result := TcaXMLObject(ObjectProp).GetDTDSequence
            else if (ObjectProp is TcaXMLList) then
              Result := TcaXMLList(ObjectProp).GetDTDSequence;
          end;
      end;
  else
    Result := '';
  end;
end;

function TcaXMLDataUtils.GetPropertyDTDElements(Instance: TObject; PropInfo: PPropInfo): string;
var
  ObjectProp: TObject;
  Intf: IcaXMLDataObject;
begin
  case PropInfo^.PropType^.Kind of
    tkClass:
      begin
        ObjectProp := TObject(GetOrdProp(Instance, PropInfo));
        if Assigned(ObjectProp) then
          begin
            if ObjectProp.GetInterface(IcaXMLDataObject, Intf) then
              Result := Intf.GetDTDElements
            else if (ObjectProp is TcaXMLData) then
              Result := TcaXMLData(ObjectProp).GetDTDElements
            else if (ObjectProp is TcaXMLDataItem) then
              Result := TcaXMLDataItem(ObjectProp).GetDTDElements
            else if (ObjectProp is TcaXMLObject) then
              Result := TcaXMLObject(ObjectProp).GetDTDElements
            else if (ObjectProp is TcaXMLList) then
              Result := TcaXMLList(ObjectProp).GetDTDElements;
          end;
      end;
  else
    Result := '';
  end;
end;

function TcaXMLDataUtils.GetPropertyList(const ClassInfo: Pointer): IcaObjectList;
var
  Index: integer;
  PropCount: integer;
  PropList: PPropList;
begin
  Result := TcaObjectList.Create;
  PropCount := GetTypeData(ClassInfo)^.PropCount;
  if PropCount > 0 then
    begin
      GetMem(PropList, PropCount * SizeOf(Pointer));
      try
        PropCount := GetPropList(ClassInfo, tkProperties, PropList);
        { TODO : Alphabetical }
        {
        for i := 0 to (Pred(iPropCount)) do
          Result.Add(PropList^[i]);
        }
        { TODO : Declaration Order }
        (Result as IcaList).Count := PropCount;
        for Index := 0 to Pred(PropCount) do
          Result[PropList^[Index].NameIndex] := TObject(PropList^[Index]);
      finally
        FreeMem(PropList);
      end;
    end;
end;

function TcaXMLDataUtils.GetTagContent(const AXML, TargetTagName: string): string;
var
  CurrentTag: string;
  CurrentTagContent: string;
  CurrentTagIndex: Integer;
  OverallIndex: Integer;
begin
  Result := '';
  CurrentTagIndex := 1;
  OverallIndex := 1;
  repeat
    CurrentTag := ParseTag(AXML, '<', '>', OverallIndex);
    if not AnsiSameText(cXMLPrologTag, CurrentTag) then
      begin
        CurrentTagContent := ParseTag(AXML, '<' + CurrentTag + '>', '</' + CurrentTag + '>', CurrentTagIndex);
        OverallIndex := CurrentTagIndex;
      end;
  until (OverallIndex < 1) or (OverallIndex > Length(AXML)) or (CurrentTag = TargetTagName);
  if OverallIndex >= 1 then
    Result := CurrentTagContent;
end;

procedure TcaXMLDataUtils.SetPropAsString(Instance: TObject; PropInfo: PPropInfo; value: string);
var
  Intf: IcaXMLDataObject;
  ObjectProp: TObject;
begin
  if (PropInfo <> nil) and (PropInfo^.SetProc <> nil) and (value <> '') then
    begin
      case PropInfo^.PropType^.Kind of
        tkString,
          tkLString,
          tkWString: SetStrProp(Instance, PropInfo, XMLToStr(Value));
        tkInteger: SetOrdProp(Instance, PropInfo, StrToInt(XMLToStr(Value)));
        tkFloat:
          if AnsiSameText(PropInfo^.PropType^.Name, 'TDateTime') then
            SetFloatProp(Instance, PropInfo, StrToDateTime(XMLToStr(Value)))
          else if AnsiSameText(PropInfo^.PropType^.Name, 'TTime') then
            SetFloatProp(Instance, PropInfo, StrToTime(XMLToStr(Value)))
          else if AnsiSameText(PropInfo^.PropType^.Name, 'TDate') then
            SetFloatProp(Instance, PropInfo, StrToDate(XMLToStr(Value)))
          else
            SetFloatProp(Instance, PropInfo, StrToFloat(XMLToStr(Value)));
        tkVariant: SetVariantProp(Instance, PropInfo, XMLToStr(Value));
        tkInt64: SetInt64Prop(Instance, PropInfo, StrToInt64(XMLToStr(Value)));
        tkChar,
          tkWChar: SetOrdProp(Instance, PropInfo, Ord(XMLToStr(Value)[1]));
        tkEnumeration: SetOrdProp(Instance, PropInfo, GetEnumValue(PropInfo^.PropType^, XMLToStr(Value)));
        tkClass:
          begin
            ObjectProp := TObject(GetOrdProp(Instance, PropInfo));
            if Assigned(ObjectProp) then
              begin
                if ObjectProp.GetInterface(IcaXMLDataObject, Intf) then
                  Intf.ElementText := Value
                else if (ObjectProp is TcaXMLData) then
                  TcaXMLData(ObjectProp).ElementText := Value
                else if (ObjectProp is TcaXMLDataItem) then
                  TcaXMLDataItem(ObjectProp).ElementText := Value
                else if (ObjectProp is TcaXMLObject) then
                  TcaXMLObject(ObjectProp).ElementText := Value
                else if (ObjectProp is TcaXMLList) then
                  TcaXMLList(ObjectProp).ElementText := Value
              end;
          end;
        //  Types not supported : 
        //    tkRecord 
        //    tkArray 
        //    tkInterface 
        //    tkDynArray 
        //    tkSet 
        //    tkClass 
        //    tkMethod 
        //    tkUnknown 
      end;
    end;
end;

procedure TcaXMLDataUtils.XMLToObject(Instance: TObject; AXML: AnsiString);
var
  CurrentTag: string;
  CurrentTagContent: string;
  CurrentTagIndex: Integer;
  OverallIndex: Integer;
begin
  CurrentTagIndex := 1;
  OverallIndex := 1;
  repeat
    CurrentTag := ParseTag(AXML, '<', '>', OverallIndex);
    CurrentTagContent := ParseTag(AXML, '<' + CurrentTag + '>', '</' + CurrentTag + '>', CurrentTagIndex);
    if (Length(CurrentTag) > 0) then
      SetPropAsString(Instance, GetPropInfo(Instance.ClassInfo, CurrentTag), CurrentTagContent);
    OverallIndex := CurrentTagIndex;
  until (OverallIndex < 1) or (OverallIndex > Length(AXML));
end;

  //---------------------------------------------------------------------------
  // TcaXMLDataset                                                             
  //---------------------------------------------------------------------------

constructor TcaXMLDataset.Create(AOwner: TComponent);
begin
  inherited;
  FNonUserChangedCols := TBits.Create;
end;

destructor TcaXMLDataset.Destroy;
begin
  if Active then Close;
  FNonUserChangedCols.Free;
  FRTTIDummyItem.Free;
  FRTTIList.Free;
  inherited;
end;

  // Overridden abstract methods from TcaBaseDataset 

  // Basic DB methods 

function TcaXMLDataset.CanOpen: Boolean;
begin
  FItemIndex := -1;
  Result := False;
  if FXMLData <> nil then
    Result := True;
end;

function TcaXMLDataset.GetFieldValue(Field: TField): Variant;
var
  Item: TcaXMLDataItem;
  UnchangedValue: Variant;
begin
  Result := Null;
  Item := GetActiveItem;
  if Item <> nil then
    begin
      case PropType(Item, Field.FieldName) of
        tkString, tkLString:
          Result := GetStrProp(Item, Field.FieldName);
        tkInteger, tkSet, tkEnumeration:
          Result := GetOrdProp(Item, Field.FieldName);
        tkFloat:
          Result := GetFloatProp(Item, Field.FieldName);
      end;
      UnchangedValue := Result;
      DoGetItemValue(Field.FieldName, Item, Result);
      if UnchangedValue <> Result then
        FNonUserChangedCols.Bits[Field.Index] := True;
    end;
end;

procedure TcaXMLDataset.DoBeforeSetFieldValue(Inserting: Boolean);
begin
  if FXMLData <> nil then
    if Inserting then
      FItemIndex := FXMLData.Add.Index;
end;

procedure TcaXMLDataset.DoClose;
begin
  inherited;
end;

procedure TcaXMLDataset.DoCreateFieldDefs;
var
  FieldDef: TFieldDef;
  FieldDefCount: Integer;
  Index: Integer;
  RTTIItem: TcaRTTIItem;
begin
  FieldDefs.Clear;
  FieldDefCount := FRTTIList.Count;
  for Index := 0 to Pred(FieldDefCount) do
    begin
      RTTIItem := FRTTIList[Index];
      FieldDef := FieldDefs.AddFieldDef;
      FieldDef.Name := RTTIItem.PropName;
      FieldDef.DataType := TypeKindToDataType(RTTIItem.PropType);
      FieldDef.Size := RTTIItem.PropSize;
      FieldDef.Required := False;
    end;
  FNonUserChangedCols.Size := FieldDefCount;
  for Index := 0 to Pred(FieldDefCount) do
    FNonUserChangedCols.Bits[Index] := False;
end;

procedure TcaXMLDataset.DoDeleteRecord;
var
  Item: TcaXMLDataItem;
begin
  Item := GetActiveItem;
  if Item <> nil then Item.Free;
end;

procedure TcaXMLDataset.GetBlobField(Field: TField; Stream: TStream);
begin
  Pass;
end;

procedure TcaXMLDataset.SetBlobField(Field: TField; Stream: TStream);
begin
  Pass;
end;

procedure TcaXMLDataset.SetFieldValue(Field: TField; Value: Variant);
var
  Item: TcaXMLDataItem;
begin
  Item := GetActiveItem;
  if Item <> nil then
    begin
      DoSetItemValue(Field.FieldName, Item, Value);
      if not FNonUserChangedCols.Bits[Field.Index] then
        SetPropValue(Item, Field.FieldName, Value);
    end;
end;

  // Buffer ID methods 

function TcaXMLDataset.AllocateRecordID: Pointer;
begin
  Result := Pointer(FItemIndex);
end;

procedure TcaXMLDataset.DisposeRecordID(Value: Pointer);
begin
  Pass;
end;

procedure TcaXMLDataset.GotoRecordID(Value: Pointer);
begin
  FItemIndex := Integer(Value);
end;

  // BookMark functions 

function TcaXMLDataset.GetBookMarkSize: Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TcaXMLDataset.AllocateBookMark(RecordID: Pointer; Bookmark: Pointer);
begin
  PInteger(Bookmark)^ := Integer(RecordID);
end;

procedure TcaXMLDataset.DoGotoBookmark(Bookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(Bookmark)^));
end;

  // Navigation methods 

function TcaXMLDataset.Navigate(GetMode: TGetMode): TGetResult;
begin
  if RecordCount < 1 then
    Result := grEOF
  else
    begin
      Result := grOK;
      case GetMode of
        gmNext:
          begin
            if FItemIndex >= RecordCount - 1 then
              Result := grEOF
            else
              Inc(FItemIndex);
          end;
        gmPrior:
          begin
            if FItemIndex <= 0 then
              begin
                Result := grBOF;
                FItemIndex := -1;
              end
            else
              Dec(FItemIndex);
          end;
        gmCurrent:
          if (FItemIndex < 0) or (FItemIndex >= RecordCount) then
            Result := grError;
      end;
    end;
end;

procedure TcaXMLDataset.DoFirst;
begin
  FItemIndex := -1;
end;

procedure TcaXMLDataset.DoLast;
begin
  FItemIndex := RecordCount;
end;

  // Overridden methods from TDataset 

function TcaXMLDataset.GetRecordCount: Integer;
begin
  Result := 0;
  if FXMLData <> nil then
    Result := FXMLData.Count;
end;

function TcaXMLDataset.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FItemIndex = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FItemIndex + 1;
end;

procedure TcaXMLDataset.CreateFields;
var
  DisplayLabel: string;
  Field: TField;
  Index: Integer;
begin
  inherited;
  for Index := 0 to Pred(FieldCount) do
    begin
      Field := Fields[Index];
      if Field.FieldName = 'ID' then
        Field.Visible := False
      else
        begin
          DisplayLabel := Field.FieldName;
          Utils.SplitCamelCaps(DisplayLabel);
          Field.DisplayLabel := DisplayLabel;
        end;
    end;
end;

procedure TcaXMLDataset.DoAfterPost;
begin
  inherited;
end;

procedure TcaXMLDataset.DoBeforePost;
begin
  inherited;
end;

procedure TcaXMLDataset.SetActive(Value: Boolean);
begin
  if Assigned(FXMLData) then
    inherited SetActive(Value)
  else
    inherited SetActive(False);
end;

procedure TcaXMLDataset.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value < RecordCount) then
    begin
      FItemIndex := Value - 1;
      Resync([]);
    end;
end;

  // Other protected methods 

procedure TcaXMLDataset.DoGetItemValue(const AColumnName: string; AItem: TcaXMLDataItem; var AValue: Variant);
begin
  if Assigned(FOnGetItemValue) then
    FOnGetItemValue(Self, AColumnName, AItem, AValue);
end;

procedure TcaXMLDataset.DoSetItemValue(const AColumnName: string; AItem: TcaXMLDataItem; var AValue: Variant);
begin
  if Assigned(FOnSetItemValue) then
    FOnSetItemValue(Self, AColumnName, AItem, AValue);
end;

  // Private methods 

function TcaXMLDataset.TypeKindToDataType(ATypeKind: TTypeKind): TFieldType;
begin
  case ATypeKind of
    tkInteger: Result := ftInteger;
    tkChar: Result := ftString;
    tkEnumeration: Result := ftSmallint;
    tkFloat: Result := ftFloat;
    tkString: Result := ftString;
    tkLString: Result := ftBlob;
    tkInt64: Result := ftInteger;
  else
    Result := ftUnknown;
  end;
end;

procedure TcaXMLDataset.UpdateRTTI;
begin
  FreeAndNil(FRTTIDummyItem);
  FRTTIDummyItem := TcaXMLDataItem(FXMLData.ItemClass.Create(nil));
  FreeAndNil(FRTTIList);
  FRTTIList := TcaRTTIList.Create(FRTTIDummyItem);
end;

  // Property methods 

function TcaXMLDataset.GetActiveItem: TcaXMLDataItem;
begin
  Result := nil;
  if FXMLData <> nil then
    begin
      if (FItemIndex = -1) and (FXMLData.Count > 0) then
        Result := FXMLData.Items[0];
      if (FItemIndex >= 0) and (FItemIndex < FXMLData.Count) then
        Result := FXMLData[FItemIndex];
    end;
end;

function TcaXMLDataset.GetRTTIList: TcaRTTIList;
begin
  Result := FRTTIList;
end;

function TcaXMLDataset.GetXMLData: TcaXMLData;
begin
  Result := FXMLData;
end;

procedure TcaXMLDataset.SetXMLData(const Value: TcaXMLData);
begin
  if Value <> FXMLData then
    begin
      FXMLData := Value;
      if Assigned(FXMLData) then
        UpdateRTTI;
    end;
end;

  // Event property methods 

function TcaXMLDataset.GetOnGetItemValue: TcaXMLDataItemValueEvent;
begin
  Result := FOnGetItemValue;
end;

function TcaXMLDataset.GetOnSetItemValue: TcaXMLDataItemValueEvent;
begin
  Result := FOnSetItemValue;
end;

procedure TcaXMLDataset.SetOnGetItemValue(const Value: TcaXMLDataItemValueEvent);
begin
  FOnGetItemValue := Value;
end;

procedure TcaXMLDataset.SetOnSetItemValue(const Value: TcaXMLDataItemValueEvent);
begin
  FOnSetItemValue := Value;
end;

  //---------------------------------------------------------------------------
  // Initialization / finalization                                             
  //---------------------------------------------------------------------------

initialization
  XMLUtils := TcaXMLDataUtils.Create;

finalization
  XMLUtils.Free;

end.

