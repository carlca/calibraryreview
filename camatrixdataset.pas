unit caMatrixDataset;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,
  DB,

  {$IFDEF D7_UP}
  Variants,
  {$ENDIF}

  // ca units
  caLog,
  caBaseDataset,
  caMatrix,
  caUtils,
  caClasses;

type

  //----------------------------------------------------------------------------
  // TcaMatrixDataset                                                           
  //----------------------------------------------------------------------------

  TcaMatrixDataset = class(TcaBaseDataset)
  private
    // Private fields
    FMatrix: TcaMatrix;
    FItemIndex: Integer;
    // Property fields
    FFileName: String;
    // Property methods
    function GetFileName: String;
    procedure SetFileName(const Value: String);
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
    procedure SetRecNo(Value: Integer); override;
    //---------------------------------------------------------------------------
    // Other protected methods                                                   
    //---------------------------------------------------------------------------
    procedure DoGetItemValue(const AColumnName: String; ARow: Integer; var AValue: Variant); virtual;
    procedure DoLoadXMLData; virtual;
    procedure DoSetItemValue(const AColumnName: String; ARow: Integer; var AValue: Variant); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Properties 
    property FileName: String read GetFileName write SetFileName;
    // Promoted properties 
    property Active;
    property ReadOnly;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaMatrixDataset                                                           
  //----------------------------------------------------------------------------

constructor TcaMatrixDataset.Create(AOwner: TComponent);
begin
  inherited;
  FMatrix := TcaMatrix.Create;
end;

destructor TcaMatrixDataset.Destroy;
begin
  if Active then Close;
  FMatrix.Free;
  inherited;
end;

  // Overridden abstract methods from TcaBaseDataset 

  // Basic DB methods 

function TcaMatrixDataset.CanOpen: Boolean;
begin
  FItemIndex := -1;
  FMatrix.LoadFromXML(FFileName);
  Result := True;
end;

function TcaMatrixDataset.GetFieldValue(Field: TField): Variant;
var
  ARow: Integer;
  ACol: Integer;
begin
  ARow := FItemIndex;
  ACol := Field.Index;
  Result := FMatrix.Strings[ACol, ARow];
end;

procedure TcaMatrixDataset.DoBeforeSetFieldValue(Inserting: Boolean);
begin
  //
end;

procedure TcaMatrixDataset.DoClose;
begin
  FreeAndNil(FMatrix);
end;

procedure TcaMatrixDataset.DoCreateFieldDefs;
var
  FieldDef: TFieldDef;
  Index: Integer;
begin
  for Index := 0 to FMatrix.ColCount - 1 do
    begin
      FieldDef := FieldDefs.AddFieldDef;
      FieldDef.Name := FMatrix.ColumnNames[Index];
      // FieldDef.DataType :=
      FieldDef.Size := FieldDef.Size;
      FieldDef.Required := FieldDef.Required;
    end;
end;

procedure TcaMatrixDataset.DoDeleteRecord;
begin
  Pass;
end;

procedure TcaMatrixDataset.GetBlobField(Field: TField; Stream: TStream);
begin
  Pass;
end;

procedure TcaMatrixDataset.SetBlobField(Field: TField; Stream: TStream);
begin
  Pass;
end;

procedure TcaMatrixDataset.SetFieldValue(Field: TField; Value: Variant);
begin
  // TODO: SetFieldValue
end;

 // Buffer ID methods

function TcaMatrixDataset.AllocateRecordID: Pointer;
begin
  Result := Pointer(FItemIndex);
end;

procedure TcaMatrixDataset.DisposeRecordID(Value: Pointer);
begin
  Pass;
end;

procedure TcaMatrixDataset.GotoRecordID(Value: Pointer);
begin
  FItemIndex := Integer(Value);
end;

  // BookMark functions

function TcaMatrixDataset.GetBookMarkSize: Integer;
begin
  Result := 0;
end;

procedure TcaMatrixDataset.AllocateBookMark(RecordID: Pointer; Bookmark: Pointer);
begin
  PInteger(Bookmark)^ := Integer(RecordID);
end;

procedure TcaMatrixDataset.DoGotoBookmark(Bookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(Bookmark)^));
end;

 // Navigation methods

function TcaMatrixDataset.Navigate(GetMode: TGetMode): TGetResult;
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

procedure TcaMatrixDataset.DoFirst;
begin
  FItemIndex := -1;
end;

procedure TcaMatrixDataset.DoLast;
begin
  FItemIndex := RecordCount;
end;

 // Overridden methods from TDataset

function TcaMatrixDataset.GetRecordCount: Integer;
begin
  Result := 0;
  // Result := FBizObject.List.Count;
end;

function TcaMatrixDataset.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FItemIndex = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FItemIndex + 1;
end;

procedure TcaMatrixDataset.CreateFields;
//  var
//    Column: TnnBizColumn;
//    Field: TField;
//    Schema: TnnBizSchema;
//    Index: Integer;
begin
  inherited;
//  Schema := FBizObject.Schema;
//  for Index := 0 to FieldCount - 1 do
//    begin
//      Field := Fields[Index];
//      Column := Schema.ColByName(Field.FieldName);
//      Field.DisplayLabel := Column.ColumnCaption;
//    end;
end;

procedure TcaMatrixDataset.DoAfterPost;
begin
  inherited;
end;

procedure TcaMatrixDataset.DoBeforePost;
begin
  inherited;
end;

procedure TcaMatrixDataset.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value < RecordCount) then
    begin
      FItemIndex := Value - 1;
      Resync([]);
    end;
end;

  // Other protected methods 

procedure TcaMatrixDataset.DoGetItemValue(const AColumnName: string; ARow: Integer; var AValue: Variant);
begin
  inherited;
end;

procedure TcaMatrixDataset.DoLoadXMLData;
begin
  inherited;
end;

procedure TcaMatrixDataset.DoSetItemValue(const AColumnName: string; ARow: Integer; var AValue: Variant);
begin
  inherited;
end;

 // Property methods

function TcaMatrixDataset.GetFileName: String;
begin
  Result := FFileName;
end;

procedure TcaMatrixDataset.SetFileName(const Value: String);
begin
  FFileName := Value;
end;

end.
