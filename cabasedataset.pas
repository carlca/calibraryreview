unit caBaseDataset;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  SysUtils,
  Windows,
  Forms,
  DB,

  caLog;

type

  //----------------------------------------------------------------------------
  // TcaRecordInfo                                                              
  //----------------------------------------------------------------------------

  PcaRecordInfo = ^TcaRecordInfo;
  TcaRecordInfo = record
    RecordID: Pointer;
    Bookmark: Pointer;
    BookMarkFlag: TBookmarkFlag;
  end;

  //----------------------------------------------------------------------------
  // TcaBaseDataset                                                             
  //----------------------------------------------------------------------------

  TcaBaseDataset = class(TDataset)
  private
    FFieldsCreated: Boolean;
    FIsOpen: Boolean;
    FStartCalculated: Integer;
    FBufferMap: TStringList;
    // Property fields
    FReadOnly: Boolean;
    // Private methods
    function GetCalculatedFieldData(Field: TField; Buffer: Pointer; RecBuffer: PChar): Boolean;
    procedure GetPhysicalFieldData(Field: TField; Buffer: Pointer; RecBuffer: PChar);
    procedure FillBufferMap;
    // Property methods
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    //---------------------------------------------------------------------------
    // These virtual abstract methods must all be overridden by descendents      
    //---------------------------------------------------------------------------
    // Basic DB methods 
    function CanOpen: Boolean; virtual; abstract;
    function GetFieldValue(Field: TField): Variant; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoCreateFieldDefs; virtual; abstract;
    procedure DoDeleteRecord; virtual; abstract;
    procedure GetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    procedure SetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    procedure SetFieldValue(Field: TField; Value: Variant); virtual; abstract;
    // Buffer ID methods 
    function AllocateRecordID: Pointer; virtual; abstract;
    procedure DisposeRecordID(Value: Pointer); virtual; abstract;
    procedure GotoRecordID(Value: Pointer); virtual; abstract;
    // BookMark functions 
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); virtual; abstract;
    procedure DoGotoBookmark(Bookmark: Pointer); virtual; abstract;
    // Navigation methods 
    function Navigate(GetMode: TGetMode): TGetResult; virtual; abstract;
    procedure DoFirst; virtual; abstract;
    procedure DoLast; virtual; abstract;
    //---------------------------------------------------------------------------
    // These virtual methods can be optionally overridden                        
    //---------------------------------------------------------------------------
    // Resource allocation 
    procedure AllocateBlobPointers(Buffer: PChar); virtual;
    procedure FreeBlobPointers(Buffer: PChar); virtual;
    procedure FreeRecordPointers(Buffer: PChar); virtual;
    // Record and field info 
    function GetDataSize: Integer; virtual;
    function GetFieldOffset(Field: TField): Integer; virtual;
    // Buffer / record conversion 
    procedure BufferToRecord(Buffer: PChar); virtual;
    procedure RecordToBuffer(Buffer: PChar); virtual;
    // Called before and after getting a set of field values 
    procedure DoBeforeGetFieldValue; virtual;
    procedure DoAfterGetFieldValue; virtual;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); virtual;
    procedure DoAfterSetFieldValue(Inserting: Boolean); virtual;
    // BookMark functions 
    function GetBookMarkSize: Integer; virtual;
    // Internal properties 
    property IsOpen: Boolean read FIsOpen;
    //---------------------------------------------------------------------------
    // These are the virtual methods from TDataset that have all been overridden 
    //---------------------------------------------------------------------------
    function AllocRecordBuffer: PChar; override;
    function GetActiveRecordBuffer: PChar; virtual;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    function IsCursorOpen: Boolean; override;
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInsert; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public virtual methods overridden from TDataset 
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
  end;

  //----------------------------------------------------------------------------
  // TcaBlobStream                                                              
  //----------------------------------------------------------------------------

  TcaBlobStream = class(TMemoryStream)
  private
    // Private fields 
    FField: TBlobField;
    FDataSet: TcaBaseDataSet;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    // Private methods 
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    // Publc methods 
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaBaseDataset                                                             
  //----------------------------------------------------------------------------

constructor TcaBaseDataset.Create(AOwner: TComponent); 
begin
  inherited;
  FBufferMap := TStringList.Create;
end;

destructor TcaBaseDataset.Destroy; 
begin
  if Active then Close;
  FBufferMap.Free;
  inherited;
end;

  // Public virtual methods overridden from TDataset 

function TcaBaseDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuffer: PChar;
begin
  Result := False;
  if FIsOpen then
    begin
      RecBuffer := GetActiveRecordBuffer;
      if RecBuffer <> nil then 
        begin
          Result := True;
          if Buffer <> nil then
            begin
              if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
                Result := GetCalculatedFieldData(Field, Buffer, RecBuffer)
              else
                begin
                  GetPhysicalFieldData(Field, Buffer, RecBuffer);
                  Result := True;
                end;
            end;
        end;
    end;
end;

function TcaBaseDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TcaBlobStream.Create(Field as TBlobField, Mode);
end;

  //----------------------------------------------------------------------------
  // TcaBaseDataset                                                             
  //----------------------------------------------------------------------------

  // Resource allocation 
 
procedure TcaBaseDataset.AllocateBlobPointers(Buffer: PChar); 
var
  Index: Integer;
  Offset: Integer;
  Stream: TMemoryStream;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].DataType in [ftBlob, ftMemo, ftGraphic] then
      begin
        Offset := GetFieldOffset(Fields[Index]);
        Stream := TMemoryStream.Create;
        Move(Pointer(Stream), (Buffer + Offset)^, SizeOf(Pointer));
      end;
end;

procedure TcaBaseDataset.FreeBlobPointers(Buffer: PChar);
var
  Index: Integer;
  Offset: Integer;
  FreeObject: TObject;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].DataType in [ftBlob, ftMemo, ftGraphic] then
      begin
        Offset := GetFieldOffset(Fields[Index]);
        Move((Buffer + Offset)^, Pointer(FreeObject), SizeOf(Pointer));
        if FreeObject <> nil then FreeObject.Free;
        FreeObject := nil;
        Move(Pointer(FreeObject), (Buffer + Offset)^, SizeOf(Pointer));
      end;
end;

procedure TcaBaseDataset.FreeRecordPointers(Buffer: PChar);
begin
  FreeBlobPointers(Buffer);
  DisposeRecordID(PcaRecordInfo(Buffer + GetDataSize).RecordID);
  if PcaRecordInfo(Buffer + GetDataSize)^.BookMark <> nil then
    begin
      FreeMem(PcaRecordInfo(Buffer + GetDataSize)^.BookMark);
      PcaRecordInfo(Buffer + GetDataSize)^.BookMark := nil;
    end;
end;

  // Record and field info 

function TcaBaseDataset.GetDataSize: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to FieldCount - 1 do
    case Fields[Index].DataType of
      ftString:
        Result := Result + Fields[Index].Size + 1; 
      ftInteger, ftSmallInt, ftDate, ftTime:
        Result := Result + SizeOf(Integer);
      ftFloat, ftCurrency, ftBCD, ftDateTime:
        Result := Result + SizeOf(Double);
      ftBoolean:
        Result := Result + SizeOf(WordBool);
      ftBlob, ftMemo, ftGraphic:
        Result := Result + SizeOf(Pointer);
    end;
end;

function TcaBaseDataset.GetFieldOffset(Field: TField): Integer;
var
  FieldPos: Integer;
  Index: Integer;
begin
  Result := 0;
  FieldPos := FBufferMap.Indexof(Field.FieldName);
  for Index := 0 to FieldPos - 1 do
    begin
      case FieldbyName(FBufferMap[Index]).DataType of
        ftString:
          Inc(Result, FieldbyName(FBufferMap[Index]).Size + 1);
        ftInteger, ftSmallInt, ftDate, ftTime:
          Inc(Result, SizeOf(Integer));
        ftDateTime, ftFloat, ftBCD, ftCurrency:
          Inc(Result, SizeOf(Double));
        ftBoolean:
          Inc(Result, SizeOf(WordBool));
        ftBlob, ftGraphic, ftMemo:
          Inc(Result, SizeOf(Pointer));
      end;
    end;
end;

  // Buffer / record conversion 

procedure TcaBaseDataset.BufferToRecord(Buffer: PChar);
var
  Index: Integer;
  Offset: Integer;
  Stream: TStream;
  TempBool: WordBool;
  TempDouble: Double;
  TempInt: Integer;
  TempStr: string;
begin
  for Index := 0 to FieldCount - 1 do
    begin
      Offset := GetFieldOffset(Fields[Index]);
      case Fields[Index].DataType of
        ftString:
          begin
            TempStr := PChar(Buffer + Offset);
            SetFieldValue(Fields[Index], TempStr);
          end;
        ftInteger, ftSmallInt, ftDate, ftTime:
          begin
            Move((Buffer + Offset)^, TempInt, SizeOf(Integer));
            SetFieldValue(Fields[Index], TempInt);
          end;
        ftFloat, ftBCD, ftCurrency, ftDateTime:
          begin
            Move((Buffer + Offset)^, TempDouble, SizeOf(Double));
            SetFieldValue(Fields[Index], TempDouble);
          end;
        ftBoolean:
          begin
            Move((Buffer + Offset)^, TempBool, SizeOf(WordBool));
            SetFieldValue(Fields[Index], TempBool);
          end;
        ftBlob, ftGraphic, ftMemo:
          begin
            Move((Buffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
            Stream.Position := 0;
            SetBlobField(Fields[Index], Stream);
          end;
      end;
    end;
end;

procedure TcaBaseDataset.RecordToBuffer(Buffer: PChar);
var
  Index: Integer;
  Offset: Integer;
  Stream: TStream;
  TempBool: WordBool;
  TempDouble: Double;
  TempInt: Integer;
  TempStr: string;
  Value: Variant;
begin
  with PcaRecordInfo(Buffer + GetDataSize)^ do
    begin
      BookmarkFlag := bfCurrent;
      RecordID := AllocateRecordID;
      if GetBookMarkSize > 0 then
        begin
          if BookMark = nil then
            GetMem(BookMark, GetBookMarkSize);
          AllocateBookMark(RecordID, BookMark);
        end
      else
        BookMark := nil;
    end;
  DoBeforeGetFieldValue;
  for Index := 0 to FieldCount - 1 do
    begin
      if not (Fields[Index].DataType in [ftBlob, ftMemo, ftGraphic]) then
        Value := GetFieldValue(Fields[Index]);
      Offset := GetFieldOffset(Fields[Index]);
      case Fields[Index].DataType of
        ftString:
          begin
            TempStr := Value;
            if length(TempStr) > Fields[Index].Size then
              System.Delete(TempStr, Fields[Index].Size, length(TempStr) - Fields[Index].Size);
            StrLCopy(PChar(Buffer + Offset), PChar(TempStr), length(TempStr));
          end;
        ftInteger, ftSmallInt, ftDate, ftTime:
          begin
            TempInt := Value;
            Move(TempInt, (Buffer + Offset)^, SizeOf(TempInt));
          end;
        ftFloat, ftBCD, ftCurrency, ftDateTime:
          begin
            TempDouble := Value;
            Move(TempDouble, (Buffer + Offset)^, SizeOf(TempDouble));
          end;
        ftBoolean:
          begin
            TempBool := Value;
            Move(TempBool, (Buffer + Offset)^, SizeOf(TempBool));
          end;
        ftBlob, ftMemo, ftGraphic:
          begin
            Move((Buffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
            Stream.Size := 0; Stream.Position := 0;
            GetBlobField(Fields[Index], Stream);
          end;
      end;
    end;
  DoAfterGetFieldValue;
end;

  // Called before and after getting a set of field values 

procedure TcaBaseDataset.DoBeforeGetFieldValue;
begin
  // Virtual 
end;

procedure TcaBaseDataset.DoAfterGetFieldValue;
begin
  // Virtual 
end;

procedure TcaBaseDataset.DoBeforeSetFieldValue(Inserting: Boolean);
begin
  // Virtual 
end;

procedure TcaBaseDataset.DoAfterSetFieldValue(Inserting: Boolean);
begin
  // Virtual 
end;

  // BookMark functions 

function TcaBaseDataset.GetBookMarkSize: Integer;
begin
  Result := 0;
end;

  //----------------------------------------------------------------------------
  // These are the virtual methods from TDataset that have all been overridden  
  //----------------------------------------------------------------------------

function TcaBaseDataset.AllocRecordBuffer: PChar;
begin
  GetMem(Result, GetRecordSize);
  FillChar(Result^, GetRecordSize, 0);
  AllocateBlobPointers(Result);
end;

function TcaBaseDataset.GetActiveRecordBuffer: PChar;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        Result := nil
      else
        Result := ActiveBuffer;
    dsCalcFields:
      Result := CalcBuffer;
    dsFilter:
      Result := nil;
    dsEdit, dsInsert:
      Result := ActiveBuffer;
  else
    Result := nil;
  end;
end;

function TcaBaseDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PcaRecordInfo(Buffer + GetDataSize).BookMarkFlag;
end;

function TcaBaseDataset.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TcaBaseDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := Navigate(GetMode);
  if (Result = grOk) then
    begin
      RecordToBuffer(Buffer);
      ClearCalcFields(Buffer);
      GetCalcFields(Buffer);
    end
  else
    if (Result = grError) and DoCheck then
      DatabaseError('No Records');
end;

function TcaBaseDataset.GetRecordSize: Word;
begin
  Result := GetDataSize + SizeOf(TcaRecordInfo) + CalcFieldsSize;
  FStartCalculated := GetDataSize + SizeOf(TcaRecordInfo);
end;

function TcaBaseDataset.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

procedure TcaBaseDataset.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FStartCalculated], CalcFieldsSize, 0);
end;

procedure TcaBaseDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FreeMem(Buffer, GetRecordSize);
end;

procedure TcaBaseDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if BookMarkSize > 0 then
    AllocateBookMark(PcaRecordInfo(Buffer + GetDataSize).RecordID, Data);
end;

procedure TcaBaseDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PcaRecordInfo(Buffer + GetDataSize).BookMarkFlag := Value;
end;

procedure TcaBaseDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then InternalLast;
  DoBeforeSetFieldValue(True);
  BufferToRecord(Buffer);
  DoAfterSetFieldValue(True);
end;

procedure TcaBaseDataset.InternalClose;
begin
  BindFields(False);
  if DefaultFields then DestroyFields;
  DoClose;
  FIsOpen := False;
end;

procedure TcaBaseDataset.InternalDelete;
begin
  DoDeleteRecord;
end;

procedure TcaBaseDataset.InternalEdit;
begin
  if GetActiveRecordBuffer <> nil then
    InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TcaBaseDataset.InternalFirst;
begin
  DoFirst;
end;

procedure TcaBaseDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  DoGotoBookMark(BookMark);
end;

procedure TcaBaseDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TcaBaseDataset.InternalInsert;
begin
  // 
end;

procedure TcaBaseDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  DoCreateFieldDefs;
end;

procedure TcaBaseDataset.InternalInitRecord(Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FillChar(Buffer^, GetRecordSize, 0);
  AllocateBlobPointers(Buffer);
end;

procedure TcaBaseDataset.InternalLast;
begin
  DoLast;
end;

procedure TcaBaseDataset.InternalOpen;
begin
  if CanOpen then
    begin
      // Bookmarks not supported 
      BookmarkSize := GetBookMarkSize;
      InternalInitFieldDefs;
      if DefaultFields and (not FFieldsCreated) then
        begin
          CreateFields;
          FFieldsCreated := True;
        end;
      BindFields(True);
      FIsOpen := True;
      FillBufferMap;
    end;
end;

procedure TcaBaseDataset.InternalPost;
begin
  if FIsOpen then
    begin
      if State = dsInsert then InternalLast;
      DoBeforeSetFieldValue(State = dsInsert);
      BufferToRecord(GetActiveRecordBuffer);
      DoAfterSetFieldValue(State = dsInsert);
    end;
end;

procedure TcaBaseDataset.InternalSetToRecord(Buffer: PChar);
begin
  GotoRecordID(PcaRecordInfo(Buffer + GetDataSize).RecordID);
end;

procedure TcaBaseDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if PcaRecordInfo(Buffer + GetDataSize)^.BookMark = nil then
    GetMem(PcaRecordInfo(Buffer + GetDataSize)^.BookMark, GetBookMarkSize);
  Move(PcaRecordInfo(Buffer + GetDataSize).BookMark^, Data, GetBookMarkSize);
end;

procedure TcaBaseDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  Data: TDateTimeRec;
  Offset: Integer;
  RecBuffer: Pchar;
  TempBool: WordBool;
  TempDouble: Double;
  TimeStamp: TTimeStamp;
begin
  if Active then
    begin
      RecBuffer := GetActiveRecordBuffer;
      if (RecBuffer <> nil) and (Buffer <> nil) then
        begin
          if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
            begin
              Inc(RecBuffer, FStartCalculated + Field.Offset);
              Boolean(RecBuffer[0]) := (Buffer <> nil);
              if Boolean(RecBuffer[0]) then
                CopyMemory(@RecBuffer[1], Buffer, Field.DataSize);
            end
          else
            begin
              Offset := GetFieldOffset(Field);
              case Field.DataType of
                ftInteger, ftDate, ftTime:
                  begin
                    Move(Integer(Buffer^), (RecBuffer + Offset)^, SizeOf(Integer));
                  end;
                ftSmallInt:
                  begin
                    Move(SmallInt(Buffer^), (RecBuffer + Offset)^, SizeOf(SmallInt));
                  end;
                ftBoolean:
                  begin
                    Move(WordBool(Buffer^), TempBool, SizeOf(WordBool));
                    Move(TempBool, (RecBuffer + Offset)^, SizeOf(WordBool));
                  end;
                ftString:
                  begin
                    StrLCopy(PChar(RecBuffer + Offset), Buffer, StrLen(PChar(Buffer)));
                  end;
                ftDateTime:
                  begin
                    Data := TDateTimeRec(Buffer^);
                    TimeStamp := MSecsToTimeStamp(Data.DateTime);
                    TempDouble := TimeStampToDateTime(TimeStamp);
                    Move(TempDouble, (RecBuffer + Offset)^, SizeOf(TempDouble));
                  end;
                ftFloat, ftCurrency:
                  begin
                    Move(Double(Buffer^), (RecBuffer + Offset)^, SizeOf(Double));
                  end;
              end;
            end;
          if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
            DataEvent(deFieldChange, Longint(Field));
        end;
    end;
end;

  // Private methods 

function TcaBaseDataset.GetCalculatedFieldData(Field: TField; Buffer: Pointer; RecBuffer: PChar): Boolean;
begin
  Result := False;
  Inc(RecBuffer, FStartCalculated + Field.Offset);
  if (RecBuffer[0] <> #0) and (Buffer <> nil) then
    begin
      Result := True;
      CopyMemory(Buffer, @RecBuffer[1], Field.DataSize)
    end;
end;

procedure TcaBaseDataset.GetPhysicalFieldData(Field: TField; Buffer: Pointer; RecBuffer: PChar);
var
  Data: TDateTimeRec;
  Offset: Integer;
  TempBool: WordBool;
  TempDouble: Double;
  TimeStamp: TTimeStamp;
begin
  Offset := GetFieldOffset(Field);
  case Field.DataType of
    ftInteger, ftTime, ftDate:
      begin
        Move((RecBuffer + Offset)^, Integer(Buffer^), SizeOf(Integer));
      end;
    ftSmallInt:
      begin
        Move((RecBuffer + Offset)^, SmallInt(Buffer^), SizeOf(SmallInt));
      end;
    ftBoolean:
      begin
        Move((RecBuffer + Offset)^, TempBool, SizeOf(WordBool));
        Move(TempBool, WordBool(Buffer^), SizeOf(WordBool));
      end;
    ftString:
      begin
        StrLCopy(Buffer, PChar(RecBuffer + Offset), StrLen(PChar(RecBuffer + Offset)));
      end;
    ftCurrency, ftFloat:
      begin
        Move((RecBuffer + Offset)^, Double(Buffer^), SizeOf(Double));
      end;
    ftDateTime:
      begin
        Move((RecBuffer + Offset)^, TempDouble, SizeOf(Double));
        TimeStamp := DateTimeToTimeStamp(TempDouble);
        Data.DateTime := TimeStampToMSecs(TimeStamp);
        Move(Data, Buffer^, SizeOf(TDateTimeRec));
      end;
  end;
end;

procedure TcaBaseDataset.FillBufferMap;
var
  Index: Integer;
begin
  FBufferMap.Clear;
  for Index := 0 to FieldCount - 1 do
    FBufferMap.Add(Fields[Index].FieldName);
end;

  // Property methods 

function TcaBaseDataset.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TcaBaseDataset.SetReadOnly(const Value: Boolean);
begin
  if Value <> FReadOnly then
    begin
      if Active then DatabaseError('Cannot change readonly property when dataset is active');
      FReadOnly := Value;
    end;
end;

  //----------------------------------------------------------------------------
  // TcaBlobStream                                                              
  //----------------------------------------------------------------------------

constructor TcaBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FMode := Mode;
  FDataSet := FField.DataSet as TcaBaseDataset;
  if Mode <> bmWrite then LoadBlobData;
end;

destructor TcaBlobStream.Destroy;
begin
  if FModified then SaveBlobData;
  inherited;
end;

  // Public methods 

function TcaBlobStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := inherited Read(Buffer, Count);
  FOpened := True;
end;

function TcaBlobStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

  // Private methods 

procedure TcaBlobStream.LoadBlobData;
var
  Offset: Integer;
  RecBuffer: PChar;
  Stream: TMemoryStream;
begin
  Self.Size := 0;
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
    begin
      Offset := FDataset.GetFieldOffset(FField);
      Move((RecBuffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
      Self.CopyFrom(Stream, 0);
    end;
  Position := 0;
end;

procedure TcaBlobStream.SaveBlobData;
var
  Offset: Integer;
  RecBuffer: PChar;
  Stream: TMemoryStream;
begin
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
    begin
      Offset := FDataset.GetFieldOffset(FField);
      Move((RecBuffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
      Stream.Size := 0;
      Stream.CopyFrom(Self, 0);
      Stream.Position := 0;
    end;
  FModified := False;
end;

end.
