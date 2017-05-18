unit caVector;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  Math,

  // ca units 
  caCell,
  caClasses,
  caTypes,
  caUtils,
  caLog;

type

  //---------------------------------------------------------------------------
  // IcaVectorStringsAligner                                                   
  //---------------------------------------------------------------------------

  TcaVector = class;

  IcaVectorStringsAligner = interface
  ['{4B90DBAC-1F92-41FB-9D57-6AED7DDC22F6}']
    function GetAlignedStrings: IcaStringList;
    function GetStringsAlignment: TcaArrayStringsAlignment;
    function GetStringsWidth: Integer;
    procedure Execute;
    procedure SetStringsAlignment(const Value: TcaArrayStringsAlignment);
    procedure SetStringsWidth(const Value: Integer);
    property AlignedStrings: IcaStringList read GetAlignedStrings;
    property StringsAlignment: TcaArrayStringsAlignment read GetStringsAlignment write SetStringsAlignment;
    property StringsWidth: Integer read GetStringsWidth write SetStringsWidth;
  end;

  //---------------------------------------------------------------------------
  // TcaVectorStringsAligner                                                   
  //---------------------------------------------------------------------------

  TcaVectorStringsAligner = class(TInterfacedObject, IcaVectorStringsAligner)
  private
    FAlignedStrings: IcaStringList;
    FStringsAlignment: TcaArrayStringsAlignment;
    FStringsWidth: Integer;
    FVector: TcaVector;
    function GetAlignedStrings: IcaStringList;
    function GetStringsAlignment: TcaArrayStringsAlignment;
    function GetStringsWidth: Integer;
    procedure SetStringsAlignment(const Value: TcaArrayStringsAlignment);
    procedure SetStringsWidth(const Value: Integer);
  public
    constructor Create(AVector: TcaVector);
    procedure Execute;
    property AlignedStrings: IcaStringList read GetAlignedStrings;
    property StringsAlignment: TcaArrayStringsAlignment read GetStringsAlignment write SetStringsAlignment;
    property StringsWidth: Integer read GetStringsWidth write SetStringsWidth;
  end;

  //---------------------------------------------------------------------------
  // IcaVector                                                                 
  //---------------------------------------------------------------------------

  IcaVector = interface
  ['{04600722-10EF-44E9-B397-F8A21194103C}']
    // Property methods 
    function GetAlignedStrings: IcaStringList;
    function GetAligner: IcaVectorStringsAligner;
    function GetCells: TList;
    function GetCount: Integer;
    function GetStringsAlignment: TcaArrayStringsAlignment;
    function GetStringsWidth: Integer;
    procedure SetCount(const Value: Integer);
    procedure SetStringsAlignment(const Value: TcaArrayStringsAlignment);
    procedure SetStringsWidth(const Value: Integer);
    // Event property methods 
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    // Protected properties 
    property Cells: TList read GetCells;
    // Public methods 
    procedure Assign(ASource: IcaVector); overload;
    procedure Assign(ASource: TcaVector); overload;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure GrowBy(Delta: Integer);
    procedure ShrinkBy(Delta: Integer);
    // Properties 
    property AlignedStrings: IcaStringList read GetAlignedStrings;
    property Aligner: IcaVectorStringsAligner read GetAligner;
    property Count: Integer read GetCount write SetCount;
    property StringsAlignment: TcaArrayStringsAlignment read GetStringsAlignment write SetStringsAlignment;
    property StringsWidth: Integer read GetStringsWidth write SetStringsWidth;
    // Event properties 
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

  IcaVectorItemString = interface
  ['{B5028F46-B8BB-4859-9359-B0D2E48449FB}']
    function GetItemString(Index: Integer): String;
  end;

  //---------------------------------------------------------------------------
  // TcaVector                                                                 
  //---------------------------------------------------------------------------

  TcaVector = class(TInterfacedObject, IcaVector, IcaVectorStringsAligner)
  private
    FAligner: IcaVectorStringsAligner;
    FCells: TList;
    // Event property fields 
    FOnChange: TNotifyEvent;
    // Property methods 
    function GetAlignedStrings: IcaStringList;
    function GetAligner: IcaVectorStringsAligner;
    function GetCells: TList;
    function GetHigh: Integer;
    function GetLow: Integer;
    function GetCount: Integer;
    function GetStringsAlignment: TcaArrayStringsAlignment;
    function GetStringsWidth: Integer;
    procedure SetCount(const Value: Integer);
    procedure SetStringsAlignment(const Value: TcaArrayStringsAlignment);
    procedure SetStringsWidth(const Value: Integer);
    // Event property methods 
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    // Event handlers 
    procedure CellChangedEvent(Sender: TObject);
  protected
    // Static protected methods 
    function CreateCellObject: TcaCell;
    // Virtual protected methods 
    function CreateCell: TcaCell; virtual; abstract;
    procedure DoChanged; virtual;
    // Protected properties 
    property Cells: TList read GetCells;
  public
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    procedure Assign(ASource: IcaVector); overload;
    procedure Assign(ASource: TcaVector); overload;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure GrowBy(Delta: Integer);
    procedure ShrinkBy(Delta: Integer);
    // Properties 
    property Aligner: IcaVectorStringsAligner read GetAligner implements IcaVectorStringsAligner;
    property AlignedStrings: IcaStringList read GetAlignedStrings;
    property High: Integer read GetHigh;
    property Low: Integer read GetLow;
    property Count: Integer read GetCount write SetCount;
    property StringsAlignment: TcaArrayStringsAlignment read GetStringsAlignment write SetStringsAlignment;
    property StringsWidth: Integer read GetStringsWidth write SetStringsWidth;
    // Event properties 
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

  //---------------------------------------------------------------------------
  // IcaIntegerVector                                                          
  //---------------------------------------------------------------------------

  IcaIntegerVector = interface(IcaVector)
  ['{20A2544D-C35D-40EF-9133-A095E324A046}']
    // Property methods 
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Value: Integer);
    // Public methods 
    function Add(const AItem: Integer): Integer;
    function IndexOf(const AItem: Integer): Integer;
    procedure AddArray(AArray: array of Integer);
    // Properties 
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaIntegerStack                                                           
  //---------------------------------------------------------------------------

  IcaIntegerStack = interface
  ['{827FE124-87BA-48DD-86D8-7C55D8DFB0F4}']
    // Property methods 
    function GetIsEmpty: Boolean;
    function GetSize: Integer;
    // Public methods 
    function Peek: Integer;
    function Pop: Integer;
    function Push(AItem: Integer): Integer;
    // Properties 
    property IsEmpty: Boolean read GetIsEmpty;
    property Size: Integer read GetSize;
  end;

  //---------------------------------------------------------------------------
  // TcaIntegerVector                                                          
  //---------------------------------------------------------------------------

  TcaIntegerVector = class(TcaVector, IcaIntegerVector,
                                        IcaVectorItemString,
                                        IcaIntegerStack)
  private
    // IcaIntegerVector property methods 
    function GetItem(Index: Integer): Integer;
    function GetItemString(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: Integer);
    // IcaIntegerStack property methods 
    function GetIsEmpty: Boolean;
    function GetSize: Integer;
  protected
    function CreateCell: TcaCell; override;
  public
    // IcaIntegerVector public methods 
    function Add(const AItem: Integer): Integer;
    function IndexOf(const AItem: Integer): Integer;
    procedure AddArray(AArray: array of Integer);
    // IcaIntegerVector properties 
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
    // IcaIntegerStack public methods 
    function Peek: Integer;
    function Pop: Integer;
    function Push(AItem: Integer): Integer;
    // IcaIntegerStack properties 
    property IsEmpty: Boolean read GetIsEmpty;
    property Size: Integer read GetSize;
  end;

  //---------------------------------------------------------------------------
  // IcaStringVector                                                           
  //---------------------------------------------------------------------------

  IcaStringVector = interface(IcaVector)
  ['{2674C3AB-683A-4A37-9682-7F99FC117675}']
    // Property methods 
    function GetItem(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: String);
    // Public methods 
    function Add(const AItem: String): Integer;
    function IndexOf(const AItem: String): Integer;
    procedure AddArray(AArray: array of String);
    // Properties 
    property Items[Index: Integer]: String read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaStringVector                                                           
  //---------------------------------------------------------------------------

  TcaStringVector = class(TcaVector, IcaStringVector, IcaVectorItemString)
  private
    function GetItem(Index: Integer): String;
    function GetItemString(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: String);
  protected
    function CreateCell: TcaCell; override;
  public
    // Public methods 
    function Add(const AItem: String): Integer;
    function IndexOf(const AItem: String): Integer;
    procedure AddArray(AArray: array of String);
    // Properties 
    property Items[Index: Integer]: String read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaSingleVector                                                           
  //---------------------------------------------------------------------------

  IcaSingleVector = interface(IcaVector)
  ['{E71CF14D-13BD-487F-8CDA-42899477995A}']
    // Property methods 
    function GetItem(Index: Integer): Single;
    procedure SetItem(Index: Integer; const Value: Single);
    // Public methods 
    function Add(const AItem: Single): Integer;
    function IndexOf(const AItem: Single): Integer;
    procedure AddArray(AArray: array of Single);
    // Properties 
    property Items[Index: Integer]: Single read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaSingleVector                                                           
  //---------------------------------------------------------------------------

  TcaSingleVector = class(TcaVector, IcaSingleVector, IcaVectorItemString)
  private
    function GetItem(Index: Integer): Single;
    function GetItemString(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: Single);
  protected
    function CreateCell: TcaCell; override;
  public
    // Public methods 
    function Add(const AItem: Single): Integer;
    function IndexOf(const AItem: Single): Integer;
    procedure AddArray(AArray: array of Single);
    // Properties 
    property Items[Index: Integer]: Single read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaDoubleVector                                                           
  //---------------------------------------------------------------------------

  IcaDoubleVector = interface(IcaVector)
  ['{E71CF14D-13BD-487F-8CDA-42899477995A}']
    // Property methods 
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Value: Double);
    // Public methods 
    function Add(const AItem: Double): Integer;
    function IndexOf(const AItem: Double): Integer;
    procedure AddArray(AArray: array of Double);
    // Properties 
    property Items[Index: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaDoubleVector                                                           
  //---------------------------------------------------------------------------

  TcaDoubleVector = class(TcaVector, IcaDoubleVector, IcaVectorItemString)
  private
    function GetItem(Index: Integer): Double;
    function GetItemString(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: Double);
  protected
    function CreateCell: TcaCell; override;
  public
    // Public methods 
    function Add(const AItem: Double): Integer;
    function IndexOf(const AItem: Double): Integer;
    procedure AddArray(AArray: array of Double);
    // Properties 
    property Items[Index: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaExtendedVector                                                         
  //---------------------------------------------------------------------------

  IcaExtendedVector = interface(IcaVector)
  ['{697807BB-A152-46C4-A958-F83316C22EA8}']
    // Property methods 
    function GetItem(Index: Integer): Extended;
    procedure SetItem(Index: Integer; const Value: Extended);
    // Public methods 
    function Add(const AItem: Extended): Integer;
    function IndexOf(const AItem: Extended): Integer;
    procedure AddArray(AArray: array of Extended);
    // Properties 
    property Items[Index: Integer]: Extended read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaExtendedVector                                                         
  //---------------------------------------------------------------------------

  TcaExtendedVector = class(TcaVector, IcaExtendedVector, IcaVectorItemString)
  private
    function GetItem(Index: Integer): Extended;
    function GetItemString(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: Extended);
  protected
    function CreateCell: TcaCell; override;
  public
    // Public methods 
    function Add(const AItem: Extended): Integer;
    function IndexOf(const AItem: Extended): Integer;
    procedure AddArray(AArray: array of Extended);
    // Properties 
    property Items[Index: Integer]: Extended read GetItem write SetItem; default;
  end;

implementation

  //---------------------------------------------------------------------------
  // IcaVectorStringsAligner                                                   
  //---------------------------------------------------------------------------

constructor TcaVectorStringsAligner.Create(AVector: TcaVector);
begin
  inherited Create;
  FVector := AVector;
  FAlignedStrings := TcaStringList.Create;
end;

function TcaVectorStringsAligner.GetAlignedStrings: IcaStringList;
begin
  Result := FAlignedStrings;
end;

procedure TcaVectorStringsAligner.Execute;
var
  Index: Integer;
  MaxWidth: Integer;
  UnAlignedStrings: IcaStringList;
  UnAlignedStr: String;
  UnAlignedString: IcaString;
  AlignedStr: String;
  PadStr: String;
  VItemStr: IcaVectorItemString;
begin
  UnAlignedStrings := TcaStringList.Create;
  // Build UnAlignedStrings 
  VItemStr := FVector as IcaVectorItemString;
  for Index := FVector.Low to FVector.High do
    begin
      UnAlignedStr := VItemStr.GetItemString(Index);
      UnAlignedStrings.Add(UnAlignedStr);
    end;
  // Find widest possible string 
  if FStringsWidth <> 0 then
    MaxWidth := FStringsWidth
  else
    MaxWidth := UnAlignedStrings.WidthOfWidest;
  // Build new list 
  FAlignedStrings.Clear;
  // cautils 
  PadStr := Utils.BuildString(#32, MaxWidth);
  for Index := UnAlignedStrings.Low to UnAlignedStrings.High do
    begin
      UnAlignedString := TcaString.Create(UnAlignedStrings[Index]);
      case FStringsAlignment of
        saLeft:     AlignedStr := UnAlignedString.PadRight(MaxWidth);
        saRight:    AlignedStr := UnAlignedString.PadLeft(MaxWidth);
        saPreZero:  AlignedStr := UnAlignedString.PreZero(MaxWidth);
      end;
      FAlignedStrings.Add(AlignedStr);
    end;
end;

procedure TcaVectorStringsAligner.SetStringsWidth(const Value: Integer);
begin
  FStringsWidth := Value;
end;

procedure TcaVectorStringsAligner.SetStringsAlignment(const Value: TcaArrayStringsAlignment);
begin
  FStringsAlignment := Value;
end;

function TcaVectorStringsAligner.GetStringsWidth: Integer;
begin
  Result := FStringsWidth;
end;

function TcaVectorStringsAligner.GetStringsAlignment: TcaArrayStringsAlignment;
begin
  Result := FStringsAlignment;
end;

  //---------------------------------------------------------------------------
  // TcaVector                                                                 
  //---------------------------------------------------------------------------

constructor TcaVector.Create;
begin
  inherited;
  FAligner := TcaVectorStringsAligner.Create(Self);
  FCells := TList.Create;
end;

destructor TcaVector.Destroy;
begin
  FCells.Free;
  inherited;
end;

  // Public methods 

procedure TcaVector.Assign(ASource: IcaVector);
var
  Index: Integer;
  Cell: TcaCell;
  SourceCell: TcaCell;
begin
  Clear;
  for Index := 0 to ASource.Count - 1 do
    begin
      SourceCell := TcaCell(ASource.Cells[Index]);
      Cell := CreateCellObject;
      FCells.Add(Cell);
      Cell.AsString := SourceCell.AsString;
    end;
end;

procedure TcaVector.Assign(ASource: TcaVector);
var
  Index: Integer;
  Cell: TcaCell;
  SourceCell: TcaCell;
begin
  Clear;
  for Index := 0 to ASource.Count - 1 do
    begin
      SourceCell := TcaCell(ASource.Cells[Index]);
      Cell := CreateCellObject;
      FCells.Add(Cell);
      Cell.AsString := SourceCell.AsString;
    end;
end;

procedure TcaVector.Clear;
begin
  FCells.Clear;
  DoChanged;
end;

procedure TcaVector.Delete(AIndex: Integer);
begin
  FCells.Delete(AIndex);
end;

procedure TcaVector.GrowBy(Delta: Integer);
var
  Index: Integer;
begin
  for Index := 1 to Delta do
    FCells.Add(CreateCellObject);
end;

procedure TcaVector.ShrinkBy(Delta: Integer);
var
  NewCount: Integer;
begin
  NewCount := Max(0, GetCount - Delta);
  while GetCount > NewCount do
    FCells.Delete(GetCount);
end;

  // Static protected methods 

function TcaVector.CreateCellObject: TcaCell;
begin
  Result := CreateCell;
  Result.OnChange := CellChangedEvent;
  DoChanged;
end;

  // Virtual protected methods 

procedure TcaVector.DoChanged;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

  // Private methods 

function TcaVector.GetAlignedStrings: IcaStringList;
begin
  FAligner.Execute;
  Result := FAligner.AlignedStrings;
end;

function TcaVector.GetAligner: IcaVectorStringsAligner;
begin
  Result := FAligner;
end;

function TcaVector.GetCells: TList;
begin
  Result := FCells;
end;

function TcaVector.GetHigh: Integer;
begin
  Result := FCells.Count - 1;
end;

function TcaVector.GetLow: Integer;
begin
  Result := 0;
end;

function TcaVector.GetCount: Integer;
begin
  Result := FCells.Count;
end;

function TcaVector.GetStringsAlignment: TcaArrayStringsAlignment;
begin
  Result := FAligner.StringsAlignment;
end;

function TcaVector.GetStringsWidth: Integer;
begin
  Result := FAligner.StringsWidth;
end;

procedure TcaVector.SetCount(const Value: Integer);
var
  Delta: Integer;
begin
  Delta := Value - GetCount;
  if Delta > 0 then
    GrowBy(Delta)
  else
    ShrinkBy(Delta);
end;

procedure TcaVector.SetStringsWidth(const Value: Integer);
begin
  FAligner.StringsWidth := Value;
end;

procedure TcaVector.SetStringsAlignment(const Value: TcaArrayStringsAlignment);
begin
  FAligner.StringsAlignment := Value;
end;

  // Event handlers 

procedure TcaVector.CellChangedEvent(Sender: TObject);
begin
  DoChanged;
end;

  // Event property methods 

function TcaVector.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

procedure TcaVector.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

  //---------------------------------------------------------------------------
  // TcaIntegerVector                                                          
  //---------------------------------------------------------------------------

  // IcaIntegerVector public methods 

function TcaIntegerVector.Add(const AItem: Integer): Integer;
var
  Cell: TcaCell;
begin
  Cell := CreateCellObject;
  TcaIntegerCell(Cell).Value := AItem;
  Result := Cells.Add(Cell);
end;

procedure TcaIntegerVector.AddArray(AArray: array of Integer);
var
  Index: Integer;
begin
  for Index := System.Low(AArray) to System.High(AArray) do
    Add(AArray[Index]);
end;

function TcaIntegerVector.IndexOf(const AItem: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count - 1 do
    begin
      if GetItem(Index) = AItem then
        begin
          Result := Index;
          Break;
        end;
    end;
end;

  // IcaIntegerStack public methods 

function TcaIntegerVector.Peek: Integer;
begin
  if Count = 0 then
    Result := 0
  else
    Result := Items[High];
end;

function TcaIntegerVector.Pop: Integer;
begin
  if Count = 0 then
    Result := 0
  else
    begin
      Result := Peek;
      Delete(High);
    end;
end;

function TcaIntegerVector.Push(AItem: Integer): Integer;
begin
  Result := Add(AItem);
end;

  // Protected methods 

function TcaIntegerVector.CreateCell: TcaCell;
begin
  Result := TcaIntegerCell.Create;
end;

  // IcaIntegerVector property methods 

function TcaIntegerVector.GetItem(Index: Integer): Integer;
begin
  Result := TcaIntegerCell(Cells[Index]).Value;
end;

function TcaIntegerVector.GetItemString(Index: Integer): String;
begin
  Result := IntToStr(GetItem(Index));
end;

procedure TcaIntegerVector.SetItem(Index: Integer; const Value: Integer);
begin
  TcaIntegerCell(Cells[Index]).Value := Value;
end;

  // IcaIntegerStack property methods 

function TcaIntegerVector.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TcaIntegerVector.GetSize: Integer;
begin
  Result := Count;
end;

  //---------------------------------------------------------------------------
  // TcaStringVector                                                           
  //---------------------------------------------------------------------------

  // Public methods 

function TcaStringVector.Add(const AItem: String): Integer;
var
  Cell: TcaCell;
begin
  Cell := CreateCellObject;
  TcaStringCell(Cell).Value := AItem;
  Result := Cells.Add(Cell);
end;

procedure TcaStringVector.AddArray(AArray: array of String);
var
  Index: Integer;
begin
  for Index := System.Low(AArray) to System.High(AArray) do
    Add(AArray[Index]);
end;

function TcaStringVector.IndexOf(const AItem: String): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count - 1 do
    begin
      if GetItem(Index) = AItem then
        begin
          Result := Index;
          Break;
        end;
    end;
end;

  // Protected methods 

function TcaStringVector.CreateCell: TcaCell;
begin
  Result := TcaStringCell.Create;
end;

  // Property methods 

function TcaStringVector.GetItem(Index: Integer): String;
begin
  Result := TcaStringCell(Cells[Index]).Value;
end;

function TcaStringVector.GetItemString(Index: Integer): String;
begin
  Result := GetItem(Index);
end;

procedure TcaStringVector.SetItem(Index: Integer; const Value: String);
begin
  TcaStringCell(Cells[Index]).Value := Value;
end;

  //---------------------------------------------------------------------------
  // TcaSingleVector                                                           
  //---------------------------------------------------------------------------

  // Public methods 

function TcaSingleVector.Add(const AItem: Single): Integer;
var
  Cell: TcaCell;
begin
  Cell := CreateCellObject;
  TcaSingleCell(Cell).Value := AItem;
  Result := Cells.Add(Cell);
end;

procedure TcaSingleVector.AddArray(AArray: array of Single);
var
  Index: Integer;
begin
  for Index := System.Low(AArray) to System.High(AArray) do
    Add(AArray[Index]);
end;

function TcaSingleVector.IndexOf(const AItem: Single): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count - 1 do
    begin
      if GetItem(Index) = AItem then
        begin
          Result := Index;
          Break;
        end;
    end;
end;

  // Protected methods 

function TcaSingleVector.CreateCell: TcaCell;
begin
  Result := TcaSingleCell.Create;
end;

  // Property methods 

function TcaSingleVector.GetItem(Index: Integer): Single;
begin
  Result := TcaSingleCell(Cells[Index]).Value;
end;

function TcaSingleVector.GetItemString(Index: Integer): String;
begin
  Result := FloatToStr(GetItem(Index));
end;

procedure TcaSingleVector.SetItem(Index: Integer; const Value: Single);
begin
  TcaSingleCell(Cells[Index]).Value := Value;
end;

  //---------------------------------------------------------------------------
  // TcaDoubleVector                                                           
  //---------------------------------------------------------------------------

  // Public methods 

function TcaDoubleVector.Add(const AItem: Double): Integer;
var
  Cell: TcaCell;
begin
  Cell := CreateCellObject;
  TcaDoubleCell(Cell).Value := AItem;
  Result := Cells.Add(Cell);
end;

procedure TcaDoubleVector.AddArray(AArray: array of Double);
var
  Index: Integer;
begin
  for Index := System.Low(AArray) to System.High(AArray) do
    Add(AArray[Index]);
end;

function TcaDoubleVector.IndexOf(const AItem: Double): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count - 1 do
    begin
      if GetItem(Index) = AItem then
        begin
          Result := Index;
          Break;
        end;
    end;
end;

  // Protected methods 

function TcaDoubleVector.CreateCell: TcaCell;
begin
  Result := TcaDoubleCell.Create;
end;

function TcaDoubleVector.GetItem(Index: Integer): Double;
begin
  Result := TcaDoubleCell(Cells[Index]).Value;
end;

function TcaDoubleVector.GetItemString(Index: Integer): String;
begin
  Result := FloatToStr(GetItem(Index));
end;

procedure TcaDoubleVector.SetItem(Index: Integer; const Value: Double);
begin
  TcaDoubleCell(Cells[Index]).Value := Value;
end;

  //---------------------------------------------------------------------------
  // TcaExtendedVector                                                         
  //---------------------------------------------------------------------------

  // Public methods 

function TcaExtendedVector.Add(const AItem: Extended): Integer;
var
  Cell: TcaCell;
begin
  Cell := CreateCellObject;
  TcaExtendedCell(Cell).Value := AItem;
  Result := Cells.Add(Cell);
end;

procedure TcaExtendedVector.AddArray(AArray: array of Extended);
var
  Index: Integer;
begin
  for Index := System.Low(AArray) to System.High(AArray) do
    Add(AArray[Index]);
end;

function TcaExtendedVector.IndexOf(const AItem: Extended): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count - 1 do
    begin
      if GetItem(Index) = AItem then
        begin
          Result := Index;
          Break;
        end;
    end;
end;

  // Protected methods 

function TcaExtendedVector.CreateCell: TcaCell;
begin
  Result := TcaExtendedCell.Create;
end;

function TcaExtendedVector.GetItem(Index: Integer): Extended;
begin
  Result := TcaExtendedCell(Cells[Index]).Value;
end;

function TcaExtendedVector.GetItemString(Index: Integer): String;
begin
  Result := FloatToStr(GetItem(Index));
end;

procedure TcaExtendedVector.SetItem(Index: Integer; const Value: Extended);
begin
  TcaExtendedCell(Cells[Index]).Value := Value;
end;

end.


