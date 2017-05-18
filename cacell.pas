unit caCell;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,
  Math,
  TypInfo,
  Variants,

  // ca units 
  caUtils,
  caTypes;

type

  //---------------------------------------------------------------------------
  // TcaCell                                                                   
  //---------------------------------------------------------------------------

  TcaCell = class(TObject)
  private
    // Private fields 
    FCellType: TcaCellType;
    FDependentCells: TList;
    FName: String;
    FSelected: Boolean;
    FShouldUpdate: Boolean;
    FTag: Integer;
    // Event property fields 
    FOnChange: TNotifyEvent;
  protected
    // Protected property methods 
    function GetAsString: String; virtual;
    procedure SetAsString(const Value: String); virtual;
    // Protected methods 
    procedure Changed;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Public class methods 
    class function StringToCellType(ACellTypeString: string): TcaCellType;
    class function VariantToCellType(AValue: Variant): TcaCellType;
    // Public methods 
    procedure AddDependentCell(ACell: TcaCell);
    // Properties 
    property AsString: String read GetAsString write SetAsString;
    property CellType: TcaCellType read FCellType write FCellType;
    property DependentCells: TList read FDependentCells;
    property Name: String read FName write FName;
    property Selected: Boolean read FSelected write FSelected;
    property ShouldUpdate: Boolean read FShouldUpdate write FShouldUpdate;
    property Tag: Integer read FTag write FTag;
    // Event properties 
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TcaCellClass = class of TcaCell;

  //---------------------------------------------------------------------------
  // TcaObjectCell                                                             
  //---------------------------------------------------------------------------

  TcaObjectCell = class(TcaCell)
  private
    // Property fields 
    FOwnsObject: Boolean;
    FValue: TObject;
    // Property methods 
    function GetOwnsObject: Boolean;
    function GetValue: TObject;
    procedure SetOwnsObject(const Value: Boolean);
    procedure SetValue(const Value: TObject);
  protected
    // Protected property methods 
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Properties 
    property Value: TObject read GetValue write SetValue;
    property OwnsObject: Boolean read GetOwnsObject write SetOwnsObject;
  end;

  //----------------------------------------------------------------------------
  // TcaIntegerCell                                                             
  //----------------------------------------------------------------------------

  TcaIntegerCell = class(TcaCell)
  private
    // Property fields 
    FValue: Integer;
    // Property methods 
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
  protected
    // Protected property methods 
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    // Properties 
    property Value: Integer read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaInt64Cell                                                               
  //----------------------------------------------------------------------------

  TcaInt64Cell = class(TcaCell)
  private
    // Property fields 
    FValue: Int64;
    // Property methods 
    function GetValue: Int64;
    procedure SetValue(const Value: Int64);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    // Properties 
    property Value: Int64 read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaSingleCell                                                              
  //----------------------------------------------------------------------------

  TcaSingleCell = class(TcaCell)
  private
    // Property fields 
    FValue: Single;
    // Property methods 
    function GetValue: Single;
    procedure SetValue(const Value: Single);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    property Value: Single read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaDoubleCell                                                              
  //----------------------------------------------------------------------------

  TcaDoubleCell = class(TcaCell)
  private
    // Property fields 
    FValue: Double;
    // Property methods 
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    // Properties 
    property Value: Double read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaExtendedCell                                                            
  //----------------------------------------------------------------------------

  TcaExtendedCell = class(TcaCell)
  private
    // Property fields 
    FValue: Extended;
    // Property methods 
    function GetValue: Extended;
    procedure SetValue(const Value: Extended);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    // Properties 
    property Value: Extended read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaStringCell                                                              
  //----------------------------------------------------------------------------

  TcaStringCell = class(TcaCell)
  private
    // Property fields 
    FValue: String;
    FMemo: TStrings;
    // Property methods 
    function GetAsMemo: TStrings;
    function GetValue: String;
    procedure SetAsMemo(const Value: TStrings);
    procedure SetValue(const Value: String);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Properties 
    property AsMemo: TStrings read GetAsMemo write SetAsMemo;
    property Value: String read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaMemoCell                                                                
  //----------------------------------------------------------------------------

  TcaMemoCell = class(TcaCell)
  private
    // Property fields 
    FValue: TStrings;
    // Property methods 
    function GetValue: TStrings;
    procedure SetValue(const Value: TStrings);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Properties 
    property Value: TStrings read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaBooleanCell                                                             
  //----------------------------------------------------------------------------

  TcaBooleanCell = class(TcaCell)
  private
    // Property fields 
    FValue: Boolean;
    // Property methods 
    function GetValue: Boolean;
    procedure SetValue(const Value: Boolean);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    // Properties 
    property Value: Boolean read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaDateTimeCell                                                            
  //----------------------------------------------------------------------------

  TcaDateTimeCell = class(TcaCell)
  private
    // Property fields 
    FValue: TDateTime;
    // Property methods 
    function GetValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    // Properties 
    property Value: TDateTime read GetValue write SetValue;
  end;

  //----------------------------------------------------------------------------
  // TcaFormulaCell                                                             
  //----------------------------------------------------------------------------

  TcaFormulaCell = class(TcaCell)
  private
    // Property fields 
    FValue: String;
    // Property methods 
    function GetValue: String;
    procedure SetValue(const Value: String);
  protected
    // Protected property methods
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create;
    // Properties 
    property Value: String read GetValue write SetValue;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaCell                                                                   
  //---------------------------------------------------------------------------

constructor TcaCell.Create;
begin
  inherited;
  FDependentCells := TList.Create;
end;

destructor TcaCell.Destroy;
begin
  FDependentCells.Free;
  inherited;
end;

  // Public class methods 

class function TcaCell.StringToCellType(ACellTypeString: String): TcaCellType;
var
  ACellType: TcaCellType;
begin
  Result := Low(TcaCellType);
  for ACellType := Low(TcaCellType) to High(TcaCellType) do
    begin
      if GetEnumName(TypeInfo(TcaCellType), Ord(ACellType)) = ACellTypeString then
        begin
          Result := ACellType;
          Break;
        end;
    end;
end;

class function TcaCell.VariantToCellType(AValue: Variant): TcaCellType;
var
  VType: TVarType;
begin
  Result := ctNil;
  if not VarIsNull(AValue) then
    begin
      VType := VarType(AValue);
      case VType of
        varOleStr, varString:
          Result := ctString;
        varSmallint, varShortInt, varInteger, varByte, varWord, varLongWord:
          Result := ctInteger;
        varSingle:
          Result := ctSingle;
        varDouble, varCurrency:
          Result := ctDouble;
        varDate:
          Result := ctDateTime;
        varBoolean:
          Result := ctBoolean;
        varInt64:
          Result := ctInt64;
      end;
    end;
end;

  // Public methods 

procedure TcaCell.AddDependentCell(ACell: TcaCell);
begin
  FDependentCells.Add(ACell);
end;

  // Protected methods 

procedure TcaCell.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

  // Protected property methods 

function TcaCell.GetAsString: String;
begin
  Result := '';
end;

procedure TcaCell.SetAsString(const Value: String);
begin
end;

  //---------------------------------------------------------------------------
  // TcaObjectCell                                                             
  //---------------------------------------------------------------------------

constructor TcaObjectCell.Create;
begin
  inherited;
  CellType := ctObject;
end;

destructor TcaObjectCell.Destroy;
begin
  if FOwnsObject then FValue.Free;
  inherited;
end;

  // Protected property methods 

function TcaObjectCell.GetAsString: String;
begin
  Result := Utils.ObjectToString(FValue);
end;

procedure TcaObjectCell.SetAsString(const Value: String);
begin
  SetValue(TObject(Utils.StringToInteger(Value)));
end;

  // Property methods 

function TcaObjectCell.GetOwnsObject: Boolean;
begin
  Result := FOwnsObject;
end;

function TcaObjectCell.GetValue: TObject;
begin
  Result := FValue;
end;

procedure TcaObjectCell.SetOwnsObject(const Value: Boolean);
begin
  FOwnsObject := Value;
end;

procedure TcaObjectCell.SetValue(const Value: TObject);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaIntegerCell                                                             
  //----------------------------------------------------------------------------

constructor TcaIntegerCell.Create;
begin
  inherited;
  CellType := ctInteger;
end;

function TcaIntegerCell.GetAsString: String;
begin
  Result := Utils.IntegerToString(FValue, '');
end;

function TcaIntegerCell.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TcaIntegerCell.SetAsString(const Value: String);
begin
  SetValue(Utils.StringToInteger(Value));
end;

procedure TcaIntegerCell.SetValue(const Value: Integer);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaInt64Cell                                                               
  //----------------------------------------------------------------------------

constructor TcaInt64Cell.Create;
begin
  inherited;
  CellType := ctInt64;
end;

function TcaInt64Cell.GetAsString: String;
begin
  Result := Utils.Int64ToString(FValue, '');
end;

function TcaInt64Cell.GetValue: Int64;
begin
  Result := FValue;
end;

procedure TcaInt64Cell.SetAsString(const Value: String);
begin
  SetValue(Utils.StringToInt64(Value));
end;

procedure TcaInt64Cell.SetValue(const Value: Int64);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaSingleCell                                                              
  //----------------------------------------------------------------------------

constructor TcaSingleCell.Create;
begin
  inherited;
  CellType := ctSingle;
end;

function TcaSingleCell.GetAsString: String;
begin
  Result := Utils.SingleToString(FValue, '');
end;

function TcaSingleCell.GetValue: Single;
begin
  Result := FValue;
end;

procedure TcaSingleCell.SetAsString(const Value: String);
begin
  SetValue(Utils.StringToSingle(Value));
end;

procedure TcaSingleCell.SetValue(const Value: Single);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaDoubleCell                                                              
  //----------------------------------------------------------------------------

constructor TcaDoubleCell.Create;
begin
  inherited;
  CellType := ctDouble;
end;

function TcaDoubleCell.GetAsString: String;
begin
  Result := Utils.DoubleToString(FValue, '');
end;

function TcaDoubleCell.GetValue: Double;
begin
  Result := FValue;
end;

procedure TcaDoubleCell.SetAsString(const Value: String);
begin
  SetValue(Utils.StringToDouble(Value));
end;

procedure TcaDoubleCell.SetValue(const Value: Double);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaExtendedCell                                                            
  //----------------------------------------------------------------------------

constructor TcaExtendedCell.Create;
begin
  inherited;
  CellType := ctExtended;
end;

function TcaExtendedCell.GetAsString: String;
begin
  Result := Utils.ExtendedToString(FValue, '');
end;

function TcaExtendedCell.GetValue: Extended;
begin
  Result := FValue;
end;

procedure TcaExtendedCell.SetAsString(const Value: String);
begin
  SetValue(Utils.StringToExtended(Value));
end;

procedure TcaExtendedCell.SetValue(const Value: Extended);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaStringCell                                                              
  //----------------------------------------------------------------------------

constructor TcaStringCell.Create;
begin
  inherited;
  CellType := ctString;
  FMemo := TStringList.Create;
end;

destructor TcaStringCell.Destroy;
begin
  FMemo.Free;
  inherited;
end;

  // Property methods 

function TcaStringCell.GetAsMemo: TStrings;
begin
  FMemo.Text := FValue;
  Result := FMemo;
end;

function TcaStringCell.GetAsString: String;
begin
  Result := FValue;
end;

function TcaStringCell.GetValue: String;
begin
  Result := FValue;
end;

procedure TcaStringCell.SetAsMemo(const Value: TStrings);
begin
  FMemo.Assign(Value);
  FValue := FMemo.Text;
end;

procedure TcaStringCell.SetAsString(const Value: String);
begin
  SetValue(Value);
end;

procedure TcaStringCell.SetValue(const Value: String);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaMemoCell                                                                
  //----------------------------------------------------------------------------

constructor TcaMemoCell.Create;
begin
  inherited;
  CellType := ctMemo;
  FValue := TStringList.Create;
end;

destructor TcaMemoCell.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TcaMemoCell.GetAsString: String;
begin
  Result := Utils.MemoToString(FValue);
end;

function TcaMemoCell.GetValue: TStrings;
begin
  Result := FValue;
end;

procedure TcaMemoCell.SetAsString(const Value: String);
begin
  Utils.StringToMemo(Value, FValue);
  Changed;
end;

procedure TcaMemoCell.SetValue(const Value: TStrings);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaBooleanCell                                                             
  //----------------------------------------------------------------------------

constructor TcaBooleanCell.Create;
begin
  inherited;
  CellType := ctBoolean;
end;

function TcaBooleanCell.GetAsString: String;
begin
  Result := Utils.BooleanToString(FValue);
end;

function TcaBooleanCell.GetValue: Boolean;
begin
  Result := FValue;
end;

procedure TcaBooleanCell.SetAsString(const Value: String);
begin
  SetValue(Utils.StringToBoolean(Value));
end;

procedure TcaBooleanCell.SetValue(const Value: Boolean);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaDateTimeCell                                                            
  //----------------------------------------------------------------------------

constructor TcaDateTimeCell.Create;
begin
  inherited;
  CellType := ctDateTime;
end;

function TcaDateTimeCell.GetAsString: String;
begin
  Result := Utils.DateTimeToString(FValue, '');
end;

function TcaDateTimeCell.GetValue: TDateTime;
begin
  Result := FValue;
end;

procedure TcaDateTimeCell.SetAsString(const Value: String);
begin
  SetValue(Utils.StringToDateTime(Value));
end;

procedure TcaDateTimeCell.SetValue(const Value: TDateTime);
begin
  FValue := Value;
  Changed;
end;

  //----------------------------------------------------------------------------
  // TcaFormulaCell                                                             
  //----------------------------------------------------------------------------

constructor TcaFormulaCell.Create;
begin
  inherited;
  CellType := ctFormula;
end;

function TcaFormulaCell.GetAsString: String;
begin
  Result := FValue;
end;

function TcaFormulaCell.GetValue: String;
begin
  Result := FValue;
end;

procedure TcaFormulaCell.SetAsString(const Value: String);
begin
  SetValue(Value);
end;

procedure TcaFormulaCell.SetValue(const Value: String);
begin
  FValue := Value;
  Changed;
end;

end.
