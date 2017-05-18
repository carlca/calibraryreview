unit ca8087;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,

  // ca units 
  caClasses,
  caTimer,
  caUtils;

type

  Tca8087ExceptionFlag = (efInvalid, efDenormalized, efZeroDivide, efOverflow, efUnderflow, efPrecision);

  Tca8087ExceptionFlags = set of Tca8087ExceptionFlag;

  Tca8087Precision = (prSingle, prDouble, prReserved, prExtended);

  Tca8087RoundingMode = (rmNearestEven, rmNegInfinity, rmPosInfinity, rmZero);

  //---------------------------------------------------------------------------
  // Tca8087                                                                   
  //---------------------------------------------------------------------------

  Tca8087 = class(TComponent)
  private
    // Private fields 
    FTimer: TcaTimer;
    // Property fields 
    FExceptionFlags: Tca8087ExceptionFlags;
    FPrecision: Tca8087Precision;
    FRoundingMode: Tca8087RoundingMode;
    // Property methods 
    function GetActive: Boolean;
    function GetExceptionFlags: Tca8087ExceptionFlags;
    function GetPrecision: Tca8087Precision;
    function GetRoundingMode: Tca8087RoundingMode;
    function GetStatusAsString: String;
    procedure SetActive(const Value: Boolean);
    procedure SetExceptionFlags(const Value: Tca8087ExceptionFlags);
    procedure SetPrecision(const Value: Tca8087Precision);
    procedure SetRoundingMode(const Value: Tca8087RoundingMode);
    // Event handlers 
    procedure TimerEvent(Sender: TObject);
    // Private methods 
    procedure UpdateStatus;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure Update;
    // Properties 
    property StatusAsString: String read GetStatusAsString;
  published
    // Properties 
    property Active: Boolean read GetActive write SetActive;
    property ExceptionFlags: Tca8087ExceptionFlags read GetExceptionFlags write SetExceptionFlags;
    property Precision: Tca8087Precision read GetPrecision write SetPrecision;
    property RoundingMode: Tca8087RoundingMode read GetRoundingMode write SetRoundingMode;
  end;

implementation

  //---------------------------------------------------------------------------
  // Tca8087                                                                   
  //---------------------------------------------------------------------------

constructor Tca8087.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TcaTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerEvent;
end;

destructor Tca8087.Destroy;
begin
  FTimer.Free;
  inherited;
end;

  // Public methods 

procedure Tca8087.Update;
begin
  UpdateStatus;
end;

  // Private methods 

procedure Tca8087.UpdateStatus;
var
  StatusWord: Word;
  ExcFlag: Tca8087ExceptionFlag;
  ExcMask: Word;
  MathUtils: IcaMathUtils;
  PrecFlags: Word;
  RoundFlags: Word;
begin
  MathUtils := Utils as IcaMathUtils;
  StatusWord := MathUtils.Get8087ControlWord;
  //---------------------------------------------------------------------------
  // EXCEPTION-FLAG MASKS 0-5                                                  
  //                                                                           
  //  0 -> IM  Invalid Operation                                               
  //  1 -> DM  Denormalized Operand                                            
  //  2 -> ZM  Zero Divide                                                     
  //  3 -> OM  Overflow                                                        
  //  4 -> UM  Underflow                                                       
  //  5 -> PM  Precision                                                       
  //---------------------------------------------------------------------------
  FExceptionFlags := [];
  for ExcFlag := Low(Tca8087ExceptionFlag) to High(Tca8087ExceptionFlag) do
    begin
      ExcMask := 1 shl Ord(ExcFlag);
      if (StatusWord and ExcMask) <> 0 then Include(FExceptionFlags, ExcFlag);
    end;
  //---------------------------------------------------------------------------
  // PRECISION CONTROL FIELD                                                   
  //                                                                           
  //  8,9 PC ->  Single Precision   (24-bit) = $00B                            
  //             Double Precision   (53-bit) = $10B                            
  //             Extended Precision (64-bit) = $11B                            
  //             Reserved                                                      
  //---------------------------------------------------------------------------
  PrecFlags := (StatusWord and $300) shr 8;
  FPrecision := Tca8087Precision(PrecFlags);
  //---------------------------------------------------------------------------
  // ROUNDING MODE                                                             
  //                                                                           
  // 10,11 RC ->  Round to nearest even      = $00B                            
  //              Round down toward -ve infinity = $01B                        
  //              Round up toward +ve infinity   = $10B                        
  //              Round toward zero (trunc)  = $11B                            
  //---------------------------------------------------------------------------
  RoundFlags := (StatusWord and $C00) shr 10;
  FRoundingMode := Tca8087RoundingMode(RoundFlags);
end;

  // Event handlers 

procedure Tca8087.TimerEvent(Sender: TObject);
begin
  UpdateStatus;
end;

  // Property methods 

function Tca8087.GetActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

function Tca8087.GetExceptionFlags: Tca8087ExceptionFlags;
begin
  Result := FExceptionFlags;
end;

function Tca8087.GetPrecision: Tca8087Precision;
begin
  Result := FPrecision;
end;

function Tca8087.GetRoundingMode: Tca8087RoundingMode;
begin
  Result := FRoundingMode;
end;

//  Tca8087ExceptionFlag = (efInvalid, efDenormalized, efZeroDivide, efOverflow, efUnderflow, efPrecision);
//
//  Tca8087ExceptionFlags = set of Tca8087ExceptionFlag;
//
//  Tca8087Precision = (prSingle, prDouble, prReserved, prExtended);
//
//  Tca8087RoundingMode = (rmNearestEven, rmNegInfinity, rmPosInfinity, rmZero);
//

function Tca8087.GetStatusAsString: String;
var
  StatusStrings: TStringList;
begin
  UpdateStatus;
  StatusStrings := Auto(TStringList.Create).Instance;
  // Exception Flags 
  StatusStrings.Add('Exception Flags');
  if efInvalid in FExceptionFlags then
    StatusStrings.Add('    Invalid');
  if efDenormalized in FExceptionFlags then
    StatusStrings.Add('    Denormalized');
  if efZeroDivide in FExceptionFlags then
    StatusStrings.Add('    ZeroDivide');
  if efOverflow in FExceptionFlags then
    StatusStrings.Add('    Overflow');
  if efUnderflow in FExceptionFlags then
    StatusStrings.Add('    Underflow');
  if efPrecision in FExceptionFlags then
    StatusStrings.Add('    Precision');
  if StatusStrings.Count = 1 then StatusStrings.Clear;
  // Precision 
  StatusStrings.Add('');
  case FPrecision of
    prSingle:     StatusStrings.Add('Precision = Single');
    prDouble:     StatusStrings.Add('Precision = Double');
    prReserved:   StatusStrings.Add('Precision = Reserved');
    prExtended:   StatusStrings.Add('Precision = Extended');
  end;
  // Rounding Mode 
  StatusStrings.Add('');
  case FRoundingMode of
    rmNearestEven:  StatusStrings.Add('Rounding Mode = Nearest Even');
    rmNegInfinity:  StatusStrings.Add('Rounding Mode = Neg Infinity');
    rmPosInfinity:  StatusStrings.Add('Rounding Mode = Pos Infinity');
    rmZero:         StatusStrings.Add('Rounding Mode = Zero');
  end;
  Result := StatusStrings.Text;
end;

procedure Tca8087.SetActive(const Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

procedure Tca8087.SetExceptionFlags(const Value: Tca8087ExceptionFlags);
begin
  FExceptionFlags := Value;
end;

procedure Tca8087.SetPrecision(const Value: Tca8087Precision);
begin
  FPrecision := Value;
end;

procedure Tca8087.SetRoundingMode(const Value: Tca8087RoundingMode);
begin
  FRoundingMode := Value;
end;

end.


