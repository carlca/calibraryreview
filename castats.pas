unit caStats;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  Math,
  Contnrs,

  // ca units 
  caClasses,
  caTypes,
  caUtils,
  caLog,
  caStatsBase;

type

  {$IFDEF D5}
  IInvokable = IUnknown;
  {$ENDIF}

  //----------------------------------------------------------------------------
  // IcaRegression                                                              
  //----------------------------------------------------------------------------

  IcaRegression = interface(IInvokable)
  ['{6B636796-F4B8-4F19-9BB2-9EFF853BCA9D}']
    // Interface methods 
    function Correlation: TcaStatsFloat;
    function CoVariance: TcaStatsFloat;
    function N: Integer;
    function ResidualSumSqr: TcaStatsFloat;
    function RSquared: TcaStatsFloat;
    function S2: TcaStatsFloat;
    function Slope: TcaStatsFloat;
    function StdErrSlope: TcaStatsFloat; 
    function StdErrY: TcaStatsFloat;
    function STEYX: TcaStatsFloat;
    function SumOfProducts: TcaStatsFloat;
    function Sxx: TcaStatsFloat;
    function Sxy: TcaStatsFloat;
    function Syy: TcaStatsFloat;
    function T_Ratio: TcaStatsFloat;
    function X: TcaStatsVector;
    function Y: TcaStatsVector;
    procedure AddXY(AX, AY: Double);
    procedure Clear;
  end;

  //----------------------------------------------------------------------------
  // TcaRegression                                                              
  //----------------------------------------------------------------------------

  {$M+}

  TcaRegression = class(TInterfacedObject, IcaRegression, IcaStatsAsString)
  private
    // Private fields 
    FMathUtils: IcaMathUtils;
    FX: TcaStatsVector;
    FY: TcaStatsVector;
    // Private methods 
    function CheckReset: Boolean;
    function Divide(ANumerator, ADenominator: TcaStatsFloat): TcaStatsFloat;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Abstract virtual methods 
    function GetS2: TcaStatsFloat; virtual; abstract;
    function GetSlope: TcaStatsFloat; virtual; abstract;
    function GetStdErrSlope: TcaStatsFloat; virtual; abstract;
    function GetYIntercept: TcaStatsFloat; virtual; abstract;
    // Interface methods - IcaRegression 
    function X: TcaStatsVector;
    function Y: TcaStatsVector;
    procedure AddXY(AX, AY: Double);
    procedure Clear;
    // Interface methods - IcaStatsAsString 
    function AsString: string;
  published
    // Interface methods - IcaRegression 
    function Correlation: TcaStatsFloat;
    function CoVariance: TcaStatsFloat;
    function YIntercept: TcaStatsFloat;
    function N: Integer;
    function ResidualSumSqr: TcaStatsFloat;
    function RSquared: TcaStatsFloat;
    function S2: TcaStatsFloat;
    function Slope: TcaStatsFloat;
    function StdErrSlope: TcaStatsFloat;
    function StdErrY: TcaStatsFloat;
    function STEYX: TcaStatsFloat;
    function SumOfProducts: TcaStatsFloat;
    function Sxx: TcaStatsFloat;
    function Sxy: TcaStatsFloat;
    function Syy: TcaStatsFloat;
    function T_Ratio: TcaStatsFloat;
  end;

  {$M-}

  //----------------------------------------------------------------------------
  // TcaForcedOriginRegression                                                  
  //----------------------------------------------------------------------------

  TcaForcedOriginRegression = class(TcaRegression)
  public
    function GetS2: TcaStatsFloat; override;
    function GetSlope: TcaStatsFloat; override;
    function GetStdErrSlope: TcaStatsFloat; override;
    function GetYIntercept: TcaStatsFloat; override;
  end;

  //----------------------------------------------------------------------------
  // TcaInterceptRegression                                                     
  //----------------------------------------------------------------------------

  TcaInterceptRegression = class(TcaRegression)
  public
    function GetS2: TcaStatsFloat; override;
    function GetSlope: TcaStatsFloat; override;
    function GetStdErrSlope: TcaStatsFloat; override;
    function GetYIntercept: TcaStatsFloat; override;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaRegression                                                              
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaRegression.Create;
begin
  inherited Create;
  FMathUtils := Utils as IcaMathUtils;
  FX := TcaStatsVector.Create;
  FY := TcaStatsVector.Create;
end;

destructor TcaRegression.Destroy;
begin
  FX.Free;
  FY.Free;
  FMathUtils := nil;
  inherited;
end;

  // Interface methods - IcaRegression 

function TcaRegression.Correlation: TcaStatsFloat;   // EXCEL.CORREL(X, Y) 
begin
  Result := CoVariance / Sqrt(FX.Variance * FY.Variance) * N;
end;

function TcaRegression.CoVariance: TcaStatsFloat;    // EXCEL.COVAR(X, Y) 
begin
  Result := Sxy / N;
end;

function TcaRegression.N: Integer;
begin
  Result := Min(FX.N, FY.N);
end;

function TcaRegression.ResidualSumSqr: TcaStatsFloat;
begin
  Result := (1 - RSquared) * FY.SumSqrDev;
end;

function TcaRegression.RSquared: TcaStatsFloat;
begin
  Result := Sqr(Correlation);
end;

function TcaRegression.S2: TcaStatsFloat;
begin
  Result := GetS2;
end;

function TcaRegression.Slope: TcaStatsFloat;
begin
  Result := GetSlope;
end;

function TcaRegression.StdErrSlope: TcaStatsFloat;
begin
  Result := GetStdErrSlope;
end;

function TcaRegression.StdErrY: TcaStatsFloat;
begin
  Result := Sqrt(S2);
end;

function TcaRegression.STEYX: TcaStatsFloat;     // EXCEL.STEYX(X, Y) 
begin
  Result := StdErrY;
end;

function TcaRegression.SumOfProducts: TcaStatsFloat;
begin
  Result := 0;
  if CheckReset then
    while FX.HasMore and FY.HasMore do
      Result := Result + (FX.Next * FY.Next);
end;

function TcaRegression.Sxx: TcaStatsFloat;
begin
  Result := FX.SumOfSquares - (Sqr(FX.Sum) / N);
end;

function TcaRegression.Sxy: TcaStatsFloat;
begin
  Result := SumOfProducts - FX.Sum * FY.Sum / N;
end;

function TcaRegression.Syy: TcaStatsFloat;
begin
  Result := FY.SumOfSquares - (Sqr(FY.Sum) / N);
end;

function TcaRegression.T_Ratio: TcaStatsFloat;
begin
  Result := Divide(Slope, StdErrSlope);
end;

function TcaRegression.X: TcaStatsVector;
begin
  Result := FX;
end;

function TcaRegression.Y: TcaStatsVector;
begin
  Result := FY;
end;

function TcaRegression.YIntercept: TcaStatsFloat;
begin
  Result := GetYIntercept;
end;

procedure TcaRegression.AddXY(AX, AY: Double);
begin
  FX.Add(AX);
  FY.Add(AY);
end;

procedure TcaRegression.Clear;
begin
  FX.Clear;
  FY.Clear;
end;

  // Interface methods - IcaStatsAsString 

function TcaRegression.AsString: string;
var
  FunctionEnumerator: TcaStatsFloatFunctionEnumerator;
begin
  FunctionEnumerator := Auto(TcaStatsFloatFunctionEnumerator.Create(Self)).Instance;
  Result := FunctionEnumerator.AsString;
end;

  // Private methods 

function TcaRegression.CheckReset: Boolean;
begin
  FX.Reset;
  FY.Reset;
  Result := FX.N = FY.N;
end;

function TcaRegression.Divide(ANumerator, ADenominator: TcaStatsFloat): TcaStatsFloat;
begin
  Result := FMathUtils.FloatDiv(ANumerator, ADenominator, 0);
end;

  //----------------------------------------------------------------------------
  // TcaForcedOriginRegression                                                  
  //----------------------------------------------------------------------------

function TcaForcedOriginRegression.GetS2: TcaStatsFloat;
begin
  Result := (FY.SumOfSquares - Sqr(SumOfProducts) / FX.SumOfSquares) / (N - 1);
end;

function TcaForcedOriginRegression.GetSlope: TcaStatsFloat;
begin
  Result := Divide(SumOfProducts, FX.SumOfSquares)
end;

function TcaForcedOriginRegression.GetStdErrSlope: TcaStatsFloat;
begin
  Result := Sqrt(Divide(S2, FX.SumOfSquares));
end;

function TcaForcedOriginRegression.GetYIntercept: TcaStatsFloat;
begin
  Result := 0;
end;

  //----------------------------------------------------------------------------
  // TcaInterceptRegression                                                     
  //----------------------------------------------------------------------------

function TcaInterceptRegression.GetS2: TcaStatsFloat;
begin
  Result := (Syy - (Sqr(Sxy) / Sxx)) / (N - 2);
end;

function TcaInterceptRegression.GetSlope: TcaStatsFloat;
begin
  Result := Divide(Sxy, Sxx);
end;

function TcaInterceptRegression.GetStdErrSlope: TcaStatsFloat;
begin
  Result := Sqrt(Divide(S2, Sxx));
end;

function TcaInterceptRegression.GetYIntercept: TcaStatsFloat;
begin
  Result := FY.Avg - Slope * FX.Avg;
end;

end.
