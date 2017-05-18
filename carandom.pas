unit caRandom;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  SysUtils,
  Windows,

  // ca units
  caClasses,
  caUtils;

const
  cAdditiveRandomSize   = 55;
  cShuffleRandomSize    = 97;
  cCombinedMaxInt1      = MaxInt - 84;
  cCombinedMaxInt2      = MaxInt - 248;

type

  PcaRandomTable = ^TcaRandomTable;
  TcaRandomTable = array[0..0] of Double;

  TcaRandomizeType = (rtSystem, rtMinStandard, rtCombined, rtAdditive, rtShuffle);

 //---------------------------------------------------------------------------
 // IcaRandom
 //---------------------------------------------------------------------------

  IcaRandom = interface
  ['{31542E2B-B33F-4AEB-A273-8DDF444B24BB}']
    // Property methods
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetLowerBound: Double;
    function GetLowerIntBound: Integer;
    function GetRandomizeType: TcaRandomizeType;
    function GetSeed: Integer;
    function GetUpperBound: Double;
    function GetUpperIntBound: Integer;
    procedure SetLowerBound(const Value: Double);
    procedure SetLowerIntBound(const Value: Integer);
    procedure SetSeed(const AValue: Integer);
    procedure SetUpperBound(const Value: Double);
    procedure SetUpperIntBound(const Value: Integer);
    // Properties
    property AsDouble: Double read GetAsDouble;
    property AsInteger: Integer read GetAsInteger;
    property Seed: Integer read GetSeed write SetSeed;
    property LowerBound: Double read GetLowerBound write SetLowerBound;
    property LowerIntBound: Integer read GetLowerIntBound write SetLowerIntBound;
    property RandomizeType: TcaRandomizeType read GetRandomizeType;
    property UpperBound: Double read GetUpperBound write SetUpperBound;
    property UpperIntBound: Integer read GetUpperIntBound write SetUpperIntBound;
  end;

 //---------------------------------------------------------------------------
 // TcaBaseRandom
 //---------------------------------------------------------------------------

  TcaBaseRandom = class(TcaInterfacedPersistent, IcaRandom)
  private
    // Property fields
    FLowerBound: Double;
    FLowerIntBound: Integer;
    FMathUtils: IcaMathUtils;
    FRandomizeType: TcaRandomizeType;
    FSeed: Integer;
    FUpperBound: Double;
    FUpperIntBound: Integer;
    // Property methods
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetLowerBound: Double;
    function GetLowerIntBound: Integer;
    function GetRandomizeType: TcaRandomizeType;
    function GetUpperBound: Double;
    function GetUpperIntBound: Integer;
    procedure SetLowerBound(const Value: Double);
    procedure SetLowerIntBound(const Value: Integer);
    procedure SetUpperBound(const Value: Double);
    procedure SetUpperIntBound(const Value: Integer);
  protected
    // Protected property methods
    function GetSeed: Integer; virtual;
    procedure SetSeed(const AValue: Integer); virtual;
    // Protected static methods
    function GetAdjustedSeed(ASeed: Integer; AMaxInt: Integer = MaxInt): Integer;
    procedure SetRandomizeType(ARandomizeType: TcaRandomizeType);
    // Protected virtual methods
    procedure DoAdjustSeed; virtual;
    procedure DoGetDouble(var ADouble: Double); virtual; abstract;
    // Protected properties
    property Seed: Integer read GetSeed write SetSeed;
  public
    constructor Create(ASeed: Integer = 0); virtual;
    // Properties
    property AsDouble: Double read GetAsDouble;
    property AsInteger: Integer read GetAsInteger;
    property LowerBound: Double read GetLowerBound write SetLowerBound;
    property LowerIntBound: Integer read GetLowerIntBound write SetLowerIntBound;
    property RandomizeType: TcaRandomizeType read GetRandomizeType;
    property UpperBound: Double read GetUpperBound write SetUpperBound;
    property UpperIntBound: Integer read GetUpperIntBound write SetUpperIntBound;
  end;

 //---------------------------------------------------------------------------
 // TcaMinStandardRandom
 //---------------------------------------------------------------------------

  TcaMinStandardRandom = class(TcaBaseRandom)
  protected
    // Protected virtual methods
    procedure DoGetDouble(var ADouble: Double); override;
  public
    // Virtual public methods
    procedure AfterConstruction; override;
    // Promoted properties
    property Seed;
  end;

 //---------------------------------------------------------------------------
 // IcaSystemRandom
 //---------------------------------------------------------------------------

  IcaSystemRandom = interface
  ['{2C2A7FDA-3837-4148-8F39-74BB2DE8A276}']
    // Public methods
    procedure Randomize;
  end;

 //---------------------------------------------------------------------------
 // TcaSystemRandom
 //---------------------------------------------------------------------------

  TcaSystemRandom = class(TcaBaseRandom, IcaSystemRandom)
  protected
    // Protected virtual methods
    procedure DoAdjustSeed; override;
    procedure DoGetDouble(var ADouble: Double); override;
  public
    // Virtual public methods
    procedure AfterConstruction; override;
    // Public methods
    procedure Randomize;
    // Promoted properties
    property Seed;
  end;

 //---------------------------------------------------------------------------
 // IcaCombinedRandom
 //---------------------------------------------------------------------------

  IcaCombinedRandom = interface
  ['{AA27D4CA-F5AB-4003-87B8-41F63E26EE45}']
    // Property methods
    function GetSeed1: Integer;
    function GetSeed2: Integer;
    procedure SetSeed1(const Value: Integer);
    procedure SetSeed2(const Value: Integer);
    // Properties
    property Seed1: Integer read GetSeed1 write SetSeed1;
    property Seed2: Integer read GetSeed2 write SetSeed2;
  end;

 //---------------------------------------------------------------------------
 // TcaCombinedRandom
 //---------------------------------------------------------------------------

  TcaCombinedRandom = class(TcaBaseRandom, IcaCombinedRandom)
  private
    FSeed1: Integer;
    FSeed2: Integer;
    // Property methods
    function GetSeed1: Integer;
    function GetSeed2: Integer;
    procedure SetSeed1(const Value: Integer);
    procedure SetSeed2(const Value: Integer);
  protected
    // Protected virtual methods
    procedure DoGetDouble(var ADouble: Double); override;
  public
    constructor Create(ASeed1: Integer = 0; ASeed2: Integer = 0); reintroduce;
    // Virtual public methods
    procedure AfterConstruction; override;
    // Properties
    property Seed1: Integer read GetSeed1 write SetSeed1;
    property Seed2: Integer read GetSeed2 write SetSeed2;
  end;

 //---------------------------------------------------------------------------
 // TcaDelegatedRandom
 //---------------------------------------------------------------------------

  TcaDelegatedRandom = class(TcaBaseRandom)
  private
    // Property fields
    FStdRandom: TcaMinStandardRandom;
    FTable: PcaRandomTable;
  protected
    // Protected property methods
    function GetSeed: Integer; override;
    procedure SetSeed(const AValue: Integer); override;
    // Protected virtual methods
    function GetTableSize: Integer; virtual; abstract;
    procedure DoInitializeTable; virtual;
    // Protected properties
    property StdRandom: TcaMinStandardRandom read FStdRandom;
    property Table: PcaRandomTable read FTable;
  public
    constructor Create(ASeed: Integer = 0); override;
    destructor Destroy; override;
  end;

 //---------------------------------------------------------------------------
 // TcaAdditiveRandom
 //---------------------------------------------------------------------------

  TcaAdditiveRandom = class(TcaDelegatedRandom)
  private
    // Private fields
    FIndex1: Integer;
    FIndex2: Integer;
  protected
    // Protected virtual methods
    function GetTableSize: Integer; override;
    procedure DoGetDouble(var ADouble: Double); override;
  public
    constructor Create(ASeed: Integer = 0); override;
    // Virtual public methods
    procedure AfterConstruction; override;
    // Promoted properties
    property Seed;
  end;

 //---------------------------------------------------------------------------
 // TcaShuffleRandom
 //---------------------------------------------------------------------------

  TcaShuffleRandom = class(TcaDelegatedRandom)
  private
    FAux: Double;
  protected
    // Protected virtual methods
    function GetTableSize: Integer; override;
    procedure DoGetDouble(var ADouble: Double); override;
    procedure DoInitializeTable; override;
  public
    // Virtual public methods
    procedure AfterConstruction; override;
    // Promoted properties
    property Seed;
  end;

 //---------------------------------------------------------------------------
 // IcaRandomTest
 //---------------------------------------------------------------------------

  TcaRandomTestType = (ttCoupon, ttGap, ttPoker, ttUniformity);

  IcaRandomTest = interface
  ['{53267511-E7B7-4DDF-9354-0DE600604D58}']
    // Property methods
    function GetChiScore: Double;
    function GetCurrentTestType: TcaRandomTestType;
    function GetDegsFreedom: Integer;
    function GetRandom: IcaRandom;
    procedure SetRandom(const Value: IcaRandom);
    // Properties
    property ChiScore: Double read GetChiScore;
    property DegsFreedom: Integer read GetDegsFreedom;
    property CurrentTestType: TcaRandomTestType read GetCurrentTestType;
    property Random: IcaRandom read GetRandom write SetRandom;
  end;

 //---------------------------------------------------------------------------
 // IcaRandomCouponCollectorsTest
 //---------------------------------------------------------------------------

  IcaRandomCouponCollectorsTest = interface(IcaRandomTest)
  ['{66159770-ADAA-4FB7-9130-7F4634961814}']
    // Public methods
    procedure Run;
  end;

 //---------------------------------------------------------------------------
 // IcaRandomGapTest
 //---------------------------------------------------------------------------

  IcaRandomGapTest = interface(IcaRandomTest)
  ['{1818C8EA-3C90-4066-903C-4BEE709AA1E5}']
    // Property methods
    function GetLowerBound: Double;
    function GetUpperBound: Double;
    procedure SetLowerBound(const Value: Double);
    procedure SetUpperBound(const Value: Double);
    // Public methods
    procedure Run;
    // Properties
    property LowerBound: Double read GetLowerBound write SetLowerBound;
    property UpperBound: Double read GetUpperBound write SetUpperBound;
  end;

 //---------------------------------------------------------------------------
 // IcaRandomPokerTest
 //---------------------------------------------------------------------------

  IcaRandomPokerTest = interface(IcaRandomTest)
  ['{44F7F081-199B-4480-AAAC-2F2E88C94512}']
    // Public methods
    procedure Run;
  end;

 //---------------------------------------------------------------------------
 // IcaRandomUniformityTest
 //---------------------------------------------------------------------------

  IcaRandomUniformityTest = interface(IcaRandomTest)
  ['{73025B1E-CCFD-4074-9A9B-4AE3E38A4C19}']
    // Public methods
    procedure Run;
  end;

 //---------------------------------------------------------------------------
 // TcaRandomTest
 //---------------------------------------------------------------------------

  TcaRandomTest = class(TcaInterfacedPersistent, IcaRandomTest,
                                                 IcaRandomCouponCollectorsTest,
                                                 IcaRandomGapTest,
                                                 IcaRandomPokerTest,
                                                 IcaRandomUniformityTest)
  private
    // Private fields
    FMathUtils: IcaMathUtils;
    // Property fields
    FChiScore: Double;
    FCurrentTestType: TcaRandomTestType;
    FDegsFreedom: Integer;
    FLowerBound: Double;
    FRandom: IcaRandom;
    FUpperBound: Double;
    // Private methods
    procedure RunCouponCollectorsTest;
    procedure RunGapTest;
    procedure RunPokerTest;
    procedure RunUniformityTest;
    // Property methods
    function GetChiScore: Double;
    function GetCurrentTestType: TcaRandomTestType;
    function GetDegsFreedom: Integer;
    function GetLowerBound: Double;
    function GetRandom: IcaRandom;
    function GetUpperBound: Double;
    procedure SetLowerBound(const Value: Double);
    procedure SetRandom(const Value: IcaRandom);
    procedure SetUpperBound(const Value: Double);
  public
    // Virtual public methods
    procedure AfterConstruction; override;
    // Method resolution clauses
    procedure IcaRandomCouponCollectorsTest.Run = RunCouponCollectorsTest;
    procedure IcaRandomGapTest.Run = RunGapTest;
    procedure IcaRandomPokerTest.Run = RunPokerTest;
    procedure IcaRandomUniformityTest.Run = RunUniformityTest;
    // Properties
    property ChiScore: Double read GetChiScore;
    property CurrentTestType: TcaRandomTestType read GetCurrentTestType;
    property DegsFreedom: Integer read GetDegsFreedom;
    property LowerBound: Double read GetLowerBound write SetLowerBound;
    property Random: IcaRandom read GetRandom write SetRandom;
    property UpperBound: Double read GetUpperBound write SetUpperBound;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaBaseRandom
 //---------------------------------------------------------------------------

constructor TcaBaseRandom.Create(ASeed: Integer = 0);
begin
  inherited Create;
  FMathUtils := Utils as IcaMathUtils;
  FSeed := ASeed;
  DoAdjustSeed;
end;

 // Protected virtual methods

procedure TcaBaseRandom.DoAdjustSeed;
begin
  FSeed := GetAdjustedSeed(FSeed);
end;

 // Protected static methods

function TcaBaseRandom.GetAdjustedSeed(ASeed: Integer; AMaxInt: Integer = MaxInt): Integer;
begin
  if ASeed > 0 then
    Result := ASeed
  else
    Result := Integer(GetTickCount);
  while Result >= (AMaxInt - 1) do
    Result := Result - AMaxInt;
end;

procedure TcaBaseRandom.SetRandomizeType(ARandomizeType: TcaRandomizeType);
begin
  FRandomizeType := ARandomizeType; 
end;

 // Protected property methods

function TcaBaseRandom.GetSeed: Integer;
begin
  Result := FSeed;
end;

procedure TcaBaseRandom.SetSeed(const AValue: Integer);
begin
  FSeed := AValue;
  DoAdjustSeed;
end;

 // Property methods

function TcaBaseRandom.GetAsDouble: Double;
begin
  DoGetDouble(Result);
  if (FLowerBound <> 0) or (FUpperBound <> 0) then
    Result := (Result * (FUpperBound - FLowerBound)) + FLowerBound;
end;

function TcaBaseRandom.GetAsInteger: Integer;
var
  ADouble: Double;
  LowerInt: Integer;
  UpperInt: Integer;
begin
  DoGetDouble(ADouble);
  if (FLowerIntBound <> 0) or (FUpperIntBound <> 0) then
    begin
      LowerInt := FLowerIntBound + 1;
      UpperInt := FUpperIntBound + 2;
      ADouble := (ADouble * (UpperInt - LowerInt)) + LowerInt;
    end;
  Result := FMathUtils.Trunc(ADouble) + 1;
  if Result > FUpperIntBound then
    Result := FLowerIntBound + Ord(Odd(Result));
  if Result > FUpperIntBound then
    Dec(Result);
end;

function TcaBaseRandom.GetLowerBound: Double;
begin
  Result := FLowerBound;
end;

function TcaBaseRandom.GetLowerIntBound: Integer;
begin
  Result := FLowerIntBound;
end;

function TcaBaseRandom.GetRandomizeType: TcaRandomizeType;
begin
  Result := FRandomizeType;
end;

function TcaBaseRandom.GetUpperBound: Double;
begin
  Result := FUpperBound;
end;

function TcaBaseRandom.GetUpperIntBound: Integer;
begin
  Result := FUpperIntBound;
end;

procedure TcaBaseRandom.SetLowerBound(const Value: Double);
begin
  FLowerBound := Value;
end;

procedure TcaBaseRandom.SetLowerIntBound(const Value: Integer);
begin
  FLowerIntBound := Value;
end;

procedure TcaBaseRandom.SetUpperBound(const Value: Double);
begin
  FUpperBound := Value;
end;

procedure TcaBaseRandom.SetUpperIntBound(const Value: Integer);
begin
  FUpperIntBound := Value;
end;

 //---------------------------------------------------------------------------
 // TcaMinStandardRandom
 //---------------------------------------------------------------------------

 // Virtual public methods

procedure TcaMinStandardRandom.AfterConstruction;
begin
  inherited;
  SetRandomizeType(rtMinStandard);
end;

 // Protected methods
 
procedure TcaMinStandardRandom.DoGetDouble(var ADouble: Double);
const
  cA = 16807;
  cQ = MaxInt div cA;
  cR = MaxInt mod cA;
  cOneOverM = 1 / MaxInt;
var
  K: Integer;
  ASeed: Integer;
begin
  ASeed := Seed;
  K := ASeed div cQ;
  ASeed := (cA * (ASeed - (K * cQ))) - (K * cR);
  if (ASeed < 0) then Inc(ASeed, MaxInt);
  ADouble := ASeed * cOneOverM;
  Seed := ASeed;
end;

 //---------------------------------------------------------------------------
 // TcaSystemRandom
 //---------------------------------------------------------------------------

 // Public methods

procedure TcaSystemRandom.Randomize;
begin
  Seed := Integer(GetTickCount);
end;

 // Virtual public methods

procedure TcaSystemRandom.AfterConstruction;
begin
  inherited;
  SetRandomizeType(rtSystem);
end;

 // Protected virtual methods

procedure TcaSystemRandom.DoAdjustSeed;
begin
  // Bypass ancestor DoAdjustSeed methods
end;

procedure TcaSystemRandom.DoGetDouble(var ADouble: Double);
var
  SavedSeed: Integer;
begin
  SavedSeed := System.RandSeed;
  System.RandSeed := Seed;
  ADouble := System.Random;
  Seed := System.RandSeed;
  System.RandSeed := SavedSeed;
end;

 //---------------------------------------------------------------------------
 // TcaCombinedRandom
 //---------------------------------------------------------------------------

constructor TcaCombinedRandom.Create(ASeed1: Integer = 0; ASeed2: Integer = 0);
begin
  inherited Create(0);
  SetSeed1(ASeed1);
  SetSeed2(ASeed2);
end;

 // Virtual public methods

procedure TcaCombinedRandom.AfterConstruction;
begin
  inherited;
  SetRandomizeType(rtCombined);
end;

 // Protected virtual methods

procedure TcaCombinedRandom.DoGetDouble(var ADouble: Double);
const
  cA1 = 40014;
  cQ1 = cCombinedMaxInt1 div cA1;
  cR1 = cCombinedMaxInt1 mod cA1;
  cOneOverM1 = 1 / cCombinedMaxInt1;
  cA2 = 40692;
  cQ2 = cCombinedMaxInt2 div cA2;
  cR2 = cCombinedMaxInt2 mod cA2;
var
  K: Integer;
  SeedDiff: Integer;
begin
  // Advance first generator
  K := FSeed1 div cQ1;
  FSeed1 := (cA1 * (FSeed1 - (K * cQ1))) - (K * cR1);
  if (FSeed1 < 0) then Inc(FSeed1, cCombinedMaxInt1);
  // Advance second generator
  K := FSeed2 div cQ2;
  FSeed2 := (cA2 * (FSeed2 - (K * cQ2))) - (K * cR2);
  if (FSeed2 < 0) then Inc(FSeed2, cCombinedMaxInt2);
  // Combine the two seeds
  SeedDiff := FSeed1 - FSeed2;
  if (SeedDiff <= 0) then
    SeedDiff := SeedDiff + cCombinedMaxInt1 - 1;
  ADouble := SeedDiff * cOneOverM1;
end;

 // Property methods

function TcaCombinedRandom.GetSeed1: Integer;
begin
  Result := FSeed1;
end;

function TcaCombinedRandom.GetSeed2: Integer;
begin
  Result := FSeed2;
end;

procedure TcaCombinedRandom.SetSeed1(const Value: Integer);
begin
  FSeed1 := GetAdjustedSeed(Value, cCombinedMaxInt1);
end;

procedure TcaCombinedRandom.SetSeed2(const Value: Integer);
begin
  FSeed2 := GetAdjustedSeed(Value, cCombinedMaxInt2);
end;

 //---------------------------------------------------------------------------
 // TcaDelegatedRandom
 //---------------------------------------------------------------------------

constructor TcaDelegatedRandom.Create(ASeed: Integer = 0);
begin
  inherited Create(ASeed);
  GetMem(FTable, GetTableSize * SizeOf(Double));
  FStdRandom := TcaMinStandardRandom.Create(ASeed);
  DoInitializeTable;
end;

destructor TcaDelegatedRandom.Destroy;
begin
  FStdRandom.Free;
  FreeMem(FTable);
  inherited;
end;

procedure TcaDelegatedRandom.DoInitializeTable;
var
  Index: Integer;
begin
  for Index := Pred(GetTableSize) downto 0 do
    FTable^[Index] := StdRandom.AsDouble;
end;

 // Protected property methods

function TcaDelegatedRandom.GetSeed: Integer;
begin
  Result := FStdRandom.Seed;
end;

procedure TcaDelegatedRandom.SetSeed(const AValue: Integer);
begin
  FStdRandom.Seed := AValue;
  DoInitializeTable;
end;

 //---------------------------------------------------------------------------
 // TcaAdditiveRandom
 //---------------------------------------------------------------------------

constructor TcaAdditiveRandom.Create(ASeed: Integer = 0);
begin
  inherited Create(ASeed);
  FIndex1 := Pred(cAdditiveRandomSize);
  FIndex2 := 23;
end;

 // Virtual public methods

procedure TcaAdditiveRandom.AfterConstruction;
begin
  inherited;
  SetRandomizeType(rtAdditive);
end;

 // Protected virtual methods

function TcaAdditiveRandom.GetTableSize: Integer;
begin
  Result := cAdditiveRandomSize;
end;

procedure TcaAdditiveRandom.DoGetDouble(var ADouble: Double);
begin
  ADouble := FTable[FIndex1] + FTable[FIndex2];
  if ADouble >= 1.0 then ADouble := ADouble - 1.0;
  FTable[FIndex1] := ADouble;
  Inc(FIndex1);
  if FIndex1 >= cAdditiveRandomSize then FIndex1 := 0;
  Inc(FIndex2);
  if FIndex2 >= cAdditiveRandomSize then FIndex2 := 0;
end;

 //---------------------------------------------------------------------------
 // TcaShuffleRandom
 //---------------------------------------------------------------------------

 // Virtual public methods

procedure TcaShuffleRandom.AfterConstruction;
begin
  inherited;
  SetRandomizeType(rtShuffle);
end;

 // Protected virtual methods

function TcaShuffleRandom.GetTableSize: Integer;
begin
  Result := cShuffleRandomSize;
end;

procedure TcaShuffleRandom.DoGetDouble(var ADouble: Double);
var
  Index: Integer;
begin
  Index := FMathUtils.Trunc(FAux * cShuffleRandomSize);
  ADouble := FTable[Index];
  FAux := ADouble;
  FTable[Index] := StdRandom.AsDouble;
end;

procedure TcaShuffleRandom.DoInitializeTable;
begin
  inherited;
  FAux := StdRandom.AsDouble;
end;

 //---------------------------------------------------------------------------
 // TcaRandomTest
 //---------------------------------------------------------------------------

const
  cUniformityCount      = 30000;
  cUniformityIntervals  = 100;
  cGapBucketCount       = 10;
  cGapsCount            = 30000;
  cPokerCount           = 30000;
  cCouponCount          = 30000;

 // Public methods

 // The coupon collectors test
 //---------------------------
 // The random numbers are read one by one, converted into a number from
 // 0 to 4. The length of the sequence required to get a complete set of
 // the digits 0..4 is counted, this will vary from 5 upwards. Once a full
 // set is obtained, start over. Bucket the lengths of these sequences.
 // Apply Chi-Squared test to the buckets.

 // Virtual public methods

procedure TcaRandomTest.AfterConstruction;
begin
  inherited;
  FMathUtils := Utils as IcaMathUtils;
end;

 // Private methods

procedure TcaRandomTest.RunCouponCollectorsTest;
var
  Bucket: array [5..20] of Integer;
  ChiSqVal: Double;
  Expected: Double;
  Index: Integer;
  LenSeq: Integer;
  NewVal: Integer;
  NumSeqs: Integer;
  NumVals: Integer;
  Occurs: array [0..4] of Boolean;
  Probs: array [5..20] of Double;
begin
  // Calculate probabilities for each bucket, algorithm from Knuth
  Probs[20] := 1.0;
  for Index := 5 to 19 do
    begin
      Probs[Index] := (120.0 * FMathUtils.Stirling2(Index - 1, 4)) / FMathUtils.IntPower(5.0, Index);
      Probs[20] := Probs[20] - Probs[Index];
    end;
  // an alternative to calculate the last probability value:
  // Probs[last] := 1.0 - ((120.0 * Stirling(Last - 1, 5)) / IntPower(5.0, Last - 1));
  NumSeqs := 0;
  FillChar(Bucket, SizeOf(Bucket), 0);
  while (NumSeqs < cCouponCount) do
    begin
      // keep getting coupons (ie random numbers) until we have collected all five
      LenSeq := 0;
      NumVals := 0;
      FillChar(Occurs, SizeOf(Occurs), 0);
      repeat
        Inc(LenSeq);
        NewVal := FMathUtils.Trunc(FRandom.AsDouble * 5);
        if not Occurs[NewVal] then
          begin
            Occurs[NewVal] := True;
            Inc(NumVals);
          end;
      until NumVals = 5;
      // update the relevant bucket depending on the number of coupons we had to collect
      if LenSeq > 20 then LenSeq := 20;
      Inc(Bucket[LenSeq]);
      Inc(NumSeqs);
    end;
  // Calculate ChiSquare value}
  ChiSqVal := 0.0;
  for Index := 5 to 20 do
    begin
      Expected := Probs[Index] * NumSeqs;
      ChiSqVal := ChiSqVal + (FMathUtils.Sqr(Expected - Bucket[Index]) / Expected);
    end;
  // Return results
  FChiScore := ChiSqVal;
  FDegsFreedom := 15;
  FCurrentTestType := ttCoupon;
end;

 // The gap test
 //-------------
 // Each random number is tested to be in the range Lower..Upper. If it
 // is a value of 1 is assigned, if not 0 is assigned. You'll get a stream
 // of 0's and 1's. The lengths of the runs of 0's are then counted. These
 // lengths are then bucketed, you'll get lengths of 0 upwards. These
 // lengths are the 'gaps' between 1's. Apply Chi-Squared test to the
 // buckets.

procedure TcaRandomTest.RunGapTest;
var
  BoundDiff: Double;
  Bucket: array [0..Pred(cGapBucketCount)] of integer;
  ChiSqVal: Double;
  Expected: Double;
  GapLen: Integer;
  Index: Integer;
  NumGaps: Integer;
  RandValue: Double;
begin
  // Calculate gaps and fill buckets
  FillChar(Bucket, SizeOf(Bucket), 0);
  GapLen := 0;
  NumGaps := 0;
  while (NumGaps < cGapsCount) do
    begin
      RandValue := FRandom.AsDouble;
      if (FLowerBound <= RandValue) and (RandValue < FUpperBound) then
        begin
          if (GapLen >= cGapBucketCount) then
            GapLen := Pred(cGapBucketCount);
          Inc(Bucket[GapLen]);
          Inc(NumGaps);
          GapLen := 0;
        end
      else
        begin
          if (GapLen < cGapBucketCount) then
            Inc(GapLen);
        end;
    end;
  BoundDiff := FUpperBound - FLowerBound;
  ChiSqVal := 0.0;
  // Do all but the last bucket
  for Index := 0 to cGapBucketCount - 2 do
    begin
      Expected := BoundDiff * FMathUtils.IntPower(1 - BoundDiff, Index) * NumGaps;
      ChiSqVal := ChiSqVal + (FMathUtils.Sqr(Expected - Bucket[Index]) / Expected);
    end;
  // Do the last bucket}
  Index := Pred(cGapBucketCount);
  Expected := FMathUtils.IntPower(1 - BoundDiff, Index) * NumGaps;
  ChiSqVal := ChiSqVal + (FMathUtils.Sqr(Expected - Bucket[Index]) / Expected);
  // Return results
  FChiScore := ChiSqVal;
  FDegsFreedom := Pred(cGapBucketCount);
  FCurrentTestType := ttGap;
end;

 // The poker test
 //---------------
 // The random numbers are grouped into 'hands' of 5, and the numbers are
 // converted into a digit from 0..9. The number of different digits in
 // each hand is then counted (1..5), and this result is bucketed. Because
 // the probability of only one digit repeated 5 times is so low, it is
 // grouped into the 2-different-digit category. Apply Chi-Squared test to
 // the buckets.

procedure TcaRandomTest.RunPokerTest;
var
  Accum: Double;
  Bucket: array [0..4] of Integer;
  BucketNumber: Integer;
  ChiSqVal: Double;
  Divisor: Double;
  Expected: Double;
  Flag: array [0..9] of Boolean;
  FlagIndex: Integer;
  Index: Integer;
  NumFives: Integer;
  Probs: array [0..4] of Double;
begin
  // Prepare
  FillChar(Bucket, SizeOf(Bucket), 0);
  NumFives := cPokerCount div 5;
  // Calculate probabilities for each bucket, algorithm from Knuth
  Accum := 1.0;
  Divisor := FMathUtils.IntPower(10.0, 5);
  for Index := 0 to 4 do
    begin
      Accum := Accum * (10.0 - Index);
      Probs[Index] := Accum * FMathUtils.Stirling2(5, Succ(Index)) / Divisor;
    end;
  // For each group of five random numbers, convert all five to a
  // number between 1 and 10, count the number of different digits
  for Index := 1 to NumFives do
    begin
      FillChar(Flag, SizeOf(Flag), 0);
      for FlagIndex := 1 to 5 do
        Flag[FMathUtils.Trunc(FRandom.AsDouble * 10.0)] := True;
      BucketNumber := -1;
      for FlagIndex := 0 to 9 do
        if Flag[FlagIndex] then Inc(BucketNumber);
      Inc(Bucket[BucketNumber]);
    end;
  // Accumulate the first bucket into the second, do calc separately -
  // it'll be the sum of the 'all the same' and 'two different digits' buckets
  Inc(Bucket[1], Bucket[0]);
  Expected := (Probs[0] + Probs[1]) * NumFives;
  ChiSqVal := FMathUtils.Sqr(Expected - Bucket[1]) / Expected;
  // Write the other buckets
  for Index := 2 to 4 do
    begin
      Expected := Probs[Index] * NumFives;
      ChiSqVal := ChiSqVal + (FMathUtils.Sqr(Expected - Bucket[Index]) / Expected);
    end;
  // Return values
  FChiScore := ChiSqVal;
  FDegsFreedom := 3;
  FCurrentTestType := ttPoker;
end;

 // The uniformity test
 //--------------------
 // The random numbers are partitioned into a number of equally sized
 // buckets between 0.0 and 1.0. On the average, each bucket should have
 // the same number of random numbers; ie they should be evenly spread
 // over the range [0.0, 0.1). Apply Chi-Squared test to the buckets.

procedure TcaRandomTest.RunUniformityTest;
var
  Bucket: array [0..Pred(cUniformityIntervals)] of Integer;
  BucketNumber: Integer;
  ChiSqVal: Double;
  Expected: Double;
  Index: Integer;
begin
  // Fill buckets
  FillChar(Bucket, SizeOf(Bucket), 0);
  for Index := 0 to Pred(cUniformityCount) do
    begin
      BucketNumber := FMathUtils.Trunc(FRandom.AsDouble * cUniformityIntervals);
      Inc(Bucket[BucketNumber]);
    end;
  // Calculate chi squared
  Expected := cUniformityCount / cUniformityIntervals;
  ChiSqVal := 0.0;
  for Index := 0 to Pred(cUniformityIntervals) do
    ChiSqVal := ChiSqVal + (FMathUtils.Sqr(Expected - Bucket[Index]) / Expected);
  // Return values
  FChiScore := ChiSqVal;
  FDegsFreedom := Pred(cUniformityIntervals);
  FCurrentTestType := ttUniformity;
end;

 // Property methods

function TcaRandomTest.GetChiScore: Double;
begin
  Result := FChiScore;
end;

function TcaRandomTest.GetCurrentTestType: TcaRandomTestType;
begin
  Result := FCurrentTestType;
end;

function TcaRandomTest.GetDegsFreedom: Integer;
begin
  Result := FDegsFreedom;
end;

function TcaRandomTest.GetLowerBound: Double;
begin
  Result := FLowerBound;
end;

function TcaRandomTest.GetRandom: IcaRandom;
begin
  Result := FRandom;
end;

function TcaRandomTest.GetUpperBound: Double;
begin
  Result := FUpperBound;
end;

procedure TcaRandomTest.SetLowerBound(const Value: Double);
begin
  FLowerBound := Value;
end;

procedure TcaRandomTest.SetRandom(const Value: IcaRandom);
begin
  FRandom := Value;
end;

procedure TcaRandomTest.SetUpperBound(const Value: Double);
begin
  FUpperBound := Value;
end;

end.
