unit caGenetic;

{$INCLUDE ca.inc}

{$DEFINE USE_POINTER_ARRAY}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  SysUtils,
  Math,

  // ca units 
  caClasses,
  caUtils,
  caVector,
  caLog,
  caTypes,
  caRandom;

const
  cOneHundred = 100;

type

  TcaPercentage = 0..cOneHundred;

  TcaFitnessResolution = (frOneDecPlace, frTwoDecPlaces, frThreeDecPlaces, frFourDecPlaces);

  PnnGeneArray = ^TcaGeneArray;
  TcaGeneArray = array[0..0] of Double;

  TcaOrganism = class;

  TcaAddChildOrganismEvent = procedure(Sender: TObject; AChildOrganism: TcaOrganism; var AAccept: Boolean) of object;

  TcaApplyConstraintsEvent = procedure(Sender: TObject; AOrganism: TcaOrganism) of object;

  TcaEvaluateOrganismEvent = procedure(Sender: TObject; AOrganism: TcaOrganism; var AFitness: Double) of object;

  //---------------------------------------------------------------------------
  // TcaChromozome                                                             
  //---------------------------------------------------------------------------

  TcaChromozome = class(TObject)
  private
    {$IFDEF USE_POINTER_ARRAY}
    FGenes: PnnGeneArray;
    {$ELSE}
    FGenes: TcaDoubleVector;
    {$ENDIF}
    FSize: Integer;
    // Property methods 
    function GetGene(Index: Integer): Double;
    function GetSize: Integer;
    procedure SetGene(Index: Integer; const Value: Double);
    procedure SetSize(const Value: Integer);
    // Private methods 
    {$IFDEF USE_POINTER_ARRAY}
    procedure FreeGenesArray;
    {$ENDIF}
    procedure ResizeGenesArray;
  public
    constructor Create;
    destructor Destroy; override;
    // Properties 
    property Genes[Index: Integer]: Double read GetGene write SetGene;
    property Size: Integer read GetSize write SetSize;
  end;

  //---------------------------------------------------------------------------
  // TcaOrganism                                                               
  //---------------------------------------------------------------------------

  TcaPopulation = class;

  TcaOrganismGroup = class;

  TcaOrganism = class(TObject)
  private
    // Private fields 
    FChromozome: TcaChromozome;
    FDateOfBirth: Integer;
    FOrganismGroup: TcaOrganismGroup;
    FPopulation: TcaPopulation;
    FParent1: TcaOrganism;
    FParent2: TcaOrganism;
    // Property fields 
    FEvalParam: Double;
    FEvalResult: Double;
    FFitness: Double;
    // Property methods 
    function GetAsString: String;
    function GetAverageGene: Double;
    function GetChromozomeSize: Integer;
    function GetEvalParam: Double;
    function GetEvalResult: Double;
    function GetFitness: Double;
    function GetGene(Index: Integer): Double;
    function GetOrganismGroup: TcaOrganismGroup;
    procedure SetEvalParam(const Value: Double);
    procedure SetEvalResult(const Value: Double);
    procedure SetFitness(const Value: Double);
    procedure SetGene(Index: Integer; const Value: Double);
    procedure SetOrganismGroup(const Value: TcaOrganismGroup);
    // Private methods 
    function HasParents: Boolean;
    function IsClone: Boolean;
    procedure CopyChromozomeFromOriginal;
    procedure CopyStateFromOriginal;
    procedure CreateChromozomeFromParents;
    procedure CreateRandomChromozome;
  public
    // Public virtual methods 
    constructor Create(APopulation: TcaPopulation; AParent1: TcaOrganism = nil; AParent2: TcaOrganism = nil);
    destructor Destroy; override;
    // Public methods 
    function Clone: TcaOrganism;
    procedure Initialize;
    procedure Mutate;
    procedure ProduceChildren(APartner: TcaOrganism);
    // Properties 
    property AsString: String read GetAsString;
    property AverageGene: Double read GetAverageGene;
    property ChromozomeSize: Integer read GetChromozomeSize;
    property EvalParam: Double read GetEvalParam write SetEvalParam;
    property EvalResult: Double read GetEvalResult write SetEvalResult;
    property Fitness: Double read GetFitness write SetFitness;
    property Genes[Index: Integer]: Double read GetGene write SetGene;
    property OrganismGroup: TcaOrganismGroup read GetOrganismGroup write SetOrganismGroup;
  end;

  //---------------------------------------------------------------------------
  // TcaOrganismGroup                                                          
  //---------------------------------------------------------------------------

  TcaRankedGroups = class;

  TcaOrganismGroup = class(TObject)
  private
    // Private fields 
    FList: TList;
    // Property fields 
    FFitness: Integer;
    FPopulation: TcaPopulation;
    FRankedGroups: TcaRankedGroups;
    FSortDirection: TcaSortDirection;
    // Property methods 
    function GetCount: Integer;
    function GetFitness: Integer;
    function GetItem(Index: Integer): TcaOrganism;
    function GetRankedGroups: TcaRankedGroups;
    function GetSortDirection: TcaSortDirection;
    procedure SetFitness(const Value: Integer);
    procedure SetRankedGroups(const Value: TcaRankedGroups);
  public
    constructor Create(APopulation: TcaPopulation);
    destructor Destroy; override;
    // Public methods 
    function FitnessExists(AFitness: Double): Boolean;
    function RandomChoice: TcaOrganism;
    procedure Add(AOrganism: TcaOrganism);
    procedure DeleteOrganism(AIndex: Integer);
    procedure Sort(ASortDirection: TcaSortDirection);
    // Log methods 
    procedure SendToLog(const AMsg: String; AClearLog: Boolean = False);
    // Properties 
    property Count: Integer read GetCount;
    property Fitness: Integer read GetFitness write SetFitness;
    property Items[Index: Integer]: TcaOrganism read GetItem; default;
    property RankedGroups: TcaRankedGroups read GetRankedGroups write SetRankedGroups;
    property SortDirection: TcaSortDirection read GetSortDirection;
  end;

  //---------------------------------------------------------------------------
  // TcaRankedGroups                                                           
  //---------------------------------------------------------------------------

  TcaRankedGroups = class(TObject)
  private
    // Private fields 
    FList: TList;
    // Property fields 
    FPopulation: TcaPopulation;
    FSortDirection: TcaSortDirection;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaOrganismGroup;
    function GetSortDirection: TcaSortDirection;    
  public
    constructor Create(APopulation: TcaPopulation);
    destructor Destroy; override;
    // Interface methods 
    function FindGroup(AFitness: Integer): TcaOrganismGroup;
    function RandomChoice: TcaOrganismGroup;
    procedure AddGroup(AGroup: TcaOrganismGroup);
    procedure Clear;
    procedure DeleteGroup(AIndex: Integer);
    procedure Sort(ASortDirection: TcaSortDirection; ASortOrganisms: Boolean = False);
    // Properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaOrganismGroup read GetItem; default;
    property SortDirection: TcaSortDirection read GetSortDirection;
  end;

  //---------------------------------------------------------------------------
  // TcaPopulation                                                             
  //---------------------------------------------------------------------------

  TcaPopulation = class(TObject)
  private
    // Private fields 
    FMathUtils: IcaMathUtils;
    FNextIndex: Int64;
    FPopulationCount: Integer;
    FRandom: IcaRandom;
    // Property fields 
    FAllowDuplicates: Boolean;
    FFitnessGroups: TcaRankedGroups;
    FTolerance: Double;    
    // Parameter property fields 
    FAlienExtent: TcaPercentage;
    FAlienRate: TcaPercentage;
    FChildCount: Integer;
    FChromozomeSize: Integer;
    FCrossoverRate: TcaPercentage;
    FFitnessResolution: TcaFitnessResolution;
    FGeneMaxValue: Double;
    FGeneMinValue: Double;
    FGenerationCount: Integer;
    FMaxGenerations: Integer;
    FMaxPopulation: Integer;
    FMutationExtent: TcaPercentage;
    FMutationRate: TcaPercentage;
    FRandSeed: Byte;
    FTargetFitness: Integer;
    // Event property fields 
    FOnAddChild: TcaAddChildOrganismEvent;
    FOnApplyConstraints: TcaApplyConstraintsEvent;
    FOnEvaluate: TcaEvaluateOrganismEvent;
    FOnGenerationCompleted: TNotifyEvent;
    FOnPopulationInitialized: TNotifyEvent;
    // Logging property fields 
    FLogging: Boolean;
    // Private methods 
    function InsertOrganism(AOrganism: TcaOrganism): Boolean;
    procedure AddUnparentedOrganism;
    procedure ApplyNaturalSelection;
    procedure CreateNewGeneration;
    procedure CreateRandomNumberGenerator;
    procedure Evolve;
    procedure EvolveOneGeneration;
    procedure InitializePopulation;
    procedure SetDefaultValues;
    // Property methods 
    function GetAllowDuplicates: Boolean;
    function GetBestOrganism: TcaOrganism;
    function GetFitnessGroups: TcaRankedGroups;
    function GetMathUtils: IcaMathUtils;
    function GetTolerance: Double;
    procedure SetAllowDuplicates(const Value: Boolean);
    procedure SetTolerance(const Value: Double);
    // Parameter property methods 
    function GetAlienExtent: TcaPercentage;
    function GetAlienRate: TcaPercentage;
    function GetChildCount: Integer;
    function GetChromozomeSize: Integer;
    function GetCrossoverRate: TcaPercentage;
    function GetFitnessResolution: TcaFitnessResolution;
    function GetGeneMaxValue: Double;
    function GetGeneMinValue: Double;
    function GetGenerationCount: Integer;
    function GetMaxGenerations: Integer;
    function GetMaxPopulation: Integer;
    function GetMutationExtent: TcaPercentage;
    function GetMutationRate: TcaPercentage;
    function GetRandom: IcaRandom;
    function GetRandSeed: Byte;
    function GetTargetFitness: Integer;
    procedure SetAlienExtent(const Value: TcaPercentage);
    procedure SetAlienRate(const Value: TcaPercentage);
    procedure SetChildCount(const Value: Integer);
    procedure SetChromozomeSize(const Value: Integer);
    procedure SetCrossoverRate(const Value: TcaPercentage);
    procedure SetFitnessResolution(const Value: TcaFitnessResolution);
    procedure SetGeneMaxValue(const Value: Double);
    procedure SetGeneMinValue(const Value: Double);
    procedure SetMaxGenerations(const Value: Integer);
    procedure SetMaxPopulation(const Value: Integer);
    procedure SetMutationExtent(const Value: TcaPercentage);
    procedure SetMutationRate(const Value: TcaPercentage);
    procedure SetRandSeed(const Value: Byte);
    procedure SetTargetFitness(const Value: Integer);
    // Event property methods 
    function GetOnAddChild: TcaAddChildOrganismEvent;
    function GetOnApplyConstraints: TcaApplyConstraintsEvent;
    function GetOnEvaluate: TcaEvaluateOrganismEvent;
    function GetOnGenerationCompleted: TNotifyEvent;
    function GetOnPopulationInitialized: TNotifyEvent;
    procedure SetOnAddChild(const Value: TcaAddChildOrganismEvent);
    procedure SetOnApplyConstraints(const Value: TcaApplyConstraintsEvent);
    procedure SetOnEvaluate(const Value: TcaEvaluateOrganismEvent);
    procedure SetOnGenerationCompleted(const Value: TNotifyEvent);
    procedure SetOnPopulationInitialized(const Value: TNotifyEvent);
    // Log property methods 
    function GetLogging: Boolean;
    procedure SetLogging(const Value: Boolean);
  protected
    // Protected static methods 
    function Evaluate(AOrganism: TcaOrganism): Double;
    procedure AddChild(AChildOrganism: TcaOrganism);
    // Protected virtual methods 
    procedure DoAddChild(AChildOrganism: TcaOrganism; var AAccept: Boolean); virtual;
    procedure DoApplyConstraints(AOrganism: TcaOrganism); virtual;
    procedure DoEvaluate(AOrganism: TcaOrganism; var AFitness: Double); virtual;
    procedure DoGenerationCompleted; virtual;
    procedure DoPopulationInitialized; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // Interface methods 
    function NextIndex: Integer;
    procedure RebuildFitnessGroups;
    procedure Run;
    // Log methods 
    procedure LogWithGenerationCount;
    procedure SendToLog(const AMsg: String; AClearLog: Boolean = False);
    // Properties 
    property FitnessGroups: TcaRankedGroups read GetFitnessGroups;
    property MathUtils: IcaMathUtils read GetMathUtils;
    property Tolerance: Double read GetTolerance write SetTolerance;
    // Parameter properties 
    property AlienExtent: TcaPercentage read GetAlienExtent write SetAlienExtent;
    property AlienRate: TcaPercentage read GetAlienRate write SetAlienRate;
    property AllowDuplicates: Boolean read GetAllowDuplicates write SetAllowDuplicates;
    property BestOrganism: TcaOrganism read GetBestOrganism;
    property ChildCount: Integer read GetChildCount write SetChildCount;
    property ChromozomeSize: Integer read GetChromozomeSize write SetChromozomeSize;
    property CrossoverRate: TcaPercentage read GetCrossoverRate write SetCrossoverRate;
    property FitnessResolution: TcaFitnessResolution read GetFitnessResolution write SetFitnessResolution;
    property GeneMaxValue: Double read GetGeneMaxValue write SetGeneMaxValue;
    property GeneMinValue: Double read GetGeneMinValue write SetGeneMinValue;
    property GenerationCount: Integer read GetGenerationCount;
    property MaxGenerations: Integer read GetMaxGenerations write SetMaxGenerations;
    property MaxPopulation: Integer read GetMaxPopulation write SetMaxPopulation;
    property MutationExtent: TcaPercentage read GetMutationExtent write SetMutationExtent;
    property MutationRate: TcaPercentage read GetMutationRate write SetMutationRate;
    property Random: IcaRandom read GetRandom;
    property RandSeed: Byte read GetRandSeed write SetRandSeed;
    property TargetFitness: Integer read GetTargetFitness write SetTargetFitness;
    // Logging options 
    property Logging: Boolean read GetLogging write SetLogging;
    // Event properties 
    property OnAddChild: TcaAddChildOrganismEvent read GetOnAddChild write SetOnAddChild;
    property OnApplyConstraints: TcaApplyConstraintsEvent read GetOnApplyConstraints write SetOnApplyConstraints;
    property OnEvaluate: TcaEvaluateOrganismEvent read GetOnEvaluate write SetOnEvaluate;
    property OnGenerationCompleted: TNotifyEvent read GetOnGenerationCompleted write SetOnGenerationCompleted;
    property OnPopulationInitialized: TNotifyEvent read GetOnPopulationInitialized write SetOnPopulationInitialized;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaChromozome                                                             
  //---------------------------------------------------------------------------

constructor TcaChromozome.Create;
begin
  inherited;
  {$IFDEF USE_POINTER_ARRAY}
  {$ELSE}
  FGenes := TcaDoubleVector.Create;
  {$ENDIF}
end;

destructor TcaChromozome.Destroy;
begin
  {$IFDEF USE_POINTER_ARRAY}
  FreeGenesArray;
  {$ELSE}
  FGenes.Free;
  {$ENDIF}
  inherited;
end;

  // Private methods 

{$IFDEF USE_POINTER_ARRAY}
procedure TcaChromozome.FreeGenesArray;
begin
  if FGenes <> nil then FreeMem(FGenes);
end;
{$ENDIF}

procedure TcaChromozome.ResizeGenesArray;
begin
  {$IFDEF USE_POINTER_ARRAY}
  FreeGenesArray;
  GetMem(FGenes, FSize * SizeOf(TcaGeneArray));
  {$ELSE}
  FGenes.Clear;
  FGenes.GrowBy(FSize);
  {$ENDIF}
end;

  // Property methods 

function TcaChromozome.GetGene(Index: Integer): Double;
begin
  {$IFDEF USE_POINTER_ARRAY}
  Result := FGenes^[Index];
  {$ELSE}
  Assert(Index < FGenes.Count);
  Result := FGenes[Index];
  {$ENDIF}
end;

function TcaChromozome.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TcaChromozome.SetGene(Index: Integer; const Value: Double);
begin
  {$IFDEF USE_POINTER_ARRAY}
  FGenes^[Index] := Value;
  {$ELSE}
  Assert(Index < FGenes.Count);  
  FGenes[Index] := Value;
  {$ENDIF}
end;

procedure TcaChromozome.SetSize(const Value: Integer);
begin
  if Value <> FSize then
    begin
      FSize := Value;
      ResizeGenesArray;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaOrganism                                                               
  //---------------------------------------------------------------------------

constructor TcaOrganism.Create(APopulation: TcaPopulation; AParent1: TcaOrganism = nil; AParent2: TcaOrganism = nil);
begin
  inherited Create;
  FChromozome := TcaChromozome.Create;
  FPopulation := APopulation;
  FParent1 := AParent1;
  FParent2 := AParent2;
end;

destructor TcaOrganism.Destroy;
begin
  FChromozome.Free;
  inherited;
end;

  // Public methods 

function TcaOrganism.Clone: TcaOrganism;
begin
  Result := TcaOrganism.Create(FPopulation, Self, Self);
  Result.Initialize;
end;

procedure TcaOrganism.Initialize;
begin
  FDateOfBirth := FPopulation.GenerationCount;
  FChromozome.Size := FPopulation.ChromozomeSize;
  if HasParents then
    CreateChromozomeFromParents
  else
    begin
      if IsClone then
        begin
          CopyChromozomeFromOriginal;
          CopyStateFromOriginal;
        end
      else
        CreateRandomChromozome;
    end;
end;

procedure TcaOrganism.Mutate;
var
  GeneIndex: Integer;
  Index: Integer;
  MutationCount: Integer;
  Random: IcaRandom;
begin
  Random := FPopulation.Random;
  Random.LowerIntBound := 0;
  Random.UpperIntBound := cOneHundred;
  if FPopulation.MutationRate > Random.AsInteger then
    begin
      MutationCount := Round(FPopulation.ChromozomeSize * FPopulation.MutationExtent / cOneHundred);
      for Index := 0 to Pred(MutationCount) do
        begin
          // Find random gene to mutate 
          Random.LowerIntBound := 0;
          Random.UpperIntBound := FPopulation.ChromozomeSize - 1;
          GeneIndex := Random.AsInteger;
          // Put random value into selected gene 
          Random.LowerBound := FPopulation.GeneMinValue;
          Random.UpperBound := FPopulation.GeneMaxValue;
          FChromozome.Genes[GeneIndex] := Random.AsDouble;
        end;
    end;
end;

procedure TcaOrganism.ProduceChildren(APartner: TcaOrganism);
var
  Child: TcaOrganism;
  Index: Integer;
begin
  for Index := 0 to Pred(FPopulation.ChildCount) do
    begin
      Child := TcaOrganism.Create(FPopulation, Self, APartner);
      Child.Initialize;
      Child.Mutate;
      FPopulation.AddChild(Child);
    end;
end;

  // Private methods 

function TcaOrganism.HasParents: Boolean;
begin
  Result := (FParent1 <> nil) and (FParent2 <> nil) and (FParent1 <> FParent2);
end;

function TcaOrganism.IsClone: Boolean;
begin
  Result := (FParent1 <> nil) and (FParent2 <> nil) and (FParent1 = FParent2);
end;

procedure TcaOrganism.CopyChromozomeFromOriginal;
var
  GeneIndex: Integer;
begin
  for GeneIndex := 0 to Pred(FChromozome.Size) do
    SetGene(GeneIndex, FParent1.Genes[GeneIndex]);
end;

procedure TcaOrganism.CopyStateFromOriginal;
begin
  FEvalParam := FParent1.EvalParam;
  FEvalResult := FParent1.EvalResult;
  FFitness := FParent1.Fitness;
end;

procedure TcaOrganism.CreateChromozomeFromParents;
var
  Index: Integer;
  Random: IcaRandom;
begin
  Random := FPopulation.Random;
  Random.LowerBound := 0;
  Random.UpperBound := 1;
  for Index := 0 to Pred(FPopulation.ChromozomeSize) do
    begin
      if Random.AsDouble < 0.5 then
        FChromozome.Genes[Index] := FParent1.Genes[Index]
      else
        FChromozome.Genes[Index] := FParent2.Genes[Index];
    end;
end;

procedure TcaOrganism.CreateRandomChromozome;
var
  Index: Integer;
  Random: IcaRandom;
begin
  Random := FPopulation.Random;
  Random.LowerBound := FPopulation.GeneMinValue;
  Random.UpperBound := FPopulation.GeneMaxValue;
  for Index := 0 to Pred(FPopulation.ChromozomeSize) do
    FChromozome.Genes[Index] := Random.AsDouble;
end;

  // Property methods 

function TcaOrganism.GetAsString: String;
begin
  Result := Format('(DOB: %d), (Fitness: %f), (Parent1: %d), (Parent1: %d)',
                   [FDateOfBirth, FFitness, Integer(FParent1), Integer(FParent2)]);
end;

function TcaOrganism.GetAverageGene: Double;
var
  Index: Integer;
begin
  Result := 0;
  if FChromozome.Size > 0 then
    begin
      for Index := 0 to Pred(FChromozome.Size) do
        Result := Result + FChromozome.Genes[Index];
      Result := Result / FChromozome.Size;
    end;
end;

function TcaOrganism.GetChromozomeSize: Integer;
begin
  Result := FPopulation.ChromozomeSize;
end;

function TcaOrganism.GetEvalParam: Double;
begin
  Result := FEvalParam;
end;

function TcaOrganism.GetEvalResult: Double;
begin
  Result := FEvalResult;
end;

function TcaOrganism.GetFitness: Double;
begin
  Result := FFitness;
end;

function TcaOrganism.GetGene(Index: Integer): Double;
begin
  Result := FChromozome.Genes[Index];
end;

function TcaOrganism.GetOrganismGroup: TcaOrganismGroup;
begin
  Result := FOrganismGroup;
end;

procedure TcaOrganism.SetEvalParam(const Value: Double);
begin
  FEvalParam := Value;
end;

procedure TcaOrganism.SetEvalResult(const Value: Double);
begin
  FEvalResult := Value;
end;

procedure TcaOrganism.SetFitness(const Value: Double);
begin
  FFitness := Value;
end;

procedure TcaOrganism.SetGene(Index: Integer; const Value: Double);
begin
  FChromozome.Genes[Index] := Value;
end;

procedure TcaOrganism.SetOrganismGroup(const Value: TcaOrganismGroup);
begin
  FOrganismGroup := Value;
end;

  //---------------------------------------------------------------------------
  // TcaOrganismGroup                                                          
  //---------------------------------------------------------------------------

constructor TcaOrganismGroup.Create(APopulation: TcaPopulation);
begin
  inherited Create;
  FPopulation := APopulation;
  FList := TList.Create;
end;

destructor TcaOrganismGroup.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to Pred(FList.Count) do
    TObject(FList[Index]).Free;
  FList.Free;
  FPopulation := nil;
  inherited;
end;

  // Public methods 

function TcaOrganismGroup.FitnessExists(AFitness: Double): Boolean;
var
  Index: Integer;
  Organism: TcaOrganism;
begin
  // TODO: Use binary search 
  Result := False;
  for Index := 0 to Pred(GetCount) do
    begin
      Organism := GetItem(Index);
      if FPopulation.MathUtils.IsEq(Organism.Fitness, AFitness) then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function TcaOrganismGroup.RandomChoice: TcaOrganism;
var
  Random: IcaRandom;
begin
  Random := FPopulation.Random;
  Random.LowerIntBound := 0;
  Random.UpperIntBound := Pred(FList.Count);
  Result := TcaOrganism(FList[Random.AsInteger]);
end;

procedure TcaOrganismGroup.Add(AOrganism: TcaOrganism);
begin
  FList.Add(AOrganism);
  AOrganism.OrganismGroup := Self;
end;

procedure TcaOrganismGroup.DeleteOrganism(AIndex: Integer);
begin
  TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

function TcaOrganismGroup_Sort(Item1, Item2: Pointer): Integer;
var
  Diff: Double;
  Organism1: TcaOrganism;
  Organism2: TcaOrganism;
  OrganismGroup: TcaOrganismGroup;
begin
  Diff := 0;
  Organism1 := TcaOrganism(Item1);
  Organism2 := TcaOrganism(Item2);
  OrganismGroup := Organism1.OrganismGroup;
  case OrganismGroup.SortDirection of
    sdAscending:    Diff := Organism1.Fitness - Organism2.Fitness;
    sdDescending:   Diff := Organism2.Fitness - Organism1.Fitness;
  end;
  if Diff < 0 then Result := -1 else if Diff > 0 then Result := 1 else Result := 0;
end;

procedure TcaOrganismGroup.Sort(ASortDirection: TcaSortDirection);
begin
  FSortDirection := ASortDirection;
  FList.Sort(TcaOrganismGroup_Sort);
end;

  // Log methods 

procedure TcaOrganismGroup.SendToLog(const AMsg: String; AClearLog: Boolean = False);
var
  Index: Integer;
  Organism: TcaOrganism;
begin
  if AClearLog then Log.Clear;
  if AMsg <> '' then Log.Send(AMsg);
  for Index := 0 to Pred(GetCount) do
    begin
      Organism := GetItem(Index);
      Log.Send('Organism', Index);
      Log.Send('Fitness', Organism.Fitness);
    end;
  Log.Send(' ');
end;

  // Property methods 

function TcaOrganismGroup.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaOrganismGroup.GetFitness: Integer;
begin
  Result := FFitness;
end;

function TcaOrganismGroup.GetItem(Index: Integer): TcaOrganism;
begin
  Result := TcaOrganism(FList[Index]);
end;

function TcaOrganismGroup.GetRankedGroups: TcaRankedGroups;
begin
  Result := FRankedGroups;
end;

function TcaOrganismGroup.GetSortDirection: TcaSortDirection;
begin
  Result := FSortDirection;
end;

procedure TcaOrganismGroup.SetFitness(const Value: Integer);
begin
  FFitness := Value;
end;

procedure TcaOrganismGroup.SetRankedGroups(const Value: TcaRankedGroups);
begin
  FRankedGroups := Value;
end;

  //---------------------------------------------------------------------------
  // TcaRankedGroups                                                           
  //---------------------------------------------------------------------------

constructor TcaRankedGroups.Create(APopulation: TcaPopulation);
begin
  inherited Create;
  FPopulation := APopulation;
  FList := TList.Create;
end;

destructor TcaRankedGroups.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to Pred(FList.Count) do
    TObject(FList[Index]).Free;
  FList.Free;
  FPopulation := nil;
  inherited;
end;

  // Interface methods 

function TcaRankedGroups.FindGroup(AFitness: Integer): TcaOrganismGroup;
var
  ACompare: Integer;
  AHigh: Integer;
  AIndex: Integer;
  ALow: Integer;
  Item: TcaOrganismGroup;
begin
  Sort(sdAscending);
  Result := nil;
  ALow := 0;
  AHigh := Pred(FList.Count);
  while ALow <= AHigh do
    begin
      AIndex := (ALow + AHigh) shr 1;
      Item := TcaOrganismGroup(FList.List^[AIndex]);
      ACompare := Item.Fitness - AFitness;
      if ACompare < 0 then
        ALow := AIndex + 1
      else
        begin
          AHigh := AIndex - 1;
          if ACompare = 0 then
            begin
              Result := Item;
              ALow := AIndex;
            end;
        end;
    end;
end;

function TcaRankedGroups.RandomChoice: TcaOrganismGroup;
var
  Random: IcaRandom;
begin
  Random := FPopulation.Random;
  Random.LowerIntBound := 0;
  Random.UpperIntBound := Pred(FList.Count);
  Result := TcaOrganismGroup(FList[Random.AsInteger]);
end;

procedure TcaRankedGroups.AddGroup(AGroup: TcaOrganismGroup);
begin
  AGroup.RankedGroups := Self;
  FList.Add(AGroup);
end;

procedure TcaRankedGroups.Clear;
begin
  while GetCount > 0 do
    DeleteGroup(0);
end;

procedure TcaRankedGroups.DeleteGroup(AIndex: Integer);
begin
  TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

function TcaRankedGroups_Sort(Item1, Item2: Pointer): Integer;
var
  Group1: TcaOrganismGroup;
  Group2: TcaOrganismGroup;
  RankedGroups: TcaRankedGroups;
begin
  Result := 0;
  Group1 := TcaOrganismGroup(Item1);
  Group2 := TcaOrganismGroup(Item2);
  RankedGroups := Group1.RankedGroups;
  case RankedGroups.SortDirection of
    sdAscending:    Result := Group1.Fitness - Group2.Fitness;
    sdDescending:   Result := Group2.Fitness - Group1.Fitness;
  end;
end;

procedure TcaRankedGroups.Sort(ASortDirection: TcaSortDirection; ASortOrganisms: Boolean = False);
var
  Group: TcaOrganismGroup;
  GroupIndex: Integer;
begin
  FSortDirection := ASortDirection;
  FList.Sort(TcaRankedGroups_Sort);
  if ASortOrganisms then
    begin
      for GroupIndex := 0 to Pred(GetCount) do
        begin
          Group := GetItem(GroupIndex);
          Group.Sort(sdAscending);
        end;
    end;
end;

  // Property methods 

function TcaRankedGroups.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaRankedGroups.GetItem(Index: Integer): TcaOrganismGroup;
begin
  Result := TcaOrganismGroup(FList[Index]);
end;

function TcaRankedGroups.GetSortDirection: TcaSortDirection;
begin
  Result := FSortDirection;
end;

  //---------------------------------------------------------------------------
  // TcaPopulation                                                             
  //---------------------------------------------------------------------------

  // Public virtual methods 

procedure TcaPopulation.AfterConstruction;
begin
  inherited;
  FLogging := False;
  FMathUtils := Utils as IcaMathUtils;
  SetDefaultValues;
  CreateRandomNumberGenerator;
  FFitnessGroups := TcaRankedGroups.Create(Self);
end;

procedure TcaPopulation.BeforeDestruction;
begin
  inherited;
  FFitnessGroups.Free;
end;

  // Public methods 

function TcaPopulation.NextIndex: Integer;
begin
  Inc(FNextIndex);
  Result := FNextIndex;
end;

procedure TcaPopulation.RebuildFitnessGroups;
var
  SavedOrganisms: TList;
  Group: TcaOrganismGroup;
  GroupIndex: Integer;
  Organism: TcaOrganism;
  OrganismIndex: Integer;
begin
  SavedOrganisms := TList.Create;
  try
    // Clone every organism and save in temp list 
    for GroupIndex := 0 to Pred(FFitnessGroups.Count) do
      begin
        Group := FFitnessGroups[GroupIndex];
        for OrganismIndex := 0 to Pred(Group.Count) do
          begin
            Organism := Group[OrganismIndex];
            SavedOrganisms.Add(Organism.Clone);
          end;
      end;
    // Empty out everything 
    FitnessGroups.Clear;
    FPopulationCount := 0;
    // Insert saved organisms back into groups 
    for OrganismIndex := 0 to Pred(SavedOrganisms.Count) do
      InsertOrganism(TcaOrganism(SavedOrganisms[OrganismIndex]));
  finally
    SavedOrganisms.Free;
  end;
end;

procedure TcaPopulation.Run;
begin
  FFitnessGroups.Clear;
  FGenerationCount := 0;
  FPopulationCount := 0;
  InitializePopulation;
  DoPopulationInitialized;
  Evolve;
end;

  // Log methods 

procedure TcaPopulation.LogWithGenerationCount;
begin
  if FLogging then SendToLog(Format('Generation %d', [FGenerationCount]), True);
end;

procedure TcaPopulation.SendToLog(const AMsg: String; AClearLog: Boolean = False);
var
  Group: TcaOrganismGroup;
  GroupIndex: Integer;
  OrgIndex: Integer;
  Organism: TcaOrganism;
begin
  if AClearLog then Log.Clear;
  Log.Freeze;
  if AMsg <> '' then Log.Send(AMsg);
  for GroupIndex := 0 to Pred(FFitnessGroups.Count) do
    begin
      Group := FFitnessGroups[GroupIndex];
      Log.Send('Group.Fitness', Group.Fitness);
      for OrgIndex := 0 to Pred(Group.Count) do
        begin
          Organism := Group[OrgIndex];
          Log.Send('        Organism.Fitness', Organism.Fitness);
          Log.Send('        Organism.EvalParam', Organism.EvalParam);
          Log.Send('        Organism.EvalResult', Organism.EvalResult);
          Log.Send(' ')
        end;
    end;
  Log.Send(' ');
  Log.Unfreeze;  
end;

  // Protected virtual methods 

procedure TcaPopulation.DoAddChild(AChildOrganism: TcaOrganism; var AAccept: Boolean);
begin
  if Assigned(FOnAddChild) then FOnAddChild(Self, AChildOrganism, AAccept);
end;

procedure TcaPopulation.DoApplyConstraints(AOrganism: TcaOrganism);
begin
  if Assigned(FOnApplyConstraints) then FOnApplyConstraints(Self, AOrganism);
end;

procedure TcaPopulation.DoEvaluate(AOrganism: TcaOrganism; var AFitness: Double);
begin
  if Assigned(FOnEvaluate) then FOnEvaluate(Self, AOrganism, AFitness);
end;

procedure TcaPopulation.DoGenerationCompleted;
begin
  if Assigned(FOnGenerationCompleted) then FOnGenerationCompleted(Self);    
end;

procedure TcaPopulation.DoPopulationInitialized;
begin
  if Assigned(FOnPopulationInitialized) then FOnPopulationInitialized(Self);    
end;

  // Protected static methods 

function TcaPopulation.Evaluate(AOrganism: TcaOrganism): Double;
begin
  DoEvaluate(AOrganism, Result);
end;

procedure TcaPopulation.AddChild(AChildOrganism: TcaOrganism);
var
  Accept: Boolean;
begin
  Accept := True;
  DoAddChild(AChildOrganism, Accept);
  if Accept then InsertOrganism(AChildOrganism);
end;

  // Private methods 

procedure TcaPopulation.AddUnparentedOrganism;
var
  Inserted: Boolean;
  Organism: TcaOrganism;
begin
  Inserted := False;
  while not Inserted do
    begin
      Organism := TcaOrganism.Create(Self);
      Organism.Initialize;
      Inserted := InsertOrganism(Organism);
      if not Inserted then Organism.Free;
    end;
end;

function TcaPopulation.InsertOrganism(AOrganism: TcaOrganism): Boolean;
var
  ActualFitness: Double;
  Group: TcaOrganismGroup;
  OKToAdd: Boolean;
  RoundedFitness: Integer;
begin
  Result := False;
  // If fitness is already set, the organism is a clone and 
  // no evaluation is needed 
  if AOrganism.Fitness <> 0 then
    ActualFitness := AOrganism.Fitness
  else
    begin
      DoApplyConstraints(AOrganism);
      ActualFitness := 0;
      DoEvaluate(AOrganism, ActualFitness);
    end;
  if ActualFitness < MaxInt then
    begin
      AOrganism.Fitness := ActualFitness;
      RoundedFitness := FMathUtils.Trunc(ActualFitness);
      Group := FFitnessGroups.FindGroup(RoundedFitness);
      if Group = nil then
        begin
          Group := TcaOrganismGroup.Create(Self);
          Group.Fitness := RoundedFitness;
          FFitnessGroups.AddGroup(Group);
        end;
      OKToAdd := True;
      if not FAllowDuplicates then
        OKToAdd := not Group.FitnessExists(AOrganism.Fitness);
      if OKToAdd then
        begin
          Group.Add(AOrganism);
          Inc(FPopulationCount);
        end
      else
        begin
          if Group.Count = 0 then
            Group.Free;
        end;
      Result := OKToAdd;
    end;
end;

procedure TcaPopulation.ApplyNaturalSelection;
var
  WorstGroup: TcaOrganismGroup;
  PopulationSurplus: Integer;
begin
  FFitnessGroups.Sort(sdDescending);
  while FPopulationCount > FMaxPopulation do
    begin
      WorstGroup := FFitnessGroups[0];
      // Delete a whole group as long as some population will be left 
      if (FPopulationCount - WorstGroup.Count) >= FMaxPopulation then
        begin
          // Adjust overall population count 
          Dec(FPopulationCount, WorstGroup.Count);
          FFitnessGroups.DeleteGroup(0);
        end
      else
        begin
          // If deleting the whole group would send the population negative, delete within group 
          PopulationSurplus := FPopulationCount - FMaxPopulation;
          // Reduce the population count 
          Dec(FPopulationCount, PopulationSurplus);
          // Sort organisms within group by Actual Fitness (IOW, *not* the rounded fitness) 
          WorstGroup.Sort(sdDescending);
          // Delete the weakest organisms until population surplus is zero 
          while PopulationSurplus > 0 do
            begin
              WorstGroup.DeleteOrganism(0);
              Dec(PopulationSurplus);
            end;
        end;
    end;
end;

procedure TcaPopulation.CreateNewGeneration;
var
  Group: TcaOrganismGroup;
  GroupIndex: Integer;
  Organism: TcaOrganism;
  OrganismIndex: Integer;
  Partner: TcaOrganism;
  CrossoverRateTarget: TcaPercentage;
  RandomGroup: TcaOrganismGroup;
begin
  // For each organism group in the population 
  for GroupIndex := 0 to Pred(FFitnessGroups.Count) do
    begin
      Group := FFitnessGroups[GroupIndex];
      for OrganismIndex := 0 to Pred(Group.Count) do
        begin
          // for each organism in each organism group 
          Organism := Group[OrganismIndex];
          // CrossoverRate determines how often crossover occurs 
          FRandom.LowerIntBound := 0;
          FRandom.UpperIntBound := cOneHundred;
          CrossoverRateTarget := FRandom.AsInteger;
          if FCrossoverRate > CrossoverRateTarget then
            begin
              RandomGroup := FFitnessGroups.RandomChoice;
              Partner := RandomGroup.RandomChoice;
              while Organism = Partner do
                begin
                  RandomGroup := FFitnessGroups.RandomChoice;
                  Partner := RandomGroup.RandomChoice;
                end;
              Organism.ProduceChildren(Partner);
            end;
        end;
    end;
end;

procedure TcaPopulation.CreateRandomNumberGenerator;
begin
  FRandom := TcaAdditiveRandom.Create(FRandSeed);
end;

procedure TcaPopulation.Evolve;
var
  Evolving: Boolean;
begin
  FMathUtils.ZeroTolerance := FTolerance;
  // Iterate over generations until a stopping condition is met 
  Evolving := True;
  while Evolving do
    begin
      EvolveOneGeneration;
      FFitnessGroups.Sort(sdAscending, True);
      LogWithGenerationCount;
      DoGenerationCompleted;
      // If max gen count matters and generations reached 
      if (FMaxGenerations > 0) and (FGenerationCount = FMaxGenerations) then
        Evolving := False;
      // If fitness goal has been reached 
      if FFitnessGroups[0].Fitness <= FTargetFitness then
        Evolving := False;
    end;
  FMathUtils.RestoreDefaultZeroTolerance;
end;

procedure TcaPopulation.EvolveOneGeneration;
var
  AlienRateTarget: TcaPercentage;
  AlienCount: Integer;
  Index: Integer;
begin
  Inc(FGenerationCount);
  FRandom.LowerIntBound := 0;
  FRandom.UpperIntBound := cOneHundred;
  // AlienRate is the percentage of generations in which new 
  // organisms are randomly added to the population 
  AlienRateTarget := FRandom.AsInteger;
  if FAlienRate > AlienRateTarget then
    begin
      // AlienExtent is the number of new random organisms relative to the current population count 
      AlienCount := FMathUtils.EquiRound(FPopulationCount * FAlienExtent / cOneHundred);
      for Index := 0 to Pred(AlienCount) do
        AddUnparentedOrganism;
    end;
  // Create new generation of organisms based on parents 
  CreateNewGeneration;
  // Weed out poor organisms 
  ApplyNaturalSelection;
end;

procedure TcaPopulation.InitializePopulation;
var
  Index: Integer;
begin
  for Index := 0 to Pred(FMaxPopulation) do
    AddUnparentedOrganism;
  FFitnessGroups.Sort(sdAscending, True);
  LogWithGenerationCount;
end;

procedure TcaPopulation.SetDefaultValues;
begin
  FAlienExtent := 8;
  FAlienRate := 10;
  FChildCount := 1;
  FChromozomeSize := 32;
  FCrossoverRate := 32;
  FGeneMaxValue := High(Integer);
  FGeneMinValue := Low(Integer);
  FMaxGenerations := 128;
  FMaxPopulation := 256;
  FMutationExtent := 1;
  FMutationRate := 8;
  FRandSeed := 1;
  FTargetFitness := 0;
end;

  // Property methods 

function TcaPopulation.GetAllowDuplicates: Boolean;
begin
  Result := FAllowDuplicates;
end;

function TcaPopulation.GetBestOrganism: TcaOrganism;
var
  FitnessGroups: TcaRankedGroups;
  BestGroup: TcaOrganismGroup;
begin
  Result := nil;
  FitnessGroups := GetFitnessGroups;
  if FitnessGroups.Count > 0 then
    begin
      FitnessGroups.Sort(sdAscending);
      BestGroup := FitnessGroups[0];
      if BestGroup.Count > 0 then
        Result := BestGroup[0];
    end;  
end;

function TcaPopulation.GetFitnessGroups: TcaRankedGroups;
begin
  Result := FFitnessGroups;
end;

function TcaPopulation.GetMathUtils: IcaMathUtils;
begin
  Result := FMathUtils;
end;

function TcaPopulation.GetTolerance: Double;
begin
  Result := FTolerance;
end;

procedure TcaPopulation.SetAllowDuplicates(const Value: Boolean);
begin
  FAllowDuplicates := Value;
end;

procedure TcaPopulation.SetTolerance(const Value: Double);
begin
  FTolerance := Value;
end;

  // Parameter property methods 

function TcaPopulation.GetAlienExtent: TcaPercentage;
begin
  Result := FAlienExtent;
end;

function TcaPopulation.GetAlienRate: TcaPercentage;
begin
  Result := FAlienRate;
end;

function TcaPopulation.GetChildCount: Integer;
begin
  Result := FChildCount;
end;

function TcaPopulation.GetChromozomeSize: Integer;
begin
  Result := FChromozomeSize;
end;

function TcaPopulation.GetCrossoverRate: TcaPercentage;
begin
  Result := FCrossoverRate;
end;

function TcaPopulation.GetFitnessResolution: TcaFitnessResolution;
begin
  Result := FFitnessResolution;
end;

function TcaPopulation.GetGeneMaxValue: Double;
begin
  Result := FGeneMaxValue;
end;

function TcaPopulation.GetGeneMinValue: Double;
begin
  Result := FGeneMinValue;
end;

function TcaPopulation.GetGenerationCount: Integer;
begin
  Result := FGenerationCount;
end;

function TcaPopulation.GetMaxGenerations: Integer;
begin
  Result := FMaxGenerations;
end;

function TcaPopulation.GetMaxPopulation: Integer;
begin
  Result := FMaxPopulation;
end;

function TcaPopulation.GetMutationExtent: TcaPercentage;
begin
  Result := FMutationExtent;
end;

function TcaPopulation.GetMutationRate: TcaPercentage;
begin
  Result := FMutationRate;
end;

function TcaPopulation.GetRandSeed: Byte;
begin
  Result := FRandSeed;
end;

function TcaPopulation.GetRandom: IcaRandom;
begin
  Result := FRandom;
end;

function TcaPopulation.GetTargetFitness: Integer;
begin
  Result := FTargetFitness;
end;

procedure TcaPopulation.SetAlienExtent(const Value: TcaPercentage);
begin
  FAlienExtent := Value;
end;

procedure TcaPopulation.SetAlienRate(const Value: TcaPercentage);
begin
  FAlienRate := Value;
end;

procedure TcaPopulation.SetChildCount(const Value: Integer);
begin
  FChildCount := Value;
end;

procedure TcaPopulation.SetChromozomeSize(const Value: Integer);
begin
  FChromozomeSize := Value;
end;

procedure TcaPopulation.SetCrossoverRate(const Value: TcaPercentage);
begin
  FCrossoverRate := Value;
end;

procedure TcaPopulation.SetFitnessResolution(const Value: TcaFitnessResolution);
begin
  FFitnessResolution := Value;
end;

procedure TcaPopulation.SetGeneMaxValue(const Value: Double);
begin
  FGeneMaxValue := Value;
end;

procedure TcaPopulation.SetGeneMinValue(const Value: Double);
begin
  FGeneMinValue := Value;
end;

procedure TcaPopulation.SetMaxGenerations(const Value: Integer);
begin
  FMaxGenerations := Value;
end;

procedure TcaPopulation.SetMaxPopulation(const Value: Integer);
begin
  FMaxPopulation := Value;
end;

procedure TcaPopulation.SetMutationExtent(const Value: TcaPercentage);
begin
  FMutationExtent := Value;
end;

procedure TcaPopulation.SetMutationRate(const Value: TcaPercentage);
begin
  FMutationRate := Value;
end;

procedure TcaPopulation.SetRandSeed(const Value: Byte);
begin
  FRandSeed := Value;
end;

procedure TcaPopulation.SetTargetFitness(const Value: Integer);
begin
  FTargetFitness := Value;
end;

  // Event property methods 

function TcaPopulation.GetOnAddChild: TcaAddChildOrganismEvent;
begin
  Result := FOnAddChild;
end;

function TcaPopulation.GetOnApplyConstraints: TcaApplyConstraintsEvent;
begin
  Result := FOnApplyConstraints;
end;

function TcaPopulation.GetOnEvaluate: TcaEvaluateOrganismEvent;
begin
  Result := FOnEvaluate;
end;

function TcaPopulation.GetOnGenerationCompleted: TNotifyEvent;
begin
  Result := FOnGenerationCompleted;
end;

function TcaPopulation.GetOnPopulationInitialized: TNotifyEvent;
begin
  Result := FOnPopulationInitialized;
end;

procedure TcaPopulation.SetOnAddChild(const Value: TcaAddChildOrganismEvent);
begin
  FOnAddChild := Value;
end;

procedure TcaPopulation.SetOnApplyConstraints(const Value: TcaApplyConstraintsEvent);
begin
  FOnApplyConstraints := Value;
end;

procedure TcaPopulation.SetOnEvaluate(const Value: TcaEvaluateOrganismEvent);
begin
  FOnEvaluate := Value;
end;

procedure TcaPopulation.SetOnGenerationCompleted(const Value: TNotifyEvent);
begin
  FOnGenerationCompleted := Value;
end;

procedure TcaPopulation.SetOnPopulationInitialized(const Value: TNotifyEvent);
begin
  FOnPopulationInitialized := Value;
end;

  // Log property methods 

function TcaPopulation.GetLogging: Boolean;
begin
  Result := FLogging;
end;

procedure TcaPopulation.SetLogging(const Value: Boolean);
begin
  FLogging := Value;
end;

end.


