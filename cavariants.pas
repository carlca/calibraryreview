unit caVariants;

{$INCLUDE ca.inc}

interface

uses

  Windows,
  SysUtils,
  Classes;

type

 //---------------------------------------------------------------------------
 // TcaVarArray
 //---------------------------------------------------------------------------

  TcaVarArray = class(TObject)
  private
    // Private fields
    FArray: Variant;
    FCapacity: Integer;
    FCount: Integer;
    // Private methods
    procedure Grow;
    procedure ReDim;
    // Property methods
    function GetItem(Index: Integer): Variant;
    procedure SetItem(Index: Integer; const Value: Variant);
  public
    constructor Create;
    // Public methods
    function Add(AItem: Variant): Integer;
    // Properties
    property Count: Integer read FCount;
    property Items[Index: Integer]: Variant read GetItem write SetItem; default;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaVarArray
 //---------------------------------------------------------------------------

constructor TcaVarArray.Create;
begin
  inherited;
  FCapacity := 100;
  FArray := VarArrayCreate([0, 0], varVariant);
  ReDim;
end;

 // Public methods

function TcaVarArray.Add(AItem: Variant): Integer;
begin
  Inc(FCount);
  if FCount = FCapacity then Grow;
  Result := FCount - 1;
  FArray[Result] := AItem;
end;

 // Private methods

procedure TcaVarArray.Grow;
begin
  Inc(FCapacity, 100);
  ReDim;
end;

procedure TcaVarArray.ReDim;
begin
  VarArrayRedim(FArray, FCapacity);
end;

 // Property methods

function TcaVarArray.GetItem(Index: Integer): Variant;
begin
  Result := FArray[Index];
end;

procedure TcaVarArray.SetItem(Index: Integer; const Value: Variant);
begin
  FArray[Index] := Value;
end;

end.
