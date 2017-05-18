unit caStructures;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,
  Math,
  Contnrs,

  // ca units
  caClasses,
  caUtils,
  caCell,
  caMatrix,
  caVector,
  caTypes;

type

 //---------------------------------------------------------------------------
 // IcaUniquePairItem
 //---------------------------------------------------------------------------

  IcaUniquePairItem = interface
  ['{B69EDC95-151F-43C7-9E31-CB8232F242C7}']
    function GetItem1: Integer;
    function GetItem1AsString: String;
    function GetItem2: Integer;
    function GetItem2AsString: String;
    procedure SetItem1(const Value: Integer);
    procedure SetItem2(const Value: Integer);
    // Properties
    property Item1: Integer read GetItem1 write SetItem1;
    property Item2: Integer read GetItem2 write SetItem2;
    property Item1AsString: String read GetItem1AsString;
    property Item2AsString: String read GetItem2AsString;
  end;

 //---------------------------------------------------------------------------
 // TcaUniquePairItem
 //---------------------------------------------------------------------------

  TcaUniquePairItem = class(TInterfacedObject, IcaUniquePairItem)
  private
    FItem1: Integer;
    FItem2: Integer;
    // Property methods
    function GetItem1: Integer;
    function GetItem1AsString: String;
    function GetItem2: Integer;
    function GetItem2AsString: String;
    procedure SetItem1(const Value: Integer);
    procedure SetItem2(const Value: Integer);
  public
    property Item1: Integer read GetItem1 write SetItem1;
    property Item2: Integer read GetItem2 write SetItem2;
    property Item1AsString: String read GetItem1AsString;
    property Item2AsString: String read GetItem2AsString;
  end;

 //---------------------------------------------------------------------------
 // IcaUniquePairList
 //---------------------------------------------------------------------------

  IcaUniquePairList = interface
  ['{5A7E6C1C-67C2-4A4C-B94A-01F63AF273D5}']
    // Property methods
    function GetItemCount: Integer;
    function GetPair(Index: Integer): TcaUniquePairItem;
    function GetPairCount: Integer;
    function GetReverseItems: Boolean;
    function GetZeroBased: Boolean;
    procedure SetItemCount(const Value: Integer);
    procedure SetReverseItems(const Value: Boolean);
    procedure SetZeroBased(const Value: Boolean);
    // Properties
    property ItemCount: Integer read GetItemCount write SetItemCount;
    property PairCount: Integer read GetPairCount;
    property Pairs[Index: Integer]: TcaUniquePairItem read GetPair; default;
    property ReverseItems: Boolean read GetReverseItems write SetReverseItems;
    property ZeroBased: Boolean read GetZeroBased write SetZeroBased;
  end;

 //---------------------------------------------------------------------------
 // TcaUniquePairList
 //---------------------------------------------------------------------------

  TcaUniquePairList = class(TInterfacedObject, IcaUniquePairList)
  private
    FItemCount: Integer;
    FList: TObjectList;
    FReverseItems: Boolean;
    FZeroBased: Boolean;
    // Property methods
    function GetItemCount: Integer;
    function GetPair(Index: Integer): TcaUniquePairItem;
    function GetPairCount: Integer;
    function GetReverseItems: Boolean;
    function GetZeroBased: Boolean;
    procedure SetItemCount(const Value: Integer);
    procedure SetReverseItems(const Value: Boolean);
    procedure SetZeroBased(const Value: Boolean);
    // Private methods
    procedure BuildPairList;
  public
    constructor Create;
    destructor Destroy; override;
    // Properties
    property ItemCount: Integer read GetItemCount write SetItemCount;
    property PairCount: Integer read GetPairCount;
    property Pairs[Index: Integer]: TcaUniquePairItem read GetPair; default;
    property ReverseItems: Boolean read GetReverseItems write SetReverseItems;
    property ZeroBased: Boolean read GetZeroBased write SetZeroBased;
  end;

 //---------------------------------------------------------------------------
 // IcaCustomIntegerSet
 //---------------------------------------------------------------------------

  IcaCustomIntegerSet = interface
  ['{DB360B2F-D77A-4F04-A891-8D10C28D61BC}']
    // Property methods
    function GetChoose: Integer;
    function GetFrom: Integer;
    procedure SetChoose(const Value: Integer);
    procedure SetFrom(const Value: Integer);
    // Input properties
    property Choose: Integer read GetChoose write SetChoose;
    property From: Integer read GetFrom write SetFrom;
    // Output properties
  end;

 //---------------------------------------------------------------------------
 // TcaCustomIntegerSet
 //---------------------------------------------------------------------------

  TcaCustomIntegerSet = class(TInterfacedObject, IcaCustomIntegerSet)
  private
    FChoose: Integer;
    FFrom: Integer;
    // Property methods
    function GetChoose: Integer;
    function GetFrom: Integer;
    procedure SetChoose(const Value: Integer);
    procedure SetFrom(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    // Input properties
    property Choose: Integer read GetChoose write SetChoose;
    property From: Integer read GetFrom write SetFrom;
    // Output properties
  end;

 //---------------------------------------------------------------------------
 // IcaPermutation
 //---------------------------------------------------------------------------

  IcaPermutation = interface
  ['{D3D6C372-EED2-4533-AF5F-97BDD2955B56}']
  end;

 //---------------------------------------------------------------------------
 // TcaPermutation
 //---------------------------------------------------------------------------

  TcaPermutation = class(TInterfacedObject, IcaCustomIntegerSet, IcaPermutation)
  private
    FBase: IcaCustomIntegerSet;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    // Implementor
    property Base: IcaCustomIntegerSet read FBase implements IcaCustomIntegerSet;
  end;

 //---------------------------------------------------------------------------
 // IcaPermutationList
 //---------------------------------------------------------------------------

  IcaPermutationList = interface
  ['{AA6CFB07-278A-48C1-877C-451647AC6716}']
    // Property methods
    function GetPermutation(Index: Integer): IcaPermutation;
    // Properties
    property Items[Index: Integer]: IcaPermutation read GetPermutation;
  end;

 //---------------------------------------------------------------------------
 // TcaPermutationList
 //---------------------------------------------------------------------------

  TcaPermutationList = class(TInterfacedObject, IcaCustomIntegerSet, IcaPermutationList)
  private
    FBase: IcaCustomIntegerSet;
    FList: TInterfaceList;
    // Property methods
    function GetPermutation(Index: Integer): IcaPermutation;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    // Implementor
    property Base: IcaCustomIntegerSet read FBase implements IcaCustomIntegerSet;
    // Properties
    property Items[Index: Integer]: IcaPermutation read GetPermutation;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaUniquePairItem
 //---------------------------------------------------------------------------

function TcaUniquePairItem.GetItem1: Integer;
begin
  Result := FItem1;
end;

function TcaUniquePairItem.GetItem1AsString: String;
begin
  Result := IntToStr(FItem1);
end;

function TcaUniquePairItem.GetItem2: Integer;
begin
  Result := FItem2;
end;

function TcaUniquePairItem.GetItem2AsString: String;
begin
  Result := IntToStr(FItem2);
end;

procedure TcaUniquePairItem.SetItem1(const Value: Integer);
begin
  FItem1 := Value;
end;

procedure TcaUniquePairItem.SetItem2(const Value: Integer);
begin
  FItem2 := Value;
end;

 //---------------------------------------------------------------------------
 // TcaUniquePairList
 //---------------------------------------------------------------------------

constructor TcaUniquePairList.Create;
begin
  inherited;
  FList := TObjectList.Create(True);
  FZeroBased := True;
  FReverseItems := False;
end;

destructor TcaUniquePairList.Destroy;
begin
  FList.Free;
  inherited;
end;

 // Private methods

procedure TcaUniquePairList.BuildPairList;
var
  Index1: Integer;
  Index2: Integer;
  Index2Start: Integer;  
  Item: TcaUniquePairItem;
  Offset: Integer;
begin
  Offset := 1 - Ord(FZeroBased);
  FList.Clear;
  for Index1 := 0 to FItemCount - 1 do
    begin
      if FReverseItems then
        Index2Start := 0
      else
        Index2Start := Index1 + 1;
      for Index2 := Index2Start to FItemCount - 1 do
        begin
          Item := TcaUniquePairItem.Create;
          Item.Item1 := Index1 + Offset;
          Item.Item2 := Index2 + Offset;
          FList.Add(Item);
        end;
    end;
end;

 // Property methods

function TcaUniquePairList.GetItemCount: Integer;
begin
  Result := FItemCount;
end;

function TcaUniquePairList.GetPair(Index: Integer): TcaUniquePairItem;
begin
  Result := TcaUniquePairItem(FList[Index]);
end;

function TcaUniquePairList.GetPairCount: Integer;
begin
  Result := FList.Count;
end;

function TcaUniquePairList.GetReverseItems: Boolean;
begin
  Result := FReverseItems;
end;

function TcaUniquePairList.GetZeroBased: Boolean;
begin
  Result := FZeroBased;
end;

procedure TcaUniquePairList.SetItemCount(const Value: Integer);
begin
  FItemCount := Value;
  BuildPairList;
end;

procedure TcaUniquePairList.SetReverseItems(const Value: Boolean);
begin
  FReverseItems := Value;
  BuildPairList;
end;

procedure TcaUniquePairList.SetZeroBased(const Value: Boolean);
begin
  FZeroBased := Value;
  BuildPairList;
end;

 //---------------------------------------------------------------------------
 // TcaCustomIntegerSet
 //---------------------------------------------------------------------------

constructor TcaCustomIntegerSet.Create;
begin
  inherited;
end;

destructor TcaCustomIntegerSet.Destroy;
begin
  inherited;
end;

function TcaCustomIntegerSet.GetChoose: Integer;
begin
  Result := FChoose;
end;

function TcaCustomIntegerSet.GetFrom: Integer;
begin
  Result := FFrom;
end;

procedure TcaCustomIntegerSet.SetChoose(const Value: Integer);
begin
  FChoose := Value;
end;

procedure TcaCustomIntegerSet.SetFrom(const Value: Integer);
begin
  FFrom := Value;
end;

 //---------------------------------------------------------------------------
 // TcaPermutation
 //---------------------------------------------------------------------------

constructor TcaPermutation.Create;
begin
  inherited;
  FBase := TcaCustomIntegerSet.Create;
end;

destructor TcaPermutation.Destroy;
begin
  inherited;
end;

 //---------------------------------------------------------------------------
 // TcaPermutationList
 //---------------------------------------------------------------------------

constructor TcaPermutationList.Create;
begin
  inherited;
  FBase := TcaCustomIntegerSet.Create;
  FList := TInterfaceList.Create;
end;

destructor TcaPermutationList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TcaPermutationList.GetPermutation(Index: Integer): IcaPermutation;
begin
  Result := FList[Index] as IcaPermutation;
end;

end.
