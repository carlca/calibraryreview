unit caTextStore;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Sysutils,
  Classes,

  // ca units 
  caConsts,
  caUtils,
  caLog,
  caIni;

type

  //```````````````````````````````````````````````````````````````````````````
  // TcaTextItem                                                               
  //```````````````````````````````````````````````````````````````````````````

  TcaTextItem = class(TComponent)
  private
    // Property fields 
    FText: string;
  published
    // Published properties 
    property Text: string read FText write FText;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaTextList                                                               
  //```````````````````````````````````````````````````````````````````````````

  TcaTextList = class(TObject)
  private
    // Private fields 
    FList: TList;
    FOwner: TComponent;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaTextItem;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    // Public methods 
    function FindText(const AName: string): TcaTextItem;
    procedure AddItem(AItem: TcaTextItem);
    procedure AddText(const AName, AText: string);
    procedure Clear;
    // Properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaTextItem read GetItem; default;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaTextStore                                                              
  //```````````````````````````````````````````````````````````````````````````

  TcaTextStore = class(TComponent)
  private
    // Private fields 
    FStrings: TStrings;
    FTextList: TcaTextList;
    // Private methods 
    procedure UpdateFromStrings;
    procedure UpdateStrings;
    procedure UpdateTextList;
    // Property methods 
    function GetStrings: TStrings;
    function GetText(const AName: string): string;
    procedure SetStrings(const Value: TStrings);
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Properties 
    property Strings: TStrings read GetStrings write SetStrings;
    property Text[const AName: string]: string read GetText; default;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaTextList                                                               
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaTextList.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
end;

destructor TcaTextList.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Public methods 

function TcaTextList.FindText(const AName: string): TcaTextItem;
var
  Index: Integer;
  Item: TcaTextItem;
begin
  Result := nil;
  for Index := 0 to Pred(FList.Count) do
    begin
      Item := GetItem(Index);
      if AnsiSameText(Item.Name, AName) then
        begin
          Result := Item;
          Break;
        end;
    end;
end;

procedure TcaTextList.AddItem(AItem: TcaTextItem);
begin
  FList.Add(AItem);
end;

procedure TcaTextList.AddText(const AName, AText: string);
var
  Item: TcaTextItem;
begin
  Item := TcaTextItem.Create(FOwner);
  Item.Name := AName;
  Item.Text := AText;
  FList.Add(Item);
end;

procedure TcaTextList.Clear;
begin
  FList.Clear;
end;

  // Property methods 

function TcaTextList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaTextList.GetItem(Index: Integer): TcaTextItem;
begin
  Result := TcaTextItem(FList[Index]);
end;

  //---------------------------------------------------------------------------
  // TcaTextStore                                                              
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaTextStore.Create(AOwner: TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
  FTextList := TcaTextList.Create(AOwner);
end;

destructor TcaTextStore.Destroy;
begin
  FStrings.Free;
  FreeAndNil(FTextList);
  inherited;
end;

  // Private methods 

procedure TcaTextStore.UpdateFromStrings;
var
  Index: Integer;
  TextName: string;
  TextValue: string;
  TextItem: TcaTextItem;
begin
  for Index := 0 to Pred(FStrings.Count) do
    begin
      TextName := FStrings.Names[Index];
      TextValue := FStrings.Values[TextName];
      TextItem := FTextList.FindText(TextName);
      if not Assigned(TextItem) then
        FTextList.AddText(TextName, TextValue);
    end;
  for Index := 0 to Pred(FTextList.Count) do
    begin
      TextItem := FTextList[Index];
      if FStrings.IndexOfName(TextItem.Name) < 0 then
        TextItem.Free;
    end;
end;

procedure TcaTextStore.UpdateStrings;
var
  Index: Integer;
  TextItem: TcaTextItem;
begin
  FStrings.Clear;
  for Index := 0 to Pred(FTextList.Count) do
    begin
      TextItem := FTextList[Index];
      FStrings.Values[TextItem.Name] := TextItem.Text;
    end;
end;

procedure TcaTextStore.UpdateTextList;
var
  Index: Integer;
  TextItem: TcaTextItem;
begin
  if Assigned(Owner) then
    begin
      FTextList.Clear;
      for Index := 0 to Pred(Owner.ComponentCount) do
        begin
          if Owner.Components[Index] is TcaTextItem then
            begin
              TextItem := TcaTextItem(Owner.Components[Index]);
              FTextList.AddItem(TextItem);              
            end;
        end;
    end;
end;

  // Property methods 

function TcaTextStore.GetStrings: TStrings;
begin
  UpdateTextList;
  UpdateStrings;
  Result := FStrings;
end;

function TcaTextStore.GetText(const AName: string): string;
var
  TextItem: TcaTextItem;
begin
  Result := '';
  UpdateTextList;
  UpdateStrings;
  TextItem := FTextList.FindText(AName);
  if Assigned(TextItem) then
    Result := TextItem.Text;
end;

procedure TcaTextStore.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
  UpdateFromStrings;
end;

initialization
  RegisterClass(TcaTextItem);

finalization
  UnRegisterClass(TcaTextItem);

end.
