unit caUndoRedo;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  SysUtils,
  Contnrs,
  Math,

  // ca units 
  caClasses,
  caLog;

type

  //---------------------------------------------------------------------------
  // IcaCommandItem                                                            
  //---------------------------------------------------------------------------

  IcaCommandItem = interface
  ['{ADD5ADA5-7145-4735-AA05-15029DB0E6C5}']
    // Property methods 
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    // Interface methods 
    procedure Execute;
    procedure Reverse;
    // Properties 
    property Description: string read GetDescription write SetDescription;
  end;

  //---------------------------------------------------------------------------
  // TcaCommandItem                                                            
  //---------------------------------------------------------------------------

  TcaCommandItem = class(TInterfacedObject, IcaCommandItem)
  private
    // Private fields 
    FDescription: string;
    // Property methods 
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    // Interface methods 
    procedure Execute;
    procedure Reverse;
  protected
    // Protected methods 
    procedure DoExecute; virtual; abstract;
    procedure DoReverse; virtual; abstract;
  end;

  //----------------------------------------------------------------------------
  // IcaCommandList                                                             
  //----------------------------------------------------------------------------

  IcaCommandList = interface
  ['{B20E722E-3736-46B9-A863-54697CC9BFAB}']
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): IcaCommandItem;
    function GetItemIndex: Integer;
    function GetName: string;
    procedure SetName(const Value: string);
    // Interface methods 
    function Add(const ADescription: string): IcaCommandItem;
    function IndexOf(AItem: IcaCommandItem): Integer;
    procedure Clear;
    procedure Redo;
    procedure Undo;
    // Properties 
    property Count: Integer read GetCount;
    property ItemIndex: Integer read GetItemIndex;
    property Items[Index: Integer]: IcaCommandItem read GetItem; default;
    property Name: string read GetName write SetName;
  end;

  //----------------------------------------------------------------------------
  // TcaCommandList                                                             
  //----------------------------------------------------------------------------

  TcaCommandList = class(TInterfacedObject, IcaCommandList)
  private
    // Private fields 
    FItemIndex: Integer;
    FList: IInterfaceList;
    FName: string;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): IcaCommandItem;
    function GetItemIndex: Integer;
    function GetName: string;
    procedure SetName(const Value: string);
    // Interface methods 
    function Add(const ADescription: string): IcaCommandItem;
    function IndexOf(AItem: IcaCommandItem): Integer;
    procedure Clear;
    procedure Redo;
    procedure Undo;
  protected
    // Protected methods 
    function CreateItem: IcaCommandItem; virtual; abstract;
  public
    // Create/Destroy 
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  //---------------------------------------------------------------------------
  // IcaUndoRedo                                                               
  //---------------------------------------------------------------------------

  IcaUndoRedo = interface
  ['{59D77B49-C863-4E3C-AB17-8D01C847F98C}']
    // Property methods 
    function GetRedoItems: TStrings;
    function GetUndoItems: TStrings;
    // Interface methods 
    function Add(const ADescription: string): IcaCommandItem;
    function AddCommandList(const AName: string): IcaCommandList;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure SelectCommandList(const AName: string; ACreateIfNotFound: Boolean);
    procedure Redo;
    procedure RedoGroup(ARedoCount: Integer);
    procedure Undo;
    procedure UndoGroup(AUndoCount: Integer);
    // Properties 
    property RedoItems: TStrings read GetRedoItems;
    property UndoItems: TStrings read GetUndoItems;
  end;

  //---------------------------------------------------------------------------
  // TcaUndoRedo                                                               
  //---------------------------------------------------------------------------

  TcaUndoRedo = class(TInterfacedObject, IcaUndoRedo, IcaLoggable)
  private
    // Private fields 
    FCommandList: IcaCommandList;
    FList: IInterfaceList;
    FRedoItems: TStrings;
    FUndoItems: TStrings;
    // Property methods 
    function GetRedoItems: TStrings;
    function GetUndoItems: TStrings;
    // Interface methods - IcaUndoRedo 
    function Add(const ADescription: string): IcaCommandItem;
    function AddCommandList(const AName: string): IcaCommandList;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure SelectCommandList(const AName: string; ACreateIfNotFound: Boolean);
    procedure Redo;
    procedure RedoGroup(ARedoCount: Integer);
    procedure Undo;
    procedure UndoGroup(AUndoCount: Integer);
    // Interface methods - IcaLoggable 
    procedure SendToLog(const AMsg: String; AClearLog: Boolean = False);
  protected
    // Protected methods 
    function CreateCommandList: IcaCommandList; virtual; abstract;
  public
    // Create/Destroy 
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaCommandItem                                                            
  //---------------------------------------------------------------------------

  // Interface methods 

procedure TcaCommandItem.Execute;
begin
  DoExecute;
end;

procedure TcaCommandItem.Reverse;
begin
  DoReverse;
end;

  // Property methods 

function TcaCommandItem.GetDescription: string;
begin
  Result := FDescription;
end;

procedure TcaCommandItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

  //---------------------------------------------------------------------------
  // TcaCommandItem                                                            
  //---------------------------------------------------------------------------

  // No implementation 

  //----------------------------------------------------------------------------
  // TcaCommandList                                                             
  //----------------------------------------------------------------------------

  // Create/Destroy 

procedure TcaCommandList.AfterConstruction;
begin
  inherited;
  FList := TInterfaceList.Create;
  FItemIndex := -1;
end;

procedure TcaCommandList.BeforeDestruction;
begin
  inherited;
end;

  // Interface methods 

function TcaCommandList.Add(const ADescription: string): IcaCommandItem;
begin
  while FItemIndex >= 0 do
    begin
      FList.Delete(0);
      Dec(FItemIndex);
    end;
  Result := CreateItem;
  Result.Description := ADescription;
  FList.Insert(0, Result);
end;

function TcaCommandList.IndexOf(AItem: IcaCommandItem): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TcaCommandList.Clear;
begin
  FList.Clear;
end;

procedure TcaCommandList.Redo;
var
  CommandItem: IcaCommandItem;
begin
  CommandItem := FList[FItemIndex] as IcaCommandItem;
  CommandItem.Execute;
  if FItemIndex >= 0 then
    Dec(FItemIndex);
end;

procedure TcaCommandList.Undo;
var
  CommandItem: IcaCommandItem;
begin
  if FItemIndex < FList.Count then
    Inc(FItemIndex);
  CommandItem := FList[FItemIndex] as IcaCommandItem;
  CommandItem.Reverse;
end;

  // Property methods 

function TcaCommandList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaCommandList.GetItem(Index: Integer): IcaCommandItem;
begin
  Result := FList[Index] as IcaCommandItem;
end;

function TcaCommandList.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

function TcaCommandList.GetName: string;
begin
  Result := FName;
end;

procedure TcaCommandList.SetName(const Value: string);
begin
  FName := Value;
end;

  //---------------------------------------------------------------------------
  // TcaUndoRedo                                                               
  //---------------------------------------------------------------------------

procedure TcaUndoRedo.AfterConstruction;
begin
  inherited;
  FList := TInterfaceList.Create;
  FRedoItems := TStringList.Create;
  FUndoItems := TStringList.Create;
end;

procedure TcaUndoRedo.BeforeDestruction;
begin
  inherited;
  FRedoItems.Free;
  FUndoItems.Free;
end;

  // Interface methods - IcaUndoRedo 

function TcaUndoRedo.Add(const ADescription: string): IcaCommandItem;
begin
  Result := nil;
  if Assigned(FCommandList) then
    Result := FCommandList.Add(ADescription);
end;

function TcaUndoRedo.AddCommandList(const AName: string): IcaCommandList;
begin
  Result := CreateCommandList;
  Result.Name := AName;
  FList.Add(Result);
end;

function TcaUndoRedo.CanRedo: Boolean;
begin
  Result := False;
  if Assigned(FCommandList) then
    Result := (FCommandList.Count <> 0) and (FCommandList.ItemIndex >= 0);
end;

function TcaUndoRedo.CanUndo: Boolean;
begin
  Result := False;
  if Assigned(FCommandList) then
    Result := (FCommandList.Count <> 0) and (FCommandList.ItemIndex < Pred(FCommandList.Count));
end;

procedure TcaUndoRedo.SelectCommandList(const AName: string; ACreateIfNotFound: Boolean);
var
  Index: Integer;
begin
  FCommandList := nil;
  for Index := 0 to Pred(Flist.Count) do
    if (FList[Index] as IcaCommandList).Name = AName then
      begin
        FCommandList := FList[Index] as IcaCommandList;
        Break;
      end;
  if ACreateIfNotFound and (not Assigned(FCommandList)) then
    FCommandList := AddCommandList(AName);
end;

procedure TcaUndoRedo.Redo;
begin
  if Assigned(FCommandList) then
    FCommandList.Redo;
end;

procedure TcaUndoRedo.RedoGroup(ARedoCount: Integer);
var
  Index: Integer;
begin
  for Index := 1 to ARedoCount do Redo;
end;

procedure TcaUndoRedo.Undo;
begin
  if Assigned(FCommandList) then
    FCommandList.Undo;
end;

procedure TcaUndoRedo.UndoGroup(AUndoCount: Integer);
var
  Index: Integer;
begin
  for Index := 1 to AUndoCount do Undo;
end;

  // Interface methods - IcaLoggable 

procedure TcaUndoRedo.SendToLog(const AMsg: String; AClearLog: Boolean = False);
var
  Index: Integer;
begin
  if AClearLog then Log.Clear;
  if AMsg <> '' then Log.Send(AMsg);
  Log.Send('ItemIndex', FCommandList.ItemIndex);
  Log.Indent;
  for Index := 0 to Pred(FCommandList.Count) do
    begin
      Log.BeginLine;
      Log.Send('Index', Index);
      Log.Send('', FCommandList[Index].Description);
      Log.SendLine;
    end;
  Log.Outdent;
end;

  // Property methods 

function TcaUndoRedo.GetRedoItems: TStrings;
var
  Index: Integer;
begin
  FRedoItems.Clear;
  if Assigned(FCommandList) then
    for Index := FCommandList.ItemIndex downto 0 do
      FRedoItems.AddObject(FCommandList[Index].Description, Pointer(Index));
  Result := FRedoItems;
end;

function TcaUndoRedo.GetUndoItems: TStrings;
var
  Index: Integer;
begin
  FUndoItems.Clear;
  if Assigned(FCommandList) then
    for Index := Succ(FCommandList.ItemIndex) to Pred(FCommandList.Count) do
      FUndoItems.AddObject(FCommandList[Index].Description, Pointer(Index));
  Result := FUndoItems;
end;

end.


