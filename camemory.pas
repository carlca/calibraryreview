unit caMemory;

{$INCLUDE ca.inc}

interface

uses
  Windows, TypInfo, SysUtils, Classes, Forms;

function MemListAsXML: String;
function MemObject(Index: Integer): TObject;
function MemObjectCount: Integer;

implementation

var
  ObjList: array[0..65535] of Pointer;
  FirstFree: Integer = -1;

function GetClassAttributes(PItem: Pointer; var AObjAddr, AObjClass, AObjName, AObjSize, AObjUnit: ShortString): Boolean;
var
  IsClass: Boolean;
  IsOwnedComponent: Boolean;
  Item: TObject;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  Item := TObject(PItem);
  try
    IsClass := PTypeInfo(Item.ClassInfo).Kind = tkClass;
    if IsClass then
      begin
        TypeData := GetTypeData(PTypeInfo(Item.ClassInfo));
        IsOwnedComponent := False;
        if Item is TComponent then
          IsOwnedComponent := TComponent(Item).Owner <> nil;
        if not IsOwnedComponent then
          begin
            PropInfo := GetPropInfo(PTypeInfo(Item.ClassInfo), 'Name');
            AObjAddr := IntToHex(Cardinal(PItem), 8);
            AObjName := 'NoName';
            if PropInfo <> nil then
              AObjName := GetStrProp(Item, PropInfo);
            AObjClass := PTypeInfo(Item.ClassInfo).Name;
            if AObjClass = 'TFont' then
              AObjName := 'NoName';
            AObjSize := IntToStr(TypeData.ClassType.InstanceSize);
            AObjUnit := TypeData.UnitName;
          end
        else
          IsClass := False;
      end;
  except
    IsClass := False;
  end;
  Result := IsClass;
end;

function MemListAsXML: String;
const
  CRLF = #13#10;
var
  Index: Integer;
  ObjAddr, ObjClass, ObjName, ObjSize, ObjUnit: ShortString;
begin
  Result := '<MemList>' + CRLF;
  for Index := 0 to FirstFree - 1 do
    begin
      if GetClassAttributes(ObjList[Index], ObjAddr, ObjClass, ObjName, ObjSize, ObjUnit) then
        begin
          Result := Result + '  <Object Name="' + ObjName + '" Class="' + ObjClass + '">' + CRLF;
          Result := Result + '    <Address>' + ObjAddr + '</Address>' + CRLF;
          Result := Result + '    <Size>' + ObjSize + '</Size>' + CRLF;
          Result := Result + '    <Unit>' + ObjUnit + '</Unit>' + CRLF;
          Result := Result + '  </Object>' + CRLF;
        end;
    end;
  Result := Result + '</MemList>' + CRLF;
end;

procedure WriteMemListToLogFile;
var
  LogFileName: ShortString;
  LogFile: TextFile;
begin
  LogFileName := ExtractFilePath(ParamStr(0));
  AssignFile(LogFile, LogFileName + 'MemList.xml');
  try
    Rewrite(LogFile);
    Write(LogFile, MemListAsXML);
  finally
    CloseFile(LogFile);
  end;
end;

function MemObject(Index: Integer): TObject;
begin
  Result := nil;
  if Index < FirstFree then
    Result := TObject(ObjList[Index]);
end;

function MemObjectCount: Integer;
begin
  Result := FirstFree;
end;

procedure AddPointer(P: Pointer);
begin
  if FirstFree >= Length(ObjList) then
    MessageBox(0, 'ObjectList is full', 'caMemory', MB_OK)
  else
    begin
      Inc(FirstFree);
      ObjList[FirstFree] := P;
    end;
end;

procedure DeletePointer(P: Pointer);
var
  Index: Integer;
begin
  for Index := 0 to FirstFree - 1 do
    begin
      if ObjList[Index] = P then
        begin
          Dec(FirstFree);
          Move(ObjList[Index + 1], ObjList[Index], (FirstFree - Index) * SizeOf(Pointer));
          Break;
        end;
    end;
end;

var
  DefaultMemoryManager: TMemoryManager;

function EnhGetMem(Size: Integer): Pointer;
begin
  Result := DefaultMemoryManager.GetMem(Size);
  AddPointer(Result);
end;

function EnhFreeMem(P: Pointer): Integer;
begin
  Result := DefaultMemoryManager.FreeMem(P);
  DeletePointer(P);
end;

function EnhReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Result := DefaultMemoryManager.ReallocMem(P, Size);
  DeletePointer(P);
  AddPointer(Result);
end;

var
  EnhancedMemoryManager: TMemoryManager =
   (GetMem: EnhGetMem;
    FreeMem: EnhFreeMem;
    ReallocMem: EnhReallocMem);

procedure InitializeMemoryManager;
begin
  GetMemoryManager(DefaultMemoryManager);
  SetMemoryManager(EnhancedMemoryManager);
end;

procedure FinalizeMemoryManager;
begin
  SetMemoryManager(DefaultMemoryManager);
  WriteMemListToLogFile;
end;

initialization
  InitializeMemoryManager;

finalization
  FinalizeMemoryManager;

end.
