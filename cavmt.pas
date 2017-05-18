unit caVmt;

{$INCLUDE ca.inc}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Contnrs;

type

  //---------------------------------------------------------------------------
  // Published method record                                                   
  //---------------------------------------------------------------------------

  PVmtMethod = ^TVmtMethod;
  TVmtMethod = packed record
    Size: Word;
    Address: Pointer;
    Name: ShortString;
  end;

  //---------------------------------------------------------------------------
  // Params for stdcall methods                                                
  //---------------------------------------------------------------------------

  PMethodParam = ^TMethodParam;
  TMethodParam = packed record
    TypeInfo: PPTypeInfo;
    Name: ShortString;
  end;

  //---------------------------------------------------------------------------
  // Published method table                                                    
  //---------------------------------------------------------------------------

  PVmtMethodTable = ^TVmtMethodTable;
  TVmtMethodTable = packed record
    Count: Word;
    Methods: array[0..MaxListSize] of Byte;
  end;

  //---------------------------------------------------------------------------
  // Dummy entries - we are not interested in these                            
  //---------------------------------------------------------------------------

  PVmtAutoTable = Pointer;

  PVmtInitTable = Pointer;

  PVmtFieldTable = Pointer;

  PVmtDynMethodTable = Pointer;

  //---------------------------------------------------------------------------
  // Virtual Method Table                                                      
  //---------------------------------------------------------------------------

  PVmt = ^TVmt;
  TVmt = record
    SelfPtr:      TClass;
    IntfTable:    PInterfaceTable;
    AutoTable: PVmtAutoTable;
    InitTable: PVmtInitTable;
    TypeInfo: PTypeInfo;
    FieldTable: PVmtFieldTable;
    MethodTable: PVmtMethodTable;
    DynMethodTable: PVmtDynMethodTable;
    ClassName: PShortString;
    InstanceSize: Cardinal;
    ClassParent: ^TClass;
    SafeCallException: Pointer;
    AfterConstruction: Pointer;
    BeforeDestruction: Pointer;
    Dispatch: Pointer;
    DefaultHandler: Pointer;
    NewInstance: Pointer;
    FreeInstance: Pointer;
    Destroy: Pointer;
  end;

  //---------------------------------------------------------------------------
  // TcaVmtMethodParam                                                         
  //---------------------------------------------------------------------------

  TcaVmtMethodParam = class(TObject)
  private
    // Private fields
    FParam: PMethodParam;
    // Property fields
    FTypeName: String;
    // Property methods
    function GetParamName: ShortString;
    function GetParamClassType: TClass;
    function GetParamTypeInfo: PPTypeInfo;
    function GetParamTypeName: String;
    // Private methods
    function GetClassName: String;
    function GetEnumName: String;
    function GetMethodName: String;
    procedure Update;
  public
    constructor Create(AParam: PMethodParam);
    destructor Destroy; override;
    // Properties
    property ParamName: ShortString read GetParamName;
    property ParamClassType: TClass read GetParamClassType;
    property ParamTypeInfo: PPTypeInfo read GetParamTypeInfo;
    property ParamTypeName: String read GetParamTypeName;
  end;

  //---------------------------------------------------------------------------
  // TcaVmtMethod                                                              
  //---------------------------------------------------------------------------

  TcaVmtMethod = class(TObject)
  private
    // Private fields
    FMethod: PVmtMethod;
    FParams: TObjectList;
    // Property methods
    function GetAddress: Pointer;
    function GetName: ShortString;
    function GetParam(Index: Integer): TcaVmtMethodParam;
    function GetParamCount: Integer;
    function GetSize: Word;
    // Private methods
    function GetBaseSize: Integer;
    procedure Update;
    procedure UpdateParams;
  public
    constructor Create(AMethod: PVmtMethod);
    destructor Destroy; override;
    // Properties
    property Address: Pointer read GetAddress;
    property Name: ShortString read GetName;
    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: TcaVmtMethodParam read GetParam;
    property Size: Word read GetSize;
  end;

  //---------------------------------------------------------------------------
  // IcaVmt                                                                    
  //---------------------------------------------------------------------------

  IcaVmt = interface
  ['{3A7CB947-AFFF-4719-AB54-45D28D8D92C7}']
    // Property methods
    function GetMethodCount: Integer;
    function GetMethod(Index: Integer): TcaVmtMethod;
    // Public methods
    function MethodByName(const AMethodName: String): TcaVmtMethod;
    // Properties
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TcaVmtMethod read GetMethod;
  end;

  //---------------------------------------------------------------------------
  // TcaVmt                                                                    
  //---------------------------------------------------------------------------

  TcaVmt = class(TInterfacedObject, IcaVmt)
  private
    // Private fields
    FClass: TClass;
    FMethods: TObjectList;
    FMethodTable: PVmtMethodTable;
    FVmt: PVmt;
    // Private methods
    procedure Update;
    procedure UpdateMethodTable;
    procedure UpdateMethods;
    procedure UpdateVmt;
    // Property methods
    function GetMethodCount: Integer;
    function GetMethod(Index: Integer): TcaVmtMethod;
  protected
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;
    // Public methods
    function MethodByName(const AMethodName: String): TcaVmtMethod;
    // Properties
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TcaVmtMethod read GetMethod;
  end;

const
  cMethodPadding = 6;

implementation

  //---------------------------------------------------------------------------
  // TcaVmtMethodParam                                                         
  //---------------------------------------------------------------------------

constructor TcaVmtMethodParam.Create(AParam: PMethodParam);
begin
  inherited Create;
  FParam := AParam;
  Update;
end;

destructor TcaVmtMethodParam.Destroy;
begin
  inherited;
end;

  // Private methods 

function TcaVmtMethodParam.GetClassName: String;
begin
  Result := GetParamClassType.ClassName;
end;

function TcaVmtMethodParam.GetEnumName: String;
begin

end;

function TcaVmtMethodParam.GetMethodName: String;
begin

end;

procedure TcaVmtMethodParam.Update;
begin
  case GetParamTypeInfo^.Kind of
    tkInteger:      FTypeName := 'Integer';
    tkChar:         FTypeName := 'Char';
    tkEnumeration:  FTypeName :=  GetEnumName;
    tkFloat:        FTypeName := 'Double';
    tkString:       FTypeName := 'ShortString';
    tkClass:        FTypeName :=  GetClassName;
    tkMethod:       FTypeName :=  GetMethodName;
    tkWChar:        FTypeName := 'WideString';
    tkLString:      FTypeName := 'String';
    tkWString:      FTypeName := 'WideString';
  else
    FTypeName := '';
  end;
end;

  // Property methods 

function TcaVmtMethodParam.GetParamName: ShortString;
begin
  Result := FParam^.Name;
end;

function TcaVmtMethodParam.GetParamClassType: TClass;
var
  PropTypeData: PTypeData;
  PropTypeInfo: TTypeInfo;
begin
  PropTypeInfo := FParam^.TypeInfo^^;
  PropTypeData := GetTypeData(@PropTypeInfo);
  Result := PropTypeData.ClassType;
end;

function TcaVmtMethodParam.GetParamTypeInfo: PPTypeInfo;
begin
  Result := FParam^.TypeInfo;
end;

function TcaVmtMethodParam.GetParamTypeName: string;
begin
  Result := FTypeName;
end;

  //---------------------------------------------------------------------------
  // TcaVmtMethod                                                              
  //---------------------------------------------------------------------------

constructor TcaVmtMethod.Create(AMethod: PVmtMethod);
begin
  inherited Create;
  FMethod := AMethod;
  FParams := TObjectList.Create(True);
  Update;
end;

destructor TcaVmtMethod.Destroy;
begin
  FParams.Free;
  inherited;
end;

  // Private methods 

function TcaVmtMethod.GetBaseSize: Integer;
begin
  Result := SizeOf(Word) + SizeOf(Pointer) + Length(FMethod^.Name) + 1;
end;

procedure TcaVmtMethod.Update;
begin
  UpdateParams;
end;

procedure TcaVmtMethod.UpdateParams;
var
  MethodParam: TcaVmtMethodParam;
  Param: PMethodParam;
  ParamSize: Integer;
  TotalSize: Integer;
begin
  FParams.Clear;
  TotalSize := GetBaseSize + cMethodPadding;
  if GetSize > TotalSize then
    begin
      Param := PMethodParam(PChar(FMethod) + TotalSize);
      // Loop through all the method parameters.
      while GetSize - TotalSize > SizeOf(PTypeInfo) do
        begin
          MethodParam := TcaVmtMethodParam.Create(Param);
          FParams.Add(MethodParam);
          // Increment the pointer past the TypeInfo pointer, the param name,
          // its length, and a trailing #0 byte.
          ParamSize := SizeOf(PTypeInfo) + Length(Param^.Name) + 2;
          Param := PMethodParam(PChar(Param) + ParamSize);
          Inc(TotalSize, ParamSize);
        end;
    end;
end;

  // Property methods 

function TcaVmtMethod.GetAddress: Pointer;
begin
  Result := FMethod^.Address;
end;

function TcaVmtMethod.GetName: ShortString;
begin
  Result := FMethod^.Name;
end;

function TcaVmtMethod.GetParam(Index: Integer): TcaVmtMethodParam;
begin
  Result := TcaVmtMethodParam(FParams[Index]);
end;

function TcaVmtMethod.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function TcaVmtMethod.GetSize: Word;
begin
  Result := FMethod^.Size;
end;

  //---------------------------------------------------------------------------
  // TcaVmt                                                                    
  //---------------------------------------------------------------------------

constructor TcaVmt.Create(AClass: TClass);
begin
  inherited Create;
  FClass := AClass;
  FMethods := TObjectList.Create(True);
  Update;
end;

destructor TcaVmt.Destroy;
begin
  FMethods.Free;
  inherited;
end;

  // Public methods 

function TcaVmt.MethodByName(const AMethodName: string): TcaVmtMethod;
var
  Index: Integer;
  Method: TcaVmtMethod;
begin
  Result := nil;
  for Index := 0 to Pred(GetMethodCount) do
    begin
      Method := GetMethod(Index);
      if Method.Name = AMethodName then
        begin
          Result := Method;
          Break;
        end;
    end;
end;

procedure TcaVmt.Update;
begin
  UpdateVmt;
  UpdateMethodTable;
  UpdateMethods;
end;

procedure TcaVmt.UpdateVmt;
begin
  FVmt := PVmt(FClass);
  Dec(FVmt);
end;

procedure TcaVmt.UpdateMethodTable;
begin
  FMethodTable := FVmt^.MethodTable;
end;

procedure TcaVmt.UpdateMethods;
var
  Index: Integer;
  Method: TcaVmtMethod;
  MethodPtr: PChar;
  MethodTable: PVmtMethodTable;
  AMethodCount: Integer;
  Vmt: PVmt;
  ParentClass: TClass;
begin
  Vmt := FVmt;
  FMethods.Clear;
  while Vmt <> nil do
    begin
      MethodTable := Vmt^.MethodTable;
      if MethodTable <> nil then
        begin
          MethodPtr := @MethodTable.Methods;
          AMethodCount := MethodTable^.Count;
          for Index := 0 to Pred(AMethodCount) do
            begin
              Method := TcaVmtMethod.Create(PVmtMethod(MethodPtr));
              FMethods.Add(Method);
              Inc(MethodPtr, PVmtMethod(MethodPtr)^.Size);
            end;
        end;
      if Vmt^.ClassParent <> nil then
        begin
          ParentClass := Vmt^.ClassParent^;
          if ParentClass <> nil then
            begin
              Vmt := PVmt(ParentClass);
              Dec(Vmt);
            end;
        end
      else
        Vmt := nil;
    end;
end;

  // Property methods 

function TcaVmt.GetMethod(Index: Integer): TcaVmtMethod;
begin
  Result := TcaVmtMethod(FMethods[Index]);
end;

function TcaVmt.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

end.
