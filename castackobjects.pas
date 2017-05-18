unit caStackObjects;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  SysUtils,
  Classes,

  // ca units
  caClasses,
  caUtils;

type

 //---------------------------------------------------------------------------
 // TcaStackInteger
 //---------------------------------------------------------------------------

  TcaStackInteger = class
  private
    // Property fields
    FIndex: Integer;
    FValue: Integer;
    // Property methods
    function GetAsString: String;
    procedure SetAsString(const Value: String);
  protected
    // Protected virtual methods
    procedure DoCreate(AValue : Integer ); virtual;
    procedure DoDestroy; virtual;
  public
    constructor Create(AValue: Integer);
    destructor Destroy; override;
    // Public class methods
    class function CreateObject(AValue: Integer): TcaStackInteger;
    class procedure FreeObject;
    // Properties
    property AsString: String read GetAsString write SetAsString;
    property Index: Integer read FIndex;
    property Value: Integer read FValue write FValue;
  end;

  TcaStackIntegerRec = record
    _VMT: TClass;
    _FValue : Integer;
  end;

var
  caStackIntegerRec: TcaStackIntegerRec;

implementation

 //---------------------------------------------------------------------------
 // TcaStackInteger
 //---------------------------------------------------------------------------

constructor TcaStackInteger.Create(AValue: Integer);
begin
  DoCreate(AValue);
end;

destructor TcaStackInteger.Destroy;
begin
  DoDestroy;
end;

 // Public class methods

class function TcaStackInteger.CreateObject(AValue: Integer): TcaStackInteger;
begin
  InitInstance(@caStackIntegerRec);
  Result := TcaStackInteger(@caStackIntegerRec);
  try
    Result.DoCreate(AValue);
    Result.AfterConstruction;
  except
    Result.DoDestroy;
    Result.CleanupInstance;
    raise;
  end;
end;

class procedure TcaStackInteger.FreeObject;
var
  Obj: TcaStackInteger;
begin
  Obj := TcaStackInteger(@caStackIntegerRec);
  Obj.BeforeDestruction;
  Obj.DoDestroy;
  Obj.CleanupInstance;
end;

 // Protected virtual methods

procedure TcaStackInteger.DoCreate(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TcaStackInteger.DoDestroy;
begin
  // Nada
end;

 // Property methods

function TcaStackInteger.GetAsString: String;
begin
  Result := Utils.IntegerToString(FValue, '');
end;

procedure TcaStackInteger.SetAsString(const Value: String);
begin
  FValue := Utils.StringToInteger(Value);
end;

end.
