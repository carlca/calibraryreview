unit caMath;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  Sysutils,

  // ca units
  caClasses;

type

 //---------------------------------------------------------------------------
 // IcaValidNum
 //---------------------------------------------------------------------------

  IcaValidNumType = (vtInteger, vtFloat);

  IcaValidNum = interface
  ['{EF66F544-E5E5-438F-9C4B-5140E94AB183}']
    function GetIsValid: Boolean;
    function GetNumType: IcaValidNumType;
    function GetNumString: String;
    procedure SetNumType(const Value: IcaValidNumType);
    procedure SetNumString(const Value: String);
    property IsValid: Boolean read GetIsValid;
    property NumType: IcaValidNumType read GetNumType write SetNumType;
    property NumString: String read GetNumString write SetNumString;
  end;

 //---------------------------------------------------------------------------
 // TcaValidNum
 //---------------------------------------------------------------------------

  TcaValidNum = class(TInterfacedObject, IcaValidNum)
  private
    FIsValid: Boolean;
    FNumString: String;
    FNumType: IcaValidNumType;
    FValidChars: set of Char;
    FValidated: Boolean;
    function GetIsValid: Boolean;
    function GetNumString: String;
    function GetNumType: IcaValidNumType;
    procedure SetNumString(const Value: String);
    procedure SetNumType(const Value: IcaValidNumType);
    procedure UpdateValidChars;
  public
    constructor Create(ANumType: IcaValidNumType); 
    property IsValid: Boolean read GetIsValid;
    property NumType: IcaValidNumType read GetNumType write SetNumType;
    property NumString: String read GetNumString write SetNumString;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaValidNum
 //---------------------------------------------------------------------------

constructor TcaValidNum.Create(ANumType: IcaValidNumType);
begin
  inherited Create;
  SetNumType(ANumType);
end;

function TcaValidNum.GetIsValid: Boolean;
var
  Index: Integer;
begin
  if FValidated then
    Result := FIsValid
  else
    begin
      Result := True;
      for Index := 1 to Length(FNumString) do
        if not (FNumString[Index] in FValidChars) then
          begin
            FIsValid := False;
            Result := FIsValid;
            FValidated := True;
            Break;
          end;
    end;
end;

function TcaValidNum.GetNumType: IcaValidNumType;
begin
  Result := FNumType;
end;

function TcaValidNum.GetNumString: String;
begin
  Result := FNumString;
end;

procedure TcaValidNum.SetNumType(const Value: IcaValidNumType);
begin
  if Value <> FNumType then
    begin
      FNumType := Value;
      UpdateValidChars;
    end;
end;

procedure TcaValidNum.SetNumString(const Value: String);
begin
  if Value <> FNumString then
    begin
      FNumString := Value;
      FValidated := False;
    end;
end;

procedure TcaValidNum.UpdateValidChars;
begin
  case FNumType of
    vtInteger:  FValidChars := ['+', '-', '0'..'9'];
    vtFloat:    FValidChars := [DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
  end;
end;

end.
