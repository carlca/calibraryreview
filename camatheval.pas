unit caMathEval;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  SysUtils,
  Math,

  // ca units
  caClasses,
  caTypes,
  caVector,
  caUtils;

const
  cComma = ',';
  cDoubleComma = ',,';

type

  EcaMathEvalException = class(Exception);

  TcaOperandVariableEvent = procedure(Sender: TObject; const AOperand: String; var AValue: Extended) of object;

 //---------------------------------------------------------------------------
 // IcaMathTokens
 //---------------------------------------------------------------------------

  IcaMathTokens = interface
  ['{474F2872-F6BE-4BF6-84E2-94129C1111D5}']
    // Property methods
    function GetExpression: String;
    function GetTokens: TStrings;
    procedure SetExpression(const Value: String);
    // Properties
    property Expression: String read GetExpression write SetExpression;
    property Tokens: TStrings read GetTokens;
  end;

 //---------------------------------------------------------------------------
 // TcaMathTokens
 //---------------------------------------------------------------------------

  TcaMathTokens = class(TInterfacedObject, IcaMathTokens)
  private
    FExpression: String;
    FTokens: TStrings;
    // Property methods
    function GetExpression: String;
    function GetTokens: TStrings;
    procedure SetExpression(const Value: String);
    // Private methods
    procedure UpdateTokens;
  public
    constructor Create;
    destructor Destroy; override;
    // Properties
    property Expression: String read GetExpression write SetExpression;
    property Tokens: TStrings read GetTokens;
  end;

 //---------------------------------------------------------------------------
 // IcaInFixToPostfix
 //---------------------------------------------------------------------------

  IcaInFixToPostfix = interface
  ['{F84D2A15-9F76-415F-AE91-EAF176F80A95}']
    // Property methods
    function GetPostfixTokens: TStrings;
    function GetInfixTokens: TStrings;
    procedure SetInfixTokens(const Value: TStrings);
    // Properties
    property PostfixTokens: TStrings read GetPostfixTokens;
    property InfixTokens: TStrings read GetInfixTokens write SetInfixTokens;
  end;

 //---------------------------------------------------------------------------
 // TcaInfixToPostfix
 //---------------------------------------------------------------------------

  TcaInfixToPostfix = class(TInterfacedObject, IcaInFixToPostfix)
  private
    FPostfixTokens: TStrings;
    FInfixTokens: TStrings;
    // Property methods
    function GetPostfixTokens: TStrings;
    function GetInfixTokens: TStrings;
    procedure SetInfixTokens(const Value: TStrings);
    // Private methods
    procedure UpdatePostfixTokens;
  public
    constructor Create;
    destructor Destroy; override;
    // Properties
    property PostfixTokens: TStrings read GetPostfixTokens;
    property InfixTokens: TStrings read GetInfixTokens write SetInfixTokens;
  end;

 //---------------------------------------------------------------------------
 // IcaPostfixEvaluator
 //---------------------------------------------------------------------------

  IcaPostfixEvaluator = interface
  ['{44D75E82-81D5-457F-A82A-A46C9FCC0173}']
    // Property methods
    function GetPostfixTokens: TStrings;
    function GetTrigUnits: TcaTrigUnits;
    function GetValue: Extended;
    procedure SetPostfixTokens(const Value: TStrings);
    procedure SetTrigUnits(const Value: TcaTrigUnits);
    // Event property methods
    function GetOnGetOperandVariable: TcaOperandVariableEvent;
    procedure SetOnGetOperandVariable(const Value: TcaOperandVariableEvent);
    // Properties
    property PostfixTokens: TStrings read GetPostfixTokens write SetPostfixTokens;
    property TrigUnits: TcaTrigUnits read GetTrigUnits write SetTrigUnits;
    property Value: Extended read GetValue;
    // Event properties
    property OnGetOperandVariable: TcaOperandVariableEvent read GetOnGetOperandVariable write SetOnGetOperandVariable;
  end;

 //---------------------------------------------------------------------------
 // TcaPostfixEvaluator
 //---------------------------------------------------------------------------

  TcaPostfixEvaluator = class(TInterfacedObject, IcaPostfixEvaluator)
  private
    FPostfixTokens: TStrings;
    FTrigUnits: TcaTrigUnits;
    FValue: Extended;
    FOnGetOperandVariable: TcaOperandVariableEvent;
    // Property methods
    function GetPostfixTokens: TStrings;
    function GetTrigUnits: TcaTrigUnits;
    function GetValue: Extended;
    procedure SetPostfixTokens(const Value: TStrings);
    procedure SetTrigUnits(const Value: TcaTrigUnits);
    // Event property methods
    function GetOnGetOperandVariable: TcaOperandVariableEvent;
    procedure SetOnGetOperandVariable(const Value: TcaOperandVariableEvent);
    // Private methods
    function GetOperandValue(const AOperand: String): Extended;
    function GetTrigOperand(AOperand: Extended): Extended;
    procedure Evaluate;
  protected
    // Protected methods
    procedure DoGetOperandVariable(const AOperand: String; var AValue: Extended); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // Properties
    property PostfixTokens: TStrings read GetPostfixTokens write SetPostfixTokens;
    property TrigUnits: TcaTrigUnits read GetTrigUnits write SetTrigUnits;
    property Value: Extended read GetValue;
    // Event properties
    property OnGetOperandVariable: TcaOperandVariableEvent read GetOnGetOperandVariable write SetOnGetOperandVariable;
  end;

 //---------------------------------------------------------------------------
 // IcaMathEvaluator
 //---------------------------------------------------------------------------

  IcaMathEvaluator = interface
  ['{BD670DED-CBF3-448C-81B6-C10568F7BA00}']
    // Property methods
    function GetExpression: String;
    function GetTrigUnits: TcaTrigUnits;
    function GetValue: Extended;
    function GetValueAsString: String;
    procedure SetExpression(const Value: String);
    procedure SetTrigUnits(const Value: TcaTrigUnits);
    // Event property methods
    function GetOnGetOperandVariable: TcaOperandVariableEvent;
    procedure SetOnGetOperandVariable(const Value: TcaOperandVariableEvent);
    // Properties
    property Expression: String read GetExpression write SetExpression;
    property TrigUnits: TcaTrigUnits read GetTrigUnits write SetTrigUnits;
    property Value: Extended read GetValue;
    property ValueAsString: String read GetValueAsString;
    // Event properties
    property OnGetOperandVariable: TcaOperandVariableEvent read GetOnGetOperandVariable write SetOnGetOperandVariable;
  end;

 //---------------------------------------------------------------------------
 // TcaMathEvaluator
 //---------------------------------------------------------------------------

  TcaMathEvaluator = class(TInterfacedObject, IcaMathEvaluator)
  private
    FExpression: String;
    FInfixToPostfix: IcaInFixToPostfix;
    FMathTokens: IcaMathTokens;
    FOnGetOperandVariable: TcaOperandVariableEvent;
    FPostfixEvaluator: IcaPostfixEvaluator;
    FValue: Extended;
    // Property methods
    function GetExpression: String;
    function GetTrigUnits: TcaTrigUnits;
    function GetValue: Extended;
    function GetValueAsString: String;
    procedure SetExpression(const Value: String);
    procedure SetTrigUnits(const Value: TcaTrigUnits);
    // Event property methods
    function GetOnGetOperandVariable: TcaOperandVariableEvent;
    procedure SetOnGetOperandVariable(const Value: TcaOperandVariableEvent);
    // Private methods
    procedure UpdateValue;
    // Event handler
    procedure OperandVariableEvent(Sender: TObject; const AOperand: String; var AValue: Extended);
  protected
    // Protected methods
    procedure DoGetOperandVariable(const AOperand: String; var AValue: Extended); virtual;
  public
    constructor Create;
    property Expression: String read GetExpression write SetExpression;
    property TrigUnits: TcaTrigUnits read GetTrigUnits write SetTrigUnits;
    property Value: Extended read GetValue;
    property ValueAsString: String read GetValueAsString;
    // Event properties
    property OnGetOperandVariable: TcaOperandVariableEvent read GetOnGetOperandVariable write SetOnGetOperandVariable;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaMathTokens
 //---------------------------------------------------------------------------

constructor TcaMathTokens.Create;
begin
  inherited;
  FTokens := TStringList.Create;
end;

destructor TcaMathTokens.Destroy;
begin
  FTokens.Free;
  inherited;
end;

function TcaMathTokens.GetExpression: String;
begin
  Result := FExpression;
end;

function TcaMathTokens.GetTokens: TStrings;
begin
  Result := FTokens;
end;

procedure TcaMathTokens.SetExpression(const Value: String);
begin
  FExpression := Value;
  UpdateTokens;
end;

procedure TcaMathTokens.UpdateTokens;
var
  Index: Integer;
  Ch: Char;
  ChPrev: Char;
  TokensStr: String;
  AExpression: String;
  PreDelim: String;
  PostDelim: String;
  ZeroInserts: IcaIntegerVector;
begin
  AExpression := LowerCase(FExpression);
  Utils.StripChar(#32, AExpression);
  // Check for any '-' operators used for negation or '+' used as a prefix
  ZeroInserts := TcaIntegerVector.Create;
  for Index := 1 to Length(AExpression) do
    begin
      Ch := AExpression[Index];
      if (Ch = '-') or (Ch = '+') then
        begin
          // If the '-' is the first token or if it is not preceeded
          // by an operand, then insert a '0' operand before the '-'
          if Index = 1 then
            ZeroInserts.Add(Index)
          else
            begin
              ChPrev := AExpression[Index - 1];
              if Utils.IsOperator(ChPrev) or Utils.IsBracket(ChPrev) then
                ZeroInserts.Add(Index);
            end;
        end;
    end;
  for Index := ZeroInserts.Count - 1 downto 0 do
    Insert('0', AExpression, ZeroInserts[Index]);
  // Break expression into comma separated tokens
  TokensStr := '';
  for Index := 1 to Length(AExpression) do
    begin
      PreDelim := '';
      PostDelim := '';
      Ch := AExpression[Index];
      if Utils.IsOperator(Ch) or Utils.IsBracket(Ch) then
        begin
          PreDelim := cComma;
          PostDelim := cComma;
        end;
      TokensStr := TokensStr + PreDelim + Ch + PostDelim;
    end;
  if TokensStr[1] = cComma then
    Utils.DeleteFromStart(TokensStr, 1);
  Utils.Replace(TokensStr, cDoubleComma, cComma);
  FTokens.CommaText := TokensStr;
end;

 //---------------------------------------------------------------------------
 // TcaInfixToPostfix
 //---------------------------------------------------------------------------

constructor TcaInfixToPostfix.Create;
begin
  inherited;
  FPostfixTokens := TStringList.Create;
  FInfixTokens := TStringList.Create;
end;

destructor TcaInfixToPostfix.Destroy;
begin
  FPostfixTokens.Free;
  FInfixTokens.Free;
  inherited;
end;

function TcaInfixToPostfix.GetPostfixTokens: TStrings;
begin
  Result := FPostfixTokens;
end;

function TcaInfixToPostfix.GetInfixTokens: TStrings;
begin
  Result := FInfixTokens;
end;

procedure TcaInfixToPostfix.SetInfixTokens(const Value: TStrings);
begin
  FInfixTokens.Assign(Value);
  UpdatePostfixTokens;
end;

procedure TcaInfixToPostfix.UpdatePostfixTokens;
var
  Index: Integer;
  OperatorStack: IcaStringStack;
  Token: String;
begin
  FPostfixTokens.Clear;
  OperatorStack := TcaStringList.Create;
  for Index := 0 to FInfixTokens.Count - 1 do
    begin
      Token := FInfixTokens[Index];
      // Add operand to output
      if Utils.IsOperand(Token) then
        begin
          // Check for special constants
          if Token = 'pi' then Token := FloatToStr(Pi);
          if Token = 'e' then Token := FloatToStr(Exp(1));
          FPostfixTokens.Add(Token);
          Continue;
        end;
      // Push open bracket on to stack
      if Token = '(' then
        begin
          OperatorStack.Push(Token);
          Continue;
        end;
      // Add pending operators to output
      if Token = ')' then
        begin
          // Pop operators from stack until an open bracket is found
          while OperatorStack.Peek <> '(' do
            begin
              if OperatorStack.IsEmpty then
                raise EcaMathEvalException.Create('Mismatched bracket error');
              FPostfixTokens.Add(OperatorStack.Pop);
            end;
          // Discard the open bracket
          OperatorStack.Pop;
          Continue;
        end;
      // Push function onto the stack
      if Utils.IsFunction(Token) then
        begin
          OperatorStack.Push(Token);
          Continue;
        end;
      // Add pending operators to output
      if Utils.IsOperator(Token) then
        begin
          while
          (not OperatorStack.IsEmpty) and
          (not (Utils.GetOperatorPrecedence(OperatorStack.Peek, Token) = opLower)) and
          (not (Utils.GetOperatorPrecedence(OperatorStack.Peek, Token) = opSameRightAssoc)) do
            FPostfixTokens.Add(OperatorStack.Pop);
          OperatorStack.Push(Token);
          Continue;
        end;
    end;
  // Add remaining operators to output
  while not OperatorStack.IsEmpty do
    begin
      if OperatorStack.Peek = '(' then
        raise EcaMathEvalException.Create('Mismatched bracket error');
      FPostfixTokens.Add(OperatorStack.Pop);
    end;
  OperatorStack := nil;
end;

 //---------------------------------------------------------------------------
 // TcaPostfixEvaluator
 //---------------------------------------------------------------------------

constructor TcaPostfixEvaluator.Create;
begin
  inherited;
  FPostfixTokens := TStringList.Create;
end;

destructor TcaPostfixEvaluator.Destroy;
begin
  FPostfixTokens.Free;
  inherited;
end;

 // Protected methods

procedure TcaPostfixEvaluator.DoGetOperandVariable(const AOperand: String; var AValue: Extended);
begin
  if Assigned(FOnGetOperandVariable) then
    FOnGetOperandVariable(Self, AOperand, AValue);    
end;

 // Private methods

procedure TcaPostfixEvaluator.Evaluate;
var
  EvalResult: Extended;
  Index: Integer;
  LeftOperand: Extended;
  Operand: Extended;
  OperandStack: IcaStringStack;
  RightOperand: Extended;
  Token: String;
  MathUtils: IcaMathUtils;
begin
  MathUtils := Utils as IcaMathUtils;
  OperandStack := TcaStringList.Create;
  for Index := 0 to FPostfixTokens.Count - 1 do
    begin
      Token := FPostfixTokens[Index];
      if Utils.IsOperand(Token) then
        OperandStack.Push(Token)
      else
        begin
          EvalResult := 0;
          if Utils.IsOperator(Token) then
            begin
              RightOperand := GetOperandValue(OperandStack.Pop);
              LeftOperand := GetOperandValue(OperandStack.Pop);
              if Token = '+' then EvalResult := LeftOperand + RightOperand else
              if Token = '-' then EvalResult := LeftOperand - RightOperand else
              if Token = '*' then EvalResult := LeftOperand * RightOperand else
              if Token = '/' then EvalResult := LeftOperand / RightOperand else
              if Token = '^' then EvalResult := Power(LeftOperand, RightOperand);
            end;
          if Utils.IsFunction(Token) then
            begin
              Operand := GetOperandValue(OperandStack.Pop);
              if Token = 'cos' then EvalResult := Cos(GetTrigOperand(Operand)) else
              if Token = 'exp' then EvalResult := Exp(Operand) else
              if Token = 'int' then EvalResult := Int(Operand) else
              if Token = 'sin' then EvalResult := Sin(GetTrigOperand(Operand)) else
              if Token = 'frac' then EvalResult := Frac(Operand) else
              if Token = 'round' then EvalResult := Round(Operand) else
              if Token = 'trunc' then EvalResult := MathUtils.Trunc(Operand) else
              if Token = 'tan' then EvalResult := Tan(GetTrigOperand(Operand)) else
              if Token = 'ln' then EvalResult := Ln(Operand) else
              if Token = 'log10' then EvalResult := Log10(Operand) else
              if Token = 'sqrt' then EvalResult := Sqrt(Operand);
            end;
          OperandStack.Push(FloatToStr(EvalResult));
        end;
    end;
  if (OperandStack as IcaStringList).Count <> 1 then
    raise EcaMathEvalException.Create('Postfix evaluator stack count <> 1');
  // FValue := StrToFloat(OperandStack.Pop);
  FValue := GetOperandValue(OperandStack.Pop);
end;

function TcaPostfixEvaluator.GetTrigOperand(AOperand: Extended): Extended;
begin
  if FTrigUnits = tuDegrees then
    Result := DegToRad(AOperand)
  else
    Result := AOperand;
end;

function TcaPostfixEvaluator.GetOperandValue(const AOperand: String): Extended;
var
  Handled: Boolean;
  OperandChar: Char;
begin
  Result := 0;
  Handled := False;
  if AOperand <> '' then
    begin
      OperandChar := AOperand[1];
      if OperandChar in ['a'..'z', 'A'..'Z'] then
        begin
          DoGetOperandVariable(AOperand, Result);
          Handled := True;
        end;
    end;
  if not Handled then
    Result := Utils.Str2Float(AOperand, 0);
end;

function TcaPostfixEvaluator.GetPostfixTokens: TStrings;
begin
  Result := FPostfixTokens;
end;

function TcaPostfixEvaluator.GetTrigUnits: TcaTrigUnits;
begin
  Result := FTrigUnits;
end;

function TcaPostfixEvaluator.GetValue: Extended;
begin
  Result := FValue;
end;

procedure TcaPostfixEvaluator.SetPostfixTokens(const Value: TStrings);
begin
  FPostfixTokens.Assign(Value);
  Evaluate;
end;

procedure TcaPostfixEvaluator.SetTrigUnits(const Value: TcaTrigUnits);
begin
  FTrigUnits := Value;
end;

 // Event property methods

function TcaPostfixEvaluator.GetOnGetOperandVariable: TcaOperandVariableEvent;
begin
  Result := FOnGetOperandVariable;
end;

procedure TcaPostfixEvaluator.SetOnGetOperandVariable(const Value: TcaOperandVariableEvent);
begin
  FOnGetOperandVariable := Value;
end;

 //---------------------------------------------------------------------------
 // TcaMathEvaluator
 //---------------------------------------------------------------------------

constructor TcaMathEvaluator.Create;
begin
  inherited;
  FMathTokens := TcaMathTokens.Create;
  FInfixToPostfix := TcaInFixToPostfix.Create;
  FPostfixEvaluator := TcaPostfixEvaluator.Create;
  FPostfixEvaluator.OnGetOperandVariable := OperandVariableEvent;
  SetTrigUnits(tuDegrees);
end;

 // Protected methods

procedure TcaMathEvaluator.DoGetOperandVariable(const AOperand: String; var AValue: Extended);
begin
  if Assigned(FOnGetOperandVariable) then
    FOnGetOperandVariable(Self, AOperand, AValue);
end;

 // Event handler

procedure TcaMathEvaluator.OperandVariableEvent(Sender: TObject; const AOperand: String; var AValue: Extended);
begin
  DoGetOperandVariable(AOperand, AValue);
end;

 // Property methods

function TcaMathEvaluator.GetExpression: String;
begin
  Result := FExpression;
end;

function TcaMathEvaluator.GetTrigUnits: TcaTrigUnits;
begin
  Result := FPostfixEvaluator.TrigUnits;
end;

procedure TcaMathEvaluator.SetExpression(const Value: String);
begin
  FExpression := Value;
  UpdateValue;
end;

function TcaMathEvaluator.GetValue: Extended;
begin
  Result := FValue;
end;

function TcaMathEvaluator.GetValueAsString: String;
begin
  Result := FloatToStr(FValue);
end;

procedure TcaMathEvaluator.SetTrigUnits(const Value: TcaTrigUnits);
begin
  FPostfixEvaluator.TrigUnits := Value;
end;

procedure TcaMathEvaluator.UpdateValue;
begin
  if FExpression <> '' then
    begin
      FMathTokens.Expression := FExpression;
      FInfixToPostfix.InfixTokens := FMathTokens.Tokens;
      FPostfixEvaluator.PostfixTokens := FInfixToPostfix.PostfixTokens;
      FValue := FPostfixEvaluator.Value;
    end
  else
    FValue := 0;
end;

 // Event property methods

function TcaMathEvaluator.GetOnGetOperandVariable: TcaOperandVariableEvent;
begin
  Result := FOnGetOperandVariable;
end;

procedure TcaMathEvaluator.SetOnGetOperandVariable(const Value: TcaOperandVariableEvent);
begin
  FOnGetOperandVariable := Value;
end;

end.
