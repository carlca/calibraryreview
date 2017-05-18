unit caXml;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,
  Graphics,

  // ca units 
  caTypes,
  caConsts,
  caClasses,
  caVector,
  caLog,
  caUtils;

type

  IcaXmlBuilder = interface;

  TcaXmlWriteEvent = procedure(Sender: TObject; AXmlBuilder: IcaXmlBuilder) of object;

  TcaXmlReaderEndTagEvent = procedure(Sender: TObject; const ATag: string; ALevel: Integer) of object;

  TcaXmlReaderDataEvent = procedure(Sender: TObject; const ATag, AData, AAttributes: string; ALevel: Integer) of object;

  TcaXmlReaderTagEvent = procedure(Sender: TObject; const ATag, AAttributes: string; ALevel: Integer) of object;

  TcaXmlAttributeEvent = procedure(Sender: TObject; const AName, AValue: string) of object;

  //---------------------------------------------------------------------------
  // IcaXmlStreamable                                                          
  //---------------------------------------------------------------------------

  IcaXmlStreamable = interface
  ['{57A2E187-82DE-4A31-94E6-BADE71BB2C73}']
    // Property methods 
    function GetOnWriteToXml: TcaXmlWriteEvent;
    function GetXmlBuilder: IcaXmlBuilder;
    procedure SetOnWriteToXml(const Value: TcaXmlWriteEvent);
    procedure SetXmlBuilder(const Value: IcaXmlBuilder);
    // Public methods 
    procedure WriteToXml;
    // Properties 
    property XmlBuilder: IcaXmlBuilder read GetXmlBuilder write SetXmlBuilder;
    // Event Properties 
    property OnWriteToXml: TcaXmlWriteEvent read GetOnWriteToXml write SetOnWriteToXml;
  end;

  //---------------------------------------------------------------------------
  // TcaXmlStreamable                                                          
  //---------------------------------------------------------------------------

  TcaXmlStreamable = class(TcaInterfacedPersistent, IcaXmlStreamable)
  private
    // Private fields 
    FXmlBuilder: IcaXmlBuilder;
    // Property fields 
    FOnWriteToXml: TcaXmlWriteEvent;
  protected
    // Protected property methods 
    function GetOnWriteToXml: TcaXmlWriteEvent;
    function GetXmlBuilder: IcaXmlBuilder;
    procedure SetOnWriteToXml(const Value: TcaXmlWriteEvent);
    procedure SetXmlBuilder(const Value: IcaXmlBuilder);
    // Protected methods 
    procedure DoWriteToXml(AXmlBuilder: IcaXmlBuilder); virtual;
  public
    constructor Create;
    // Public methods 
    procedure WriteToXml;
    // Properties 
    property XmlBuilder: IcaXmlBuilder read GetXmlBuilder write SetXmlBuilder;
    // Event Properties 
    property OnWriteToXml: TcaXmlWriteEvent read GetOnWriteToXml write SetOnWriteToXml;
  end;

  //---------------------------------------------------------------------------
  // IcaXmlAttributes                                                          
  //---------------------------------------------------------------------------

  IcaXmlAttributes = interface
  ['{35DF1CBA-5968-4494-BE62-ABEBF7EC9CDD}']
    // Protected interface property methods 
    function GetCommaText: string;
    function GetValue(const Name: string): string;
    procedure SetCommaText(const Value: string);
    procedure SetValue(const Name, Value: string);
    // interface methods...
    procedure Clear;
    // interface properties...
    property CommaText: string read GetCommaText write SetCommaText;
    property Values[const Name: string]: string read GetValue write SetValue; default;
  end;

  //---------------------------------------------------------------------------
  // TcaXmlAttributes                                                          
  //---------------------------------------------------------------------------

  TcaXmlAttributes = class(TcaStringList, IcaXmlAttributes)
  private
    // Private methods 
    procedure UpdateAttributes(const AAttributes: string);
  protected
    // Protected interface property methods 
    function GetValue(const Name: string): string;
  public
    constructor Create; overload;
    constructor Create(const AAttributes: string); overload;
    // Public properties 
    property Values[const Name: string]: string read GetValue; default;
  end;

  //---------------------------------------------------------------------------
  // IcaXmlBuilder                                                             
  //---------------------------------------------------------------------------

  IcaXmlBuilder = interface
  ['{3E12C174-6A53-454F-8F84-EA1AFB895224}']
    // Property methods 
    function GetAsText: string;
    function GetIndentSpaces: Integer;
    procedure SetAsText(const Value: string);
    procedure SetIndentSpaces(const Value: Integer);
    // Public base methods 
    function MakeAttribute(const AName: string; const AValue: Boolean): string; overload;
    function MakeAttribute(const AName: string; const AValue: Double): string; overload;
    function MakeAttribute(const AName: string; const AValue: Integer): string; overload;
    function MakeAttribute(const AName: string; const AValue: string): string; overload;
    procedure Add(const AElement: string; ALevel: Integer = -1);
    procedure AddTag(const AElement: string; ALevel: Integer = -1); overload;
    procedure AddTag(const AElement: string; AAttributes: string; ALevel: Integer = -1); overload;
    procedure AddTagWithEnd(const AElement: string; ALevel: Integer = -1); overload;
    procedure AddTagWithEnd(const AElement: string; AAttributes: string; ALevel: Integer = -1); overload;
    procedure AddText(const AText: string);
    procedure EmptyTag(const AElement: string; ALevel: Integer = -1);
    procedure EndTag(const AElement: string = ''; ALevel: Integer = -1);
    procedure SaveXmlToFile(const AFileName: string);
    procedure SaveXmlToStream(const AStream: TStream);
    // Public utilty methods derived from base methods 
    procedure WriteValue(const AName: string; AValue: Boolean; ALevel: Integer = -1); overload;
    procedure WriteValue(const AName: string; AValue: Double; ALevel: Integer = -1); overload;
    procedure WriteValue(const AName: string; AValue: Integer; ALevel: Integer = -1); overload;
    procedure WriteValue(const AName, AValue: string; ALevel: Integer = -1); overload;
    // Properties 
    property AsText: string read GetAsText write SetAsText;
    property IndentSpaces: Integer read GetIndentSpaces write SetIndentSpaces;
  end;

  //---------------------------------------------------------------------------
  // TcaXmlBuilder                                                             
  //---------------------------------------------------------------------------

  TcaXmlBuilder = class(TcaStringList, IcaXmlBuilder)
  private
    // Private fields 
    FIndentSpaces: Integer;
    FLevel: Integer;
    FTagStack: IcaStringStack;
    // Property methods 
    function GetAsText: string;
    function GetIndentSpaces: Integer;
    procedure SetAsText(const Value: string);
    procedure SetIndentSpaces(const Value: Integer);
    // Private methods 
    function BuildTag(const AElement: string; ALevel: Integer; ATagType: TcaXmlTagType): string;
    function GetLevel(ALevel: Integer): Integer;
    function StripDecoration(const AText: string): string;
  public
    constructor Create; overload;
    constructor CreateUtf8; overload;
    // Public methods 
    function MakeAttribute(const AName: string; const AValue: Boolean): string; overload;
    function MakeAttribute(const AName: string; const AValue: Double): string; overload;
    function MakeAttribute(const AName: string; const AValue: Integer): string; overload;
    function MakeAttribute(const AName: string; const AValue: string): string; overload;
    procedure Add(const AElement: string; ALevel: Integer = -1); reintroduce;
    procedure AddTag(const AElement: string; ALevel: Integer = -1); overload;
    procedure AddTag(const AElement: string; AAttributes: string; ALevel: Integer = -1); overload;
    procedure AddTagWithEnd(const AElement: string; ALevel: Integer = -1); overload;
    procedure AddTagWithEnd(const AElement: string; AAttributes: string; ALevel: Integer = -1); overload;
    procedure AddText(const AText: string);
    procedure EmptyTag(const AElement: string; ALevel: Integer = -1);
    procedure EndTag(const AElement: string = ''; ALevel: Integer = -1);
    procedure SaveXmlToFile(const AFileName: string);
    procedure SaveXmlToStream(const AStream: TStream);
    // Public utilty methods derived from base methods 
    procedure WriteValue(const AName: string; AValue: Boolean; ALevel: Integer = -1); overload;
    procedure WriteValue(const AName: string; AValue: Double; ALevel: Integer = -1); overload;
    procedure WriteValue(const AName: string; AValue: Integer; ALevel: Integer = -1); overload;
    procedure WriteValue(const AName, AValue: string; ALevel: Integer = -1); overload;
    // Properties 
    property AsText: string read GetAsText write SetAsText;
    property IndentSpaces: Integer read GetIndentSpaces write SetIndentSpaces;
  end;

  //---------------------------------------------------------------------------
  // IcaXmlReader                                                              
  //---------------------------------------------------------------------------

  IcaXmlReader = interface
  ['{0F128E01-95A2-4B2A-8D2E-94F3602EA28D}']
    // Property methods 
    function GetAsText: string;
    function GetDocumentName: string;
    function GetOnData: TcaXmlReaderDataEvent;
    function GetOnEndTag: TcaXmlReaderEndTagEvent;
    function GetOnTag: TcaXmlReaderTagEvent;
    procedure SetAsText(const Value: string);    
    procedure SetOnData(const Value: TcaXmlReaderDataEvent);
    procedure SetOnEndTag(const Value: TcaXmlReaderEndTagEvent);
    procedure SetOnTag(const Value: TcaXmlReaderTagEvent);
    // Public methods 
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromXml(const AFileName: string);
    procedure Parse;
    // Properties 
    property AsText: string read GetAsText write SetAsText;
    property DocumentName: string read GetDocumentName;
    // Event properties 
    property OnData: TcaXmlReaderDataEvent read GetOnData write SetOnData;
    property OnEndTag: TcaXmlReaderEndTagEvent read GetOnEndTag write SetOnEndTag;
    property OnTag: TcaXmlReaderTagEvent read GetOnTag write SetOnTag;
  end;

  //---------------------------------------------------------------------------
  // TcaXmlReader                                                              
  //---------------------------------------------------------------------------

  TcaXmlReader = class(TcaInterfacedPersistent, IcaXmlReader)
  private
    // Private fields 
    FXml: IcaStringList;
    FXmlTokens: IcaStringList;
    // Property fields 
    FOnData: TcaXmlReaderDataEvent;
    FOnEndTag: TcaXmlReaderEndTagEvent;
    FOnTag: TcaXmlReaderTagEvent;
    // Property methods 
    function GetDocumentName: string;
    function GetOnData: TcaXmlReaderDataEvent;
    function GetOnEndTag: TcaXmlReaderEndTagEvent;
    function GetOnTag: TcaXmlReaderTagEvent;
    procedure SetOnData(const Value: TcaXmlReaderDataEvent);
    procedure SetOnEndTag(const Value: TcaXmlReaderEndTagEvent);
    procedure SetOnTag(const Value: TcaXmlReaderTagEvent);
    // Private methods 
    function IsEndTag(const AToken: IcaString): Boolean;
    function IsTag(const AToken: IcaString): Boolean;
    function MakeString(const AString: string): IcaString;
    function StripToken(const AToken: IcaString): string;
    procedure UpdateXmlTokens;
  protected
    // Protected methods 
    procedure DoData(const ATag, AData, AAttributes: string; ALevel: Integer); virtual;
    procedure DoEndTag(const ATag: string; ALevel: Integer); virtual;
    procedure DoTag(const ATag, AAttributes: string; ALevel: Integer); virtual;
  public
    constructor Create;
    // Public methods 
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromXml(const AFileName: string);
    procedure Parse;
    // Properties 
    property AsText: string read GetAsText write SetAsText;
    property DocumentName: string read GetDocumentName;
    // Event properties 
    property OnTag: TcaXmlReaderTagEvent read GetOnTag write SetOnTag;
    property OnEndTag: TcaXmlReaderEndTagEvent read GetOnEndTag write SetOnEndTag;
    property OnData: TcaXmlReaderDataEvent read GetOnData write SetOnData;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaXmlStreamable                                                          
  //---------------------------------------------------------------------------

constructor TcaXmlStreamable.Create;
begin
  inherited;
  FXmlBuilder := TcaXmlBuilder.Create;
end;

  // Public methods 

procedure TcaXmlStreamable.WriteToXml;
begin
  DoWriteToXml(FXmlBuilder);
end;

  // Protected methods 

procedure TcaXmlStreamable.DoWriteToXml(AXmlBuilder: IcaXmlBuilder);
begin
  if Assigned(FOnWriteToXml) then
    FOnWriteToXml(Self, AXmlBuilder);
end;

  // Property methods 

function TcaXmlStreamable.GetOnWriteToXml: TcaXmlWriteEvent;
begin
  Result := FOnWriteToXml;
end;

function TcaXmlStreamable.GetXmlBuilder: IcaXmlBuilder;
begin
  Result := FXmlBuilder;
end;

procedure TcaXmlStreamable.SetOnWriteToXml(const Value: TcaXmlWriteEvent);
begin
  FOnWriteToXml := Value;
end;

procedure TcaXmlStreamable.SetXmlBuilder(const Value: IcaXmlBuilder);
begin
  FXmlBuilder := nil;
  FXmlBuilder := Value;
end;

  //---------------------------------------------------------------------------
  // TcaXmlAttributes                                                          
  //---------------------------------------------------------------------------

constructor TcaXmlAttributes.Create;
begin
  inherited;
end;

constructor TcaXmlAttributes.Create(const AAttributes: string);
begin
  inherited Create;
  UpdateAttributes(AAttributes);
end;

  // Protected interface property methods 

function TcaXmlAttributes.GetValue(const Name: string): string;
begin
  Result := inherited Values[Name];
end;

  // Private methods 

procedure TcaXmlAttributes.UpdateAttributes(const AAttributes: string);
var
  Attribute: string;
  AttrStart: Integer;
  Ch: Char;
  Index: Integer;
  InQuotes: Boolean;
begin
  AttrStart := 1;
  InQuotes := False;
  for Index := 1 to Length(AAttributes) do
    begin
      if Index > AttrStart then
        begin
          Ch := AAttributes[Index];
          if Ch = cDoubleQuote then
            begin
              InQuotes := not InQuotes;
              if not InQuotes then
                begin
                  Attribute := Copy(AAttributes, AttrStart, Index - AttrStart + 1);
                  Utils.StripChar(cDoubleQuote, Attribute);
                  Add(Attribute);
                  AttrStart := Index + 2;
                end;
            end;
        end;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaXmlBuilder                                                             
  //---------------------------------------------------------------------------

constructor TcaXmlBuilder.Create;
begin
  inherited;
  FTagStack := TcaStringList.Create;
  FIndentSpaces := 2;
end;

constructor TcaXmlBuilder.CreateUtf8;
begin
  Create;
  inherited Add('<?xml version="1.0" encoding="utf-8"?>');
end;

// Public methods 

function TcaXmlBuilder.MakeAttribute(const AName: string; const AValue: Boolean): string;
begin
  Result := Format('%s="%s"', [AName, Utils.BooleanToString(AValue)]);
end;

function TcaXmlBuilder.MakeAttribute(const AName: string; const AValue: Double): string;
begin
  Result := Format('%s="%s"', [AName, FloatToStr(AValue)]);
end;

function TcaXmlBuilder.MakeAttribute(const AName: string; const AValue: Integer): string;
begin
  Result := Format('%s="%s"', [AName, IntToStr(AValue)]);
end;

function TcaXmlBuilder.MakeAttribute(const AName: string; const AValue: string): string;
begin
  Result := Format('%s="%s"', [AName, AValue]);
end;

procedure TcaXmlBuilder.Add(const AElement: string; ALevel: Integer = -1);
begin
  inherited Add(BuildTag(AElement, GetLevel(ALevel), ttText));
end;

procedure TcaXmlBuilder.AddTag(const AElement: string; ALevel: Integer = -1);
begin
  inherited Add(BuildTag(AElement, GetLevel(ALevel), ttStart));
  Inc(FLevel);
  FTagStack.Push(AElement);
end;

procedure TcaXmlBuilder.AddTag(const AElement: string; AAttributes: string; ALevel: Integer = -1);
var
  Attributes: string;
  AttributesList: IcaStringList;
  ElementAndAttributes: string;
  Index: Integer;
begin
  AttributesList := TcaStringList.Create;
  AttributesList.CommaText := AAttributes;
  Attributes := '';
  for Index := 0 to AttributesList.Count - 1 do
    Attributes := Attributes + ' ' + AttributesList[Index];
  Utils.ReplaceChar(Attributes, #32, '~');
  ElementAndAttributes := AElement + Attributes;
  AddTag(ElementAndAttributes, ALevel);
  FTagStack.Pop;
  FTagStack.Push(AElement);
end;

procedure TcaXmlBuilder.AddTagWithEnd(const AElement: string; ALevel: Integer);
var
  XmlText: string;
begin
  AddTag(AElement, ALevel);
  XmlText := GetAsText;
  System.Delete(XmlText, Length(XmlText) - 1, 2);
  System.Insert('/', XmlText, Length(XmlText));
  SetAsText(XmlText);
  Dec(FLevel);
  FTagStack.Pop;
end;

procedure TcaXmlBuilder.AddTagWithEnd(const AElement: string; AAttributes: string; ALevel: Integer);
var
  XmlText: string;
begin
  AddTag(AElement, AAttributes, ALevel);
  XmlText := GetAsText;
  System.Delete(XmlText, Length(XmlText) - 1, 2);
  System.Insert('/', XmlText, Length(XmlText));
  SetAsText(XmlText);
  Dec(FLevel);  
  FTagStack.Pop;
end;

procedure TcaXmlBuilder.AddText(const AText: string);
var
  XmlText: string;
begin
  XmlText := GetAsText;
  if Length(XmlText) > 2 then
    if (XmlText[Length(XmlText) - 1] = #13) and (XmlText[Length(XmlText)] = #10) then
      System.Delete(XmlText, Length(XmlText) - 1, 2);
  XmlText := XmlText + AText;
  SetAsText(XmlText);
end;

procedure TcaXmlBuilder.EndTag(const AElement: string = ''; ALevel: Integer = -1);
var
  Element: string;
  LastTag: string;
  XmlText: string;
  AddToText: Boolean;
  LastLine: string;
  ChevPos: Integer;
begin
  AddToText := False;
  LastTag := FTagStack.Pop;

  XmlText := GetAsText;
  if Length(XmlText) > 2 then
    if (XmlText[Length(XmlText) - 1] = #13) and (XmlText[Length(XmlText)] = #10) then
      begin
        System.Delete(XmlText, Length(XmlText) - 1, 2);
        AddToText := XmlText[Length(XmlText)] <> '>';
      end;

  Dec(FLevel);
  if AElement <> '' then
    Element := AElement
  else
    Element := LastTag;

  if AddToText then
    begin
      XmlText := XmlText + Trim(BuildTag(Element, GetLevel(ALevel), ttEnd));
      SetAsText(XmlText);
    end
  else
    inherited Add(BuildTag(Element, GetLevel(ALevel), ttEnd));

  if Self.Count >= 2 then
    begin
      if StripDecoration(Self[Count - 2]) = StripDecoration(Self[Count - 1]) then
        begin
          Delete(Self.Count - 1);
          LastLine := Self[Count - 1];
          ChevPos := Pos('>', LastLine);
          System.Insert(' /', LastLine, ChevPos);
          Self[Count - 1] := LastLine;
        end;
    end;
end;

function TcaXmlBuilder.StripDecoration(const AText: string): string;
begin
  Result := StringReplace(AText, '<', '', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);  
end;

procedure TcaXmlBuilder.EmptyTag(const AElement: string; ALevel: Integer = -1);
begin
  inherited Add(BuildTag(AElement, GetLevel(ALevel) + 1, ttEmpty));
end;

procedure TcaXmlBuilder.SetIndentSpaces(const Value: Integer);
begin
  FIndentSpaces := Value;
end;

procedure TcaXmlBuilder.SaveXmlToFile(const AFileName: string);
begin
  SaveToFile(AFileName);
end;

procedure TcaXmlBuilder.SaveXmlToStream(const AStream: TStream);
begin
  SaveToStream(AStream);
end;

  // Public utilty methods derived from base methods 

procedure TcaXmlBuilder.WriteValue(const AName: string; AValue: Boolean; ALevel: Integer = -1);
begin
  WriteValue(AName, Utils.BooleanToString(AValue), ALevel);
end;

procedure TcaXmlBuilder.WriteValue(const AName: string; AValue: Double; ALevel: Integer = -1);
begin
  WriteValue(AName, Utils.DoubleToString(AValue, ''), ALevel);
end;

procedure TcaXmlBuilder.WriteValue(const AName: string; AValue: Integer; ALevel: Integer = -1);
begin
  WriteValue(AName, Utils.IntegerToString(AValue, ''), ALevel);
end;

procedure TcaXmlBuilder.WriteValue(const AName, AValue: string; ALevel: Integer = -1);
begin
  AddTag(AName, GetLevel(ALevel));
  Add(AValue, GetLevel(ALevel));
  EndTag;
end;

  // Private methods 

function TcaXmlBuilder.BuildTag(const AElement: string; ALevel: Integer; ATagType: TcaXmlTagType): string;
var
  T1, T2, T3: string;
  TagElement: string;
begin
  TagElement := AElement;
  Utils.ReplaceChar(TagElement, ' ', '_');
  Utils.ReplaceChar(TagElement, '~', ' ');
  T1 := '';
  T2 := '';
  T3 := '';
  case ATagType of
    ttStart:
      begin
        T1 := TagElement;
      end;
    ttEnd:
      begin
        T1 := '/';
        T2 := TagElement;
      end;
    ttEmpty:
      begin
        T1 := TagElement;
        T2 := ' ';
        T3 := '/';
      end;
    ttText:;
  end;
  if ATagType = ttText then
    Result := Utils.Indent(AElement, ALevel * FIndentSpaces)
  else
    Result := Utils.Indent(Format('<%s%s%s>', [T1, T2, T3]), ALevel * FIndentSpaces);
end;

function TcaXmlBuilder.GetIndentSpaces: Integer;
begin
  Result := FIndentSpaces;
end;

function TcaXmlBuilder.GetLevel(ALevel: Integer): Integer;
begin
  if ALevel >= 0 then
    Result := ALevel
  else
    Result := FLevel;
end;

function TcaXmlBuilder.GetAsText: string;
begin
  Result := Text;
end;

procedure TcaXmlBuilder.SetAsText(const Value: string);
begin
  Text := Value;
end;

  //---------------------------------------------------------------------------
  // TcaXmlReader                                                              
  //---------------------------------------------------------------------------

constructor TcaXmlReader.Create;
begin
  inherited;
  FXml := TcaStringList.Create;
  FXmlTokens := TcaStringList.Create;
end;

  // Public methods 

procedure TcaXmlReader.LoadFromStream(Stream: TStream);
begin
  FXml.LoadFromStream(Stream);
end;

procedure TcaXmlReader.LoadFromXml(const AFileName: string);
begin
  FXml.LoadFromFile(AFileName);
end;

procedure TcaXmlReader.Parse;
var
  Attributes: string;
  ExpectingData: Boolean;
  Index: Integer;
  LastAttributes: string;
  LastLevel: Integer;
  LastTag: string;
  Level: Integer;
  SpacePos: Integer;
  StrippedToken: IcaString;
  Tag: string;
  Token: IcaString;
begin
  UpdateXmlTokens;
  Level := 0;
  LastTag := '';
  LastLevel := 0;
  LastAttributes := '';
  ExpectingData := False;
  for Index := 0 to FXmlTokens.Count - 1 do
    begin
      Token := MakeString(FXmlTokens[Index]);
      if IsTag(Token) then
        begin
          if IsEndTag(Token) then
            begin
              if ExpectingData then
                begin
                  DoData(LastTag, '', LastAttributes, LastLevel);
                  ExpectingData := False;
                end;
              Dec(Level);
              DoEndTag(StripToken(Token), Level);
            end
          else
            begin
              StrippedToken := TcaString.Create(StripToken(Token));
              Attributes := '';
              SpacePos := StrippedToken.PosFromStart(' ');
              if SpacePos > 0 then
                begin
                  Attributes := StrippedToken.S;
                  Utils.DeleteUntilChar(Attributes, ' ', True);
                  Tag := StrippedToken.Left(SpacePos - 1)
                end
              else
                Tag := StrippedToken.S;
              LastTag := Tag;
              LastLevel := Level;
              LastAttributes := Attributes;
              DoTag(Tag, Attributes, Level);
              Inc(Level);
              ExpectingData := True;
            end;
        end
      else
        begin
          DoData(LastTag, StripToken(Token), LastAttributes, LastLevel);
          ExpectingData := False;
        end;
    end;
end;

  // Protected methods 

procedure TcaXmlReader.DoData(const ATag, AData, AAttributes: string; ALevel: Integer);
begin
  if Assigned(FOnData) then FOnData(Self, ATag, AData, AAttributes, ALevel);
end;

procedure TcaXmlReader.DoEndTag(const ATag: string; ALevel: Integer);
begin
  if Assigned(FOnEndTag) then FOnEndTag(Self, ATag, ALevel);
end;

procedure TcaXmlReader.DoTag(const ATag, AAttributes: string; ALevel: Integer);
begin
  if Assigned(FOnTag) then FOnTag(Self, ATag, AAttributes, ALevel);
end;

  // Private methods 

function TcaXmlReader.IsEndTag(const AToken: IcaString): Boolean;
begin
  Result := (AToken.Left(2) = '</');
end;

function TcaXmlReader.IsTag(const AToken: IcaString): Boolean;
begin
  Result := AToken.Left(1) = '<';
end;

function TcaXmlReader.MakeString(const AString: string): IcaString;
begin
  Result := TcaString.Create(AString);
end;

function TcaXmlReader.StripToken(const AToken: IcaString): string;
begin
  if IsTag(AToken) then
    begin
      if IsEndTag(AToken) then
        AToken.DeleteFromStart(2)
      else
        AToken.DeleteFromStart(1);
    end;
  Result := Trim(AToken.S);
end;

procedure TcaXmlReader.UpdateXmlTokens;
var
  Parser: IcaParser;
  Xml: string;
begin
  FXmlTokens.Clear;
  Parser := Utils as IcaParser;
  Parser.Initialize;
  Parser.TokenDelimiters := '>';
  Xml := FXml.Text;
  Utils.Replace(Xml, '</', '></');
  Parser.StringToParse := Xml;
  Parser.IgnoreBlanks := True;
  while Parser.HasMoreTokens do
    FXmlTokens.Add(Parser.NextToken);
end;

  // Property methods 

function TcaXmlReader.GetDocumentName: string;
begin
  Result := '';
  UpdateXmlTokens;
  if FXmlTokens.Count > 0 then
    Result := StripToken(MakeString(FXmlTokens[0]));
end;

function TcaXmlReader.GetOnData: TcaXmlReaderDataEvent;
begin
  Result := FOnData;
end;

function TcaXmlReader.GetOnEndTag: TcaXmlReaderEndTagEvent;
begin
  Result := FOnEndTag;
end;

function TcaXmlReader.GetOnTag: TcaXmlReaderTagEvent;
begin
  Result := FOnTag;
end;

procedure TcaXmlReader.SetOnData(const Value: TcaXmlReaderDataEvent);
begin
  FOnData := Value;
end;

procedure TcaXmlReader.SetOnEndTag(const Value: TcaXmlReaderEndTagEvent);
begin
  FOnEndTag := Value;
end;

procedure TcaXmlReader.SetOnTag(const Value: TcaXmlReaderTagEvent);
begin
  FOnTag := Value;
end;

end.


