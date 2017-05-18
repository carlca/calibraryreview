unit caHTML;

{$INCLUDE ca.inc}

interface

uses
  Windows, SysUtils, Messages, Classes, caUtils, caClasses, caMatrix, Graphics,
  caGraphics;

type

  TcaHTMLAlignment = (haLeft, haRight, haTop, haTexttop, haMiddle,
                      haAbsMiddle, haBaseline, haBottom, haAbsBottom);

 //----------------------------------------------------------------------------
 // TcaHTMLFont
 //----------------------------------------------------------------------------

  TcaFontNumber = 0..3;

  TcaHTMLFont = class(TObject)
  private
    FBold: Boolean;
    FColor: String;
    FSize: Integer;
    FTypeFace: String;
  public
    procedure Assign(ASourceFont: TcaHTMLFont);
    property Bold: Boolean read FBold write FBold;
    property Color: String read FColor write FColor;
    property Size: Integer read FSize write FSize;
    property TypeFace: String read FTypeFace write FTypeFace;
  end;

 //----------------------------------------------------------------------------
 // TcaHTML
 //----------------------------------------------------------------------------

  TcaHTML = class(TcaStringList)
  private
    FDefaultCellHeight: Integer;
    FFontBold: Boolean;
    FFont1: TcaHTMLFont;
    FFont2: TcaHTMLFont;
    FFont3: TcaHTMLFont;
    function GetAlignment(AAlign: TcaHTMLAlignment): String;
    procedure AddHeading(ALevel: Integer; const AText: String);
    procedure SetFont(const ABold: Boolean; const AColorName: String; ASize: Integer; ATypeFace: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Blank;
    procedure Body(ABackColor: String = ''; const ABackImage: String = ''; AOnLoad: String = '';
                   ALink: String = ''; AVLink: String = ''; AALink: String = ''; ATextColor: String = '');
    procedure Bold(const AText: String = '');
    procedure CheckBox(const AName, AText: String; AChecked: Boolean; AFontNumber: TcaFontNumber = 0);
    procedure Data(AWidth: Integer; AHeight: Integer = 0; AAlign: TcaHTMLAlignment = haLeft; AText: String = '';
                   AFontNumber: TcaFontNumber = 0; ABackColor: String = '';
                   AVAlign: TcaHTMLAlignment = haMiddle; AEndData: Boolean = True);
    procedure EndBody;
    procedure EndBold;
    procedure EndData;
    procedure EndFont;
    procedure EndForm;
    procedure EndHead;
    procedure EndHRef;
    procedure EndHTML;
    procedure EndItalic;
    procedure EndOrderedList;
    procedure EndPara;
    procedure EndRow;
    procedure EndSelect;
    procedure EndTable;
    procedure EndUnderline;
    procedure EndUnOrderedList;
    procedure Font(AFontNumber: TcaFontNumber); overload;
    procedure Font(const ABold: Boolean; const AColorName: String; ASize: Integer; const ATypeFace: String); overload;
    procedure Form(const AName, AAction: String; const AMethod: String = 'POST');
    procedure H1(const AText: String);
    procedure H2(const AText: String);
    procedure H3(const AText: String);
    procedure H4(const AText: String);
    procedure H5(const AText: String);
    procedure H6(const AText: String);
    procedure Head;
    procedure HiddenField(const AName, AValue: String);
    procedure HorzRule;
    procedure HRef(const AURL: String; const ATarget: String = '';
                   const AOnMouseOver: String = ''; const AOnMouseOut: String = '';
                   const AOnMouseDown: String = ''; const AOnMouseUp: String = '');
    procedure HTML;
    procedure HTMLHead(const ATitle: String);
    procedure Image(const AImage: String; AWidth: Integer = 0; AHeight: Integer = 0; ABorder: Integer = 0;
                    const AOnClick: String = ''; AAlign: TcaHTMLAlignment = haLeft; const AAlt: String = '';
                    AEndHRef: Boolean = False);
    procedure RadioOption(const AName, AText, AValue: String; AChecked: Boolean; AFontNumber: Integer = 0);
    procedure InputText(const AName: String; AValue: String = ''; ASize: Integer = 0;
                        AMaxLength: Integer = 0; APassword: Boolean = False);
    procedure Italic(const AText: String = '');
    procedure ListItem(const AText: String);
    procedure OrderedList;
    procedure Para(const AText: String = ''; AFontNumber: TcaFontNumber = 0);
    procedure Row;
    procedure Select(AName: String; ASize: Integer);
    procedure SelectOption(const AText, AValue: String; ASelected: Boolean = False);
    procedure SubmitButton(const AName, ACaption: String);
    procedure Table(AWidth: Integer; AUsesPercent: Boolean = False; ABorder: Integer = 0; AHeight: Integer = 0; AExtraHTML: String = '');
    procedure TextArea(const AName, AValue: String; ACols, ARows: Integer; AFontNumber: TcaFontNumber = 0);
    procedure Title(const AText: String);
    procedure Underline;
    procedure UnOrderedList;
    property DefaultCellHeight: Integer read FDefaultCellHeight write FDefaultCellHeight;
    property Font1: TcaHTMLFont read FFont1;
    property Font2: TcaHTMLFont read FFont2;
    property Font3: TcaHTMLFont read FFont3;
  end;

 //----------------------------------------------------------------------------
 // TcaWebPage
 //----------------------------------------------------------------------------

  TcaProcessWebTagEvent = procedure(Sender: TObject; const TagStr: String; var NewStr: String) of object;

  TcaWebPage = class(TObject)
  private
    FAppName: String;
    FBackColor: String;
    FBackImage: String;
    FDebugTables: Boolean;
    FDocsFolder: String;
    FHiddenFields: TStrings;
    FHoverColor: String;
    FLAlign: TcaHTMLAlignment;
    FLinkActiveColor: String;
    FLinkColor: String;
    FLinkVisitedColor: String;
    FLWidth: Integer;
    FOnLoad: String;
    FOnProcessTag: TcaProcessWebTagEvent;
    FPage: TcaHTML;
    FRadioName: String;
    FRAlign: TcaHTMLAlignment;
    FRWidth: Integer;
    FScriptsFolder: String;
    FText: String;
    FTextColor: String;
    function GetFont1: TcaHTMLFont;
    function GetFont2: TcaHTMLFont;
    function GetFont3: TcaHTMLFont;
    function GetHTML: String;
    function MakeHelpLink(const ACaption, AHelp: String): String;
    function CleanText(const AText: String): String;
    function CleanURL(const AText: String): String;
    procedure FindTags(const S: String; var Tag1, Tag2: Integer);
    procedure ProcessTags;
    function GetDefaultCellHeight: Integer;
    procedure SetDefaultCellHeight(const Value: Integer);
  protected
    function GetPageName: String; virtual;
    function BuildLinkURL(const AFromSuffix, AToSuffix, AExtra: String): String;
    procedure BuildBackLink(const AText: String);
    procedure BuildLink(const AText, AFromSuffix, AToSuffix, AExtra: String);
    procedure BuildLinkImage(const AImage, AURL, ATarget, AStatus, AAlt: String;
                             AUseRow: Boolean = True; AUseTable: Boolean = True);
    procedure BuildLinkText(const AText: String; AFontNumber: TcaFontNumber = 0);
    procedure BuildSubmitLink(const AText, AToPage: String);
    procedure BuildPage; virtual;
    procedure DoGetTableExtraHTML(var AExtraHTML: String); virtual;
    procedure DoProcessTag(const TagStr: String; var NewStr: String); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginForm; virtual;
    procedure BeginPage(const ATitle: String; AFontNumber: TcaFontNumber = 0; AKillFocusRect: Boolean = False);
    procedure BeginRadio(const ACaption, AName, AHelp: String; AFontNumber: TcaFontNumber = 0);
    procedure BeginSelect(const ACaption, AName, AHelp: String; AFontNumber: TcaFontNumber = 0);
    procedure BeginTable(AWidth: Integer; AUsesPercent: Boolean = False; ABorder: Integer = 0; AHeight: Integer = 0);
    procedure BeginText;
    procedure BlankRow;
    procedure DoubleText(const ALeftText, ARightText: String; AFontNumber: TcaFontNumber = 0);
    procedure EndFont;
    procedure EndForm;
    procedure EndPage;
    procedure EndPara;
    procedure EndRadio;
    procedure EndSelect;
    procedure EndTable;
    procedure EndText(AFontNumber: TcaFontNumber = 0);
    procedure LeftText(const AText: String; AFontNumber: TcaFontNumber = 0);
    procedure LoadFromFile(const AFileName: String);
    procedure Para(const AText: String = ''; AFontNumber: TcaFontNumber = 0);
    procedure Radio(const AText, AValue: String; AChecked: Boolean; AFontNumber: TcaFontNumber = 0);
    procedure RightText(const AText: String; AFontNumber: TcaFontNumber = 0);
    procedure Select(const AText, AValue: String; ASelected: Boolean = False);
    procedure AddText(const AText: String);
    procedure TextArea(const AName, AValue: String; ACols, ARows: Integer; AFontNumber: TcaFontNumber = 0);
    procedure TextLine(const AText: String; AFontNumber: TcaFontNumber = 0);
    property BackColor: String read FBackColor write FBackColor;
    property BackImage: String read FBackImage write FBackImage;
    property DebugTables: Boolean read FDebugTables write FDebugTables;
    property DefaultCellHeight: Integer read GetDefaultCellHeight write SetDefaultCellHeight;
    property DocsFolder: String read FDocsFolder write FDocsFolder;
    property Font1: TcaHTMLFont read GetFont1;
    property Font2: TcaHTMLFont read GetFont2;
    property Font3: TcaHTMLFont read GetFont3;
    property HoverColor: String read FHoverColor write FHoverColor;
    property HTML: String read GetHTML;
    property LAlign: TcaHTMLAlignment read FLAlign write FLAlign;
    property LinkActiveColor: String read FLinkActiveColor write FLinkActiveColor;
    property LinkColor: String read FLinkColor write FLinkColor;
    property LinkVisitedColor: String read FLinkVisitedColor write FLinkVisitedColor;
    property LWidth: Integer read FLWidth write FLWidth;
    property OnLoad: String read FOnLoad write FOnLoad;
    property OnProcessTag: TcaProcessWebTagEvent read FOnProcessTag write FOnProcessTag;
    property Page: TcaHTML read FPage;
    property PageName: String read GetPageName;
    property RAlign: TcaHTMLAlignment read FRAlign write FRAlign;
    property RWidth: Integer read FRWidth write FRWidth;
    property ScriptsFolder: String read FScriptsFolder write FScriptsFolder;
    property TextColor: String read FTextColor write FTextColor;
  end;

function Lorum: String;
function PostModern: String;

implementation

function Lorum: String;
begin
  Result := 'lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diem nonummy ';
  Result := Result + 'nibh euismod tincidunt ut lacreet dolore magna aliguam erat volutpat. ut ';
  Result := Result + 'wisis enim ad minim veniam, quis nostrud exerci tution ullamcorper suscipit ';
  Result := Result + 'lobortis nisl ut aliquip ex ea commodo consequat. duis te feugifacilisi. ';
  Result := Result + 'duis autem dolor in hendrerit in vulputate velit esse molestie consequat, ';
  Result := Result + 'vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et ';
  Result := Result + 'iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue ';
  Result := Result + 'duis dolore te feugat nulla facilisi. ut wisi enim ad minim veniam, quis ';
  Result := Result + 'nostrud exerci taion ullamcorper suscipit lobortis nisl ut aliquip ex en ';
  Result := Result + 'commodo consequat.';
end;

function PostModern: String;
begin
  Result := 'Latin-looking words arranged in nonsensical sentences, set in columns to give the ';
  Result := Result + 'appearance of text on a page. Dummy text is used by the designer to help ';
  Result := Result + 'approximate the look of a page at a stage in the design process before the ';
  Result := Result + 'written text has been received. This way the designer is able, in a very real ';
  Result := Result + 'sense, to shape the material presence of words before they are written and before ';
  Result := Result + 'their meaning is known. Conventionally, due to constraints of time, ability, budget, ';
  Result := Result + 'and habit, the designer is not the author. So conventionally, the student of ';
  Result := Result + 'typography is not encouraged to write (or even read prepared copy) which would waste ';
  Result := Result + 'valuable time spent getting to grips with the mechanics of text layout. Such ';
  Result := Result + 'established working/teaching methods increase the danger of the typographer ';
  Result := Result + 'becoming detached from the meaning of texts. The treatment of copy in purely ';
  Result := Result + 'formal terms, reduced to blocks of texture on a page, has lead to the typographer''s ';
  Result := Result + 'obsession with craft and disregard of meaning. Dummy text is text that is not ';
  Result := Result + 'meant to be read, but only looked at; a shape. The choice of latin is crucial to ';
  Result := Result + 'this function in that it is a dead language. Working with dummy text, the ';
  Result := Result + 'designer only brings into play part of his/her array of tools/skills to convey ';
  Result := Result + 'meaning.';
end;

 //----------------------------------------------------------------------------
 // TcaHTMLFont
 //----------------------------------------------------------------------------

procedure TcaHTMLFont.Assign(ASourceFont: TcaHTMLFont);
begin
  Bold := ASourceFont.Bold;
  Color := ASourceFont.Color;
  Size := ASourceFont.Size;
  TypeFace := ASourceFont.TypeFace;
end;

 //----------------------------------------------------------------------------
 // TcaHTML
 //----------------------------------------------------------------------------

constructor TcaHTML.Create;
begin
  inherited;
  FFont1 := TcaHTMLFont.Create;
  FFont2 := TcaHTMLFont.Create;
  FFont3 := TcaHTMLFont.Create;
end;

destructor TcaHTML.Destroy;
begin
  FFont1.Free;
  FFont2.Free;
  FFont3.Free;
  inherited;
end;

procedure TcaHTML.AddHeading(ALevel: Integer; const AText: String);
begin
  Add(Format('<h%d>%s</h%d>', [ALevel, AText, ALevel]));
end;

procedure TcaHTML.Blank;
begin
  Add('<br>');
end;

procedure TcaHTML.Bold(const AText: String = '');
var
  BoldStr: String;
begin
  BoldStr := '<b>';
  if AText <> '' then BoldStr := BoldStr + Format('%s</b>', [AText]);
  Add(BoldStr);
end;

procedure TcaHTML.H1(const AText: String);
begin
  AddHeading(1, AText);
end;

procedure TcaHTML.H2(const AText: String);
begin
  AddHeading(2, AText);
end;

procedure TcaHTML.H3(const AText: String);
begin
  AddHeading(3, AText);
end;

procedure TcaHTML.H4(const AText: String);
begin
  AddHeading(4, AText);
end;

procedure TcaHTML.H5(const AText: String);
begin
  AddHeading(5, AText);
end;

procedure TcaHTML.H6(const AText: String);
begin
  AddHeading(6, AText);
end;

procedure TcaHTML.HTMLHead(const ATitle: String);
begin
  HTML;
  Head;
  Title(ATitle);
  EndHead;
end;

procedure TcaHTML.HorzRule;
begin
  Add('<hr>');
end;

procedure TcaHTML.Italic(const AText: String = '');
var
  ItalicStr: String;
begin
  ItalicStr := '<i>';
  if AText <> '' then ItalicStr := ItalicStr + Format('%s</i>', [AText]);
  Add(ItalicStr);
end;

procedure TcaHTML.Title(const AText: String);
begin
  Add(Format('<title>%s</title>', [AText]));
end;

procedure TcaHTML.EndHead;
begin
  Add('</head>');
end;

procedure TcaHTML.Head;
begin
  Add('<head>');
end;

procedure TcaHTML.EndHTML;
begin
  Add('</html>');
end;

procedure TcaHTML.HTML;
begin
  Add('<html>');
end;

procedure TcaHTML.EndBody;
begin
  Add('</body>');
end;

procedure TcaHTML.Body(ABackColor: String = ''; const ABackImage: String = ''; AOnLoad: String = '';
                       ALink: String = ''; AVLink: String = ''; AALink: String = ''; ATextColor: String = '');
var
  BodyStr: String;
begin
  BodyStr := '<body ';
  if ABackColor <> '' then
    BodyStr := BodyStr + Format('bgcolor="%s" ', [ABackColor]);
  if ABackImage <> '' then
    BodyStr := BodyStr + Format('background="%s" ', [ABackImage]);
  if AOnLoad <> '' then
    BodyStr := BodyStr + Format('onload="%s" ', [AOnLoad]);
  if ALink <> '' then
    BodyStr := BodyStr + Format('link="%s" ', [ALink]);
  if AVLink <> '' then
    BodyStr := BodyStr + Format('vlink="%s" ', [AVLink]);
  if AALink <> '' then
    BodyStr := BodyStr + Format('alink="%s" ', [AALink]);
  if ATextColor <> '' then
    BodyStr := BodyStr + Format('text="%s" ', [ATextColor]);
  BodyStr := Trim(BodyStr) + '>';
  Add(BodyStr);
end;

procedure TcaHTML.Data(AWidth: Integer; AHeight: Integer = 0; AAlign: TcaHTMLAlignment = haLeft; AText: String = '';
                       AFontNumber: TcaFontNumber = 0; ABackColor: String = ''; 
                       AVAlign: TcaHTMLAlignment = haMiddle; AEndData: Boolean = True);
var
  AlignStr: String;
  VAlignStr: String;
  DataStr: String;
  CellHeight: Integer;
begin
  if AHeight = 0 then
    CellHeight := FDefaultCellHeight
  else
    CellHeight := AHeight;
  AlignStr := GetAlignment(AAlign);
  VAlignStr := GetAlignment(AVAlign);
  if AText = '' then
    begin
      DataStr := Format('<td width="%d" align="%s" valign="%s" ', [AWidth, AlignStr, VAlignStr]);
      if CellHeight <> 0 then DataStr := DataStr + Format('height="%d" ', [CellHeight]);
      if ABackColor <> '' then DataStr := DataStr + Format('bgcolor="%s" ', [ABackColor]);
      DataStr := Trim(DataStr) + '>';
      Add(DataStr);
    end
  else
    begin
      AText := StringReplace(AText, '_', '&nbsp;', [rfReplaceAll]);
      DataStr := Format('<td width="%d" align="%s" ', [AWidth, AlignStr]);
      if CellHeight <> 0 then DataStr := DataStr + Format('height="%d" ', [CellHeight]);
      if ABackColor <> '' then DataStr := DataStr + Format('bgcolor="%s" ', [ABackColor]);
      Add(Trim(DataStr) + '>');
      if AFontNumber > 0 then Font(AFontNumber);
      Add(AText);
      if AFontNumber > 0 then EndFont;
      if AEndData then EndData;
    end;
end;

procedure TcaHTML.EndBold;
begin
  Add('</b>');
end;

procedure TcaHTML.EndData;
begin
  Add('</td>');
end;

procedure TcaHTML.EndFont;
begin
  Add('</font>');
  if FFontBold then
    begin
      Add('</b>');
      FFontBold := False;
    end;
end;

procedure TcaHTML.EndForm;
begin
  Add('</form>');
end;

procedure TcaHTML.EndHRef;
begin
  Add('</a>');
end;

procedure TcaHTML.EndItalic;
begin
  Add('</i>');
end;

procedure TcaHTML.EndOrderedList;
begin
  Add('</ol>');
end;

procedure TcaHTML.EndPara;
begin
  Add('</p>');
end;

procedure TcaHTML.Image(const AImage: String; AWidth: Integer = 0; AHeight: Integer = 0; ABorder: Integer = 0;
                        const AOnClick: String = ''; AAlign: TcaHTMLAlignment = haLeft; const AAlt: String = '';
                        AEndHRef: Boolean = False);
var
  ImgStr: String;
  AlignStr: String;
begin
  AlignStr := GetAlignment(AAlign);
  ImgStr := Format('<img src="%s" ', [AImage]);
  if AWidth <> 0 then ImgStr := ImgStr + Format('width="%d" ', [AWidth]);
  if AHeight <> 0 then ImgStr := ImgStr + Format('height="%d" ', [AHeight]);
  ImgStr := ImgStr + Format('border="%d" ', [ABorder]);
  if AOnClick <> '' then ImgStr := ImgStr + Format('onclick="%s" ', [AOnClick]);
  if AAlt <> '' then ImgStr := ImgStr + Format('alt="%s" ', [AAlt]);
  ImgStr := Trim(ImgStr) + '>';
  if AEndHRef then ImgStr := ImgStr + '</a>';
  Add(ImgStr);
end;

procedure TcaHTML.Para(const AText: String = ''; AFontNumber: TcaFontNumber = 0);
begin
  if AText <> '' then
    begin
      if AFontNumber > 0 then Font(AFontNumber);
      Add('<p>' + AText + '</p>');
      if AFontNumber > 0 then EndFont;
    end
  else
    Add('<p>');
end;

procedure TcaHTML.Font(AFontNumber: TcaFontNumber);
begin
  case AFontNumber of
    1:  SetFont(FFont1.Bold, FFont1.Color, FFont1.Size, FFont1.TypeFace);
    2:  SetFont(FFont2.Bold, FFont2.Color, FFont2.Size, FFont2.TypeFace);
    3:  SetFont(FFont3.Bold, FFont3.Color, FFont3.Size, FFont3.TypeFace);
  end;
end;

procedure TcaHTML.Font(const ABold: Boolean; const AColorName: String; ASize: Integer; const ATypeFace: String);
begin
  SetFont(ABold, AColorName, ASize, ATypeFace);
end;

procedure TcaHTML.SetFont(const ABold: Boolean; const AColorName: String; ASize: Integer; ATypeFace: String);
var
  FontStr: String;
begin
  FontStr := '<font ';
  if AColorName <> '' then FontStr := FontStr + Format('color="%s" ', [AColorName]);
  FontStr := FontStr + Format('size="%d" ', [ASize]);
  if ATypeFace <> '' then FontStr := FontStr + Format('face="%s" ', [ATypeFace]);
  FontStr := Trim(FontStr) + '>';
  if ABold then
    begin
      FontStr := FontStr + '<b>';
      FFontBold := True;
    end;
  Add(FontStr);
end;

procedure TcaHTML.EndRow;
begin
  Add('</tr>');
  Add('');
end;

function TcaHTML.GetAlignment(AAlign: TcaHTMLAlignment): String;
begin
  case AAlign of
    haLeft:       Result := 'Left';
    haRight:      Result := 'Right';
    haTop:        Result := 'Top';
    haTexttop:    Result := 'Texttop';
    haMiddle:     Result := 'Middle';
    haAbsMiddle:  Result := 'AbsMiddle';
    haBaseline:   Result := 'Baseline';
    haBottom:     Result := 'Bottom';
    haAbsBottom:  Result := 'AbsBottom';
  else
    Result := '';
  end;
end;

procedure TcaHTML.Row;
begin
  Add('<tr>');
end;

procedure TcaHTML.EndSelect;
begin
  Add('</select>');
end;

procedure TcaHTML.EndTable;
begin
  Add('</table>');
end;

procedure TcaHTML.EndUnOrderedList;
begin
  Add('</ul>');
end;

procedure TcaHTML.EndUnderline;
begin
  Add('</u>');
end;

procedure TcaHTML.Form(const AName, AAction: String; const AMethod: String = 'POST');
begin
  Add(Format('<form name="%s" action="%s" method="%s">', [AName, AAction, AMethod]));
end;

procedure TcaHTML.HRef(const AURL: String; const ATarget: String = ''; 
                       const AOnMouseOver: String = ''; const AOnMouseOut: String = '';
                       const AOnMouseDown: String = ''; const AOnMouseUp: String = '');
var
  HRefStr: String;
begin
  HRefStr := Format('<a href="%s" ', [AURL]);
  if ATarget <> '' then HRefStr := HRefStr + Format('target="%s" ', [ATarget]);
  if AOnMouseOver <> '' then HRefStr := HRefStr + Format('OnMouseOver="%s" ', [AOnMouseOver]);
  if AOnMouseOut <> '' then HRefStr := HRefStr + Format('OnMouseOut="%s" ', [AOnMouseOut]);
  if AOnMouseDown <> '' then HRefStr := HRefStr + Format('OnMouseDown="%s" ', [AOnMouseDown]);
  if AOnMouseUp <> '' then HRefStr := HRefStr + Format('OnMouseUp="%s" ', [AOnMouseUp]);
  HRefStr := Trim(HRefStr) + '>';
  Add(HRefStr);
end;

procedure TcaHTML.HiddenField(const AName, AValue: String);
var
  HidPos: Integer;
  Index: Integer;
  PageStr: String;
begin
  for Index := Count - 1 downto 0 do
    begin
      PageStr := Strings[Index];
      HidPos := Pos(Format('<input type=hidden name="%s"', [AName]), PageStr);
      if HidPos >= 1 then Self.Delete(Index);
    end;
  Add(Format('<input type=hidden name="%s" value="%s">', [AName, AValue]));
end;

procedure TcaHTML.RadioOption(const AName, AText, AValue: String; AChecked: Boolean; AFontNumber: Integer = 0);
var
  RadioStr: String;
begin
  RadioStr := Format('<input name="%s" type=radio value=%s ', [AName, AValue]);
  if AChecked then RadioStr := RadioStr + 'checked ';
  RadioStr := Trim(RadioStr) + '>';
  Add(RadioStr);
  Font(AFontNumber);
  Add(AText);
  EndFont;
end;

procedure TcaHTML.CheckBox(const AName, AText: String; AChecked: Boolean; AFontNumber: TcaFontNumber = 0);
var
  CheckStr: String;
begin
  CheckStr := Format('<input NAME="%s" type=checkbox value=checkbox ', [AName]);
  if AChecked then CheckStr := CheckStr + 'checked ';
  CheckStr := Trim(CheckStr) + '>';
  Add(CheckStr);
  Font(AFontNumber);
  Add(AText);
  EndFont;
end;

procedure TcaHTML.TextArea(const AName, AValue: String; ACols, ARows: Integer; AFontNumber: TcaFontNumber = 0);
var
  TextAreaStr: String;
begin
  TextAreaStr := Format('<textarea name="%s" cols="%d" rows="%d">', [AName, ACols, ARows]);
  Font(AFontNumber);
  Add(TextAreaStr);
  Add(AValue);
  Add('</textarea>');
  EndFont;  
end;

procedure TcaHTML.InputText(const AName: String; AValue: String = ''; ASize: Integer = 0;
                            AMaxLength: Integer = 0; APassword: Boolean = False);
var
  InputStr: String;
begin
  if APassword then
    InputStr := '<input type=password '
  else
    InputStr := '<input type=text ';
  if AName <> '' then InputStr := InputStr + Format('name="%s" ', [AName]);
  if AValue <> '' then InputStr := InputStr + Format('value="%s" ', [AValue]);
  if ASize <> 0 then InputStr := InputStr + Format('size="%d" ', [ASize]);
  if AMaxLength <> 0 then InputStr := InputStr + Format('maxlength="%d" ', [AMaxLength]);
  InputStr := Trim(InputStr) + '>';
  Add(InputStr);
end;

procedure TcaHTML.ListItem(const AText: String);
begin
  Add(Format('<li>%s', [AText]));
end;

procedure TcaHTML.OrderedList;
begin
  Add('<ol>');
end;

procedure TcaHTML.Select(AName: String; ASize: Integer);
begin
  Add(Format('<select name="%s" size="%d">', [AName, ASize]));
end;

procedure TcaHTML.SelectOption(const AText, AValue: String; ASelected: Boolean = False);
var
  SelStr: String;
begin
  SelStr := '';
  if ASelected then SelStr := 'selected ';
  Add(Format('<option %svalue="%s">%s</option>', [SelStr, AValue, AText]));
end;

procedure TcaHTML.SubmitButton(const AName, ACaption: String);
begin
  Add(Format('<input type=submit name="%s" value="%s">', [AName, ACaption]));
end;

procedure TcaHTML.Table(AWidth: Integer; AUsesPercent: Boolean = False; ABorder: Integer = 0; AHeight: Integer = 0; AExtraHTML: String = '');
var
  TblStr: String;
  PctStr: String;
begin
  TblStr := '<table ';
  PctStr := '';
  if AUsesPercent then PctStr := '%';
  if AWidth <> 0 then TblStr := TblStr + Format('width="%d%s" ', [AWidth, PctStr]);
  TblStr := TblStr + Format('border="%d" ', [ABorder]);
  if AHeight <> 0 then TblStr := TblStr + Format('height="%d" ', [AHeight]);
  TblStr := Trim(TblStr) + AExtraHTML + '>';
  Add(TblStr);
end;

procedure TcaHTML.UnOrderedList;
begin
  Add('<ul>');
end;

procedure TcaHTML.Underline;
begin
  Add('<u>');
end;

 //----------------------------------------------------------------------------
 // TcaWebPage
 //----------------------------------------------------------------------------

constructor TcaWebPage.Create;
begin
  inherited;
  FPage := TcaHTML.Create;
  FLAlign := haRight;
  FLWidth := 300;
  FRAlign := haLeft;
  FRWidth := 200;
  FAppName := LowerCase(Utils.AppName);
  FHiddenFields := TStringList.Create;
end;

destructor TcaWebPage.Destroy;
begin
  FPage.Free;
  FHiddenFields.Free;
  inherited;
end;

procedure TcaWebPage.BeginPage(const ATitle: String; AFontNumber: TcaFontNumber = 0; AKillFocusRect: Boolean = False);
begin
  FPage.Clear;
  FPage.HTMLHead(CleanText(ATitle));
  if FHoverColor <> '' then
    begin
      FPage.Add('<style type="text/css">');
      FPage.Add('a:link, a:visited {text-decoration: none}');
      FPage.Add(Format('a:hover {background: %s}', [FHoverColor]));
      FPage.Add('a:focus {outline: none}');
      FPage.Add('</style>');
    end;
  if AKillFocusRect then
    begin
      FPage.Add('<script>');
      FPage.Add('<!-- cloaking enabled');
      FPage.Add('function ssh() {');
      FPage.Add('window.focus(); }');
      FPage.Add('// cloaking disabled -->');
      FPage.Add('</script>');
    end;
  FPage.Body(FBackColor, FBackImage, FOnLoad, FLinkColor, FLinkVisitedColor, FLinkActiveColor, FTextColor);
  if AFontNumber > 0 then FPage.Font(AFontNumber);
  if ATitle <> '' then FPage.Para(CleanText(ATitle));
  if AFontNumber > 0 then EndFont;
end;

procedure TcaWebPage.EndPage;
begin
  FPage.EndBody;
  FPage.EndHTML;
end;

procedure TcaWebPage.BeginTable(AWidth: Integer; AUsesPercent: Boolean = False; ABorder: Integer = 0; AHeight: Integer = 0);
var
  ExtraHTML: String;
begin
  if FDebugTables then ABorder := 1;
  ExtraHTML := '';
  DoGetTableExtraHTML(ExtraHTML);
  FPage.Table(AWidth, AUsesPercent, ABorder, AHeight, ExtraHTML);
end;

procedure TcaWebPage.EndTable;
begin
  FPage.EndTable;
end;

procedure TcaWebPage.BeginSelect(const ACaption, AName, AHelp: String; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Row;
  FPage.Data(FLWidth, 0, FLAlign, MakeHelpLink(ACaption, AHelp), AFontNumber);
  FPage.Data(FRWidth, 0);
  FPage.Select(AName, 1);
end;

procedure TcaWebPage.Select(const AText, AValue: String; ASelected: Boolean = False);
begin
  FPage.SelectOption(CleanText(AText), AValue, ASelected);
end;

procedure TcaWebPage.EndSelect;
begin
  FPage.EndSelect;
  FPage.EndData;
  FPage.EndRow;
end;

procedure TcaWebPage.BeginRadio(const ACaption, AName, AHelp: String; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Row;
  FPage.Data(FLWidth, 0, FLAlign, MakeHelpLink(ACaption, AHelp), AFontNumber);
  FPage.Data(FRWidth);
  FRadioName := AName;
end;

procedure TcaWebPage.Radio(const AText, AValue: String; AChecked: Boolean; AFontNumber: TcaFontNumber = 0);
begin
  FPage.RadioOption(FRadioName, CleanText(AText), AValue, AChecked, AFontNumber);
end;

procedure TcaWebPage.EndRadio;
begin
  FPage.EndData;
  FPage.EndRow;
end;

procedure TcaWebPage.BlankRow;
begin
  FPage.Row;
  FPage.Data(FLWidth, 0, FLAlign, '_');
  FPage.Data(FRWidth, 0, FRAlign, '_');
  FPage.EndRow;
end;

procedure TcaWebPage.LeftText(const AText: String; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Row;
  FPage.Data(FLWidth, 0, FLAlign, CleanText(AText), AFontNumber);
  FPage.Data(FRWidth, 0, FRAlign, '_');
  FPage.EndRow;
end;

procedure TcaWebPage.RightText(const AText: String; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Row;
  FPage.Data(FLWidth, 0, FLAlign, '_');
  FPage.Data(FRWidth, 0, FRAlign, CleanText(AText), AFontNumber);
  FPage.EndRow;
end;

procedure TcaWebPage.DoubleText(const ALeftText, ARightText: String; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Row;
  FPage.Data(FLWidth, 0, FLAlign, CleanText(ALeftText), AFontNumber);
  FPage.Data(FRWidth, 0, FRAlign, CleanText(ARightText), AFontNumber);
  FPage.EndRow;
end;

procedure TcaWebPage.Para(const AText: String = ''; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Para(CleanText(AText), AFontNumber);
end;

function TcaWebPage.CleanText(const AText: String): String;
var
  S: String;
begin
  S := StringReplace(AText, '%20', '&nbsp;', [rfReplaceAll]);
  Result := StringReplace(S, '_', '&nbsp;', [rfReplaceAll]);
end;

function TcaWebPage.CleanURL(const AText: String): String;
begin
  Result := StringReplace(AText, ' ', '%20', [rfReplaceAll]);
end;

procedure TcaWebPage.BeginText;
begin
  FText := '';
end;

procedure TcaWebPage.EndText(AFontNumber: TcaFontNumber = 0);
begin
  FPage.Font(AFontNumber);
  FPage.Add(FText);
  FPage.EndFont;
  FText := '';
end;

procedure TcaWebPage.AddText(const AText: String);
begin
  FText := FText + ' ' + AText;
end;

procedure TcaWebPage.BeginForm;
begin
  Page.Form(PageName, '/' + FScriptsFolder + '/' + Utils.AppName);
end;

procedure TcaWebPage.BuildLinkText(const AText: String; AFontNumber: TcaFontNumber = 0);
begin
  Page.Bold;
  Page.Underline;
  TextLine(AText, AFontNumber);
  Page.EndUnderline;
  Page.EndBold;
end;

procedure TcaWebPage.BuildBackLink(const AText: String);
begin
  BeginTable(360);
  BlankRow;
  FPage.Row;
  FPage.Data(120);
  FPage.EndData;
  FPage.Data(120);
  FPage.HRef('javascript:history.back();');
  BuildLinkText(AText, 3);
  FPage.EndHRef;
  FPage.EndData;
  FPage.EndRow;
  EndTable;
end;

procedure TcaWebPage.BuildLink(const AText, AFromSuffix, AToSuffix, AExtra: String);
var
  FromHRef: String;
  ToHRef: String;
begin
  BeginTable(360);
  BlankRow;
  FPage.Row;
  FPage.Data(120);
  FPage.EndData;
  FPage.Data(120);
  FromHRef := Utils.AppName + '?pagefrom=' + PageName + AFromSuffix;
  ToHRef := '&pageto=' + PageName + AToSuffix;
  FPage.HRef(FromHRef + ToHRef + '&' + AExtra);
  BuildLinkText(AText, 3);
  FPage.EndHRef;
  FPage.EndData;
  FPage.EndRow;
  EndTable;
end;

procedure TcaWebPage.BuildLinkImage(const AImage, AURL, ATarget, AStatus, AAlt: String;
                                    AUseRow: Boolean = True; AUseTable: Boolean = True);
var
  OnMouseOver: String;
  OnMouseOut: String;
begin
  if AUseTable then BeginTable(500, False, 0, 40);
  if AUseRow then Page.Row;
  Page.Data(500, 0, haRight, '', 0, '', haMiddle, False);
  OnMouseOver := Format('window.status=''%s''; return true;', [AStatus]);
  OnMouseOut := 'window.status=''''';
  Page.HRef(AURL, ATarget, OnMouseOver, OnMouseOut);
  Page.Image(AImage, 176, 40, 0, '', haBottom, AAlt);
  Page.EndHRef;
  Page.EndData;
  if AUseRow then Page.EndRow;
  if AUseTable then EndTable;
end;

function TcaWebPage.BuildLinkURL(const AFromSuffix, AToSuffix, AExtra: String): String;
var
  FromHRef: String;
  ToHRef: String;
begin
  FromHRef := Utils.AppName + '?pagefrom=' + PageName + AFromSuffix;
  ToHRef := '&pageto=' + PageName + AToSuffix;
  Result := FromHRef + ToHRef + '&' + AExtra;
end;

procedure TcaWebPage.BuildSubmitLink(const AText, AToPage: String);
begin
  FPage.HiddenField('pagefrom', PageName);
  FPage.HiddenField('pageto', AToPage);
  BeginTable(360);
  BlankRow;
  FPage.Row;
  FPage.Data(120);
  FPage.EndData;
  FPage.Data(120);
  FPage.HRef('javascript:document.forms.' + PageName + '.submit();');
  BuildLinkText(AText, 3);
  FPage.EndHRef;
  FPage.EndData;
  FPage.EndRow;
  EndTable;
end;

procedure TcaWebPage.TextLine(const AText: String; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Font(AFontNumber);
  FPage.Add(AText);
  FPage.EndFont;
end;

procedure TcaWebPage.TextArea(const AName, AValue: String; ACols, ARows: Integer; AFontNumber: TcaFontNumber = 0);
begin
  FPage.Row;
  FPage.Data(FLWidth);
  FPage.TextArea(AName, AValue, ACols, ARows, AFontNumber);
  FPage.EndData;
  FPage.Data(FRWidth, 0, FRAlign, '_');
  FPage.EndRow;
end;

procedure TcaWebPage.EndFont;
begin
  FPage.EndFont;
end;

function TcaWebPage.GetHTML: String;
begin
  BuildPage;
  ProcessTags;
  Result := FPage.Text;
end;

function TcaWebPage.MakeHelpLink(const ACaption, AHelp: String): String;
var
  FatURL: String;
begin
  if AHelp <> '' then
    begin
      FatURL := Format('?HelpName=%s&HelpText=%s', [CleanURL(ACaption), CleanURL(AHelp)]);
      Result := '<A HREF=' + FAppName + FatURL + '>' + ACaption + '</A>';
    end
  else
    Result := ACaption;
end;

procedure TcaWebPage.BuildPage;
begin
  // Virtual
end;

procedure TcaWebPage.DoGetTableExtraHTML(var AExtraHTML: String);
begin
  // Virtual
end;

function TcaWebPage.GetFont1: TcaHTMLFont;
begin
  Result := FPage.Font1;
end;

function TcaWebPage.GetFont2: TcaHTMLFont;
begin
  Result := FPage.Font2;
end;

function TcaWebPage.GetFont3: TcaHTMLFont;
begin
  Result := FPage.Font3;
end;

procedure TcaWebPage.ProcessTags;
var
  Index: Integer;
  Tag1: Integer;
  Tag2: Integer;
  Line: String;
  TagStr: String;
  TagStrTnT: String;
  NewStrTnT: String;
begin
  for Index := 0 to FPage.Count - 1 do
    begin
      Line := FPage[Index];
      FindTags(Line, Tag1, Tag2);
      while (Tag1 > 0) and (Tag2 > 0) do
        begin
          TagStr := Copy(Line, Tag1, Tag2 - Tag1 + 1);
          TagStrTnT := TagStr;
          System.Delete(TagStrTnT, 1, 1);
          SetLength(TagStrTnT, Length(TagStrTnT) - 1);
          NewStrTnT := TagStrTnT;
          DoProcessTag(TagStrTnT, NewStrTnT);
          Line := StringReplace(Line, TagStr, NewStrTnT, []);
          FindTags(Line, Tag1, Tag2);
        end;
      FPage[Index] := Line;
    end;
end;

procedure TcaWebPage.FindTags(const S: String; var Tag1, Tag2: Integer);
var
  Index: Integer;
begin
  Tag1 := 0;
  Tag2 := 0;
  for Index := 1 to Length(S) do
    begin
      if S[Index] = '~' then
        begin
          if Tag1 = 0 then
            Tag1 := Index
          else
            begin
              Tag2 := Index;
              Break;
            end;
        end;
    end;
end;

procedure TcaWebPage.DoProcessTag(const TagStr: String; var NewStr: String);
begin
  if Assigned(FOnProcessTag) then FOnProcessTag(Self, TagStr, NewStr);
end;

procedure TcaWebPage.EndForm;
begin
  FPage.EndForm;
end;

procedure TcaWebPage.EndPara;
begin
  FPage.EndPara;
end;

function TcaWebPage.GetPageName: String;
begin
  Result := '';
end;

procedure TcaWebPage.LoadFromFile(const AFileName: String);
begin
  FPage.LoadFromFile(Utils.AppPath + AFileName);
end;

function TcaWebPage.GetDefaultCellHeight: Integer;
begin
  Result := FPage.DefaultCellHeight;
end;

procedure TcaWebPage.SetDefaultCellHeight(const Value: Integer);
begin
  FPage.DefaultCellHeight := Value;
end;

end.
