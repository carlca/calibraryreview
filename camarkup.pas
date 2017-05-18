unit caMarkup;

{$INCLUDE ca.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,

  caClasses,
  caUtils,
  // caUtils,
  caGraphics;

type

 //---------------------------------------------------------------------------
 // IcaHTMLElement
 //---------------------------------------------------------------------------

  IcaHTMLElement = interface
  ['{EA991982-EC63-4D17-9DF8-FD373629DB4D}']
    // Property methods
    function GetAscent: Integer;
    function GetBeginText: String;
    function GetBounds: TRect;
    function GetBreakLine: Boolean;
    function GetEndText: String;
    function GetFontColor: TColor;
    function GetFontName: String;
    function GetFontSize: Integer;
    function GetFontStyle: TFontStyles;
    function GetHeight: Integer;
    function GetText: String;
    function GetWidth: Integer;
    procedure SetAscent(const Value: Integer);
    procedure SetBeginText(const Value: String);
    procedure SetBounds(const Value: TRect);
    procedure SetBreakLine(const Value: Boolean);
    procedure SetEndText(const Value: String);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontName(const Value: String);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetHeight(const Value: Integer);
    procedure SetText(const Value: String);
    procedure SetWidth(const Value: Integer);
    // Public methods
    procedure Break(ACanvas: TCanvas; AvailableWidth: Integer);
    // Properties
    property Ascent: Integer read GetAscent write SetAscent;
    property BeginText: String read GetBeginText write SetBeginText;
    property Bounds: TRect read GetBounds write SetBounds;
    property BreakLine: Boolean read GetBreakLine write SetBreakLine;
    property EndText: String read GetEndText write SetEndText;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property Height: Integer read GetHeight write SetHeight;
    property Text: String read GetText write SetText;
    property Width: Integer read GetWidth write SetWidth;
  end;

 //---------------------------------------------------------------------------
 // TcaHTMLElement
 //---------------------------------------------------------------------------

  TcaHTMLElement = class(TInterfacedObject, IcaHTMLElement)
  private
    FAscent: Integer;
    FBeginText: String;
    FBounds: TRect;
    FBreakLine: Boolean;
    FEndText: String;
    FFontColor: TColor;
    FFontName: String;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FHeight: Integer;
    FText: String;
    FWidth: Integer;
    function GetAscent: Integer;
    function GetBeginText: String;
    function GetBounds: TRect;
    function GetBreakLine: Boolean;
    function GetEndText: String;
    function GetFontColor: TColor;
    function GetFontName: String;
    function GetFontSize: Integer;
    function GetFontStyle: TFontStyles;
    function GetHeight: Integer;
    function GetText: String;
    function GetWidth: Integer;
    procedure SetAscent(const Value: Integer);
    procedure SetBeginText(const Value: String);
    procedure SetBounds(const Value: TRect);
    procedure SetBreakLine(const Value: Boolean);
    procedure SetEndText(const Value: String);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontName(const Value: String);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetHeight(const Value: Integer);
    procedure SetText(const Value: String);
    procedure SetWidth(const Value: Integer);
  public
    procedure Break(ACanvas: TCanvas; AvailableWidth: Integer);
    property Ascent: Integer read GetAscent write SetAscent;
    property BeginText: String read GetBeginText write SetBeginText;
    property Bounds: TRect read GetBounds write SetBounds;
    property BreakLine: Boolean read GetBreakLine write SetBreakLine;
    property EndText: String read GetEndText write SetEndText;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property Height: Integer read GetHeight write SetHeight;
    property Text: String read GetText write SetText;
    property Width: Integer read GetWidth write SetWidth;
  end;

 //---------------------------------------------------------------------------
 // IcaHTMLElementList
 //---------------------------------------------------------------------------

  IcaHTMLElementList = interface
  ['{BA19C11B-22A6-46A8-A945-648AD4AD2ED0}']
    // Property methods
    function GetItem(Index: Integer): TcaHTMLElement;
    procedure SetItem(Index: Integer; const Item: TcaHTMLElement);
    // Public methods
    function Add(Item: TcaHTMLElement): Integer;
    function Extract(Item: TcaHTMLElement): TcaHTMLElement;
    function First: TcaHTMLElement;
    function IndexOf(Item: TcaHTMLElement): Integer;
    function Last: TcaHTMLElement;
    function Remove(Item: TcaHTMLElement): Integer;
    procedure Insert(Index: Integer; Item: TcaHTMLElement);
    // Properties
    property Items[Index: Integer]: TcaHTMLElement read GetItem write SetItem;
  end;

 //---------------------------------------------------------------------------
 // IcaHTMLElementStack
 //---------------------------------------------------------------------------

  IcaHTMLElementStack = interface
  ['{22A74502-8838-4A61-A267-27CDFD9CF6CE}']
    procedure Push(Item: TcaHTMLElement);
    function Pop: TcaHTMLElement;
    function Peek: TcaHTMLElement;
  end;

 //---------------------------------------------------------------------------
 // TcaHTMLElementList
 //---------------------------------------------------------------------------

  TcaHTMLElementList = class(TInterfacedObject, IcaList, IcaHTMLElementList, IcaHTMLElementStack)
  private
    FListBase: IcaList;
    function GetItem(Index: Integer): TcaHTMLElement;
    procedure SetItem(Index: Integer; const Value: TcaHTMLElement);
  public
    constructor Create;
    function Add(Item: TcaHTMLElement): Integer;
    function Extract(Item: TcaHTMLElement): TcaHTMLElement;
    function First: TcaHTMLElement;
    function IndexOf(Item: TcaHTMLElement): Integer;
    function Last: TcaHTMLElement;
    function Peek: TcaHTMLElement;
    function Pop: TcaHTMLElement;
    function Remove(Item: TcaHTMLElement): Integer;
    procedure Push(Item: TcaHTMLElement);
    procedure Insert(Index: Integer; Item: TcaHTMLElement);
    property Items[Index: Integer]: TcaHTMLElement read GetItem write SetItem;
    property ListBase: IcaList read FListBase implements IcaList;
  end;

 //---------------------------------------------------------------------------
 // IcaMarkupViewer
 //---------------------------------------------------------------------------

  IcaMarkupViewer = interface
  ['{4DAB603A-C02D-4C0C-BABD-8EC67DCDDC20}']
    // Property methods
    function GetColor: TColor;
    function GetLines: TStrings;
    function GetMarginLeft: Integer;
    function GetMarginRight: Integer;
    function GetMarginTop: Integer;
    function GetMouseOverElement: TcaHTMLElement;
    procedure SetColor(const Value: TColor);
    procedure SetLines(const Value: TStrings);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
    procedure SetMouseOverElement(const Value: TcaHTMLElement);
    // Properties
    property Color: TColor read GetColor write SetColor;
    property Lines: TStrings read GetLines write SetLines;
    property MarginLeft: Integer read GetMarginLeft write SetMarginLeft;
    property MarginRight: Integer read GetMarginRight write SetMarginRight;
    property MarginTop: Integer read GetMarginTop write SetMarginTop;
    property MouseOverElement: TcaHTMLElement read GetMouseOverElement write SetMouseOverElement;
  end;

 //---------------------------------------------------------------------------
 // TcaCustomMarkupViewer
 //---------------------------------------------------------------------------

  TcaCustomMarkupViewer = class(TCustomControl, IcaMarkupViewer)
  private
    FBitmap: TBitmap;
    FColor: TColor;
    FElementStack: IcaHTMLElementStack;
    FFrameBottom: Integer;
    FFrameTop: Integer;
    FLines: TStrings;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    FMouseOverElement: TcaHTMLElement;
    FPageBottom: Integer;
    FScrollBar: TScrollbar;
    FTagStack: IcaHTMLElementStack;
    // Property methods
    function GetColor: TColor;
    function GetLines: TStrings;
    function GetMarginLeft: Integer;
    function GetMarginRight: Integer;
    function GetMarginTop: Integer;
    function GetMouseOverElement: TcaHTMLElement;
    procedure SetColor(const Value: TColor);
    procedure SetLines(const Value: TStrings);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
    procedure SetMouseOverElement(const Value: TcaHTMLElement);
    // Private methods
    function GetElement(Index: Integer): TcaHTMLElement;
    function GetElementCount: Integer;
    procedure ClearBreakText;
    procedure LinesChanged;
    procedure LinesChangedEvent(Sender: TObject);
    procedure ParseHTML;
    procedure RenderHTML;
    procedure ScrollViewerEvent(Sender: TObject);
    procedure UpdateConstraints;
    procedure UpdateElementDimensions;
    procedure UpdateMouseOverElement(X, Y: Integer);
    // Message handlers
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    property Color: TColor read GetColor write SetColor;
    property Lines: TStrings read GetLines write SetLines;
    property MarginLeft: Integer read GetMarginLeft write SetMarginLeft;
    property MarginRight: Integer read GetMarginRight write SetMarginRight;
    property MarginTop: Integer read GetMarginTop write SetMarginTop;
    property MouseOverElement: TcaHTMLElement read GetMouseOverElement write SetMouseOverElement;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

 //---------------------------------------------------------------------------
 // TcaMarkupViewer
 //---------------------------------------------------------------------------

  TcaMarkupViewer = class(TcaCustomMarkupViewer)
  public
    property MouseOverElement;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property Lines;
    property MarginLeft;
    property MarginRight;
    property MarginTop;
    property OnMouseMove;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaHTMLElement
 //---------------------------------------------------------------------------

procedure TcaHTMLElement.Break(ACanvas: TCanvas; AvailableWidth: Integer);
var
  S: String;
  Index: Integer;
  Wth: Integer;
begin
  ACanvas.Font.Name := FFontName;
  ACanvas.Font.Size := FFontSize;
  ACanvas.Font.Style := FFontStyle;
  ACanvas.Font.Color := FFontColor;
  if FBeginText = '' then
    S := FText
  else
    S := FEndText;
  if ACanvas.TextWidth(S) <= AvailableWidth then
    begin
      FBeginText := S;
      FEndText := '';
    end
  else
    begin
      for Index := Length(S) downto 1 do
        begin
          if S[Index] = #32 then
            begin
              Wth := ACanvas.TextWidth(Copy(S, 1, Index));
              if Wth <= AvailableWidth then
                begin
                  FBeginText := Copy(S, 1, Index);
                  FEndText := Copy(S, Index + 1, Length(S));
                  System.Break;
                end;
            end;
        end;
    end;
end;

function TcaHTMLElement.GetAscent: Integer;
begin
  Result := FAscent;
end;

function TcaHTMLElement.GetBeginText: String;
begin
  Result := FBeginText;
end;

function TcaHTMLElement.GetBounds: TRect;
begin
  Result := FBounds;
end;

function TcaHTMLElement.GetBreakLine: Boolean;
begin
  Result := FBreakLine;
end;

function TcaHTMLElement.GetEndText: String;
begin
  Result := FEndText;
end;

function TcaHTMLElement.GetFontColor: TColor;
begin
  Result := FFontColor;
end;

function TcaHTMLElement.GetFontName: String;
begin
  Result := FFontName;
end;

function TcaHTMLElement.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TcaHTMLElement.GetFontStyle: TFontStyles;
begin
  Result := FFontStyle;
end;

function TcaHTMLElement.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TcaHTMLElement.GetText: String;
begin
  Result := FText;
end;

function TcaHTMLElement.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TcaHTMLElement.SetAscent(const Value: Integer);
begin
  FAscent := Value;
end;

procedure TcaHTMLElement.SetBeginText(const Value: String);
begin
  FBeginText := Value;
end;

procedure TcaHTMLElement.SetBounds(const Value: TRect);
begin
  FBounds := Value;
end;

procedure TcaHTMLElement.SetBreakLine(const Value: Boolean);
begin
  FBreakLine := Value;
end;

procedure TcaHTMLElement.SetEndText(const Value: String);
begin
  FEndText := Value;
end;

procedure TcaHTMLElement.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
end;

procedure TcaHTMLElement.SetFontName(const Value: String);
begin
  FFontName := Value;
end;

procedure TcaHTMLElement.SetFontSize(const Value: Integer);
begin
  FFontSize := Value;
end;

procedure TcaHTMLElement.SetFontStyle(const Value: TFontStyles);
begin
  FFontStyle := Value;
end;

procedure TcaHTMLElement.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure TcaHTMLElement.SetText(const Value: String);
begin
  FText := Value;
end;

procedure TcaHTMLElement.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

 //---------------------------------------------------------------------------
 // TcaHTMLElementList
 //---------------------------------------------------------------------------

constructor TcaHTMLElementList.Create;
begin
  inherited;
  FListBase := TcaList.Create;
end;

function TcaHTMLElementList.Add(Item: TcaHTMLElement): Integer;
begin
  Result := FListBase.List.Add(Pointer(Item));
end;

function TcaHTMLElementList.Extract(Item: TcaHTMLElement): TcaHTMLElement;
begin
  Result := TcaHTMLElement(FListBase.List.Extract(Pointer(Item)));
end;

function TcaHTMLElementList.First: TcaHTMLElement;
begin
  Result := TcaHTMLElement(FListBase.List.First);
end;

function TcaHTMLElementList.GetItem(Index: Integer): TcaHTMLElement;
begin
  Result := TcaHTMLElement(FListBase.List.Items[Index]);
end;

function TcaHTMLElementList.IndexOf(Item: TcaHTMLElement): Integer;
begin
  Result := FListBase.List.IndexOf(Pointer(Item));
end;

procedure TcaHTMLElementList.Insert(Index: Integer;
  Item: TcaHTMLElement);
begin
  FListBase.List.Insert(Index, Pointer(Item));
end;

function TcaHTMLElementList.Last: TcaHTMLElement;
begin
  Result := TcaHTMLElement(FListBase.List.Last);
end;

function TcaHTMLElementList.Peek: TcaHTMLElement;
begin
  if FListBase.Count = 0 then
    Result := nil
  else
    Result := Last;
end;

function TcaHTMLElementList.Pop: TcaHTMLElement;
begin
  Result := Peek;
  if Result <> nil then
    FListBase.Delete(FListBase.Count - 1);
end;

procedure TcaHTMLElementList.Push(Item: TcaHTMLElement);
begin
  Add(Item);
end;

function TcaHTMLElementList.Remove(Item: TcaHTMLElement): Integer;
begin
  Result := FListBase.List.Remove(Pointer(Item));
end;

procedure TcaHTMLElementList.SetItem(Index: Integer;
  const Value: TcaHTMLElement);
begin
  FListBase.List.Items[Index] := Pointer(Value);
end;

 //---------------------------------------------------------------------------
 // TcaCustomMarkupViewer
 //---------------------------------------------------------------------------

constructor TcaCustomMarkupViewer.Create(AOwner: TComponent);
begin
  inherited;
  FElementStack := TcaHTMLElementList.Create;
  FTagStack := TcaHTMLElementList.Create;
  Width := 300;
  Height := 275;
  FMarginLeft := 5;
  FMarginRight := 5;
  FMargintop := 5;
  FColor := clWindow;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChangedEvent;
end;

procedure TcaCustomMarkupViewer.CreateWnd;
begin
  inherited;
  FScrollBar := TScrollBar.create(Self);
  FScrollBar.Kind := sbVertical;
  FScrollBar.Parent := Self;
  FScrollBar.Align := alRight;
  FScrollBar.Min := 0;
  FScrollBar.Max := 0;
  FScrollBar.OnChange := ScrollViewerEvent;
  FFrameTop := 0;
  FFrameBottom := ClientHeight;
  FBitmap := TBitmap.Create;
  FBitmap.Width := ClientWidth - FScrollBar.Width;
  FBitmap.Height := ClientHeight;
end;

destructor TcaCustomMarkupViewer.Destroy;
begin
  FBitmap.Free;
  FLines.Free;
  inherited;
end;

procedure TcaCustomMarkupViewer.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TcaCustomMarkupViewer.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverElement := nil;
end;

function TcaCustomMarkupViewer.GetColor: TColor;
begin
  Result := FColor;
end;

function TcaCustomMarkupViewer.GetLines: TStrings;
begin
  Result := FLines;
end;

function TcaCustomMarkupViewer.GetMarginLeft: Integer;
begin
  Result := FMarginLeft;
end;

function TcaCustomMarkupViewer.GetMarginRight: Integer;
begin
  Result := FMarginRight;
end;

function TcaCustomMarkupViewer.GetMarginTop: Integer;
begin
  Result := FMarginTop;
end;

procedure TcaCustomMarkupViewer.ScrollViewerEvent(Sender: TObject);
begin
  FFrameTop := FScrollBar.Position;
  FFrameBottom := FFrameTop + ClientHeight - 1;
  if GetElementCount > 0 then RenderHTML;
  UpdateConstraints;
  Canvas.Draw(0, 0, FBitmap);
end;

procedure TcaCustomMarkupViewer.UpdateElementDimensions;
var
  Index: Integer;
  Element: TcaHTMLElement;
  ElementStr: String;
  TextMetrics: TTextMetric;
begin
  for Index := 0 to GetElementCount - 1 do
    begin
      Element := GetElement(Index);
      ElementStr := Element.Text;
      Canvas.Font.Name := Element.FontName;
      Canvas.Font.Size := Element.FontSize;
      Canvas.Font.Style := Element.FontStyle;
      Canvas.Font.Color := Element.FontColor;
      GetTextMetrics(Canvas.Handle, TextMetrics);
      Element.Height := TextMetrics.tmHeight;
      Element.Ascent := TextMetrics.tmAscent;
      Element.Width := Canvas.TextWidth(ElementStr);
    end;
end;

procedure TcaCustomMarkupViewer.ClearBreakText;
var
  Index: Integer;
  Element: TcaHTMLElement;
begin
  for Index := 0 to GetElementCount - 1 do
    begin
      Element := GetElement(Index);
      Element.BeginText := '';
      Element.EndText := '';
    end;
end;

function TcaCustomMarkupViewer.GetElementCount: Integer;
begin
  Result := (FElementStack as IcaList).Count;
end;

function TcaCustomMarkupViewer.GetElement(Index: Integer): TcaHTMLElement;
begin
  Result := (FElementStack as IcaHTMLElementList).Items[Index];
end;

function TcaCustomMarkupViewer.GetMouseOverElement: TcaHTMLElement;
begin
  Result := FMouseOverElement;
end;

procedure TcaCustomMarkupViewer.Paint;
var
  OffPageHeight: Integer;
  MathUtils: IcaMathUtils;
begin
  MathUtils := Utils as IcaMathUtils;
  FBitmap.Width := ClientWidth - FScrollBar.Width;
  FBitmap.Height := ClientHeight;
  if GetElementCount > 0 then RenderHTML;
  UpdateConstraints;
  Canvas.Draw(0, 0, FBitmap);
  FScrollBar.Min := 0;
  OffPageHeight := FPageBottom - ClientHeight;
  if OffPageHeight > 0 then
    FScrollBar.Max := OffPageHeight
  else
    FScrollBar.Max := 0;
  FScrollBar.Position := 0;
  FScrollBar.LargeChange := MathUtils.Trunc(0.8 * ClientHeight);
end;

procedure TcaCustomMarkupViewer.RenderHTML;
var
  BaseLine: Integer;
  BreakPos: Integer;
  Element: TcaHTMLElement;
  Index: Integer;
  EmtCount: Integer;
  IsEndOfLine: Boolean;
  LineEnd: Integer;
  LineStart: Integer;
  MaxAscent: Integer;
  MaxHeight: Integer;
  PendingBreak: boolean;
  R: TRect;
  XPos: Integer;
  YPos: Integer;

  procedure SetFont(AElement: TcaHTMLElement);
  begin
    FBitmap.Canvas.Font.Name := AElement.FontName;
    FBitmap.Canvas.Font.Size := AElement.FontSize;
    FBitmap.Canvas.Font.Style := AElement.FontStyle;
    FBitmap.Canvas.Font.Color := AElement.FontColor;
  end;

  procedure RenderString(AElement: TcaHTMLElement);
  var
    S: String;
    Wth: Integer;
    Hgt: Integer;
    R: TRect;
    TextYPos: Integer;
  begin
    SetFont(AElement);
    if AElement.BeginText <> '' then
      begin
        S := AElement.BeginText;
        Wth := FBitmap.Canvas.TextWidth(S);
        Hgt := FBitmap.Canvas.TextHeight(S);
        TextYPos := YPos + BaseLine - AElement.Ascent - FFrameTop;
        FBitmap.Canvas.TextOut(XPos, TextYPos, S);
        R := Rect(XPos, TextYPos, XPos + Wth, TextYPos + Hgt);
        AElement.Bounds := R;
        Inc(XPos, Wth);
      end;
  end;

begin
  LineEnd := 0;
  R := Rect(0, 0, FBitmap.Width, FBitmap.Height);
  FBitmap.Canvas.Brush.Color := Color;
  FBitmap.Canvas.FillRect(R);
  ClearBreakText;
  FBitmap.Canvas.Brush.Style := bsClear;
  YPos := FMarginTop;
  LineStart := 0;
  PendingBreak := False;
  EmtCount := GetElementCount;
  repeat
    Index := LineStart;
    BreakPos := FBitmap.Width - FMarginRight;
    MaxHeight := 0;
    MaxAscent := 0;
    IsEndOfLine := False;
    repeat
      Element := GetElement(Index);
      if Element.BreakLine then
        begin
          if not PendingBreak then
            begin
              PendingBreak := True;
              IsEndOfLine := True;
              LineEnd := Index - 1;
            end
          else
            PendingBreak := False;
        end;
      if not PendingBreak then
        begin
          if Element.Height > MaxHeight then MaxHeight := Element.Height;
          if Element.Ascent > MaxAscent then MaxAscent := Element.Ascent;
          Element.Break(FBitmap.Canvas, BreakPos);
          if Element.BeginText <> '' then
            begin
              BreakPos := BreakPos - FBitmap.Canvas.TextWidth(Element.BeginText);
              if Element.EndText = '' then
                begin
                  if Index >= EmtCount - 1 then
                    begin
                      IsEndOfLine := True;
                      LineEnd := Index;
                    end
                  else
                    begin
                      Inc(Index);
                    end
                end
              else
                begin
                  IsEndOfLine := True;
                  LineEnd := Index;
                end;
            end
          else
            begin
              IsEndOfLine := True;
              LineEnd := Index;
            end;
        end;
    until IsEndOfLine;

    XPos := FMarginLeft;
    BaseLine := MaxAscent;
    if (YPos + MaxHeight >= FFrameTop) and (YPos <= FFrameBottom) then
      for Index := LineStart to LineEnd do
        begin
          Element := GetElement(Index);
          RenderString(Element);
        end;
    YPos := YPos + MaxHeight;
    if not Pendingbreak then
      LineStart := LineEnd
    else
      LineStart := LineEnd + 1;
  until (LineEnd >= EmtCount - 1) and (Element.EndText = '');
  FPageBottom := YPos;
end;

procedure TcaCustomMarkupViewer.ParseHTML;
var
  AText: String;
  BreakLine: boolean;
  Element: TcaHTMLElement;
  ElementColor: TColor;
  ElementText: String;
  FontColor: TColor;
  FontName: String;
  FontSize: Integer;
  FontStyle: TFontStyles;
  TagPos: Integer;

  procedure PushTag;
  begin
    Element := TcaHTMLElement.Create;
    Element.FontName := FontName;
    Element.FontSize := FontSize;
    Element.FontStyle := FontStyle;
    Element.FontColor := FontColor;
    FTagStack.Push(Element);
  end;

  procedure PopTag;
  begin
    Element := FTagStack.Pop;
    if Element <> nil then
      begin
        FontName := Element.FontName;
        FontSize := Element.FontSize;
        FontStyle := Element.FontStyle;
        FontColor := Element.FontColor;
        Element.Free;
      end;
  end;

  procedure PushElement;
  begin
    Element := TcaHTMLElement.Create;
    Element.Text := ElementText;
    Element.FontName := FontName;
    Element.FontSize := FontSize;
    Element.FontStyle := FontStyle;
    Element.FontColor := FontColor;
    Element.BreakLine := BreakLine;
    BreakLine := False;
    FElementStack.Push(Element);
  end;

  procedure ParseTag(ParseText: String);
  var
    HasParams: Boolean;
    OldFontSize: Integer;
    P: Integer;
    Param: String;
    S: String;
    TagStr: String;
    TagValue: String;
  begin
    S := Trim(ParseText);
    HasParams := False;
    P := Pos(' ', S);
    if P = 0 then
      TagStr := S   // tag only
    else
      begin // tag + attributes
        TagStr := Copy(S, 1, P - 1);
        S := Trim(Copy(S, P + 1, length(S)));
        HasParams := True;
      end;
      // handle TagStr
    TagStr := Lowercase(TagStr);
    if TagStr = 'br' then
      BreakLine := True
    else if TagStr = 'b' then
      begin // bold
        PushTag;
        FontStyle := FontStyle + [fsbold];
      end
    else if TagStr = '/b' then
      begin // cancel bold
        FontStyle := FontStyle - [fsbold];
        PopTag;
      end
    else if TagStr = 'i' then
      begin // italic
        PushTag;
        FontStyle := FontStyle + [fsitalic];
      end
    else if TagStr = '/i' then
      begin // cancel italic
        FontStyle := FontStyle - [fsitalic];
        PopTag;
      end
    else if TagStr = 'u' then
      begin // underline
        PushTag;
        FontStyle := FontStyle + [fsunderline];
      end
    else if TagStr = '/u' then
      begin // cancel underline
        FontStyle := FontStyle - [fsunderline];
        PopTag;
      end
    else if TagStr = 'font' then
      begin
        PushTag;
      end
    else if TagStr = '/font' then
      begin
        PopTag
      end;
    if HasParams then
      begin
        repeat
          P := Pos('="', S);
          if P > 0 then
            begin
              Param := lowercase(Trim(Copy(S, 1, P - 1)));
              delete(S, 1, P + 1);
              P := Pos('"', S);
              if P > 0 then
                begin
                  TagValue := Copy(S, 1, P - 1);
                  Delete(S, 1, P);
                  if Param = 'face' then
                    begin
                      FontName := TagValue;
                    end
                  else if Param = 'size' then
                    begin
                      OldFontSize := FontSize;
                      FontSize := StrToIntDef(TagValue, -1);
                      if FontSize = -1 then FontSize := OldFontSize;
                    end
                  else if Param = 'color' then
                    begin
                      ElementColor := Utils.HTMLColorToColor(TagValue);
                      if ElementColor <> clNone then
                        FontColor := ElementColor;
                    end;
                end;
            end;
        until P = 0;
      end;
  end;

begin
  AText := TrimRight(StringReplace(FLines.Text, #13#10, ' ', [rfReplaceAll]));
  (FElementStack as IcaList).Clear;
  (FTagStack as IcaList).Clear;
  FontStyle := [];
  FontName := 'Arial';
  FontSize := 12;
  FontColor := clWindowText;
  BreakLine := False;
  repeat
    TagPos := Pos('<', AText);
    if TagPos = 0 then
      begin
        ElementText := AText;
        PushElement;
      end
    else
      begin
        if TagPos > 1 then
          begin
            ElementText := Copy(AText, 1, TagPos - 1);
            PushElement;
            Delete(AText, 1, TagPos - 1);
          end;
        TagPos := Pos('>', AText);
        if TagPos > 0 then
          begin
            ParseTag(Copy(AText, 2, TagPos - 2));
            Delete(AText, 1, TagPos);
          end;
      end;
  until TagPos = 0;
end;

procedure TcaCustomMarkupViewer.SetColor(const Value: TColor);
begin
  if Value <> FColor then
    begin
      FColor := Value;
      Invalidate;
    end;                    
end;

procedure TcaCustomMarkupViewer.LinesChangedEvent(Sender: TObject);
begin
  LinesChanged;
end;

procedure TcaCustomMarkupViewer.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TcaCustomMarkupViewer.LinesChanged;
begin
  ParseHTML;
  UpdateElementDimensions;
  Invalidate;
end;

procedure TcaCustomMarkupViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  UpdateMouseOverElement(X, Y);
  inherited;
end;

procedure TcaCustomMarkupViewer.SetMarginLeft(const Value: Integer);
begin
  if Value <> FMarginLeft then
    begin
      FMarginLeft := Value;
      Invalidate;
    end;
end;

procedure TcaCustomMarkupViewer.SetMarginRight(const Value: Integer);
begin
  if Value <> FMarginRight then
    begin
      FMarginRight := Value;
      Invalidate;
    end;
end;

procedure TcaCustomMarkupViewer.SetMarginTop(const Value: Integer);
begin
  if Value <> FMarginTop then
    begin
      FMarginTop := Value;
      Invalidate;
    end;
end;

procedure TcaCustomMarkupViewer.SetMouseOverElement(const Value: TcaHTMLElement);
begin
  FMouseOverElement := Value;
end;

procedure TcaCustomMarkupViewer.UpdateConstraints;
var
  Index: Integer;
  Element: TcaHTMLElement;
  MaxWidth: Integer;
begin
  MaxWidth := 0;
  for Index := 0 to GetElementCount - 1 do
    begin
      Element := GetElement(Index);
      if Element.Width > MaxWidth then MaxWidth := Element.Width;
    end;
  Constraints.MinWidth := MaxWidth;
end;

procedure TcaCustomMarkupViewer.UpdateMouseOverElement(X, Y: Integer);
var
  Index: Integer;
  Element: TcaHTMLElement;
begin
  FMouseOverElement := nil;
  for Index := 0 to GetElementCount - 1 do
    begin
      Element := GetElement(Index);
      if Utils.InRect(X, Y, Element.Bounds) then
        begin
          FMouseOverElement := Element;
          Break;
        end;
    end;
end;

end.
