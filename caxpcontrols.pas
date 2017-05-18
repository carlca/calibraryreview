unit caXPControls;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  Messages,
  Controls,
  Sysutils,
  Graphics,
  Forms,
  ExtCtrls,
  Math,
  Buttons,

  // ca units 
  caConsts,
  caControls,
  caGraphics,
  caTypes,
  caClasses,
  caLog,
  caUtils,
  caFrame;

type

  //---------------------------------------------------------------------------
  // TcaXPMutex                                                                
  //---------------------------------------------------------------------------

  TcaCustomXPCheckBox = class;

  TcaXPMutex = class(TComponent)
  private
    // Private fields 
    FList: TList;
    // Published property methods 
    FOnUpdate: TNotifyEvent;
  protected
    // Protected methods 
    procedure Add(ACheckbox: TcaCustomXPCheckBox);
    procedure DoUpdate(ACheckbox: TcaCustomXPCheckBox); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure Update(ACheckbox: TcaCustomXPCheckBox);
  published
    // Published properties 
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  //---------------------------------------------------------------------------
  // TcaXPGraphicControl                                                       
  //---------------------------------------------------------------------------

  TcaXPGraphicControl = class(TcaGraphicControl)
  private
    // Property fields 
    FAlignment: TAlignment;
    FDisabledTextColor: TColor;
    FEnabledTextColor: TColor;
    FFrameColor: TColor;
    FMouseOverFrameColor: TColor;
    // Component message handlers 
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    // Property methods 
    procedure SetAlignment(const Value: TAlignment);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetEnabledTextColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetMouseOverFrameColor(const Value: TColor);
  protected
    // Protected virtual methods 
    function GetBackgroundColor: TColor; virtual;
    function GetFrameColor: TColor; virtual;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure BufferedPaint(C: TCanvas; R: TRect); override;
    // Protected properties 
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor;
    property EnabledTextColor: TColor read FEnabledTextColor write SetEnabledTextColor;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property MouseOverFrameColor: TColor read FMouseOverFrameColor write SetMouseOverFrameColor;
  public
    // lifetime...
    constructor Create(AOwner: TComponent); override;
    // public properties...
  end;

  //---------------------------------------------------------------------------
  // TcaXPButton                                                               
  //---------------------------------------------------------------------------

  TcaXPButton = class(TcaXPGraphicControl)
  private
    // Property fields 
    FGlyph: TBitmap;
    FShowGlyph: Boolean;
    // Property methods 
    procedure SetGlyph(const Value: TBitmap);
    procedure SetShowGlyph(const Value: Boolean);
  protected
    // Protected methods 
    procedure BufferedPaint(C: TCanvas; R: TRect); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // New properties 
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property ShowGlyph: Boolean read FShowGlyph write SetShowGlyph;
    // Promoted properties 
    property Action;
    property Align;
    property Alignment;
    property AutoSize;
    property AutoSizeMargin;
    property Anchors;
    property Caption;
    property DisabledTextColor;
    property Enabled;
    property EnabledTextColor;
    property Font;
    property FrameColor;
    property MouseOverFrameColor;
    property Visible;
    // Event properties 
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  //---------------------------------------------------------------------------
  // TcaCustomXPCheckBox                                                       
  //---------------------------------------------------------------------------

  TcaCustomXPCheckBox = class(TcaXPGraphicControl)
  private
    FCheckedFrameColor: TColor;
    FCheckedGlyph: TBitmap;
    FCheckedSmallGlyph: TBitmap;
    FGrayedGlyph: TBitmap;
    FGrayedSmallGlyph: TBitmap;
    FUncheckedGlyph: TBitmap;
    FUncheckedSmallGlyph: TBitmap;
    // Property fields 
    FChecked: Boolean;
    FMutex: TcaXPMutex;
    FShowGlyph: Boolean;
    FToggleWhenClicked: Boolean;
    FUseSmallGlyphs: Boolean;
    // Property methods 
    function GetChecked: Boolean;
    function GetShowGlyph: Boolean;
    function GetToggleWhenClicked: Boolean;
    procedure SetChecked(const Value: Boolean);
    procedure SetCheckedFrameColor(const Value: TColor);
    procedure SetCheckedGlyph(const Value: TBitmap);
    procedure SetCheckedSmallGlyph(const Value: TBitmap);
    procedure SetGrayedGlyph(const Value: TBitmap);
    procedure SetGrayedSmallGlyph(const Value: TBitmap);
    procedure SetShowGlyph(const Value: Boolean);
    procedure SetToggleWhenClicked(const Value: Boolean);
    procedure SetUncheckedGlyph(const Value: TBitmap);
    procedure SetUncheckedSmallGlyph(const Value: TBitmap);
    procedure SetUseSmallGlyphs(const Value: Boolean);
    // Private methods 
    function SelectGlyph: TBitmap;
    procedure CreateBitmaps;
    procedure FreeBitmaps;
  protected
    // Protected virtual methods 
    function GetAdjustSizeDelta: Integer; override;
    function GetBackgroundColor: TColor; override;
    function GetFrameColor: TColor; override;
    procedure Click; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BufferedPaint(C: TCanvas; R: TRect); override;
    // Properties 
    property Checked: Boolean read GetChecked write SetChecked;
    property CheckedFrameColor: TColor read FCheckedFrameColor write SetCheckedFrameColor;
    property CheckedGlyph: TBitmap read FCheckedGlyph write SetCheckedGlyph;
    property CheckedSmallGlyph: TBitmap read FCheckedSmallGlyph write SetCheckedSmallGlyph;
    property GrayedGlyph: TBitmap read FGrayedGlyph write SetGrayedGlyph;
    property GrayedSmallGlyph: TBitmap read FGrayedSmallGlyph write SetGrayedSmallGlyph;
    property ShowGlyph: Boolean read GetShowGlyph write SetShowGlyph;
    property ToggleWhenClicked: Boolean read GetToggleWhenClicked write SetToggleWhenClicked;
    property UncheckedGlyph: TBitmap read FUncheckedGlyph write SetUncheckedGlyph;
    property UncheckedSmallGlyph: TBitmap read FUncheckedSmallGlyph write SetUncheckedSmallGlyph;
    property UseSmallGlyphs: Boolean read FUseSmallGlyphs write SetUseSmallGlyphs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    property Mutex: TcaXPMutex read FMutex write FMutex;
    procedure ToggleCheck;
  end;

  //---------------------------------------------------------------------------
  // TcaXPCheckBox                                                             
  //---------------------------------------------------------------------------

  TcaXPCheckBox = class(TcaCustomXPCheckBox)
  published
    // Properties 
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSizeMargin;
    property Caption;
    property Checked;
    property CheckedFrameColor;
    property CheckedGlyph;
    property CheckedSmallGlyph;
    property DisabledTextColor;
    property Enabled;
    property EnabledTextColor;
    property Font;
    property FrameColor;
    property GrayedGlyph;
    property GrayedSmallGlyph;
    property MouseOverFrameColor;
    property Mutex;
    property ShowGlyph;
    property ToggleWhenClicked;
    property UncheckedGlyph;
    property UncheckedSmallGlyph;
    property UseSmallGlyphs;
    property Visible;
    // Event properties 
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses Types;

{$R CAXPCHECK.RES}

  //---------------------------------------------------------------------------
  // TcaXPMutex                                                                
  //---------------------------------------------------------------------------

constructor TcaXPMutex.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
end;

destructor TcaXPMutex.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Public methods 


procedure TcaXPMutex.Update(ACheckbox: TcaCustomXPCheckBox);
var
  Checkbox: TcaCustomXPCheckBox;
  Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
    begin
      Checkbox := TcaCustomXPCheckBox(FList[Index]);
      Checkbox.Checked := Checkbox = ACheckbox;
    end;
  DoUpdate(ACheckbox);
end;

  // Protected methods 

procedure TcaXPMutex.Add(ACheckbox: TcaCustomXPCheckBox);
begin
  if FList.IndexOf(ACheckbox) = -1 then
    FList.Add(ACheckbox);
end;

procedure TcaXPMutex.DoUpdate(ACheckbox: TcaCustomXPCheckBox);
begin
  if Assigned(FOnUpdate) then FOnUpdate(ACheckbox);
end;

procedure TcaXPMutex.Loaded;
var
  Checkbox: TcaCustomXPCheckBox;
  Form: TCustomForm;
  Index: Integer;
begin
  inherited;
  if Owner is TCustomForm then
    begin
      Form := TCustomForm(Owner);
      for Index := 0 to Form.ComponentCount - 1 do
        begin
          if Form.Components[Index] is TcaCustomXPCheckBox then
            begin
              Checkbox := TcaCustomXPCheckBox(Form.Components[Index]);
              if Checkbox.Mutex = Self then
                Add(Checkbox);
            end;
        end;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaXPGraphicControl                                                       
  //---------------------------------------------------------------------------

constructor TcaXPGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  Width := 100;
  Height := 22;
  {$IFDEF D7_UP}
  FFrameColor := clMedGray;
  {$ELSE}
  FFrameColor := clMidGray;
  {$ENDIF}
  FMouseOverFrameColor := clHighlight;
  FDisabledTextColor := clBtnShadow;
  FEnabledTextColor := clWindowText;
end;

  // Protected methods 

function TcaXPGraphicControl.GetBackgroundColor: TColor;
begin
  if Enabled and MouseIsOver then
    Result := ColorUtils.GetFlatToolbarsSelectedColor
  else
    Result := ColorUtils.GetFlatToolbarsColor;
end;

function TcaXPGraphicControl.GetFrameColor: TColor;
begin
  if Enabled and MouseIsOver then
    Result := FMouseOverFrameColor
  else
    Result := FFrameColor;
end;

procedure TcaXPGraphicControl.DoMouseEnter;
begin
  inherited;
  RequestPaint;
end;

procedure TcaXPGraphicControl.DoMouseLeave;
begin
  inherited;
  RequestPaint;
end;

procedure TcaXPGraphicControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  RequestPaint;
end;

procedure TcaXPGraphicControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  RequestPaint;
end;

procedure TcaXPGraphicControl.BufferedPaint(C: TCanvas; R: TRect);

  function GetFontColor: TColor;
  begin
    if Enabled then
      Result := FEnabledTextColor
    else
      Result := FDisabledTextColor;
  end;

  procedure DrawBackground;
  begin
    C.Brush.Style := bsSolid;
    C.Brush.Color := GetBackgroundColor;
    C.FillRect(R);
  end;

  procedure DrawFrame;
  begin
    C.Brush.Color := GetFrameColor;
    C.FrameRect(R);
  end;

  procedure DrawText;
  var
    Offset: Integer;
    TextHeight: Integer;
    TextLeft: Integer;
    TextTop: Integer;
    TextWidth: Integer;
  begin
    C.Brush.Color := GetBackgroundColor;
    C.Font.Assign(Font);
    TextHeight := C.TextHeight(Caption);
    TextTop := ((R.Bottom - R.Top) div 2) - (TextHeight div 2) - 1;
    C.Font.Color := GetFontColor;
    Offset := Ord(MouseIsDown);
    TextWidth := C.TextWidth(Caption);
    TextLeft := 4;
    case FAlignment of
      taLeftJustify:    TextLeft := 4;
      taRightJustify:   TextLeft := ClientWidth - TextWidth - 4;
      taCenter:         TextLeft := (ClientWidth div 2) - (TextWidth div 2);
    end;
    C.TextOut(R.Left + TextLeft + Offset, TextTop + Offset, Caption);
  end;

begin
  inherited;
  DrawBackground;
  DrawFrame;
  DrawText;
end;

  // Private methods 

procedure TcaXPGraphicControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  RequestPaint;
end;

procedure TcaXPGraphicControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  RequestPaint;
end;

procedure TcaXPGraphicControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RequestPaint;
end;

  // Property methods 

procedure TcaXPGraphicControl.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
    begin
      FAlignment := Value;
      RequestPaint;
    end;                  
end;

procedure TcaXPGraphicControl.SetDisabledTextColor(const Value: TColor);
begin
  if Value <> FDisabledTextColor then
    begin
      FDisabledTextColor := Value;
      RequestPaint;
    end;
end;

procedure TcaXPGraphicControl.SetEnabledTextColor(const Value: TColor);
begin
  if Value <> FEnabledTextColor then
    begin
      FEnabledTextColor := Value;
      RequestPaint;
    end;                  
end;

procedure TcaXPGraphicControl.SetFrameColor(const Value: TColor);
begin
  if Value <> FFrameColor then
    begin
      FFrameColor := Value;
      RequestPaint;
    end;
end;

procedure TcaXPGraphicControl.SetMouseOverFrameColor(const Value: TColor);
begin
  if Value <> FMouseOverFrameColor then
    begin
      FMouseOverFrameColor := Value;
      RequestPaint;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaXPButton                                                               
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaXPButton.Create(AOwner: TComponent);
begin
  inherited;
  FGlyph := TBitmap.Create;
end;

destructor TcaXPButton.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

  // Protected methods 

procedure TcaXPButton.BufferedPaint(C: TCanvas; R: TRect);

  procedure DrawGlyph;
  var
    X, Y: Integer;
    DisabledBmp: TBitmap;
  begin
    X := 4;
    if Alignment = taLeftJustify then
      X := ClientWidth - FGlyph.Width - 4;
    Y := (Height div 2) - (FGlyph.Height div 2);
    FGlyph.Transparent := True;
    if Enabled then
      C.Draw(X, Y, FGlyph)
    else
      begin
        DisabledBmp := Auto(TBitmap.Create).Instance;
        DisabledBmp.Width := FGlyph.Width;
        DisabledBmp.Height := FGlyph.Height;
        DrawState(DisabledBmp.Canvas.Handle, 0, nil, 0, 0, 0, 0, FGlyph.Width, FGlyph.Height, DST_BITMAP or DSS_DISABLED);
        // ColorUtils.CreateDisabledBitmap(FGlyph, DisabledBmp);
        C.Draw(X, Y, DisabledBmp)
      end;
  end;

begin
  inherited;
  if FShowGlyph and (Alignment <> taCenter) then
    DrawGlyph;
end;

  // Property methods 

procedure TcaXPButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  RequestPaint;
end;

procedure TcaXPButton.SetShowGlyph(const Value: Boolean);
begin
  if Value <> FShowGlyph then
    begin
      FShowGlyph := Value;
      RequestPaint;
    end;                    
end;

  //---------------------------------------------------------------------------
  // TcaXPCheckBox                                                             
  //---------------------------------------------------------------------------

constructor TcaCustomXPCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FShowGlyph := True;
  FToggleWhenClicked := True;
  CreateBitmaps;
  FCheckedFrameColor := clHighlight;
  AutosizeMargin := 6;
end;

destructor TcaCustomXPCheckBox.Destroy;
begin
  FreeBitmaps;
  inherited;
end;

  // Public methods 

procedure TcaCustomXPCheckBox.ToggleCheck;
begin
  FChecked := not FChecked;
  if FMutex <> nil then FMutex.Update(Self);
  RequestPaint;
end;

  // Protected virtual methods 

function TcaCustomXPCheckBox.GetAdjustSizeDelta: Integer;
var
  Glyph: TBitmap;
begin
  Result := inherited GetAdjustSizeDelta;
  Glyph := SelectGlyph;
  if Assigned(Glyph) then
    Inc(Result, Glyph.Width);
end;

function TcaCustomXPCheckBox.GetBackgroundColor: TColor;
begin
  Result := inherited GetBackgroundColor;
  if Enabled then
    begin
      if MouseIsOver and FChecked then
        Result := ColorUtils.GetFlatToolbarsDownedSelectedColor else
      if (not MouseIsOver) and FChecked then
        Result := ColorUtils.GetFlatToolbarsDownedColor;
    end;
end;

function TcaCustomXPCheckBox.GetFrameColor: TColor;
begin
  Result := inherited GetFrameColor;
  if Enabled then
    begin
      if (not MouseIsOver) and FChecked then
        Result := FCheckedFrameColor;
    end;
end;

procedure TcaCustomXPCheckBox.Click;
begin
  if FToggleWhenClicked then ToggleCheck;
  inherited;
end;

procedure TcaCustomXPCheckBox.BufferedPaint(C: TCanvas; R: TRect);

  procedure DrawGlyph;
  var
    DestRect: TRect;
    Glyph: TBitmap;
    GlyphHeight: Integer;
    GlyphGap: Integer;
    GlyphRect: TRect;
  begin
    Glyph := SelectGlyph;
    if Assigned(Glyph) then
      begin
        GlyphHeight := Glyph.Height;
        GlyphGap := Max((((R.Bottom - R.Top) - GlyphHeight) div 2), 0);
        DestRect.Left := R.Right - Glyph.Width - GlyphGap;
        DestRect.Right := R.Right - GlyphGap;
        DestRect.Top := R.Top + GlyphGap + 1;
        DestRect.Bottom := R.Top + GlyphGap + Glyph.Height;
        GlyphRect := Rect(0, 0, Glyph.Width, Glyph.Height);
        C.BrushCopy(DestRect, Glyph, GlyphRect, Glyph.TransparentColor);
      end;
  end;

begin
  inherited;
  if FShowGlyph then DrawGlyph;
end;

  // Private methods 

function TcaCustomXPCheckBox.SelectGlyph: TBitmap;
begin
  if Enabled then
    begin
      if FChecked then
        begin
          if FUseSmallGlyphs then
            Result := FCheckedSmallGlyph
          else
            Result := FCheckedGlyph;
        end
      else
        begin
          if FUseSmallGlyphs then
            Result := FUncheckedSmallGlyph
          else
            Result := FUncheckedGlyph;
        end;
    end
  else
    begin
      if FChecked then
        begin
          if FUseSmallGlyphs then
            Result := FGrayedSmallGlyph
          else
            Result := FGrayedGlyph;
        end
      else
        begin
          if FUseSmallGlyphs then
            Result := FUncheckedSmallGlyph
          else
            Result := FUncheckedGlyph;
        end;
    end;
end;

procedure TcaCustomXPCheckBox.CreateBitmaps;
begin
  FCheckedGlyph := TBitmap.Create;
  FCheckedGlyph.LoadFromResourceName(HInstance, 'XPCHECKED');
  FCheckedSmallGlyph := TBitmap.Create;
  FCheckedSmallGlyph.LoadFromResourceName(HInstance, 'XPCHECKEDSMALL');
  FUncheckedGlyph := TBitmap.Create;
  FUncheckedGlyph.LoadFromResourceName(HInstance, 'XPUNCHECKED');
  FUncheckedSmallGlyph := TBitmap.Create;
  FUncheckedSmallGlyph.LoadFromResourceName(HInstance, 'XPUNCHECKEDSMALL');
  FGrayedGlyph := TBitmap.Create;
  FGrayedGlyph.LoadFromResourceName(HInstance, 'XPGRAYED');
  FGrayedSmallGlyph := TBitmap.Create;
  FGrayedSmallGlyph.LoadFromResourceName(HInstance, 'XPGRAYEDSMALL');
end;

procedure TcaCustomXPCheckBox.FreeBitmaps;
begin
  FCheckedGlyph.Free;
  FCheckedSmallGlyph.Free;
  FUncheckedGlyph.Free;
  FUncheckedSmallGlyph.Free;
  FGrayedGlyph.Free;
  FGrayedSmallGlyph.Free;
end;

  // Property methods 

function TcaCustomXPCheckBox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TcaCustomXPCheckBox.GetShowGlyph: Boolean;
begin
  Result := FShowGlyph;
end;

function TcaCustomXPCheckBox.GetToggleWhenClicked: Boolean;
begin
  Result := FToggleWhenClicked;
end;

procedure TcaCustomXPCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FMutex) then
    FMutex := nil;
end;

procedure TcaCustomXPCheckBox.SetChecked(const Value: Boolean);
begin
  if Value <> FChecked then
    begin
      FChecked := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomXPCheckBox.SetCheckedFrameColor(const Value: TColor);
begin
  if Value <> FCheckedFrameColor then
    begin
      FCheckedFrameColor := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomXPCheckBox.SetShowGlyph(const Value: Boolean);
begin
  if Value <> FShowGlyph then
    begin
      FShowGlyph := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomXPCheckBox.SetToggleWhenClicked(const Value: Boolean);
begin
  FToggleWhenClicked := Value;
end;

procedure TcaCustomXPCheckBox.SetCheckedGlyph(const Value: TBitmap);
begin
  FCheckedGlyph.Assign(Value);
end;

procedure TcaCustomXPCheckBox.SetCheckedSmallGlyph(const Value: TBitmap);
begin
  FCheckedSmallGlyph.Assign(Value);
end;

procedure TcaCustomXPCheckBox.SetGrayedGlyph(const Value: TBitmap);
begin
  FGrayedGlyph.Assign(Value);
end;

procedure TcaCustomXPCheckBox.SetGrayedSmallGlyph(const Value: TBitmap);
begin
  FGrayedSmallGlyph.Assign(Value);
end;

procedure TcaCustomXPCheckBox.SetUncheckedGlyph(const Value: TBitmap);
begin
  FUncheckedGlyph.Assign(Value);
end;

procedure TcaCustomXPCheckBox.SetUncheckedSmallGlyph(const Value: TBitmap);
begin
  FUncheckedSmallGlyph.Assign(Value);
end;

procedure TcaCustomXPCheckBox.SetUseSmallGlyphs(const Value: Boolean);
begin
  if Value <> FUseSmallGlyphs then
    begin
      FUseSmallGlyphs := Value;
      RequestPaint;
    end;
end;

end.
