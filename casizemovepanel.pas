unit caSizeMovePanel;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Menus,

  // ca units 
  caIni,
  caConsts,
  caControls;

type

  TcaSizeMoveOption = (smSizing, smMoving);

  TcaSizeMoveOptions = set of TcaSizeMoveOption;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizingBand                                                             
  //```````````````````````````````````````````````````````````````````````````

  TcaSizingBand = class(TCustomControl)
  protected
    procedure Paint; override;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMovePanelCaptionProperties                                         
  //```````````````````````````````````````````````````````````````````````````

  TcaSizeMovePanelCaptionProperties = class(TPersistent)
  private
    // Property fields 
    FBrushColor: TColor;
    FFont: TFont;
    FText: TCaption;
    FTextBorderWidth: Integer;
    // Event property fields 
    FOnChanged: TNotifyEvent;
    // Property methods 
    procedure SetBrushColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: TCaption);
    procedure SetTextBorderWidth(const Value: Integer);
    // Private methods 
    procedure Changed;
    // Event handlers 
    procedure FontChangedEvent(Sender: TObject);
  protected
    // Event triggers 
    procedure DoChanged; virtual;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    procedure Assign(Source: TPersistent); override;
    // Event properties 
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    // Published properties 
    property BrushColor: TColor read FBrushColor write SetBrushColor;
    property Font: TFont read FFont write SetFont;
    property Text: TCaption read FText write SetText;
    property TextBorderWidth: Integer read FTextBorderWidth write SetTextBorderWidth;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMovePanel                                                          
  //```````````````````````````````````````````````````````````````````````````

  TcaSizeMovePanel = class(TcaCustomFormPanel)
  private
    // Private fields 
    FBands: array[1..4] of TcaSizingBand;
    FCurX: Integer;
    FCurY: Integer;
    FFixX: Integer;
    FFixY: Integer;
    FMoving: Boolean;
    FMovX: Integer;
    FMovY: Integer;
    FSizing: Boolean;
    // Property fields 
    FCaptionProperties: TcaSizeMovePanelCaptionProperties;
    FIsLoaded: Boolean;
    FIsLoading: Boolean;
    FMainControl: TWinControl;
    FMainGapBottom: Integer;
    FMainGapLeft: Integer;
    FMainGapRight: Integer;
    FMainGapTop: Integer;
    FShowGrabHandle: Boolean;
    FOptions: TcaSizeMoveOptions;
    FTitle: TCaption;
    FSizingPixels: Integer;
    FStoreState: Boolean;
    // Event property fields 
    FOnMovePanel: TNotifyEvent;
    FOnSizePanel: TNotifyEvent;
    // Property methods 
    procedure SetCaptionProperties(const Value: TcaSizeMovePanelCaptionProperties);
    procedure SetMainControl(const Value: TWinControl);
    procedure SetMainGapBottom(const Value: Integer);
    procedure SetMainGapLeft(const Value: Integer);
    procedure SetMainGapRight(const Value: Integer);
    procedure SetMainGapTop(const Value: Integer);
    procedure SetShowGrabHandle(const Value: Boolean);
    procedure SetSizingPixels(const Value: Integer);
    procedure SetTitle(const Value: TCaption);
    // Private methods 
    function BuildStateStoreName: string;
    function GetNewHeight: Integer;
    function GetNewWidth: Integer;
    function LoadState: Boolean;
    procedure SaveState;
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    // Event handlers 
    procedure CaptionPropertiesChangedEvent(Sender: TObject);
  protected
    // Protected methods 
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure AlignMain; virtual;
    procedure DoBeforeShowForm(AForm: TForm); override;
    procedure DoMovePanel; virtual;
    procedure DoSizePanel; virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure Resize; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    // New properties 
    property CaptionProperties: TcaSizeMovePanelCaptionProperties read FCaptionProperties write SetCaptionProperties;
    property MainControl: TWinControl read FMainControl write SetMainControl;
    property MainGapBottom: Integer read FMainGapBottom write SetMainGapBottom;
    property MainGapLeft: Integer read FMainGapLeft write SetMainGapLeft;
    property MainGapRight: Integer read FMainGapRight write SetMainGapRight;
    property MainGapTop: Integer read FMainGapTop write SetMainGapTop;
    property Options: TcaSizeMoveOptions read FOptions write FOptions;
    property ShowGrabHandle: Boolean read FShowGrabHandle write SetShowGrabHandle;
    property SizingPixels: Integer read FSizingPixels write SetSizingPixels;
    property StoreState: Boolean read FStoreState write FStoreState;
    property Title: TCaption read FTitle write SetTitle;
    // New event properties 
    property OnMovePanel: TNotifyEvent read FOnMovePanel write FOnMovePanel;
    property OnSizePanel: TNotifyEvent read FOnSizePanel write FOnSizePanel;
    // Promoted inherited properties 
    // TcaCustomPanel 
    property Frame;
    property SubClassForm;
    property Transparent;
    property OnClickOutside;
    // TcaCustomFormPanel event properties 
    property OnCreateForm;
    // TCustomPanel 
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    // property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    // Promoted inherited event properties 
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizingBand                                                             
  //```````````````````````````````````````````````````````````````````````````

procedure TcaSizingBand.Paint;
var
  R: TRect;
begin
  inherited Paint;
  Canvas.Brush.Color := clTeal;
  R := ClientRect;
  Canvas.FillRect(R);
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMovePanelCaptionProperties                                         
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaSizeMovePanelCaptionProperties.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := FontChangedEvent;
end;

destructor TcaSizeMovePanelCaptionProperties.Destroy;
begin
  FFont.Free;
  inherited;
end;

  // Public methods 

procedure TcaSizeMovePanelCaptionProperties.Assign(Source: TPersistent);
var
  SourceProperties: TcaSizeMovePanelCaptionProperties;
begin
  if Source is TcaSizeMovePanelCaptionProperties then
    begin
      SourceProperties := TcaSizeMovePanelCaptionProperties(Source);
      FBrushColor := SourceProperties.BrushColor;
      FFont.Assign(SourceProperties.Font);
      FText := SourceProperties.Text;
      FTextBorderWidth := SourceProperties.TextBorderWidth;
    end
  else
    inherited;
end;

  // Event triggers 

procedure TcaSizeMovePanelCaptionProperties.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

  // Private methods 

procedure TcaSizeMovePanelCaptionProperties.Changed;
begin
  DoChanged;
end;

  // Event handlers 

procedure TcaSizeMovePanelCaptionProperties.FontChangedEvent(Sender: TObject);
begin
  Changed;
end;

  // Property methods 

procedure TcaSizeMovePanelCaptionProperties.SetBrushColor(const Value: TColor);
begin
  if Value <> FBrushColor then
    begin
      FBrushColor := Value;
      Changed;
    end;
end;

procedure TcaSizeMovePanelCaptionProperties.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TcaSizeMovePanelCaptionProperties.SetText(const Value: TCaption);
begin
  if Value <> FText then
    begin
      FText := Value;
      Changed;
    end;
end;

procedure TcaSizeMovePanelCaptionProperties.SetTextBorderWidth(const Value: Integer);
begin
  if Value <> FTextBorderWidth then
    begin
      FTextBorderWidth := Value;
      Changed;
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMovePanel                                                          
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaSizeMovePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptionProperties := TcaSizeMovePanelCaptionProperties.Create;
  FCaptionProperties.OnChanged := CaptionPropertiesChangedEvent;
  ControlStyle := ControlStyle - [csSetCaption];
  FSizingPixels := 24;
  FMainGapLeft := 5;
  FMainGapRight := 5;
  FMainGapTop := 24;
  FMainGapBottom := 36;
  FOptions := [smSizing, smMoving];
end;

destructor TcaSizeMovePanel.Destroy;
begin
  inherited Destroy;
end;

  // Public methods 

procedure TcaSizeMovePanel.Resize;
begin
  inherited Resize;
  AlignMain;
end;

procedure TcaSizeMovePanel.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer);
begin
  inherited;
  if FStoreState then SaveState;
end;

  // Protected methods 

procedure TcaSizeMovePanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
end;

procedure TcaSizeMovePanel.AlignMain;
begin
  if FMainControl <> nil then
    begin
      FMainControl.Left := FMainGapLeft;
      FMainControl.Width := ClientWidth - FMainGapLeft - FMainGapRight;
      FMainControl.Top := FMainGapTop;
      FMainControl.Height := ClientHeight - FMainGapTop - FMainGapBottom;
    end;
end;

procedure TcaSizeMovePanel.DoMovePanel;
begin
  if FStoreState and (not FIsLoading) and FIsLoaded then SaveState;
  if Assigned(FOnMovePanel) then FOnMovePanel(Self);
end;

procedure TcaSizeMovePanel.DoBeforeShowForm(AForm: TForm);
begin
  SetMainControl(AForm);
  AForm.Align := alNone;
end;

procedure TcaSizeMovePanel.DoSizePanel;
begin
  if Assigned(FOnSizePanel) then FOnSizePanel(Self);
end;

procedure TcaSizeMovePanel.Loaded;
begin
  inherited;
  if FStoreState then
    begin
      LoadState;
    end;
  Resize;
end;

procedure TcaSizeMovePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
var
  Band: TcaSizingBand;
  CaptionHeight: Integer;
  OwnerForm: TWinControl;
  Index: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  BringToFront;
  if smSizing in FOptions then
    begin
      if (X > Width - FSizingPixels) and (Y > Height - FSizingPixels) then
        begin
          OwnerForm := Application.MainForm;
          CaptionHeight := GetSystemMetrics(SM_CYCAPTION);
          FFixX := ClientOrigin.x - OwnerForm.Left - 4;
          FFixY := ClientOrigin.y - CaptionHeight - OwnerForm.Top - 4;
          FMovX := FFixX + Width - 1;
          FMovY := FFixY + Height - 1;
          FCurX := X;
          FCurY := Y;
          FSizing := True;
          for Index := 1 to 4 do
            begin
              FBands[Index] := TcaSizingBand.Create(Owner);
              Band := FBands[Index];
              Band.Visible := False;
              Band.Parent := OwnerForm;
              Band.Color := clBlack;
              Band.Height := 1;
              Band.Width := 1;
              Band.Visible := True;
            end;
          Rectangle(FFixX, FFixY, FMovX, FMovY);
        end;
    end;
  if (smMoving in FOptions) and (not FSizing) then
    begin
      FSizing := False;
      FMoving := True;
      ReleaseCapture;
      Self.Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
    end;
end;

procedure TcaSizeMovePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FSizing then
    begin
      if not (akRight in Anchors) then
        begin
          Inc(FMovX, X - FCurX);
          FCurX := X;
        end;
      if not (akBottom in Anchors) then
        begin
          Inc(FMovY, Y - FCurY);
          FCurY := Y;
          Rectangle(FFixX, FFixY, FMovX, FMovY);
        end;
      DoSizePanel;
    end;
  if FMoving then DoMovePanel;
end;

procedure TcaSizeMovePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMoving := False;
  for Index := 1 to 4 do
    if FBands[Index] <> nil then
      begin
        FBands[Index].Free;
        FBands[Index] := nil;
      end;
  if FSizing and (smSizing in FOptions) then
    begin
      Width := GetNewWidth;
      Height := GetNewHeight;
      AlignMain;
      FSizing := False;
    end;
end;

procedure TcaSizeMovePanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (MainControl = AComponent) then
    MainControl := nil;
end;

procedure TcaSizeMovePanel.Paint;

  procedure DoLine(C: TColor; Px: Integer);
  begin
    // Canvas.Pen.Width := 2;
    Canvas.Pen.Color := C;
    Canvas.MoveTo(Width - Px, Height - 4);
    Canvas.LineTo(Width - 3, Height - Px - 1);
  end;

  procedure DrawCaption;
  var
    TextH: Integer;
    CaptL: Integer;
    CaptR: Integer;
    CaptT: Integer;
    CaptB: Integer;
    Gap: Integer;
  begin
    Gap := FCaptionProperties.TextBorderWidth;
    Canvas.Font.Assign(FCaptionProperties.Font);
    TextH := Canvas.TextHeight(FCaptionProperties.Text);
    CaptL := FMainGapLeft;
    CaptR := Width - Gap * 2 - 2;
    CaptT := 3;
    CaptB := CaptT + TextH + Gap * 2;
    Canvas.Brush.Color := FCaptionProperties.BrushColor;
    Canvas.FillRect(Rect(CaptL, CaptT, CaptR, CaptB));
    Canvas.Font.Assign(FCaptionProperties.Font);
    Canvas.TextOut(CaptL + Gap, CaptT + Gap, FCaptionProperties.Text);
  end;

begin
  inherited Paint;
  if FShowGrabHandle then
    begin
      DoLine(clDkGray, 13);
      DoLine(clDkGray, 9);
      DoLine(clDkGray, 5);
    end;
  DrawCaption;
end;

  // Private methods 

function TcaSizeMovePanel.BuildStateStoreName: string;
var
  Control: TControl;
begin
  Result := '';
  Control := Self;
  while Control <> nil do
    begin
      Result := Control.Name + '.' + Result;
      Control := Control.Parent;
    end;
  if Length(Result) > 0 then
    System.SetLength(Result, Length(Result) - 1);
end;

function TcaSizeMovePanel.GetNewHeight: Integer;
begin
  Result := FMovY - FFixY + 1;
end;

function TcaSizeMovePanel.GetNewWidth: Integer;
begin
  Result := FMovX - FFixX + 1;
end;

function TcaSizeMovePanel.LoadState: Boolean;
var
  Ini: IcaIni;
  ALeft: Integer;
  ATop: Integer;
  AWidth: Integer;
  AHeight: Integer;
begin
  FIsLoading := True;
  Result := False;
  Ini := TcaIni.Create(BuildStateStoreName);
  ALeft := Ini.Integers[cLeft];
  ATop := Ini.Integers[cTop];
  AWidth := Ini.Integers[cWidth];
  AHeight := Ini.Integers[cHeight];
  if (ALeft <> 0) and (ATop <> 0) and (AWidth <> 0) and (AHeight <> 0) then
    begin
      SetBounds(ALeft, ATop, AWidth, AHeight);
      Result := True;
    end;
  FIsLoading := False;
  FIsLoaded := True;
end;

procedure TcaSizeMovePanel.SaveState;
var
  Ini: IcaIni;
begin
  Ini := TcaIni.Create(BuildStateStoreName);
  Ini.Integers[cLeft] := Left;
  Ini.Integers[cTop] := Top;
  Ini.Integers[cWidth] := Width;
  Ini.Integers[cHeight] := Height;
end;

procedure TcaSizeMovePanel.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  // Left 
  FBands[1].SetBounds(X1, Y1, 1, Y2 - Y1 + 1);
  // Top 
  FBands[2].SetBounds(X1, Y1, X2 - X1 + 1, 1);
  // Right 
  FBands[3].SetBounds(X2, Y1, 1, FBands[1].Height);
  // Bottom 
  FBands[4].SetBounds(X1, Y2, FBands[2].Width, 1);
end;

  // Event handlers 

procedure TcaSizeMovePanel.CaptionPropertiesChangedEvent(Sender: TObject);
begin
  Invalidate;
end;

  // Property methods 

procedure TcaSizeMovePanel.SetCaptionProperties(const Value: TcaSizeMovePanelCaptionProperties);
begin
  FCaptionProperties.Assign(Value);
end;

procedure TcaSizeMovePanel.SetMainControl(const Value: TWinControl);
begin
  if Value <> FMainControl then
    begin
      FMainControl := Value;
      if FMainControl <> nil then
        FMainControl.Parent := Self;
      AlignMain;
    end;
end;

procedure TcaSizeMovePanel.SetMainGapBottom(const Value: Integer);
begin
  if Value <> FMainGapBottom then
    begin
      FMainGapBottom := Value;
      AlignMain;
    end;
end;

procedure TcaSizeMovePanel.SetMainGapLeft(const Value: Integer);
begin
  if Value <> FMainGapLeft then
    begin
      FMainGapLeft := Value;
      AlignMain;
    end;
end;

procedure TcaSizeMovePanel.SetMainGapRight(const Value: Integer);
begin
  if Value <> FMainGapRight then
    begin
      FMainGapRight := Value;
      AlignMain;
    end;
end;

procedure TcaSizeMovePanel.SetMainGapTop(const Value: Integer);
begin
  if Value <> FMainGapTop then
    begin
      FMainGapTop := Value;
      AlignMain;
    end;
end;

procedure TcaSizeMovePanel.SetShowGrabHandle(const Value: Boolean);
begin
  if Value <> FShowGrabHandle then
    begin
      FShowGrabHandle := Value;
      Invalidate;
    end;
end;

procedure TcaSizeMovePanel.SetSizingPixels(const Value: Integer);
begin
  if Value <> FSizingPixels then
    begin
      FSizingPixels := Value;
    end;
end;

procedure TcaSizeMovePanel.SetTitle(const Value: TCaption);
begin
  if Value <> FTitle then
    begin
      FTitle := Value;
      Invalidate;
    end;
end;

end.
