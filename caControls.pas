unit caControls;

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
  StdCtrls,
  ExtCtrls,
  Dialogs,

  // ca units 
  caUtils,
  caTypes,
  caClasses,
  caFrame;

type

  TcaPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect) of object;

  TcaClickOutsideEvent = procedure(Sender: TObject; AClickedControl: TControl) of object;

  //---------------------------------------------------------------------------
  // IcaComponentFactory                                                       
  //---------------------------------------------------------------------------

  IcaComponentFactory = interface
  ['{3D7E850A-B6DD-4B60-9722-5456A12FF94D}']
    function GetComponentClass: TComponentClass;
    function GetComponentOwner: TComponent;
    function GetNextComponentName: String;
    function NewComponent: TComponent;
    procedure SetComponentClass(const Value: TComponentClass);
    procedure SetComponentOwner(const Value: TComponent);
    property ComponentClass: TComponentClass read GetComponentClass write SetComponentClass;
    property ComponentOwner: TComponent read GetComponentOwner write SetComponentOwner;
  end;

  //---------------------------------------------------------------------------
  // TcaComponentFactory                                                       
  //---------------------------------------------------------------------------

  TcaComponentFactory = class(TInterfacedObject, IcaComponentFactory)
  private
    FComponentClass: TComponentClass;
    FComponentOwner: TComponent;
    function GetComponentClass: TComponentClass;
    function GetComponentOwner: TComponent;
    procedure SetComponentClass(const Value: TComponentClass);
    procedure SetComponentOwner(const Value: TComponent);
  public
    constructor Create(AClass: TComponentClass; AOwner: TComponent); overload;
    function GetNextComponentName: String;
    property ComponentClass: TComponentClass read GetComponentClass write SetComponentClass;
    function NewComponent: TComponent;
  end;

  //---------------------------------------------------------------------------
  // IcaMargin                                                                 
  //---------------------------------------------------------------------------

  IcaMargin = interface
  ['{2E99C170-05EE-40A0-A074-BA4F485551C8}']
    // Property methods 
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetOnChanged: TNotifyEvent;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure SetBottom(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetOnChanged(const Value: TNotifyEvent);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
    // Properties 
    property Bottom: Integer read GetBottom write SetBottom;
    property Left: Integer read GetLeft write SetLeft;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
    property Right: Integer read GetRight write SetRight;
    property Top: Integer read GetTop write SetTop;
  end;

  //---------------------------------------------------------------------------
  // TcaMargin                                                                 
  //---------------------------------------------------------------------------

  TcaMargin = class(TcaInterfacedPersistent, IcaMargin)
  private
    FBottom: Integer;
    FLeft: Integer;
    FOnChanged: TNotifyEvent;
    FRight: Integer;
    FTop: Integer;
    FUpdateCount: Integer;
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetOnChanged: TNotifyEvent;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure Changed;
    procedure SetBottom(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetOnChanged(const Value: TNotifyEvent);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
  protected
    procedure DoChanged; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Bottom: Integer read GetBottom write SetBottom;
    property Left: Integer read GetLeft write SetLeft;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
    property Right: Integer read GetRight write SetRight;
    property Top: Integer read GetTop write SetTop;
  end;

  //---------------------------------------------------------------------------
  // IcaCustomPanel                                                            
  //---------------------------------------------------------------------------

  IcaCustomPanel = interface
  ['{EB2CD5CD-954A-46B3-A009-614FB67923E8}']
    // Property methods 
    function GetTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
    // Properties 
    property Transparent: Boolean read GetTransparent write SetTransparent;
  end;

  //---------------------------------------------------------------------------
  // TcaCustomPanel                                                            
  //---------------------------------------------------------------------------

  TcaCustomPanel = class(TCustomPanel, IcaCustomPanel, IcaFrame)
  private
    // Private fields 
    FDefaultWindowProc: TWndMethod;
    FFrame: TcaFrameProperties;
    FFrameImpl: IcaFrame;
    FOffScreenBitmap: TBitmap;
    FOnClickOutside: TcaClickOutsideEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FSubClassForm: Boolean;
    FTransparent: Boolean;
    // Property methods 
    function GetTransparent: Boolean;
    procedure SetSubClassForm(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    // Private methods 
    procedure CreateFrameObjects;
    procedure CreateOffScreenBitmap;
    procedure FreeFrameObjects;
    procedure FreeOffScreenBitmap;
    procedure HookWindowProc(var Message: TMessage);
    procedure ReplaceWindowProc;
    procedure RestoreWindowProc;
    // Component mesage handlers 
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMMouseenter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    // Protected methods 
    function GetOffScreenCanvas: TCanvas;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoClickOutside(AClickedControl: TControl); virtual;
    procedure DoHookWindowProc(var Message: TMessage); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure Paint; override;
    procedure UpdateOffScreenBitmap;
    procedure UpdateOnScreenBitmap;
    // Protected properties 
    property Frame: TcaFrameProperties read FFrame write FFrame;
    property FrameImpl: IcaFrame read FFrameImpl implements IcaFrame;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property SubClassForm: Boolean read FSubClassForm write SetSubClassForm;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    // Protected event properties 
    property OnClickOutside: TcaClickOutsideEvent read FOnClickOutside write FOnClickOutside;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // TcaPanel                                                                  
  //---------------------------------------------------------------------------

  TcaPanel = class(TcaCustomPanel)
  public
    // TCustomPanel 
    property DockManager;
  published
    // TcaCustomPanel 
    property Frame;
    property SubClassForm;
    property Transparent;
    // Event properties 
    property OnClickOutside;
    // TCustomPanel 
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
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
    property Visible;
    // Event properties 
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  //---------------------------------------------------------------------------
  // TcaFormPanel                                                              
  //---------------------------------------------------------------------------

  TcaFormCreateEvent = procedure(Sender: TObject; var AForm: TForm) of object;

  TcaFormShowEvent = procedure(Sender: TObject; AForm: TForm) of object;

  TcaCustomFormPanel = class(TcaPanel)
  private
    // Private fields 
    FForm: TForm;
    // Event property fields 
    FOnBeforeShowForm: TcaFormShowEvent;
    FOnCreateForm: TcaFormCreateEvent;
    // Private methods 
    procedure UpdateChildForm;
  protected
    // Protected methods 
    procedure CreateWnd; override;
    procedure DoCreateForm(var AForm: TForm); virtual;
    procedure DoBeforeShowForm(AForm: TForm); virtual;
    // Event properties 
    property OnCreateForm: TcaFormCreateEvent read FOnCreateForm write FOnCreateForm;
    property OnBeforeShowForm: TcaFormShowEvent read FOnBeforeShowForm write FOnBeforeShowForm;
  public
    // Public methods 
    procedure CreateChildForm(AForm: TForm); overload;
    procedure CreateChildForm(AFormClass: TFormClass); overload;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaFormPanel                                                              
  //```````````````````````````````````````````````````````````````````````````

  TcaFormPanel = class(TcaCustomFormPanel)
  published
    // TcaCustomFormPanel event properties 
    property OnCreateForm;
    // Promoted properties 
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    // property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
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
    property Visible;
    // Event properties 
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  //---------------------------------------------------------------------------
  // IcaGraphicControl                                                         
  //---------------------------------------------------------------------------

  IcaGraphicControl = interface
  ['{52F8AC8E-BCB3-4088-BF18-6EAC25F2B316}']
    function GetCanvas: TCanvas;
    function GetMouseIsDown: Boolean;
    function GetMouseIsOver: Boolean;
    function GetOnPaint: TcaPaintEvent;
    procedure Paint;
    procedure SetOnPaint(const Value: TcaPaintEvent);
    property Canvas: TCanvas read GetCanvas;
    property MouseIsDown: Boolean read GetMouseIsDown;
    property MouseIsOver: Boolean read GetMouseIsOver;
    property OnPaint: TcaPaintEvent read GetOnPaint write SetOnPaint;
  end;

  //---------------------------------------------------------------------------
  // TcaGraphicControl                                                         
  //---------------------------------------------------------------------------

  TcaGraphicControl = class(TControl, IcaGraphicControl)
  private
    // Private fields 
    FAutoSizeMargin: Integer;
    FCanvas: TCanvas;
    FMouseIsDown: Boolean;
    FMouseIsOver: Boolean;
    FOffScreenBitmap: TBitmap;
    FOnPaint: TcaPaintEvent;
    FUpdateCount: Integer;
    // Property methods 
    function GetCanvas: TCanvas;
    function GetMouseIsDown: Boolean;
    function GetMouseIsOver: Boolean;
    function GetOnPaint: TcaPaintEvent;
    procedure SetAutoSizeMargin(const Value: Integer);
    procedure SetOnPaint(const Value: TcaPaintEvent);
    // Component message handlers 
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    // Windows message handlers 
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    // Protected methods 
    function GetAdjustSizeDelta: Integer; virtual;
    function GetOffScreenCanvas: TCanvas;
    procedure AdjustSize; override;
    procedure BufferedPaint(C: TCanvas; R: TRect); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; virtual;
    procedure RequestPaint; virtual;
    procedure UpdateOnScreenBitmap;
    // Protected properties 
    property AutoSizeMargin: Integer read FAutoSizeMargin write SetAutoSizeMargin;
    property Canvas: TCanvas read GetCanvas;
    property MouseIsDown: Boolean read GetMouseIsDown;
    property MouseIsOver: Boolean read GetMouseIsOver;
    property OffScreenCanvas: TCanvas read GetOffScreenCanvas;
    property OnPaint: TcaPaintEvent read GetOnPaint write SetOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  //---------------------------------------------------------------------------
  // TcaXPGraphicControl                                                       
  //---------------------------------------------------------------------------

  TcaXPGraphicControl = class(TcaGraphicControl)
  private
  protected
  public
  end;

  //---------------------------------------------------------------------------
  // IcaSpacer                                                                 
  //---------------------------------------------------------------------------

  IcaSpacer = interface
  ['{18414C8C-B37A-4111-8881-03B9F4C8088B}']
    // Property methods 
    function GetGrooved: Boolean;
    procedure SetGrooved(const Value: Boolean);
    // Properties 
    property Grooved: Boolean read GetGrooved write SetGrooved;
  end;

  //---------------------------------------------------------------------------
  // TcaSpacer                                                                 
  //---------------------------------------------------------------------------

  TcaSpacer = class(TcaGraphicControl, IcaSpacer)
  private
    FGrooved: Boolean;
    // Property methods 
    function GetGrooved: Boolean;
    procedure SetGrooved(const Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    // Properties 
    property Align;
    property Grooved: Boolean read GetGrooved write SetGrooved;
    property Width default 8;
    property Height default 25;
  end;

  //---------------------------------------------------------------------------
  // TcaSplitter                                                               
  //---------------------------------------------------------------------------

  TcaSplitter = class(TSplitter)
  private
    // Private fields 
    FPosition: Integer;
    // Property methods 
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    // Private methods 
    function FindControl: TControl;
    procedure UpdateControlSize;
    procedure UpdatePosition;
  published
    property Position: Integer read GetPosition write SetPosition;
  end;

  //---------------------------------------------------------------------------
  // TcaMemo                                                                   
  //---------------------------------------------------------------------------

  TcaMemo = class(TMemo)
  published
    property Action;
  end;

  //---------------------------------------------------------------------------
  // IcaGraphicBox                                                             
  //---------------------------------------------------------------------------

  IcaGraphicBox = interface
  ['{E1D58E76-1FDA-41E5-906E-24060574BE6C}']
    // Property methods 
    function GetColor: TColor;
    function GetFrameColor: TColor;
    function GetFrameWidth: Integer;
    procedure SetColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameWidth(const Value: Integer);
    // Properties 
    property Color: TColor read GetColor write SetColor;
    property FrameColor: TColor read GetFrameColor write SetFrameColor;
    property FrameWidth: Integer read GetFrameWidth write SetFrameWidth;
  end;

  //---------------------------------------------------------------------------
  // TcaGraphicBox                                                             
  //---------------------------------------------------------------------------

  TcaGraphicBox = class(TcaGraphicControl, IcaGraphicBox)
  private
    // Property fields 
    FColor: TColor;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    // Property methods 
    function GetColor: TColor;
    function GetFrameColor: TColor;
    function GetFrameWidth: Integer;
    procedure SetColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameWidth(const Value: Integer);
  protected
    // Protected methods 
    procedure BufferedPaint(C: TCanvas; R: TRect); override;
  published
    // Properties 
    property Align;
    property Color: TColor read GetColor write SetColor;
    property FrameColor: TColor read GetFrameColor write SetFrameColor;
    property FrameWidth: Integer read GetFrameWidth write SetFrameWidth;
    // Event properties 
    property OnPaint;     
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaComponentFactory                                                       
  //---------------------------------------------------------------------------

constructor TcaComponentFactory.Create(AClass: TComponentClass; AOwner: TComponent);
begin
  inherited Create;
  FComponentClass := AClass;
  FComponentOwner := AOwner;
end;

function TcaComponentFactory.GetComponentClass: TComponentClass;
begin
  Result := FComponentClass;
end;

function TcaComponentFactory.GetComponentOwner: TComponent;
begin
  Result := FComponentOwner;
end;

function TcaComponentFactory.GetNextComponentName: String;
var
  BaseName: String;
  Index: Integer;
begin
  BaseName := FComponentClass.ClassName;
  System.Delete(BaseName, 1, 1);
  Index := 1;
  while FComponentOwner.FindComponent(BaseName + IntToStr(Index)) <> nil do
    Inc(Index);
  Result := BaseName + IntToStr(Index);
end;

function TcaComponentFactory.NewComponent: TComponent;
var
  CompState: IcaComponentState;
begin
  Result := TComponent(FComponentClass.Create(FComponentOwner));
  if FComponentOwner <> nil then
    begin
      CompState := TcaComponentState.Create;
      CompState.Component := FComponentOwner;
      if CompState.IsDesigning then
        if (FComponentOwner is TCustomForm) or (FComponentOwner is TDataModule) then
          Result.Name := GetNextComponentName;
    end;
end;

procedure TcaComponentFactory.SetComponentClass(const Value: TComponentClass);
begin
  FComponentClass := Value;
end;

procedure TcaComponentFactory.SetComponentOwner(const Value: TComponent);
begin
  FComponentOwner := Value;
end;

  //---------------------------------------------------------------------------
  // TcaMargin                                                                 
  //---------------------------------------------------------------------------

procedure TcaMargin.Assign(Source: TPersistent);
var
  AMargin: TcaMargin;
begin
  if Source is TcaMargin then
    begin
      AMargin := TcaMargin(Source);
      Left := AMargin.Left;
      Top := AMargin.Top;
      Right := AMargin.Right;
      Bottom := AMargin.Bottom;
    end;
  inherited;
end;

procedure TcaMargin.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcaMargin.Changed;
begin
  if FUpdateCount = 0 then DoChanged;
end;

procedure TcaMargin.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TcaMargin.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

function TcaMargin.GetBottom: Integer;
begin
  Result := FBottom;
end;

function TcaMargin.GetLeft: Integer;
begin
  Result := FLeft;
end;

function TcaMargin.GetOnChanged: TNotifyEvent;
begin
  Result := FOnChanged;
end;

function TcaMargin.GetRight: Integer;
begin
  Result := FRight;
end;

function TcaMargin.GetTop: Integer;
begin
  Result := FTop;
end;

procedure TcaMargin.SetBottom(const Value: Integer);
begin
  if Value <> FBottom then
    begin
      FBottom := Value;
      Changed;
    end;
end;

procedure TcaMargin.SetLeft(const Value: Integer);
begin
  if Value <> FLeft then
    begin
      FLeft := Value;
      Changed;
    end;
end;

procedure TcaMargin.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

procedure TcaMargin.SetRight(const Value: Integer);
begin
  if Value <> FRight then
    begin
      FRight := Value;
      Changed;
    end;
end;

procedure TcaMargin.SetTop(const Value: Integer);
begin
  if Value <> FTop then
    begin
      FTop := Value;
      Changed;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaCustomPanel                                                            
  //---------------------------------------------------------------------------

constructor TcaCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  CreateOffScreenBitmap;
  CreateFrameObjects;
end;

destructor TcaCustomPanel.Destroy;
begin
  FreeFrameObjects;
  FreeOffScreenBitmap;
  RestoreWindowProc;
  inherited;
end;

  // Protected methods 

function TcaCustomPanel.GetOffScreenCanvas: TCanvas;
begin
  Result := nil;
  if FOffScreenBitmap <> nil then
    begin
      FOffScreenBitmap.Width := Width;
      FOffScreenBitmap.Height := Height;
      Result := FOffScreenBitmap.Canvas;
    end;
end;

procedure TcaCustomPanel.CreateWnd;
begin
  inherited;
end;

procedure TcaCustomPanel.DoClickOutside(AClickedControl: TControl);
begin
  if Assigned(FOnClickOutside) then FOnClickOutside(Self, AClickedControl);
end;

procedure TcaCustomPanel.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TcaCustomPanel.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TcaCustomPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    begin
      if NewStyleControls then
      begin
        if FTransparent then ExStyle := ExStyle or WS_EX_TRANSPARENT;
      end;
    end;
end;

procedure TcaCustomPanel.Paint;
var
  C: TCanvas;
begin
  if not FTransparent then
    begin
      C := GetOffScreenCanvas;
      C.Brush.Color := Color;
      C.Brush.Style := bsSolid;
      C.FillRect(Rect(0, 0, Width, Height));
      FFrameImpl.Update;
      UpdateOnScreenBitmap;
    end
  else
    inherited;
end;

procedure TcaCustomPanel.UpdateOffScreenBitmap;
begin
  BitBlt(FOffScreenBitmap.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TcaCustomPanel.UpdateOnScreenBitmap;
begin
  BitBlt(Canvas.Handle, 0, 0, Width, Height, FOffScreenBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

  // Private methods 

procedure TcaCustomPanel.CreateFrameObjects;
begin
  FFrameImpl := TcaFrame.Create; // FFrameImpl: IcaFrame
  FFrameImpl.Control := Self;
  FFrameImpl.AdjustOffsets(0, 0, -2, -2);
  FFrameImpl.Sides := [sdLeft, sdTop, sdRight, sdBottom];
  FFrameImpl.Style := fsRaisedPanel;
  FFrameImpl.LineColor := clBtnShadow;
  FFrameImpl.FocusedSides := [sdLeft, sdTop, sdRight, sdBottom];
  FFrameImpl.FocusedStyle := fsRaisedPanel;
  FFrameImpl.FocusedLineColor := clBtnShadow;
  FFrame := TcaFrameProperties.Create;
  FFrame.Frame := FFrameImpl;
end;

procedure TcaCustomPanel.CreateOffScreenBitmap;
begin
  FOffScreenBitmap := TBitmap.Create;
end;

procedure TcaCustomPanel.FreeFrameObjects;
begin
  FFrame.Free;
end;

procedure TcaCustomPanel.FreeOffScreenBitmap;
begin
  FOffScreenBitmap.Free;
end;

procedure TcaCustomPanel.HookWindowProc(var Message: TMessage);
begin
  DoHookWindowProc(Message);
  FDefaultWindowProc(Message);
end;

procedure TcaCustomPanel.ReplaceWindowProc;
begin
  if Owner is TForm then
    begin
      FDefaultWindowProc := TControl(Owner).WindowProc;
      TControl(Owner).WindowProc := HookWindowProc;
    end;
end;

procedure TcaCustomPanel.RestoreWindowProc;
begin
  if Owner is TForm then
    if Assigned(FDefaultWindowProc) then
      TControl(Owner).WindowProc := FDefaultWindowProc;
end;

  // Component mesage handlers 

procedure TcaCustomPanel.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  DoClickOutside(Message.Sender);
end;

procedure TcaCustomPanel.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  FFrameImpl.Focused := Focused;
end;

procedure TcaCustomPanel.DoHookWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_LBUTTONDOWN then
    Pass;
  inherited;
end;

procedure TcaCustomPanel.CMMouseenter(var Message: TMessage);
begin
  inherited;
  DoMouseEnter;
end;

procedure TcaCustomPanel.CMMouseleave(var Message: TMessage);
begin
  inherited;
  DoMouseLeave;
end;

  // Property methods 

function TcaCustomPanel.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

procedure TcaCustomPanel.SetSubClassForm(const Value: Boolean);
begin
  if Value <> FSubClassForm then
    begin
      FSubClassForm := Value;
      if FSubClassForm then
        ReplaceWindowProc
      else
        RestoreWindowProc;
    end;
end;

procedure TcaCustomPanel.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
    begin
      FTransparent := Value;
      RecreateWnd;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaCustomFormPanel                                                        
  //---------------------------------------------------------------------------

  // Public methods 

procedure TcaCustomFormPanel.CreateChildForm(AForm: TForm);
begin
  FForm := AForm;
  UpdateChildForm;
end;

procedure TcaCustomFormPanel.CreateChildForm(AFormClass: TFormClass);
begin
  FForm := AFormClass.Create(Application);
  UpdateChildForm;
end;

  // Protected methods 

procedure TcaCustomFormPanel.CreateWnd;
begin
  inherited;
  UpdateChildForm;
end;

procedure TcaCustomFormPanel.DoBeforeShowForm(AForm: TForm);
begin
  if Assigned(FOnBeforeShowForm) then FOnBeforeShowForm(Self, FForm);
end;

procedure TcaCustomFormPanel.DoCreateForm(var AForm: TForm);
begin
  if Assigned(FOnCreateForm) then FOnCreateForm(Self, FForm);
end;

  // Private methods 

procedure TcaCustomFormPanel.UpdateChildForm;
begin
  if not Assigned(FForm) then
    if Assigned(FOnCreateForm) then FOnCreateForm(Self, FForm);
  if FForm <> nil then
    begin
      FForm.BorderStyle := bsNone;
      FForm.BorderIcons := [];
      FForm.Caption := '';
      FForm.Align := alClient;
      FForm.Parent := Self;
      DoBeforeShowForm(FForm);
      FForm.Show;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaGraphicControl                                                         
  //---------------------------------------------------------------------------

constructor TcaGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  FOffScreenBitmap := TBitmap.Create;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FAutoSizeMargin := 5;
end;

destructor TcaGraphicControl.Destroy;
begin
  if GetCaptureControl = Self then SetCaptureControl(nil);
  FCanvas.Free;
  FOffScreenBitmap.Free;
  inherited;
end;

  // Public methods 

procedure TcaGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcaGraphicControl.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then Invalidate;
end;

  // Protected methods 

function TcaGraphicControl.GetAdjustSizeDelta: Integer;
begin
  Result := FAutoSizeMargin * 2 + 1;
end;

function TcaGraphicControl.GetOffScreenCanvas: TCanvas;
begin
  Result := nil;
  if FOffScreenBitmap <> nil then
    begin
      FOffScreenBitmap.Width := Width;
      FOffScreenBitmap.Height := Height;
      Result := FOffScreenBitmap.Canvas;
    end;
end;

procedure TcaGraphicControl.AdjustSize;
begin
  if Parent <> nil then
    begin
      Canvas.Font.Assign(Font);
      Width := Canvas.TextWidth(Caption) + GetAdjustSizeDelta;
    end;
end;

procedure TcaGraphicControl.BufferedPaint(C: TCanvas; R: TRect); 
begin
  // Virtual 
end;

procedure TcaGraphicControl.DoMouseEnter;
begin
  // Virtual 
end;

procedure TcaGraphicControl.DoMouseLeave;
begin
  // Virtual 
end;

procedure TcaGraphicControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseIsDown := True;
end;

procedure TcaGraphicControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseIsDown := False;
end;

procedure TcaGraphicControl.Paint;
var
  C: TCanvas;
  CompState: IcaComponentState;
begin
  CompState := TcaComponentState.Create(Self);
  if not CompState.IsDestroying then
    if AutoSize then AdjustSize;
  C := GetOffScreenCanvas;
  BufferedPaint(C, ClientRect);
  if Assigned(FOnPaint) then
    FOnPaint(Self, C, ClientRect);
  UpdateOnScreenBitmap;
end;

procedure TcaGraphicControl.RequestPaint;
begin
  Invalidate;
end;

procedure TcaGraphicControl.UpdateOnScreenBitmap;
begin
  BitBlt(FCanvas.Handle, 0, 0, Width, Height, FOffScreenBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

  // Message handlers 

procedure TcaGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseIsOver := True;
  DoMouseEnter;
end;

procedure TcaGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseIsOver := False;
  DoMouseLeave;
end;

procedure TcaGraphicControl.WMPaint(var Message: TWMPaint);
begin
  if (FOffScreenBitmap <> nil) and (Message.DC <> 0) then
    begin
      FCanvas.Lock;
      try
        FCanvas.Handle := Message.DC;
        try
          Paint;
        finally
          FCanvas.Handle := 0;
        end;
      finally
        FCanvas.Unlock;
      end;
    end;
end;

  // Property methods 

function TcaGraphicControl.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TcaGraphicControl.GetMouseIsDown: Boolean;
begin
  Result := FMouseIsDown;
end;

function TcaGraphicControl.GetMouseIsOver: Boolean;
begin
  Result := FMouseIsOver;
end;

function TcaGraphicControl.GetOnPaint: TcaPaintEvent;
begin
  Result := FOnPaint;
end;

procedure TcaGraphicControl.SetAutoSizeMargin(const Value: Integer);
begin
  if Value <> FAutoSizeMargin then
    begin
      FAutoSizeMargin := Value;
      RequestPaint;
    end;
end;

procedure TcaGraphicControl.SetOnPaint(const Value: TcaPaintEvent);
begin
  FOnPaint := Value;
end;

  //---------------------------------------------------------------------------
  // TcaSpacer                                                                 
  //---------------------------------------------------------------------------

constructor TcaSpacer.Create(AOwner: TComponent);
begin
  inherited;
  Width := 8;
  Height := 25;
end;

  // Protected methods 

type
  TControlEx = class(TControl);

procedure TcaSpacer.Paint;
var
  C: TCanvas;
  X1: Integer;
  X2: Integer;
  Y1: Integer;
  Y2: Integer;
  O: TControlEx;
  R: TRect;
begin
  if FGrooved then
    begin
      C := OffScreenCanvas;
      X1 := Width div 2;
      X2 := X1 + 1;
      Y1 := 1;
      Y2 := Height;
      if Owner <> nil then
        if Owner is TControl then
          begin
            O := TControlEx(Owner);
            C.Brush.Color := O.Color;
            R := ClientRect;
            C.FillRect(R);
          end;
      C.Pen.Style := psSolid;
      C.Pen.Color := clBtnShadow;
      C.MoveTo(X1, Y1);
      C.LineTo(X1, Y2);
      C.Pen.Color := clBtnHighlight;
      C.MoveTo(X2, Y1);
      C.LineTo(X2, Y2);
      UpdateOnScreenBitmap;
    end;
end;

  // Property methods 

function TcaSpacer.GetGrooved: Boolean;
begin
  Result := FGrooved;
end;

procedure TcaSpacer.SetGrooved(const Value: Boolean);
begin
  if Value <> FGrooved then
    begin
      FGrooved := Value;
      RequestPaint;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaSplitter                                                               
  //---------------------------------------------------------------------------

function TcaSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TcaSplitter.UpdateControlSize;
var
  Control: TControl;
begin
  Control := FindControl;
  if Control <> nil then
    begin
      case Align of
        alNone:     Pass;
        alTop:      Control.Height := FPosition;
        alBottom:   Control.Height := FPosition;
        alLeft:     Control.Width := FPosition;
        alRight:    Control.Width := FPosition;
        alClient:   Pass;
        {$IFDEF D7_UP}
        alCustom:   Pass;
        {$ENDIF}
      end;
    end;
end;

procedure TcaSplitter.UpdatePosition;
var
  Control: TControl;
begin
  Control := FindControl;
  if Control <> nil then
    begin
      case Align of
        alNone:     Pass;
        alTop:      FPosition := Control.Height;
        alBottom:   FPosition := Control.Height;
        alLeft:     FPosition := Control.Width;
        alRight:    FPosition := Control.Width;
        alClient:   Pass;
        {$IFDEF D7_UP}
        alCustom:   Pass;
        {$ENDIF}
      end;
    end;
end;

  // Property methods 

function TcaSplitter.GetPosition: Integer;
begin
  UpdatePosition;
  Result := FPosition;
end;

procedure TcaSplitter.SetPosition(const Value: Integer);
begin
  if Value <> FPosition then
    begin
      FPosition := Value;
      UpdateControlSize;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaGraphicBox                                                             
  //---------------------------------------------------------------------------

  // Protected methods 

procedure TcaGraphicBox.BufferedPaint(C: TCanvas; R: TRect);
var
  P1, P2, P3, P4: TPoint;
begin
  C.Brush.Color := FColor;
  C.Brush.Style := bsSolid;
  C.Pen.Color := FFrameColor;
  C.Pen.Style := psSolid;
  P1 := Point(0, 0);
  P2 := Point(Pred(Width), 0);
  P3 := Point(Pred(Width), Pred(Height));
  P4 := Point(0, Pred(Height));
  C.Polygon([P1, P2, P3, P4]);
end;

  // Property methods 

function TcaGraphicBox.GetColor: TColor;
begin
  Result := FColor;
end;

function TcaGraphicBox.GetFrameColor: TColor;
begin
  Result := FFrameColor;
end;

function TcaGraphicBox.GetFrameWidth: Integer;
begin
  Result := FFrameWidth;
end;

procedure TcaGraphicBox.SetColor(const Value: TColor);
begin
  if Value <> FColor then
    begin
      FColor := Value;
      RequestPaint;
    end;
end;

procedure TcaGraphicBox.SetFrameColor(const Value: TColor);
begin
  if Value <> FFrameColor then
    begin
      FFrameColor := Value;
      RequestPaint;
    end;
end;

procedure TcaGraphicBox.SetFrameWidth(const Value: Integer);
begin
  if Value <> FFrameWidth then
    begin
      FFrameWidth := Value;
      RequestPaint;
    end;
end;

end.


