unit caMenu;

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
  ExtCtrls,
  Forms,

  // ca units 
  caLog,
  caUtils,
  caTypes,
  caClasses,
  caControls,
  caGraphics,
  caButtons,
  caSizeBar;

type

  TcaMenuItemButton = class;

  TcaMenuPage = class;

  TcaCustomMenu = class;

  TcaMenu = class;

  TcaMenuPageList = class;

  TcaMenuItemButtonList = class;

  TcaSortMenuPagesEvent = procedure(Sender: TObject; AMenuPages: TcaMenuPageList) of object;

  TcaSortMenuButtonsEvent = procedure(Sender: TObject; AMenuButtons: TcaMenuItemButtonList) of object;

  TcaMenuPageEvent = procedure(Sender: TObject; AMenuPage: TcaMenuPage) of object;

  TcaMenuItemButtonShowingEvent = procedure(Sender: TObject; AMenuButton: TcaMenuItemButton; var IsShowing: Boolean) of object;

  TcaMenuPageShowingEvent = procedure(Sender: TObject; AMenuPage: TcaMenuPage; var IsShowing: Boolean) of object;

  //---------------------------------------------------------------------------
  // TcaMenuMouseAwareButtonProperties                                         
  //---------------------------------------------------------------------------

  TcaMenuMouseAwareButtonProperties = class(TcaCustomButtonProperties)
  published
    // Promoted properties 
    property Color;
    property Color3DLight;
    property ColorBtnHighlight;
    property ColorBtnShadow;
    property ColorWindowFrame;
    property Font;
    property Style;
  end;

  //---------------------------------------------------------------------------
  // TcaMenuCommonPageButtonProperties                                         
  //---------------------------------------------------------------------------

  TcaMenuCommonPageButtonProperties = class(TcaCustomButtonProperties)
  published
    // Promoted properties 
    property Height;
  end;

  //---------------------------------------------------------------------------
  // TcaMenuCommonItemButtonProperties                                         
  //---------------------------------------------------------------------------

  TcaMenuCommonItemButtonProperties = class(TcaCustomButtonProperties)
  private
    // Private fields 
    FButtonGap: Integer;
    FCentered: Boolean;
    FHighlightColor: TColor;
    FHighlightTextColor: TColor;
    FSpacing: Integer;
    FTopGap: Integer;
    // Property methods 
    procedure SetButtonGap(const Value: Integer);
    procedure SetCentered(const Value: Boolean);
    procedure SetHighlightColor(const Value: TColor);
    procedure SetHighlightTextColor(const Value: TColor);
    procedure SetSpacing(const Value: Integer);
    procedure SetTopGap(const Value: Integer);
  public
    // Public methods 
    procedure Assign(Source: TPersistent); override;
  published
    // Promoted properties 
    property Left;
    property Height;
    property Width;
    // Published properties 
    property ButtonGap: Integer read FButtonGap write SetButtonGap;
    property Centered: Boolean read FCentered write SetCentered;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor;
    property HighlightTextColor: TColor read FHighlightTextColor write SetHighlightTextColor;
    property Spacing: Integer read FSpacing write SetSpacing;
    property TopGap: Integer read FTopGap write SetTopGap;
  end;

  //---------------------------------------------------------------------------
  // TcaMenuButtonProperties                                                   
  //---------------------------------------------------------------------------

  TcaMenuButtonProperties = class(TcaNotifyProperties)
  private
    FMenu: TcaCustomMenu;
    // Property fields 
    FCommonItemButtons: TcaMenuCommonItemButtonProperties;
    FCommonPageButtons: TcaMenuCommonPageButtonProperties;
    FDefaultItemButtons: TcaMenuMouseAwareButtonProperties;
    FDefaultPageButtons: TcaMenuMouseAwareButtonProperties;
    FMouseOverItemButtons: TcaMenuMouseAwareButtonProperties;
    FMouseOverPageButtons: TcaMenuMouseAwareButtonProperties;
    // Property methods 
    procedure SetCommonItemButtons(const Value: TcaMenuCommonItemButtonProperties);
    procedure SetCommonPageButtons(const Value: TcaMenuCommonPageButtonProperties);
    procedure SetDefaultItemButtons(const Value: TcaMenuMouseAwareButtonProperties);
    procedure SetDefaultPageButtons(const Value: TcaMenuMouseAwareButtonProperties);
    procedure SetMouseOverItemButtons(const Value: TcaMenuMouseAwareButtonProperties);
    procedure SetMouseOverPageButtons(const Value: TcaMenuMouseAwareButtonProperties);
    // Private methods 
    procedure ChangeNotifier;
    procedure CreateNestedProperties;
    procedure FreeNestedProperties;
    procedure InitializeCommonItemButtons;
    procedure InitializeCommonPageButtons;
    procedure InitializeDefaultItemButtons;
    procedure InitializeDefaultPageButtons;
    procedure InitializeMouseOverItemButtons;
    procedure InitializeMouseOverPageButtons;
    procedure InitializeNestedProperties;
    procedure UpdateItemButtons;
  protected
    // Protected virtual methods 
    procedure Finalize; override;
    procedure Initialize; override;
  public
    // Properties 
    property Menu: TcaCustomMenu read FMenu write FMenu;
  published
    // Published properties 
    property CommonItemButtons: TcaMenuCommonItemButtonProperties read FCommonItemButtons write SetCommonItemButtons;
    property CommonPageButtons: TcaMenuCommonPageButtonProperties read FCommonPageButtons write SetCommonPageButtons;
    property DefaultItemButtons: TcaMenuMouseAwareButtonProperties read FDefaultItemButtons write SetDefaultItemButtons;
    property DefaultPageButtons: TcaMenuMouseAwareButtonProperties read FDefaultPageButtons write SetDefaultPageButtons;
    property MouseOverItemButtons: TcaMenuMouseAwareButtonProperties read FMouseOverItemButtons write SetMouseOverItemButtons;
    property MouseOverPageButtons: TcaMenuMouseAwareButtonProperties read FMouseOverPageButtons write SetMouseOverPageButtons;
  end;

  //---------------------------------------------------------------------------
  // TcaMenuPageButton                                                         
  //---------------------------------------------------------------------------

  TcaMenuPageButton = class(TcaDesignTimeButton)
  private
  protected
  public
  end;

  //---------------------------------------------------------------------------
  // TcaMenuItemButton                                                         
  //---------------------------------------------------------------------------

  TcaMenuItemButton = class(TcaSpeedButton)
  private
    // Property fields 
    FCommonProperties: TcaMenuCommonItemButtonProperties;
    FDefaultColor: TColor;
    FDefaultFont: TFont;
    FHighlightColor: TColor;
    FHighlighted: Boolean;
    FHighlightTextColor: TColor;
    FShowButton: Boolean;
    FSortIndex: Integer;
    FMenuPage: TcaMenuPage;
    // Property methods 
    procedure SetCommonProperties(const Value: TcaMenuCommonItemButtonProperties);
    procedure SetHighlightColor(const Value: TColor);
    procedure SetHighlighted(const Value: Boolean);
    procedure SetHighlightTextColor(const Value: TColor);
    // Private methods 
    procedure UpdateFromCommonProperties;
    procedure UpdateHighlight;
    // Control message handlers 
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Properties 
    property CommonProperties: TcaMenuCommonItemButtonProperties read FCommonProperties write SetCommonProperties;
    property MenuPage: TcaMenuPage read FMenuPage write FMenuPage;
    property ShowButton: Boolean read FShowButton write FShowButton;
    property SortIndex: Integer read FSortIndex write FSortIndex;
  published
    // Published properties 
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor;
    property Highlighted: Boolean read FHighlighted write SetHighlighted;
    property HighlightTextColor: TColor read FHighlightTextColor write SetHighlightTextColor;
  end;

  //---------------------------------------------------------------------------
  // TcaMenuItemButtonList                                                     
  //---------------------------------------------------------------------------

  TcaMenuItemButtonList = class(TObject)
  private
    // Private fields 
    FList: TList;
    FMenuPage: TcaMenuPage;
    // Property methods 
    function GetCount: Integer;
    function GetMenuButton(Index: Integer): TcaMenuItemButton;
  public
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    function IndexOf(AMenuButton: TcaMenuItemButton): Integer;
    procedure Add(AMenuButton: TcaMenuItemButton);
    procedure Clear;
    procedure Sort(Compare: TListSortCompare);
    // Properties 
    property Count: Integer read GetCount;
    property MenuButtons[Index: Integer]: TcaMenuItemButton read GetMenuButton; default;
    property MenuPage: TcaMenuPage read FMenuPage write FMenuPage;
  end;

  //----------------------------------------------------------------------------
  // TcaMenuPage                                                                
  //----------------------------------------------------------------------------

  TcaMenuPage = class(TcaCustomPanel)
  private
    // Property fields 
    FActive: Boolean;
    FAllowAllUp: Boolean;
    FPageButton: TcaMenuPageButton;
    FButtonList: TcaMenuItemButtonList;
    FCurrentButton: Integer;
    FFillColor: TColor;
    FFillColor2: TColor;
    FFocusControl: TWinControl;
    FMenu: TcaCustomMenu;
    FOnActiveChanged: TNotifyEvent;
    FOnAfterSortMenuButtons: TcaSortMenuButtonsEvent;
    FOnBeforeSortMenuButtons: TcaSortMenuButtonsEvent;
    FOnIsMenuButtonShowing: TcaMenuItemButtonShowingEvent;
    FItemButtonStartIndex: Integer;
    FPanel: TCustomPanel;
    FPanelGap: Integer;
    FScrollUpButton: TcaRepeatSpeedButton;
    FScrollDownButton: TcaRepeatSpeedButton;
    FShowPage: Boolean;
    FShowPageName: Boolean;
    FSortIndex: Integer;
    // Property methods 
    function GetButtonCaption: TCaption;
    function GetButtonCount: Integer;
    function GetButton(Index: Integer): TcaMenuItemButton;
    function GetButtonDown(Index: Integer): Boolean;
    function GetButtonStyle: TcaButtonStyle;
    function GetCurrentButton: TcaMenuItemButton;
    function GetDownButton: TcaMenuItemButton;
    function GetGlyph: TBitmap;
    function GetLastButton: TcaMenuItemButton;
    procedure SetActive(const Value: Boolean);
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetButtonCaption(const Value: TCaption);
    procedure SetButtonDown(Index: Integer; const Down: Boolean);
    procedure SetButtonStyle(const Value: TcaButtonStyle);
    procedure SetCurrentButton(const Value: TcaMenuItemButton);
    procedure SetFillColor(const Value: TColor);
    procedure SetFillColor2(const Value: TColor);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetMenu(const Value: TcaCustomMenu);
    procedure SetPanel(const Value: TCustomPanel);
    procedure SetPanelGap(const Value: Integer);
    procedure SetShowPageName(const Value: Boolean);
    // Private methods 
    function GetBottomOffset: Integer;
    procedure AlignPanel;
    procedure PositionItemButton;
    procedure PositionScrollButtons;
    procedure SetAllBtnsEnabled(const IsEnabled: Boolean);
    procedure SetButtonUnderline(const ButtonNum: Integer; const DoUnderline: Boolean);
    procedure UpdateButtonList;
    // Event handlers
    procedure ItemButtonEnterEvent(Sender: TObject);
    procedure ItemButtonLeaveEvent(Sender: TObject);
    procedure PageButtonClickEvent(Sender: TObject);
    procedure PageButtonEnterEvent(Sender: TObject);
    procedure PageButtonLeaveEvent(Sender: TObject);
    procedure ScrollButtonClickEvent(Sender: TObject);
    // Windows message handlers 
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    // Component message handlers 
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    // Protected methods 
    function CreateButton: TcaMenuItemButton; virtual;
    procedure CreateWnd; override;
    procedure DoActiveChanged; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    function AddButton(const AButtonCaption: string; AClickEvent: TNotifyEvent): TcaMenuItemButton;
    function GetNextButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
    function GetNextEnabledButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
    function GetPriorButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
    function GetPriorEnabledButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
    procedure AllButtonsClearHighlight;
    procedure AllButtonsDown;
    procedure AllButtonsUp;
    procedure ClearButtons;
    procedure ClickCurrentButton;
    procedure DisableButtons;
    procedure EnableButtons;
    procedure UnderlineNextButton;
    procedure UnderlinePrevButton;
    procedure UpdateItemButtons;
    procedure UpdatePageButton;    
    // TCustomPanel 
    property DockManager;
    // TcaMenuPage 
    property ButtonDown[Index: Integer]: Boolean read GetButtonDown write SetButtonDown;
    property Buttons[Index: Integer]: TcaMenuItemButton read GetButton;
    property CurrentButton: TcaMenuItemButton read GetCurrentButton write SetCurrentButton;
    property DownButton: TcaMenuItemButton read GetDownButton;
    property LastButton: TcaMenuItemButton read GetLastButton;
    property Menu: TcaCustomMenu read FMenu write SetMenu;
    property ShowPage: Boolean read FShowPage write FShowPage;
    property SortIndex: Integer read FSortIndex write FSortIndex;
  published
    // TcaMenuPage 
    property Active: Boolean read FActive write SetActive;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp;
    property ButtonCaption: TCaption read GetButtonCaption write SetButtonCaption;
    property ButtonCount: Integer read GetButtonCount;
    property ButtonStyle: TcaButtonStyle read GetButtonStyle write SetButtonStyle;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColor2: TColor read FFillColor2 write SetFillColor2;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
    property OnAfterSortMenuButtons: TcaSortMenuButtonsEvent read FOnAfterSortMenuButtons write FOnAfterSortMenuButtons;
    property OnBeforeSortMenuButtons: TcaSortMenuButtonsEvent read FOnBeforeSortMenuButtons write FOnBeforeSortMenuButtons;
    property OnIsMenuButtonShowing: TcaMenuItemButtonShowingEvent read FOnIsMenuButtonShowing write FOnIsMenuButtonShowing;
    property Panel: TCustomPanel read FPanel write SetPanel;
    property PanelGap: Integer read FPanelGap write SetPanelGap;
    property ShowPageName: Boolean read FShowPageName write SetShowPageName;
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
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
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

  //---------------------------------------------------------------------------
  // TcaMenuPageList                                                           
  //---------------------------------------------------------------------------

  TcaMenuPageList = class(TObject)
  private
    FList: TList;
    FMenu: TcaCustomMenu;
    // Property methods 
    function GetCount: Integer;
    function GetMenuPage(Index: Integer): TcaMenuPage;
  protected
    property Menu: TcaCustomMenu read FMenu write FMenu;
  public
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    function IndexOf(AMenuPage: TcaMenuPage): Integer;
    procedure Add(AMenuPage: TcaMenuPage);
    procedure Clear;
    procedure Sort(Compare: TListSortCompare);
    // Properties 
    property Count: Integer read GetCount;
    property MenuPages[Index: Integer]: TcaMenuPage read GetMenuPage; default;
  end;

  //---------------------------------------------------------------------------
  // TcaCustomMenu                                                             
  //---------------------------------------------------------------------------

  TcaCustomMenu = class(TcaCustomPanel)
  private
    FBigWidth: Integer;
    FButtonProperties: TcaMenuButtonProperties;
    FEnabled: Boolean;
    FOnActivateMenuPage: TcaMenuPageEvent;
    FOnAddMenuPage: TcaMenuPageEvent;
    FOnAfterSortMenuPages: TcaSortMenuPagesEvent;
    FOnBeforeSortMenuPages: TcaSortMenuPagesEvent;
    FOnSortMenuPagesRequested: TNotifyEvent;
    FOnIsMenuPageShowing: TcaMenuPageShowingEvent;
    FPageList: TcaMenuPageList;
    FPageMargin: TcaMargin;
    FSizeBar: TcaSizeBarProperties;
    FSizeBarCtrl: TcaSizeBar;
    FSizeBarMargin: TcaMargin;
    FState: TcaMenuState;
    FUpdatePages: Boolean;
    FUseSizeBar: Boolean;
    // Property methods 
    function GetActivePage: TcaMenuPage;
    function GetFirstPage: TcaMenuPage;
    function GetFirstVisiblePage: TcaMenuPage;
    function GetLastPage: TcaMenuPage;
    function GetPage(Index: Integer): TcaMenuPage;
    function GetSizeBarBtnEnabled: Boolean;
    procedure SetActivePage(const Value: TcaMenuPage);
    procedure SetBigWidth(const Value: Integer);
    procedure SetButtonProperties(const Value: TcaMenuButtonProperties);
    procedure SetEnabled(const Value: Boolean); reintroduce;
    procedure SetSizeBarBtnEnabled(const Value: Boolean);
    procedure SetState(const Value: TcaMenuState);
    procedure SetUseSizeBar(const Value: Boolean);
    // Event handlers 
    procedure PageMarginChangedEvent(Sender: TObject);
    procedure SizeBarMarginChangedEvent(Sender: TObject);
    procedure SizeBarStateChangeEvent(Sender: TObject);
    // Private methods 
    procedure ArrangeMenu;
    procedure CreateButtonProperties;
    procedure CreateObjects;
    procedure CreatePageList;
    procedure CreatePageMargin;
    procedure CreateSizeBarControl;
    procedure CreateSizeBarMargin;
    procedure CreateSizeBarProperties;
    procedure FreeObjects;
    procedure PositionSizeBar;
    procedure UpdateEnabledState;
    procedure UpdatePageButtons;
  protected
    // Protected methods 
    function GetClientRect: TRect; override;
    procedure ActiveChanged(APage: TcaMenuPage);
    procedure CreateWnd; override;
    procedure DoActivateMenuPage(APage: TcaMenuPage); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PositionPages;
    // Protected properties 
    property ActivePage: TcaMenuPage read GetActivePage write SetActivePage;
    property BigWidth: Integer read FBigWidth write SetBigWidth;
    property ButtonProperties: TcaMenuButtonProperties read FButtonProperties write SetButtonProperties;
    property FirstPage: TcaMenuPage read GetFirstPage;
    property FirstVisiblePage: TcaMenuPage read GetFirstVisiblePage;
    property LastPage: TcaMenuPage read GetLastPage;
    property OnActivateMenuPage: TcaMenuPageEvent read FOnActivateMenuPage write FOnActivateMenuPage;
    property OnAfterSortMenuPages: TcaSortMenuPagesEvent read FOnAfterSortMenuPages write FOnAfterSortMenuPages;
    property OnBeforeSortMenuPages: TcaSortMenuPagesEvent read FOnBeforeSortMenuPages write FOnBeforeSortMenuPages;
    property OnSortMenuPagesRequested: TNotifyEvent read FOnSortMenuPagesRequested write FOnSortMenuPagesRequested;
    property PageMargin: TcaMargin read FPageMargin write FPageMargin;
    property Pages[Index: Integer]: TcaMenuPage read GetPage;
    property SizeBar: TcaSizeBarProperties read FSizeBar write FSizeBar;
    property SizeBarBtnEnabled: Boolean read GetSizeBarBtnEnabled write SetSizeBarBtnEnabled;
    property SizeBarMargin: TcaMargin read FSizeBarMargin write FSizeBarMargin;
    property State: TcaMenuState read FState write SetState;
    property UpdatePages: Boolean read FUpdatePages write FUpdatePages;
    property UseSizeBar: Boolean read FUseSizeBar write SetUseSizeBar;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    function AddMenuPage(ASortMenuPages: Boolean = True): TcaMenuPage;
    function FindMenuPageCaption(const AMenuPageCaption: TCaption): TcaMenuPage;
    function FindMenuPageName(const AMenuPageName: string): TcaMenuPage;
    function PageCount: Integer;
    procedure AllButtonsClearHighlight;
    procedure Clear;
    procedure GotoNextPage;
    procedure GotoPriorPage;
    procedure Resize; override;
    procedure SortMenuPages;
    procedure UpdatePageList;
    procedure UpdateItemButtons;
  published
    // Published properties 
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OnAddMenuPage: TcaMenuPageEvent read FOnAddMenuPage write FOnAddMenuPage;
    property OnIsMenuPageShowing: TcaMenuPageShowingEvent read FOnIsMenuPageShowing write FOnIsMenuPageShowing;
  end;

  //---------------------------------------------------------------------------
  // TcaMenu                                                                   
  //---------------------------------------------------------------------------

  TcaMenu = class(TcaCustomMenu)
  public
    // TCustomPanel 
    property DockManager;
    property Pages;
    // TcaMenu 
    property FirstVisiblePage;
  published
    // TcaMenu 
    property ActivePage;
    property BigWidth;
    property ButtonProperties;
    property OnAfterSortMenuPages;
    property OnBeforeSortMenuPages;
    property PageMargin;
    property SizeBar;
    property SizeBarBtnEnabled;
    property SizeBarMargin;
    property State;
    property UpdatePages;
    property UseSizeBar;
    // TCustomPanel 
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Caption;
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
    property Frame;
    property FullRepaint;
    property Locked;
    property OnActivateMenuPage;
    property OnAddMenuPage;
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
    property OnIsMenuPageShowing;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSortMenuPagesRequested;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
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
  end;

function MenuPageSortFunction(Item1, Item2: Pointer): Integer;
function MenuButtonSortFunction(Item1, Item2: Pointer): Integer;

implementation

function MenuPageSortFunction(Item1, Item2: Pointer): Integer;
var
  Page1: TcaMenuPage;
  Page2: TcaMenuPage;
begin
  Page1 := TcaMenuPage(Item1);
  Page2 := TcaMenuPage(Item2);
  Result := Page1.SortIndex - Page2.SortIndex;
end;

function MenuButtonSortFunction(Item1, Item2: Pointer): Integer;
var
  Btn1: TcaMenuItemButton;
  Btn2: TcaMenuItemButton;
begin
  Btn1 := TcaMenuItemButton(Item1);
  Btn2 := TcaMenuItemButton(Item2);
  Result := Btn1.SortIndex - Btn2.SortIndex;
end;

  //---------------------------------------------------------------------------
  // TcaMenuCommonItemButtonProperties                                         
  //---------------------------------------------------------------------------

  // Public methods 

procedure TcaMenuCommonItemButtonProperties.Assign(Source: TPersistent);
var
  SourceProps: TcaMenuCommonItemButtonProperties;
begin
  inherited;
  if Source is TcaMenuCommonItemButtonProperties then
    begin
      SourceProps := TcaMenuCommonItemButtonProperties(Source);
      FButtonGap := SourceProps.ButtonGap;
      FCentered := SourceProps.Centered;
      FHighlightColor := SourceProps.HighlightColor;
      FHighlightTextColor := SourceProps.HighlightTextColor;
      FSpacing := SourceProps.Spacing;
      FTopGap := SourceProps.TopGap;
      Height := SourceProps.Height;
      Left := SourceProps.Left;
      Top := SourceProps.Top;
      Width := SourceProps.Width;
    end;
end;

  // Property methods 

procedure TcaMenuCommonItemButtonProperties.SetButtonGap(const Value: Integer);
begin
  SetIntegerProperty(FButtonGap, Value);
end;

procedure TcaMenuCommonItemButtonProperties.SetCentered(const Value: Boolean);
begin
  SetBooleanProperty(FCentered, Value);
end;

procedure TcaMenuCommonItemButtonProperties.SetHighlightColor(const Value: TColor);
begin
  SetColorProperty(FHighlightColor, Value);
end;

procedure TcaMenuCommonItemButtonProperties.SetHighlightTextColor(const Value: TColor);
begin
  SetColorProperty(FHighlightTextColor, Value);
end;

procedure TcaMenuCommonItemButtonProperties.SetSpacing(const Value: Integer);
begin
  SetIntegerProperty(FSpacing, Value);
end;

procedure TcaMenuCommonItemButtonProperties.SetTopGap(const Value: Integer);
begin
  SetIntegerProperty(FTopGap, Value);
end;

  //---------------------------------------------------------------------------
  // TcaMenuButtonProperties                                                   
  //---------------------------------------------------------------------------

  // Protected virtual methods 

procedure TcaMenuButtonProperties.Finalize;
begin
  inherited;
  FreeNestedProperties;
end;

procedure TcaMenuButtonProperties.Initialize;
begin
  inherited;
  CreateNestedProperties;
  InitializeNestedProperties;
end;

  // Private methods 

procedure TcaMenuButtonProperties.ChangeNotifier;
begin
  UpdateItemButtons;  
end;

procedure TcaMenuButtonProperties.CreateNestedProperties;
begin
  FCommonItemButtons := TcaMenuCommonItemButtonProperties.Create(ChangeNotifier);
  FCommonPageButtons := TcaMenuCommonPageButtonProperties.Create(ChangeNotifier);
  FDefaultItemButtons := TcaMenuMouseAwareButtonProperties.Create(ChangeNotifier);
  FDefaultPageButtons := TcaMenuMouseAwareButtonProperties.Create(ChangeNotifier);
  FMouseOverItemButtons := TcaMenuMouseAwareButtonProperties.Create(ChangeNotifier);
  FMouseOverPageButtons := TcaMenuMouseAwareButtonProperties.Create(ChangeNotifier);
end;

procedure TcaMenuButtonProperties.FreeNestedProperties;
begin
  FCommonItemButtons.Free;
  FCommonPageButtons.Free;
  FDefaultItemButtons.Free;
  FDefaultPageButtons.Free;
  FMouseOverItemButtons.Free;
  FMouseOverPageButtons.Free;
end;

procedure TcaMenuButtonProperties.InitializeCommonItemButtons;
begin
  FCommonItemButtons.Left := 0;
  FCommonItemButtons.Height := 26;
  FCommonItemButtons.Width := 100;
  FCommonItemButtons.ButtonGap := 8;
  FCommonItemButtons.Centered := True;
  FCommonItemButtons.HighlightColor := clBlack;
  FCommonItemButtons.HighlightTextColor := clBlack;
  FCommonItemButtons.Spacing := 0;
  FCommonItemButtons.TopGap := 12;
end;

procedure TcaMenuButtonProperties.InitializeCommonPageButtons;
begin
  FCommonPageButtons.Height := 26;
end;

procedure TcaMenuButtonProperties.InitializeDefaultItemButtons;
begin
  FDefaultItemButtons.Color := clBtnShadow;
  FDefaultItemButtons.Color3DLight := cl3DLight;
  FDefaultItemButtons.ColorBtnHighlight := clBtnFace;
  FDefaultItemButtons.ColorBtnShadow := cl3DDkShadow;
  FDefaultItemButtons.ColorWindowFrame := clWindowFrame;
  FDefaultItemButtons.Font.Color := clWindowText;
  FDefaultItemButtons.Font.Height := -11;
  FDefaultItemButtons.Font.Name := 'MS Sans Serif';
  FDefaultItemButtons.Font.Size := 8;
  FDefaultItemButtons.Style := bsFlat;
end;

procedure TcaMenuButtonProperties.InitializeDefaultPageButtons;
begin
  FDefaultPageButtons.Color := clBtnFace;
  FDefaultPageButtons.Color3DLight := cl3DLight;
  FDefaultPageButtons.ColorBtnHighlight := clBtnHighlight;
  FDefaultPageButtons.ColorBtnShadow := clBtnShadow;
  FDefaultPageButtons.ColorWindowFrame := clWindowFrame;
  FDefaultPageButtons.Font.Charset := DEFAULT_CHARSET;
  FDefaultPageButtons.Font.Color := clWindowText;
  FDefaultPageButtons.Font.Size := 8;
  FDefaultPageButtons.Font.Name := 'MS Sans Serif';
  FDefaultPageButtons.Font.Style := [fsBold];
  FDefaultPageButtons.Style := bsThin;
end;

procedure TcaMenuButtonProperties.InitializeMouseOverItemButtons;
begin
  FMouseOverItemButtons.Color := clBtnShadow;
  FMouseOverItemButtons.Color3DLight := cl3DLight;
  FMouseOverItemButtons.ColorBtnHighlight := clBtnFace;
  FMouseOverItemButtons.ColorBtnShadow := cl3DDkShadow;
  FMouseOverItemButtons.ColorWindowFrame := clWindowFrame;
  FMouseOverItemButtons.Font.Charset := DEFAULT_CHARSET;
  FMouseOverItemButtons.Font.Color := clWindowText;
  FMouseOverItemButtons.Font.Size := 8;
  FMouseOverItemButtons.Font.Name := 'MS Sans Serif';
  FMouseOverItemButtons.Font.Style := [];
  FMouseOverItemButtons.Style := bsThin;
end;

procedure TcaMenuButtonProperties.InitializeMouseOverPageButtons;
begin
  FMouseOverPageButtons.Color := clBtnFace;
  FMouseOverPageButtons.Color3DLight := cl3DLight;
  FMouseOverPageButtons.ColorBtnHighlight := clBtnHighlight;
  FMouseOverPageButtons.ColorBtnShadow := clBtnShadow;
  FMouseOverPageButtons.ColorWindowFrame := clWindowFrame;
  FMouseOverPageButtons.Font.Charset := DEFAULT_CHARSET;
  FMouseOverPageButtons.Font.Color := clBlue;
  FMouseOverPageButtons.Font.Size := 8;
  FMouseOverPageButtons.Font.Name := 'MS Sans Serif';
  FMouseOverPageButtons.Font.Style := [fsBold];
  FMouseOverPageButtons.Style := bsDefault;
end;

procedure TcaMenuButtonProperties.InitializeNestedProperties;
begin
  InitializeCommonItemButtons;
  InitializeCommonPageButtons;
  InitializeDefaultItemButtons;
  InitializeDefaultPageButtons;
  InitializeMouseOverItemButtons;
  InitializeMouseOverPageButtons;
end;

procedure TcaMenuButtonProperties.UpdateItemButtons;
begin
  if FMenu <> nil then FMenu.UpdateItemButtons;
end;

  // Property methods 

procedure TcaMenuButtonProperties.SetCommonItemButtons(const Value: TcaMenuCommonItemButtonProperties);
begin
  FCommonItemButtons.Assign(Value);
end;

procedure TcaMenuButtonProperties.SetCommonPageButtons(const Value: TcaMenuCommonPageButtonProperties);
begin
  FCommonPageButtons.Assign(Value);
end;

procedure TcaMenuButtonProperties.SetDefaultItemButtons(const Value: TcaMenuMouseAwareButtonProperties);
begin
  FDefaultItemButtons.Assign(Value);
end;

procedure TcaMenuButtonProperties.SetDefaultPageButtons(const Value: TcaMenuMouseAwareButtonProperties);
begin
  FDEfaultPageButtons.Assign(Value);
end;

procedure TcaMenuButtonProperties.SetMouseOverItemButtons(const Value: TcaMenuMouseAwareButtonProperties);
begin
  FMouseOverItemButtons.Assign(Value);
end;

procedure TcaMenuButtonProperties.SetMouseOverPageButtons(const Value: TcaMenuMouseAwareButtonProperties);
begin
  FMouseOverPageButtons.Assign(Value);
end;

  //---------------------------------------------------------------------------
  // TcaMenuItemButton                                                         
  //---------------------------------------------------------------------------

constructor TcaMenuItemButton.Create(AOwner: TComponent);
begin
  inherited;
  FCommonProperties := TcaMenuCommonItemButtonProperties.Create(nil);
  FDefaultColor := clBtnFace;
  FDefaultFont := TFont.Create;
  FDefaultFont.Assign(Font);
  FHighlightColor := clBtnHighlight;
  FHighlightTextColor := Font.Color;
  FShowButton := True;
end;

destructor TcaMenuItemButton.Destroy;
begin
  FDefaultFont.Free;
  FCommonProperties.Free;
  inherited;
end;

  // Private methods 

{$IFDEF NEVER}

  Btn.Width := FMenu.ButtonProperties.CommonItemButtons.Width;
  if FMenu.ButtonProperties.CommonItemButtons.Centered then
    Btn.Left := (Width div 2) - (Btn.Width div 2);
  Btn.Height := FMenu.ButtonProperties.CommonItemButtons.Height;
  Btn.HighlightColor := FMenu.ButtonProperties.CommonItemButtons.HighlightColor;
  Btn.HighlightTextColor := FMenu.ButtonProperties.CommonItemButtons.HighlightTextColor;
  if not FMenu.ButtonProperties.CommonItemButtons.Centered then
    Btn.Left := FMenu.ButtonProperties.CommonItemButtons.Left;
  Btn.Spacing := FMenu.ButtonProperties.CommonItemButtons.Spacing;
  Btn.Top := ATop + (Index - FItemButtonStartIndex) * (AButtonGap + Btn.Height);

{$ENDIF}

procedure TcaMenuItemButton.UpdateFromCommonProperties;
begin
  BeginUpdate;
  Width := FCommonProperties.Width;
  if FCommonProperties.Centered then
    Left := (Parent.Width div 2) - (Width div 2)
  else
    Left := FCommonProperties.Left;
  Height := FCommonProperties.Height;
  FHighlightColor := FCommonProperties.HighlightColor;
  FHighlightTextColor := FCommonProperties.HighlightTextColor;
  Spacing := FCommonProperties.Spacing;
  Top := FCommonProperties.Top;
  EndUpdate;
end;

procedure TcaMenuItemButton.UpdateHighlight;
begin
  if FDefaultFont <> nil then
    begin
      if FHighlighted then
        begin
          Color := FHighlightColor;
          Font.Color := FHighlightTextColor;
        end
      else
        begin
          Color := FDefaultColor;
          Font.Assign(FDefaultFont);
        end;
    end;
end;

  // Control message handlers 

procedure TcaMenuItemButton.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if not FHighlighted then
    FDefaultColor := Color;
end;

procedure TcaMenuItemButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not FHighlighted then
    FDefaultFont.Assign(Font);
end;

  // Property methods 

procedure TcaMenuItemButton.SetCommonProperties(const Value: TcaMenuCommonItemButtonProperties);
begin
  FCommonProperties.Assign(Value);
  UpdateFromCommonProperties;
end;

procedure TcaMenuItemButton.SetHighlightColor(const Value: TColor);
begin
  FHighlightColor := Value;
  UpdateHighlight;
end;

procedure TcaMenuItemButton.SetHighlightTextColor(const Value: TColor);
begin
  FHighlightTextColor := Value;
  UpdateHighlight;
end;

procedure TcaMenuItemButton.SetHighlighted(const Value: Boolean);
begin
  FHighlighted := Value;
  UpdateHighlight;
end;

  //---------------------------------------------------------------------------
  // TcaMenuItemButtonList                                                     
  //---------------------------------------------------------------------------

constructor TcaMenuItemButtonList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TcaMenuItemButtonList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TcaMenuItemButtonList.Add(AMenuButton: TcaMenuItemButton);
begin
  FList.Add(AMenuButton);
end;

procedure TcaMenuItemButtonList.Clear;
begin
  FList.Clear;
end;

function TcaMenuItemButtonList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaMenuItemButtonList.GetMenuButton(Index: Integer): TcaMenuItemButton;
begin
  Result := TcaMenuItemButton(FList[Index]);
end;

function TcaMenuItemButtonList.IndexOf(AMenuButton: TcaMenuItemButton): Integer;
begin
  Result := FList.IndexOf(AMenuButton);
end;

procedure TcaMenuItemButtonList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

  //---------------------------------------------------------------------------
  // TcaMenuPage                                                               
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaMenuPage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FButtonList := TcaMenuItemButtonList.Create;
  FButtonList.MenuPage := Self;
  FPageButton := TcaMenuPageButton.Create(Self);
  FPageButton.OnClick := PageButtonClickEvent;
  FPageButton.OnMouseEnter := PageButtonEnterEvent;
  FPageButton.OnMouseLeave := PageButtonLeaveEvent;
  FPageButton.Cursor := crHandPoint;
  FFillColor := clBtnShadow;
  FFillColor2 := clNone;
  Frame.Sides := [];
  // Scroll up button 
  FScrollUpButton := TcaRepeatSpeedButton.Create(Self);
  FScrollUpButton.Style := bsFlat;
  FScrollUpButton.Font.Name := 'Marlett';
  FScrollUpButton.Caption := 't';
  FScrollUpButton.OnClick := ScrollButtonClickEvent;
  FScrollUpButton.Delay := 600;
  FScrollUpButton.Interval := 200;
  FScrollUpButton.AutoRepeat := True;
  // Scroll down button 
  FScrollDownButton := TcaRepeatSpeedButton.Create(Self);
  FScrollDownButton.Style := bsFlat;
  FScrollDownButton.Font.Name := 'Marlett';
  FScrollDownButton.Caption := 'u';
  FScrollDownButton.OnClick := ScrollButtonClickEvent;
  FScrollDownButton.Delay := 600;
  FScrollDownButton.Interval := 200;
  FScrollDownButton.AutoRepeat := True;
end;

destructor TcaMenuPage.Destroy;
begin
  FButtonList.Free;
  inherited;
end;

  // Public methods 

function TcaMenuPage.AddButton(const AButtonCaption: string; AClickEvent: TNotifyEvent): TcaMenuItemButton;
var
  Factory: IcaComponentFactory;
begin
  FMenu.UpdatePageList;
  Factory := TcaComponentFactory.Create(TcaMenuItemButton, Owner);
  Result := TcaMenuItemButton(Factory.NewComponent);

  Result.Properties := FMenu.ButtonProperties.DefaultItemButtons;
  Result.Caption := AButtonCaption;
  Result.OnClick := AClickEvent;
  Result.Cursor := crHandPoint;

  if LastButton <> nil then
    Result.Top := LastButton.Top + 1
  else
    Result.Top := FMenu.ButtonProperties.CommonPageButtons.Height;

  Result.Parent := Self;
  Result.MenuPage := Self;
  FMenu.UpdateItemButtons;
end;

function TcaMenuPage.GetNextButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
var
  Index: Integer;
begin
  Result := nil;
  UpdateButtonList;
  for Index := 0 to FButtonList.Count - 1 do
    begin
      if FButtonList[Index] = AButton then
        begin
          if Index < FButtonList.Count - 1 then
            Result := FButtonList[Index + 1];
          Break;
        end;
    end;
end;

function TcaMenuPage.GetNextEnabledButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
begin
  Result := GetNextButton(AButton);
  while (Result <> nil) and (not Result.Enabled) do
    Result := GetNextButton(Result);
end;

function TcaMenuPage.GetPriorButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
var
  Index: Integer;
begin
  Result := nil;
  UpdateButtonList;
  for Index := 0 to FButtonList.Count - 1 do
    begin
      if FButtonList[Index] = AButton then
        begin
          if Index > 0 then
            Result := FButtonList[Index - 1];
          Break;
        end;
    end;
end;

function TcaMenuPage.GetPriorEnabledButton(AButton: TcaMenuItemButton): TcaMenuItemButton;
begin
  Result := GetPriorButton(AButton);
  while (Result <> nil) and (not Result.Enabled) do
    Result := GetPriorButton(Result);
end;

procedure TcaMenuPage.AllButtonsClearHighlight;
var
  Btn: TcaMenuItemButton;
  Index: Integer;
begin
  UpdateButtonList;
  for Index := 0 to ButtonCount - 1 do
    begin
      Btn := FButtonList[Index];
      Btn.Highlighted := False;
      if Btn.MouseIsDown then
        Btn.Properties := FMenu.ButtonProperties.MouseOverItemButtons
      else
        Btn.Properties := FMenu.ButtonProperties.DefaultItemButtons;
    end;
end;

procedure TcaMenuPage.AllButtonsDown;
var
  Index: Integer;
begin
  for Index := 0 to ButtonCount - 1 do
    ButtonDown[Index] := True;
end;

procedure TcaMenuPage.AllButtonsUp;
var
  Index: Integer;
begin
  for Index := 0 to ButtonCount - 1 do
    ButtonDown[Index] := False;
end;

procedure TcaMenuPage.ClearButtons;
var
  Index: Integer;
  MenuButtonList: TList;
  MenuButton: TcaMenuItemButton;
begin
  MenuButtonList := Auto(TList.Create).Instance;
  for Index := 0 to Pred(Owner.ComponentCount) do
    begin
      if Owner.Components[Index] is TcaMenuItemButton then
        begin
          MenuButton := TcaMenuItemButton(Owner.Components[Index]);
          if MenuButton.MenuPage = Self then
            MenuButtonList.Add(Owner.Components[Index]);
        end;
    end;
  while MenuButtonList.Count > 0 do
    begin
      Owner.RemoveComponent(TComponent(MenuButtonList[0]));
      TcaMenuItemButton(MenuButtonList[0]).Free;
      MenuButtonList.Delete(0);      
    end;
end;

procedure TcaMenuPage.ClickCurrentButton;
var
  Btn: TcaMenuItemButton;
begin
  Btn := GetCurrentButton;
  if Btn <> nil then Btn.Click;
end;

procedure TcaMenuPage.DisableButtons;
begin
  SetAllBtnsEnabled(False);
end;

procedure TcaMenuPage.EnableButtons;
begin
  SetAllBtnsEnabled(True);
end;

procedure TcaMenuPage.UnderlineNextButton;
begin
  SetButtonUnderline(FCurrentButton, False);
  if FCurrentButton = ButtonCount - 1 then
    FCurrentButton := 0
  else
    Inc(FCurrentButton);
  SetButtonUnderline(FCurrentButton, True);
end;

procedure TcaMenuPage.UnderlinePrevButton;
begin
  SetButtonUnderline(FCurrentButton, False);
  if FCurrentButton = 0 then
    FCurrentButton := ButtonCount - 1
  else
    Dec(FCurrentButton);
  SetButtonUnderline(FCurrentButton, True);
end;

procedure TcaMenuPage.UpdateItemButtons;
var
  AButtonGap: Integer;
  ATop: Integer;
  BottomOffset: Integer;
  Btn: TcaMenuItemButton;
  DownInvisibleCount: Integer;
  DownVisible: Boolean;
  Index: Integer;
  InvisibleCount: Integer;
  UpVisible: Boolean;
begin
  if FActive then
    begin
      UpdateButtonList;
      BottomOffset := GetBottomOffset;
      AButtonGap := FMenu.ButtonProperties.CommonItemButtons.ButtonGap;
      ATop := FPageButton.Height + FMenu.ButtonProperties.CommonItemButtons.TopGap;
      InvisibleCount := 0;
      DownInvisibleCount := 0;
      for Index := 0 to FButtonList.Count - 1 do
        begin
          Btn := FButtonList[Index];
          Btn.BeginUpdate;

          Btn.CommonProperties := FMenu.ButtonProperties.CommonItemButtons;
          Btn.Top := ATop + (Index - FItemButtonStartIndex) * (AButtonGap + Btn.Height);
          Btn.Properties := FMenu.ButtonProperties.DefaultItemButtons;

          UpVisible := Btn.Top >= ATop;
          DownVisible := Btn.Top + Btn.Height < Height - BottomOffset;
          Btn.Visible := UpVisible and DownVisible;
          if not DownVisible then Inc(DownInvisibleCount);
          if not Btn.Visible then Inc(InvisibleCount);

          Btn.OnMouseEnter := ItemButtonEnterEvent;
          Btn.OnMouseLeave := ItemButtonLeaveEvent;
          Btn.UseMouseOver := False;

          Btn.EndUpdate;
        end;
      FScrollUpButton.Visible := InvisibleCount > 0;
      FScrollDownButton.Visible := InvisibleCount > 0;
      FScrollUpButton.Enabled := FItemButtonStartIndex > 0;
      FScrollDownButton.Enabled := DownInvisibleCount > 0;
      UpdateButtonList;
    end
  else
    begin
      FScrollUpButton.Visible := FActive;
      FScrollDownButton.Visible := FActive;
    end;
end;

procedure TcaMenuPage.UpdatePageButton;
begin
  if Assigned(FMenu) then
    FPageButton.Properties := FMenu.ButtonProperties.DefaultPageButtons;
end;

  // Protected methods 

function TcaMenuPage.CreateButton: TcaMenuItemButton;
begin
  Result := TcaMenuItemButton.Create(Owner);
end;

procedure TcaMenuPage.CreateWnd;
begin
  inherited;
  FPageButton.Parent := Self;
  FScrollUpButton.Parent := Self;
  FScrollDownButton.Parent := Self;
end;

procedure TcaMenuPage.DoActiveChanged;
begin
  if Assigned(FOnActiveChanged) then FOnActiveChanged(Self);
end;

procedure TcaMenuPage.Loaded;
begin
  inherited;
  UpdatePageButton;
end;

procedure TcaMenuPage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    begin
      if AComponent = FFocusControl then
        FFocusControl := nil;
      if AComponent = FPageButton then
        FPageButton := nil;
    end;
end;

procedure TcaMenuPage.Paint;
var
  R: TRect;
  BottomOffset: Integer;
  X0, X1, Y, YOffset: Integer;
begin
  inherited;
  BottomOffset := GetBottomOffset;
  R := Rect(1, FPageButton.Height + 3, Width - 1, Height - BottomOffset);
  Canvas.Brush.Color := FFillColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(R);
  if FFillColor2 <> clNone then
    begin
      X0 := R.Left;
      X1 := R.Right;
      Canvas.Pen.Color := FFillColor2;
      Canvas.Pen.Style := psSolid;
      YOffset := R.Top * 2;
      for Y := R.Top to R.Bottom div 2 do
        begin
          Canvas.MoveTo(X0, Y * 2 - YOffset);
          Canvas.LineTo(X1, Y * 2 - YOffset);
        end;
    end;
end;

procedure TcaMenuPage.Resize;
begin
  PositionItemButton;
  PositionScrollButtons;
  AlignPanel;
  inherited;
  FItemButtonStartIndex := 0;
  UpdateItemButtons;
end;

  // Private methods 

function TcaMenuPage.GetBottomOffset: Integer;
begin
  if Self = FMenu.LastPage then
    Result := 0
  else
    Result := 3;
end;

procedure TcaMenuPage.AlignPanel;
var
  R: TRect;
begin
  if FPanel <> nil then
    begin
      R.Top := FPageButton.Height + 3 + FPanelGap;
      R.Left := 0 + FPanelGap + 2;
      R.Right := Self.Width - FPanelGap - 2;
      R.Bottom := Self.Height - FPanelGap * 2 + 1;
      FPanel.BoundsRect := R;
    end;
end;

procedure TcaMenuPage.PositionItemButton;
begin
  if Menu <> nil then
    FPageButton.SetBounds(0, 0, Width, Menu.ButtonProperties.CommonPageButtons.Height);
end;

procedure TcaMenuPage.PositionScrollButtons;
begin
  FScrollUpButton.SetBounds(Width - 20, FPageButton.Height + FMenu.ButtonProperties.CommonItemButtons.TopGap - 2, 16, 16);
  FScrollDownButton.SetBounds(Width - 20, Height - GetBottomOffset - 20, 16, 16);
end;

procedure TcaMenuPage.SetAllBtnsEnabled(const IsEnabled: Boolean);
var
  Index: Integer;
begin
  UpdateButtonList;
  for Index := 0 to FButtonList.Count - 1 do
    FButtonList[Index].Enabled := IsEnabled;
end;

procedure TcaMenuPage.SetButtonUnderline(const ButtonNum: Integer; const DoUnderline: Boolean);
var
  Btn: TcaMenuItemButton;
begin
  UpdateButtonList;
  if (ButtonNum >= 0) and (ButtonNum < GetButtonCount) then
    begin
      Btn := FButtonList[ButtonNum];
      if DoUnderline then
        Btn.Font.Style := Btn.Font.Style + [fsUnderline]
      else
        Btn.Font.Style := Btn.Font.Style - [fsUnderline];
    end;
end;

procedure TcaMenuPage.UpdateButtonList;
var
  Index: Integer;
  Btn: TcaMenuItemButton;
  IsShowing: Boolean;
begin
  FButtonList.Clear;
  for Index := 0 to ControlCount - 1 do
    begin
      if Controls[Index] is TcaMenuItemButton then
        begin
          Btn := TcaMenuItemButton(Controls[Index]);
          // Btn.MenuPage := Self;
          Btn.SortIndex := Btn.Top;
          IsShowing := True;
          if Assigned(FOnIsMenuButtonShowing) then
            FOnIsMenuButtonShowing(Self, Btn, IsShowing);
          if IsShowing then
            FButtonList.Add(Btn)
          else
            Btn.Visible := False;
        end;
      if Assigned(FOnBeforeSortMenuButtons) then FOnBeforeSortMenuButtons(Self, FButtonList);
      FButtonList.Sort(MenuButtonSortFunction);
      if Assigned(FOnAfterSortMenuButtons) then FOnAfterSortMenuButtons(Self, FButtonList);
    end;
end;

  // Windows message handlers 

procedure TcaMenuPage.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  FPageButton.Color := clBtnFace;
  FPageButton.Invalidate;
end;

  // Component message handlers 

procedure TcaMenuPage.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
end;

  // Event handlers 

procedure TcaMenuPage.ItemButtonEnterEvent(Sender: TObject);
var
  ItemButton: TcaMenuItemButton;
begin
  if not (csDesigning in ComponentState) then
    begin
      ItemButton := Sender as TcaMenuItemButton;
      if not ItemButton.Highlighted then
        ItemButton.Properties := FMenu.ButtonProperties.MouseOverItemButtons;
    end;
end;

procedure TcaMenuPage.ItemButtonLeaveEvent(Sender: TObject);
var
  ItemButton: TcaMenuItemButton;
begin
  if not (csDesigning in ComponentState) then
    begin
      ItemButton := Sender as TcaMenuItemButton;
      if not ItemButton.Highlighted then
        ItemButton.Properties := FMenu.ButtonProperties.DefaultItemButtons;
    end;
end;

procedure TcaMenuPage.PageButtonClickEvent(Sender: TObject);
begin
  Active := not Active;
  Click;
  if FFocusControl <> nil then FFocusControl.SetFocus;
end;

procedure TcaMenuPage.PageButtonEnterEvent(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    FPageButton.Properties := FMenu.ButtonProperties.MouseOverPageButtons;
end;

procedure TcaMenuPage.PageButtonLeaveEvent(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    FPageButton.Properties := FMenu.ButtonProperties.DefaultPageButtons;
end;

procedure TcaMenuPage.ScrollButtonClickEvent(Sender: TObject);
begin
  if Sender = FScrollUpButton then
    Dec(FItemButtonStartIndex)
  else
    Inc(FItemButtonStartIndex);
  if FItemButtonStartIndex < 0 then FItemButtonStartIndex := 0;
  if FItemButtonStartIndex >= FButtonList.Count then FItemButtonStartIndex := FButtonList.Count - 1;
  UpdateItemButtons;
end;

  // Property methods 

function TcaMenuPage.GetButtonCaption: TCaption;
begin
  Result := FPageButton.Caption;
end;

function TcaMenuPage.GetButtonCount: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to ControlCount - 1 do
    if (Controls[Index] is TcaMenuItemButton) and (Controls[Index] <> FPageButton) then
      Inc(Result);
end;

function TcaMenuPage.GetButton(Index: Integer): TcaMenuItemButton;
begin
  Result := FButtonList[Index];
end;

function TcaMenuPage.GetButtonDown(Index: Integer): Boolean;
var
  Btn: TcaMenuItemButton;
begin
  if Index >= ButtonCount then raise EListError.Create('Index out of range');
  UpdateButtonList;
  Btn := FButtonList[Index];
  Result := Btn.Down;
end;

function TcaMenuPage.GetButtonStyle: TcaButtonStyle;
begin
  Result := FPageButton.Style;
end;

function TcaMenuPage.GetCurrentButton: TcaMenuItemButton;
begin
  Result := nil;
  UpdateButtonList;
  if FButtonList.Count > 0 then
    Result := FButtonList[FCurrentButton];
end;

function TcaMenuPage.GetDownButton: TcaMenuItemButton;
var
  Index: Integer;
  AButton: TcaMenuItemButton;
begin
  Result := nil;
  UpdateButtonList;
  for Index := 0 to FButtonList.Count - 1 do
    begin
      AButton := FButtonList[Index];
      if AButton.Down then
        begin
          Result := AButton;
          Break;
        end;
    end;
end;

function TcaMenuPage.GetGlyph: TBitmap;
begin
  Result := FPageButton.Glyph;
end;

function TcaMenuPage.GetLastButton: TcaMenuItemButton;
begin
  Result := nil;
  if FButtonList.Count > 0 then
    Result := FButtonList[FButtonList.Count - 1];
end;

procedure TcaMenuPage.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
    begin
      FActive := Value;
      if (FMenu <> nil) and FMenu.UpdatePages then
        begin
          FMenu.ActiveChanged(Self);
          DoActiveChanged;
        end;
    end;
end;

procedure TcaMenuPage.SetAllowAllUp(const Value: Boolean);
var
  Index: Integer;
begin
  if Value <> FAllowAllUp then
    begin
      FAllowAllUp := Value;
      UpdateButtonList;
      for Index := 0 to FButtonList.Count - 1 do
        FButtonList[Index].AllowAllUp := FAllowAllUp;
    end;
end;

procedure TcaMenuPage.SetButtonCaption(const Value: TCaption);
begin
  FPageButton.Caption := Value;
end;

procedure TcaMenuPage.SetButtonDown(Index: Integer; const Down: Boolean);
begin
  if Index >= ButtonCount then raise EListError.Create('Index out of range');
  UpdateButtonList;
  FButtonList[Index].Down := Down;
end;

procedure TcaMenuPage.SetButtonStyle(const Value: TcaButtonStyle);
begin
  FPageButton.Style := Value;
end;

procedure TcaMenuPage.SetCurrentButton(const Value: TcaMenuItemButton);
var
  Index: Integer;
begin
  UpdateButtonList;
  if Value <> GetCurrentButton then
    begin
      SetButtonUnderline(FCurrentButton, False);
      Index := FButtonList.IndexOf(Value);
      if Index >= 0 then
        begin
          FCurrentButton := Index;
          SetButtonUnderline(FCurrentButton, True);
        end;
    end;
end;

procedure TcaMenuPage.SetFillColor(const Value: TColor);
begin
  if Value <> FFillColor then
    begin
      FFillColor := Value;
      Invalidate;
    end;
end;

procedure TcaMenuPage.SetFillColor2(const Value: TColor);
begin
  if Value <> FFillColor2 then
    begin
      FFillColor2 := Value;
      Invalidate;
    end;
end;

procedure TcaMenuPage.SetGlyph(const Value: TBitmap);
begin
  FPageButton.Glyph := Value;
end;

procedure TcaMenuPage.SetMenu(const Value: TcaCustomMenu);
begin
  FMenu := Value;
end;

procedure TcaMenuPage.SetPanel(const Value: TCustomPanel);
begin
  if Value <> FPanel then
    begin
      FPanel := Value;
      FPanel.Parent := Self;
      AlignPanel;
    end;
end;

procedure TcaMenuPage.SetPanelGap(const Value: Integer);
begin
  if Value <> FPanelGap then
    begin
      FPanelGap := Value;
      AlignPanel;
    end;
end;

procedure TcaMenuPage.SetShowPageName(const Value: Boolean);
begin
  if Value <> FShowPageName then
    begin
      FShowPageName := Value;
      if FShowPageName then
        FPageButton.Caption := Name;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaMenuPageList                                                           
  //---------------------------------------------------------------------------

constructor TcaMenuPageList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TcaMenuPageList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TcaMenuPageList.Add(AMenuPage: TcaMenuPage);
begin
  FList.Add(AMenuPage);
end;

procedure TcaMenuPageList.Clear;
begin
  FList.Clear;
end;

function TcaMenuPageList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaMenuPageList.GetMenuPage(Index: Integer): TcaMenuPage;
begin
  Result := TcaMenuPage(FList[Index]);
end;

function TcaMenuPageList.IndexOf(AMenuPage: TcaMenuPage): Integer;
begin
  Result := FList.IndexOf(AMenuPage);
end;

procedure TcaMenuPageList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

  //---------------------------------------------------------------------------
  // TcaCustomMenu                                                             
  //---------------------------------------------------------------------------

constructor TcaCustomMenu.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  CreateObjects;

//  FButtonProperties := TcaMenuButtonProperties.Create(nil);
//
//  FSizeBarCtrl := TcaSizeBar.Create(Self);
//  FSizeBarCtrl.OnStateChange := SizeBarStateChangeEvent;
//
//  FSizeBar := TcaSizeBarProperties.Create;
//  FSizeBar.SizeBar := FSizeBarCtrl;
//
//  FPageMargin := TcaMargin.Create;
//  FPageMargin.OnChanged := PageMarginChangedEvent;
//
//  FSizeBarMargin := TcaMargin.Create;
//  FSizeBarMargin.OnChanged := SizeBarMarginChangedEvent;
//
//  FPageList := TcaMenuPageList.Create;

  FBigWidth := 160;
  FEnabled := True;
end;

destructor TcaCustomMenu.Destroy;
begin
  FreeObjects;
  inherited;
end;

  // Public methods 

function TcaCustomMenu.AddMenuPage(ASortMenuPages: Boolean = True): TcaMenuPage;
var
  LastPage: TcaMenuPage;
  Factory: IcaComponentFactory;
begin
  Factory := TcaComponentFactory.Create(TcaMenuPage, Owner);
  Result := TcaMenuPage(Factory.NewComponent);
  Result.Menu := Self;
  Result.Parent := Self;
  Result.Enabled := FEnabled;  
  LastPage := GetLastPage;
  if LastPage = nil then
    Result.Top := 0
  else
    Result.Top := LastPage.Top + 1;
  if ASortMenuPages then
    begin
      UpdatePageList;
      ArrangeMenu;
    end;
  ActiveChanged(Result);
  if Assigned(FOnAddMenuPage) then FOnAddMenuPage(Self, Result);
  UpdatePageButtons;
end;

function TcaCustomMenu.FindMenuPageCaption(const AMenuPageCaption: TCaption): TcaMenuPage;
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  Result := nil;
  for Index := 0 to Pred(FPageList.Count) do
    begin
      Page := FPageList[Index];
      if Page.ButtonCaption = AMenuPageCaption then
        begin
          Result := Page;
          Break;
        end;
    end;
end;

function TcaCustomMenu.FindMenuPageName(const AMenuPageName: string): TcaMenuPage;
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  Result := nil;
  for Index := 0 to Pred(FPageList.Count) do
    begin
      Page := FPageList[Index];
      if Page.Name = AMenuPageName then
        begin
          Result := Page;
          Break;
        end;
    end;
end;

function TcaCustomMenu.PageCount: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to ControlCount - 1 do
    if Controls[Index] is TcaMenuPage then Inc(Result);
end;

procedure TcaCustomMenu.AllButtonsClearHighlight;
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  for Index := 0 to Pred(FPageList.Count) do
    begin
      Page := FPageList[Index];
      Page.AllButtonsClearHighlight;
    end;
end;

procedure TcaCustomMenu.Clear;
var
  Index: Integer;
  MenuPageList: TList;
begin
  MenuPageList := Auto(TList.Create).Instance;
  for Index := 0 to Pred(ControlCount) do
    if Controls[Index] is TcaMenuPage then
      MenuPageList.Add(Controls[Index]);
  while MenuPageList.Count > 0 do
    begin
      RemoveControl(TControl(MenuPageList[0]));
      TcaMenuPage(MenuPageList[0]).ClearButtons;
      TcaMenuPage(MenuPageList[0]).Free;
      MenuPageList.Delete(0);
    end;
  UpdatePageList;
end;

procedure TcaCustomMenu.GotoNextPage;
var
  Index: Integer;
begin
  Index := FPageList.IndexOf(GetActivePage);
  if Index = Pred(FPageList.Count) then
    Index := 0
  else
    Inc(Index);
  SetActivePage(FPageList[Index]);
end;

procedure TcaCustomMenu.GotoPriorPage;
var
  Index: Integer;
begin
  Index := FPageList.IndexOf(GetActivePage);
  if Index = 0 then
    Index := Pred(FPageList.Count)
  else
    Dec(Index);
  SetActivePage(FPageList[Index]);
end;

procedure TcaCustomMenu.Resize;
begin
  inherited;
  ArrangeMenu;
end;

procedure TcaCustomMenu.SortMenuPages;
var
  Index: Integer;
begin
  if Assigned(FOnSortMenuPagesRequested) then FOnSortMenuPagesRequested(Self);    
  UpdatePageList;
  ArrangeMenu;
  for Index := 0 to Pred(FPageList.Count) do
    FPageList[Index].UpdateItemButtons;
end;

procedure TcaCustomMenu.UpdatePageList;
var
  CompState: IcaComponentState;
  Index: Integer;
  IsShowing: Boolean;
  Page: TcaMenuPage;
begin
  CompState := TcaComponentState.Create(Self);
  if not CompState.IsDestroying then
    begin
      FPageList.Clear;
      for Index := 0 to ControlCount - 1 do
        if Controls[Index] is TcaMenuPage then
          begin
            Page := TcaMenuPage(Controls[Index]);
            Page.Menu := Self;
            Page.SortIndex := Page.Top;
            IsShowing := True;
            if Assigned(FOnIsMenuPageShowing) then
              FOnIsMenuPageShowing(Self, Page, IsShowing);
            if IsShowing then
              FPageList.Add(Page)
            else
              Page.Visible := False;
          end;
      if Assigned(FOnBeforeSortMenuPages) then FOnBeforeSortMenuPages(Self, FPageList);
      FPageList.Sort(MenuPageSortFunction);
      if Assigned(FOnAfterSortMenuPages) then FOnAfterSortMenuPages(Self, FPageList);
    end;
end;

procedure TcaCustomMenu.UpdateItemButtons;
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  for Index := 0 to Pred(FPageList.Count) do
    begin
      Page := FPageList[Index];
      Page.UpdateItemButtons;
    end;
end;

  // Protected methods 

function TcaCustomMenu.GetClientRect: TRect;
begin
  Result := Rect(0, 0, Width - FrameImpl.RightWidth, Height);
end;

procedure TcaCustomMenu.ActiveChanged(APage: TcaMenuPage);
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  if (FPageList <> nil) and (FPageList.Count > 0) then
    begin
      for Index := 0 to Pred(FPageList.Count) do
        begin
          Page := FPageList[Index];
          if Page <> APage then
            begin
              FUpdatePages := False;
              Page.Active := False;
              FUpdatePages := True;
            end;
        end;
    end;
  PositionPages;
  if APage.Active then
    DoActivateMenuPage(APage);
end;

procedure TcaCustomMenu.CreateWnd;
begin
  inherited;
  PositionSizeBar;
end;

procedure TcaCustomMenu.DoActivateMenuPage(APage: TcaMenuPage);
begin
  if Assigned(FOnActivateMenuPage) then
    FOnActivateMenuPage(Self, APage);    
end;

procedure TcaCustomMenu.Loaded;
var
  AFirstPage: TcaMenuPage;
  CompState: IcaComponentState;
begin
  inherited;
  UpdatePageList;
  FUpdatePages := True;
  CompState := TcaComponentState.Create;
  CompState.Component := Self;
  if CompState.IsRunTime then
    begin
      AFirstPage := GetFirstVisiblePage;
      if AFirstPage <> nil then
        begin
          AFirstPage.HandleNeeded;
          AFirstPage.Active := True;
        end;
    end;
  ArrangeMenu;
  UpdatePageButtons;
  UpdateItemButtons;
end;

procedure TcaCustomMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TcaMenuPage) then
    UpdatePageList;
end;

procedure TcaCustomMenu.PositionPages;
var
  ButtonHeight: Integer;
  Index: Integer;
  Page: TcaMenuPage;
  NextTop: Integer;
  PageHeight: Integer;
begin
  if FPageList.Count > 0 then
    begin
      if FPageList.Count = 1 then
        begin
          Page := GetPage(0);
          Page.BoundsRect := Rect(FPageMargin.Left,
                                  FPageMargin.Top,
                                  Width - FPageMargin.Right,
                                  Height - FPageMargin.Bottom);
        end
      else
        begin
          NextTop := FPageMargin.Top;
          for Index := 0 to Pred(FPageList.Count) do
            begin
              Page := FPageList[Index];
              if Page.Visible then
                begin
                  ButtonHeight := FButtonProperties.CommonPageButtons.Height;
                  if Page.Active then
                    PageHeight := Height - (Pred(FPageList.Count)) * ButtonHeight - FPageMargin.Top
                  else
                    PageHeight := ButtonHeight;
                  Page.BoundsRect := Rect(FPageMargin.Left,
                                          NextTop,
                                          Width - FPageMargin.Right,
                                          NextTop + PageHeight);
                  Page.Resize;
                  Inc(NextTop, Page.Height);
                end;
            end;
        end;
    end;
end;

  // Private methods 

procedure TcaCustomMenu.ArrangeMenu;
begin
  PositionPages;
  PositionSizeBar;
end;

procedure TcaCustomMenu.CreateButtonProperties;
begin
  FButtonProperties := TcaMenuButtonProperties.Create(nil);
end;

procedure TcaCustomMenu.CreateObjects;
begin
  CreateButtonProperties;
  CreateSizeBarControl;
  CreateSizeBarProperties;
  CreatePageMargin;
  CreateSizeBarMargin;
  CreatePageList;
end;

procedure TcaCustomMenu.CreatePageList;
begin
  FPageList := TcaMenuPageList.Create;
end;

procedure TcaCustomMenu.CreatePageMargin;
begin
  FPageMargin := TcaMargin.Create;
  FPageMargin.OnChanged := PageMarginChangedEvent;
end;

procedure TcaCustomMenu.CreateSizeBarControl;
begin
  FSizeBarCtrl := TcaSizeBar.Create(Self);
  FSizeBarCtrl.OnStateChange := SizeBarStateChangeEvent;
end;

procedure TcaCustomMenu.CreateSizeBarMargin;
begin
  FSizeBarMargin := TcaMargin.Create;
  FSizeBarMargin.OnChanged := SizeBarMarginChangedEvent;
end;

procedure TcaCustomMenu.CreateSizeBarProperties;
begin
  FSizeBar := TcaSizeBarProperties.Create;
  FSizeBar.SizeBar := FSizeBarCtrl;
end;

procedure TcaCustomMenu.FreeObjects;
begin
  FButtonProperties.Free;
  FSizeBar.Free;
  FPageMargin.Free;
  FSizeBarMargin.Free;
  FPageList.Free;
end;

procedure TcaCustomMenu.PositionSizeBar;
var
  AvailRect: IcaRect;
begin
  AvailRect := TcaRect.Create;
  if FSizeBarCtrl <> nil then
    begin
      if FUseSizeBar then
        begin
          FSizeBarCtrl.Parent := Self;
          AvailRect.Left := Width - FPageMargin.Right;
          AvailRect.Top := 0;
          AvailRect.Right := Width;
          AvailRect.Bottom := Height;

          AvailRect.Adjust(FSizeBarMargin.Left,
                           FSizeBarMargin.Top,
                           -FSizeBarMargin.Right,
                           -FSizeBarMargin.Bottom);
          FSizeBarCtrl.BoundsRect := AvailRect.Rect;

          FSizeBarCtrl.BigAndSmall := baLeftRight;
        end
      else
        FSizeBarCtrl.Parent := nil;
    end;
end;

procedure TcaCustomMenu.UpdateEnabledState;
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  for Index := 0 to Pred(FPageList.Count) do
    begin
      Page := FPageList[Index];
      Page.Enabled := FEnabled;
    end;
end;

procedure TcaCustomMenu.UpdatePageButtons;
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  for Index := 0 to Pred(FPageList.Count) do
    begin
      Page := FPageList[Index];
      Page.UpdatePageButton;
    end;
end;

  // Event handlers 

procedure TcaCustomMenu.PageMarginChangedEvent(Sender: TObject);
begin
  ArrangeMenu;
end;

procedure TcaCustomMenu.SizeBarMarginChangedEvent(Sender: TObject);
begin
  PositionSizeBar;
end;

procedure TcaCustomMenu.SizeBarStateChangeEvent(Sender: TObject);
begin
  SetState(TcaMenuState(FSizeBarCtrl.State));
end;

  // Property methods 

function TcaCustomMenu.GetActivePage: TcaMenuPage;
var
  Index: Integer;
  Page: TcaMenuPage;
begin
  Result := nil;
  for Index := 0 to Pred(FPageList.Count) do
    begin
      Page := FPageList[Index];
      if Page.Active then
        begin
          Result := Page;
          Break;
        end;
    end;
end;

function TcaCustomMenu.GetFirstPage: TcaMenuPage;
begin
  Result := nil;
  if FPageList.Count > 0 then
    Result := FPageList[0];
end;

function TcaCustomMenu.GetFirstVisiblePage: TcaMenuPage;
var
  Index: Integer;
begin
  Result := nil;
  if FPageList.Count > 0 then
    begin
      Index := 0;
      while (not FPageList[Index].Visible) and (Index < Pred(FPageList.Count)) do
        Inc(Index);
      if FPageList[Index].Visible then
        Result := FPageList[Index];
    end;
end;

function TcaCustomMenu.GetLastPage: TcaMenuPage;
begin
  Result := nil;
  if FPageList.Count > 0 then
    Result := GetPage(Pred(FPageList.Count));
end;

function TcaCustomMenu.GetPage(Index: Integer): TcaMenuPage;
begin
  Result := nil;
  if (FPageList <> nil) and (FPageList.Count > 0) then
    Result := FPageList[Index];
end;

function TcaCustomMenu.GetSizeBarBtnEnabled: Boolean;
begin
  Result := FSizeBarCtrl.Enabled;
end;

procedure TcaCustomMenu.SetActivePage(const Value: TcaMenuPage);
var
  OldActivePage: TcaMenuPage;
  NewActivePage: TcaMenuPage;
begin
  OldActivePage := GetActivePage;
  NewActivePage := Value;
  if NewActivePage <> OldActivePage then
    begin
      if NewActivePage <> nil then
        begin
          if (NewActivePage.Menu <> nil) and (NewActivePage.Menu <> Self) then
            raise Exception.Create('Menu page belongs to another menu');
          NewActivePage.Active := True;
        end;
    end;
end;

procedure TcaCustomMenu.SetBigWidth(const Value: Integer);
begin
  if Value <> FBigWidth then
    begin
      FBigWidth := Value;
      if FSizeBarCtrl.State = bsBig then
        Width := FBigWidth;
      Resize;
    end;
end;

procedure TcaCustomMenu.SetButtonProperties(const Value: TcaMenuButtonProperties);
begin
  FButtonProperties.Assign(Value);
end;

procedure TcaCustomMenu.SetEnabled(const Value: Boolean);
begin
  inherited Enabled := True;
  if Value <> FEnabled then
    begin
      FEnabled := Value;
      UpdateEnabledState;
    end;
end;

procedure TcaCustomMenu.SetSizeBarBtnEnabled(const Value: Boolean);
begin
  FSizeBarCtrl.Enabled := Value;
end;

procedure TcaCustomMenu.SetState(const Value: TcaMenuState);
begin
  if Value <> FState then
    begin
      FState := Value;
      if FSizeBarCtrl <> nil then FSizeBarCtrl.State := TcaSizeBarState(FState);
      if FState = meBig then
        Width := FBigWidth
      else
        Width := FSizeBar.Width;
      PositionSizeBar;
    end;
end;

procedure TcaCustomMenu.SetUseSizeBar(const Value: Boolean);
begin
  if Value <> FUseSizeBar then
    begin
      FUseSizeBar := Value;
      ArrangeMenu;
    end;
end;

end.

