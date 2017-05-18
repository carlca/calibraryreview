unit caTrayIcon;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Windows,
  Messages,
  Sysutils,
  SyncObjs,
  Controls,
  Forms,
  ShellAPI,
  Graphics,
  Menus,

  // ca utils 
  caTypes,
  caUtils,
  caFormHook;

const
  WM_TRAYICON = WM_USER + 1;

type

  //----------------------------------------------------------------------------
  // TcaTrayIcon                                                                
  //----------------------------------------------------------------------------

  TcaTrayIcon = class(TcaFormHook)
  private
    // Private fields 
    FConfirmShutdown: Boolean;
    FDefaultMenuItemsAdded: Boolean;
    FIcon: TIcon;    
    FIconSection: TCriticalSection;
    FIconVisible: Boolean;
    FOKToClose: Boolean;
    // Property fields 
    FIconHint: string;
    FImageIndex: Integer;
    FImages: TImageList;
    FPopupMenu: TPopupMenu;
    FShowMenuItemVisible: Boolean;
    FShutdownMenuItemVisible: Boolean;
    // Event property fields 
    FOnShowApplication: TNotifyEvent;
    FOnShowPopupMenu: TNotifyEvent;
    // Property methods 
    procedure SetIconHint(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    // Private methods 
    function GetIconHandle: HICON;
    function OwnerForm: TForm;
    function OwnerFormHandle: HWND;
    procedure AddDefaultSeparator;
    procedure AddDefaultShowMenuItem;
    procedure AddDefaultShutdownMenuItem;
    procedure CheckDefaultMenuItems;
    procedure CleanupSystemTray;
    procedure CreateCriticalSection;
    procedure FreeCriticalSection;
    procedure FormClosing;
    procedure FormHiding;
    procedure FormShowing;
    procedure Minimize;
    procedure MouseDoubleClick;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState);
    procedure RemoveTrayIcon;
    procedure ShowApplication;
    procedure ShowPopupMenu;
    procedure UpdateTrayIcon;
    // Event handlers 
    procedure MenuItemShowEvent(Sender: TObject);
    procedure MenuItemShutdownEvent(Sender: TObject);
  protected
    // Protected methods 
    procedure DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
    procedure DoReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
    procedure DoShowPopupMenu; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure SendToSystemTray;
    procedure Shutdown;
    // Public properties 
    property OKToClose: Boolean read FOKToClose write FOKToClose;
    property ShowMenuItemVisible: Boolean read FShowMenuItemVisible write FShowMenuItemVisible;
    property ShutdownMenuItemVisible: Boolean read FShutdownMenuItemVisible write FShutdownMenuItemVisible;
  published
    // Published properties 
    property ConfirmShutdown: Boolean read FConfirmShutdown write FConfirmShutdown;
    property IconHint: string read FIconHint write SetIconHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Images: TImageList read FImages write SetImages;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    // Event properties 
    property OnShowApplication: TNotifyEvent read FOnShowApplication write FOnShowApplication;
    property OnShowPopupMenu: TNotifyEvent read FOnShowPopupMenu write FOnShowPopupMenu;
  end;

implementation

  {$R caTrayIcon.res}

  //----------------------------------------------------------------------------
  // TcaTrayIcon                                                                
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaTrayIcon.Create(AOwner: TComponent);
begin
  inherited;
  FShowMenuItemVisible := True;
  FShutdownMenuItemVisible := True;
  Application.ShowMainForm := False;
  CreateCriticalSection;
  FIcon := TIcon.Create;
end;

destructor TcaTrayIcon.Destroy;
begin
  FreeCriticalSection;
  FIcon.Free;
  inherited;
end;

  // Public methods 

procedure TcaTrayIcon.SendToSystemTray;
begin
  ShowWindow(OwnerFormHandle, SW_HIDE);
  OwnerForm.Visible := False;
end;

procedure TcaTrayIcon.Shutdown;
var
  Response: TcaMsgDialogResponse;
begin
  FOKToClose := True;
  if FConfirmShutdown then
    begin
      Response := Utils.QueryCloseApp(OwnerFormHandle, 'shutdown');
      case Response of
        mgYes:  FOKToClose := True;
        mgNo:   FOKToClose := False;
      else
        FOKToClose := False;
      end;
    end;
  if FOKToClose then
    SendMessage(OwnerFormHandle, WM_CLOSE, 0, 0);
end;

  // Protected methods 

procedure TcaTrayIcon.DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  inherited;
  case Msg.Msg of
    WM_SHOWWINDOW:
      begin
        if Boolean(Msg.WParam) then
          FormShowing
        else
          FormHiding;
      end;
    WM_CLOSE:
      begin
        if FOKToClose then
          begin
            Unhook;            
            FormClosing;
            RemoveTrayIcon;
            CleanupSystemTray;
            Application.ProcessMessages;
          end
        else
          begin
            SendToSystemTray;
            UpdateTrayIcon;
            Handled := True;
          end;
      end;
    WM_SYSCOMMAND:
      if (Msg.wParam = SC_MINIMIZE) then
        begin
          Minimize;
          Handled := True;
        end;
  end;
end;

procedure TcaTrayIcon.DoReceiveMessage(Msg: TMessage; var Handled: Boolean);
var
  Shift: TShiftState;
  Down: Boolean;
  Button: TMouseButton;

  function IsMouseMessage(AMsg: Word): Boolean;
  begin
    Result := (AMsg = WM_LBUTTONDOWN) or (AMsg = WM_MBUTTONDOWN) or (AMsg = WM_RBUTTONDOWN) or
              (AMsg = WM_LBUTTONUP) or (AMsg = WM_MBUTTONUP) or (AMsg = WM_RBUTTONUP) or (AMsg = WM_LBUTTONDBLCLK);
  end;

begin
  inherited;
  Handled := False;
  Button := mbLeft;
  Down := False;
  if (Msg.Msg = WM_TRAYICON) then
    begin
      if IsMouseMessage(Msg.lParam) then
        begin
          Handled := True;
          case Msg.lParam of
            WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK:
              Button := mbLeft;
            WM_MBUTTONDOWN, WM_MBUTTONUP:
              Button := mbMiddle;
            WM_RBUTTONDOWN, WM_RBUTTONUP:
              Button := mbRight;
          end;
          case Msg.lParam of
            WM_LBUTTONDOWN, WM_MBUTTONDOWN, WM_RBUTTONDOWN:
              Down := True;
            WM_LBUTTONUP, WM_MBUTTONUP, WM_RBUTTONUP, WM_LBUTTONDBLCLK:
              Down := False;
          end;
          if Msg.lParam = WM_LBUTTONDBLCLK then
            begin
              if FShowMenuItemVisible then
                MouseDoubleClick
              else
                Handled := False;
            end;
          Shift := KeysToShiftState(Msg.WParam);
          if Down then
            MouseDown(Button, Shift)
          else
            MouseUp(Button, Shift);
        end;
    end;
end;

procedure TcaTrayIcon.DoShowPopupMenu;
begin
  if Assigned(FOnShowPopupMenu) then
    FOnShowPopupMenu(Self);    
end;

procedure TcaTrayIcon.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPopupMenu) then
    FPopupMenu := nil;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

  // Private methods 

function TcaTrayIcon.OwnerForm: TForm;
begin
  Result := Owner as TForm;
end;

function TcaTrayIcon.GetIconHandle: HICON;
begin
  Result := 0;
  if not (csDestroying in ComponentState) then
    begin
      if Assigned(FImages) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
        begin
          FImages.GetIcon(FImageIndex, FIcon);
          // Result := FIcon.ReleaseHandle;
          Result := FIcon.Handle;
        end;
    end;
end;

function TcaTrayIcon.OwnerFormHandle: HWND;
begin
  Result := (Owner as TForm).Handle;
end;

procedure TcaTrayIcon.AddDefaultSeparator;
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Insert(0, MenuItem);
end;

procedure TcaTrayIcon.AddDefaultShowMenuItem;
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Show...';
  MenuItem.Default := True;
  MenuItem.OnClick := MenuItemShowEvent;
  MenuItem.Visible := FShowMenuItemVisible;
  FPopupMenu.Items.Insert(0, MenuItem);
end;

procedure TcaTrayIcon.AddDefaultShutdownMenuItem;
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Shutdown';
  MenuItem.OnClick := MenuItemShutdownEvent;
  MenuItem.Visible := FShutdownMenuItemVisible;
  FPopupMenu.Items.Insert(0, MenuItem);
end;

procedure TcaTrayIcon.CheckDefaultMenuItems;
begin
  if Assigned(FPopupMenu) and (not FDefaultMenuItemsAdded) then
    begin
      AddDefaultSeparator;
      AddDefaultShutdownMenuItem;
      AddDefaultShowMenuItem;
      FDefaultMenuItemsAdded := True;
    end;
end;

function SystemTrayEnumFunc(AHandle: HWND; lParam: Integer): Boolean;
var
  R: TRect;
  X: Integer;
  Y: Integer;
  Buf: array[0..7] of Char;
begin
  if GetClientRect(AHandle, R) then
    begin
      GetClassName(AHandle, Buf, SizeOf(Buf));
      if Buf[0] = 'T' then
        begin
          X := 0;
          while X < R.Right do
            begin
              Y := 2;
              while Y < R.Bottom do
                begin
                  PostMessage(AHandle, WM_MOUSEMOVE, 0, MakeLParam(X, Y));
                  Inc(Y, 8);
                end;
              Inc(X, 8);
            end;
        end;
    end;
  Result := True;
end;

procedure TcaTrayIcon.CleanupSystemTray;
var
  TaskbarHandle: HWND;
begin
  TaskbarHandle := FindWindow('Shell_TrayWnd', nil);
  if TaskbarHandle <> 0 then
    EnumChildWindows(TaskbarHandle, @SystemTrayEnumFunc, 0);
end;

procedure TcaTrayIcon.CreateCriticalSection;
begin
  FIconSection := TCriticalSection.Create;
end;

procedure TcaTrayIcon.FreeCriticalSection;
begin
  FIconSection.Free;
end;

procedure TcaTrayIcon.FormClosing;
begin
end;

procedure TcaTrayIcon.FormHiding;
begin
  UpdateTrayIcon;
end;

procedure TcaTrayIcon.FormShowing;
begin
end;

procedure TcaTrayIcon.Minimize;
begin
end;

procedure TcaTrayIcon.MouseDoubleClick;
begin
  ShowApplication;
end;

procedure TcaTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState);
begin
  if Button = mbRight then
    ShowPopupMenu
  else
    begin
    end;
end;

procedure TcaTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState);
begin
end;

procedure TcaTrayIcon.RemoveTrayIcon;
var
  IconData: TNotifyIconData;
begin
  FIconSection.Enter;
  try
    IconData.cbSize := SizeOf(IconData);
    IconData.Wnd := Handle;
    IconData.uID := Tag;
    IconData.uFlags := (NIF_ICON or NIF_TIP or NIF_MESSAGE);
    IconData.uCallbackMessage := WM_TRAYICON;
    if Shell_NotifyIcon(NIM_DELETE, @IconData) then
      FIconVisible := False;
  finally
    FIconSection.Leave;
  end;
end;

procedure TcaTrayIcon.ShowApplication;
var
  FormHandle: HWND;
begin
  FormHandle := OwnerFormHandle;
  if FormHandle <> 0 then
    begin
      ShowWindow(FormHandle, SW_SHOW);
      OwnerForm.Visible := True;
      if Assigned(FOnShowApplication) then
        FOnShowApplication(Self);
    end;
end;

procedure TcaTrayIcon.ShowPopupMenu;
var
  MousePos: TPoint;
begin
  if Assigned(FPopupMenu) then
    begin
      DoShowPopupMenu;
      CheckDefaultMenuItems;
      GetCursorPos(MousePos);
      SetForegroundWindow(OwnerFormHandle);
      PopupMenu.Popup(MousePos.X, MousePos.Y);
    end;
end;

procedure TcaTrayIcon.UpdateTrayIcon;
var
  IconData: TNotifyIconData;
begin
  if not (csDesigning in ComponentState) and (not FOKToClose) then
    begin
      FIconSection.Enter;
      try
        IconData.cbSize := SizeOf(IconData);
        IconData.Wnd := Handle;
        IconData.uID := Tag;
        IconData.uFlags := (NIF_ICON or NIF_TIP or NIF_MESSAGE);
        IconData.uCallbackMessage := WM_TRAYICON;
        StrPCopy(IconData.szTip, FIconHint);
        IconData.hIcon := GetIconHandle;
        if FIconVisible then
          Shell_NotifyIcon(NIM_MODIFY, @IconData)
        else
          begin
            if Shell_NotifyIcon(NIM_ADD, @IconData) then
              FIconVisible := True;
          end;
      finally
        FIconSection.Leave;
      end;
    end;
end;

  // Event handlers 

procedure TcaTrayIcon.MenuItemShowEvent(Sender: TObject);
begin
  ShowApplication;
end;

procedure TcaTrayIcon.MenuItemShutdownEvent(Sender: TObject);
begin
  ShowApplication;
  Shutdown;
end;

  // Property methods 

procedure TcaTrayIcon.SetIconHint(const Value: string);
begin
  if Value <> FIconHint then
    begin
      FIconHint := Value;
      UpdateTrayIcon;
    end;
end;

procedure TcaTrayIcon.SetImageIndex(const Value: Integer);
begin
  if Value <> FImageIndex then
    begin
      FImageIndex := Value;
      UpdateTrayIcon;
    end;
end;

procedure TcaTrayIcon.SetImages(const Value: TImageList);
begin
  if Value <> FImages then
    begin
      FImages := Value;
      UpdateTrayIcon;
    end;
end;

end.
