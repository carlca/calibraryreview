unit caMessages;

interface

uses

  // Standard Delphi units 
  SysUtils,
  Classes,
  Messages;

type

  //---------------------------------------------------------------------------
  // IcaMessages                                                               
  //---------------------------------------------------------------------------

  IcaMessages = interface
  ['{DBAB0823-0BA3-466C-A5FF-532EDE3603C2}']
    // Interface methods 
    function MsgToString(AMsg: Word): string;
  end;

  //---------------------------------------------------------------------------
  // TcaMessages                                                               
  //---------------------------------------------------------------------------

  TcaMessages = class(TInterfacedObject, IcaMessages)
  private
    // Private fields 
    FList: TStrings;
    // Private methods 
    procedure BuildEmptyList;
    procedure BuildMessageList;
  protected
    // Interface methods 
    function MsgToString(AMsg: Word): string;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaMessages                                                               
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaMessages.Create;
begin
  inherited;
  FList := TStringList.Create;
  BuildEmptyList;
  BuildMessageList;
end;

destructor TcaMessages.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Interface methods 

function TcaMessages.MsgToString(AMsg: Word): string;
begin
  Result := '';
  if AMsg < WM_USER then
    Result := FList[AMsg];
end;

  // Private methods 

procedure TcaMessages.BuildEmptyList;
var
  Index: Integer;
begin
  for Index := 0 to Pred(WM_USER) do
    FList.Add('');
end;

procedure TcaMessages.BuildMessageList;
begin
  FList[$0000] := 'WM_NULL';
  FList[$0001] := 'WM_CREATE ';
  FList[$0002] := 'WM_DESTROY ';
  FList[$0003] := 'WM_MOVE ';
  FList[$0005] := 'WM_SIZE ';
  FList[$0006] := 'WM_ACTIVATE ';
  FList[$0007] := 'WM_SETFOCUS ';
  FList[$0008] := 'WM_KILLFOCUS ';
  FList[$000A] := 'WM_ENABLE ';
  FList[$000B] := 'WM_SETREDRAW ';
  FList[$000C] := 'WM_SETTEXT ';
  FList[$000D] := 'WM_GETTEXT ';
  FList[$000E] := 'WM_GETTEXTLENGTH ';
  FList[$000F] := 'WM_PAINT ';
  FList[$0010] := 'WM_CLOSE ';
  FList[$0011] := 'WM_QUERYENDSESSION ';
  FList[$0012] := 'WM_QUIT ';
  FList[$0013] := 'WM_QUERYOPEN ';
  FList[$0014] := 'WM_ERASEBKGND ';
  FList[$0015] := 'WM_SYSCOLORCHANGE ';
  FList[$0016] := 'WM_ENDSESSION ';
  FList[$0017] := 'WM_SYSTEMERROR ';
  FList[$0018] := 'WM_SHOWWINDOW ';
  FList[$0019] := 'WM_CTLCOLOR ';
  FList[$001A] := 'WM_SETTINGCHANGE ';
  FList[$001B] := 'WM_DEVMODECHANGE ';
  FList[$001C] := 'WM_ACTIVATEAPP ';
  FList[$001D] := 'WM_FONTCHANGE ';
  FList[$001E] := 'WM_TIMECHANGE ';
  FList[$001F] := 'WM_CANCELMODE ';
  FList[$0020] := 'WM_SETCURSOR ';
  FList[$0021] := 'WM_MOUSEACTIVATE ';
  FList[$0022] := 'WM_CHILDACTIVATE ';
  FList[$0023] := 'WM_QUEUESYNC ';
  FList[$0024] := 'WM_GETMINMAXINFO ';
  FList[$0026] := 'WM_PAINTICON ';
  FList[$0027] := 'WM_ICONERASEBKGND ';
  FList[$0028] := 'WM_NEXTDLGCTL ';
  FList[$002A] := 'WM_SPOOLERSTATUS ';
  FList[$002B] := 'WM_DRAWITEM ';
  FList[$002C] := 'WM_MEASUREITEM ';
  FList[$002D] := 'WM_DELETEITEM ';
  FList[$002E] := 'WM_VKEYTOITEM ';
  FList[$002F] := 'WM_CHARTOITEM ';
  FList[$0030] := 'WM_SETFONT ';
  FList[$0031] := 'WM_GETFONT ';
  FList[$0032] := 'WM_SETHOTKEY ';
  FList[$0033] := 'WM_GETHOTKEY ';
  FList[$0037] := 'WM_QUERYDRAGICON ';
  FList[$0039] := 'WM_COMPAREITEM ';
  FList[$003D] := 'WM_GETOBJECT ';
  FList[$0041] := 'WM_COMPACTING ';
  FList[$0044] := 'WM_COMMNOTIFY ';
  FList[$0046] := 'WM_WINDOWPOSCHANGING ';
  FList[$0047] := 'WM_WINDOWPOSCHANGED ';
  FList[$0048] := 'WM_POWER ';
  FList[$004A] := 'WM_COPYDATA ';
  FList[$004B] := 'WM_CANCELJOURNAL ';
  FList[$004E] := 'WM_NOTIFY ';
  FList[$0050] := 'WM_INPUTLANGCHANGEREQUEST ';
  FList[$0051] := 'WM_INPUTLANGCHANGE ';
  FList[$0052] := 'WM_TCARD ';
  FList[$0053] := 'WM_HELP ';
  FList[$0054] := 'WM_USERCHANGED ';
  FList[$0055] := 'WM_NOTIFYFORMAT ';
  FList[$007B] := 'WM_CONTEXTMENU ';
  FList[$007C] := 'WM_STYLECHANGING ';
  FList[$007D] := 'WM_STYLECHANGED ';
  FList[$007E] := 'WM_DISPLAYCHANGE ';
  FList[$007F] := 'WM_GETICON ';
  FList[$0080] := 'WM_SETICON ';
  FList[$0081] := 'WM_NCCREATE ';
  FList[$0082] := 'WM_NCDESTROY ';
  FList[$0083] := 'WM_NCCALCSIZE ';
  FList[$0084] := 'WM_NCHITTEST ';
  FList[$0085] := 'WM_NCPAINT ';
  FList[$0086] := 'WM_NCACTIVATE ';
  FList[$0087] := 'WM_GETDLGCODE ';
  FList[$00A0] := 'WM_NCMOUSEMOVE ';
  FList[$00A1] := 'WM_NCLBUTTONDOWN ';
  FList[$00A2] := 'WM_NCLBUTTONUP ';
  FList[$00A3] := 'WM_NCLBUTTONDBLCLK ';
  FList[$00A4] := 'WM_NCRBUTTONDOWN ';
  FList[$00A5] := 'WM_NCRBUTTONUP ';
  FList[$00A6] := 'WM_NCRBUTTONDBLCLK ';
  FList[$00A7] := 'WM_NCMBUTTONDOWN ';
  FList[$00A8] := 'WM_NCMBUTTONUP ';
  FList[$00A9] := 'WM_NCMBUTTONDBLCLK ';
  FList[$00AB] := 'WM_NCXBUTTONDOWN ';
  FList[$00AC] := 'WM_NCXBUTTONUP ';
  FList[$00AD] := 'WM_NCXBUTTONDBLCLK ';
  FList[$00FF] := 'WM_INPUT ';
  FList[$0100] := 'WM_KEYDOWN ';
  FList[$0100] := 'WM_KEYFIRST ';
  FList[$0101] := 'WM_KEYUP ';
  FList[$0102] := 'WM_CHAR ';
  FList[$0103] := 'WM_DEADCHAR ';
  FList[$0104] := 'WM_SYSKEYDOWN ';
  FList[$0105] := 'WM_SYSKEYUP ';
  FList[$0106] := 'WM_SYSCHAR ';
  FList[$0107] := 'WM_SYSDEADCHAR ';
  FList[$0108] := 'WM_KEYLAST ';
  FList[$010D] := 'WM_IME_STARTCOMPOSITION ';
  FList[$010E] := 'WM_IME_ENDCOMPOSITION ';
  FList[$010F] := 'WM_IME_COMPOSITION ';
  FList[$010F] := 'WM_IME_KEYLAST ';
  FList[$0110] := 'WM_INITDIALOG ';
  FList[$0111] := 'WM_COMMAND ';
  FList[$0112] := 'WM_SYSCOMMAND ';
  FList[$0113] := 'WM_TIMER ';
  FList[$0114] := 'WM_HSCROLL ';
  FList[$0115] := 'WM_VSCROLL ';
  FList[$0116] := 'WM_INITMENU ';
  FList[$0117] := 'WM_INITMENUPOPUP ';
  FList[$011F] := 'WM_MENUSELECT ';
  FList[$0120] := 'WM_MENUCHAR ';
  FList[$0121] := 'WM_ENTERIDLE ';
  FList[$0122] := 'WM_MENURBUTTONUP ';
  FList[$0123] := 'WM_MENUDRAG ';
  FList[$0124] := 'WM_MENUGETOBJECT ';
  FList[$0125] := 'WM_UNINITMENUPOPUP ';
  FList[$0126] := 'WM_MENUCOMMAND ';
  FList[$0127] := 'WM_CHANGEUISTATE ';
  FList[$0128] := 'WM_UPDATEUISTATE ';
  FList[$0129] := 'WM_QUERYUISTATE ';
  FList[$0132] := 'WM_CTLCOLORMSGBOX ';
  FList[$0133] := 'WM_CTLCOLOREDIT ';
  FList[$0134] := 'WM_CTLCOLORLISTBOX ';
  FList[$0135] := 'WM_CTLCOLORBTN ';
  FList[$0136] := 'WM_CTLCOLORDLG ';
  FList[$0137] := 'WM_CTLCOLORSCROLLBAR ';
  FList[$0138] := 'WM_CTLCOLORSTATIC ';
  FList[$0200] := 'WM_MOUSEFIRST ';
  FList[$0200] := 'WM_MOUSEMOVE ';
  FList[$0201] := 'WM_LBUTTONDOWN ';
  FList[$0202] := 'WM_LBUTTONUP ';
  FList[$0203] := 'WM_LBUTTONDBLCLK ';
  FList[$0204] := 'WM_RBUTTONDOWN ';
  FList[$0205] := 'WM_RBUTTONUP ';
  FList[$0206] := 'WM_RBUTTONDBLCLK ';
  FList[$0207] := 'WM_MBUTTONDOWN ';
  FList[$0208] := 'WM_MBUTTONUP ';
  FList[$0209] := 'WM_MBUTTONDBLCLK ';
  FList[$020A] := 'WM_MOUSELAST ';
  FList[$020A] := 'WM_MOUSEWHEEL ';
  FList[$0210] := 'WM_PARENTNOTIFY ';
  FList[$0211] := 'WM_ENTERMENULOOP ';
  FList[$0212] := 'WM_EXITMENULOOP ';
  FList[$0213] := 'WM_NEXTMENU ';
  FList[$0214] := 'WM_SIZING ';
  FList[$0215] := 'WM_CAPTURECHANGED ';
  FList[$0216] := 'WM_MOVING ';
  FList[$0218] := 'WM_POWERBROADCAST ';
  FList[$0219] := 'WM_DEVICECHANGE ';
  FList[$0220] := 'WM_MDICREATE ';
  FList[$0221] := 'WM_MDIDESTROY ';
  FList[$0222] := 'WM_MDIACTIVATE ';
  FList[$0223] := 'WM_MDIRESTORE ';
  FList[$0224] := 'WM_MDINEXT ';
  FList[$0225] := 'WM_MDIMAXIMIZE ';
  FList[$0226] := 'WM_MDITILE ';
  FList[$0227] := 'WM_MDICASCADE ';
  FList[$0228] := 'WM_MDIICONARRANGE ';
  FList[$0229] := 'WM_MDIGETACTIVE ';
  FList[$0230] := 'WM_MDISETMENU ';
  FList[$0231] := 'WM_ENTERSIZEMOVE ';
  FList[$0232] := 'WM_EXITSIZEMOVE ';
  FList[$0233] := 'WM_DROPFILES ';
  FList[$0234] := 'WM_MDIREFRESHMENU ';
  FList[$0281] := 'WM_IME_SETCONTEXT ';
  FList[$0282] := 'WM_IME_NOTIFY ';
  FList[$0283] := 'WM_IME_CONTROL ';
  FList[$0284] := 'WM_IME_COMPOSITIONFULL ';
  FList[$0285] := 'WM_IME_SELECT ';
  FList[$0286] := 'WM_IME_CHAR ';
  FList[$0288] := 'WM_IME_REQUEST ';
  FList[$0290] := 'WM_IME_KEYDOWN ';
  FList[$0291] := 'WM_IME_KEYUP ';
  FList[$02A0] := 'WM_NCMOUSEHOVER ';
  FList[$02A1] := 'WM_MOUSEHOVER ';
  FList[$02A2] := 'WM_NCMOUSELEAVE ';
  FList[$02A3] := 'WM_MOUSELEAVE ';
  FList[$02B1] := 'WM_WTSSESSION_CHANGE ';
  FList[$02C0] := 'WM_TABLET_FIRST ';
  FList[$02DF] := 'WM_TABLET_LAST ';
  FList[$0300] := 'WM_CUT ';
  FList[$0301] := 'WM_COPY ';
  FList[$0302] := 'WM_PASTE ';
  FList[$0303] := 'WM_CLEAR ';
  FList[$0304] := 'WM_UNDO ';
  FList[$0305] := 'WM_RENDERFORMAT ';
  FList[$0306] := 'WM_RENDERALLFORMATS ';
  FList[$0307] := 'WM_DESTROYCLIPBOARD ';
  FList[$0308] := 'WM_DRAWCLIPBOARD ';
  FList[$0309] := 'WM_PAINTCLIPBOARD ';
  FList[$030A] := 'WM_VSCROLLCLIPBOARD ';
  FList[$030B] := 'WM_SIZECLIPBOARD ';
  FList[$030C] := 'WM_ASKCBFORMATNAME ';
  FList[$030D] := 'WM_CHANGECBCHAIN ';
  FList[$030E] := 'WM_HSCROLLCLIPBOARD ';
  FList[$030F] := 'WM_QUERYNEWPALETTE ';
  FList[$0310] := 'WM_PALETTEISCHANGING ';
  FList[$0311] := 'WM_PALETTECHANGED ';
  FList[$0312] := 'WM_HOTKEY ';
  FList[$0317] := 'WM_PRINT ';
  FList[$0318] := 'WM_PRINTCLIENT ';
  FList[$0319] := 'WM_APPCOMMAND ';
  FList[$031A] := 'WM_THEMECHANGED ';
  FList[$0358] := 'WM_HANDHELDFIRST ';
  FList[$035F] := 'WM_HANDHELDLAST ';
  FList[$0380] := 'WM_PENWINFIRST ';
  FList[$038F] := 'WM_PENWINLAST ';
  FList[$0390] := 'WM_COALESCE_FIRST ';
  FList[$039F] := 'WM_COALESCE_LAST ';
  FList[$03E0] := 'WM_DDE_INITIATE ';
  FList[$03E1] := 'WM_DDE_TERMINATE ';
  FList[$03E2] := 'WM_DDE_ADVISE ';
  FList[$03E3] := 'WM_DDE_UNADVISE ';
  FList[$03E4] := 'WM_DDE_ACK ';
  FList[$03E5] := 'WM_DDE_DATA ';
  FList[$03E6] := 'WM_DDE_REQUEST ';
  FList[$03E7] := 'WM_DDE_POKE ';
  FList[$03E8] := 'WM_DDE_EXECUTE ';
  FList[$03E9] := 'WM_DDE_LAST ';
end;

end.
