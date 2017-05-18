unit caTransparentWnd;

{$INCLUDE ca.inc}

interface

uses
  Windows, Messages, Classes, Controls, Forms;

type
  TcaTransparentWnd = class(TComponent)
  private
    { Private declarations }
  protected
    _percent : shortint;
    { Protected declarations }
  public
    procedure SetTransparent(percent : shortint); overload;
    procedure SetTransparentHWND(hwnd: longint; percent : shortint);
    procedure SetTransparent; overload;
    procedure SetOpaqueHWND(hwnd : longint);
    procedure SetOpaque;
    { Public declarations }
  published
    property Percent: shortint read _percent write _percent default 0;
    { Published declarations }
  end;

implementation

const LWA_ALPHA = $2;
const GWL_EXSTYLE = (-20);
const WS_EX_LAYERED = $80000;
const WS_EX_TRANSPARENT = $20;

procedure TcaTransparentWnd.SetOpaqueHWND(hwnd: longint);
var
  old: longint;
begin
  old := GetWindowLongA(hwnd,GWL_EXSTYLE);
  SetWindowLongA(hwnd, GWL_EXSTYLE, old and ((not 0)-WS_EX_LAYERED) );
end;

procedure TcaTransparentWnd.SetOpaque;
begin
  Self.SetOpaqueHWND((Self.Owner as TForm).Handle);
end;

procedure TcaTransparentWnd.SetTransparent;
begin
  Self.SetTransparentHWND((Self.Owner as TForm).Handle,100-Self._percent);
end;

procedure TcaTransparentWnd.SetTransparentHWND(hwnd: longint; percent : shortint);
var
  SetLayeredWindowAttributes: function (hwnd: LongInt; crKey: byte; bAlpha: byte; dwFlags: LongInt): LongInt; stdcall;

  old: longint;
  User32: Cardinal;
begin
  User32 := LoadLibrary('USER32');
  if User32 <> 0 then
  try
    SetLayeredWindowAttributes := GetProcAddress(User32, 'SetLayeredWindowAttributes');
    if @SetLayeredWindowAttributes <> nil then
    begin
      old := GetWindowLongA(hwnd,GWL_EXSTYLE);
      SetWindowLongA(hwnd,GWL_EXSTYLE,old or WS_EX_LAYERED);
      SetLayeredWindowAttributes(hwnd, 0, (255 * percent) DIV 100, LWA_ALPHA);
    end;
  finally
    FreeLibrary(User32);
  end;
end;

procedure TcaTransparentWnd.SetTransparent(percent: shortint);
begin
  Self.SetTransparentHWND((Self.Owner as TForm).Handle,100 - percent);
end;

end.
