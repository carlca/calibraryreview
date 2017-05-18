unit caTimer;

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
  ExtCtrls,
  MMSystem,

  caTypes;

type
  TcaTimer = class;

  //----------------------------------------------------------------------------
  // TcaTimerThread                                                             
  //----------------------------------------------------------------------------

  TcaTimerThread = class(TThread)
  private
    FTimer: TcaTimer;
  protected
    procedure DoExecute;
  public
    constructor CreateTimerThread(Timer: TcaTimer);
    procedure Execute; override;
  end;

  //----------------------------------------------------------------------------
  // IcaTimer                                                                   
  //----------------------------------------------------------------------------

  IcaTimer = interface
  ['{0BB08B16-3CDF-4211-BC6E-D08B563F9F09}']
    function GetEnabled: Boolean;
    function GetInterval: LongWord;
    function GetOnTimer: TNotifyEvent;
    function GetPriority: TThreadPriority;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: LongWord);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetPriority(const Value: TThreadPriority);
    procedure Start;
    procedure Stop;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: LongWord read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property Priority: TThreadPriority read GetPriority write SetPriority;
  end;

  //----------------------------------------------------------------------------
  // TcaTimer                                                                   
  //----------------------------------------------------------------------------

  TcaTimer = class(TComponent, IcaTimer)
  private
    FInterval: LongWord;
    FPriority: TThreadPriority;
    FOnTimer: TNotifyEvent;
    FContinue: Boolean;
    FRunning: Boolean;
    FEnabled: Boolean;
    function GetEnabled: Boolean;
    function GetInterval: LongWord;
    function GetOnTimer: TNotifyEvent;
    function GetPriority: TThreadPriority;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: LongWord);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetPriority(const Value: TThreadPriority);
  protected
    property Continue: Boolean read FContinue write FContinue;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: LongWord read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property Priority: TThreadPriority read GetPriority write SetPriority;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaTimerThread                                                             
  //----------------------------------------------------------------------------

constructor TcaTimerThread.CreateTimerThread(Timer: TcaTimer);
begin
  inherited Create(True);
  FTimer := Timer;
  FreeOnTerminate := True;
end;

procedure TcaTimerThread.Execute;
var
  SleepTime, Last: LongWord;
begin
  while FTimer.Continue do
    begin
      Last := timeGetTime;
      Synchronize(DoExecute);
      SleepTime := FTimer.Interval - (timeGetTime - Last);
      if SleepTime < 10 then SleepTime := 10;
      Sleep(SleepTime);
    end;
end;

procedure TcaTimerThread.DoExecute;
begin
  if Assigned(FTimer.OnTimer) then FTimer.OnTimer(FTimer);
end;

  //----------------------------------------------------------------------------
  // TcaTimer                                                                   
  //----------------------------------------------------------------------------

constructor TcaTimer.Create(Owner: TComponent);
begin
  inherited;
  FPriority := tpNormal;
end;

destructor TcaTimer.Destroy;
begin
  Stop;
  inherited;
end;

function TcaTimer.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TcaTimer.GetInterval: LongWord;
begin
  Result := FInterval;
end;

function TcaTimer.GetOnTimer: TNotifyEvent;
begin
  Result := FOnTimer;
end;

function TcaTimer.GetPriority: TThreadPriority;
begin
  Result := FPriority;
end;

procedure TcaTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
    begin
      FEnabled := Value;
      if FEnabled then
        Start
      else
        Stop;
    end;
end;

procedure TcaTimer.SetInterval(const Value: LongWord);
begin
  FInterval := Value;
end;

procedure TcaTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  FOnTimer := Value;
end;

procedure TcaTimer.SetPriority(const Value: TThreadPriority);
begin
  FPriority := Value;
end;

procedure TcaTimer.Start;
var
  TimerThread: TcaTimerThread;
begin
  if not FRunning then
    begin
      FContinue := True;
      if not (csDesigning in ComponentState) then
        begin
          TimerThread := TcaTimerThread.CreateTimerThread(Self);
          TimerThread.Priority := FPriority;
          TimerThread.Resume;
        end;
      FRunning := True;
    end;
end;

procedure TcaTimer.Stop;
begin
  FContinue := False;
  FRunning := False;
end;

end.
