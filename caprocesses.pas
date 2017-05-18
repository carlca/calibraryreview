unit caProcesses;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  Tlhelp32,
  PSAPI,
  Forms,

  // ca units 
  caClasses,
  caConsts,
  caTypes,
  caLog,
  caUtils;

type

  //---------------------------------------------------------------------------
  // TcaProcess                                                                
  //---------------------------------------------------------------------------

  TcaProcessList = class;

  TcaProcess = class(TObject)
  private
    // Property fields 
    FMemoryUsage: DWORD;
    FModuleFileName: string;
    FProcessID: Cardinal;
  public
    // Properties
    property MemoryUsage: DWORD read FMemoryUsage write FMemoryUsage;
    property ModuleFileName: string read FModuleFileName write FModuleFileName;
    property ProcessID: Cardinal read FProcessID write FProcessID;
  end;

  //---------------------------------------------------------------------------
  // IcaProcessList                                                            
  //---------------------------------------------------------------------------

  IcaProcessList = interface
  ['{83065397-15E1-42F9-A7DE-6DEF99A7C880}']
    // Property methods 
    function GetProcess(Index: Integer): TcaProcess;
    function GetProcessCount: Integer;
    // Interface methods 
    function FindProcessByName(const AName: String; AAllowSubString: Boolean = False): TcaProcess;
    function FindProcessID(AProcessID: Cardinal): TcaProcess;
    function StopProcessByName(const AName: String; AAllowSubString: Boolean = False): Boolean;
    procedure GetProcessNamesAsStrings(AList: TStrings; AShowMemory: Boolean);
    procedure SortByProcessName;
    procedure SortByAscendingMemory;
    procedure SortByDescendingMemory;
    procedure Update;
    // Interface properties 
    property ProcessCount: Integer read GetProcessCount;
    property Processes[Index: Integer]: TcaProcess read GetProcess; default;
  end;

  //---------------------------------------------------------------------------
  // TcaProcessList                                                            
  //---------------------------------------------------------------------------

  TcaProcessList = class(TInterfacedObject, IcaProcessList)
  private
    // Private fields 
    FList: TObjectList;
    // Property methods - IcaProcessList 
    function GetProcess(Index: Integer): TcaProcess;
    function GetProcessCount: Integer;
    // Private methods 
    procedure BuildList;
    function StopProcessByID(AProcessID: Cardinal): Boolean;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Interface methods - IcaProcessList 
    function FindProcessByName(const AName: String; AAllowSubString: Boolean = False): TcaProcess;
    function FindProcessID(AProcessID: Cardinal): TcaProcess;
    function StopProcessByName(const AName: String; AAllowSubString: Boolean = False): Boolean;
    procedure GetProcessNamesAsStrings(AList: TStrings; AShowMemory: Boolean);
    procedure SortByProcessName;
    procedure SortByAscendingMemory;
    procedure SortByDescendingMemory;
    procedure Update;
    // Interface properties - IcaProcessList 
    property ProcessCount: Integer read GetProcessCount;
    property Processes[Index: Integer]: TcaProcess read GetProcess; default;
  end;

  //---------------------------------------------------------------------------
  // TcaWindow                                                                 
  //---------------------------------------------------------------------------

  TcaWindow = class(TObject)
  private
    FHandle: HWND;
    // property methods 
    function GetCaption: string;
    function GetHeight: Integer;
    function GetIconic: Boolean;
    function GetLeft: Integer;
    function GetParent: HWND;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function GetWindowClass: string;
    // private methods 
    function Rect: TRect;
  public
    // properties 
    property Caption: string read GetCaption;
    property Handle: HWND read FHandle write FHandle;
    property Height: Integer read GetHeight;
    property Iconic: Boolean read GetIconic;
    property Left: Integer read GetLeft;
    property Parent: HWND read GetParent;
    property Top: Integer read GetTop;
    property Visible: Boolean read GetVisible;
    property Width: Integer read GetWidth;
    property WindowClass: string read GetWindowClass;
  end;

  //---------------------------------------------------------------------------
  // IcaWindows                                                                
  //---------------------------------------------------------------------------

  IcaWindows = interface
  ['{BC8869C1-B80B-42D4-8E70-7B945E2FCA72}']
    // event property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaWindow;
    function GetOnUpdate: TNotifyEvent;
    procedure SetOnUpdate(const Value: TNotifyEvent);
    // interface methods 
    function FindCaption(const ACaption: string): TcaWindow;
    function FindHandle(AHandle: HWND): TcaWindow;
    function FindWindowClass(const AWindowClass: string): TcaWindow;
    procedure GetCaptionsAsStrings(AStrings: TStrings; AVisible: Boolean = True);
    // properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaWindow read GetItem; default;
    // event properties 
    property OnUpdate: TNotifyEvent read GetOnUpdate write SetOnUpdate;
  end;

  //---------------------------------------------------------------------------
  // TcaWindowsThread                                                          
  //---------------------------------------------------------------------------

  TcaWindowsThread = class(TThread)
  private
    // private fields 
    FInterval: Integer;
    FUpdateProc: TThreadMethod;
  protected
    // protected methods 
    procedure Execute; override;
  public
    // create/destroy 
    constructor CreateThread(AUpdateProc: TThreadMethod; AInterval: Integer);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // TcaWindows                                                                
  //---------------------------------------------------------------------------

  TcaWindows = class(TInterfacedObject, IcaWindows)
  private
    // private fields 
    FLastCaptions: string;
    FWindows: TObjectList;
    FThread: TcaWindowsThread;
    FUpdating: Boolean;
    // event property fields 
    FOnUpdate: TNotifyEvent;
    // private methods 
    procedure Update;
    // event property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaWindow;
    function GetOnUpdate: TNotifyEvent;
    procedure SetOnUpdate(const Value: TNotifyEvent);
  protected
    // protected methods 
    procedure DoUpdate; virtual;
    // interface methods 
    function FindCaption(const ACaption: string): TcaWindow;
    function FindHandle(AHandle: HWND): TcaWindow;
    function FindWindowClass(const AWindowClass: string): TcaWindow;
    procedure GetCaptionsAsStrings(AStrings: TStrings; AVisible: Boolean = True);
  public
    // create/destroy 
    constructor Create(AInterval: Integer);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // IcaProcessLauncher                                                        
  //---------------------------------------------------------------------------

  TcaProcessThread = class;

  TcaProcessOption = (poNoteExit, poForceExit);

  TcaProcessOptions = set of TcaProcessOption;

  TcaProcessIdleEvent = procedure(Sender: TObject; AProcessID: Cardinal) of object;

  TcaProcessExitEvent = procedure(Sender: TObject; AExitCode: DWORD) of object;

  IcaProcessLauncher = interface
  ['{0797722E-B50A-4D25-B152-487ED44C6F25}']
    // Property methods 
    function GetActive: Boolean;
    function GetAppCaption: string;
    function GetAppName: string;
    function GetCommandLine: string;
    function GetFolder: string;
    function GetIdle: Boolean;
    function GetJustOneInstance: Boolean;
    function GetProcessID: Cardinal;
    function GetShowWindow: TcaShowWindow;
    function GetThread: TcaProcessThread;
    function GetWaitOptions: TcaProcessOptions;
    procedure SetAppCaption(const Value: string);
    procedure SetAppName(const Value: string);
    procedure SetCommandLine(const Value: string);
    procedure SetFolder(const Value: string);
    procedure SetIdle(const Value: Boolean);
    procedure SetJustOneInstance(const Value: Boolean);
    procedure SetProcessID(const Value: Cardinal);
    procedure SetShowWindow(const Value: TcaShowWindow);
    procedure SetThread(const Value: TcaProcessThread);
    procedure SetWaitOptions(const Value: TcaProcessOptions);
    // Event property methods 
    function GetOnFinished: TcaProcessExitEvent;
    function GetOnIdle: TcaProcessIdleEvent;
    procedure SetOnFinished(const Value: TcaProcessExitEvent);
    procedure SetOnIdle(const Value: TcaProcessIdleEvent);
    // Interface methods 
    procedure DoFinished(ExitCode: DWORD);
    procedure DoIdle;
    procedure Execute;
    procedure ReleaseApplication;
    // Properties 
    property Active: Boolean read GetActive;
    property AppCaption: string read GetAppCaption write SetAppCaption;
    property AppName: string read GetAppName write SetAppName;
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property Folder: string read GetFolder write SetFolder;
    property Idle: Boolean read GetIdle write SetIdle;
    property JustOneInstance: Boolean read GetJustOneInstance write SetJustOneInstance;
    property ProcessID: Cardinal read GetProcessID write SetProcessID;
    property ShowWindow: TcaShowWindow read GetShowWindow write SetShowWindow;
    property Thread: TcaProcessThread read GetThread write SetThread;
    property WaitOptions: TcaProcessOptions read GetWaitOptions write SetWaitOptions;
    // Event properties 
    property OnFinished: TcaProcessExitEvent read GetOnFinished write SetOnFinished;
    property OnIdle: TcaProcessIdleEvent read GetOnIdle write SetOnIdle;
  end;

  //---------------------------------------------------------------------------
  // TcaProcessLauncher                                                        
  //---------------------------------------------------------------------------

  TcaProcessLauncher = class(TInterfacedObject, IcaProcessLauncher)
  protected
    // Private fields 
    FAppCaption: string;
    FAppName: string;
    FCommandLine: string;
    FFolder: string;
    FIdle: Boolean;
    FJustOneInstance: Boolean;
    FProcessID: Cardinal;
    FShowWindow: TcaShowWindow;
    FThread: TcaProcessThread;
    FWaitOptions: TcaProcessOptions;
    // Event property fields 
    FOnFinished: TcaProcessExitEvent;
    FOnIdle: TcaProcessIdleEvent;
    // Interface property methods - IcaProcessLauncher 
    function GetActive: Boolean;
    function GetAppCaption: string;
    function GetAppName: string;
    function GetCommandLine: string;
    function GetFolder: string;
    function GetIdle: Boolean;
    function GetJustOneInstance: Boolean;
    function GetProcessID: Cardinal;
    function GetShowWindow: TcaShowWindow;
    function GetThread: TcaProcessThread;
    function GetWaitOptions: TcaProcessOptions;
    procedure SetAppCaption(const Value: string);
    procedure SetAppName(const Value: string);
    procedure SetCommandLine(const Value: string);
    procedure SetFolder(const Value: string);
    procedure SetIdle(const Value: Boolean);
    procedure SetJustOneInstance(const Value: Boolean);
    procedure SetProcessID(const Value: Cardinal);
    procedure SetShowWindow(const Value: TcaShowWindow);
    procedure SetThread(const Value: TcaProcessThread);
    procedure SetWaitOptions(const Value: TcaProcessOptions);
    // Event property methods - IcaProcessLauncher 
    function GetOnFinished: TcaProcessExitEvent;
    function GetOnIdle: TcaProcessIdleEvent;
    procedure SetOnFinished(const Value: TcaProcessExitEvent);
    procedure SetOnIdle(const Value: TcaProcessIdleEvent);
    // Private methods 
    function IsActive: Boolean;
  protected
    // Protected event triggers 
    procedure DoFinished(ExitCode: DWORD); virtual;
    procedure DoIdle; virtual;
    // Protected properties 
    property Thread: TcaProcessThread read FThread write FThread;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Interface methods - IcaProcessLauncher 
    procedure Execute;
    procedure ReleaseApplication;
    // Interface properties - IcaProcessLauncher 
    property Active: Boolean read GetActive;
    property AppCaption: string read GetAppCaption write SetAppCaption;
    property AppName: string read GetAppName write SetAppName;
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property Folder: string read GetFolder write SetFolder;
    property Idle: Boolean read GetIdle write SetIdle;
    property JustOneInstance: Boolean read GetJustOneInstance write SetJustOneInstance;
    property ProcessID: Cardinal read GetProcessID write SetProcessID;
    property ShowWindow: TcaShowWindow read GetShowWindow write SetShowWindow;
    property WaitOptions: TcaProcessOptions read GetWaitOptions write SetWaitOptions;
    // Interface event properties - IcaProcessLauncher 
    property OnFinished: TcaProcessExitEvent read GetOnFinished write SetOnFinished;
    property OnIdle: TcaProcessIdleEvent read GetOnIdle write SetOnIdle;
  end;

  //---------------------------------------------------------------------------
  // TcaProcessThread                                                          
  //---------------------------------------------------------------------------

  TcaProcessThread = class(TThread)
  private
    // Private fields 
    FProcessInfo: TProcessInformation;
    FProcessLauncher: IcaProcessLauncher;
    // Private methods 
    procedure LaunchNewProcess;
    // Event handlers 
    procedure ResetThread(Sender: TObject);
  protected
    // Protected methods 
    procedure Execute; override;
  public
    // Create/Destroy 
    constructor CreateThread(const AProcessLauncher: IcaProcessLauncher);
  end;

  function EnumerateWindows(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;

implementation

uses Types;

  //---------------------------------------------------------------------------
  // TcaProcessList                                                            
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaProcessList.Create;
begin
  inherited;
  FList := TObjectList.Create(True);
  Update;
end;

destructor TcaProcessList.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Public methods 

function TcaProcessList.FindProcessByName(const AName: String; AAllowSubString: Boolean = False): TcaProcess;
var
  Index: Integer;
  Process: TcaProcess;
  FoundProcess: Boolean;
begin
  Result := nil;
  for Index := 0 to GetProcessCount - 1 do
    begin
      Process := GetProcess(Index);
      if AAllowSubString then
        FoundProcess := Pos(LowerCase(AName), LowerCase(Process.ModuleFileName)) > 0
      else
        FoundProcess := LowerCase(Process.ModuleFileName) = LowerCase(AName);
      if FoundProcess then
        begin
          Result := Process;
          Break;
        end;
    end;
end;

function TcaProcessList.FindProcessID(AProcessID: Cardinal): TcaProcess;
var
  Index: Integer;
  Process: TcaProcess;
begin
  Result := nil;
  for Index := 0 to GetProcessCount - 1 do
    begin
      Process := GetProcess(Index);
      if Process.ProcessID = AProcessID then
        begin
          Result := Process;
          Break;
        end;
    end;
end;

procedure TcaProcessList.GetProcessNamesAsStrings(AList: TStrings; AShowMemory: Boolean);
var
  Index: Integer;
  Process: TcaProcess;
  ListEntry: String;
begin
  AList.Clear;
  AList.BeginUpdate;
  try
    for Index := 0 to GetProcessCount - 1 do
      begin
        Process := GetProcess(Index);
        ListEntry := Process.ModuleFileName;
        if AShowMemory then
          ListEntry := ListEntry + Format(' [%dK]', [Process.MemoryUsage div 1024]);
        AList.Add(ListEntry);
      end;
  finally
    AList.EndUpdate;
  end;
end;

function AscendingMemorySortFunc(Item1, Item2: Pointer): Integer;
var
  Process1: TcaProcess;
  Process2: TcaProcess;
begin
  Process1 := TcaProcess(Item1);
  Process2 := TcaProcess(Item2);
  Result := Process1.MemoryUsage - Process2.MemoryUsage;
end;

procedure TcaProcessList.SortByAscendingMemory;
begin
  FList.Sort(AscendingMemorySortFunc);
end;

function DescendingMemorySortFunc(Item1, Item2: Pointer): Integer;
var
  Process1: TcaProcess;
  Process2: TcaProcess;
begin
  Process1 := TcaProcess(Item1);
  Process2 := TcaProcess(Item2);
  Result := Process2.MemoryUsage - Process1.MemoryUsage;
end;

procedure TcaProcessList.SortByDescendingMemory;
begin
  FList.Sort(DescendingMemorySortFunc);
end;

function ProcessNameSortFunc(Item1, Item2: Pointer): Integer;
var
  Process1: TcaProcess;
  Process2: TcaProcess;
begin
  Process1 := TcaProcess(Item1);
  Process2 := TcaProcess(Item2);
  Result := AnsiCompareText(Process1.ModuleFileName, Process2.ModuleFileName);
end;

procedure TcaProcessList.SortByProcessName;
begin
  FList.Sort(ProcessNameSortFunc);
end;

function TcaProcessList.StopProcessByID(AProcessID: Cardinal): Boolean;
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_TERMINATE, LongBool(False), AProcessID);
  Result := TerminateProcess(ProcessHandle, 0);
end;

function TcaProcessList.StopProcessByName(const AName: String; AAllowSubString: Boolean = False): Boolean;
var
  Process: TcaProcess;
begin
  Result := False;
  Process := FindProcessByName(AName, AAllowSubString);
  if Process <> nil then
    Result := StopProcessByID(Process.ProcessID);
end;

procedure TcaProcessList.Update;
begin
  BuildList;
end;

  // Private methods 

procedure TcaProcessList.BuildList;

  function GetProcessMemoryUsage(APID: Cardinal): DWORD;
  var
    ProcessHandle: THandle;
    PMC: TProcessMemoryCounters;
  begin
    Result := 0;
    ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, False, aPID);
    if ProcessHandle <> 0 then
      try
        PMC.cb:=SizeOf(PMC);
        GetProcessMemoryInfo(ProcessHandle, @PMC, SizeOf(PMC));
        Result := PMC.WorkingSetSize;
      finally
        CloseHandle(ProcessHandle);
      end;
  end;

  procedure BuildListForNT4;
  var
    Handle: THandle;
    Index: Integer;
    ModuleFileName: array[0..MAX_PATH] of Char;
    Needed: DWORD;
    PIDs: array[0..1024] of DWORD;
    Process: TcaProcess;
    ProcessCount: Integer;
  begin
    if EnumProcesses(@PIDs, Sizeof(PIDs), Needed) then
      begin
        ProcessCount := (Needed div Sizeof(DWORD));
        for Index := 0 to ProcessCount - 1 do
          begin
            if PIDs[Index] <> 0 then
              begin
                Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PIDs[Index]);
                if Handle <> 0 then
                  begin
                    try
                      Process := TcaProcess.Create;
                      if GetModuleFileNameEx(Handle, 0, ModuleFileName, Sizeof(ModuleFileName)) = 0 then
                        begin
                          Process.ModuleFileName := '[System]';
                          Process.ProcessID := INVALID_HANDLE_VALUE;
                        end
                      else
                        begin
                          Process.ModuleFileName := ModuleFileName;
                          Process.ProcessID := PIDs[Index];
                        end;
                    finally
                      CloseHandle(Handle);
                    end;
                    Process.MemoryUsage := GetProcessMemoryUsage(PIDs[Index]);
                    FList.Add(Process);
                  end;
              end;
          end;
      end;
  end;

  procedure BuildListFor9xAnd2000;
  var
    NextProc: Boolean;
    ProcEntry: TProcessEntry32;
    Process: TcaProcess;
    SnapProcHandle: THandle;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if SnapProcHandle <> THandle(-1) then
      try
        ProcEntry.dwSize := Sizeof(ProcEntry);
        NextProc := Process32First(SnapProcHandle, ProcEntry);
        while NextProc do
          begin
            Process := TcaProcess.Create;
            Process.ModuleFileName := ProcEntry.szExeFile;
            Process.ProcessID := ProcEntry.th32ProcessID;
            Process.MemoryUsage := GetProcessMemoryUsage(Process.ProcessID);
            FList.Add(Process);
            NextProc := Process32Next(SnapProcHandle, ProcEntry);
          end;
      finally
        CloseHandle(SnapProcHandle);
      end;
  end;

begin
  FList.Clear;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion = 4) then
    BuildListForNT4
  else
    BuildListFor9xAnd2000;
end;

  // Property methods 

function TcaProcessList.GetProcess(Index: Integer): TcaProcess;
begin
  Result := TcaProcess(FList[Index]);
end;

function TcaProcessList.GetProcessCount: Integer;
begin
  Result := FList.Count;
end;

  //---------------------------------------------------------------------------
  // TcaWindow                                                                 
  //---------------------------------------------------------------------------

  // private methods 

function TcaWindow.Rect: TRect;
var
  Rect: TRect;
begin
  Result := Types.Rect(0, 0, 0, 0);
  if Windows.GetWindowRect(FHandle, Rect) then
    Result := Rect;
end;

  // property methods 

function TcaWindow.GetCaption: string;
var
  TextLen: Integer;
begin
  TextLen := Succ(GetWindowTextLength(FHandle));
  SetLength(Result, TextLen);
  GetWindowText(FHandle, PChar(Result), TextLen);
end;

function TcaWindow.GetHeight: Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function TcaWindow.GetIconic: Boolean;
begin
  Result := IsIconic(FHandle);
end;

function TcaWindow.GetLeft: Integer;
begin
  Result := Rect.Left;
end;

function TcaWindow.GetParent: HWND;
begin
  Result := Windows.GetParent(FHandle);
end;

function TcaWindow.GetTop: Integer;
begin
  Result := Rect.Top;
end;

function TcaWindow.GetVisible: Boolean;
begin
  Result := IsWindowVisible(FHandle);
end;

function TcaWindow.GetWidth: Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function TcaWindow.GetWindowClass: string;
begin
  Result := Utils.GetWindowClass(FHandle);  
end;

  //---------------------------------------------------------------------------
  // TcaWindowsThread                                                          
  //---------------------------------------------------------------------------

  // create/destroy 

constructor TcaWindowsThread.CreateThread(AUpdateProc: TThreadMethod; AInterval: Integer);
begin
  inherited Create(True);
  FUpdateProc := AUpdateProc;
  FInterval := AInterval;
  FreeOnTerminate := True;
  Resume;
end;

destructor TcaWindowsThread.Destroy;
begin
  inherited;
end;

  // protected methods 

procedure TcaWindowsThread.Execute;
begin
  while not Terminated do
    begin
      Synchronize(FUpdateProc);
      Sleep(FInterval);
    end;
end;

  //---------------------------------------------------------------------------
  // TcaWindows                                                                
  //---------------------------------------------------------------------------

  // create/destroy 

constructor TcaWindows.Create(AInterval: Integer);
begin
  inherited Create;
  FWindows := TObjectList.Create(True);
  FThread := TcaWindowsThread.CreateThread(Update, AInterval);
end;

destructor TcaWindows.Destroy;
begin
  FThread.Terminate;
  FWindows.Free;
  inherited;
end;

  // interface methods 

function TcaWindows.FindCaption(const ACaption: string): TcaWindow;
var
  Index: Integer;
  Window: TcaWindow;
begin
  while FUpdating do
    Application.ProcessMessages;
  Result := nil;
  for Index := 0 to Pred(GetCount) do
    begin
      Window := GetItem(Index);
      if Window.Caption = ACaption then
        begin
          Result := Window;
          Break;
        end;
    end;
end;

function TcaWindows.FindHandle(AHandle: HWND): TcaWindow;
var
  Index: Integer;
  Window: TcaWindow;
begin
  while FUpdating do
    Application.ProcessMessages;
  Result := nil;
  for Index := 0 to Pred(GetCount) do
    begin
      Window := GetItem(Index);
      if Window.Handle = AHandle then
        begin
          Result := Window;
          Break;
        end;
    end;
end;

function TcaWindows.FindWindowClass(const AWindowClass: string): TcaWindow;
var
  Index: Integer;
  Window: TcaWindow;
begin
  while FUpdating do
    Application.ProcessMessages;
  Result := nil;
  for Index := 0 to Pred(GetCount) do
    begin
      Window := GetItem(Index);
      if Window.WindowClass = AWindowClass then
        begin
          Result := Window;
          Break;
        end;
    end;
end;

procedure TcaWindows.GetCaptionsAsStrings(AStrings: TStrings; AVisible: Boolean = True);
var
  Index: Integer;
  Window: TcaWindow;
begin
  AStrings.BeginUpdate;
  AStrings.Clear;
  for Index := 0 to Pred(GetCount) do
    begin
      Window := GetItem(Index);
      if (Window.Visible = AVisible) and (Trim(Window.Caption) <> '') then
        AStrings.Add(Window.Caption);
    end;
  AStrings.EndUpdate;
end;

  // protected methods 

procedure TcaWindows.DoUpdate;
begin
  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

  // private methods 

procedure TcaWindows.Update;
var
  Captions: TStrings;
begin
  FUpdating := True;
  Captions := Auto(TStringList.Create).Instance;
  FWindows.Clear;
  EnumWindows(@EnumerateWindows, Integer(FWindows));
  GetCaptionsAsStrings(Captions);
  if Captions.Text <> FLastCaptions then
    begin
      DoUpdate;
      FLastCaptions := Captions.Text;
    end;
  FUpdating := False;
end;

function EnumerateWindows(hWnd: HWND; lParam: LPARAM): BOOL;
var
  AWindows: TObjectList;
  Window: TcaWindow;
begin
  AWindows := TObjectList(lParam);
  Window := TcaWindow.Create;
  Window.Handle := hWnd;
  AWindows.Add(Window);
  Result := True;
end;

  // event property methods 

function TcaWindows.GetCount: Integer;
begin
  Result := FWindows.Count;
end;

function TcaWindows.GetItem(Index: Integer): TcaWindow;
begin
  Result := TcaWindow(FWindows[Index]);
end;

  // event property methods 

function TcaWindows.GetOnUpdate: TNotifyEvent;
begin
  Result := FOnUpdate;
end;

procedure TcaWindows.SetOnUpdate(const Value: TNotifyEvent);
begin
  FOnUpdate := Value;
end;

  //---------------------------------------------------------------------------
  // TcaProcessLauncher                                                        
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaProcessLauncher.Create;
begin
  inherited;
  FShowWindow := swShowNormal;
  FWaitOptions := [];
end;

destructor TcaProcessLauncher.Destroy;
begin
  ReleaseApplication;
  inherited;
end;

  // Interface methods - IcaProcessLauncher 

procedure TcaProcessLauncher.Execute;
begin
  if not GetActive then
    begin
      FThread := TcaProcessThread.CreateThread(Self as IcaProcessLauncher);
      if (FWaitOptions = []) then FThread := nil;
    end;
end;

procedure TcaProcessLauncher.ReleaseApplication;
begin
  if (FThread <> nil) then
  try
    FThread.DoTerminate;
  except
    on E: Exception do ;
  end;
end;

  // Protected event triggers 

procedure TcaProcessLauncher.DoFinished(ExitCode: DWORD);
begin
  if Assigned(FOnFinished) then FOnFinished(Self, ExitCode);    
end;

procedure TcaProcessLauncher.DoIdle;
begin
  if Assigned(FOnIdle) then FOnIdle(Self, FProcessID);
end;

  // Private methods 

function TcaProcessLauncher.IsActive: Boolean;
begin
  Result := GetActive;
end;

  // Interface properties - IcaProcessLauncher 

function TcaProcessLauncher.GetActive: Boolean;
begin
  if FWaitOptions = [] then
    Result := False
  else
    Result := FThread <> nil;
end;

function TcaProcessLauncher.GetAppCaption: string;
begin
  Result := FAppCaption;
end;

function TcaProcessLauncher.GetAppName: string;
begin
  Result := FAppName;
end;

function TcaProcessLauncher.GetCommandLine: string;
begin
  Result := FCommandLine;
end;

function TcaProcessLauncher.GetFolder: string;
begin
  Result := FFolder;
end;

function TcaProcessLauncher.GetIdle: Boolean;
begin
  Result := FIdle;
end;

function TcaProcessLauncher.GetJustOneInstance: Boolean;
begin
  Result := FJustOneInstance;
end;

function TcaProcessLauncher.GetProcessID: Cardinal;
begin
  Result := FProcessID;
end;

function TcaProcessLauncher.GetShowWindow: TcaShowWindow;
begin
  Result := FShowWindow;
end;

function TcaProcessLauncher.GetThread: TcaProcessThread;
begin
  Result := FThread;
end;

function TcaProcessLauncher.GetWaitOptions: TcaProcessOptions;
begin
  Result := FWaitOptions;
end;

procedure TcaProcessLauncher.SetAppCaption(const Value: string);
begin
  FAppCaption := Value;
end;

procedure TcaProcessLauncher.SetAppName(const Value: string);
begin
  if not IsActive then FAppName := Trim(Value);
end;

procedure TcaProcessLauncher.SetCommandLine(const Value: string);
begin
  if not IsActive then FCommandLine := Trim(Value);
end;

procedure TcaProcessLauncher.SetFolder(const Value: string);
begin
  if not IsActive then FFolder := Trim(Value);
end;

procedure TcaProcessLauncher.SetIdle(const Value: Boolean);
begin
  if not IsActive then FIdle := Value;
end;

procedure TcaProcessLauncher.SetJustOneInstance(const Value: Boolean);
begin
  FJustOneInstance := Value;
end;

procedure TcaProcessLauncher.SetProcessID(const Value: Cardinal);
begin
  FProcessID := Value;
end;

procedure TcaProcessLauncher.SetShowWindow(const Value: TcaShowWindow);
begin
  if not IsActive then FShowWindow := Value;
end;

procedure TcaProcessLauncher.SetThread(const Value: TcaProcessThread);
begin
  FThread := Value;
end;

procedure TcaProcessLauncher.SetWaitOptions(const Value: TcaProcessOptions);
begin
  if not IsActive then FWaitOptions := Value;
end;

  // Event property methods - IcaProcessLauncher 

function TcaProcessLauncher.GetOnFinished: TcaProcessExitEvent;
begin
  Result := FOnFinished;
end;

function TcaProcessLauncher.GetOnIdle: TcaProcessIdleEvent;
begin
  Result := FOnIdle;
end;

procedure TcaProcessLauncher.SetOnFinished(const Value: TcaProcessExitEvent);
begin
  FOnFinished := Value;
end;

procedure TcaProcessLauncher.SetOnIdle(const Value: TcaProcessIdleEvent);
begin
  FOnIdle := Value;
end;

  //---------------------------------------------------------------------------
  // TcaProcessThread                                                          
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaProcessThread.CreateThread(const AProcessLauncher: IcaProcessLauncher);
begin
  inherited Create(True);
  FProcessLauncher := AProcessLauncher;
  FreeOnTerminate := True;
  OnTerminate := ResetThread;
  Resume;
end;

  // Protected methods 

procedure TcaProcessThread.Execute;
var
  AppWnd: HWND;
begin
  inherited;
  if FProcessLauncher.JustOneInstance then
    begin
      AppWnd := FindWindow(nil, PChar(FProcessLauncher.AppCaption));
      if AppWnd <> 0 then
        ShowWindow(AppWnd, SW_NORMAL)
      else
        LaunchNewProcess;
    end
  else
    LaunchNewProcess;
end;

  // Private methods 

procedure TcaProcessThread.LaunchNewProcess;
var
  ExitCode: DWORD;
  ReturnCode: DWORD;
  StartupInfo: TStartupInfo;
begin
  ExitCode := 0;
  // Set startup info 
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := cShowWindowValues[FProcessLauncher.ShowWindow];
  if CreateProcess(PChar(FProcessLauncher.CommandLine),
                   nil,
                   nil,
                   nil,
                   False,
                   CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
                   nil,
                   nil,
                   StartupInfo,
                   FProcessInfo) then
    begin
      // Fire event when launched process is idle, IOW ready to receive messages 
      ReturnCode := WaitForInputIdle(FProcessInfo.hProcess, INFINITE);
      FProcessLauncher.Idle := ReturnCode = 0;
      FProcessLauncher.ProcessID := FProcessInfo.dwProcessId;
      FProcessLauncher.DoIdle;
      // Wait if necessary 
      ReturnCode := WAIT_TIMEOUT;
      while (ReturnCode <> WAIT_OBJECT_0) and (FProcessLauncher.WaitOptions <> []) do
        ReturnCode := WaitForSingleObject(FProcessInfo.hProcess, 1000);
      GetExitCodeProcess(FProcessInfo.hProcess , ExitCode);
      // Notify process end 
      if (FProcessLauncher.WaitOptions * [poNoteExit]) <> [] then
        FProcessLauncher.DoFinished(ExitCode);
    end;
  // Tidy up 
  if (FProcessLauncher.WaitOptions = []) then
    begin
      CloseHandle(FProcessInfo.hProcess);
      CloseHandle(FProcessInfo.hThread);
      FProcessInfo.hProcess := 0;
    end;
  ResetThread(nil);
end;

  // Event handlers 

procedure TcaProcessThread.ResetThread(Sender: TObject);
var
  ExitCode: DWORD;
begin
  if FProcessInfo.hProcess <> 0 then
    begin
      GetExitCodeProcess(FProcessInfo.hProcess, ExitCode);
      if FProcessInfo.hProcess <> 0 then
        begin
          if ((FProcessLauncher.WaitOptions * [poForceExit]) <> []) then
            TerminateProcess(FProcessInfo.hProcess, ExitCode);
          CloseHandle(FProcessInfo.hProcess);
        end;
      if (FProcessInfo.hThread <> 0) then
        CloseHandle(FProcessInfo.hThread);
      FProcessInfo.hProcess := 0;
    end;
  FProcessLauncher.Thread := nil;
end;

end.

