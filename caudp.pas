unit caUDP;

interface

uses
  Windows, SysUtils, Classes, WinSock, syncobjs;

type

  TcaUDPDataEvent = procedure(Sender: TObject; const Buffer: Pointer; const RecvSize: Integer;
    const Peer: string; const Port: Integer) of object;

  TcaUDPSender = class(TComponent)
  private
    // Private fields 
    FHandle: TSocket;
    FActive: Boolean;
    FRemoteIP: String;
    FRemoteHost: String;
    FRemotePort: Word;
    CS: TCriticalSection;
    Procedure SetActive(const Value: Boolean);
    Procedure SetRemoteIP(const Value: String);
    Procedure SetRemoteHost(const Value: String);
    Procedure SetRemotePort(const Value: Word);
  public
    // Public methods 
    Class function ResolveHost(const psHost: string; var psIP: string): u_long; virtual;
    Class function ResolveIP(const psIP: string): string; virtual;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Connect;
    Procedure Disconnect;
    Function SendBuf(var Buffer; BufSize: Integer): Integer;
    property Handle: TSocket read FHandle;
  published
    { Published declarations }
    property Active: Boolean read FActive write SetActive default False;
    property RemoteIP: String read FRemoteIP write SetRemoteIP;
    property RemoteHost: String read FRemoteHost write SetRemoteHost;
    property RemotePort: Word read FRemotePort write SetRemotePort;
  end;

  TUDPReceiver = class;

  TUDPReceiverThread = class(TThread)
  protected
    FReceiver: TUDPReceiver;
    FBuffer: Pointer;
    FRecvSize: Integer;
    FPeer: string;
    FPort: Integer;
    FBufSize: Integer;
    procedure SetBufSize(const Value: Integer);
  public
    procedure Execute; override;
    procedure UDPRead;
  published
    Property BufSize: Integer read FBufSize write SetBufSize;
    Property Receiver: TUDPReceiver read FReceiver write FReceiver;
  end;

  TUDPReceiver = class(TComponent)
  private
    { Private declarations }
    FHandle: TSocket;
    FActive: Boolean;
    FPort: Word;
    FBufferSize: Integer;
    FMulticastIP : String;
//    FUDPBuffer: Pointer;
    FOnUDPData: TcaUDPDataEvent;
    FUDPReceiverThread: TUDPReceiverThread;
    Procedure SetActive(const Value: Boolean);
    Procedure SetPort(const Value: Word);
    Procedure SetBufferSize(const Value: Integer);
    procedure SetMulticastIP(const Value: String);
  protected
    { Protected declarations }
  public
    { Public declarations }
    Class Function BindMulticast(const Socket: TSocket; const IP:String): LongInt; virtual;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Connect;
    Procedure Disconnect;
    procedure DoUDPRead(const Buffer: Pointer; const RecvSize:Integer; const Peer: string; const Port: Integer); virtual;
    property Handle: TSocket read FHandle;
  published
    { Published declarations }
    property Active: Boolean read FActive write SetActive default False;
    property Port: Word read FPort write SetPort;
    property BufferSize: Integer read FBufferSize write SetBufferSize default 65000;
    property OnUDPData: TcaUDPDataEvent read FOnUDPData write FOnUDPData;
    property MulticastIP: String read FMulticastIP write SetMulticastIP;
  end;

type // exception
  EBufferUDP = Exception;

procedure Register;

resourcestring
  EUDPNOTACTIVE = 'UDP Socket not connected';
  EUDPACTIVED = 'UDP Socket already connected';
  EWSAError = 'Socket Error : %d';
  EUNABLERESOLVEHOST = 'Unable to resolve host: %s';
  EUNABLERESOLVEIP = 'Unable to resolve IP: %s';
  EZEROBYTESEND = '0 bytes were sent.';
  EPACKAGETOOBIG = 'Package Size Too Big: %d';
  ENOREMOTESIDE = 'Remote Host/IP not identified!';
  ESIZEOUTOFBOUNDARY = 'Size value is out of boundary!';
  EWSAENOBUFS        = 'An operation on a socket could not be performed because the system lacked sufficient buffer space or because a queue was full.';
  EWSANOTINITIALISED = 'A successful WSAStartup must occur before using this function.';
  EWSAENETDOWN       = 'The network subsystem has failed.';
  EWSAEFAULT         = 'optval is not in a valid part of the process address space or optlen argument is too small.';
  EWSAEINPROGRESS    = 'A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function.';
  EWSAEINVAL         = 'level is not valid, or the information in optval is not valid.';
  EWSAENETRESET      = 'Connection has timed out when SO_KEEPALIVE is set.';
  EWSAENOPROTOOPT    = 'The option is unknown or unsupported for the specified provider.';
  EWSAENOTCONN       = 'Connection has been reset when SO_KEEPALIVE is set.';
  EWSAENOTSOCK       = 'The descriptor is not a socket.';
  EWSAUNKNOW         = 'Unknow socket error.';
implementation

procedure Register;
begin
  RegisterComponents('Samson', [TcaUDPSender, TUDPReceiver]);
end;

Type
  TIMR = Packed Record
    imr_multiaddr: LongInt;
    imr_interface: LongInt;
  End;

{ TcaUDPSender }

procedure TcaUDPSender.Connect;
Var
  Faddr: TSockAddrIn;
begin
  CS.Enter;
  try
    If FActive then
      Raise EBufferUDP.CreateRes(@EUDPACTIVED);

    If ((FRemoteHost='') and (FRemoteIP='')) then
      Raise EBufferUDP.CreateRes(@ENOREMOTESIDE);

    If Not (csDesigning in ComponentState) then
    Begin
      FHandle:= WinSock.Socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
      If FHandle = INVALID_SOCKET then
        Raise EBufferUDP.CreateResFmt(@EWSAError, [WSAGetLastError]);

      with faddr do begin
        sin_family := PF_INET;
        sin_port := WinSock.htons(FRemotePort);
    //    sin_addr.s_addr := WinSock.ResolveHost(fsHost, fsPeerAddress);
        if length(FRemoteIP) > 0 then begin
          sin_addr.s_addr := WinSock.inet_addr(PChar(FRemoteIP));
        end;
      end;
      WinSock.connect(FHandle, faddr, Sizeof(faddr));
    End;

    FActive:= True;
  finally
    CS.Leave;
  end;
end;

constructor TcaUDPSender.Create(AOwner: TComponent);
begin
  inherited;
  CS:= TCriticalSection.Create;
  FActive:= False;
  FHandle := INVALID_SOCKET;
//  FReceiveTimeout := -1;
end;

destructor TcaUDPSender.Destroy;
begin
  Active:= False;
  CS.Free;
  inherited;
end;

procedure TcaUDPSender.Disconnect;
Var
  OldHandle: TSocket;
begin
  CS.Enter;
  try
    If FActive then
    Begin
      OldHandle:= FHandle;
      FHandle:= INVALID_SOCKET;
      CloseSocket(OldHandle);
    End;
  finally
    FActive:= False;
    CS.Leave;
  end;
end;

class function TcaUDPSender.ResolveHost(const psHost: string;
  var psIP: string): u_long;
Var
  pa: PChar;
  sa: TInAddr;
  aHost: PHostEnt;
begin
  psIP := psHost;
  // Sometimes 95 forgets who localhost is
  if CompareText(psHost, 'LOCALHOST') = 0 then
  begin
    sa.S_un_b.s_b1 := #127;
    sa.S_un_b.s_b2 := #0;
    sa.S_un_b.s_b3 := #0;
    sa.S_un_b.s_b4 := #1;
    psIP := '127.0.0.1';
    Result := sa.s_addr;
  end else begin
    // Done if is tranlated (ie There were numbers}
    Result := inet_addr(PChar(psHost));
    // If no translation, see if it resolves}
    if Result = u_long(INADDR_NONE) then begin
      aHost := Winsock.GetHostByName(PChar(psHost));
      if aHost = nil then
      begin
        Result:= 0;
        psIP:= '';
        Exit;
        //Raise EBufferUDP.CreateResFmt(@EUNABLERESOLVEHOST, [psHost]);
      end else
      begin
        pa := aHost^.h_addr_list^;
        sa.S_un_b.s_b1 := pa[0];
        sa.S_un_b.s_b2 := pa[1];
        sa.S_un_b.s_b3 := pa[2];
        sa.S_un_b.s_b4 := pa[3];
        psIP:= String(inet_ntoa(sa));
        //psIP := TInAddrToString(sa);
      end;
      Result := sa.s_addr;
    end;
  end;
end;

class function TcaUDPSender.ResolveIP(const psIP: string): string;
var
  i: Integer;
  P: PHostEnt;
begin
  result := '';
  if CompareText(psIP, '127.0.0.1') = 0 then
  begin
    result := 'LOCALHOST';
  end else
  begin
    i := Winsock.inet_addr(PChar(psIP));
    P := Winsock.GetHostByAddr(@i, 4, PF_INET);
    If P = nil then
    Begin
      Result:= '';
      Exit;
      // Raise EBufferUDP.CreateResFmt(@EUNABLERESOLVEIP, [psIP]);
      //CheckForSocketError2(SOCKET_ERROR, [WSANO_DATA]);
    End else
    Begin
      result := P.h_name;
    End;
  end;
end;

Function TcaUDPSender.SendBuf(var Buffer; BufSize: Integer): Integer;
begin
  CS.Enter;
  try
    Result:= 0;
    If BufSize<=0 then
      Exit;
    If Not FActive then
      Raise EBufferUDP.CreateRes(@EUDPNOTACTIVE);

    Result:= Winsock.send(FHandle, Buffer, BufSize, 0);
    If Result<>BufSize then
    Begin
      Case Result of
        0:
          Raise EBufferUDP.CreateRes(@EZEROBYTESEND);
        SOCKET_ERROR:
          If WSAGetLastError = WSAEMSGSIZE then
            Raise EBufferUDP.CreateResFmt(@EPACKAGETOOBIG, [BufSize])
      End;{CASE}
    End;
  finally
    CS.Leave;
  end;
end;

procedure TcaUDPSender.SetActive(const Value: Boolean);
begin
  If FActive<>Value then
  Begin
    If Value then
      Connect
    Else
      Disconnect;
  End;
end;

procedure TcaUDPSender.SetRemoteHost(const Value: String);
Var
  IsConnected: Boolean;
begin
  If FRemoteHost<>Value then
  Begin
    IsConnected:= Active;
    Active:= False;
    FRemoteHost:= Value;
    If Not (csDesigning in ComponentState) then
      ResolveHost(FRemoteHost, FRemoteIP);
    // Resovle IP
    Active:= IsConnected;
  End;
end;

procedure TcaUDPSender.SetRemoteIP(const Value: String);
Var
  IsConnected: Boolean;
begin
  If FRemoteIP<>Value then
  Begin
    IsConnected:= Active;
    Active:= False;
    FRemoteIP:= Value;
    // Resovle Host name
    If Not (csDesigning in ComponentState) then
      FRemoteHost:= ResolveIP(FRemoteIP);
    Active:= IsConnected;
  End;
end;

procedure TcaUDPSender.SetRemotePort(const Value: Word);
Var
  IsConnected: Boolean;
begin
  If FRemotePort<>Value then
  Begin
    IsConnected:= Active;
    Active:= False;
    FRemotePort:= Value;
    Active:= IsConnected;
  End;
end;

{ TUDPReceiver }

class function TUDPReceiver.BindMulticast(const Socket: TSocket;
  const IP: String): LongInt;
Var
  lpMulti: TIMR;
Begin
  lpMulti.imr_multiaddr := inet_addr(PChar(IP));
  lpMulti.imr_interface := 0;
  Result:= SetSockOpt(Socket, IPPROTO_IP, IP_ADD_MEMBERSHIP, @lpMulti, Sizeof(lpMulti));
End;

procedure TUDPReceiver.Connect;
var
  m_addr: TSockAddrIn;
begin
  If FActive then
    Raise EBufferUDP.CreateRes(@EUDPACTIVED);

  If csDesigning in ComponentState then
  Begin
    FActive:= True;
    Exit;
  End;

  // SOCKET
  FHandle := Winsock.Socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
  If FHandle = INVALID_SOCKET then
    Raise EBufferUDP.CreateResFmt(@EWSAError, [WSAGetLastError]);

  // BIND
  With m_addr do begin
    sin_family := PF_INET;
    sin_port := Winsock.htons(FPort);
    sin_addr.s_addr := INADDR_ANY;
  End;
  If WinSock.bind(FHandle, m_addr, Sizeof(m_addr))=SOCKET_ERROR then
    Raise EBufferUDP.CreateResFmt(@EWSAError, [WSAGetLastError]);

  // Bind Multicast
  If FMulticastIP<>'' then
    If BindMulticast(FHandle, FMulticastIP)=SOCKET_ERROR then
      Case WSAGetLastError of
        WSAENOBUFS:        Raise EBufferUDP.CreateRes(@EWSAENOBUFS       );
        WSANOTINITIALISED: Raise EBufferUDP.CreateRes(@EWSANOTINITIALISED);
        WSAENETDOWN:       Raise EBufferUDP.CreateRes(@EWSAENETDOWN      );
        WSAEFAULT:         Raise EBufferUDP.CreateRes(@EWSAEFAULT        );
        WSAEINPROGRESS:    Raise EBufferUDP.CreateRes(@EWSAEINPROGRESS   );
        WSAEINVAL:         Raise EBufferUDP.CreateRes(@EWSAEINVAL        );
        WSAENETRESET:      Raise EBufferUDP.CreateRes(@EWSAENETRESET     );
        WSAENOPROTOOPT:    Raise EBufferUDP.CreateRes(@EWSAENOPROTOOPT   );
        WSAENOTCONN:       Raise EBufferUDP.CreateRes(@EWSAENOTCONN      );
        WSAENOTSOCK:       Raise EBufferUDP.CreateRes(@EWSAENOTSOCK      );
        Else
          Raise EBufferUDP.CreateRes(@EWSAUNKNOW);
      End; {CASE}

  // Thread read
  FUDPReceiverThread := TUDPReceiverThread.Create(True);
  With FUDPReceiverThread do
  Begin
    Receiver:= Self;
    BufSize:= FBufferSize;
    FreeOnTerminate := True;
    Resume;
  End;

  FActive:= True;
end;

constructor TUDPReceiver.Create(AOwner: TComponent);
begin
  inherited;
  FHandle := INVALID_SOCKET;
  FActive:= False;
  FBufferSize:= 65000;
  FMulticastIP:= '';
end;

destructor TUDPReceiver.Destroy;
begin
  Active:= False;
  inherited;
end;

procedure TUDPReceiver.Disconnect;
Var
  OldHandle: TSocket;
begin
  If Not FActive then
    Exit;

  try
    OldHandle:= FHandle;
    FHandle:= INVALID_SOCKET;
    CloseSocket(OldHandle);
  finally
    FActive:= False;
  end;

  If FUDPReceiverThread <> nil then
  Begin
    FUDPReceiverThread.Terminate;
    FUDPReceiverThread.WaitFor;
  End;
end;

procedure TUDPReceiver.DoUDPRead(const Buffer: Pointer; const RecvSize:Integer;
  const Peer: string; const Port: Integer);
begin
  If Assigned(FOnUDPData) then begin
    FOnUDPData(Self, Buffer, RecvSize, Peer, Port);
  End;
end;

procedure TUDPReceiver.SetActive(const Value: Boolean);
begin
  If FActive<>Value then
  Begin
    If Value then
      Connect
    Else
      Disconnect;
  End;
end;

procedure TUDPReceiver.SetBufferSize(const Value: Integer);
begin
  If FBufferSize<>Value then
  Begin
    If ((Value>=1024) and (Value<=65000)) then
      FBufferSize:= Value
    Else
      Raise EBufferUDP.CreateRes(@ESIZEOUTOFBOUNDARY);
  End;
end;

procedure TUDPReceiver.SetMulticastIP(const Value: String);
Var
  IsConnected: Boolean;
begin
  If Value<>FMulticastIP then
  Begin
    IsConnected:= Active;
    Active:= False;
    FMulticastIP:= Value;
    Active:= IsConnected;
  End;
end;

procedure TUDPReceiver.SetPort(const Value: Word);
Var
  IsConnected: Boolean;
begin
  If FPort<>Value then
  Begin
    IsConnected:= Active;
    Active:= False;
    FPort:= Value;
    Active:= IsConnected;
  End;
end;

{ TUDPReceiverThread }

procedure TUDPReceiverThread.Execute;
var
  i: Integer;
  addr_remote: TSockAddrin;
  arSize: Integer;
begin
  GetMem(FBuffer, FBufSize);
  arSize:= SizeOf(addr_remote);
  while FReceiver.Active and not Terminated do
  Begin
    i := arSize;
    FRecvSize := Winsock.RecvFrom(FReceiver.Handle, FBuffer^, FBufSize, 0, addr_remote, i);
    If FReceiver.Active and (FRecvSize>0) then
    Begin
      //fsData := Copy(fListener.fsUDPBuffer, 1, iByteCount);
      FPeer := String(inet_ntoa(addr_remote.sin_addr));
      //FPeer := String(TWinshoe.TInAddrToString(addr_remote.sin_addr));
      FPort := Winsock.NToHS(addr_remote.sin_port);
      Synchronize(UDPRead);
    End;
  End;
  FreeMem(FBuffer);
end;

procedure TUDPReceiverThread.SetBufSize(const Value: Integer);
begin
  If FBufSize<> Value then
    FBufSize:= Value;
end;

procedure TUDPReceiverThread.UDPRead;
begin
  FReceiver.DoUDPRead(FBuffer, FRecvSize, FPeer, FPort);
end;

Var
  GWSADATA: TWSADATA;
initialization
  WSAStartup(MakeWord(2, 0), GWSADATA);
finalization
  WSACleanup;
end.
