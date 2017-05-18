unit caTextStream;

interface

uses
  Classes,
  Sysutils;

type

  //---------------------------------------------------------------------------
  // TcaBufferStream                                                           
  //---------------------------------------------------------------------------

  TcaBufferStreamState = (bsUnknown, bsRead, bsWrite);

  TcaBufferStream = class(TStream)
  private
    // private fields 
    FStream: TStream;
    // property fields 
    FBufEnd: PChar;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufSize: Cardinal;
    FState: TcaBufferStreamState;
    // event property fields 
    FOnFillBuffer: TNotifyEvent;
    FOnFlushBuffer: TNotifyEvent;
    // private methods 
    function GetBufPosition: Integer;
  protected
    // protected methods 
    function FillBuffer: Boolean; virtual;
    function FlushBuffer: Boolean; virtual;
    procedure AfterFillBuffer; virtual;
    procedure AfterFlushBuffer; virtual;
    procedure PutBack(Ch: Char); virtual;
    // protected properties 
    property BufEnd: PChar read FBufEnd;
    property Buffer: PChar read FBuffer;
    property BufPosition: Integer read GetBufPosition;
    property BufPtr: PChar read FBufPtr;
    property BufSize: Cardinal read FBufSize;
    property State: TcaBufferStreamState read FState;
    property Stream: TStream read FStream;
  public
    // create/destroy 
    constructor Create(AStream: TStream); overload; virtual;
    destructor Destroy; override;
    // public methods 
    function IsEOF: Boolean;
    function Read(var ABuffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;
    function Write(const ABuffer; Count: Integer): Integer; override;
    // event properties 
    property OnFillBuffer: TNotifyEvent read FOnFillBuffer write FOnFillBuffer;
    property OnFlushBuffer: TNotifyEvent read FOnFlushBuffer write FOnFlushBuffer;
  end;

  //---------------------------------------------------------------------------
  // TcaTextStream                                                             
  //---------------------------------------------------------------------------

  TcaTextStream = class(TcaBufferStream)
  private
    // private fields 
    FFilename: string;
    // property fields 
    FOwnStream: Boolean;
    // private methods 
    procedure FreeStream; virtual;
  protected
    // protected properties 
    property Filename: string read FFilename;
    property OwnStream: Boolean read FOwnStream write FOwnStream;
  public
    // create/destroy 
    constructor Create(const AFilename: string; AMode: Word); overload;
    destructor Destroy; override;
    // public methods 
    function GetChar: Char;
    function GetFloat: Extended;
    function GetInteger: LongInt;
    function GetLine: string;
    function GetToken(const Delimiters: string): string;
    function PutChar(Ch: Char): TcaTextStream;
    function PutEndOfLine: TcaTextStream;
    function PutFloat(Flt: Extended): TcaTextStream;
    function PutInteger(Int: LongInt): TcaTextStream;
    function PutLine(const Str: string): TcaTextStream;
    function PutPChar(const Str: PChar): TcaTextStream;
    function PutSpace: TcaTextStream;
    function PutString(const Str: string): TcaTextStream;
    function PutTab: TcaTextStream;
    function PutWideChar(WCh: WideChar): TcaTextStream;
    procedure Format(const Fmt: string; Args: array of const);
    procedure FormatLn(const Fmt: string; Args: array of const);
    procedure SkipSpaces;
    procedure WriteArgs(Args: array of const);
    procedure WriteLn(Args: array of const);
  end;

implementation

const
  cBufferSize: Integer = 8192 * 4;

  //---------------------------------------------------------------------------
  // TcaTextStream                                                             
  //---------------------------------------------------------------------------

  // create/destroy 

constructor TcaBufferStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FBufSize := cBufferSize;
  GetMem(FBuffer, FBufSize);
  FBufEnd := FBuffer + FBufSize;
  FState := bsUnknown;
end;

destructor TcaBufferStream.Destroy;
begin
  if FState = bsWrite then FlushBuffer;
  FreeMem(FBuffer, FBufSize);
  inherited;
end;

  // public methods 

function TcaBufferStream.IsEOF: Boolean;
begin
  Result := (FBufPtr = FBufEnd) and (FStream.Position = FStream.Size);
end;

function TcaBufferStream.Read(var ABuffer; Count: Integer): Integer;
var
  Ptr: PChar;
  NumBytes: Cardinal;
begin
  if FState = bsWrite then
    FlushBuffer
  else
    begin
      if FBufPtr = nil then
        FBufPtr := FBufEnd;   // empty buffer, so force a FillBuffer call 
    end;
  // The user might ask for more than one bufferful               
  // Prepare to loop until all the requested bytes have been read 
  Ptr := @ABuffer;
  Result := 0;
  while Count > 0 do
    begin
      // If the buffer is empty, then fill it 
      if FBufPtr = FBufEnd then
        if not FillBuffer then
          Break;
      NumBytes := FBufEnd - FBufPtr;
      if Cardinal(Count) < NumBytes then
      NumBytes := Count;
      // Copy the buffer to the user's memory 
      Move(BufPtr^, Ptr^, NumBytes);
      // Increment the pointers. The stream’s buffer is always within a single 
      // segment, but the user's buffer might cross segment boundaries         
      Dec(Count, NumBytes);
      Inc(FBufPtr, NumBytes);
      Inc(Result, NumBytes);
      Ptr := Ptr + NumBytes;
    end;
end;

function TcaBufferStream.Seek(Offset: Integer; Origin: Word): Integer;
var
  CurrentPosition: LongInt;
begin
  CurrentPosition := FStream.Position + GetBufPosition;
  case Origin of
    soFromBeginning: Result := Offset;
    soFromCurrent:   Result := FStream.Position + GetBufPosition + Offset;
    soFromEnd:       Result := FStream.Size - Offset;
  else
    raise Exception.CreateFmt('Invalid stream origin: %d', [Origin]);
  end;
  if Result <> CurrentPosition then
    begin
      // Flush a partial write 
      if (FState = bsWrite) and not FlushBuffer then
        raise EStreamError.Create('Seek error');
      FStream.Position := Result;
      FBufPtr := nil;
      FState := bsUnknown;
    end;
end;

function TcaBufferStream.Write(const ABuffer; Count: Integer): Integer;
var
  Ptr: Pointer;
  NumBytes: Cardinal;
begin
  // If the stream is for reading, then ignore the current buffer 
  // by forcing the position of the underlying stream to match    
  // the buffered stream's position                               
  if FState = bsRead then
    FStream.Position := Position
  else
    begin
      if FBufPtr = nil then
        begin
          // Unknown state, so start with an empty buffer 
          FBufPtr := FBuffer;
          FBufEnd := FBuffer + FBufSize;
        end;
    end;
  // The user might write more than one bufferful                    
  // Prepare to loop until all the requested bytes have been written 
  Ptr := @ABuffer;
  Result := 0;                   // Total number of bytes written 
  while Count > 0 do
    begin
      NumBytes := FBufEnd - FBufPtr;
      if Cardinal(Count) < NumBytes then
        NumBytes := Count;
      Move(Ptr^, FBufPtr^, NumBytes);
      Dec(Count, NumBytes);
      Inc(FBufPtr, NumBytes);
      Inc(Result, NumBytes);
      Ptr := PChar(Ptr) + NumBytes;
      if FBufPtr = FBufEnd then
        if not FlushBuffer then
          Break;
    end;
  if FBufPtr <> FBuffer then
    FState := bsWrite;
end;

  // protected methods 

function TcaBufferStream.FillBuffer: Boolean;
var
  NumBytes: Cardinal;
begin
  NumBytes := FStream.Read(FBuffer^, FBufSize);
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + NumBytes;
  // If nothing was read, it must be the end of file 
  Result := NumBytes > 0;
  if Result then
    FState := bsRead
  else
    FState := bsUnknown;
  AfterFillBuffer;
end;

function TcaBufferStream.FlushBuffer: Boolean;
var
  NumBytes: Cardinal;
begin
  NumBytes := FBufPtr - FBuffer;
  Result := NumBytes = Cardinal(FStream.Write(FBuffer^, NumBytes));
  FBufPtr := FBuffer;
  FState := bsUnknown;
  AfterFlushBuffer;
end;

procedure TcaBufferStream.AfterFillBuffer;
begin
  if Assigned(FOnFillBuffer) then FOnFillBuffer(Self);
end;

procedure TcaBufferStream.AfterFlushBuffer;
begin
  if Assigned(FOnFlushBuffer) then FOnFlushBuffer(Self);
end;

procedure TcaBufferStream.PutBack(Ch: Char);
begin
  if FBufPtr <= FBuffer then
    raise EStreamError.Create('PutBack overflow');
  Dec(FBufPtr);
  FBufPtr[0] := Ch;
end;

  // private methods 

function TcaBufferStream.GetBufPosition: Integer;
begin
  Result := 0;
  case FState of
    bsUnknown:  Result := 0;
    bsRead:     Result := FBufPtr - FBufEnd;
    bsWrite:    Result := FBufPtr - FBuffer;
  end;
end;

  //---------------------------------------------------------------------------
  // TcaTextStream                                                             
  //---------------------------------------------------------------------------

  // create/destroy 

constructor TcaTextStream.Create(const AFilename: string; AMode: Word);
begin
  inherited Create(TFileStream.Create(AFilename, AMode));
  FFilename := AFilename;
  FOwnStream := True;
end;

destructor TcaTextStream.Destroy;
begin
  inherited Destroy;
  FreeStream;
end;

  // public methods 

function TcaTextStream.GetChar: Char;
begin
  ReadBuffer(Result, 1);
end;

function TcaTextStream.GetFloat: Extended;
begin
  SkipSpaces;
  Result := StrToFloat(GetToken(''));
end;

function TcaTextStream.GetInteger: LongInt;
begin
  SkipSpaces;
  Result := StrToInt(GetToken(''));
end;

function TcaTextStream.GetLine: string;
var
  Ch: Char;
begin
  Result := '';
  while (Read(Ch, 1) = 1) and not (Ch in [#10, #13]) do
    Result := Result + Ch;
  if Ch = #13 then
    begin
      if (Read(Ch, 1) = 1) and (Ch <> #10) then
        PutBack(Ch);
    end;
end;

function TcaTextStream.GetToken(const Delimiters: string): string;
var
  Ch: Char;
begin
  Result := '';
  while Read(Ch, 1) = 1 do
    begin
      if (Length(Delimiters) = 0) and (Ch < ' ') then
        begin
          Putback(Ch);
          Break;
        end
      else
        begin
          if (Length(Delimiters) > 0) and (Pos(Ch, Delimiters) > 0) then
            begin
              Putback(Ch);
              Break;
            end;
        end;
      Result := Result + Ch;
    end;
end;

function TcaTextStream.PutChar(Ch: Char): TcaTextStream;
begin
  WriteBuffer(Ch, 1);
  Result := Self;
end;

function TcaTextStream.PutEndOfLine: TcaTextStream;
begin
  PutChar(#13);
  PutChar(#10);
  Result := Self;
end;

function TcaTextStream.PutFloat(Flt: Extended): TcaTextStream;
begin
  PutString(FloatToStr(Flt));
  Result := Self;
end;

function TcaTextStream.PutInteger(Int: Integer): TcaTextStream;
begin
  PutString(IntToStr(Int));
  Result := Self;
end;

function TcaTextStream.PutLine(const Str: string): TcaTextStream;
begin
  WriteBuffer(Str[1], Length(Str));
  PutEndOfLine;
  Result := Self;
end;

function TcaTextStream.PutPChar(const Str: PChar): TcaTextStream;
begin
  WriteBuffer(Str[0], StrLen(Str));
  Result := Self;
end;

function TcaTextStream.PutSpace: TcaTextStream;
begin
  PutChar(#32);
  Result := Self;
end;

function TcaTextStream.PutString(const Str: string): TcaTextStream;
begin
  WriteBuffer(Str[1], Length(Str));
  Result := Self;
end;

function TcaTextStream.PutTab: TcaTextStream;
begin
  PutChar(#9);
  Result := Self;
end;

function TcaTextStream.PutWideChar(WCh: WideChar): TcaTextStream;
begin
  WriteBuffer(WCh, SizeOf(WCh));
  Result := Self;
end;

procedure TcaTextStream.Format(const Fmt: string; Args: array of const);
begin
  PutString(SysUtils.Format(Fmt, Args));
end;

procedure TcaTextStream.FormatLn(const Fmt: string; Args: array of const);
begin
  PutString(SysUtils.Format(Fmt, Args));
  PutEndOfLine;
end;

procedure TcaTextStream.SkipSpaces;
var
  C: Char;
begin
  while Read(C, 1) = 1 do
    begin
      if C > #32 then
        begin
          Putback(C);
          Break;
        end;
    end;
end;

procedure TcaTextStream.WriteArgs(Args: array of const);
var
  I: Integer;
begin
  for I := Low(Args) to High(Args) do
    begin
      case Args[I].VType of
        vtInteger:         PutInteger(Args[I].VInteger);
        vtBoolean:
          if Args[I].VBoolean then
            PutString('True')
          else
            PutString('False');
        vtChar:            PutChar(Args[I].VChar);
        vtExtended:        PutFloat(Args[I].VInteger);
        vtString:          PutString(Args[I].VString^);
        vtPointer:         Format('%p', [Args[I].VPointer]);
        vtPChar:           PutPChar(Args[I].VPChar);
        vtClass:           PutString(Args[I].VClass.ClassName);
        vtObject:
          begin
            PutChar('(');
            PutString(Args[I].VObject.ClassName);
            PutChar(')');
          end;
        vtAnsiString:      PutString(string(Args[I].VAnsiString));
        vtWideChar:        PutWideChar(Args[I].VWideChar);
        vtCurrency:        PutFloat(Args[I].VCurrency^);
        vtVariant:         PutString(Args[I].VVariant^);
      end;
      if (I < High(Args)) and (Args[I].VType <> vtChar) then
        PutSpace;
    end;
end;

procedure TcaTextStream.WriteLn(Args: array of const);
begin
  WriteArgs(Args);
  PutEndOfLine;
end;

  // private methods 

procedure TcaTextStream.FreeStream;
begin
  if FOwnStream then
  begin
    FStream.Free;
    FStream := nil;
  end;
end;

end.
