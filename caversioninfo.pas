unit caVersionInfo;

{$H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  ppchar = ^Pchar;

    TVersionResources = (vrCompanyName, vrFileDescription, vrFileVersion,
                         vrInternalName, vrLegalCopyright, vrOriginalFilename,
                         vrProductName, vrProductVersion, vrComments, vrFlags,
                         vrShortFileVersion, vrDateTime, vrFileSize);

  TcaVersionLabel = class(TCustomLabel)
  private
    { Private declarations }
    FVersionResource: TVersionResources;
    FInfoPrefix: string;
    FShowInfoPrefix: boolean;
    FVersionResourceKey: string;
    FLangCharset: string;
    FInfoString: string;                  // read only caption - current version info
    FFilename: string;                    // '' = current exe, DLL whatever.
    FDateTimeFormat: String;
    FFileSizeFormat: String;

    function CalcLangCharset(Buffer: pointer; Buflen: UINT): String;
    function Slice(var S: string; Delimiter: string): string;
    procedure SetupInfoPrefix;
    procedure SetupResourceKey;
    function GetStringFileInfo(Buffer: Pchar; size: integer): string;
    function GetFixedFileInfo(Buffer: PChar; size: integer): string;
    function GetDateTimeInfo: String;
    function GetFileSizeInfo: String;
    function GetInfo: string;
    procedure SetupCaption;
  protected
    { Protected declarations }
    procedure SetFileSizeFormat(Value: String);    
    procedure SetDateTimeFormat(Value: String);
    procedure SetFilename(Value: string);
    procedure SetInfoPrefix(Value: String);
    function GetInfoPrefix: string;
    procedure SetVersionResource(Value: TVersionResources);
    procedure SetShowInfoPrefix(Value: boolean);
    procedure SetVersionResourceKey(Value: string);
    procedure SetLangCharset(Value: string);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property VersionResource: TVersionResources read FVersionResource
                                                write SetVersionResource;
    property VersionResourceKey: string read FVersionResourceKey
                                         write SetVersionResourceKey;
    property InfoPrefix: String read GetInfoPrefix write SetInfoPrefix;
    property ShowInfoPrefix: boolean read FShowInfoPrefix write SetShowInfoPrefix;
    property LangCharset: string read FLangCharset write SetLangCharset;
    property WordWrap;
    property Align;
    property Color;
    property Font;
    property AutoSize;
    property Alignment;
    property ParentFont;
    property Visible;
    property Transparent;
    property InfoString: string read FInfoString; // Read only copy of caption
    property Filename: string read FFilename write SetFilename;
    property DateTimeFormat: String read FDateTimeFormat write SetDateTimeFormat;
    property FileSizeFormat: String read FFileSizeFormat write SetFileSizeFormat;
  end;

const
    // The order of this array must be the same as the VersionResources
    // enum type as that is used for the index lookup
    VersionLookup: array[TVersionResources, 0..1] of string = (
                    ('CompanyName', 'Company Name:'),
                    ('FileDescription', 'File Description:'),
                    ('FileVersion', 'File Version:'),
                    ('InternalName', 'Internal Name:'),
                    ('LegalCopyright', 'Legal Copyright:'),
                    ('OriginalFilename', 'Original Filename:'),
                    ('ProductName', 'Product Name:'),
                    ('ProductVersion', 'Product Version:'),
                    ('Comments', 'Comments:'),
                    ('Flags', 'Flags:'),
                    ('FileVersion', 'v'),
                    ('DateTime', 'File Date/Time:'),
                    ('Filesize', 'File Size:'));

                    
implementation

procedure Register;
begin
  RegisterComponents('Leviathan', [TcaVersionLabel]);
end;

constructor TcaVersionLabel.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FInfoString := '';
    FFilename := '';
    FDateTimeFormat := ShortDateFormat;
    FFileSizeFormat := '#,#0" Bytes"';
    WordWrap := false;
    Autosize := true;
    ShowInfoPrefix := true;
    LangCharset :='-1';   {-1 = auto detect}
    VersionResource := vrFileVersion;
end;

destructor TcaVersionLabel.Destroy;
begin
    inherited Destroy;
end;

procedure TcaVersionLabel.SetVersionResource(Value: TVersionResources);
begin
    FVersionResource := Value;
    SetupResourceKey;
    SetupInfoPrefix;
end;

procedure TcaVersionLabel.SetFilename(Value: string);
begin
  FFilename := Value;
  SetupCaption;
end;

procedure TcaVersionLabel.SetDateTimeFormat(Value: String);
begin
  if Value = '' then exit;

  FDateTimeFormat := Value;
  SetupCaption;
end;

procedure TcaVersionLabel.SetFileSizeFormat(Value: String);
begin
  if Value = '' then exit;

  FFileSizeFormat := Value;
  SetupCaption;
end;

procedure TcaVersionLabel.SetupInfoPrefix;
var s: string;
begin
    s := VersionLookup[FVersionResource, 1];
    InfoPrefix := s;
end;

procedure TcaVersionLabel.SetupResourceKey;
var s: string;
begin
    s := VersionLookup[FVersionResource, 0];
    VersionResourceKey := s;
end;

function TcaVersionLabel.GetFixedFileInfo(Buffer: PChar; size: integer): string;
var
//  ValLen: integer;
  ValLen: UINT;
  FixedFileInfo: PVSFixedFileInfo;
begin
    if VerQueryValue(buffer, '\', Pointer(FixedFileInfo), ValLen) then
    begin
        Result := '';
        if (ValLen > 1) then
        begin
            if FixedFileInfo.dwFileFlags and VS_FF_DEBUG <> 0 then
                Result := Result+', Debug Build';
            if FixedFileInfo.dwFileFlags and VS_FF_PRERELEASE <> 0 then
                Result := Result+', Pre-Release Build';
            if FixedFileInfo.dwFileFlags and VS_FF_PATCHED <> 0 then
                Result := Result+', Patched Build';
            if FixedFileInfo.dwFileFlags and VS_FF_PRIVATEBUILD <> 0  then
                Result := Result+', Private Build';
            if FixedFileInfo.dwFileFlags and VS_FF_INFOINFERRED <> 0  then
                Result := Result+', InfoInferred';
            if FixedFileInfo.dwFileFlags and VS_FF_SPECIALBUILD <> 0  then
                Result := Result+', Special Build';
            if result <> '' then
                if Result[1] = ',' then Delete(Result, 1, 2);
        end;
    end
    else Result := '< Error retrieving fixed version info >';
end;

function TcaVersionLabel.GetStringFileInfo(Buffer: Pchar; size: integer): string;
//var vallen, Translen: integer;
var vallen, Translen: UINT;
    VersionPointer: pointer;
    TransBuffer: pointer;
    Major, Minor: integer;
begin
    if FLangCharSet = '-1' then
    begin
        VerQueryValue(buffer, '\VarFileInfo\Translation',
                        TransBuffer, TransLen);
        if TransLen >= 4 then
        begin
          FLangCharSet := CalcLangCharSet(TransBuffer, TransLen);
        end
        else
        begin
            Result := '< Error retrieving translation info >';
            exit;
        end;
    end;

    if VerQueryValue(buffer, pchar('\StringFileInfo\'+FLangCharSet+'\'+
                     VersionResourceKey),
                     VersionPointer, vallen) then
    begin
        if (Vallen > 1) then
        begin
            SetLength(Result, vallen);
            StrLCopy(Pchar(Result), VersionPointer, vallen);
            // special case for 'short' file versions
            if FVersionResource = vrShortFileVersion then
            begin
                try
                    Major := StrtoInt(Slice(Result, '.'));
                except
                    Major := 0;
                end;
                try
                    Minor := StrtoInt(Slice(Result, '.'));
                except
                    Minor := 0;
                end;
                Result := Format('%d.%.2d', [Major, Minor]);
            end;
        end
        else Result := '< No Version Info >';
    end
    else result := '< Error retrieving string version info >';
end;

function TcaVersionLabel.GetDateTimeInfo: String;
var
  DT: TDateTime;
  FileHandle: Longint;
  Filecheck: Array[0..255] of char;
begin

  Filecheck := '';

  if FFilename = '' then
    GetModuleFileName(HInstance, Filecheck, 255)
  else
    StrPCopy(Filecheck, FFilename);

  FileHandle := CreateFile(Filecheck, GENERIC_READ, FILE_SHARE_READ,
                           nil, OPEN_EXISTING, 0, 0);
  if FileHandle = -1 then
  begin
    result := '<Error getting file handle>';
    exit;
  end;

  DT := FileDateToDateTime(FileGetDate(FileHandle));
  CloseHandle(FileHandle);
  result := FOrmatDateTime(FDateTimeFormat, DT);
end;

function TcaVersionLabel.GetFileSizeInfo: String;
var
  fs: Longint;
  FileHandle: Longint;
  Filecheck: Array[0..255] of char;
begin

  Filecheck := '';

  if FFilename = '' then
    GetModuleFileName(HInstance, Filecheck, 255)
  else
    StrPCopy(Filecheck, FFilename);

  FileHandle := CreateFile(Filecheck, GENERIC_READ, FILE_SHARE_READ,
                           nil, OPEN_EXISTING, 0, 0);
  if FileHandle = -1 then
  begin
    result := '<Error getting file handle>';
    exit;
  end;

  fs := GetFileSize(FileHandle, nil);
  CloseHandle(FileHandle);
  result := FormatFloat(FFileSizeFormat, fs);
end;


// Looks at the translation structure and returns the hex formatted string
// containing the language and code page identifier
function TcaVersionLabel.CalcLangCharset(Buffer: pointer; Buflen: UINT): String;
begin
(*
Buffer points to the following structure? Not as far as I can tell it doesn't!
Oh well, go with what works...
WORD=16 bit value
WCHAR=WideChar
Var {
      WORD  wLength;
      WORD  wValueLength;
      WORD  wType;
      WCHAR szKey[];
      WORD  Padding[];
      DWORD Value[];
    };
*)

Result := Format('%4.4x%4.4x', [LoWord(UInt(Buffer^)), HiWord(UInt(Buffer^))]);

end;

{Called at run time to get version information}
function TcaVersionLabel.GetInfo: string;
//var dump, size: integer;
var dump : DWORD;
    size: integer;
    buffer: pchar;
    FileCheck: array[0..255] of char;
begin
    if csDesigning in Self.ComponentState then result := '< No design info >'
    else
    begin
        Filecheck := '';

        if FFilename = '' then
          GetModuleFileName(HInstance, Filecheck, 255)
        else
          StrPCopy(Filecheck, FFilename);
          
        size := GetFileVersionInfoSize(Filecheck, dump);
        if  size = 0 then
        begin
            Result := '< No Data Available >';
        end
        else
        begin
            buffer := StrAlloc(size+1);
            try
                if not GetFileVersionInfo(FileCheck, 0, size, buffer) then
                    result := '< Error retrieving version info >'
                else
                begin
                    case FVersionResource of
                        vrFlags: Result := GetFixedFileInfo(buffer, size);
                        vrDateTime: result := GetDateTimeInfo;
                        vrFileSize: result := GetFileSizeInfo;
                    else
                        Result := GetStringFileInfo(buffer, size);
                    end;
                end;
            finally
                StrDispose(Buffer);
            end;
        end;
    end;
    if ShowInfoPrefix then Result := InfoPrefix+' '+Result;
end;

procedure TcaVersionLabel.SetInfoPrefix(Value: String);
begin
    if FInfoPrefix = Value then exit;
    FInfoPrefix := Value;
    {The caption needs to be recalculated everytime the prefix is
    changed, otherwise the detaults override the user specified one}
    SetupCaption;
end;

procedure TcaVersionLabel.SetVersionResourceKey(Value: string);
begin
    if FVersionResourceKey = Value then exit;
    FVersionResourceKey := Value;
    InfoPrefix := Value;
end;

function TcaVersionLabel.GetInfoPrefix: string;
begin
    result := FInfoPrefix;
end;

procedure TcaVersionLabel.SetShowInfoPrefix(Value: boolean);
begin
    if FShowInfoPrefix = value then exit;
    FShowInfoPrefix := Value;
    SetupCaption;
end;

procedure TcaVersionLabel.SetLangCharset(Value: string);
begin
    if FLangCharSet = Value then exit;
    FLangCharSet := Value;
    SetupCaption;
end;

procedure TcaVersionLabel.SetupCaption;
begin
    Caption := GetInfo;
    FInfoString := Caption;
end;

// Find first section of string - remove from front of string and return it
function TcaVersionLabel.Slice(var S: string; Delimiter: string): string;
var p : Integer;
begin
  p := pos(Delimiter, S);
  if p = 0 then
  begin
    Result := S;
    S:='';
  end
  else
  begin
    Result := Copy(S,1,p-1);
    Delete(S,1,p+Length(Delimiter)-1);
  end;
end;

end.

