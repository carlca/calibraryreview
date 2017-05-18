unit caUtils;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Graphics,
  Math,
  FileCtrl,
  Masks,
  ActiveX,
  ComObj,
  TypInfo,
  Dialogs,
  Forms,
  StdCtrls,
  ExtCtrls,
  Registry,
  Clipbrd,
  DB,

  // ca units 
  caTypes,
  caConsts,
  caTimer,
  caClasses;

type

  EcaIEEEMath = class(EcaException);

  TcaFileSearchEvent = procedure(Sender: TObject; var Continue: Boolean) of object;

  //---------------------------------------------------------------------------
  // IcaUtils                                                                  
  //---------------------------------------------------------------------------

  IcaUtils = interface
    ['{45103F38-42ED-4B98-AE75-6F46A66ECE8B}']
    // Property methods 
    function GetOnContinueFileSearch: TcaFileSearchEvent;
    procedure SetOnContinueFileSearch(const Value: TcaFileSearchEvent);
    // Conversion methods 
    // To string 
    function ArrayToString(AList: array of string; Delimiter: string; Shrink: Boolean = False): string;
    function BooleanToString(ABoolean: Boolean): string;
    function DateTimeToString(ADateTime: TDateTime; const ADateFormat: string): string;
    function DoubleToString(ADouble: Double; const AFloatFormat: string): string;
    function ExtendedToString(AExtended: Extended; const AFloatFormat: string): string;
    function IntegerToString(AInteger: Integer; const AIntegerFormat: string): string;
    function Int64ToString(AInt64: Int64; const AIntegerFormat: string): string;
    function MemoToString(AMemo: TStrings): string;
    function ObjectToString(AObject: TObject): string;
    function SingleToString(ASingle: Single; const AFloatFormat: string): string;
    // From string 
    function StringToBoolean(const ABooleanStr: string): Boolean;
    function StringToDateTime(const ADateTimeStr: string): TDateTime;
    function StringToDouble(const ADoubleStr: string): Double;
    function StringToExtended(const AExtendedStr: string): Extended;
    function StringToInt64(const AInt64Str: string): Int64;
    function StringToInteger(const AIntegerStr: string): Integer;
    function StringToSingle(const ASingleStr: string): Single;
    function StrToFloat(const AFloatStr: string): Extended;
    procedure StringToMemo(const AMemoStr: string; AMemo: TStrings);
    // Other conversions 
    function ExtendedToDateTime(AExtended: Extended): TDateTime;
    function IntegerToDateTime(AInteger: Integer): TDateTime;
    function SingleToDateTime(ASingle: Single): TDateTime;
    function Str2Float(const S: string; Default: Extended): Extended;
    function Str2Int(const S: string; Default: Integer): Integer;
    function IntToShortMonth(AIndex: Integer): string;
    function IntToLongMonth(AIndex: Integer): string;
    function ShortMonthToInt(AShortMonth: string): Integer;
    function LongMonthToInt(ALongMonth: string): Integer;
    function StreamToString(AStream: TStream): string;
    // Comparison menthods 
    function CompareBooleans(const B1, B2: Boolean): TcaCompareResult;
    function CompareDateTimes(const D1, D2: TDateTime): TcaCompareResult;
    function CompareDoubles(const D1, D2: Double): TcaCompareResult;
    function CompareExtendeds(const E1, E2: Extended): TcaCompareResult;
    function CompareIntegers(const I1, I2: Integer): TcaCompareResult;
    function CompareInt64s(const I1, I2: Int64): TcaCompareResult;
    function CompareSingles(const S1, S2: Single): TcaCompareResult;
    function CompareStrings(const S1, S2: string; ACaseInsensitive: Boolean = False): TcaCompareResult;
    function OperatorToString(AOperator: TcaOperator): string;
    // Array comparison menthods 
    function DoubleArraysEqual(A1, A2: array of Double): Boolean;
    // string methods 
    function BuildString(Ch: Char; Count: Integer): string;
    function CopyUntilChar(var S: string; Ch: Char; DeleteAfter: Boolean = False): string;
    function DoubleQuoted(const S: string): string;
    function FastCharPos(const ASource: string; const C: Char; StartPos: Integer = 1): Integer;
    function FastPos(const ASourceString, AFindString: string; StartPos: Integer = 1; CaseSensitive: Boolean = False): Integer;
    function Format(const Format: string): string; overload;
    function Format(const Format: string; const Args: array of const): string; overload;
    function Format(const Format: string; const Args: array of const; AUseEscapes: Boolean): string; overload;
    function GetNextToken(var S: string; Delim: Char): string;
    function Hash(const S: string): Integer;
    function Indent(const S: string; N: Integer): string;
    function IsBlankOrZero(const S: string): Boolean;
    function IsInteger(const s: string): Boolean;
    function IsNumeric(const S: string): Boolean;
    function LastChar(const S: string): Char;
    function LeftStr(const S: string; N: Integer; ACase: TcaLetterCase = caAny): string;
    function PadInt(N: Integer; Ch: Char; Len: Integer): string;
    function PadLeft(const S: string; Ch: Char; Len: Integer): string;
    function PadRight(const S: string; Ch: Char; Len: Integer): string;
    function PadZero(N: Integer; Len: Integer): string;
    function PosFromEnd(const SubS, S: string): Integer;
    function PosFromStart(const SubS, S: string): Integer;
    function RightStr(const S: string; N: Integer; ACase: TcaLetterCase = caAny): string;
    function StartsWith(AString: string; SearchString: string; ACaseInsensitive: boolean = false): boolean;
    procedure ChangeCase(var S: string; ACase: TcaLetterCase);
    procedure CleanFilenameString(var S: string);
    procedure CleanIntString(var S: string);
    procedure CleanNumString(var S: string);
    procedure CleanString(var S: string);
    procedure DeleteFromChar(var S: string; Ch: Char; IncludeChar: Boolean; N: Integer = 0);
    procedure DeleteFromEnd(var S: string; N: Integer);
    procedure DeleteFromStart(var S: string; N: Integer);
    procedure DeleteUntilChar(var S: string; Ch: Char; IncludeChar: Boolean);
    procedure EnsureLastChar(var S: string; Ch: Char);
    procedure EnsureBackslash(var S: string);
    procedure MoveStr(const Source: string; SourcePos: Integer; var Dest: string; const DestPos, Count: Integer);
    procedure ReadStringFromBuffer(var S: string; var Buffer: PChar);
    procedure Replace(var S: string; const OldSub, NewSub: string; CaseSensitive: Boolean = False);
    procedure ReplaceChar(var S: string; const OldChar, NewChar: Char);
    procedure SplitCamelCaps(var S: string);
    procedure SortStrings(AStrings: TStrings);
    procedure StripChar(C: Char; var S: string);
    procedure StripLastCRLF(var S: string);
    procedure StripLeadingLowerCase(var S: string);
    procedure StripDoubleQuotes(var S: string);
    procedure WriteStringToBuffer(const S: string; var Buffer: PChar);
    // TStrings support 
    function MultiReplace(const S: string; AReplaceStrings: TStrings): string; overload;
    procedure MaskDelete(AStrings, AMasks: TStrings);
    procedure MultiReplace(AStrings, AReplaceStrings: TStrings); overload;
    // Record support 
    procedure FillRecordFalse(var Rec; Size: Integer);
    procedure FillRecordTrue(var Rec; Size: Integer);
    // File methods 
    function RemoveExt(const AFileName: string): string;
    function ExtractFileName(const FileName: string; IncludeExt: boolean = true): string;
    procedure GetFileList(const APath, AFileSpec: string; AFileList: TcaStringList; IncludeDotRefs: Boolean = False); overload;
    procedure GetFileList(const APath, AFileSpec: string; AFileList: IcaStringList; IncludeDotRefs: Boolean = False); overload;
    procedure GetFileList(const APath, AFileSpec: string; AFileList: TStrings; IncludeDotRefs: Boolean = False); overload;
    procedure SearchForFiles(const APath, AFileSpec: string; AOutList: TStrings); overload;
    procedure SearchForFiles(const APath, AFileSpec: string; AOutList: IcaStringList); overload;
    // Math evaluator methods 
    function GetOperatorPrecedence(const AOperator, AReferenceOperator: string): TcaOperatorPrecedence;
    function IsBracket(const S: string): Boolean;
    function IsFunction(const S: string): Boolean;
    function IsOperand(const S: string): Boolean;
    function IsOperator(const S: string): Boolean;
    // Graphics methods 
    function GetBitmapRect(ABitmap: TBitmap): TRect;
    function HTMLColorToColor(const AHTMLColor: string): TColor;
    // Font methods 
    function FontsEqual(AFont1, AFont2: TFont): Boolean;
    // TRect methods 
    function InRect(X, Y: Integer; Rect: TRect): Boolean; overload;
    function InRect(APoint: TPoint; Rect: TRect): Boolean; overload;
    procedure AdjustRect(var ARect: TRect; const dL, dT, dR, dB: Integer);
    procedure DecodeRect(const ARect: TRect; var ALeft, ATop, AWidth, AHeight: Integer);
    // Dialog methods 
    function QueryDeleteData(AHandle: HWND; const ADataSynonym: string = ''): TcaMsgDialogResponse;
    function QuerySaveData(AHandle: HWND): TcaMsgDialogResponse;
    function QueryCloseApp(AHandle: HWND; const ACloseSynonym: string = ''): TcaMsgDialogResponse;
    // System methods cautils 
    function AltKeyDown: Boolean;
    function ControlKeyDown: Boolean;
    function ShiftKeyDown: Boolean;
    function IsKeyDown(AKeyCode: Word): Boolean;
    function IsLargeFonts: Boolean;
    function EnableDebugPrivilege(const Enable: Boolean): Boolean;
    function FolderExists(const AFolder: string): Boolean;
    function GetComputerName: string;
    function GetEnvironmentVariable(const AName: string): string;
    function GetFileVersion(const AFileName: string; ABlockCount: Word = 4): string;
    function GetSystemMenuFont: TFont;
    function GetGUIDAsString: string;
    function GetUserName: string;
    function GetWindowClass(Wnd: HWND): string;
    function GetWindowProcess(Wnd: HWND): string;
    function GetWindowsFolder: string;
    function GetWindowsVersion: TcaWinVersion;
    function WinExecAndWait32(FileName: string; Visibility: Integer): Integer;
    procedure AppClose(AHandle: HWND);
    procedure AppMaximize(AHandle: HWND);
    procedure AppMinimize(AHandle: HWND);
    procedure AppMove(AHandle: HWND);
    procedure AppRestore(AHandle: HWND);
    procedure AppSize(AHandle: HWND);
    procedure AppToggleNormalAndMaximized(AHandle: HWND);
    procedure SendKey(AHandle: HWND; AKey: Word; const shift: TShiftState; IsSpecialKey: Boolean);
    procedure SetEnvironmentVariable(const AName, AValue: string; ABroadcast: Boolean = True);
    procedure SetWindowStayOnTop(AHandle: HWND; IsStayOnTop: Boolean);
    // Application path methods - works with EXEs and DLLs 
    function AppName: string;
    function AppPath: string;
    function AppPathAndName: string;
    function ConfigPath: string;
    function DataPath: string;
    function IsTestApp: Boolean;
    // Development/Debugging 
    function ApplicationIsDelphi: Boolean;
    function DelphiIsRunning: Boolean;
    // IEEE-754 Support for NANs and INFs 
    function NAN: Double;
    function PositiveInfinity: Double;
    function NegativeInfinity: Double;
    function IsNAN(const N: Double): Boolean;
    function IsInfinity(const N: Double): Boolean;
    function DoubleToHex(const N: Double): string;
    function HexToDouble(const Hex: string): Double;
    // Excel related methods 
    function A1Ref(C: Byte; R: Integer): string;
    function CRRef(const A1Ref: string): TcaColRow;
    // Component / TPersistent support 
    function CopyPublishedProperties(Src, Dest: TPersistent; CopyClassPointers, CopyChildren: Boolean): Boolean;
    function DeepCopy(Src, Dest: TPersistent): Boolean;
    function PersistentAsString(AObject: TPersistent): string;
    function PublishedPropertiesMatch(AItem1, AItem2: TPersistent; ACompareClassPointers: Boolean = False): Boolean;
    function ShallowCopy(Src, Dest: TPersistent; CopyClassPointers: Boolean = False): Boolean;
    // Design by Contract constructs 
    function FailsAll(AConditions: array of Boolean): Boolean;
    function FailsAny(AConditions: array of Boolean): Boolean;
    // Interface support 
    function GetImplementingObject(const Intf: IUnknown): TObject;
    // Method support 
    function MethodsMatch(AMethod1, AMethod2: TMethod): Boolean;
    // Time support methods 
    function CurrentYear: Word;
    function DateTimeToTimePoint(ADateTime: TDateTime): TcaTimePoint;
    function Now: TcaTimePoint;
    // Message dialog methods 
    function MessageDialog(const ACaption, AMsg: string; ADlgType: TMsgDlgType;
      AButtonCaptions: array of string; ADefaultIndex: Integer; AHelpCtx: Longint): TcaMessageDialogResponse;
    // Visual control support 
    procedure SetControlFont(AControl: TControl; AColor: TColor; ASize: Integer; AStyle: TFontStyles);
    // Events - FileSearch cancel 
    property OnContinueFileSearch: TcaFileSearchEvent read GetOnContinueFileSearch write SetOnContinueFileSearch;
    // Clipboard methods
    procedure CopyDatasetToClipboard(ADataset: TDataset);
  end;

  //---------------------------------------------------------------------------
  // IcaKeys                                                                   
  //---------------------------------------------------------------------------

  IcaKeys = interface
    ['{DD51F155-6119-411D-83CC-C439845CD351}']
    // Property methods 
    function GetAllKeys: TcaByteSet;
    function GetAlphaKeys: TcaByteSet;
    function GetControlKeys: TcaByteSet;
    function GetDecimalKeys: TcaByteSet;
    function GetHexKeys: TcaByteSet;
    function GetIntegerKeys: TcaByteSet;
    function GetNumericKeys: TcaByteSet;
    function GetVisibleKeys: TcaByteSet;
    // Properties 
    property AllKeys: TcaByteSet read GetAllKeys;
    property AlphaKeys: TcaByteSet read GetAlphaKeys;
    property ControlKeys: TcaByteSet read GetControlKeys;
    property DecimalKeys: TcaByteSet read GetDecimalKeys;
    property HexKeys: TcaByteSet read GetHexKeys;
    property IntegerKeys: TcaByteSet read GetIntegerKeys;
    property NumericKeys: TcaByteSet read GetNumericKeys;
    property VisibleKeys: TcaByteSet read GetVisibleKeys;
  end;

  //---------------------------------------------------------------------------
  // IcaMathUtils                                                              
  //---------------------------------------------------------------------------

  IcaMathUtils = interface
    ['{E2CA737A-80EC-42C4-98EC-35730C2C8E27}']
    // Property methods 
    function GetUseCheck8087: Boolean;
    function GetZeroTolerance: Extended;
    procedure SetUseCheck8087(const Value: Boolean);
    procedure SetZeroTolerance(const Value: Extended);
    // Public methods 
    function BankersRound(const X: Extended): Integer;
    function Equal(const N1, N2: Double): Boolean; overload;
    function Equal(const N1, N2: Extended): Boolean; overload;
    function Equal(const N1, N2: Single): Boolean; overload;
    function EquiRound(const X: Extended): Integer;
    function Exp(const N: Double): Double;
    function FloatDiv(const Numerator, Divisor, Default: Extended): Extended;
    function FloatPower(const ABase, AExponent: Extended): Extended;
    function IntPower(const ABase: Extended; AExponent: Integer): Extended;
    function RoundTo(const Value: Extended; Places: Integer): Extended;
    function SafeFrac(const X: Extended): Extended;
    function SafeRound(const X: Extended): Integer;
    function SafeStrToInt(const AStrVal: string): Integer;
    function SafeTrunc(const X: Extended): Integer;
    function Sign(const N: Double): Integer;
    function Sqr(X: Extended): Extended;
    function Trunc(X: Extended): Int64;
    function VBInt(X: Extended): Int64;
    // Comparisons 
    function IsGreaterThanZero(const AValue: Double): Boolean; overload;
    function IsGreaterThanZero(const AValue: Extended): Boolean; overload;
    function IsGreaterThanZero(const AValue: Single): Boolean; overload;
    function IsNonZero(const AValue: Double): Boolean; overload;
    function IsNonZero(const AValue: Extended): Boolean; overload;
    function IsNonZero(const AValue: Single): Boolean; overload;
    function IsZero(const AValue: Double): Boolean; overload;
    function IsZero(const AValue: Extended): Boolean; overload;
    function IsZero(const AValue: Single): Boolean; overload;
    // Is Equal to 
    function IsEq(const AValue1, AValue2: Double): Boolean; overload;
    function IsEq(const AValue1, AValue2: Extended): Boolean; overload;
    function IsEq(const AValue1, AValue2: Single): Boolean; overload;
    // Is Not Equal to 
    function IsNEq(const AValue1, AValue2: Double): Boolean; overload;
    function IsNEq(const AValue1, AValue2: Extended): Boolean; overload;
    function IsNEq(const AValue1, AValue2: Single): Boolean; overload;
    // Is Greater Than 
    function IsGT(const AValue1, AValue2: Double): Boolean; overload;
    function IsGT(const AValue1, AValue2: Extended): Boolean; overload;
    function IsGT(const AValue1, AValue2: Single): Boolean; overload;
    // Is Greater Than or Equal 
    function IsGTE(const AValue1, AValue2: Double): Boolean; overload;
    function IsGTE(const AValue1, AValue2: Extended): Boolean; overload;
    function IsGTE(const AValue1, AValue2: Single): Boolean; overload;
    // Is Less Than 
    function IsLT(const AValue1, AValue2: Double): Boolean; overload;
    function IsLT(const AValue1, AValue2: Extended): Boolean; overload;
    function IsLT(const AValue1, AValue2: Single): Boolean; overload;
    // Is Less Than or Equal 
    function IsLTE(const AValue1, AValue2: Double): Boolean; overload;
    function IsLTE(const AValue1, AValue2: Extended): Boolean; overload;
    function IsLTE(const AValue1, AValue2: Single): Boolean; overload;
    // Is Greater Than Zero 
    function IsGT0(const AValue: Double): Boolean; overload;
    function IsGT0(const AValue: Extended): Boolean; overload;
    function IsGT0(const AValue: Single): Boolean; overload;
    // Is Greater Than or Equal to Zero 
    function IsGTE0(const AValue: Double): Boolean; overload;
    function IsGTE0(const AValue: Extended): Boolean; overload;
    function IsGTE0(const AValue: Single): Boolean; overload;
    // Is Less Than Zero 
    function IsLT0(const AValue: Double): Boolean; overload;
    function IsLT0(const AValue: Extended): Boolean; overload;
    function IsLT0(const AValue: Single): Boolean; overload;
    // Is Less Than or Equal to Zero 
    function IsLTE0(const AValue: Double): Boolean; overload;
    function IsLTE0(const AValue: Extended): Boolean; overload;
    function IsLTE0(const AValue: Single): Boolean; overload;
    // Is Zero 
    function Is0(const AValue: Double): Boolean; overload;
    function Is0(const AValue: Extended): Boolean; overload;
    function Is0(const AValue: Single): Boolean; overload;
    // Is Non Zero 
    function IsN0(const AValue: Double): Boolean; overload;
    function IsN0(const AValue: Extended): Boolean; overload;
    function IsN0(const AValue: Single): Boolean; overload;
    // Is Multiple of 
    function IsMultipleOf(const AValue1, AValue2: Integer): Boolean; overload;
    function IsMultipleOf(const AValue1, AValue2: Double): Boolean; overload;
    function IsMultipleOf(const AValue1, AValue2: Extended): Boolean; overload;
    function IsMultipleOf(const AValue1, AValue2: Single): Boolean; overload;
    // Log methods 
    function Ln(const N: Double): Double;
    // Zero tolerance     
    procedure RestoreDefaultZeroTolerance;
    procedure RestoreSavedZeroTolerance;
    procedure SaveCurrentZeroTolerance;
    // Other math methods 
    function IsNAN(const N: Double): Boolean;
    function Stirling2(AElements, ASubsets: Integer): Double;
    // 8087 FPU Methods 
    function Get8087ControlWord: Word;
    function Valid8087ControlWord: Boolean;
    procedure Check8087;
    procedure Check8087ControlWord;
    procedure Fix8087ControlWord;
    procedure Clear8087StatusWord;
    // Properties 
    property UseCheck8087: Boolean read GetUseCheck8087 write SetUseCheck8087;
    property ZeroTolerance: Extended read GetZeroTolerance write SetZeroTolerance;
  end;

  //---------------------------------------------------------------------------
  // IcaFastMath                                                               
  //---------------------------------------------------------------------------

  {$IFDEF USE_FAST_MATH}

  IcaFastMath = interface
    ['{B97EF6A1-6030-4504-B36D-3D095A14AF95}']
    function Exp(N: Double): Double;
    function ExpVar(var N: Double): Double;
    function Ln(N: Double): Double;
    function FloatPower(const ABase, AExponent: Double): Double;
    function Sqrt(const N: Double): Double;
  end;

  {$ENDIF}

  //---------------------------------------------------------------------------
  // IcaParser                                                                 
  //---------------------------------------------------------------------------

  IcaParser = interface
    ['{3FA58161-458F-4490-95AF-271B7014DF8D}']
    // Property methods 
    function GetIgnoreBlanks: Boolean;
    function GetStringToParse: string;
    function GetTokenCount: Integer;
    function GetTokenDelimiters: string;
    procedure SetIgnoreBlanks(const Value: Boolean);
    procedure SetStringToParse(const Value: string);
    procedure SetTokenDelimiters(const Value: string);
    // Public methods 
    function GetToken(Index: Integer; FromEnd: Boolean = False): string;
    function HasMoreTokens: Boolean;
    function NextToken: string;
    procedure GetTokens(TokenList: TStrings); overload;
    procedure GetTokens(TokenList: IcaStringList); overload;
    procedure Initialize;
    // Properties 
    property IgnoreBlanks: Boolean read GetIgnoreBlanks write SetIgnoreBlanks;
    property StringToParse: string read GetStringToParse write SetStringToParse;
    property TokenCount: Integer read GetTokenCount;
    property TokenDelimiters: string read GetTokenDelimiters write SetTokenDelimiters;
  end;

  //---------------------------------------------------------------------------
  // IcaMouseUtils                                                             
  //---------------------------------------------------------------------------

  IcaMouseUtils = interface
    ['{E7873E94-AE51-4226-B5D9-3307896B9763}']
    // Property methods 
    function GetButtonDown: Boolean;
    function GetLeftButtonDown: Boolean;
    function GetMiddleButtonDown: Boolean;
    function GetMouseTrackerActive: Boolean;
    function GetRightButtonDown: Boolean;
    procedure SetMouseTrackerActive(const Value: Boolean);
    // Event property methods 
    function GetOnMouseDown: TMouseEvent;
    procedure SetOnMouseDown(const Value: TMouseEvent);
    // Public methods 
    procedure MouseStateChanged(AMouseDownButton: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // Properties 
    property LeftButtonDown: Boolean read GetLeftButtonDown;
    property MiddleButtonDown: Boolean read GetMiddleButtonDown;
    property MouseTrackerActive: Boolean read GetMouseTrackerActive write SetMouseTrackerActive;
    property RightButtonDown: Boolean read GetRightButtonDown;
    property ButtonDown: Boolean read GetButtonDown;
    // Event properties 
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
  end;

  //---------------------------------------------------------------------------
  // TcaUtils                                                                  
  //---------------------------------------------------------------------------

  TcaUtils = class(TInterfacedObject, IcaUtils,
      IcaKeys,
      IcaMathUtils,
      {$IFDEF USE_FAST_MATH}
      IcaFastMath,
      {$ENDIF}
      IcaParser,
      IcaMouseUtils)
  private
    // Private fields (IcaKeys) 
    FAllKeys: TcaByteSet;
    FAlphaKeys: TcaByteSet;
    FControlKeys: TcaByteSet;
    FDecimalKeys: TcaByteSet;
    FHexKeys: TcaByteSet;
    FIntegerKeys: TcaByteSet;
    FNumericKeys: TcaByteSet;
    FVisibleKeys: TcaByteSet;
    // Private fields (IcaMathUtils) 
    FSavedZeroTolerance: Extended;
    FUseCheck8087: Boolean;
    FZeroTolerance: Extended;
    // Private fields (IcaParser) 
    FIgnoreBlanks: Boolean;
    FStringToParse: string;
    FTokenDelimiters: string;
    FTokens: TStrings;
    FTokenIndex: Integer;
    // Event property fields 
    FOnContinueFileSearch: TcaFileSearchEvent;
    // IcaMouseUtils - Private fields 
    FMouseIsDown: Boolean;
    FMouseTrackerTimer: TcaTimer;
    FOnMouseDown: TMouseEvent;
    // Property methods (IcaKeys) 
    function GetAllKeys: TcaByteSet;
    function GetAlphaKeys: TcaByteSet;
    function GetControlKeys: TcaByteSet;
    function GetDecimalKeys: TcaByteSet;
    function GetHexKeys: TcaByteSet;
    function GetIntegerKeys: TcaByteSet;
    function GetNumericKeys: TcaByteSet;
    function GetVisibleKeys: TcaByteSet;
    // Private methods (IcaKeys) 
    procedure InitializeKeys;
    // Property methods (IcaMathUtils) 
    function GetUseCheck8087: Boolean;
    function GetZeroTolerance: Extended;
    procedure SetUseCheck8087(const Value: Boolean);
    procedure SetZeroTolerance(const Value: Extended);
    // Private methods (IcaMathUtils) 
    procedure CheckZeroTolerance;
    // Property methods (IcaParser) 
    function GetIgnoreBlanks: Boolean;
    function GetStringToParse: string;
    function GetTokenCount: Integer;
    function GetTokenDelimiters: string;
    procedure SetIgnoreBlanks(const Value: Boolean);
    procedure SetStringToParse(const Value: string);
    procedure SetTokenDelimiters(const Value: string);
    // Private methods (IcaParser) 
    procedure InitializeParser;
    // Private methods - Delphi replacements 
    function Pos(Substr: string; S: string): Integer;
    // Event property methods 
    function GetOnContinueFileSearch: TcaFileSearchEvent;
    procedure SetOnContinueFileSearch(const Value: TcaFileSearchEvent);
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Conversion methods 
    // To string 
    function ArrayToString(AList: array of string; Delimiter: string; Shrink: Boolean = False): string;
    function BooleanToString(ABoolean: Boolean): string;
    function DateTimeToString(ADateTime: TDateTime; const ADateFormat: string): string;
    function DoubleToString(ADouble: Double; const AFloatFormat: string): string;
    function ExtendedToString(AExtended: Extended; const AFloatFormat: string): string;
    function IntegerToString(AInteger: Integer; const AIntegerFormat: string): string;
    function Int64ToString(AInt64: Int64; const AIntegerFormat: string): string;
    function MemoToString(AMemo: TStrings): string;
    function ObjectToString(AObject: TObject): string;
    function SingleToString(ASingle: Single; const AFloatFormat: string): string;
    // From string 
    function StringToBoolean(const ABooleanStr: string): Boolean;
    function StringToDateTime(const ADateTimeStr: string): TDateTime;
    function StringToDouble(const ADoubleStr: string): Double;
    function StringToExtended(const AExtendedStr: string): Extended;
    function StringToInt64(const AInt64Str: string): Int64;
    function StringToInteger(const AIntegerStr: string): Integer;
    function StringToSingle(const ASingleStr: string): Single;
    function StrToFloat(const AFloatStr: string): Extended;
    procedure StringToMemo(const AMemoStr: string; AMemo: TStrings);
    // Other conversions 
    function ExtendedToDateTime(AExtended: Extended): TDateTime;
    function IntegerToDateTime(AInteger: Integer): TDateTime;
    function SingleToDateTime(ASingle: Single): TDateTime;
    function Str2Float(const S: string; Default: Extended): Extended;
    function Str2Int(const S: string; Default: Integer): Integer;
    function IntToShortMonth(AIndex: Integer): string;
    function IntToLongMonth(AIndex: Integer): string;
    function ShortMonthToInt(AShortMonth: string): Integer;
    function LongMonthToInt(ALongMonth: string): Integer;
    function StreamToString(AStream: TStream): string;
    // Comparison menthods 
    function CompareBooleans(const B1, B2: Boolean): TcaCompareResult;
    function CompareDateTimes(const D1, D2: TDateTime): TcaCompareResult;
    function CompareDoubles(const D1, D2: Double): TcaCompareResult;
    function CompareExtendeds(const E1, E2: Extended): TcaCompareResult;
    function CompareIntegers(const I1, I2: Integer): TcaCompareResult;
    function CompareInt64s(const I1, I2: Int64): TcaCompareResult;
    function CompareSingles(const S1, S2: Single): TcaCompareResult;
    function CompareStrings(const S1, S2: string; ACaseInsensitive: Boolean = False): TcaCompareResult;
    function OperatorToString(AOperator: TcaOperator): string;
    // Array comparison menthods 
    function DoubleArraysEqual(A1, A2: array of Double): Boolean;
    // string methods 
    function BuildString(Ch: Char; Count: Integer): string;
    function CopyUntilChar(var S: string; Ch: Char; DeleteAfter: Boolean = False): string;
    function DoubleQuoted(const S: string): string;
    function FastCharPos(const ASource: string; const C: Char; StartPos: Integer = 1): Integer;
    function FastPos(const ASourceString, AFindString: string; StartPos: Integer = 1; CaseSensitive: Boolean = False): Integer;
    function Format(const Format: string): string; overload;
    function Format(const Format: string; const Args: array of const): string; overload;
    function Format(const Format: string; const Args: array of const; AUseEscapes: Boolean): string; overload;
    function GetNextToken(var S: string; Delim: Char): string;
    function Hash(const S: string): Integer;
    function Indent(const S: string; N: Integer): string;
    function IsBlankOrZero(const S: string): Boolean;
    function IsInteger(const s: string): Boolean;
    function IsNumeric(const S: string): Boolean;
    function LastChar(const S: string): Char;
    function LeftStr(const S: string; N: Integer; ACase: TcaLetterCase = caAny): string;
    function PadInt(N: Integer; Ch: Char; Len: Integer): string;
    function PadLeft(const S: string; Ch: Char; Len: Integer): string;
    function PadRight(const S: string; Ch: Char; Len: Integer): string;
    function PadZero(N: Integer; Len: Integer): string;
    function PosFromStart(const SubS, S: string): Integer;
    function PosFromEnd(const SubS, S: string): Integer;
    function RightStr(const S: string; N: Integer; ACase: TcaLetterCase = caAny): string;
    function StartsWith(AString: string; SearchString: string; ACaseInsensitive: boolean = false): boolean;
    procedure ChangeCase(var S: string; ACase: TcaLetterCase);
    procedure CleanFilenameString(var S: string);
    procedure CleanIntString(var S: string);
    procedure CleanNumString(var S: string);
    procedure CleanString(var S: string);
    procedure DeleteFromStart(var S: string; N: Integer);
    procedure DeleteUntilChar(var S: string; Ch: Char; IncludeChar: Boolean);
    procedure DeleteFromChar(var S: string; Ch: Char; IncludeChar: Boolean; N: Integer = 0);
    procedure DeleteFromEnd(var S: string; N: Integer);
    procedure EnsureLastChar(var S: string; Ch: Char);
    procedure EnsureBackslash(var S: string);
    procedure MoveStr(const Source: string; SourcePos: Integer; var Dest: string; const DestPos, Count: Integer);
    procedure ReadStringFromBuffer(var S: string; var Buffer: PChar);
    procedure Replace(var S: string; const OldSub, NewSub: string; CaseSensitive: Boolean = False);
    procedure ReplaceChar(var S: string; const OldChar, NewChar: Char);
    procedure SplitCamelCaps(var S: string);
    procedure SortStrings(AStrings: TStrings);
    procedure StripChar(C: Char; var S: string);
    procedure StripLastCRLF(var S: string);
    procedure StripLeadingLowerCase(var S: string);
    procedure StripDoubleQuotes(var S: string);
    procedure WriteStringToBuffer(const S: string; var Buffer: PChar);
    // TStrings support 
    function MultiReplace(const S: string; AReplaceStrings: TStrings): string; overload;
    procedure MaskDelete(AStrings, AMasks: TStrings);
    procedure MultiReplace(AStrings, AReplaceStrings: TStrings); overload;
    // Record support 
    procedure FillRecordFalse(var Rec; Size: Integer);
    procedure FillRecordTrue(var Rec; Size: Integer);
    // File methods 
    function RemoveExt(const AFileName: string): string;
    function ExtractFileName(const FileName: string; IncludeExt: boolean = true): string;
    procedure GetFileList(const APath, AFileSpec: string; AFileList: TcaStringList; IncludeDotRefs: Boolean = False); overload;
    procedure GetFileList(const APath, AFileSpec: string; AFileList: IcaStringList; IncludeDotRefs: Boolean = False); overload;
    procedure GetFileList(const APath, AFileSpec: string; AFileList: TStrings; IncludeDotRefs: Boolean = False); overload;
    procedure SearchForFiles(const APath, AFileSpec: string; AOutList: TStrings); overload;
    procedure SearchForFiles(const APath, AFileSpec: string; AOutList: IcaStringList); overload;
    // IcaMathUtils 
    function BankersRound(const X: Extended): Integer;
    function Equal(const N1, N2: Double): Boolean; overload;
    function Equal(const N1, N2: Extended): Boolean; overload;
    function Equal(const N1, N2: Single): Boolean; overload;
    function EquiRound(const X: Extended): Integer;
    function Exp(const N: Double): Double;
    function FloatDiv(const Numerator, Divisor, Default: Extended): Extended;
    function FloatPower(const ABase, AExponent: Extended): Extended;
    function RoundTo(const Value: Extended; Places: Integer): Extended;
    function IntPower(const ABase: Extended; AExponent: Integer): Extended;
    function SafeFrac(const X: Extended): Extended;
    function SafeRound(const X: Extended): Integer;
    function SafeStrToInt(const AStrVal: string): Integer;
    function SafeTrunc(const X: Extended): Integer;
    function Sign(const N: Double): Integer;
    function Sqr(X: Extended): Extended;
    function Trunc(X: Extended): Int64;
    function VBInt(X: Extended): Int64;
    // Comparisons 
    function IsGreaterThanZero(const AValue: Double): Boolean; overload;
    function IsGreaterThanZero(const AValue: Extended): Boolean; overload;
    function IsGreaterThanZero(const AValue: Single): Boolean; overload;
    function IsNonZero(const AValue: Double): Boolean; overload;
    function IsNonZero(const AValue: Extended): Boolean; overload;
    function IsNonZero(const AValue: Single): Boolean; overload;
    function IsZero(const AValue: Double): Boolean; overload;
    function IsZero(const AValue: Extended): Boolean; overload;
    function IsZero(const AValue: Single): Boolean; overload;
    // Is Equal to 
    function IsEq(const AValue1, AValue2: Double): Boolean; overload;
    function IsEq(const AValue1, AValue2: Extended): Boolean; overload;
    function IsEq(const AValue1, AValue2: Single): Boolean; overload;
    // Is Equal to 
    function IsNEq(const AValue1, AValue2: Double): Boolean; overload;
    function IsNEq(const AValue1, AValue2: Extended): Boolean; overload;
    function IsNEq(const AValue1, AValue2: Single): Boolean; overload;
    // Is Greater Than 
    function IsGT(const AValue1, AValue2: Double): Boolean; overload;
    function IsGT(const AValue1, AValue2: Extended): Boolean; overload;
    function IsGT(const AValue1, AValue2: Single): Boolean; overload;
    // Is Greater Than or Equal 
    function IsGTE(const AValue1, AValue2: Double): Boolean; overload;
    function IsGTE(const AValue1, AValue2: Extended): Boolean; overload;
    function IsGTE(const AValue1, AValue2: Single): Boolean; overload;
    // Is Less Than 
    function IsLT(const AValue1, AValue2: Double): Boolean; overload;
    function IsLT(const AValue1, AValue2: Extended): Boolean; overload;
    function IsLT(const AValue1, AValue2: Single): Boolean; overload;
    // Is Less Than or Equal 
    function IsLTE(const AValue1, AValue2: Double): Boolean; overload;
    function IsLTE(const AValue1, AValue2: Extended): Boolean; overload;
    function IsLTE(const AValue1, AValue2: Single): Boolean; overload;
    // Is Greater Than Zero 
    function IsGT0(const AValue: Double): Boolean; overload;
    function IsGT0(const AValue: Extended): Boolean; overload;
    function IsGT0(const AValue: Single): Boolean; overload;
    // Is Greater Than or Equal to Zero 
    function IsGTE0(const AValue: Double): Boolean; overload;
    function IsGTE0(const AValue: Extended): Boolean; overload;
    function IsGTE0(const AValue: Single): Boolean; overload;
    // Is Less Than Zero 
    function IsLT0(const AValue: Double): Boolean; overload;
    function IsLT0(const AValue: Extended): Boolean; overload;
    function IsLT0(const AValue: Single): Boolean; overload;
    // Is Less Than or Equal to Zero 
    function IsLTE0(const AValue: Double): Boolean; overload;
    function IsLTE0(const AValue: Extended): Boolean; overload;
    function IsLTE0(const AValue: Single): Boolean; overload;
    // Is Zero 
    function Is0(const AValue: Double): Boolean; overload;
    function Is0(const AValue: Extended): Boolean; overload;
    function Is0(const AValue: Single): Boolean; overload;
    // Is Non Zero 
    function IsN0(const AValue: Double): Boolean; overload;
    function IsN0(const AValue: Extended): Boolean; overload;
    function IsN0(const AValue: Single): Boolean; overload;
    // Is Multiple of 
    function IsMultipleOf(const AValue1, AValue2: Integer): Boolean; overload;
    function IsMultipleOf(const AValue1, AValue2: Double): Boolean; overload;
    function IsMultipleOf(const AValue1, AValue2: Extended): Boolean; overload;
    function IsMultipleOf(const AValue1, AValue2: Single): Boolean; overload;
    // Log methods 
    function Ln(const N: Double): Double;
    // Zero tolerance 
    procedure RestoreDefaultZeroTolerance;
    procedure RestoreSavedZeroTolerance;
    procedure SaveCurrentZeroTolerance;
    // IcaFastMath 
    {$IFDEF USE_FAST_MATH}
    // Method resolution clause 
    function IcaFastMath.Exp = FastExp;
    function IcaFastMath.ExpVar = FastExpVar;
    function IcaFastMath.Ln = FastLn;
    function IcaFastMath.FloatPower = FastFloatPower;
    function IcaFastMath.Sqrt = FastSqrt;
    // Methods 
    function FastExp(X: Double): Double;
    function FastExpVar(var N: Double): Double;
    function FastLn(X: Double): Double;
    function FastFloatPower(const ABase, AExponent: Double): Double;
    function FastSqrt(const N: Double): Double;
    {$ENDIF}
    // Other math methods 
    function Stirling2(AElements, ASubsets: Integer): Double;
    // 8087 FPU Methods 
    function Get8087ControlWord: Word;
    function Valid8087ControlWord: Boolean;
    procedure Check8087;
    procedure Check8087ControlWord;
    procedure Fix8087ControlWord;
    procedure Clear8087StatusWord;
    // Math evaluator methods 
    function GetOperatorPrecedence(const AOperator, AReferenceOperator: string): TcaOperatorPrecedence;
    function IsBracket(const S: string): Boolean;
    function IsFunction(const S: string): Boolean;
    function IsOperand(const S: string): Boolean;
    function IsOperator(const S: string): Boolean;
    // Graphics methods 
    function GetBitmapRect(ABitmap: TBitmap): TRect;
    function HTMLColorToColor(const AHTMLColor: string): TColor;
    // Font methods 
    function FontsEqual(AFont1, AFont2: TFont): Boolean;
    // TRect methods 
    function InRect(X, Y: Integer; Rect: TRect): Boolean; overload;
    function InRect(APoint: TPoint; Rect: TRect): Boolean; overload;
    procedure AdjustRect(var ARect: TRect; const dL, dT, dR, dB: Integer);
    procedure DecodeRect(const ARect: TRect; var ALeft, ATop, AWidth, AHeight: Integer);
    // Dialog methods 
    function QueryDeleteData(AHandle: HWND; const ADataSynonym: string = ''): TcaMsgDialogResponse;
    function QuerySaveData(AHandle: HWND): TcaMsgDialogResponse;
    function QueryCloseApp(AHandle: HWND; const ACloseSynonym: string = ''): TcaMsgDialogResponse;
    // System methods 
    function AltKeyDown: Boolean;
    function ControlKeyDown: Boolean;
    function ShiftKeyDown: Boolean;
    function IsKeyDown(AKeyCode: Word): Boolean;
    function IsLargeFonts: Boolean;
    function EnableDebugPrivilege(const Enable: Boolean): Boolean;
    function FolderExists(const AFolder: string): Boolean;
    function GetComputerName: string;
    function GetWindowClass(Wnd: HWND): string;
    function GetWindowProcess(Wnd: HWND): string;
    function GetWindowsFolder: string;
    function GetWindowsVersion: TcaWinVersion;
    function GetEnvironmentVariable(const AName: string): string;
    function GetFileVersion(const AFileName: string; ABlockCount: Word = 4): string;
    function GetSystemMenuFont: TFont;
    function GetGUIDAsString: string;
    function GetUserName: string;
    function WinExecAndWait32(FileName: string; Visibility: Integer): Integer;
    procedure AppClose(AHandle: HWND);
    procedure AppMaximize(AHandle: HWND);
    procedure AppMinimize(AHandle: HWND);
    procedure AppMove(AHandle: HWND);
    procedure AppRestore(AHandle: HWND);
    procedure AppSize(AHandle: HWND);
    procedure AppToggleNormalAndMaximized(AHandle: HWND);
    procedure SendKey(AHandle: HWND; AKey: Word; const shift: TShiftState; IsSpecialKey: Boolean);
    procedure SetEnvironmentVariable(const AName, AValue: string; ABroadcast: Boolean = True);
    procedure SetWindowStayOnTop(AHandle: HWND; IsStayOnTop: Boolean);
    // Application path methods - works with EXEs and DLLs 
    function AppName: string;
    function AppPath: string;
    function AppPathAndName: string;
    function ConfigPath: string;
    function DataPath: string;
    function IsTestApp: Boolean;
    // Development/Debugging 
    function ApplicationIsDelphi: Boolean;
    function DelphiIsRunning: Boolean;
    // IEEE-754 Support for NANs and INFs 
    function DoubleToHex(const N: Double): string;
    function HexToDouble(const Hex: string): Double;
    function IsInfinity(const N: Double): Boolean;
    function IsNAN(const N: Double): Boolean;
    function NAN: Double;
    function NegativeInfinity: Double;
    function PositiveInfinity: Double;
    // Excel related methods 
    function A1Ref(C: Byte; R: Integer): string;
    function CRRef(const A1Ref: string): TcaColRow;
    // Component / TPersistent support 
    function CopyPublishedProperties(Src, Dest: TPersistent; CopyClassPointers, CopyChildren: Boolean): Boolean;
    function DeepCopy(Src, Dest: TPersistent): Boolean;
    function PersistentAsString(AObject: TPersistent): string;
    function PublishedPropertiesMatch(AItem1, AItem2: TPersistent; ACompareClassPointers: Boolean = False): Boolean;
    function ShallowCopy(Src, Dest: TPersistent; CopyClassPointers: Boolean = False): Boolean;
    // Design by Contract constructs 
    function FailsAll(AConditions: array of Boolean): Boolean;
    function FailsAny(AConditions: array of Boolean): Boolean;
    // Interface support 
    function GetImplementingObject(const Intf: IUnknown): TObject;
    // Method support 
    function MethodsMatch(AMethod1, AMethod2: TMethod): Boolean;
    // Time support methods 
    function CurrentYear: Word;
    function DateTimeToTimePoint(ADateTime: TDateTime): TcaTimePoint;
    function Now: TcaTimePoint;
    // Message dialog methods 
    function MessageDialog(const ACaption, AMsg: string; ADlgType: TMsgDlgType;
      AButtonCaptions: array of string; ADefaultIndex: Integer; AHelpCtx: Longint): TcaMessageDialogResponse;
    // Visual control support 
    procedure SetControlFont(AControl: TControl; AColor: TColor; ASize: Integer; AStyle: TFontStyles);
    // Public methods (IcaParser) 
    function GetToken(Index: Integer; FromEnd: Boolean = False): string;
    function HasMoreTokens: Boolean;
    function NextToken: string;
    procedure GetTokens(TokenList: TStrings); overload;
    procedure GetTokens(TokenList: IcaStringList); overload;
    procedure IcaParser.Initialize = InitializeParser;
    // Properties (IcaKeys) 
    property AllKeys: TcaByteSet read GetAllKeys;
    property AlphaKeys: TcaByteSet read GetAlphaKeys;
    property ControlKeys: TcaByteSet read GetControlKeys;
    property DecimalKeys: TcaByteSet read GetDecimalKeys;
    property HexKeys: TcaByteSet read GetHexKeys;
    property IntegerKeys: TcaByteSet read GetIntegerKeys;
    property NumericKeys: TcaByteSet read GetNumericKeys;
    property VisibleKeys: TcaByteSet read GetVisibleKeys;
    // Properties (IcaMathUtils) 
    property UseCheck8087: Boolean read GetUseCheck8087 write SetUseCheck8087;
    property ZeroTolerance: Extended read GetZeroTolerance write SetZeroTolerance;
    // Properties (IcaParser) 
    property IgnoreBlanks: Boolean read GetIgnoreBlanks write SetIgnoreBlanks;
    property StringToParse: string read GetStringToParse write SetStringToParse;
    property TokenDelimiters: string read GetTokenDelimiters write SetTokenDelimiters;
    // Events - FileSearch cancel 
    property OnContinueFileSearch: TcaFileSearchEvent read GetOnContinueFileSearch write SetOnContinueFileSearch;
    // Clipboard methods
    procedure CopyDatasetToClipboard(ADataset: TDataset);
    //---------------
    // IcaMouseUtils 
    //---------------
    // Public methods 
    procedure MouseStateChanged(AMouseDownButton: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseTrackerTimerEvent(Sender: TObject);
    // Property methods 
    function GetButtonDown: Boolean;
    function GetLeftButtonDown: Boolean;
    function GetMiddleButtonDown: Boolean;
    function GetMouseTrackerActive: Boolean;
    function GetRightButtonDown: Boolean;
    procedure SetMouseTrackerActive(const Value: Boolean);
    // Event property methods 
    function GetOnMouseDown: TMouseEvent;
    procedure SetOnMouseDown(const Value: TMouseEvent);
    // Properties 
    property ButtonDown: Boolean read GetButtonDown;
    property LeftButtonDown: Boolean read GetLeftButtonDown;
    property MiddleButtonDown: Boolean read GetMiddleButtonDown;
    property MouseTrackerActive: Boolean read GetMouseTrackerActive write SetMouseTrackerActive;
    property RightButtonDown: Boolean read GetRightButtonDown;
    // Event properties 
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
  end;

  //---------------------------------------------------------------------------
  // CoUtilsFactory                                                            
  //---------------------------------------------------------------------------

  CoUtilsFactory = class
  public
    class function Instance: IcaUtils;
  end;

var
  Utils: IcaUtils = nil;

procedure Pass;

implementation

uses
  caLog,
  caProcesses,
  caFastStrings,
  caFastFuncs,
  caRTTI;

procedure Pass;
begin
end;

  //---------------------------------------------------------------------------
  // TcaUtils                                                                  
  //---------------------------------------------------------------------------

constructor TcaUtils.Create;
begin
  inherited;
  Check8087;
  FTokens := TStringList.Create;
  InitializeParser;
  InitializeKeys;
  CheckZeroTolerance;
  FMouseTrackerTimer := TcaTimer.Create(nil);
  FMouseTrackerTimer.OnTimer := MouseTrackerTimerEvent;
end;

destructor TcaUtils.Destroy;
begin
  FTokens.Free;
  FMouseTrackerTimer.Free;
  inherited;
end;

  //---------------------------------------------------------------------------
  // Conversion methods                                                        
  //---------------------------------------------------------------------------

  // To string 

function TcaUtils.ArrayToString(AList: array of string; Delimiter: string; Shrink: Boolean = False): string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Length(AList) -1 do
    begin
      if Shrink then
      begin
        if (AList[Index] <> '') then
          begin
            if Result <> '' then
              Result := Result + Delimiter;
            Result := Result + AList[Index];
          end
      end
      else
        begin
          if Index <> 0 then
            Result := Result + Delimiter;
          Result := Result + AList[Index];
        end;
    end;
end;

function TcaUtils.BooleanToString(ABoolean: Boolean): string;
begin
  if ABoolean then Result := 'True' else Result := 'False';
end;

function TcaUtils.DateTimeToString(ADateTime: TDateTime; const ADateFormat: string): string;
begin
  if ADateFormat <> '' then
  begin
    if ADateFormat = ' ' then
      Result := ''
    else
      Result := FormatDateTime(ADateFormat, ADateTime)
  end
  else
    Result := FormatDateTime('c', ADateTime);
end;

function TcaUtils.DoubleToString(ADouble: Double; const AFloatFormat: string): string;
begin
  if AFloatFormat <> '' then
    begin
      if AFloatFormat = ' ' then
        Result := ''
      else
        Result := FormatFloat(AFloatFormat, ADouble);
    end
  else
    Result := FloatToStr(ADouble);
end;

function TcaUtils.ExtendedToString(AExtended: Extended; const AFloatFormat: string): string;
begin
  if AFloatFormat <> '' then
    begin
      if AFloatFormat = ' ' then
        Result := ''
      else
        Result := FormatFloat(AFloatFormat, AExtended);
    end
  else
    Result := FloatToStr(AExtended);
end;

function TcaUtils.Int64ToString(AInt64: Int64; const AIntegerFormat: string): string;
begin
  if AIntegerFormat <> '' then
    Result := FormatFloat(AIntegerFormat, AInt64)
  else
    Result := IntToStr(AInt64);
end;

function TcaUtils.IntegerToString(AInteger: Integer; const AIntegerFormat: string): string;
begin
  if AIntegerFormat <> '' then
    Result := FormatFloat(AIntegerFormat, AInteger)
  else
    Result := IntToStr(AInteger);
end;

function TcaUtils.MemoToString(AMemo: TStrings): string;
begin
  Result := AMemo.Text;
end;

function TcaUtils.ObjectToString(AObject: TObject): string;
var
  AName: string;
  AHexValue: string;
begin
  AName := AObject.ClassName;
  AHexValue := Format('%.8x', [LongWord(AObject)]);
  Result := AName + ': ' + AHexValue;
end;

function TcaUtils.SingleToString(ASingle: Single; const AFloatFormat: string): string;
begin
  if AFloatFormat <> '' then
    Result := FormatFloat(AFloatFormat, ASingle)
  else
    Result := FloatToStr(ASingle);
end;

  // From string 

function TcaUtils.StringToBoolean(const ABooleanStr: string): Boolean;
begin
  Result := (Length(ABooleanStr) > 0) and (ABooleanStr[1] in ['T', 't', 'Y', 'y']);
end;

function TcaUtils.StringToDateTime(const ADateTimeStr: string): TDateTime;
begin
  Result := SysUtils.StrToDate(ADateTimeStr);
end;

function TcaUtils.StringToDouble(const ADoubleStr: string): Double;
begin
  Result := Str2Float(ADoubleStr, 0);
end;

function TcaUtils.StringToExtended(const AExtendedStr: string): Extended;
begin
  Result := Str2Float(AExtendedStr, 0);
end;

function TcaUtils.StringToInt64(const AInt64Str: string): Int64;
begin
  Result := SysUtils.StrToInt64(AInt64Str);
end;

function TcaUtils.StringToInteger(const AIntegerStr: string): Integer;
begin
  Result := Str2Int(AIntegerStr, 0);
end;

function TcaUtils.StringToSingle(const ASingleStr: string): Single;
begin
  Result := Str2Float(ASingleStr, 0);
end;

function TcaUtils.StrToFloat(const AFloatStr: string): Extended;
begin
  Result := StringToExtended(AFloatStr);
end;

procedure TcaUtils.StringToMemo(const AMemoStr: string; AMemo: TStrings);
begin
  AMemo.Text := AMemoStr;
end;

  // Other conversion 

function TcaUtils.ExtendedToDateTime(AExtended: Extended): TDateTime;
var
  ADouble: Double;
begin
  ADouble := AExtended;
  Result := ADouble;
end;

function TcaUtils.IntegerToDateTime(AInteger: Integer): TDateTime;
var
  ADouble: Double;
begin
  ADouble := AInteger;
  Result := ADouble;
end;

function TcaUtils.SingleToDateTime(ASingle: Single): TDateTime;
var
  ADouble: Double;
begin
  ADouble := ASingle;
  Result := ADouble;
end;

function TcaUtils.Str2Float(const S: string; Default: Extended): Extended;
var
  S2: string;
begin
  S2 := S;
  CleanNumString(S2);
  if not TextToFloat(PChar(S2 + #0), Result, fvExtended) then
    Result := Default;
end;

function TcaUtils.Str2Int(const S: string; Default: Integer): Integer;
var
  S2: string;
begin
  S2 := S;
  CleanIntString(S2);
  Result := StrToIntDef(S2, Default);
end;

function TcaUtils.IntToLongMonth(AIndex: Integer): string;
begin
  Result := '';
  if AIndex in [1..12] then
    Result := LongMonthNames[AIndex];
end;

function TcaUtils.IntToShortMonth(AIndex: Integer): string;
begin
  Result := '';
  if AIndex in [1..12] then
    Result := ShortMonthNames[AIndex];
end;

function TcaUtils.LongMonthToInt(ALongMonth: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 1 to 12 do
    if LongMonthNames[Index] = ALongMonth then
      begin
        Result := Index;
        Break;
      end;
end;

function TcaUtils.ShortMonthToInt(AShortMonth: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 1 to 12 do
    if ShortMonthNames[Index] = AShortMonth then
      begin
        Result := Index;
        Break;
      end;
end;

function TcaUtils.StreamToString(AStream: TStream): string;
var
  StrStream: TStringStream;
begin
  StrStream := TStringStream.Create('');
  try
    StrStream.CopyFrom(AStream, 0);
    StrStream.Position := 0;
    Result := StrStream.DataString;
  finally
    StrStream.Free;
  end;
end;

  //---------------------------------------------------------------------------
  // string methods                                                            
  //---------------------------------------------------------------------------

function TcaUtils.BuildString(Ch: Char; Count: Integer): string;
begin
  if Count > 0 then
    Result := StringOfChar(Ch, Count)
  else
    Result := '';
end;

function TcaUtils.CopyUntilChar(var S: string; Ch: Char; DeleteAfter: Boolean = False): string;
var
  Index: Integer;
begin
  Result := '';
  if Self.Pos(Ch, S) > 0 then
    begin
      for Index := 1 to Length(S) do
        begin
          if S[Index] = Ch then
            Break;
          Result := Result + S[Index];
        end;
      if DeleteAfter then
        DeleteUntilChar(S, Ch, True);
    end;
end;

function TcaUtils.DoubleQuoted(const S: string): string;
begin
  Result := #34 + S + #34;
end;

function TcaUtils.FastCharPos(const ASource: string; const C: Char; StartPos: Integer = 1): Integer;
begin
  Result := caFastStrings.FastCharPos(ASource, C, StartPos);
end;

function TcaUtils.FastPos(const ASourceString, AFindString: string; StartPos: Integer = 1; CaseSensitive: Boolean = False): Integer;
begin
  if CaseSensitive then
    Result := caFastStrings.FastPos(ASourceString, AFindString, Length(ASourceString), Length(AFindString), StartPos)
  else
    Result := caFastStrings.FastPosNoCase(ASourceString, AFindString, Length(ASourceString), Length(AFindString), StartPos);
end;

function TcaUtils.Format(const Format: string): string;
begin
  Result := Self.Format(Format, [], True);
end;

function TcaUtils.Format(const Format: string; const Args: array of const): string;
begin
  Result := Self.Format(Format, Args, True);
end;

function TcaUtils.Format(const Format: string; const Args: array of const; AUseEscapes: Boolean): string;
var
  S: string;
begin
  S := Format;
  if AUseEscapes then
    begin
      Replace(S, '<lf>', '' + #10);
      Replace(S, '<cr>', '' + #13);
      Replace(S, '<tab>', '' + #9);
      Replace(S, '\\', '\');
    end;
  Result := Sysutils.Format(S, Args);
end;

function TcaUtils.GetNextToken(var S: string; Delim: Char): string;
var
  P: Integer;
begin
  P := Self.Pos(';', S);
  if P > 0 then
    begin
      Result := Copy(S, 1, P - 1);
      System.Delete(S, 1, P);
    end
  else
    Result := S;
end;

function TcaUtils.Hash(const S: string): Integer;
var
  Bytes: TByteArray absolute S;
  BufSize: Integer;
  I, X: Integer;
begin
  Result := 0;
  BufSize := Length(S);
  for I := 0 to BufSize - 1 do
    begin
      Result := (Result shl 4) + Bytes[I];
      X := Result and $F0000000;
      if (X <> 0) then
        Result := Result xor (X shr 24);
      Result := Result and (not X);
    end;
end;

function TcaUtils.Indent(const S: string; N: Integer): string;
begin
  Result := BuildString(#32, N) + S;
end;

function TcaUtils.IsBlankOrZero(const S: string): Boolean;
begin
  Result := (S = '') or (S = '0');
end;

function TcaUtils.IsInteger(const s: string): Boolean;
var
  Index: Integer;
  NumChars: string;
begin
  Result := S <> '';
  NumChars := '0123456789';
  for Index := 1 to Length(S) do
    if Self.Pos(S[Index], NumChars) = 0 then
      begin
        Result := False;
        Break;
      end;
end;

function TcaUtils.IsNumeric(const S: string): Boolean;
var
  Index: Integer;
  NumChars: string;
begin
  Result := S <> '';
  NumChars := '0123456789' + DecimalSeparator + ThousandSeparator;
  for Index := 1 to Length(S) do
    if Self.Pos(S[Index], NumChars) = 0 then
      begin
        Result := False;
        Break;
      end;
end;

function TcaUtils.LastChar(const S: string): Char;
begin
  if Length(S) > 0 then
    Result := S[Length(S)]
  else
    Result := #0;
end;

function TcaUtils.LeftStr(const S: string; N: Integer; ACase: TcaLetterCase = caAny): string;
begin
  Result := Copy(S, 1, N);
  ChangeCase(Result, ACase);
end;

function TcaUtils.PadInt(N: Integer; Ch: Char; Len: Integer): string;
var
  S: string;
begin
  S := IntToStr(N);
  Result := PadLeft(S, Ch, Len);
end;

function TcaUtils.PadLeft(const S: string; Ch: Char; Len: Integer): string;
var
  N: Integer;
begin
  N := Length(S);
  if N < Len then
    Result := BuildString(Ch, Len - N) + S
  else
    Result := S;
end;

function TcaUtils.PadRight(const S: string; Ch: Char; Len: Integer): string;
var
  N: Integer;
begin
  N := Length(S);
  if N < Len then
    Result := S + BuildString(Ch, Len - N)
  else
    Result := S;
end;

function TcaUtils.PadZero(N: Integer; Len: Integer): string;
begin
  Result := PadInt(N, '0', Len);
end;

function TcaUtils.PosFromStart(const SubS, S: string): Integer;
begin
  Result := Self.Pos(SubS, S);
end;

function TcaUtils.PosFromEnd(const SubS, S: string): Integer;
var
  Index: Integer;
  S2: string;
  SubPos: Integer;
begin
  Result := 0;
  for Index := Length(S) downto 1 do
    begin
      S2 := S;
      Delete(S2, 1, Index);
      SubPos := Pos(SubS, S2);
      if SubPos > 0 then
        begin
          Result := Index + 1;
          Break;
        end;
    end;
end;

function TcaUtils.RightStr(const S: string; N: Integer; ACase: TcaLetterCase = caAny): string;
begin
  Result := '';
  if S <> '' then
    begin
      Result := Copy(S, Length(S) - N + 1, N);
      ChangeCase(Result, ACase);
    end;
end;

function TcaUtils.StartsWith(AString: string; SearchString: string; ACaseInsensitive: boolean = false): boolean;
var
  Str1: string;
  Str2: string;
begin
  Str1 := LeftStr(AString, Length(SearchString));
  Str2 := SearchString;
  Result := CompareStrings(Str1, Str2, ACaseInsensitive) = crEqual;
end;

procedure TcaUtils.ChangeCase(var S: string; ACase: TcaLetterCase);
begin
  case ACase of
    caLower: S := LowerCase(S);
    caUpper: S := UpperCase(S);
    caAny: ;
  end;
end;

procedure TcaUtils.CleanFilenameString(var S: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(S) do
    if S[Index] in [#0..#31, '/', '*', '?', '"', '<', '>', '|'] then S[Index] := '¬';
  StripChar('¬', S);
end;

procedure TcaUtils.CleanIntString(var S: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(S) do
    if not (S[Index] in ['0'..'9', '-', '+']) then S[Index] := #32;
  StripChar(#32, S);
  if S = '' then S := '0';
end;

procedure TcaUtils.CleanNumString(var S: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(S) do
    if not (S[Index] in ['0'..'9', '-', '+', 'e', 'E', DecimalSeparator]) then S[Index] := #32;
  StripChar(#32, S);
  if S = '' then S := '0';
end;

procedure TcaUtils.CleanString(var S: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(S) do
    if S[Index] < #32 then
      S[Index] := #32;
end;

procedure TcaUtils.DeleteFromStart(var S: string; N: Integer);
begin
  S := RightStr(S, Length(S) - N);
end;

procedure TcaUtils.DeleteUntilChar(var S: string; Ch: Char; IncludeChar: Boolean);
var
  P: Integer;
begin
  P := Self.Pos(Ch, S);
  if P > 0 then
    begin
      if not IncludeChar then Dec(P);
      if P > 0 then DeleteFromStart(S, P);
    end;
end;

procedure TcaUtils.DeleteFromChar(var S: string; Ch: Char; IncludeChar: Boolean; N: Integer = 0);
var
  P: Integer;
begin
  P := Self.Pos(Ch, S);
  if P > 0 then
    begin
      if not IncludeChar then Inc(P);
      if N = 0 then N := Length(S);
      System.Delete(S, P, N);
    end;
end;

procedure TcaUtils.DeleteFromEnd(var S: string; N: Integer);
begin
  S := LeftStr(S, Length(S) - N);
end;

procedure TcaUtils.EnsureLastChar(var S: string; Ch: Char);
begin
  if LastChar(S) <> Ch then S := S + Ch;
end;

procedure TcaUtils.EnsureBackslash(var S: string);
begin
  EnsureLastChar(S, '\');
end;

procedure TcaUtils.MoveStr(const Source: string; SourcePos: Integer; var Dest: string; const DestPos, Count: Integer);
begin
  UniqueString(Dest);
  asm
    PUSH    ESI
    PUSH    EDI
    PUSH    EBX

    MOV     EAX, DEST
    OR      EAX, EAX
    JZ      @DONE
    MOV     EDI, [EAX]      // Dest address
    OR      EDI, EDI
    JZ      @DONE           // Sbort if null Dest pointer
    MOV     ESI, Source     // Source address
    OR      ESI, ESI
    JZ      @DONE           // Abort if null
    MOV     ECX, Count      // Copy count
    JECXZ   @DONE           // Abort if zero

    MOV     EBX, SourcePos  // Start location
    OR      EBX, EBX
    JZ      @L1
    DEC     EBX             // Zero based
@L1:
    MOV     EDX, [ESI-4]    // Source length
    OR      EDX, EDX
    JZ      @DONE           // Abort if null string

    SUB     EDX, EBX        // Remaining
    JZ      @DONE
    JS      @DONE
    ADD     ESI, EBX        // Start address

    CMP     ECX, EDX        // Count>remaining ?
    JBE     @L2             // Yes, then OK
    MOV     ECX, EDX        // Use remaining
@L2:
    MOV     EBX, DestPos    // Destination location
    OR      EBX, EBX
    JZ      @L3
    DEC     EBX             // Zero based
@L3:
    MOV     EDX, [EDI-4]    // Destination length
    OR      EDX, EDX
    JZ      @DONE           // Abort if null string

    SUB     EDX, EBX        // Remaining
    JZ      @DONE
    JS      @DONE
    ADD     EDI, EBX        // Start location

    CMP     ECX, EDX        // Count > remaining ?
    JBE     @L4             // Yes, then OK
    MOV     ECX, EDX        // Use remaining
@L4:
    CLD                     // Make sure we go forward
    REPNZ   MOVSB           // Do the move (finally)
@DONE:
    POP     EBX
    POP     EDI
    POP     ESI
  end;
end;

procedure TcaUtils.ReadStringFromBuffer(var S: string; var Buffer: PChar);
var
  Start: PChar;
begin
  Start := Buffer;
  while not (Buffer^ in [#13, #26]) do Inc(Buffer);
  SetString(S, Start, Integer(Buffer - Start));
  if Buffer^ = #13 then Inc(Buffer);
  if Buffer^ = #10 then Inc(Buffer);
end;

procedure TcaUtils.Replace(var S: string; const OldSub, NewSub: string; CaseSensitive: Boolean = False);
begin
  S := caFastStrings.FastReplace(S, OldSub, NewSub, CaseSensitive);
end;

procedure TcaUtils.ReplaceChar(var S: string; const OldChar, NewChar: Char);
begin
  UniqueString(S);
  asm
    PUSH    ESI
    PUSH    EBX

    MOV     EAX, S
    OR      EAX, EAX
    JZ      @EXIT
    MOV     ESI, [EAX]    // Source address
    OR      ESI, ESI      // Zero source ?
    JZ      @EXIT         // Yes, then bail

    MOV     AH, OldChar   // Search char
    MOV     BL, NewChar   // Replacement char
    CMP     AH, BL        // Same?
    JZ      @EXIT         // Yes, nothing to do but abort

    MOV     ECX, [ESI-4]  // Source length
    JECXZ   @EXIT         // Bail if zero

    CLD                   // Insure we go forward
@NEXT:
    LODSB                 // Get source character
    CMP     AL, AH        // Match ?
    JNE     @SKIP         // No, then skip
    MOV     [ESI-1], BL   // Yes, then replace
@SKIP:
    DEC     ECX
    JNZ     @NEXT

@EXIT:
    POP     EBX
    POP     ESI
  end;
end;

procedure TcaUtils.SplitCamelCaps(var S: string);
var
  C: Char;
  Index: Integer;
  TempStr: string;
begin
  TempStr := S;
  S := '';
  for Index := 1 to Length(TempStr) do
    begin
      C := TempStr[Index];
      if C in ['A'..'Z', '0'..'9'] then
        S := S + ' ';
      S := S + C;
    end;
end;

procedure TcaUtils.SortStrings(AStrings: TStrings);
var
  SortList: TStringList;
begin
  SortList := TStringList.Create;
  try
    SortList.Assign(AStrings);
    SortList.Sort;
    AStrings.Assign(SortList);
  finally
    SortList.Free;
  end;
end;

procedure TcaUtils.StripChar(C: Char; var S: string);
begin
  while Self.Pos(C, S) > 0 do
    System.Delete(S, Self.Pos(C, S), 1);
end;

procedure TcaUtils.StripLastCRLF(var S: string);
begin
  while (Length(S) > 0) and (S[Length(S)] in [#10, #13]) do
    System.Delete(S, Length(S), 1);
end;

procedure TcaUtils.StripLeadingLowerCase(var S: string);
var
  Index: Integer;
  UpperPos: Integer;
begin
  UpperPos := 0;
  for Index := 1 to Length(S) do
    begin
      if S[Index] in ['A'..'Z', '0'..'9'] then
        begin
          UpperPos := Index;
          Break;
        end;
    end;
  if UpperPos > 1 then
    DeleteFromStart(S, UpperPos - 1);
end;

procedure TcaUtils.StripDoubleQuotes(var S: string);
begin
  if Length(S) > 2 then
    begin
      if (LeftStr(S, 1) = #34) and (RightStr(S, 1) = #34) then
        begin
          DeleteFromStart(S, 1);
          DeleteFromEnd(S, 1);
        end;
    end;
end;

procedure TcaUtils.WriteStringToBuffer(const S: string; var Buffer: PChar);
var
  Index: Integer;
begin
  for Index := 1 to Length(S) do
    begin
      Buffer^ := S[Index];
      Inc(Buffer);
    end;
  Word(Pointer(Buffer)^) := $0A0D;
  Inc(Buffer, 2);
end;

  // TStrings support 

procedure TcaUtils.MaskDelete(AStrings, AMasks: TStrings);
var
  Index: Integer;
  MaskIndex: Integer;
begin
  for Index := Pred(AStrings.Count) downto 0 do
    begin
      for MaskIndex := 0 to Pred(AMasks.Count) do
        begin
          if MatchesMask(AStrings[Index], AMasks[MaskIndex]) then
            begin
              AStrings.Delete(Index);
              Break;
            end;
        end;
    end;
end;

function TcaUtils.MultiReplace(const S: string; AReplaceStrings: TStrings): string;
var
  Index: Integer;
  NewStr: string;
  OldStr: string;
begin
  Result := S;
  for Index := 0 to Pred(AReplaceStrings.Count) do
    begin
      OldStr := AReplaceStrings.Names[Index];
      NewStr := AReplaceStrings.Values[OldStr];
      Result := StringReplace(Result, OldStr, NewStr, [rfReplaceAll]);
    end;
end;

procedure TcaUtils.MultiReplace(AStrings, AReplaceStrings: TStrings);
var
  S: string;
  Index: Integer;
  ReplaceIndex: Integer;
  NewStr: string;
  OldStr: string;
begin
  for Index := 0 to Pred(AStrings.Count) do
    begin
      for ReplaceIndex := 0 to Pred(AReplaceStrings.Count) do
        begin
          S := AStrings[Index];
          OldStr := AReplaceStrings.Names[ReplaceIndex];
          NewStr := AReplaceStrings.Values[OldStr];
          AStrings[Index] := StringReplace(S, OldStr, NewStr, [rfReplaceAll]);
        end;
    end;
end;

  // Record support 

procedure TcaUtils.FillRecordFalse(var Rec; Size: Integer);
begin
  FillChar(Rec, Size, 0);
end;

procedure TcaUtils.FillRecordTrue(var Rec; Size: Integer);
begin
  FillChar(Rec, Size, 1);
end;

  //---------------------------------------------------------------------------
  // Comparison menthods                                                       
  //---------------------------------------------------------------------------

function TcaUtils.CompareBooleans(const B1, B2: Boolean): TcaCompareResult;
begin
  if B1 = B2 then Result := crEqual else
    if B1 > B2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.CompareDateTimes(const D1, D2: TDateTime): TcaCompareResult;
begin
  if D1 = D2 then Result := crEqual else
    if D1 > D2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.CompareDoubles(const D1, D2: Double): TcaCompareResult;
begin
  if D1 = D2 then Result := crEqual else
    if D1 > D2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.CompareExtendeds(const E1, E2: Extended): TcaCompareResult;
begin
  if E1 = E2 then Result := crEqual else
    if E1 > E2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.CompareIntegers(const I1, I2: Integer): TcaCompareResult;
begin
  if I1 = I2 then Result := crEqual else
    if I1 > I2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.CompareInt64s(const I1, I2: Int64): TcaCompareResult;
begin
  if I1 = I2 then Result := crEqual else
    if I1 > I2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.CompareSingles(const S1, S2: Single): TcaCompareResult;
begin
  if S1 = S2 then Result := crEqual else
    if S1 > S2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.CompareStrings(const S1, S2: string; ACaseInsensitive: Boolean = False): TcaCompareResult;
var
  Str1: string;
  Str2: string;
begin
  if ACaseInsensitive then
    begin
      Str1 := LowerCase(S1);
      Str2 := LowerCase(S2);
    end
  else
    begin
      Str1 := S1;
      Str2 := S2;
    end;
  if Str1 = Str2 then Result := crEqual else
    if Str1 > Str2 then Result := crFirstGreater
    else Result := crSecondGreater;
end;

function TcaUtils.OperatorToString(AOperator: TcaOperator): string;
begin
  Result := '';
  case AOperator of
    opEqual: Result := '=';
    opLessThan: Result := '<';
    opGreaterThan: Result := '>';
    opNotEqual: Result := '<>';
    opGreaterThanOrEqual: Result := '>=';
    opLessThanOrEqual: Result := '<=';
    opUnspecified: Result := '';
  end;
end;

  // Array comparison menthods 

function TcaUtils.DoubleArraysEqual(A1, A2: array of Double): Boolean;
begin
  raise Exception.Create('Not implemented yet');
end;

  //---------------------------------------------------------------------------
  // File methods                                                              
  //---------------------------------------------------------------------------

function TcaUtils.RemoveExt(const AFileName: string): string;
begin
  Result := ChangeFileExt(AFileName, '');
end;

function TcaUtils.ExtractFileName(const FileName: string; IncludeExt: boolean = true): string;
begin
  if IncludeExt then
    Result := Sysutils.ExtractFileName(FileName)
  else
    Result := RemoveExt(Sysutils.ExtractFileName(FileName));
end;

procedure TcaUtils.GetFileList(const APath, AFileSpec: string; AFileList: TcaStringList; IncludeDotRefs: Boolean = False);
begin
  GetFileList(APath, AFileSpec, AFileList.GetStrings, IncludeDotRefs);
end;

procedure TcaUtils.GetFileList(const APath, AFileSpec: string; AFileList: IcaStringList; IncludeDotRefs: Boolean = False);
begin
  GetFileList(APath, AFileSpec, AFileList.GetStrings, IncludeDotRefs);
end;

procedure TcaUtils.GetFileList(const APath, AFileSpec: string; AFileList: TStrings; IncludeDotRefs: Boolean = False);
var
  FileAttrib: Integer;
  FindResult: Integer;
  FName: string;
  FoundFirst: Boolean;
  FullFileName: string;
  IsDotRef: Boolean;
  Mask: TMask;
  OKToAdd: Boolean;
  Path: string;
  SRec: TSearchRec;
begin
  Path := APath;
  EnsureLastChar(Path, '\');
  AFileList.Clear;
  FindResult := FindFirst(Path + AFileSpec, faAnyFile, SRec);
  FoundFirst := FindResult = WN_SUCCESS;
  try
    Mask := TMask.Create(AFileSpec);
    try
      while FindResult = WN_SUCCESS do
        begin
          FName := SRec.FindData.cFileName;
          IsDotRef := FName[1] = '.';
          OKToAdd := IncludeDotRefs or ((not IncludeDotRefs) and (not IsDotRef));
          if OKToAdd then
            begin
              FullFileName := Path + FName;
              if Mask.Matches(FName) then
                begin
                  FileAttrib := SRec.Attr;
                  AFileList.AddObject(FName, Pointer(FileAttrib));
                end;
            end;
          FindResult := FindNext(SRec);
        end;
    finally
      Mask.Free;
    end;
  finally
    if FoundFirst then FindClose(SRec);
  end;
end;

procedure TcaUtils.SearchForFiles(const APath, AFileSpec: string; AOutList: IcaStringList);
begin
  SearchForFiles(APath, AFileSpec, AOutList.GetStrings);
end;

procedure TcaUtils.SearchForFiles(const APath, AFileSpec: string; AOutList: TStrings);
var
  Continue: Boolean;
  FileList: TStringList;
  FileName: string;
  FullFileName: string;
  Index: Integer;
  IsDir: Boolean;
  Mask: TMask;
  Path: string;
  Stack: IcaStringStack;
begin
  Continue := True;
  Mask := TMask.Create(AFileSpec);
  try
    AOutList.Clear;
    Stack := TcaStringList.Create;
    Stack.Push(APath);
    FileList := TStringList.Create;
    try
      while Stack.HasItems do
        begin
          Path := Stack.Pop;
          GetFileList(Path, '*.*', FileList);
          for Index := 0 to FileList.Count - 1 do
            begin
              if Assigned(FOnContinueFileSearch) then
                FOnContinueFileSearch(Self, Continue);
              if not Continue then Break;
              FileName := FileList[Index];
              EnsureLastChar(Path, '\');
              FullFileName := Path + FileName;
              IsDir := (Integer(FileList.Objects[Index]) and faDirectory) <> 0;
              if IsDir then
                Stack.Push(FullFileName)
              else
                begin
                  if Mask.Matches(FullFileName) then
                    AOutList.Add(FullFileName);
                end;
            end;
        end;
    finally
      FileList.Free;
    end;
  finally
    Mask.Free;
  end;
end;

  //---------------------------------------------------------------------------
  // Math methods                                                              
  //---------------------------------------------------------------------------

function TcaUtils.BankersRound(const X: Extended): Integer;
begin
  Check8087;
  Result := Round(X);
end;

function TcaUtils.Equal(const N1, N2: Double): Boolean;
begin
  Check8087;
  CheckZeroTolerance;
  Result := Abs(N1 - N2) < FZeroTolerance;
end;

function TcaUtils.Equal(const N1, N2: Extended): Boolean;
begin
  Check8087;
  CheckZeroTolerance;
  Result := Abs(N1 - N2) < FZeroTolerance;
end;

function TcaUtils.Equal(const N1, N2: Single): Boolean;
begin
  Check8087;
  CheckZeroTolerance;
  Result := Abs(N1 - N2) < FZeroTolerance;
end;

function TcaUtils.EquiRound(const X: Extended): Integer;
begin
  {$IFDEF USE_SAFE_ROUND}
  Result := SafeRound(X);
  {$ELSE}
  Check8087;
  Result := Self.Trunc(X) + Self.Trunc(Frac(X) * 2);
  {$ENDIF}
end;

function TcaUtils.Exp(const N: Double): Double;
begin
  Check8087;
  Result := System.Exp(N);
end;

function TcaUtils.FloatDiv(const Numerator, Divisor, Default: Extended): Extended;
var
  ArgsOK: Boolean;
begin
  Check8087;
  ArgsOK := True;
  if IsZero(Divisor) then ArgsOK := False;
  if IsNAN(Numerator) or IsNAN(Divisor) then ArgsOK := False;
  if ArgsOK then
    Result := Numerator / Divisor
  else
    Result := Default;
end;

function _FloatPower(Base, Exponent: Double): Double;
asm
  FLDLN2
  FLD Base
  FYL2X

  FLD Exponent
  FMUL

  FLDL2E
  FMUL
  FLD       ST(0)
  FRNDINT
  FSUB      ST(1), ST
  FXCH      ST(1)
  F2XM1
  FLD1
  FADD
  FSCALE
  FSTP      ST(1)
  FWAIT
end;

function TcaUtils.FloatPower(const ABase, AExponent: Extended): Extended;
begin
  Check8087;
  {$IFDEF USE_DELPHI_MATH_POWER}
  Result := Math.Power(ABase, AExponent);
  {$ELSE}
  if AExponent = 0.0 then
    Result := 1.0
  else
    if ABase = 0.0 then
      Result := 0
    else
      Result := _FloatPower(ABase, AExponent);
  {$ENDIF}
end;

function _IntPower(ABase: Double; AExponent: Integer): Double;
var
  SqrValue: Double;
begin
  case AExponent of
    0: Result := 1.0;
    1: Result := ABase;
    2: Result := ABase * ABase;
  else
    SqrValue := _IntPower(ABase, AExponent div 2);
    SqrValue := SqrValue * SqrValue;
    if Odd(AExponent) then
      Result := SqrValue * ABase
    else
      Result := SqrValue;
  end;
end;

function TcaUtils.RoundTo(const Value: Extended; Places: Integer): Extended;
var
  TenTo: Double;
begin
  TenTo := _IntPower(10, Places);
  Result := Int(Value * TenTo + 0.5) / TenTo
end;

function TcaUtils.IntPower(const ABase: Extended; AExponent: Integer): Extended;
begin
  Check8087;
  {$IFDEF USE_DELPHI_INT_POWER}
  Result := Math.IntPower(ABase, AExponent);
  {$ELSE}
  Result := _IntPower(ABase, AExponent);
  {$ENDIF}
end;

function TcaUtils.SafeFrac(const X: Extended): Extended;
var
  StrVal: string;
  PointPos: Integer;
  IntVal: Integer;
begin
  StrVal := Trim(Format('%18.6f', [X]));
  PointPos := FastPos(StrVal, DecimalSeparator);
  System.Delete(StrVal, 1, PointPos);
  IntVal := SafeStrToInt(StrVal);
  Result := IntVal / cMillion;
end;

function TcaUtils.SafeRound(const X: Extended): Integer;
var
  IsNeg: Boolean;
  PointPos: Integer;
  StrVal: string;
  DecDigit: Char;
begin
  IsNeg := IsLT0(X);
  Result := SafeTrunc(X);
  StrVal := FloatToStr(X);
  PointPos := FastPos(StrVal, DecimalSeparator);
  if PointPos > 0 then
    begin
      if Length(StrVal) > PointPos then
        begin
          DecDigit := StrVal[Succ(PointPos)];
          if Ord(DecDigit) >= $35 then
            Inc(Result);
        end;
    end;
  if Result < 0 then Result := -Result;
  if IsNeg then Result := -Result;
end;

function TcaUtils.SafeStrToInt(const AStrVal: string): Integer;
var
  Index: Integer;
  Multiplier: Integer;
  DigitChar: Char;
  Digit: Integer;
begin
  Result := 0;
  Multiplier := 1;
  for Index := Length(AStrVal) downto 1 do
    begin
      DigitChar := AStrVal[Index];
      if DigitChar in [#$30..#$39] then
        begin
          Digit := Ord(DigitChar) - $30;
          Result := Result + Digit * Multiplier;
        end;
      Multiplier := Multiplier * 10;
    end;
end;

function TcaUtils.SafeTrunc(const X: Extended): Integer;
var
  PointPos: Integer;
  StrVal: string;
begin
  StrVal := Trim(Format('%16.1f', [X]));
  PointPos := FastPos(StrVal, DecimalSeparator);
  System.Delete(StrVal, PointPos, Length(StrVal));
  Result := SafeStrToInt(StrVal);
end;

function TcaUtils.Sign(const N: Double): Integer;
begin
  if N < 0 then
    Result := -1
  else
    Result := 1;
end;

function TcaUtils.Sqr(X: Extended): Extended;
begin
  Check8087;
  Result := System.Sqr(X);
end;

//  function _Trunc(X: Extended): Integer;
//  var cw1,cw2: Word;
//  asm
//        FLD     X
//        FNSTCW  cw1.Word          // save
//        FNSTCW  cw2.Word        // scratch
//        FWAIT
//        OR      cw2.Word, $0F00  // trunc toward zero, full precision
//        FLDCW   cw2.Word
//        FISTP   Result.Integer { was Int64 }
//        FWAIT
//        FLDCW   cw1.Word
//  end;


function TcaUtils.Trunc(X: Extended): Int64;
var
  StrVal: string;
begin
  // Check8087; 
  // Result := System.Trunc(X); 
  // Result := _Trunc(X); 
  StrVal := Trim(Format('%20.0f', [X]));
  Result := StrToInt64(StrVal);
end;

function TcaUtils.VBInt(X: Extended): Int64;
var
  IntVal: Int64;
begin
  Check8087;
  IntVal := EquiRound(System.Int(X));
  if IsLT0(X) then
    Result := IntVal - 1
  else
    Result := IntVal;
end;

  // Comparisons 

function TcaUtils.IsGreaterThanZero(const AValue: Double): Boolean;
begin
  Check8087;
  CheckZeroTolerance;
  Result := AValue > FZeroTolerance;
end;

function TcaUtils.IsGreaterThanZero(const AValue: Extended): Boolean;
begin
  Check8087;
  CheckZeroTolerance;
  Result := AValue > FZeroTolerance;
end;

function TcaUtils.IsGreaterThanZero(const AValue: Single): Boolean;
begin
  Check8087;
  CheckZeroTolerance;
  Result := AValue > FZeroTolerance;
end;

function TcaUtils.IsNonZero(const AValue: Double): Boolean;
begin
  Check8087;
  Result := not IsZero(AValue);
end;

function TcaUtils.IsNonZero(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := not IsZero(AValue);
end;

function TcaUtils.IsNonZero(const AValue: Single): Boolean;
begin
  Check8087;
  Result := not IsZero(AValue);
end;

function TcaUtils.IsZero(const AValue: Double): Boolean;
begin
  Check8087;
  Result := Equal(AValue, 0);
end;

function TcaUtils.IsZero(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := Equal(AValue, 0);
end;

function TcaUtils.IsZero(const AValue: Single): Boolean;
begin
  Check8087;
  Result := Equal(AValue, 0);
end;

  // Is Equal to 

function TcaUtils.IsEq(const AValue1, AValue2: Double): Boolean;
begin
  Check8087;
  Result := Abs(AValue1 - AValue2) < FZeroTolerance;
end;

function TcaUtils.IsEq(const AValue1, AValue2: Extended): Boolean;
begin
  Check8087;
  Result := Abs(AValue1 - AValue2) < FZeroTolerance;
end;

function TcaUtils.IsEq(const AValue1, AValue2: Single): Boolean;
begin
  Check8087;
  Result := Abs(AValue1 - AValue2) < FZeroTolerance;
end;

  // Is Equal to 

function TcaUtils.IsNEq(const AValue1, AValue2: Double): Boolean;
begin
  Check8087;
  Result := Abs(AValue1 - AValue2) > FZeroTolerance;
end;

function TcaUtils.IsNEq(const AValue1, AValue2: Extended): Boolean;
begin
  Check8087;
  Result := Abs(AValue1 - AValue2) > FZeroTolerance;
end;

function TcaUtils.IsNEq(const AValue1, AValue2: Single): Boolean;
begin
  Check8087;
  Result := Abs(AValue1 - AValue2) > FZeroTolerance;
end;

  // Is Greater Than 

function TcaUtils.IsGT(const AValue1, AValue2: Double): Boolean;
begin
  Check8087;
  Result := (AValue1 - AValue2) > FZeroTolerance;
end;

function TcaUtils.IsGT(const AValue1, AValue2: Extended): Boolean;
begin
  Check8087;
  Result := (AValue1 - AValue2) > FZeroTolerance;
end;

function TcaUtils.IsGT(const AValue1, AValue2: Single): Boolean;
begin
  Check8087;
  Result := (AValue1 - AValue2) > FZeroTolerance;
end;

  // Is Greater Than or Equal 

function TcaUtils.IsGTE(const AValue1, AValue2: Double): Boolean;
begin
  Check8087;
  Result := (AValue1 - AValue2) > -FZeroTolerance;
end;

function TcaUtils.IsGTE(const AValue1, AValue2: Extended): Boolean;
begin
  Check8087;
  Result := (AValue1 - AValue2) > -FZeroTolerance;
end;

function TcaUtils.IsGTE(const AValue1, AValue2: Single): Boolean;
begin
  Check8087;
  Result := (AValue1 - AValue2) > -FZeroTolerance;
end;

  // Is Less Than 

function TcaUtils.IsLT(const AValue1, AValue2: Double): Boolean;
begin
  Check8087;
  Result := (AValue2 - AValue1) > FZeroTolerance;
end;

function TcaUtils.IsLT(const AValue1, AValue2: Extended): Boolean;
begin
  Check8087;
  Result := (AValue2 - AValue1) > FZeroTolerance;
end;

function TcaUtils.IsLT(const AValue1, AValue2: Single): Boolean;
begin
  Check8087;
  Result := (AValue2 - AValue1) > FZeroTolerance;
end;

  // Is Less Than or Equal 

function TcaUtils.IsLTE(const AValue1, AValue2: Double): Boolean;
begin
  Check8087;
  Result := (AValue2 - AValue1) > -FZeroTolerance;
end;

function TcaUtils.IsLTE(const AValue1, AValue2: Extended): Boolean;
begin
  Check8087;
  Result := (AValue2 - AValue1) > -FZeroTolerance;
end;

function TcaUtils.IsLTE(const AValue1, AValue2: Single): Boolean;
begin
  Check8087;
  Result := (AValue2 - AValue1) > -FZeroTolerance;
end;

  // Is Greater Than Zero 

function TcaUtils.IsGT0(const AValue: Double): Boolean;
begin
  Check8087;
  Result := AValue > FZeroTolerance;
end;

function TcaUtils.IsGT0(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := AValue > FZeroTolerance;
end;

function TcaUtils.IsGT0(const AValue: Single): Boolean;
begin
  Check8087;
  Result := AValue > FZeroTolerance;
end;

  // Is Greater Than or Equal to Zero 

function TcaUtils.IsGTE0(const AValue: Double): Boolean;
begin
  Check8087;
  Result := AValue > -FZeroTolerance;
end;

function TcaUtils.IsGTE0(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := AValue > -FZeroTolerance;
end;

function TcaUtils.IsGTE0(const AValue: Single): Boolean;
begin
  Check8087;
  Result := AValue > -FZeroTolerance;
end;

  // Is Less Than Zero 

function TcaUtils.IsLT0(const AValue: Double): Boolean;
begin
  Check8087;
  Result := AValue < -FZeroTolerance;
end;

function TcaUtils.IsLT0(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := AValue < -FZeroTolerance;
end;

function TcaUtils.IsLT0(const AValue: Single): Boolean;
begin
  Check8087;
  Result := AValue < -FZeroTolerance;
end;

  // Is Less Than or Equal to Zero 

function TcaUtils.IsLTE0(const AValue: Double): Boolean;
begin
  Check8087;
  Result := AValue < FZeroTolerance;
end;

function TcaUtils.IsLTE0(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := AValue < FZeroTolerance;
end;

function TcaUtils.IsLTE0(const AValue: Single): Boolean;
begin
  Check8087;
  Result := AValue < FZeroTolerance;
end;

  // Is Zero 

function TcaUtils.Is0(const AValue: Double): Boolean;
begin
  Check8087;
  Result := Abs(AValue) < FZeroTolerance;
end;

function TcaUtils.Is0(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := Abs(AValue) < FZeroTolerance;
end;

function TcaUtils.Is0(const AValue: Single): Boolean;
begin
  Check8087;
  Result := Abs(AValue) < FZeroTolerance;
end;

  // Is Non Zero 

function TcaUtils.IsN0(const AValue: Double): Boolean;
begin
  Check8087;
  Result := Abs(AValue) > FZeroTolerance;
end;

function TcaUtils.IsN0(const AValue: Extended): Boolean;
begin
  Check8087;
  Result := Abs(AValue) > FZeroTolerance;
end;

function TcaUtils.IsN0(const AValue: Single): Boolean;
begin
  Check8087;
  Result := Abs(AValue) > FZeroTolerance;
end;

  // Is Multiple of 

function TcaUtils.IsMultipleOf(const AValue1, AValue2: Integer): Boolean;
begin
  Result := (AValue1 mod AValue2) = 0;
end;

function TcaUtils.IsMultipleOf(const AValue1, AValue2: Double): Boolean;
var
  M: Extended;
begin
  Check8087;
  M := AValue1 - (AValue2 * Floor(AValue1 / AValue2));
  Result := Is0(M);
end;

function TcaUtils.IsMultipleOf(const AValue1, AValue2: Extended): Boolean;
var
  M: Extended;
begin
  Check8087;
  M := AValue1 - (AValue2 * Floor(AValue1 / AValue2));
  Result := Is0(M);
end;

function TcaUtils.IsMultipleOf(const AValue1, AValue2: Single): Boolean;
var
  M: Extended;
begin
  Check8087;
  M := AValue1 - (AValue2 * Floor(AValue1 / AValue2));
  Result := Is0(M);
end;

  // Log methods 

function TcaUtils.Ln(const N: Double): Double;
begin
  Check8087;
  Result := System.Ln(N);
end;

  // Zero tolerance 

procedure TcaUtils.RestoreDefaultZeroTolerance;
begin
  FZeroTolerance := cDefaultZeroTolerance;
end;

procedure TcaUtils.RestoreSavedZeroTolerance;
begin
  FZeroTolerance := FSavedZeroTolerance;
end;

procedure TcaUtils.SaveCurrentZeroTolerance;
begin
  FSavedZeroTolerance := FZeroTolerance;
end;

  // IcaFastMath 

  {$IFDEF USE_FAST_MATH}

  // Awaiting feedback from Robert Lee 

const
  cDblExpA: Double = $100000 / cLn2x; // 2^20 / cLn2
  cDblExpC = 68243;
  cDblLnA: Double = cLn2x / $100000;

  cSngDivA: Single = -1;
  cSngExpA: Double = $800000 / cLn2x; // 2^23 / cLn2
  cSngExpC = 545947;
  cSngLnA: Double = cLn2x / $800000;
  cSngLnC = 545947;
  cSngNExpA: Double = -$800000 / cLn2x; // 2^23 / cLn2
  cSngOne: Single = 1;

function _DblExp(x: Double): Double;
var
  Y: Double;
asm
  fld     x
  fmul    cDblExpA
  fistp   dword ptr y[4]
  mov     dword ptr y[0], 0
  add     dword ptr y[4], ($3FF00000 - cDblExpC)
  fld     y
end;

function _DblLn(X: Double): Double;
asm
  sub     dword ptr x[4], ($3FF00000 - cDblExpC);
  fild    dword ptr x[4]
  fmul    cDblLnA
end;

function _SngExp(X: Single): Double;
// ~ 10x faster 
asm
  fld     x
  fmul    cSngExpA
  fistp   dword ptr x
  add     dword ptr x,($3F800000 - cSngExpC)
  fld     x
end;

function _SngExp1(var X: Single): Double;
// special case when x can be prevented from being pushed on the stack 
// ~14.5x faster 
asm
  fld     dword ptr [eax]
  fmul    cSngExpA
  fistp   dword ptr [esp-4]
  add     dword ptr [esp-4],($3F800000 - cSngExpC)
  fld     dword ptr [esp-4]
end;


function _SngLn(X: Single): Double;
// ~ 7x faster 
// very poor accuracy as x -> 1.0 
asm
  sub     dword ptr x, ($3F800000 - cSngLnC)
  fild    dword ptr x
  fmul    cSngLnA
end;

function _SngDiv(X: Single): Single;
// ~20 faster in some circumstances 
var
  Y: Single;
asm
  sub     dword ptr x, ($3F800000 - cSngLnC)
  fild    dword ptr x
  fmul    cSngDivA
  fistp   dword ptr y
  add     dword ptr y,($3F800000 - cSngExpC)
  fld     y  // 1/x
end;

function Sigmoid(X: Single): Single;
// ~1.6x faster than 1 / (1 + fexp) 
asm
  fld     x
  fmul    cSngNExpA
  fistp   dword ptr x
  add     dword ptr x,($3F800000 - cSngExpC)
  fld     x
  fld     dword ptr cSngOne
  fadd    st(1), st(0);
  fdivr
end;

function Sigmoid1(var X: Single): Single;
// ~2.3x faster than 1 / (1 + fexp) 
asm
  fld     dword ptr [esp - 4]
  fmul    cSngNExpA
  fistp   dword ptr [esp - 4]
  add     dword ptr [esp - 4], ($3F800000 - cSngExpC)
  fld     dword ptr [esp - 4]
  fld     dword ptr cSngOne
  fadd    st(1), st(0);
  fdivr
end;

function Sigmoid0(X: Single): Single;
begin
  Result := 1 / (1 + _SngExp(-X));
end;


function TcaUtils.FastExp(X: Double): Double;
var
  Y: Double;
asm
  fld     x
  fmul    cDblExpA
  fistp   dword ptr y[4]
  mov     dword ptr y[0], 0
  add     dword ptr y[4], ($3FF00000 - cDblExpC)
  fld     y
end;

function TcaUtils.FastExpVar(var N: Double): Double;
begin
  Check8087;
  Result := 0;
end;

function TcaUtils.FastFloatPower(const ABase, AExponent: Double): Double;
begin
  Check8087;
  Result := 0;
end;

function TcaUtils.FastLn(X: Double): Double;
asm
  sub     dword ptr x[4], ($3FF00000 - cDblExpC);
  fild    dword ptr x[4]
  fmul    cDblLnA
end;

function TcaUtils.FastSqrt(const N: Double): Double;
begin
  Check8087;
  Result := 0;
end;

  {$IFDEF NEVER}

  // dxxx is a double based version 
  // fxxx is a single based version 

const
  ln2 = 0.693147181;
  dexpA: double = $100000 / ln2; // 2^20 / ln2
  dlnA: double = ln2 / $100000;
  fexpA: double = $800000 / ln2; // 2^23 / ln2
  flnA: double = ln2 / $800000;
  fexpC = 545947;
  flnC = 545947;
  dexpC = 68243;
  fnexpA: double = -$800000 / ln2; // 2^23 / ln2
  fone: single = 1;
  fdivA: single = -1;

function dexp(x: single): double;
var y: double;
asm
  fld x
  fmul dexpA
  fistp dword ptr y[4]
  mov dword ptr y[0],0
  add dword ptr y[4],($3FF00000-dExpC)
  fld y
end;

function dln(x: double): double;
asm
  sub dword ptr x[4],($3FF00000-dExpC);
  fild dword ptr x[4]
  fmul dlnA
end;

function fexp(x: single): double;
// ~ 10x faster 
asm
  fld x
  fmul fexpA
  fistp dword ptr x
  add dword ptr x,($3F800000-fExpC)
  fld x
end;

function fexp1(var x: single): double;
// special case when x can be prevented from being pushed on the stack 
// ~14.5x faster 
asm
  fld dword ptr [eax]
  fmul fexpA
  fistp dword ptr [esp-4]
  add dword ptr [esp-4],($3F800000-fExpC)
  fld dword ptr [esp-4]
end;


function fln(x: single): double;
// ~ 7x faster 
// very poor accuracy as x -> 1.0 
asm
  sub dword ptr x, ($3F800000-flnC)
  fild dword ptr x
  fmul flnA
end;

function fdiv(x: single): single;
// ~20 faster in some circumstances 
var y: single;
asm
  sub dword ptr x, ($3F800000-flnC)
  fild dword ptr x
  fmul fdivA
  fistp dword ptr y
  add dword ptr y,($3F800000-fExpC)
  fld y  // 1/x
end;

function sigmoid(x: single): single;
// ~1.6x faster than 1/(1+fexp) 
asm
  fld x
  fmul fnexpA
  fistp dword ptr x
  add dword ptr x,($3F800000-fExpC)
  fld x
  fld dword ptr fone
  fadd st(1),st(0);
  fdivr
end;

function sigmoid1(var x: single): single;
// ~2.3x faster than 1/(1+fexp) 
asm
  fld dword ptr [esp-4]
  fmul fnexpA
  fistp dword ptr [esp-4]
  add dword ptr [esp-4],($3F800000-fExpC)
  fld dword ptr [esp-4]
  fld dword ptr fone
  fadd st(1),st(0);
  fdivr
end;

function sigmoid0(x: single): single;
begin
  result := 1 / (1 + fexp(-x));
end;

{$ENDIF}

{$ENDIF}

  // Other math methods 

function TcaUtils.Stirling2(AElements, ASubsets: Integer): Double;
// The number of partitions of a set of m elements into n nonempty subsets is S(m, n), 
// the Stirling number of the second kind. 
begin
  if (AElements < ASubsets) then
    Result := 0.0
  else if (AElements = ASubsets) then
    Result := 1.0
  else
    case ASubsets of
      0: Result := 0.0;
      1: Result := 1.0;
      2: Result := IntPower(2.0, AElements - 1) - 1.0;
    else
      Result := (Stirling2(AElements - 1, ASubsets) * ASubsets) + Stirling2(AElements - 1, ASubsets - 1);
    end;
end;

// 8087 FPU Methods 

function TcaUtils.Get8087ControlWord: Word;
begin
  asm
    FSTCW   RESULT
  end;
end;

function TcaUtils.Valid8087ControlWord: Boolean;
begin
  Result := ((Default8087CW xor Get8087ControlWord) and not $E060) = 0;
end;

procedure TcaUtils.Check8087;
begin
  if FUseCheck8087 then
    begin
      {$IFDEF CHECK_387_CONTROL}
      Check8087ControlWord;
      {$ELSE}
      {$IFDEF FIX_387_CONTROL}
      Fix8087ControlWord;
      {$ENDIF}
      {$ENDIF}
      {$IFDEF CLEAR_387_STATUS}
      Clear8087StatusWord;
      {$ENDIF}
    end;
end;

procedure TcaUtils.Check8087ControlWord;
begin
  if not Valid8087ControlWord then Fix8087ControlWord;
end;

procedure TcaUtils.Fix8087ControlWord;
begin
  Set8087CW(Default8087CW);
end;

procedure TcaUtils.Clear8087StatusWord;
asm
  FNCLEX
end;

  //---------------------------------------------------------------------------
  // Math evaluator methods                                                    
  //---------------------------------------------------------------------------

function TcaUtils.GetOperatorPrecedence(const AOperator, AReferenceOperator: string): TcaOperatorPrecedence;
var
  Precedence: Integer;
  RefPrecedence: Integer;

  function GetPrecedence(const S: string): Integer;
  begin
    Result := 0;
    if Self.Pos(LowerCase(S) + ' ', '^ ') > 0 then Result := 1 else
      if Self.Pos(LowerCase(S) + ' ', '* / ') > 0 then Result := 2 else
        if Self.Pos(LowerCase(S) + ' ', '+ - ') > 0 then Result := 3 else
          if Self.Pos(LowerCase(S) + ' ', '( ) ') > 0 then Result := 4;
  end;

begin
  Precedence := GetPrecedence(AOperator);
  RefPrecedence := GetPrecedence(AReferenceOperator);
  if Precedence < RefPrecedence then
    Result := opHigher
  else
    begin
      if Precedence > RefPrecedence then
        Result := opLower
      else
        begin
          if Precedence = 1 then
            Result := opSameRightAssoc
          else
            Result := opSameLeftAssoc;
        end;
    end;
end;

function TcaUtils.IsBracket(const S: string): Boolean;
begin
  Result := (Length(S) = 1) and (Self.Pos(S, '()') > 0);
end;

function TcaUtils.IsFunction(const S: string): Boolean;
var
  AFunction: string;
begin
  AFunction := ' ' + LowerCase(S) + ' ';
  Result := Self.Pos(AFunction, cEvalFunctions) > 0;
end;

function TcaUtils.IsOperand(const S: string): Boolean;
begin
  Result := True;
  if IsOperator(S) or IsBracket(S) or IsFunction(S) then Result := False;
end;

function TcaUtils.IsOperator(const S: string): Boolean;
begin
  Result := Self.Pos(S, '+-*/^') > 0;
end;

  //---------------------------------------------------------------------------
  // Graphics methods                                                          
  //---------------------------------------------------------------------------

function TcaUtils.GetBitmapRect(ABitmap: TBitmap): TRect;
begin
  Result := Rect(0, 0, ABitmap.Width, ABitmap.Height);
end;

function TcaUtils.HTMLColorToColor(const AHTMLColor: string): TColor;
var
  ColorStr: string;
begin
  if (AHTMLColor[1] = '#') and (AHTMLColor <> '#') then
    ColorStr := '$' + Copy(AHTMLColor, 6, 2) + Copy(AHTMLColor, 4, 2) + Copy(AHTMLColor, 2, 2)
  else
    ColorStr := 'cl' + AHTMLColor;
  Result := StringToColor(ColorStr);
end;

  //---------------------------------------------------------------------------
  // Font methods                                                              
  //---------------------------------------------------------------------------

function TcaUtils.FontsEqual(AFont1, AFont2: TFont): Boolean;
begin
  Result := (AFont1.Name = AFont2.Name) and
    (AFont1.Size = AFont2.Size) and
    (AFont1.Style = AFont2.Style) and
    (AFont1.Color = AFont2.Color);
end;

  //---------------------------------------------------------------------------
  // TRect methods                                                             
  //---------------------------------------------------------------------------

function TcaUtils.InRect(X, Y: Integer; Rect: TRect): Boolean;
begin
  Result := (X >= Rect.Left) and (X <= Rect.Right) and
    (Y >= Rect.Top) and (Y <= Rect.Bottom);
end;

function TcaUtils.InRect(APoint: TPoint; Rect: TRect): Boolean;
begin
  Result := InRect(APoint.X, APoint.Y, Rect);
end;

procedure TcaUtils.AdjustRect(var ARect: TRect; const dL, dT, dR, dB: Integer);
begin
  Inc(ARect.Left, dl);
  Inc(ARect.Top, dT);
  Inc(ARect.Right, dR);
  Inc(ARect.Bottom, dB);
end;

procedure TcaUtils.DecodeRect(const ARect: TRect; var ALeft, ATop, AWidth, AHeight: Integer);
begin
  ALeft := ARect.Left;
  ATop := ARect.Top;
  AWidth := ARect.Right - ARect.Left + 1;
  AHeight := ARect.Bottom - ARect.Top + 1;
end;

  //---------------------------------------------------------------------------
  // Dialog methods                                                            
  //---------------------------------------------------------------------------

function TcaUtils.QueryDeleteData(AHandle: HWND; const ADataSynonym: string = ''): TcaMsgDialogResponse;
var
  DataSynonym: string;
  Response: Word;
  DlgType: Cardinal;
  Msg: string;
begin
  if ADataSynonym <> '' then
    DataSynonym := ADataSynonym
  else
    DataSynonym := 'this data';
  DlgType := MB_YESNO or MB_ICONQUESTION;
  if AHandle = 0 then DlgType := DlgType or MB_TASKMODAL;
  Msg := Format('Do you really want to delete %s ?', [DataSynonym]);
  Response := MessageBox(AHandle, PChar(Msg), 'Confirm', DlgType);
  case Response of
    IDNO: Result := mgNo;
    IDYES: Result := mgYes;
  else
    Result := mgNo;
  end;
end;

function TcaUtils.QuerySaveData(AHandle: HWND): TcaMsgDialogResponse;
var
  Response: Word;
  DlgType: Cardinal;
begin
  DlgType := MB_YESNOCANCEL or MB_ICONQUESTION;
  if AHandle = 0 then DlgType := DlgType or MB_TASKMODAL;
  Response := MessageBox(AHandle, 'Changes have been made.' + #13 + #10 +
    'Do you wish to save the changes ?', 'Confirm', DlgType);
  case Response of
    IDCANCEL: Result := mgCancel;
    IDNO: Result := mgNo;
    IDYES: Result := mgYes;
  else
    Result := mgCancel;
  end;
end;

function TcaUtils.QueryCloseApp(AHandle: HWND; const ACloseSynonym: string = ''): TcaMsgDialogResponse;
var
  CloseSynonym: string;
  Response: Word;
  DlgType: Cardinal;
  Msg: string;
begin
  if ACloseSynonym <> '' then
    CloseSynonym := ACloseSynonym
  else
    CloseSynonym := 'Close';
  DlgType := MB_YESNO or MB_ICONQUESTION;
  if AHandle = 0 then DlgType := DlgType or MB_TASKMODAL;
  Msg := Format('Do you really want to %s this application ?', [CloseSynonym]);
  Response := MessageBox(AHandle, PChar(Msg), 'Confirm', DlgType);
  case Response of
    IDNO: Result := mgNo;
    IDYES: Result := mgYes;
  else
    Result := mgNo;
  end;
end;

  //---------------------------------------------------------------------------
  // System methods                                                            
  //---------------------------------------------------------------------------

function TcaUtils.AltKeyDown: Boolean;
begin
  Result := IsKeyDown(VK_MENU);
end;

function TcaUtils.ControlKeyDown: Boolean;
begin
  Result := IsKeyDown(VK_CONTROL);
end;

function TcaUtils.ShiftKeyDown: Boolean;
begin
  Result := IsKeyDown(VK_SHIFT);
end;

function TcaUtils.IsKeyDown(AKeyCode: Word): Boolean;
begin
  Result := HiByte(GetKeyState(AKeyCode)) <> 0;
end;

function TcaUtils.IsLargeFonts: Boolean;
var
  DC: hDC;
begin
  DC := GetDC(Application.MainForm.Handle);
  Result := GetDeviceCaps(DC, LOGPIXELSX) > 96;
  ReleaseDC(Application.MainForm.Handle, DC);
end;

function TcaUtils.EnableDebugPrivilege(const Enable: Boolean): Boolean;
const
  PrivAttrs: array[Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
  SE_DEBUG_NAME = 'SeDebugPrivilege';
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
  ReturnLength: Cardinal;
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token) then
    begin
      TokenPriv.PrivilegeCount := 1;
      LookupPrivilegeValue(nil, SE_DEBUG_NAME, TokenPriv.Privileges[0].Luid);
      TokenPriv.Privileges[0].Attributes := PrivAttrs[Enable];
      AdjustTokenPrivileges(Token, False, TokenPriv, SizeOf(TokenPriv), nil, ReturnLength);
      Result := GetLastError = ERROR_SUCCESS;
      CloseHandle(Token);
    end;
end;

function TcaUtils.FolderExists(const AFolder: string): Boolean;
begin
  Result := DirectoryExists(AFolder);
end;

  // TcaWinVersion = (wvUnknown, wvWin95, wvWin98, wvWinNT4, wvWin2000, wvWinXP);

function TcaUtils.GetWindowsVersion: TcaWinVersion;
var
  IsWin95, IsWin98, IsWinNT, IsWin2K, IsWinXP: Boolean;
begin
  IsWin95 := (Win32MajorVersion = 4) and (Win32MinorVersion = 0) and
    (Win32Platform = VER_PLATFORM_WIN32_WINDOWS);
  IsWin98 := (Win32MajorVersion = 4) and (Win32MinorVersion = 10) and
    (Win32Platform = VER_PLATFORM_WIN32_WINDOWS);
  IsWinNT := (Win32MajorVersion = 4) and (Win32MinorVersion = 0) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
  IsWinXP := (Win32MajorVersion = 5) and (Win32MinorVersion = 1) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
  IsWin2K := (Win32MajorVersion = 5) and (Win32MinorVersion = 0) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
  Result := wvUnknown;
  if IsWin95 then Result := wvWin95 else
    if IsWin98 then Result := wvWin98 else
      if IsWinNT then Result := wvWinNT else
        if IsWinXP then Result := wvWinXP else
          if IsWin2k then Result := wvWin2K;
end;

function TcaUtils.GetEnvironmentVariable(const AName: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := Auto(TRegistry.Create(KEY_READ)).Instance;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment', False) then
    Result := Reg.ReadString(AName);
end;

function TcaUtils.GetFileVersion(const AFileName: string; ABlockCount: Word = 4): string;
var
  BufferSize: DWORD;
  FileVer: DWORD;
  FileVerUpper: DWORD;
  FileVerLower: DWORD;
  Buffer: Pointer;
  FileInfo: Pointer;
  VersionParts: array[1..4] of Word;
begin
  // Set default value 
  Result := '';
  if (ABlockCount < 1) or (ABlockCount > 4) then
    ABlockCount := 4;
  // Does a version resource exist ? 
  BufferSize := GetFileVersionInfoSize(PChar(AFileName), FileVer);
  if (BufferSize > 0) then
    begin
      GetMem(Buffer, BufferSize);
      try
        // Get information 
        GetFileVersionInfo(PChar(AFileName), 0, BufferSize, Buffer);
        VerQueryValue(Buffer, '\', FileInfo, FileVer);
        // Read version blocks 
        FileVerUpper := PVSFixedFileInfo(FileInfo)^.dwFileVersionMS;
        VersionParts[1] := HiWord(FileVerUpper);
        VersionParts[2] := LoWord(FileVerUpper);
        FileVerLower := PVSFixedFileInfo(FileInfo)^.dwFileVersionLS;
        VersionParts[3] := HiWord(FileVerLower);
        VersionParts[4] := LoWord(FileVerLower);
      finally
        FreeMem(Buffer);
      end;
      // Format result string 
      Result := IntToStr(VersionParts[1]);
      for FileVer := 2 to ABlockCount do
        Result := Result + '.' + Format('%.2d', [VersionParts[FileVer]]);
    end;
end;

function TcaUtils.GetSystemMenuFont: TFont;
var
  ncMetrics: TNonClientMetrics;
begin
  ncMetrics.cbSize := SizeOf(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(TNonClientMetrics), @ncMetrics, 0);
  Result := TFont.Create;
  Result.Handle := CreateFontIndirect(ncMetrics.lfMenuFont);
end;

function TcaUtils.GetGUIDAsString: string;
var
  Guid: TGuid;
  GuidStr: string[38];
begin
  CoCreateGuid(Guid);
  GuidStr := GUIDToString(Guid);
  SetLength(Result, 32);
  Move(GuidStr[2], Result[1], 8);
  Move(GuidStr[11], Result[9], 4);
  Move(GuidStr[16], Result[13], 4);
  Move(GuidStr[21], Result[17], 4);
  Move(GuidStr[26], Result[21], 12);
end;

function TcaUtils.GetUserName: string;
const
  MAX_LEN = 20;
var
  Size: DWORD;
begin
  SetLength(Result, MAX_LEN);
  Windows.GetUserName(PChar(Result), Size);
  SetLength(Result, Pred(Size));
end;

function TcaUtils.GetComputerName: string;
var
  Size: DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH;
  SetLength(Result, Size);
  Windows.GetComputerName(PChar(Result), Size);
  SetLength(Result, Size);
end;

function TcaUtils.GetWindowClass(Wnd: HWND): string;
var
  ClassName: string;
begin
  SetLength(ClassName, cAPIBufferLength);
  GetClassName(Wnd, PChar(ClassName), cAPIBufferLength);
  Result := ClassName;
end;

function TcaUtils.GetWindowProcess(Wnd: HWND): string;
var
  Process: TcaProcess;
  ProcessID: LongWord;
  ProcessList: TcaProcessList;
begin
  Result := '';
  if GetWindowThreadProcessId(Wnd, @ProcessID) <> 0 then
    begin
      ProcessList := TcaProcessList.Create;
      try
        Process := ProcessList.FindProcessID(ProcessID);
        if Process <> nil then
          Result := Process.ModuleFileName;
      finally
        ProcessList.Free;
      end;
    end;
end;

function TcaUtils.GetWindowsFolder: string;
var
  WinFolder: string;
  ZeroPos: Integer;
begin
  Result := '';
  SetLength(WinFolder, MAX_PATH);
  GetWindowsDirectory(PChar(WinFolder), MAX_PATH);
  ZeroPos := Self.Pos(#0, WinFolder);
  if ZeroPos > 1 then
    begin
      Result := Copy(WinFolder, 1, ZeroPos - 1);
      EnsureLastChar(Result, '\');
    end;
end;

// SW_SHOWNORMAL

function TcaUtils.WinExecAndWait32(FileName: string; Visibility: Integer): Integer;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CardinalResult: Cardinal;
  Flags: Cardinal;
begin
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  Flags := CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS;
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if CreateProcess(nil, PChar(FileName), nil, nil, False, Flags, nil, nil, StartupInfo, ProcessInfo) then
    begin
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, CardinalResult);
      Result := CardinalResult;
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end
  else
    Result := -1
end;

procedure TcaUtils.AppClose(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

procedure TcaUtils.AppMaximize(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
end;

procedure TcaUtils.AppMinimize(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

procedure TcaUtils.AppMove(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SYSCOMMAND, SC_MOVE, 0);
end;

procedure TcaUtils.AppRestore(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TcaUtils.AppSize(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SYSCOMMAND, SC_SIZE, 0);
end;

procedure TcaUtils.AppToggleNormalAndMaximized(AHandle: HWND);
var
  WindowPlacement: TWindowPlacement;
begin
  GetWindowPlacement(AHandle, @WindowPlacement);
  if WindowPlacement.showCmd = SW_SHOWNORMAL then
    AppMaximize(AHandle)
  else
    AppRestore(AHandle);
end;

procedure TcaUtils.SendKey(AHandle: HWND; AKey: Word; const shift: TShiftState; IsSpecialKey: Boolean);
type
  TShiftKeyInfo = record
    Shift: Byte;
    VKey: Byte;
  end;

  ByteSet = set of 0..7;

const
  ShiftKeys: array[1..3] of TShiftKeyInfo =
  ((Shift: Ord(ssCtrl); VKey: VK_CONTROL),
    (Shift: Ord(ssShift); VKey: VK_SHIFT),
    (Shift: Ord(ssAlt); VKey: VK_MENU));

var
  Flag: DWORD;
  bShift: ByteSet absolute shift;
  Index: Integer;
begin
  Windows.SetFocus(AHandle);
  for Index := 1 to 3 do
    begin
      if ShiftKeys[Index].Shift in bShift then
        keybd_event(ShiftKeys[Index].VKey, MapVirtualKey(ShiftKeys[Index].VKey, 0), 0, 0);
    end;
  if IsSpecialKey then
    Flag := KEYEVENTF_EXTENDEDKEY
  else
    Flag := 0;

  keybd_event(AKey, MapVirtualKey(AKey, 0), Flag, 0);
  Flag := Flag or KEYEVENTF_KEYUP;
  keybd_event(AKey, MapVirtualKey(AKey, 0), Flag, 0);

  for Index := 3 downto 1 do
    begin
      if ShiftKeys[Index].Shift in bShift then
        keybd_event(ShiftKeys[Index].VKey, MapVirtualKey(ShiftKeys[Index].VKey, 0), KEYEVENTF_KEYUP, 0);
    end;
end;

procedure TcaUtils.SetEnvironmentVariable(const AName, AValue: string; ABroadcast: Boolean = True);
var
  Reg: TRegistry;
  Rst: Cardinal;
begin
  Reg := Auto(TRegistry.Create(KEY_READ or KEY_WRITE)).Instance;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment', False) then
    begin
      Reg.WriteExpandString(AName, AValue);
      if ABroadcast then
        SendMessageTimeout(HWND_BROADCAST,
          WM_SETTINGCHANGE,
          0,
          LParam(PChar('Environment')),
          SMTO_ABORTIFHUNG,
          5000,
          Rst);
      Windows.SetEnvironmentVariable(PChar(AName), PChar(AValue));
    end;
end;

procedure TcaUtils.SetWindowStayOnTop(AHandle: HWND; IsStayOnTop: Boolean);
begin
  if IsStayOnTop then
    SetWindowPos(AHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE)
  else
    SetWindowPos(AHandle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
end;

  //---------------------------------------------------------------------------
  // Application path methods - works with EXEs and DLLs                       
  //---------------------------------------------------------------------------

function TcaUtils.AppName: string;
begin
  Result := ExtractFileName(AppPathAndName);
end;

function TcaUtils.AppPath: string;
begin
  Result := ExtractFilePath(AppPathAndName);
  if Result[Length(Result)] <> '\' then Result := Result + '\';
end;

function TcaUtils.AppPathAndName: string;
var
  Path: array[0..MAX_PATH - 1] of Char;
begin
  if IsLibrary then
    SetString(Result, Path, GetModuleFileName(HInstance, Path, SizeOf(Path)))
  else
    Result := ParamStr(0);
end;

function TcaUtils.ConfigPath: string;
begin
  Result := AppPath + cConfig + '\';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function TcaUtils.DataPath: string;
begin
  Result := AppPath + cData + '\';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function TcaUtils.ApplicationIsDelphi: Boolean;
begin
  Result := LowerCase(AppName) = 'delphi32.exe';
end;

function TcaUtils.DelphiIsRunning: Boolean;
begin
  Result := (FindWindow('TAppBuilder', nil) <> 0) and (DebugHook <> 0);
end;

function TcaUtils.IsTestApp: Boolean;
begin
  Result := Self.Pos('Test', AppName) > 0;
end;

  //---------------------------------------------------------------------------
  // IEEE-754 Support for NANs and INFs                                        
  //---------------------------------------------------------------------------

const
  NANQuietBits: Int64 = Int64($7FFFFFFFFFFFFFFF);
  PositiveInfinityBits: Int64 = Int64($7FF0000000000000);
  NegativeInfinityBits: Int64 = Int64($FFF0000000000000);
  NonInfinityBits: Int64 = Int64($000FFFFFFFFFFFFF);

var
  NANQuiet: Double absolute NANQuietBits;

function TcaUtils.DoubleToHex(const N: Double): string;
var
  Overlay: array[1..2] of Integer absolute N;
begin
  Result := IntToHex(Overlay[2], 8) + IntToHex(Overlay[1], 8);
end;

function TcaUtils.HexToDouble(const Hex: string): Double;
var
  N: Double;
  Overlay: array[1..2] of Integer absolute N;
begin
  if Length(Hex) <> 16
    then raise EcaIEEEMath.Create('Invalid hex string for HexToDouble');
  Overlay[1] := StrToInt('$' + Copy(Hex, 9, 8));
  Overlay[2] := StrToInt('$' + Copy(Hex, 1, 8));
  Result := N;
end;

function TcaUtils.IsInfinity(const N: Double): Boolean;
var
  Overlay: Int64 absolute N;
begin
  Result := (Overlay and PositiveInfinityBits) = PositiveInfinityBits;
end;

function TcaUtils.IsNAN(const N: Double): Boolean;
var
  Overlay: Int64 absolute N;
begin
  Result := ((Overlay and PositiveInfinityBits) = PositiveInfinityBits) and
    ((Overlay and NonInfinityBits) <> 0);
end;

function TcaUtils.NAN: Double;
begin
  Result := NANQuiet;
end;

function TcaUtils.NegativeInfinity: Double;
var
  N: Double absolute NegativeInfinityBits;
begin
  Result := N;
end;

function TcaUtils.PositiveInfinity: Double;
var
  N: Double absolute PositiveInfinityBits;
begin
  Result := N;
end;

  //---------------------------------------------------------------------------
  // Excel related methods                                                     
  //---------------------------------------------------------------------------

function TcaUtils.A1Ref(C: Byte; R: Integer): string;
var
  Hi, Lo: Byte;
  HiLo: string;
begin
  Hi := 0;
  while C > 26 do
    begin
      Inc(Hi);
      Dec(C, 26);
    end;
  Lo := C;
  HiLo := '';
  if Hi > 0 then
    HiLo := HiLo + Chr(Hi + $40);
  HiLo := HiLo + Chr(Lo + $40);
  if R > 0 then
    Result := Format('%s%d', [HiLo, R])
  else
    Result := HiLo;
end;

function TcaUtils.CRRef(const A1Ref: string): TcaColRow;
var
  A1Char: Char;
  AlphaPart: string;
  ColValue: Integer;
  Index: Integer;
  Mult26: Integer;
  NumPart: string;
begin
  AlphaPart := '';
  NumPart := '';
  Result.Col := 0;
  Result.Row := 0;
  for Index := 1 to Length(A1Ref) do
    begin
      A1Char := UpCase(A1Ref[Index]);
      if A1Char in ['A'..'Z'] then
        AlphaPart := AlphaPart + A1Char
      else
        begin
          if A1Char in ['0'..'9'] then
            NumPart := NumPart + A1Char;
        end;
    end;
  if Length(AlphaPart) > 0 then
    begin
      Mult26 := 0;
      for Index := Length(AlphaPart) downto 1 do
        begin
          ColValue := Ord(AlphaPart[Index]) - Ord('A') + 1;
          Result.Col := Result.Col + ColValue * Mult26;
          Mult26 := (Length(AlphaPart) - Index) * 26;
        end;
    end
end;

  //---------------------------------------------------------------------------
  // Component / TPersistent support                                           
  //---------------------------------------------------------------------------

function TcaUtils.CopyPublishedProperties(Src, Dest: TPersistent; CopyClassPointers, CopyChildren: Boolean): Boolean;
var
  DestChild: TPersistent;
  Index: Integer;
  PropName: string;
  RTTIItem: TcaRTTIItem;
  RTTIList: IcaRTTIList;
  SrcChild: TPersistent;
begin
  Result := False;
  RTTIList := TcaRTTIList.Create(Src);
  for Index := 0 to Pred(RTTIList.Count) do
    begin
      RTTIItem := RTTIList[Index];
      PropName := RTTIItem.PropName;
      if IsPublishedProp(Dest, PropName) then
        begin
          case RTTIItem.PropType of
            tkInteger..tkString, tkLString:
              SetPropValue(Dest, PropName, GetPropValue(Src, PropName));
            tkClass:
              if CopyChildren then
                begin
                  SrcChild := TPersistent(GetOrdProp(Src, PropName));
                  DestChild := TPersistent(GetOrdProp(Dest, PropName));
                  if (SrcChild <> nil) and (DestChild <> nil) then
                    // Recursive call to CopyPublishedProperties 
                    CopyPublishedProperties(SrcChild, DestChild, False, True);
                end
              else
                if CopyClassPointers then
                  SetOrdProp(Dest, PropName, GetOrdProp(Src, PropName));
            tkMethod:
              if CopyClassPointers then SetMethodProp(Dest, PropName, GetMethodProp(Src, PropName));
          end;
          Result := True;
        end;
    end;
end;

function TcaUtils.DeepCopy(Src, Dest: TPersistent): Boolean;
begin
  Result := CopyPublishedProperties(Src, Dest, False, True);
end;

function TcaUtils.PersistentAsString(AObject: TPersistent): string;
var
  Child: TPersistent;
  Index: Integer;
  List: TStringList;
  ObjectName: string;
  PropName: string;
  PropValue: string;
  RTTIItem: TcaRTTIItem;
  RTTIList: IcaRTTIList;
begin
  List := TStringList.Create;
  try
    RTTIList := TcaRTTIList.Create(AObject);
    ObjectName := '<object>';
    if AObject is TComponent then
      ObjectName := TComponent(AObject).Name;
    List.Add(ObjectName + ': ' + AObject.ClassName);
    for Index := 0 to Pred(RTTIList.Count) do
      begin
        RTTIItem := RTTIList[Index];
        if RTTIItem.PropType = tkClass then
          begin
            Child := TPersistent(GetOrdProp(AObject, RTTIItem.PropName));
            // Recursive call to PersistentAsString 
            List.Text := List.Text + PersistentAsString(Child);
          end
        else
          begin
            PropName := RTTIItem.PropName;
            PropValue := RTTIItem.PropValueAsString;
            List.Values[PropName] := PropValue;
          end;
      end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TcaUtils.PublishedPropertiesMatch(AItem1, AItem2: TPersistent; ACompareClassPointers: Boolean = False): Boolean;
var
  Index: Integer;
  Item1Class: Longint;
  Item1Method: TMethod;
  Item1Prop: Variant;
  Item2Class: Longint;
  Item2Method: TMethod;
  Item2Prop: Variant;
  PropInfo: PPropInfo;
  PropList: PPropList;
  PropName: string;
  PropType: TTypeKind;
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
begin
  Result := False;
  if AItem1.ClassType = AItem1.ClassType then
    begin
      Result := True;
      TypeInfo := AItem1.ClassInfo;
      TypeData := GetTypeData(TypeInfo);
      if TypeData.PropCount <> 0 then
        begin
          GetMem(PropList, SizeOf(PPropInfo) * TypeData.PropCount);
          try
            GetPropInfos(TypeInfo, PropList);
            for Index := 0 to TypeData.PropCount - 1 do
              begin
                // Get RTTI for each published object property 
                PropInfo := PropList[Index];
                PropName := PropInfo^.Name;
                PropType := PropInfo^.PropType^.Kind;
                case PropType of
                  tkInteger..tkString, tkLString:
                    begin
                      Item1Prop := GetPropValue(AItem1, PropName);
                      Item2Prop := GetPropValue(AItem2, PropName);
                      if Item1Prop <> Item2Prop then
                        begin
                          Result := False;
                          Break;
                        end;
                    end;
                  tkClass:
                    if ACompareClassPointers then
                      begin
                        Item1Class := GetOrdProp(AItem1, PropName);
                        Item2Class := GetOrdProp(AItem2, PropName);
                        if Item1Class <> Item2Class then
                          begin
                            Result := False;
                            Break;
                          end;
                      end;
                  tkMethod:
                    if ACompareClassPointers then
                      begin
                        Item1Method := GetMethodProp(AItem1, PropName);
                        Item2Method := GetMethodProp(AItem2, PropName);
                        if (Item1Method.Code <> Item2Method.Code)
                          or (Item1Method.Data <> Item2Method.Data) then
                          begin
                            Result := False;
                            Break;
                          end;
                      end;
                end;
              end;
          finally
            FreeMem(PropList);
          end;
        end;
    end;
end;

function TcaUtils.ShallowCopy(Src, Dest: TPersistent; CopyClassPointers: Boolean = False): Boolean;
begin
  Result := CopyPublishedProperties(Src, Dest, CopyClassPointers, False);
end;

  // Design by Contract constructs 

function TcaUtils.FailsAll(AConditions: array of Boolean): Boolean;
var
  Index: Integer;
begin
  Result := True;
  for Index := Low(AConditions) to High(AConditions) do
    if AConditions[Index] then
      begin
        Result := False;
        Break;
      end;
end;

function TcaUtils.FailsAny(AConditions: array of Boolean): Boolean;
var
  Index: Integer;
begin
  Result := True;
  for Index := Low(AConditions) to High(AConditions) do
    if not AConditions[Index] then
      begin
        Result := False;
        Break;
      end;
end;

  // Interface support 

  // Thanks to  Hallvard Vassbotn (hallvard.vassbotn@c2i.net) for this
  // See http://hallvards.blogspot.com/2006/09/hack11-get-guid-of-interface-reference.html for latest bloggage!

function TcaUtils.GetImplementingObject(const Intf: IUnknown): TObject;
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint

type

  PAdjustSelfThunk = ^TAdjustSelfThunk;

  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte: (AdjustmentByte: shortint);
      AddLong: (AdjustmentLong: longint);
  end;

  PInterfaceMT = ^TInterfaceMT;

  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;

  TInterfaceRef = ^PInterfaceMT;

var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := Pointer(Intf);
  if Assigned(Result) then
  try
    QueryInterfaceThunk := TInterfaceRef(Intf)^.QueryInterfaceThunk;
    case QueryInterfaceThunk.AddInstruction of
      AddByte: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte);
      AddLong: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong);
    else
      Result := nil;
    end;
  except
    Result := nil;
  end;
end;

  // Method support 

function TcaUtils.MethodsMatch(AMethod1, AMethod2: TMethod): Boolean;
begin
  Result := Int64(AMethod1) = Int64(AMethod2);
end;

  // Time support methods 

function TcaUtils.CurrentYear: Word;
begin
  Result := Self.Now.Year;
end;

function TcaUtils.DateTimeToTimePoint(ADateTime: TDateTime): TcaTimePoint;
begin
  DecodeDate(ADateTime, Result.Year, Result.Month, Result.Day);
  DecodeTime(ADateTime, Result.Hour, Result.Min, Result.Sec, Result.MSec);
end;

function TcaUtils.Now: TcaTimePoint;
var
  NowDT: TDateTime;
begin
  NowDT := Sysutils.Now;
  Result := DateTimeToTimePoint(NowDT);
end;

  // Message Dialog methods 

var
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help');

  //----------------------------
  // *!*!*!* HACK ALERT *!*!*!* 
  //----------------------------

function TcaUtils.MessageDialog(const ACaption, AMsg: string; ADlgType: TMsgDlgType;
  AButtonCaptions: array of string; ADefaultIndex: Integer; AHelpCtx: Longint): TcaMessageDialogResponse;
var
  Button: TButton;
  Buttons: TMsgDlgButtons;
  Dlg: TForm;
  Index: Integer;
  Response: Integer;
  TotalBtnWidth: Integer;
  MaxWidth: Integer;
  TextWidth: Integer;
  DummyPanel: TPanel;

  function Button1: TButton;
  begin
    Result := TButton(Dlg.FindChildControl(ButtonNames[mbYes]));
  end;

  function Button2: TButton;
  begin
    Result := TButton(Dlg.FindChildControl(ButtonNames[mbNo]));
  end;

  function Button3: TButton;
  begin
    Result := TButton(Dlg.FindChildControl(ButtonNames[mbOK]));
  end;

  procedure SetButtonCaptions;
  begin
    Dlg.Caption := ACaption;
    if mbYes in Buttons then
      Button1.Caption := AButtonCaptions[0];
    if mbNo in Buttons then
      Button2.Caption := AButtonCaptions[1];
    if mbOK in Buttons then
      Button3.Caption := AButtonCaptions[2];
  end;

  procedure SetDefaultButton;
  begin
    if mbYes in Buttons then
      Button1.Default := False;
    if mbNo in Buttons then
      Button2.Default := False;
    if mbOK in Buttons then
      Button3.Default := False;
    Button := TButton(Dlg.FindChildControl(ButtonNames[TMsgDlgBtn(ADefaultIndex)]));
    if Assigned(Button) then
      begin
        Button.Default := True;
        Dlg.ActiveControl := nil;
      end;
  end;

  procedure CalculateMaxButtonWidth;
  begin
    Dlg.Canvas.Font := Dlg.Font;
    MaxWidth := 0;
    if mbYes in Buttons then
      begin
        TextWidth := Dlg.Canvas.TextWidth(Button1.Caption) + 20;
        MaxWidth := Max(MaxWidth, TextWidth);
      end;
    if mbNo in Buttons then
      begin
        TextWidth := Dlg.Canvas.TextWidth(Button2.Caption) + 20;
        MaxWidth := Max(MaxWidth, TextWidth);
      end;
    if mbOK in Buttons then
      begin
        TextWidth := Dlg.Canvas.TextWidth(Button3.Caption) + 20;
        MaxWidth := Max(MaxWidth, TextWidth);
      end;
  end;

  procedure SetButtonWidths;
  begin
    if mbYes in Buttons then
      Button1.Width := MaxWidth;
    if mbNo in Buttons then
      Button2.Width := MaxWidth;
    if mbOK in Buttons then
      Button3.Width := MaxWidth;
  end;

  procedure CalculateTotalButtonWidth;
  begin
    TotalBtnWidth := 0;
    if mbYes in Buttons then
      Inc(TotalBtnWidth, MaxWidth);
    if mbNo in Buttons then
      Inc(TotalBtnWidth, MaxWidth);
    if mbOK in Buttons then
      Inc(TotalBtnWidth, MaxWidth);
  end;

  procedure CalculateDialogWidth;
  begin
    if (TotalBtnWidth + 20) > Dlg.ClientWidth then
      Dlg.ClientWidth := TotalBtnWidth + High(AButtonCaptions) * 4 + 20;
  end;

  procedure CalculateButtonPositions;
  begin
    case High(AButtonCaptions) of
      0: Button1.Left := (Dlg.ClientWidth div 2) - (Button1.Width div 2);
      1:
        begin
          Button1.Left := (Dlg.ClientWidth div 2) - Button1.Width - 2;
          Button2.Left := (Dlg.ClientWidth div 2) + 2;
        end;
      2:
        begin
          Button2.Left := (Dlg.ClientWidth div 2) - (Button1.Width div 2);
          Button1.Left := Button2.Left - Button1.Width - 4;
          Button3.Left := Button2.Left + Button2.Width + 4;
        end;
    end;
  end;

  procedure CreateDummyFocusPanel;
  begin
    if mbYes in Buttons then
      begin
        DummyPanel := TPanel.Create(Dlg);
        DummyPanel.Parent := Dlg;
        DummyPanel.Visible := True;
        DummyPanel.BoundsRect := Button1.BoundsRect;
        DummyPanel.Left := DummyPanel.Left + 1;
        DummyPanel.Width := DummyPanel.Width - 2;
        DummyPanel.Top := DummyPanel.Top + 1;
        DummyPanel.Height := DummyPanel.Height - 2;
        Dlg.ActiveControl := DummyPanel;
        DummyPanel.SendToBack;
      end;
  end;

begin
  Buttons := [];
  for Index := Low(AButtonCaptions) to High(AButtonCaptions) do
    begin
      if Index = 3 then Break;
      case Index of
        0: Include(Buttons, mbYes);
        1: Include(Buttons, mbNo);
        2: Include(Buttons, mbOK);
      end;
    end;
  Dlg := CreateMessageDialog(AMsg, ADlgType, Buttons);
  try
    SetButtonCaptions;
    SetDefaultButton;
    CalculateMaxButtonWidth;
    SetButtonWidths;
    CalculateTotalButtonWidth;
    CalculateDialogWidth;
    CalculateButtonPositions;
    CreateDummyFocusPanel;
    Dlg.Position := poScreenCenter;
    Response := Dlg.ShowModal;
    case Response of
      mrYes: Result := drFirst;
      mrNo: Result := drSecond;
      mrOk: Result := drThird;
    else
      Result := drUndefined;
    end;
  finally
    Dlg.Free;
  end;
end;

  // Visual control support 

type
  TControlEx = class(TControl);

procedure TcaUtils.SetControlFont(AControl: TControl; AColor: TColor; ASize: Integer; AStyle: TFontStyles);
var
  Control: TControlEx;
begin
  if Assigned(AControl) then
    begin
      Control := TControlEx(AControl);
      Control.Font.Color := AColor;
      Control.Font.Size := ASize;
      Control.Font.Style := AStyle;
    end;
end;


  // Clipboard Methods

procedure TcaUtils.CopyDatasetToClipboard(ADataset: TDataset);
  function GetFieldNameList(ADataset: TDataset; ADelimiter: string): string;
  var
    FieldNameString: string;
    Index: integer;
  begin
    FieldNameString := '';
    for Index := 0 to pred(ADataset.FieldCount) do
      FieldNameString := FieldNameString + ADataset.Fields[Index].FieldName + ADelimiter;
    Utils.DeleteFromEnd(FieldNameString, 1);
    Result := FieldNameString;
  end;

var
  RowString: string;
  ClipStrings: TStringList;
  Bookmark: string;
  Index: integer;
  Delimiter: string;
begin
  Delimiter := #9;
  ClipStrings := TStringList.Create;
  Bookmark := ADataset.Bookmark;
  ADataset.DisableControls;
  try
    ClipStrings.Add(GetFieldNameList(ADataset, Delimiter));
    ADataset.First;
    while not ADataset.Eof do
    begin
      RowString := '';
      for Index := 0 to pred(ADataset.FieldCount) do
        RowString := RowString + ADataset.Fields[Index].AsString + Delimiter;
      Utils.DeleteFromEnd(RowString, 1);
      ClipStrings.Add(RowString);
      ADataset.Next;
    end;    // while
    Clipboard.AsText := ClipStrings.Text;
  finally
    ADataset.Bookmark := Bookmark;
    ADataset.EnableControls;
    ClipStrings.Free;
  end;
end;


  //---------------------------------------------------------------------------
  // IcaKeys                                                                   
  //---------------------------------------------------------------------------

  // Private methods (IcaKeys) 

procedure TcaUtils.InitializeKeys;
begin
  FAllKeys := [0..255];
  FControlKeys := [VK_BACK, VK_TAB, VK_RETURN, VK_LEFT, VK_RIGHT, VK_HOME,
    VK_END, VK_INSERT, VK_DELETE, VK_F4, VK_ESCAPE];
  FIntegerKeys := [VK_0..VK_9, VK_N0..VK_N9, VK_COMMA, VK_ADD, VK_SUBTRACT, 189] + FControlKeys;
  FHexKeys := FIntegerKeys + [VK_A..VK_F];
  FDecimalKeys := [VK_PERIOD, VK_DECIMAL];
  FNumericKeys := FIntegerKeys + FDecimalKeys;
  FAlphaKeys := FAllKeys - FNumericKeys + FControlKeys;
  FVisibleKeys := FAllKeys - FControlKeys;
end;

  // Property methods (IcaKeys) 

function TcaUtils.GetAllKeys: TcaByteSet;
begin
  Result := FAllKeys;
end;

function TcaUtils.GetAlphaKeys: TcaByteSet;
begin
  Result := FAlphaKeys;
end;

function TcaUtils.GetControlKeys: TcaByteSet;
begin
  Result := FControlKeys;
end;

function TcaUtils.GetDecimalKeys: TcaByteSet;
begin
  Result := FDecimalKeys;
end;

function TcaUtils.GetHexKeys: TcaByteSet;
begin
  Result := FHexKeys;
end;

function TcaUtils.GetIntegerKeys: TcaByteSet;
begin
  Result := FIntegerKeys;
end;

function TcaUtils.GetNumericKeys: TcaByteSet;
begin
  Result := FNumericKeys;
end;

function TcaUtils.GetVisibleKeys: TcaByteSet;
begin
  Result := FVisibleKeys;
end;

  //---------------------------------------------------------------------------
  // IcaMathUtils                                                              
  //---------------------------------------------------------------------------

procedure TcaUtils.CheckZeroTolerance;
begin
  if FZeroTolerance = 0 then FZeroTolerance := cDefaultZeroTolerance;
end;

  // Property methods (IcaMathUtils) 

function TcaUtils.GetUseCheck8087: Boolean;
begin
  Result := FUseCheck8087;
end;

function TcaUtils.GetZeroTolerance: Extended;
begin
  CheckZeroTolerance;
  Result := FZeroTolerance;
end;

procedure TcaUtils.SetUseCheck8087(const Value: Boolean);
begin
  FUseCheck8087 := Value;
end;

procedure TcaUtils.SetZeroTolerance(const Value: Extended);
begin
  FZeroTolerance := Value;
end;

  //---------------------------------------------------------------------------
  // IcaParser                                                                 
  //---------------------------------------------------------------------------

  // Public methods (IcaParser) 

function TcaUtils.GetToken(Index: Integer; FromEnd: Boolean = False): string;
begin
  Result := '';
  if FTokens.Count = 0 then GetTokens(FTokens);
  if FTokens.Count > 0 then
    if (Index >= 0) and (Index < FTokens.Count) then
      begin
        if FromEnd then
          Result := FTokens[FTokens.Count - Index - 1]
        else
          Result := FTokens[Index]
      end;
end;

function TcaUtils.HasMoreTokens: Boolean;
begin
  if FTokens.Count = 0 then GetTokens(FTokens);
  Result := FTokenIndex + 1 < FTokens.Count;
end;

function TcaUtils.NextToken: string;
var
  NewTokenIndex: Integer;
begin
  Result := '';
  if FTokens.Count = 0 then GetTokens(FTokens);
  if FTokens.Count > 0 then
    begin
      NewTokenIndex := FTokenIndex;
      Inc(NewTokenIndex);
      if NewTokenIndex < FTokens.Count then
        begin
          FTokenIndex := NewTokenIndex;
          Result := FTokens[FTokenIndex];
        end;
    end;
end;

procedure TcaUtils.GetTokens(TokenList: TStrings);
var
  S: string;
  Token: string;
  Ch: Char;
  ChIndex, ChPos: Integer;
  OldDelims: string;
begin
  S := FStringToParse;
  ReplaceChar(S, #10, #13);
  ReplaceChar(S, #13, #32);
  OldDelims := FTokenDelimiters;
  try
    if FastCharPos(FTokenDelimiters, '|') = 0 then
      FTokenDelimiters := FTokenDelimiters + '|';
    TokenList.Clear;
    if Length(S) > 0 then
      begin
        Ch := S[Length(S)];
        if FastCharPos(FTokenDelimiters, Ch) = 0 then
          S := S + '|';
        ChPos := 1;
        for ChIndex := 1 to Length(S) do
          begin
            Ch := S[ChIndex];
            if FastCharPos(FTokenDelimiters, Ch) > 0 then
              begin
                Token := Trim(Copy(S, ChPos, ChIndex - ChPos));
                if FIgnoreBlanks then
                  begin
                    if Length(Token) > 0 then
                      TokenList.AddObject(Token, Pointer(ChIndex));
                  end
                else
                  TokenList.AddObject(Token, Pointer(ChIndex));
                ChPos := ChIndex + 1;
              end;
          end;
      end;
  finally
    FTokenDelimiters := OldDelims;
  end;
end;

procedure TcaUtils.GetTokens(TokenList: IcaStringList);
begin
  GetTokens(TokenList.GetStrings);
end;

  // Private methods (IcaParser) 

procedure TcaUtils.InitializeParser;
begin
  FTokens.Clear;
  FTokenDelimiters := cDefaultTokenDelimiters;
  FTokenIndex := -1;
end;

  // Property methods (IcaParser) 

function TcaUtils.GetIgnoreBlanks: Boolean;
begin
  Result := FIgnoreBlanks;
end;

function TcaUtils.GetStringToParse: string;
begin
  Result := FStringToParse;
end;

function TcaUtils.GetTokenCount: Integer;
begin
  if FTokens.Count = 0 then GetTokens(FTokens);
  Result := FTokens.Count;
end;

function TcaUtils.GetTokenDelimiters: string;
begin
  Result := FTokenDelimiters;
end;

procedure TcaUtils.SetIgnoreBlanks(const Value: Boolean);
begin
  FIgnoreBlanks := Value;
end;

procedure TcaUtils.SetStringToParse(const Value: string);
begin
  FStringToParse := Value;
end;

procedure TcaUtils.SetTokenDelimiters(const Value: string);
begin
  FTokenDelimiters := Value;
end;

  // Event property methods 

function TcaUtils.GetOnContinueFileSearch: TcaFileSearchEvent;
begin
  Result := FOnContinueFileSearch;
end;

procedure TcaUtils.SetOnContinueFileSearch(const Value: TcaFileSearchEvent);
begin
  FOnContinueFileSearch := Value;
end;

  // IcaMouseUtils - Public methods 

procedure TcaUtils.MouseStateChanged(AMouseDownButton: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, AMouseDownButton, Shift, X, Y);
end;

  // IcaMouseUtils - Event handlers 

procedure TcaUtils.MouseTrackerTimerEvent(Sender: TObject);
var
  Button: TMouseButton;
  CursorPos: TPoint;
  NewMouseIsDown: Boolean;
  Shift: TShiftState;
begin
  GetCursorPos(CursorPos);
  NewMouseIsDown := GetButtonDown;
  if NewMouseIsDown and (NewMouseIsDown <> FMouseIsDown) then
    begin
      Button := mbLeft;
      if not GetLeftButtonDown then
        begin
          if GetRightButtonDown then
            Button := mbRight
          else
            Button := mbMiddle;
        end;
      Shift := [];
      if ShiftKeyDown then Include(Shift, ssShift);
      if AltKeyDown then Include(Shift, ssAlt);
      if ControlKeyDown then Include(Shift, ssCtrl);
      case Button of
        mbLeft: Include(Shift, ssLeft);
        mbRight: Include(Shift, ssRight);
        mbMiddle: Include(Shift, ssMiddle);
      end;
      MouseStateChanged(Button, Shift, CursorPos.x, CursorPos.y);
    end;
  FMouseIsDown := NewMouseIsDown;
end;

  // IcaMouseUtils - Property methods 

function TcaUtils.GetButtonDown: Boolean;
begin
  Result := GetLeftButtonDown or GetMiddleButtonDown or GetRightButtonDown;
end;

function TcaUtils.GetLeftButtonDown: Boolean;
begin
  Result := IsKeyDown(VK_LBUTTON);
end;

function TcaUtils.GetMiddleButtonDown: Boolean;
begin
  Result := IsKeyDown(VK_MBUTTON);
end;

function TcaUtils.GetMouseTrackerActive: Boolean;
begin
  Result := FMouseTrackerTimer.Enabled;
end;

function TcaUtils.GetRightButtonDown: Boolean;
begin
  Result := IsKeyDown(VK_RBUTTON);
end;

procedure TcaUtils.SetMouseTrackerActive(const Value: Boolean);
begin
  FMouseTrackerTimer.Enabled := Value;
end;

  // IcaMouseUtils - Event property methods 

function TcaUtils.GetOnMouseDown: TMouseEvent;
begin
  Result := FOnMouseDown;
end;

procedure TcaUtils.SetOnMouseDown(const Value: TMouseEvent);
begin
  FOnMouseDown := Value;
end;

  // Private methods - Delphi replacements 

function TcaUtils.Pos(Substr: string; S: string): Integer;
begin
  Result := Self.FastPos(S, Substr, 1, True);
end;

  //---------------------------------------------------------------------------
  // CoUtilsFactory                                                            
  //---------------------------------------------------------------------------

class function CoUtilsFactory.Instance: IcaUtils;
const
  FInstance: IcaUtils = nil;
begin
  if not Assigned(FInstance) then
    FInstance := TcaUtils.Create;
  Result := FInstance;
end;

  //```````````````````````````````````````````````````````````````````````````
  // Initialization/Finalization                                               
  //```````````````````````````````````````````````````````````````````````````

initialization
  Utils := CoUtilsFactory.Instance;

end.

