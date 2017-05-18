unit caClasses;

{$INCLUDE ca.inc}

{.$DEFINE LOG_CREATE_DESTROY}
{.$DEFINE LOG_REF_COUNT}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,

  // ca units 
  caLog;

type

  {$IFDEF D5}
  IInterface = IUnknown;
  {$ENDIF}

  TcaColRow = packed record
    Col: Integer;
    Row: Integer;
  end;

  EcaException = class(Exception);

  TcaInterfacedPersistent = class;

  TcaProcedure = procedure of object;

  TcaNotifyEvent = procedure(const Sender: IInterface) of object;

  //---------------------------------------------------------------------------
  // IcaAssignable                                                             
  //---------------------------------------------------------------------------

  IcaAssignable = interface
  ['{C0C4B0D1-57DA-405D-AD25-7B071D611E15}']
    // Public methods 
    procedure AssignFromInterface(Source: IUnknown);
    procedure AssignFromObject(Source: TPersistent);
  end;

  //---------------------------------------------------------------------------
  // IcaCloneable                                                              
  //---------------------------------------------------------------------------

  IcaCloneable = interface
  ['{478BF36E-F5DD-49A8-B680-EC85FAA7FBBC}']
    // Public methods 
    function CloneAsInterface: IUnknown;
    function CloneAsObject: TcaInterfacedPersistent;
  end;

  //----------------------------------------------------------------------------
  // TcaInterfacedPersistent                                                    
  //                                                                            
  // A base for Persistent Interfaces descendents                               
  //                                                                            
  // Note that in D5 (at least), any persistent properties intended for display 
  // in the Object Inspector must still be created as object instances, not as  
  // interfaces. Hopefully this will change in D6/Kylix.                        
  //----------------------------------------------------------------------------

  TcaInterfacedPersistent = class(TPersistent, IUnknown, IcaCloneable, IcaAssignable)
  private
    {$IFDEF LOG_CREATE_DESTROY}
    FLogCreateAndDestroy: Boolean;
    {$ENDIF}
    {$IFDEF LOG_REF_COUNT}
    FLogRefCount: Boolean;
    {$ENDIF}
  protected
    // Interface support 
    FNoRefCount: Boolean;
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    // Owned list object support 
    procedure ClearListObjects(AList: TList);
  public
    constructor CreateNonRefCounted;
    // Overridden public methods 
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    // IcaAssignable 
    procedure AssignFromInterface(Source: IUnknown); virtual;
    procedure AssignFromObject(Source: TPersistent); virtual;
    // IcaCloneable 
    function CloneAsInterface: IUnknown; virtual;
    function CloneAsObject: TcaInterfacedPersistent; virtual;
    // Properties 
    {$IFDEF LOG_CREATE_DESTROY}
    property LogCreateAndDestroy: Boolean read FLogCreateAndDestroy write FLogCreateAndDestroy;
    {$ENDIF}
    {$IFDEF LOG_REF_COUNT}
    property LogRefCount: Boolean read FLogRefCount write FLogRefCount;
    {$ENDIF}
    property RefCount: Integer read FRefCount;
  end;

  TcaInterfacedPersistentClass = class of TcaInterfacedPersistent;

  //----------------------------------------------------------------------------
  // IcaString                                                                  
  //----------------------------------------------------------------------------

  IcaString = interface
  ['{48FDBB25-74B3-4A99-B048-0C36ADC0F1F4}']
    // Property methods 
    function GetS: String;
    procedure SetS(const Value: String);
    // Interface methods 
    function EndsWith(const AValue: string): Boolean;
    function IsNumChar(N: Integer): Boolean;
    function Left(N: Integer): String;
    function Length: Integer;
    function LowerCase: string;
    function Mid(N, ForN: Integer): String;
    function PadLeft(ALength: Integer): String;
    function PadRight(ALength: Integer): String;
    function PosFromEnd(const AFindStr: String): Integer;
    function PosFromStart(const AFindStr: String): Integer;
    function PreZero(ALength: Integer): String;
    function Reverse: String;
    function Right(const S: string; N: Integer): String;
    function Str2Float(const S: string; Default: Extended): Extended;
    function Str2Int(const S: string; Default: Integer): Integer;
    function UpperCase: string;
    procedure Add(const S: String);
    procedure CleanIntString(var S: string);
    procedure CleanNumString(var S: string);
    procedure DeleteFromEnd(var S: string; N: Integer);
    procedure DeleteFromStart(var S: string; N: Integer);
    procedure SplitCamelCaps;
    procedure StripChar(C: Char);
    // Properties 
    property S: String read GetS write SetS;
  end;

  //----------------------------------------------------------------------------
  // TcaString                                                                  
  //----------------------------------------------------------------------------

  TcaString = class(TInterfacedObject, IcaString)
  private
    // Property fields 
    FS: String;
    // Property methods 
    function GetS: String;
    procedure SetS(const Value: String);
  public
    // Create/Destroy 
    constructor Create(AString: String); overload;
    constructor Create(AInteger: Integer); overload;
    // Interface methods 
    function EndsWith(const AValue: string): Boolean;
    function IsNumChar(N: Integer): Boolean;
    function Left(N: Integer): String;
    function Length: Integer;
    function LowerCase: string;
    function Mid(N, ForN: Integer): String;
    function PadLeft(ALength: Integer): String;
    function PadRight(ALength: Integer): String;
    function PosFromEnd(const AFindStr: String): Integer;
    function PosFromStart(const AFindStr: String): Integer;
    function PreZero(ALength: Integer): String;
    function Ref: Integer;
    function Reverse: String;
    function Right(N: Integer): String;
    function UpperCase: string;
    procedure Add(const S: String);
    procedure DeleteFromEnd(N: Integer);
    procedure DeleteFromStart(N: Integer);
    procedure SplitCamelCaps;
    // Properties 
    property S: String read GetS write SetS;
  end;

  //----------------------------------------------------------------------------
  // IcaByteString                                                              
  //----------------------------------------------------------------------------

  IcaByteString = interface
  ['{9BCD9406-CE4F-420A-A6CD-6383DFDDAE98}']
    // Property methods 
    function GetAsString: string;
    function GetByte(Index: Integer): Byte;
    function GetCount: Integer;
    procedure SetAsString(const Value: string);
    procedure SetByte(Index: Integer; const Value: Byte);
    procedure SetCount(const Value: Integer);
    // Interface methods 
    procedure Clear;
    // Properties 
    property AsString: string read GetAsString write SetAsString;
    property Bytes[Index: Integer]: Byte read GetByte write SetByte;
    property Count: Integer read GetCount write SetCount;
  end;

  //----------------------------------------------------------------------------
  // TcaByteString                                                              
  //----------------------------------------------------------------------------

  TcaByteString = class(TcaString, IcaByteString)
  private
    // Property methods 
    function GetAsString: string;
    function GetByte(Index: Integer): Byte;
    function GetCount: Integer;
    procedure SetAsString(const Value: string);
    procedure SetByte(Index: Integer; const Value: Byte);
    procedure SetCount(const Value: Integer);
    // Private methods 
    function ByteToStr(AByte: Byte): string;
    procedure CheckIndex(Index: Integer);
    procedure GetAsStrings(AStrings: TStrings);
    procedure SetAsStrings(AStrings: TStrings);
  public
    // Create/Destroy 
    constructor Create(ACount: Integer); overload;
    // Interface methods 
    procedure Clear;
    // Properties 
    property AsString: string read GetAsString write SetAsString;
    property Bytes[Index: Integer]: Byte read GetByte write SetByte;
    property Count: Integer read GetCount write SetCount;
  end;

  //----------------------------------------------------------------------------
  // IcaStringList                                                              
  //----------------------------------------------------------------------------

  IcaStringList = interface
  ['{B4DCD412-9B9B-4C52-8AC8-B4F5144750F2}']
    // TStrings private 
    function GetAdapter: IStringsAdapter;
    function GetCommaText: string;
    function GetDuplicates: TDuplicates;
    function GetName(Index: Integer): string;
    function GetSorted: Boolean;
    function GetValue(const Name: string): string;
    procedure SetCommaText(const Value: string);
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetSorted(const Value: Boolean);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: string);
    // TStrings protected 
    function Get(Index: Integer): string;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetObject(Index: Integer): TObject;
    function GetTextStr: string;
    procedure DefineProperties(Filer: TFiler);
    procedure Put(Index: Integer; const S: string);
    procedure PutObject(Index: Integer; AObject: TObject);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetTextStr(const Value: string);
    procedure SetUpdateState(Updating: Boolean);
    // TStrings public 
    function Add(const S: string): Integer; overload;
    function Add(const S: string; const Args: array of const): Integer; overload;
    function AddObject(const S: string; AObject: TObject): Integer;
    function Equals(Strings: TStrings): Boolean;
    function GetText: PChar;
    function IndexOf(const S: string): Integer;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure AddStrings(AStrings: TStrings); overload;
    procedure AddStrings(AStrings: IcaStringList); overload;
    procedure AddStrings(AStrings: array of string); overload;
    procedure Append(const S: string);
    procedure Assign(Source: TPersistent);
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const S: string);
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetText(Text: PChar);
    procedure Sort;
    // Properties 
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Sorted: Boolean read GetSorted write SetSorted;
    property Strings[Index: Integer]: string read Get write Put; default;
    property StringsAdapter: IStringsAdapter read GetAdapter write SetStringsAdapter;
    property Text: string read GetTextStr write SetTextStr;
    property Values[const Name: string]: string read GetValue write SetValue;
    // New ca methods + properties 
    function Contains(const AString: String): Boolean;
    function GetHigh: Integer;
    function GetLow: Integer;
    function GetStrings: TStrings;
    function IsIndexValid(AIndex: Integer): Boolean;
    function IndexOfWidest: Integer;
    function WidthOfWidest: Integer;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    property High: Integer read GetHigh;
    property Low: Integer read GetLow;
  end;

  //---------------------------------------------------------------------------
  // IcaStringStack                                                            
  //---------------------------------------------------------------------------

  IcaStringStack = interface
  ['{B021D469-AE1F-428D-AFA8-8C832128B6E6}']
    // Interface methods 
    function HasItems: Boolean;
    function IsEmpty: Boolean;
    function Peek: String;
    function Pop: String;
    function Push(const AItem: String): Integer;
    function Size: Integer;
  end;

  //---------------------------------------------------------------------------
  // TcaStringList                                                             
  //---------------------------------------------------------------------------

  TcaStringList = class(TcaInterfacedPersistent, IcaStringList, IcaStringStack, IcaLoggable)
  private
    FStrings: TStrings;
    FAdapter: IStringsAdapter;
    // IcaStringList new methods 
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
  protected
    // Protected interface property methods 
    function GetAdapter: IStringsAdapter;
    function GetCommaText: string;
    function GetDuplicates: TDuplicates;
    function GetName(Index: Integer): string;
    function GetSorted: Boolean;
    function GetValue(const Name: string): string;
    procedure SetCommaText(const Value: string);
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetSorted(const Value: Boolean);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: string);
    // IcaStringList new property methods 
    function GetHigh: Integer;
    function GetLow: Integer;
    // Protected virtual methods 
    function Get(Index: Integer): string; virtual;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
  public
    constructor Create; overload;
    constructor Create(const AString: string); overload;
    constructor Create(AStrings: TStrings); overload;
    constructor Create(AStrings: array of String); overload;
    destructor Destroy; override;
    function Add(const S: string): Integer; overload; virtual;
    function Add(const S: string; const Args: array of const): Integer; overload; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    function Equals(Strings: TStrings): Boolean;
    function GetStrings: TStrings;
    function GetText: PChar; virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure AddStrings(AStrings: TStrings); overload; virtual;
    procedure AddStrings(AStrings: IcaStringList); overload; virtual;
    procedure AddStrings(AStrings: array of string); overload;
    procedure Append(const S: string);
    procedure Assign(Source: TPersistent); override;
    procedure AssignFromInterface(Source: IUnknown); override;
    procedure AssignFromObject(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure EndUpdate;
    procedure Exchange(Index1, Index2: Integer); virtual;
    procedure Insert(Index: Integer; const S: string); virtual;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PChar); virtual;
    procedure Sort;
    //  Properties 
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Sorted: Boolean read GetSorted write SetSorted;
    property Strings[Index: Integer]: string read Get write Put; default;
    property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
    property Text: string read GetTextStr write SetTextStr;
    property Values[const Name: string]: string read GetValue write SetValue;
    // IcaStringList new methods + properties 
    function Contains(const AString: String): Boolean;
    function IndexOfWidest: Integer;
    function IsIndexValid(AIndex: Integer): Boolean;
    function WidthOfWidest: Integer;
    property High: Integer read GetHigh;
    property Low: Integer read GetLow;
    // IcaStringStack 
    function HasItems: Boolean;
    function IsEmpty: Boolean;
    function Peek: String;
    function Pop: String;
    function Push(const AItem: String): Integer;
    function Size: Integer;
    // IcaLoggable 
    procedure SendToLog(const AMsg: String; AClearLog: Boolean = False);
  end;

  //---------------------------------------------------------------------------
  // IcaComponentState                                                         
  //---------------------------------------------------------------------------

  IcaComponentState = interface
  ['{2C6F6EBB-9D75-4D76-9452-CC9DE84231CD}']
    function GetComponent: TComponent;
    function GetIsAncestor: Boolean;
    function GetIsDesigning: Boolean;
    function GetIsDestroying: Boolean;
    function GetIsFixups: Boolean;
    function GetIsFreeNotification: Boolean;
    function GetIsInline: Boolean;
    function GetIsLoading: Boolean;
    function GetIsReading: Boolean;
    function GetIsRunTime: Boolean;
    function GetIsUpdating: Boolean;
    function GetIsWriting: Boolean;
    procedure SetComponent(const Value: TComponent);
    property Component: TComponent read GetComponent write SetComponent;
    property IsAncestor: Boolean read GetIsAncestor;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsFixups: Boolean read GetIsFixups;
    property IsFreeNotification: Boolean read GetIsFreeNotification;
    property IsInline: Boolean read GetIsInline;
    property IsLoading: Boolean read GetIsLoading;
    property IsReading: Boolean read GetIsReading;
    property IsRunTime: Boolean read GetIsRunTime;
    property IsUpdating: Boolean read GetIsUpdating;
    property IsWriting: Boolean read GetIsWriting;
  end;

  //---------------------------------------------------------------------------
  // TcaComponentState                                                         
  //---------------------------------------------------------------------------

  TcaComponentState = class(TInterfacedObject, IcaComponentState)
  private
    FComponent: TComponent;
    function GetComponent: TComponent;
    function GetIsAncestor: Boolean;
    function GetIsDesigning: Boolean;
    function GetIsDestroying: Boolean;
    function GetIsFixups: Boolean;
    function GetIsFreeNotification: Boolean;
    function GetIsInline: Boolean;
    function GetIsLoading: Boolean;
    function GetIsReading: Boolean;
    function GetIsRunTime: Boolean;
    function GetIsUpdating: Boolean;
    function GetIsWriting: Boolean;
    procedure SetComponent(const Value: TComponent);
  public
    constructor Create(AComponent: TComponent); overload;
    property Component: TComponent read GetComponent write SetComponent;
    property IsAncestor: Boolean read GetIsAncestor;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsFixups: Boolean read GetIsFixups;
    property IsFreeNotification: Boolean read GetIsFreeNotification;
    property IsInline: Boolean read GetIsInline;
    property IsLoading: Boolean read GetIsLoading;
    property IsReading: Boolean read GetIsReading;
    property IsRunTime: Boolean read GetIsRunTime;
    property IsUpdating: Boolean read GetIsUpdating;
    property IsWriting: Boolean read GetIsWriting;
  end;

  //---------------------------------------------------------------------------
  // IcaList                                                                   
  //---------------------------------------------------------------------------

  TcaList = class;

  IcaList = interface
  ['{B01C4736-8D07-48B0-9979-7285AE28A9A7}']
    // Property methods 
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetList: TList;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    // Public methods 
    function Expand: TList;
    procedure Clear;
    procedure CopyList(AList: TList);
    procedure CopyTo(SourceList: TcaList); overload;
    procedure CopyTo(SourceList: IcaList); overload;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    // Properties 
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property List: TList read GetList;
  end;

  //---------------------------------------------------------------------------
  // TcaList                                                                   
  //---------------------------------------------------------------------------

  TcaList = class(TInterfacedObject, IcaList)
  private
    FList: TList;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetList: TList;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
  protected
    // Protected methods 
    procedure CopyList(AList: TList);
  public
    constructor Create;
    destructor Destroy; override;
    function Expand: TList;
    procedure Clear;
    procedure CopyTo(SourceList: TcaList); overload;
    procedure CopyTo(SourceList: IcaList); overload;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property List: TList read GetList;    
  end;

  //---------------------------------------------------------------------------
  // IcaPointerList                                                            
  //---------------------------------------------------------------------------

  IcaPointerList = interface
  ['{3136D62F-00BF-4E07-9DB6-A41CB71CAB0E}']
    // Property methods 
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; const AItem: Pointer);
    // Public methods 
    function Add(AItem: Pointer; Unique: Boolean = False): Integer;
    function Extract(AItem: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(AItem: Pointer): Integer;
    function Last: Pointer;
    function Remove(AItem: Pointer): Integer;
    procedure Insert(Index: Integer; AItem: Pointer);
    // Properties 
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaPointerStack                                                           
  //---------------------------------------------------------------------------

  IcaPointerStack = interface
  ['{E1A286D0-F5A6-4758-993E-BF6DCE07D300}']
    // Interface methods 
    function HasItems: Boolean;
    function IsEmpty: Boolean;
    function Peek: Pointer;
    function Pop: Pointer;
    function Push(AItem: Pointer): Integer;
    function Size: Integer;
  end;

  //---------------------------------------------------------------------------
  // TcaPointerList                                                            
  //---------------------------------------------------------------------------

  TcaPointerList = class(TInterfacedObject, IcaList, IcaPointerList, IcaPointerStack)
  private
    // Private fields 
    FListBase: IcaList;
    // Property methods 
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; const Value: Pointer);
  public
    constructor Create;
    // Public methods 
    function Add(AItem: Pointer; Unique: Boolean = False): Integer;    
    function Extract(AItem: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(AItem: Pointer): Integer;
    function Last: Pointer;
    function Remove(AItem: Pointer): Integer;
    procedure Insert(Index: Integer; AItem: Pointer);
    // IcaPointerList interface methods 
    function HasItems: Boolean;
    function IsEmpty: Boolean;
    function Peek: Pointer;
    function Pop: Pointer;
    function Push(AItem: Pointer): Integer;
    function Size: Integer;
    // Properties 
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
    property ListBase: IcaList read FListBase implements IcaList;
  end;

  //---------------------------------------------------------------------------
  // IcaObjectList                                                             
  //---------------------------------------------------------------------------

  IcaObjectList = interface
  ['{63181262-097A-4006-A50C-6556BDF9E8E7}']
    // Property methods 
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const AItem: TObject);
    // Public methods 
    function Add(AItem: TObject; Unique: Boolean = False): Integer;
    function Extract(AItem: TObject): TObject;
    function First: TObject;
    function IndexOf(AItem: TObject): Integer;
    function Last: TObject;
    function Remove(AItem: TObject): Integer;
    procedure Insert(Index: Integer; AItem: TObject);
    // Properties 
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaObjectStack                                                            
  //---------------------------------------------------------------------------

  IcaObjectStack = interface
  ['{E6DE2996-6601-41ED-8343-0EF115BDEEC7}']
    // Interface methods  
    function HasItems: Boolean;
    function IsEmpty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    function Push(AItem: TObject): Integer;
    function Size: Integer;
  end;

  //---------------------------------------------------------------------------
  // TcaObjectList                                                             
  //---------------------------------------------------------------------------

  TcaObjectList = class(TInterfacedObject, IcaList, IcaObjectList, IcaObjectStack)
  private
    FListBase: IcaList;
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const Value: TObject);
  public
    constructor Create;
    // Public methods 
    function Add(AItem: TObject; Unique: Boolean = False): Integer;
    function Extract(AItem: TObject): TObject;
    function First: TObject;
    function IndexOf(AItem: TObject): Integer;
    function Last: TObject;
    function Remove(AItem: TObject): Integer;
    procedure Insert(Index: Integer; AItem: TObject);
    // IcaObjectList interface methods 
    function HasItems: Boolean;
    function IsEmpty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    function Push(AItem: TObject): Integer;
    function Size: Integer;
    // Properties 
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property ListBase: IcaList read FListBase implements IcaList;
  end;

  //---------------------------------------------------------------------------
  // IcaSparseMatrix                                                           
  //---------------------------------------------------------------------------

  IcaSparseMatrix = interface
  ['{49F7CC68-7362-47D3-9D43-CB8C16015E7C}']
    // Property methods 
    function GetColCount: Integer;
    function GetItem(ACol, ARow: Integer): Pointer;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetItem(ACol, ARow: Integer; const Value: Pointer);
    procedure SetRowCount(const Value: Integer);
    // Properties 
    property ColCount: Integer read GetColCount write SetColCount;
    property Items[ACol, ARow: Integer]: Pointer read GetItem write SetItem; default;
    property RowCount: Integer read GetRowCount write SetRowCount;
    // Public methods 
    procedure ReleaseElement(ACol, ARow: Integer);
    procedure ReleaseElements;
  end;

  //---------------------------------------------------------------------------
  // TcaSparseMatrix                                                           
  //---------------------------------------------------------------------------

  TcaSparseMatrix = class(TInterfacedObject, IcaSparseMatrix)
  private
    // Private fields 
    FCols: TList;
    FRowCount: Integer;
    // Property methods 
    function GetColCount: Integer;
    function GetItem(ACol, ARow: Integer): Pointer;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetItem(ACol, ARow: Integer; const Value: Pointer);
    procedure SetRowCount(const Value: Integer);
    // Private methods 
    procedure FreeRows;
  public
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    procedure ReleaseElement(ACol, ARow: Integer);
    procedure ReleaseElements;
    // Properties 
    property ColCount: Integer read GetColCount write SetColCount;
    property Items[ACol, ARow: Integer]: Pointer read GetItem write SetItem; default;
    property RowCount: Integer read GetRowCount write SetRowCount;
  end;

  //---------------------------------------------------------------------------
  // IcaStringCase                                                             
  //---------------------------------------------------------------------------

  IcaStringCase = interface
  ['{B200E7CD-996D-43FE-86C5-964F3EB32458}']
    function CaseOf(const AString: String): Integer;
    procedure AddOption(const AOption: String);
  end;

  //---------------------------------------------------------------------------
  // TcaStringCase                                                             
  //---------------------------------------------------------------------------

  TcaStringCase = class(TcaInterfacedPersistent, IcaStringCase)
  private
    FOptions: TcaStringList;
  public
    constructor Create(const AOptions: array of String);
    destructor Destroy; override;
    // Public methods 
    function CaseOf(const AString: String): Integer;
    procedure AddOption(const AOption: String);
  end;

  //---------------------------------------------------------------------------
  // IcaArgs                                                                   
  //---------------------------------------------------------------------------

  IcaArgs = interface
  ['{D0C1FFE0-2E69-4DF0-BDCC-15DFD095D9AA}']
    // Property methods 
    function Get(Index: Integer): string;
    function GetCount: Integer;
    function GetExecutable: String;
    function GetValue(const Name: string): string;
    procedure Put(Index: Integer; const S: string);
    procedure SetValue(const Name, Value: string);
    // Public methods 
    function HasArg(const AArg: String): Boolean;
    // Properties 
    property Count: Integer read GetCount;
    property Executable: String read GetExecutable;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Values[const Name: string]: string read GetValue write SetValue;
  end;

  //---------------------------------------------------------------------------
  // TcaArgs                                                                   
  //---------------------------------------------------------------------------

  TcaArgs = class(TcaStringList, IcaArgs)
  private
    // Property methods 
    function GetExecutable: String;
    // Private methods 
    procedure UpdateArgs;
  protected
    // IcaArgs methods 
    function HasArg(const AArg: String): Boolean;
  public
    // Public overridden methods 
    procedure AfterConstruction; override;
    // Properties 
    property Executable: String read GetExecutable;
  end;

  //---------------------------------------------------------------------------
  // IcaAutoFree                                                               
  //---------------------------------------------------------------------------

  IcaAutoFree = interface
  ['{FEC11200-4C4E-49D7-9F9F-C7178A687DA4}']
    // Property methods 
    function GetInstance: Pointer;
    // Properties 
    property Instance: Pointer read GetInstance;
  end;

  //---------------------------------------------------------------------------
  // TcaAutoFree                                                               
  //---------------------------------------------------------------------------

  TcaAutoFree = class(TcaInterfacedPersistent, IcaAutoFree)
  private
    // Private fields 
    FInstance: TObject;
    // Property methods 
    function GetInstance: Pointer;
  public
    constructor Create(AInstance: TObject);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // TcaProperties                                                             
  //---------------------------------------------------------------------------

  TcaProperties = class(TPersistent)
  protected
    // Protected virtual methods 
    procedure Finalize; virtual;
    procedure Initialize; virtual;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // TcaNotifyProperties                                                       
  //---------------------------------------------------------------------------

  TcaNotifyProperties = class(TcaProperties)
  private
    // Private fields 
    FNotifyMethod: TcaProcedure;
  protected
    // Protected static methods 
    procedure Changed;
    procedure SetBooleanProperty(var AProperty: Boolean; AValue: Boolean);
    procedure SetIntegerProperty(var AProperty: Integer; AValue: Integer);
    // Protected properties 
    property NotifyMethod: TcaProcedure read FNotifyMethod;
  public
    // Create/Destroy 
    constructor Create(ANotifyMethod: TcaProcedure); virtual;
    destructor Destroy; override;
  end;

function Auto(AInstance: TObject): IcaAutoFree;

var
  caInterfaceBalance: Int64 = 0;

{$IFDEF D5}
function Supports(const Instance: IUnknown; const Intf: TGUID): Boolean; overload;
{$ENDIF}

implementation

uses

  // Standard Delphi units; 
  Consts,
  SysConst,

  // ca units 
  caConsts,
  caMath,
  caUtils;

{$IFDEF D5}
function Supports(const Instance: IUnknown; const Intf: TGUID): Boolean;
var
  Unk: IUnknown;
begin
  Result := Sysutils.Supports(Instance, Intf, Unk);
end;
{$ENDIF}

  //----------------------------------------------------------------------------
  // TcaInterfacedPersistent                                                    
  //----------------------------------------------------------------------------

function InterlockedIncrement(var Addend: Integer): Integer; stdcall;
  external cKernel name 'InterlockedIncrement';

function InterlockedDecrement(var Addend: Integer): Integer; stdcall;
  external cKernel name 'InterlockedDecrement';

constructor TcaInterfacedPersistent.CreateNonRefCounted;
begin
  inherited Create;
  FNoRefCount := True;
end;

procedure TcaInterfacedPersistent.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
  {$IFDEF LOG_REF_COUNT}
  FLogRefCount := True;
  {$ENDIF}
  {$IFDEF LOG_CREATE_DESTROY}
  FLogCreateAndDestroy := True;
  Inc(caInterfaceBalance);
  if FLogCreateAndDestroy then
    Log.Send(ClassName + ' - Create', caInterfaceBalance);
  {$ENDIF}
end;

procedure TcaInterfacedPersistent.BeforeDestruction;
begin
  if RefCount <> 0 then raise Exception.Create(SInvalidPointer + ' in ' + ClassName);
  {$IFDEF LOG_CREATE_DESTROY}
  Dec(caInterfaceBalance);
  if FLogCreateAndDestroy then
    Log.Send(ClassName + ' - Destroy', caInterfaceBalance);
  {$ENDIF}
end;

class function TcaInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TcaInterfacedPersistent(Result).FRefCount := 1;
end;

function TcaInterfacedPersistent._AddRef: Integer;
begin
  if FNoRefCount then
    Result := -1
  else
    Result := InterlockedIncrement(FRefCount);
  {$IFDEF LOG_REF_COUNT}
  if FLogRefCount then
    Log.Send(ClassName + ' - _AddRef: ', FRefCount);
  {$ENDIF}
end;

function TcaInterfacedPersistent._Release: Integer;
begin
  if FNoRefCount then
    Result := -1
  else
    begin
      Result := InterlockedDecrement(FRefCount);
      {$IFDEF LOG_REF_COUNT}
      if FLogRefCount then
        Log.Send(ClassName + ' - _Release: ', FRefCount);
      {$ENDIF}
      if Result = 0 then Destroy;
    end;
end;

function TcaInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

  // Owned list object support 

procedure TcaInterfacedPersistent.ClearListObjects(AList: TList);
var
  Index: Integer;
begin
  for Index := 0 to Pred(AList.Count) do
    TObject(AList[Index]).Free;
  AList.Clear;
end;

  // IcaAssignable 

procedure TcaInterfacedPersistent.AssignFromInterface(Source: IUnknown);
var
  SourceObject: TcaInterfacedPersistent;
  SourceCloneable: IcaCloneable;
begin
  if Supports(Source, IcaCloneable, SourceCloneable) then
    begin
      SourceObject := SourceCloneable.CloneAsObject;
      try
        AssignFromObject(SourceObject);
      finally
        SourceObject.Free;
      end;
    end;
end;

procedure TcaInterfacedPersistent.AssignFromObject(Source: TPersistent);
begin
  if not Utils.ShallowCopy(Source, Self, True) then
    inherited Assign(Source);
end;

  // IcaCloneable 

function TcaInterfacedPersistent.CloneAsInterface: IUnknown;
var
  Assignable: IcaAssignable;
  CloneClass: TcaInterfacedPersistentClass;
begin
  CloneClass := TcaInterfacedPersistentClass(ClassType);
  Result := CloneClass.Create;
  Assignable := Result as IcaAssignable;
  Assignable.AssignFromObject(Self);
end;

function TcaInterfacedPersistent.CloneAsObject: TcaInterfacedPersistent;
var
  CloneClass: TcaInterfacedPersistentClass;
begin
  CloneClass := TcaInterfacedPersistentClass(ClassType);
  Result := CloneClass.Create;
  Result.AssignFromObject(Self);
end;

  //----------------------------------------------------------------------------
  // TcaString                                                                  
  //----------------------------------------------------------------------------

constructor TcaString.Create(AString: String);
begin
  inherited Create;
  FS := AString;
end;

procedure TcaString.Add(const S: String);
begin
  FS := FS + S;
end;

constructor TcaString.Create(AInteger: Integer);
begin
  inherited Create;
  FS := IntToStr(AInteger);
end;

procedure TcaString.DeleteFromStart(N: Integer);
begin
  FS := Right(Length - N);
end;

procedure TcaString.DeleteFromEnd(N: Integer);
begin
  FS := Left(Length - N);
end;

function TcaString.GetS: String;
begin
  Result := FS;
end;

function TcaString.EndsWith(const AV: string): Boolean;
var
  RightSub: string;
begin
  Result := False;
  if System.Length(S) <= Length then
    begin
      RightSub := Right(System.Length(S));
      Result := RightSub = S; 
    end;
end;

function TcaString.IsNumChar(N: Integer): Boolean;
var
  Validator: IcaValidNum;
  NumStr: String;
begin
  Validator := TcaValidNum.Create(vtFloat);
  NumStr := Mid(N, 1);
  Validator.NumString := NumStr;
  Result := Validator.IsValid;
end;

function TcaString.Left(N: Integer): String;
begin
  Result := Copy(FS, 1, N);
end;

function TcaString.Length: Integer;
begin
  Result := System.Length(FS);
end;

function TcaString.Mid(N, ForN: Integer): String;
begin
  Result := Copy(FS, N, ForN);
end;

function TcaString.PadRight(ALength: Integer): String;
begin
  Result := Utils.PadRight(FS, ' ', ALength);
end;

function TcaString.PadLeft(ALength: Integer): String;
begin
  Result := Utils.PadLeft(FS, ' ', ALength);
end;

function TcaString.PreZero(ALength: Integer): String;
begin
  Result := Utils.PadLeft(FS, '0', ALength);
end;

function TcaString.PosFromEnd(const AFindStr: String): Integer;
var
  FindStr: IcaString;
  RevFindStr: String;
  RevStr: String;
  PosVal: Integer;
begin
  FindStr := TcaString.Create(AFindStr);
  RevFindStr := FindStr.Reverse;
  RevStr := Reverse;
  PosVal := Utils.PosFromStart(RevFindStr, RevStr);
  Result := PosVal;
  if PosVal > 0 then
    Result := Length - PosVal - FindStr.Length + 2;
end;

function TcaString.PosFromStart(const AFindStr: String): Integer;
begin
  Result := Utils.PosFromStart(AFindStr, S);
end;

function TcaString.Ref: Integer;
begin
  Result := RefCount;
end;

function TcaString.Reverse: String;
var
  Index: Integer;
  Len: Integer;
begin
  Len := Length;
  SetLength(Result, Len);
  for Index := Len downto 1 do
    Result[Len - Index + 1] := FS[Index];
end;

function TcaString.Right(N: Integer): String;
begin
  Result := Copy(FS, Length - N + 1, N);
end;

procedure TcaString.SetS(const Value: String);
begin
  FS := Value;
end;

procedure TcaString.LowerCase;
begin
  FS := SysUtils.LowerCase(FS);
end;

procedure TcaString.SplitCamelCaps;
begin
  Utils.SplitCamelCaps(FS);
end;

procedure TcaString.UpperCase;
begin
  FS := SysUtils.UpperCase(FS);
end;

  //----------------------------------------------------------------------------
  // TcaByteString                                                              
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaByteString.Create(ACount: Integer);
begin
  inherited Create;
  SetCount(ACount);
end;

  // Interface methods 

procedure TcaByteString.Clear;
begin
  S := '';
end;

  // Private methods 

function TcaByteString.ByteToStr(AByte: Byte): string;
begin
  Result := Format('%.2x', [AByte]);
end;

procedure TcaByteString.CheckIndex(Index: Integer);
begin
  if Index >= GetCount then SetCount(Succ(Index));
end;

procedure TcaByteString.GetAsStrings(AStrings: TStrings);
var
  Index: Integer;
begin
  AStrings.Clear;
  Index := 1;
  while Index <= Length do
    begin
      AStrings.Add(Copy(S, Index, 2));
      Inc(Index, 2);
    end;
end;

procedure TcaByteString.SetAsStrings(AStrings: TStrings);
var
  Index: Integer;
begin
  Clear;
  for Index := 0 to Pred(AStrings.Count) do
    Add(AStrings[Index]);
end;

  // Property methods 

function TcaByteString.GetAsString: string;
begin
  Result := S;
end;

function TcaByteString.GetByte(Index: Integer): Byte;
var
  Offset: Integer;
begin
  CheckIndex(Index);
  Offset := Succ(Index * 2);
  Result := StrToInt('$' + Copy(S, Offset, 2));
end;

function TcaByteString.GetCount: Integer;
begin
  Result := Length div 2;
end;

procedure TcaByteString.SetAsString(const Value: string);
begin
  S := Value;
end;

procedure TcaByteString.SetByte(Index: Integer; const Value: Byte);
var
  Strings: TStrings;
begin
  CheckIndex(Index);
  Strings := Auto(TStringList.Create).Instance;
  GetAsStrings(Strings);
  Strings[Index] := ByteToStr(Value);
  SetAsStrings(Strings);
end;

procedure TcaByteString.SetCount(const Value: Integer);
begin
  while GetCount < Value do
    S := S + '00';
end;

  //----------------------------------------------------------------------------
  // TcaStringList                                                              
  //----------------------------------------------------------------------------

type
  TcaStringList_Protected = class(TStringList);

constructor TcaStringList.Create;
begin
  inherited;
  FStrings := TStringList.Create;
end;

constructor TcaStringList.Create(const AString: string);
begin
  inherited Create;
  FStrings := TStringList.Create;
  FStrings.Text := AString;
end;

constructor TcaStringList.Create(AStrings: TStrings);
begin
  inherited Create;
  FStrings := TStringList.Create;
  FStrings.Assign(AStrings);
end;

constructor TcaStringList.Create(AStrings: array of String);
var
  Index: Integer;
begin
  inherited Create;
  FStrings := TStringList.Create;
  for Index := System.Low(AStrings) to System.High(AStrings) do
    FStrings.Add(AStrings[Index]);
end;

destructor TcaStringList.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TcaStringList.Add(const S: string): Integer;
begin
  Result := FStrings.Add(S);
end;

function TcaStringList.Add(const S: string; const Args: array of const): Integer;
begin
  Result := Add(Format(S, Args));
end;

function TcaStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := FStrings.AddObject(S, AObject);
end;

procedure TcaStringList.AddStrings(AStrings: TStrings);
begin
  FStrings.AddStrings(AStrings);
end;

procedure TcaStringList.AddStrings(AStrings: IcaStringList);
begin
  FStrings.AddStrings(AStrings.GetStrings);
end;

procedure TcaStringList.AddStrings(AStrings: array of string);
var
  Index: Integer;
begin
  for Index := System.Low(AStrings) to System.High(AStrings) do
    Add(AStrings[Index]);
end;

procedure TcaStringList.Append(const S: string);
begin
  FStrings.Append(S);
end;

procedure TcaStringList.Assign(Source: TPersistent);
begin
  if (Source is TcaStringList) or (Source is TStrings) then
    begin
      BeginUpdate;
      try
        Clear;
        if Source is TcaStringList then
          AddStrings(TcaStringList(Source).GetStrings)
        else
          AddStrings(TStrings(Source));
      finally
        EndUpdate;
      end;
    end
  else
    inherited Assign(Source);
end;

procedure TcaStringList.AssignFromInterface(Source: IUnknown);
var
  AStringList: IcaStringList;
begin
  if Supports(Source, IcaStringList, AStringList) then
    AssignFromObject(AStringList.GetStrings);
end;

procedure TcaStringList.AssignFromObject(Source: TPersistent);
begin
  Assign(Source);
end;

procedure TcaStringList.BeginUpdate;
begin
  FStrings.BeginUpdate;
end;

procedure TcaStringList.Clear;
begin
  FStrings.Clear;
end;

function TcaStringList.Contains(const AString: String): Boolean;
begin
  Result := FStrings.IndexOf(AString) >= 0;
end;

procedure TcaStringList.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do Add(Reader.ReadString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TcaStringList.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(Get(I));
  Writer.WriteListEnd;
end;

procedure TcaStringList.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TStrings then
        Result := not Equals(TStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TcaStringList.Delete(Index: Integer);
begin
  FStrings.Delete(Index);
end;

procedure TcaStringList.EndUpdate;
begin
  FStrings.EndUpdate;
end;

function TcaStringList.Equals(Strings: TStrings): Boolean;
begin
  Result := FStrings.Equals(Strings);
end;

procedure TcaStringList.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

procedure TcaStringList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
    MOV EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TcaStringList.Exchange(Index1, Index2: Integer);
begin
  FStrings.Exchange(Index1, Index2);
end;

function TcaStringList.Get(Index: Integer): string;
begin
  Result := FStrings.Strings[Index];
end;

function TcaStringList.GetAdapter: IStringsAdapter;
begin
  Result := FStrings.StringsAdapter;
end;

function TcaStringList.GetCapacity: Integer;
begin
  Result := FStrings.Count;
end;

function TcaStringList.GetCommaText: string;
begin
  Result := FStrings.CommaText;
end;

function TcaStringList.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

function TcaStringList.GetDuplicates: TDuplicates;
begin
  Result := TStringList(FStrings).Duplicates;
end;

function TcaStringList.GetName(Index: Integer): string;
begin
  Result := FStrings.Names[Index];
end;

function TcaStringList.GetSorted: Boolean;
begin
  Result := TStringList(FStrings).Sorted;
end;

function TcaStringList.GetObject(Index: Integer): TObject;
begin
  Result := FStrings.Objects[Index];
end;

function TcaStringList.GetText: PChar;
begin
  Result := FStrings.GetText;
end;

function TcaStringList.GetTextStr: string;
begin
  Result := FStrings.Text;
end;

function TcaStringList.GetValue(const Name: string): string;
begin
  Result := FStrings.Values[Name];
end;

function TcaStringList.IndexOf(const S: string): Integer;
begin
  Result := FStrings.IndexOf(S);
end;

function TcaStringList.IndexOfName(const Name: string): Integer;
begin
  Result := FStrings.IndexOfName(Name);
end;

function TcaStringList.IndexOfObject(AObject: TObject): Integer;
begin
  Result := FStrings.IndexOfObject(AObject);
end;

procedure TcaStringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  FStrings.InsertObject(Index, S, AObject);
end;

procedure TcaStringList.LoadFromFile(const FileName: string);
begin
  FStrings.LoadFromFile(FileName);
end;

procedure TcaStringList.LoadFromStream(Stream: TStream);
begin
  FStrings.LoadFromStream(Stream);
end;

procedure TcaStringList.Move(CurIndex, NewIndex: Integer);
begin
  FStrings.Move(CurIndex, NewIndex);
end;

procedure TcaStringList.Put(Index: Integer; const S: string);
begin
  FStrings.Strings[Index] := S;
end;

procedure TcaStringList.PutObject(Index: Integer; AObject: TObject);
begin
  FStrings.Objects[Index] := AObject;
end;

procedure TcaStringList.SaveToFile(const FileName: string);
begin
  FStrings.SaveToFile(FileName);
end;

procedure TcaStringList.SaveToStream(Stream: TStream);
begin
  FStrings.SaveToStream(Stream);
end;

procedure TcaStringList.SetCapacity(NewCapacity: Integer);
begin
  FStrings.Capacity := NewCapacity;
end;

procedure TcaStringList.SetCommaText(const Value: string);
begin
  FStrings.CommaText := Value;
end;

procedure TcaStringList.SetDuplicates(const Value: TDuplicates);
begin
  TStringList(FStrings).Duplicates := Value;
end;

procedure TcaStringList.SetSorted(const Value: Boolean);
begin
  TStringList(FStrings).Sorted := Value;
end;

procedure TcaStringList.SetStringsAdapter(const Value: IStringsAdapter);
begin
  FStrings.StringsAdapter := Value;
end;

procedure TcaStringList.SetText(Text: PChar);
begin
  FStrings.Text := String(Text);
end;

procedure TcaStringList.SetTextStr(const Value: string);
begin
  FStrings.Text := Value;
end;

procedure TcaStringList.SetUpdateState(Updating: Boolean);
begin
  TcaStringList_Protected(FStrings).SetUpdateState(Updating);
end;

procedure TcaStringList.SetValue(const Name, Value: string);
begin
  FStrings.Values[Name] := Value;
end;

  //---------------------------------------------------------------------------
  // IcaStringList new methods                                                 
  //---------------------------------------------------------------------------

function TcaStringList.GetHigh: Integer;
begin
  Result := Count - 1;
end;

function TcaStringList.GetLow: Integer;
begin
  Result := 0;
end;

function TcaStringList.GetStrings: TStrings;
begin
  Result := FStrings;
end;

function TcaStringList.IndexOfWidest: Integer;
var
  Index: Integer;
  Len: Integer;
begin
  Result := -1;
  for Index := Low to High do
    begin
      Len := Length(Strings[Index]);
      if Len > Result then Result := Index;
    end;
end;

procedure TcaStringList.Insert(Index: Integer; const S: string);
begin
  FStrings.Insert(Index, S);
end;

function TcaStringList.WidthOfWidest: Integer;
var
  Widest: Integer;
begin
  Result := 0;
  Widest := IndexOfWidest;
  if Widest >= 0 then
    Result := Length(Strings[Widest]);;
end;

// IcaStringStack 

function TcaStringList.HasItems: Boolean;
begin
  Result := not IsEmpty;
end;

function TcaStringList.IsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;

function TcaStringList.IsIndexValid(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < GetCount);
end;

function TcaStringList.Peek: String;
begin
  if GetCount = 0 then
    Result := ''
  else
    Result := Strings[GetHigh];
end;

function TcaStringList.Pop: String;
begin
  if GetCount = 0 then
    Result := ''
  else
    begin
      Result := Peek;
      Delete(GetHigh);
    end;
end;

function TcaStringList.Push(const AItem: String): Integer;
begin
  Result := Add(AItem);
end;

function TcaStringList.Size: Integer;
begin
  Result := GetCount;
end;

procedure TcaStringList.Sort;
begin
  TStringList(FStrings).Sort;
end;

  // IcaLoggable 

procedure TcaStringList.SendToLog(const AMsg: String; AClearLog: Boolean = False);
var
  Index: Integer;
begin
  if AClearLog then Log.Clear;
  if AMsg <> '' then Log.Send(AMsg);
  for Index := 0 to GetCount - 1 do
    Log.Send(FStrings[Index]);  
end;

  //---------------------------------------------------------------------------
  // TcaComponentState                                                         
  //---------------------------------------------------------------------------

constructor TcaComponentState.Create(AComponent: TComponent); 
begin
  inherited Create;
  FComponent := AComponent;
end;
 
function TcaComponentState.GetComponent: TComponent;
begin
  Result := FComponent;
end;

function TcaComponentState.GetIsAncestor: Boolean;
begin
  Result := csAncestor in FComponent.ComponentState;
end;

function TcaComponentState.GetIsDesigning: Boolean;
begin
  Result := csDesigning in FComponent.ComponentState;
end;

function TcaComponentState.GetIsDestroying: Boolean;
begin
  Result := csDestroying in FComponent.ComponentState;
end;

function TcaComponentState.GetIsFixups: Boolean;
begin
  Result := csFixups in FComponent.ComponentState;
end;

function TcaComponentState.GetIsFreeNotification: Boolean;
begin
  Result := csFreeNotification in FComponent.ComponentState;
end;

function TcaComponentState.GetIsInline: Boolean;
begin
  Result := csInline in FComponent.ComponentState;
end;

function TcaComponentState.GetIsLoading: Boolean;
begin
  Result := csLoading in FComponent.ComponentState;
end;

function TcaComponentState.GetIsReading: Boolean;
begin
  Result := csReading in FComponent.ComponentState;
end;

function TcaComponentState.GetIsRunTime: Boolean;
begin
  Result := not GetIsDesigning;
end;

function TcaComponentState.GetIsUpdating: Boolean;
begin
  Result := csUpdating in FComponent.ComponentState;
end;

function TcaComponentState.GetIsWriting: Boolean;
begin
  Result := csWriting in FComponent.ComponentState;
end;

procedure TcaComponentState.SetComponent(const Value: TComponent);
begin
  FComponent := Value;
end;

  //---------------------------------------------------------------------------
  // TcaList                                                                   
  //---------------------------------------------------------------------------

constructor TcaList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TcaList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TcaList.Clear;
begin
  FList.Clear;
end;

procedure TcaList.CopyTo(SourceList: TcaList);
begin
  SourceList.CopyList(FList);
end;

procedure TcaList.CopyTo(SourceList: IcaList);
begin
  SourceList.CopyList(FList);
end;

procedure TcaList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TcaList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TcaList.Expand: TList;
begin
  Result := FList.Expand;
end;

function TcaList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TcaList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaList.GetList: TList;
begin
  Result := FList;
end;

procedure TcaList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TcaList.Pack;
begin
  FList.Pack;
end;

procedure TcaList.SetCapacity(const Value: Integer);
begin
  FList.Capacity := Value;
end;

procedure TcaList.SetCount(const Value: Integer);
begin
  FList.Count := Value;
end;

procedure TcaList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

  // Protected methods 

procedure TcaList.CopyList(AList: TList);
var
  Index: Integer;
begin
  Clear;
  for Index := 0 to Pred(GetCount) do
    FList.Add(AList[Index]);
end;

  //---------------------------------------------------------------------------
  // TcaPointerList                                                            
  //---------------------------------------------------------------------------

constructor TcaPointerList.Create;
begin
  inherited;
  FListBase := TcaList.Create;
end;

  // Public methods 

function TcaPointerList.Add(AItem: Pointer; Unique: Boolean = False): Integer;
var
  ShouldAdd: Boolean;
begin
  Result := -1;
  ShouldAdd := True;
  if Unique then ShouldAdd := IndexOf(AItem) = -1;
  if ShouldAdd then
    Result := FListBase.List.Add(AItem);
end;

function TcaPointerList.Extract(AItem: Pointer): Pointer;
begin
  Result := FListBase.List.Extract(AItem);
end;

function TcaPointerList.First: Pointer;
begin
  Result := FListBase.List.First;
end;

function TcaPointerList.IndexOf(AItem: Pointer): Integer;
begin
  Result := FListBase.List.IndexOf(AItem);
end;

function TcaPointerList.Last: Pointer;
begin
  Result := FListBase.List.Last;
end;

function TcaPointerList.Remove(AItem: Pointer): Integer;
begin
  Result := FListBase.List.Remove(AItem);
end;

procedure TcaPointerList.Insert(Index: Integer; AItem: Pointer);
begin
  FListBase.List.Insert(Index, AItem);
end;

  // IcaPointerList interface methods 

function TcaPointerList.HasItems: Boolean;
begin
  Result := FListBase.Count <> 0;
end;

function TcaPointerList.IsEmpty: Boolean;
begin
  Result := not HasItems;
end;

function TcaPointerList.Peek: Pointer;
begin
  if FListBase.Count = 0 then
    Result := nil
  else
    Result := Last;
end;

function TcaPointerList.Pop: Pointer;
begin
  Result := Peek;
  if Result <> nil then
    FListBase.Delete(FListBase.Count - 1);
end;

function TcaPointerList.Push(AItem: Pointer): Integer;
begin
  Result := Add(AItem);
end;

function TcaPointerList.Size: Integer;
begin
  Result := FListBase.Count;
end;

  // Property methods 

function TcaPointerList.GetItem(Index: Integer): Pointer;
begin
  Result := FListBase.List.Items[Index];
end;

procedure TcaPointerList.SetItem(Index: Integer; const Value: Pointer);
begin
  FListBase.List.Items[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaObjectList                                                             
  //---------------------------------------------------------------------------

constructor TcaObjectList.Create;
begin
  inherited;
  FListBase := TcaList.Create;
end;

  // Public methods 

function TcaObjectList.Add(AItem: TObject; Unique: Boolean = False): Integer;
var
  ShouldAdd: Boolean;
begin
  Result := -1;
  ShouldAdd := True;
  if Unique then ShouldAdd := IndexOf(AItem) = -1;
  if ShouldAdd then
    Result := FListBase.List.Add(Pointer(AItem));
end;

function TcaObjectList.Extract(AItem: TObject): TObject;
begin
  Result := TObject(FListBase.List.Extract(Pointer(AItem)));
end;

function TcaObjectList.First: TObject;
begin
  Result := TObject(FListBase.List.First);
end;

function TcaObjectList.IndexOf(AItem: TObject): Integer;
begin
  Result := FListBase.List.IndexOf(Pointer(AItem));
end;

function TcaObjectList.Last: TObject;
begin
  Result := TObject(FListBase.List.Last);
end;

function TcaObjectList.Remove(AItem: TObject): Integer;
begin
  Result := FListBase.List.Remove(Pointer(AItem));
end;

procedure TcaObjectList.Insert(Index: Integer; AItem: TObject);
begin
  FListBase.List.Insert(Index, Pointer(AItem));
end;

  // IcaObjectList interface methods 

function TcaObjectList.HasItems: Boolean;
begin
  Result := FListBase.Count <> 0;
end;

function TcaObjectList.IsEmpty: Boolean;
begin
  Result := not HasItems;
end;

function TcaObjectList.Peek: TObject;
begin
  if FListBase.Count = 0 then
    Result := nil
  else
    Result := Last;
end;

function TcaObjectList.Pop: TObject;
begin
  Result := Peek;
  if Result <> nil then
    FListBase.Delete(FListBase.Count - 1);
end;

function TcaObjectList.Push(AItem: TObject): Integer;
begin
  Result := Add(AItem);
end;

function TcaObjectList.Size: Integer;
begin
  Result := FListBase.Count;
end;

  // Property methods 

function TcaObjectList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(FListBase.List.Items[Index]);
end;

procedure TcaObjectList.SetItem(Index: Integer; const Value: TObject);
begin
  FListBase.List.Items[Index] := Pointer(Value);
end;

  //---------------------------------------------------------------------------
  // TcaSparseMatrix                                                           
  //---------------------------------------------------------------------------

constructor TcaSparseMatrix.Create;
begin
  inherited;
  FCols := TList.Create;
end;

destructor TcaSparseMatrix.Destroy;
begin
  FreeRows;
  FCols.Free;
  inherited;
end;

procedure TcaSparseMatrix.ReleaseElement(ACol, ARow: Integer);
var
  Item: TObject;
begin
  Item := TObject(GetItem(ACol, ARow));
  Item.Free;
  SetItem(ACol, ARow, nil);
end;

procedure TcaSparseMatrix.ReleaseElements;
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to ColCount - 1 do
    for ARow := 0 to RowCount - 1 do
      ReleaseElement(ACol, ARow);
end;

  // Private methods 

procedure TcaSparseMatrix.FreeRows;
var
  Index: Integer;
begin
  for Index := 0 to Pred(FCols.Count) do
    TObject(FCols[Index]).Free;
end;

  // Property methods 

function TcaSparseMatrix.GetColCount: Integer;
begin
  Result := FCols.Count;
end;

function TcaSparseMatrix.GetItem(ACol, ARow: Integer): Pointer;
var
  Rows: TList;
begin
  Rows := TList(FCols[ACol]);
  Result := Rows[ARow];
end;

function TcaSparseMatrix.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

procedure TcaSparseMatrix.SetColCount(const Value: Integer);
var
  RowIndex: Integer;
  Rows: TList;
begin
  if Value <> FCols.Count then
    begin
      if Value > FCols.Count then
        begin
          while FCols.Count < Value do
            begin
              Rows := TList.Create;
              for RowIndex := 0 to FRowCount - 1 do
                Rows.Add(nil);
              FCols.Add(Rows);
            end;
        end
      else
        while FCols.Count > Value do
          FCols.Delete(FCols.Count - 1);
    end;
end;

procedure TcaSparseMatrix.SetItem(ACol, ARow: Integer; const Value: Pointer);
var
  Rows: TList;
begin
  Rows := TList(FCols[ACol]);
  Rows[ARow] := Value;
end;

procedure TcaSparseMatrix.SetRowCount(const Value: Integer);
var
  ACol: Integer;
  Rows: TList;
begin
  if Value <> FRowCount then
    begin
      FRowCount := Value;
      for ACol := 0 to FCols.Count - 1 do
        begin
          Rows := TList(FCols[ACol]);
          if FRowCount > Rows.Count then
            while Rows.Count < FRowCount do Rows.Add(nil)
          else
            while Rows.Count > FRowCount do Rows.Delete(Rows.Count - 1);
        end;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaStringCase                                                             
  //---------------------------------------------------------------------------

constructor TcaStringCase.Create(const AOptions: array of String);
begin
  inherited Create;
  FOptions := TcaStringList.Create(AOptions);
end;

destructor TcaStringCase.Destroy;
begin
  FOptions.Free;
  inherited;
end;

  // Public methods 

procedure TcaStringCase.AddOption(const AOption: String);
begin
  FOptions.Add(AOption);
end;

function TcaStringCase.CaseOf(const AString: String): Integer;
begin
  Result := FOptions.IndexOf(AString);
end;

  //---------------------------------------------------------------------------
  // TcaArgs                                                                   
  //---------------------------------------------------------------------------

  // Public overridden methods 

procedure TcaArgs.AfterConstruction;
begin
  inherited;
  UpdateArgs;
end;

  // IcaArgs methods 

function TcaArgs.HasArg(const AArg: String): Boolean;
begin
  Result := (IndexOfName(AArg) >= 0) or (IndexOf(AArg) >= 0);
end;

  // Private methods 

procedure TcaArgs.UpdateArgs;
var
  Index: Integer;
  Param: String;
begin
  Clear;
  for Index := 1 to ParamCount do
    begin
      Param := ParamStr(Index);
      Add(Param);
    end;
end;

  // Property methods 

function TcaArgs.GetExecutable: String;
begin
  Result := ParamStr(0);
end;

  //---------------------------------------------------------------------------
  // TcaAutoFree                                                               
  //---------------------------------------------------------------------------

constructor TcaAutoFree.Create(AInstance: TObject);
begin
  inherited Create;
  FInstance := AInstance;
end;

destructor TcaAutoFree.Destroy;
begin
  FInstance.Free;
  inherited;
end;

  // Property methods 

function TcaAutoFree.GetInstance: Pointer;
begin
  Result := FInstance;
end;

function Auto(AInstance: TObject): IcaAutoFree;
begin
  Result := TcaAutoFree.Create(AInstance);
end;

  //---------------------------------------------------------------------------
  // TcaProperties                                                             
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaProperties.Create;
begin
  inherited;
  Initialize;
end;

destructor TcaProperties.Destroy;
begin
  inherited;
end;

  // Protected virtual methods 

procedure TcaProperties.Finalize;
begin
  // Virtual 
end;

procedure TcaProperties.Initialize;
begin
  // Virtual 
end;

  //---------------------------------------------------------------------------
  // TcaNotifyProperties                                                       
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaNotifyProperties.Create(ANotifyMethod: TcaProcedure);
begin
  inherited Create;
  FNotifyMethod := ANotifyMethod;
end;

destructor TcaNotifyProperties.Destroy;
begin
  inherited;
end;

  // Protected static methods 

procedure TcaNotifyProperties.Changed;
begin
  if Assigned(FNotifyMethod) then FNotifyMethod;
end;

  // Private methods 

procedure TcaNotifyProperties.SetBooleanProperty(var AProperty: Boolean; AValue: Boolean);
begin
  if AValue <> AProperty then
    begin
      AProperty := AValue;
      Changed;
    end;
end;

procedure TcaNotifyProperties.SetIntegerProperty(var AProperty: Integer; AValue: Integer);
begin
  if AValue <> AProperty then
    begin
      AProperty := AValue;
      Changed;
    end;
end;

end.


