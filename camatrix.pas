unit caMatrix;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,
  Math,
  Clipbrd,
  TypInfo,
  Contnrs,
  Variants,

  // ca units 
  caClasses,
  caConsts,
  caUtils,
  caCell,
  caVector,
  caExcel,
  caXML,
  caLog,
  caMime,
  caTypes;

type

  EcaMatrixError = class(EcaException);


  TcaGetBooleanAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                          ABoolean: Boolean; var AString: String) of object;

  TcaGetDateTimeAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                           ADateTime: TDateTime; var AFormat, AString: String) of object;

  TcaGetDoubleAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                         ADouble: Double; var AFormat, AString: String) of object;

  TcaGetExtendedAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                           AExtended: Extended; var AFormat, AString: String) of object;

  TcaGetFormulaAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer; var AString: String) of object;

  TcaGetInt64AsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                        AInteger: Integer; var AFormat, AString: String) of object;

  TcaGetIntegerAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                          AInteger: Integer; var AFormat, AString: String) of object;

  TcaGetMemoAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                       AMemo: TStrings; var AString: String) of object;

  TcaGetObjectAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                         AObject: TObject; var AString: String) of object;

  TcaGetSingleAsStringEvent = procedure(Sender: TObject; ACol, ARow: Integer;
                                         ASingle: Single; var AFormat, AString: String) of object;

  TcaSetCellValueEvent = procedure(Sender: TObject; ACol, ARow: Integer; const ACell: TcaCell) of object;

  TcaSetBooleanEvent = procedure(Sender: TObject; ACol, ARow: Integer; const ABoolean: Boolean) of object;

  TcaSetDateTimeEvent = procedure(Sender: TObject; ACol, ARow: Integer; const ADateTime: TDateTime) of object;

  TcaSetDoubleEvent = procedure(Sender: TObject; ACol, ARow: Integer; const ADouble: Double) of object;

  TcaSetExtendedEvent = procedure(Sender: TObject; ACol, ARow: Integer; const AExtended: Extended) of object;

  TcaSetStringEvent = procedure(Sender: TObject; ACol, ARow: Integer; const AString: string) of object;

  TcaSetFormulaEvent = procedure(Sender: TObject; ACol, ARow: Integer; const AFormula: string) of object;

  TcaSetInt64Event = procedure(Sender: TObject; ACol, ARow: Integer; const AInt64: Int64) of object;

  TcaSetIntegerEvent = procedure(Sender: TObject; ACol, ARow: Integer; const AInteger: Integer) of object;

  TcaSetMemoEvent = procedure(Sender: TObject; ACol, ARow: Integer; const AMemo: TStrings) of object;

  TcaSetObjectEvent = procedure(Sender: TObject; ACol, ARow: Integer; const AObject: TObject) of object;

  TcaSetSingleEvent = procedure(Sender: TObject; ACol, ARow: Integer; const ASingle: Single) of object;

  //----------------------------------------------------------------------------
  // IcaMatrixHeader                                                            
  //----------------------------------------------------------------------------

  IcaMatrixHeader = interface
  ['{CF673266-3EF8-48B0-9FC5-5A8C80D1316B}']
    // Property methods 
    function GetColCount: Integer;
    function GetColumnNames: String;
    function GetColumnType(Index: Integer): TcaCellType;
    function GetMagic: String;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetColumnNames(const Value: String);
    procedure SetColumnType(Index: Integer; const Value: TcaCellType);
    procedure SetMagic(const Value: String);
    procedure SetRowCount(const Value: Integer);
    // Public methods 
    procedure Read(Stream: TStream);
    procedure Write(Stream: TStream);
    // Properties 
    property ColCount: Integer read GetColCount write SetColCount;
    property ColumnNames: String read GetColumnNames write SetColumnNames;
    property ColumnTypes[ACol: Integer]: TcaCellType read GetColumnType write SetColumnType;
    property Magic: String read GetMagic write SetMagic;
    property RowCount: Integer read GetRowCount write SetRowCount;
  end;

  //----------------------------------------------------------------------------
  // TcaMatrixHeader                                                            
  //----------------------------------------------------------------------------

  TcaMatrix = class;

  TcaMatrixHeader = class(TObject)
  private
    // Private fields 
    FColCount: Integer;
    FColumnNames: String;
    FColumnTypes: TcaByteString;
    FMagic: String;
    FMatrix: TcaMatrix;
    FRowCount: Integer;
    // Private methods 
    procedure ReadColumnNames(Stream: TStream);
    procedure ReadDimensions(Stream: TStream);
    procedure ReadMagic(Stream: TStream);
    procedure ReadString(Stream: TStream; var AString: string);
    procedure WriteColumnNames(Stream: TStream);
    procedure WriteDimensions(Stream: TStream);
    procedure WriteMagic(Stream: TStream);
    procedure WriteString(Stream: TStream; const AString: string);
  public
    // Create/Destroy 
    constructor Create(const AMatrixRef: IInterface);
    destructor Destroy; override;
    // Public methods 
    function IsValid: Boolean;
    procedure Read(Stream: TStream);
    procedure Write(Stream: TStream);
    procedure UpdateFromMatrix;
    procedure UpdateMatrix;
  end;

  //---------------------------------------------------------------------------
  // TcaCellConverter                                                          
  //---------------------------------------------------------------------------

  TcaCellConverter = class(TObject)
  private
    FCol: Integer;
    FEmptyMemo: TStrings;
    FMatrix: TcaMatrix;
    FRow: Integer;
    // Converter property methods 
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsExtended: Extended;
    function GetAsFormula: String;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsMemo: TStrings;
    function GetAsObject: TObject;
    function GetAsSingle: Single;
    function GetAsString: String;
    function GetAsVariant: Variant;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsExtended(const Value: Extended);
    procedure SetAsFormula(const Value: String);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsMemo(const Value: TStrings);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsSingle(const Value: Single);
    procedure SetAsString(const Value: String);
    procedure SetAsVariant(const Value: Variant);
    // Private methods 
    function GetFormulaAsDouble(const AFormula: String): Double;
    function GetFormulaAsExtended(const AFormula: String): Extended;
    function GetFormulaAsInt64(const AFormula: String): Int64;
    function GetFormulaAsInteger(const AFormula: String): Integer;
    function GetFormulaAsSingle(const AFormula: String): Single;
    procedure AddNewCell(const ACell: TcaCell; WasSelected: Boolean; IsSetting: Boolean);
    procedure ReleaseOldCell(var WasSelected: Boolean);
  public
    constructor Create(AMatrix: TcaMatrix);
    destructor Destroy; override;
    // Properties 
    property Col: Integer read FCol write FCol;
    property Row: Integer read FRow write FRow;
    // Converter properties 
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsFormula: String read GetAsFormula write SetAsFormula;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsMemo: TStrings read GetAsMemo write SetAsMemo;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsString: String read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  //---------------------------------------------------------------------------
  // IcaMatrix                                                                 
  //---------------------------------------------------------------------------

  IcaMatrix = interface
  ['{D290F18C-9BCD-4BC2-B99D-26A4456CAC4A}']
    // Property methods 
    function GetAutoColCount: Boolean;
    function GetAutoRowCount: Boolean;
    function GetCell(ACol, ARow: Integer): TcaCell;
    function GetCellIsNumeric(ACol, ARow: Integer): Boolean;
    function GetColCount: Integer;
    function GetColumnNames: TStrings;
    function GetColumnType(Index: Integer): TcaCellType;
    function GetDateFormat: String;
    function GetFloatFormat: String;
    function GetIntegerFormat: String;
    function GetMatrixCopy: TcaMatrix;
    function GetName: String;
    function GetOwnsObject(ACol, ARow: Integer): Boolean;
    function GetRowCount: Integer;
    function GetSaveUnsorted: Boolean;
    function GetSortColumns: TcaIntegerVector;
    function GetSortDirections: TcaIntegerVector;
    function GetSorted: Boolean;
    function GetStringSortCaseInsensitive: Boolean;
    function GetXML: String;
    function GetXMLRowContent: String;
    procedure SetAutoColCount(const Value: Boolean);
    procedure SetAutoRowCount(const Value: Boolean);
    procedure SetCell(ACol, ARow: Integer; const Value: TcaCell);
    procedure SetColCount(const Value: Integer);
    procedure SetColumnNames(const Value: TStrings);
    procedure SetColumnType(Index: Integer; const Value: TcaCellType);
    procedure SetDateFormat(const Value: String);
    procedure SetFloatFormat(const Value: String);
    procedure SetIntegerFormat(const Value: String);
    procedure SetName(const Value: String);
    procedure SetOwnsObject(ACol, ARow: Integer; const Value: Boolean);
    procedure SetRowCount(const Value: Integer);
    procedure SetSaveUnsorted(const Value: Boolean);
    procedure SetSorted(const Value: Boolean);
    procedure SetStringSortCaseInsensitive(const Value: Boolean);
    procedure SetXML(const Value: String);
    // Cell access property methods 
    function GetBoolean(ACol, ARow: Integer): Boolean;
    function GetDateTime(ACol, ARow: Integer): TDateTime;
    function GetDouble(ACol, ARow: Integer): Double;
    function GetExtended(ACol, ARow: Integer): Extended;
    function GetFormula(ACol, ARow: Integer): String;
    function GetInteger(ACol, ARow: Integer): Integer;
    function GetInt64(ACol, ARow: Integer): Int64;
    function GetMemo(ACol, ARow: Integer): TStrings;
    function GetObject(ACol, ARow: Integer): TObject;
    function GetSelected(ACol, ARow: Integer): Boolean;
    function GetSingle(ACol, ARow: Integer): Single;
    function GetString(ACol, ARow: Integer): String;
    function GetVariant(ACol, ARow: Integer): Variant;
    procedure SetBoolean(ACol, ARow: Integer; const Value: Boolean);
    procedure SetDateTime(ACol, ARow: Integer; const Value: TDateTime);
    procedure SetDouble(ACol, ARow: Integer; const Value: Double);
    procedure SetExtended(ACol, ARow: Integer; const Value: Extended);
    procedure SetFormula(ACol, ARow: Integer; const Value: String);
    procedure SetInteger(ACol, ARow: Integer; const Value: Integer);
    procedure SetInt64(ACol, ARow: Integer; const Value: Int64);
    procedure SetMemo(ACol, ARow: Integer; const Value: TStrings);
    procedure SetObject(ACol, ARow: Integer; const Value: TObject);
    procedure SetSelected(ACol, ARow: Integer; const Value: Boolean);
    procedure SetSingle(ACol, ARow: Integer; const Value: Single);
    procedure SetString(ACol, ARow: Integer; const Value: String);
    procedure SetVariant(ACol, ARow: Integer; const Value: Variant);
    // Get event property methods 
    function GetOnGetBooleanAsString: TcaGetBooleanAsStringEvent;
    function GetOnGetDateTimeAsString: TcaGetDateTimeAsStringEvent;
    function GetOnGetDoubleAsString: TcaGetDoubleAsStringEvent;
    function GetOnGetExtendedAsString: TcaGetExtendedAsStringEvent;
    function GetOnGetFormulaAsString: TcaGetFormulaAsStringEvent;
    function GetOnGetInt64AsString: TcaGetInt64AsStringEvent;
    function GetOnGetIntegerAsString: TcaGetIntegerAsStringEvent;
    function GetOnGetMemoAsString: TcaGetMemoAsStringEvent;
    function GetOnGetObjectAsString: TcaGetObjectAsStringEvent;
    function GetOnGetSingleAsString: TcaGetSingleAsStringEvent;
    procedure SetOnGetBooleanAsString(const Value: TcaGetBooleanAsStringEvent);
    procedure SetOnGetDateTimeAsString(const Value: TcaGetDateTimeAsStringEvent);
    procedure SetOnGetDoubleAsString(const Value: TcaGetDoubleAsStringEvent);
    procedure SetOnGetExtendedAsString(const Value: TcaGetExtendedAsStringEvent);
    procedure SetOnGetFormulaAsString(const Value: TcaGetFormulaAsStringEvent);
    procedure SetOnGetInt64AsString(const Value: TcaGetInt64AsStringEvent);
    procedure SetOnGetIntegerAsString(const Value: TcaGetIntegerAsStringEvent);
    procedure SetOnGetMemoAsString(const Value: TcaGetMemoAsStringEvent);
    procedure SetOnGetObjectAsString(const Value: TcaGetObjectAsStringEvent);
    procedure SetOnGetSingleAsString(const Value: TcaGetSingleAsStringEvent);
    // Set event property methods 
    function GetOnSetBoolean: TcaSetBooleanEvent;
    function GetOnSetCellValue: TcaSetCellValueEvent;
    function GetOnSetDateTime: TcaSetDateTimeEvent;
    function GetOnSetDouble: TcaSetDoubleEvent;
    function GetOnSetExtended: TcaSetExtendedEvent;
    function GetOnSetString: TcaSetStringEvent;
    function GetOnSetFormula: TcaSetFormulaEvent;
    function GetOnSetInt64: TcaSetInt64Event;
    function GetOnSetInteger: TcaSetIntegerEvent;
    function GetOnSetMemo: TcaSetMemoEvent;
    function GetOnSetObject: TcaSetObjectEvent;
    function GetOnSetSingle: TcaSetSingleEvent;
    procedure SetOnSetBoolean(const Value: TcaSetBooleanEvent);
    procedure SetOnSetCellValue(const Value: TcaSetCellValueEvent);
    procedure SetOnSetDateTime(const Value: TcaSetDateTimeEvent);
    procedure SetOnSetDouble(const Value: TcaSetDoubleEvent);
    procedure SetOnSetExtended(const Value: TcaSetExtendedEvent);
    procedure SetOnSetString(const Value: TcaSetStringEvent);
    procedure SetOnSetFormula(const Value: TcaSetFormulaEvent);
    procedure SetOnSetInt64(const Value: TcaSetInt64Event);
    procedure SetOnSetInteger(const Value: TcaSetIntegerEvent);
    procedure SetOnSetMemo(const Value: TcaSetMemoEvent);
    procedure SetOnSetObject(const Value: TcaSetObjectEvent);
    procedure SetOnSetSingle(const Value: TcaSetSingleEvent);
    // Protected methods 
    function GetBooleanAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetDateTimeAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetDoubleAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetExtendedAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetFormulaAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetInt64AsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetIntegerAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetMemoAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetObjectAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetSingleAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    procedure ReleaseCell(ACol, ARow: Integer);
    procedure ReleaseCells;
    // Public methods 
    function AddCol: Integer;
    function AddRow: Integer;
    function AsString(ACopyColumnNames: Boolean = True): string; overload;
    function AsString(AColFrom, ARowFrom, AColTo, ARowTo: Integer; ACopyColumnNames: Boolean = True): string; overload;
    function FindBooleanInColumn(ABoolean: Boolean; ACol: Integer): Integer;
    function FindDateTimeInColumn(ADateTime: TDateTime; ACol: Integer): Integer;
    function FindDoubleInColumn(ADouble: Double; ACol: Integer): Integer;
    function FindExtendedInColumn(AExtended: Extended; ACol: Integer): Integer;
    function FindFormulaInColumn(const AFormula: String; ACol: Integer): Integer;
    function FindInt64InColumn(AInt64: Int64; ACol: Integer): Integer;
    function FindIntegerInColumn(AInteger: Integer; ACol: Integer): Integer;
    function FindMemoInColumn(AMemo: TStrings; ACol: Integer): Integer;
    function FindObjectInColumn(AObject: TObject; ACol: Integer): Integer;
    function FindSingleInColumn(ASingle: Single; ACol: Integer): Integer;
    function FindStringInColumn(const AString: String; ACol: Integer): Integer;
    procedure AddCols(ACount: Integer);
    procedure AddColumnNames(AColumnNames: array of string);
    procedure AddFromMatrix(Source: TcaMatrix);
    procedure AddRows(ACount: Integer);
    procedure AddRowValues(AValues: array of Variant);
    procedure AddSortColumn(const ACol: Integer; const ASortDirection: TcaSortDirection);
    procedure AssignCell(Source: TcaMatrix; const SrcCol, SrcRow, ACol, ARow: Integer);
    procedure Clear; overload;
    procedure Clear(AColCount, ARowCount: Integer); overload;
    procedure ClearCell(const ACol, ARow: Integer);
    procedure ClearRow(const ARow: Integer);
    procedure ClearCol(const ACol: Integer);    
    procedure ClearSortColumns;
    procedure CopyCol(const AFromCol, AToCol: Integer);
    procedure CopyRow(const AFromRow, AToRow: Integer);
    procedure CopyToClipboard(ACopyColumnNames: Boolean = True); overload;
    procedure CopyToClipboard(AColFrom, ARowFrom, AColTo, ARowTo: Integer; ACopyColumnNames: Boolean = True); overload;
    procedure DeleteCol(const ACol: Integer);
    procedure DeleteRow(const ARow: Integer);
    procedure InsertCol(const ACol: Integer);
    procedure InsertRow(const ARow: Integer);
    procedure MoveRowDown(const ARow: Integer);
    procedure MoveRowUp(const ARow: Integer);
    // Fill matrix methods 
    procedure Fill(AValue: Boolean); overload;
    procedure Fill(AValue: TDateTime); overload;
    procedure Fill(AValue: Double); overload;
    procedure Fill(AValue: Extended); overload;
    procedure Fill(AValue: String); overload;
    procedure Fill(AValue: Integer); overload;
    procedure Fill(AValue: Int64); overload;
    procedure Fill(AValue: TStrings); overload;
    procedure Fill(AValue: TObject); overload;
    procedure Fill(AValue: Single); overload;
    // Fill matrix column methods 
    procedure FillCol(ACol: Integer; AValue: Boolean); overload;
    procedure FillCol(ACol: Integer; AValue: TDateTime); overload;
    procedure FillCol(ACol: Integer; AValue: Double); overload;
    procedure FillCol(ACol: Integer; AValue: Extended); overload;
    procedure FillCol(ACol: Integer; AValue: String); overload;
    procedure FillCol(ACol: Integer; AValue: Integer); overload;
    procedure FillCol(ACol: Integer; AValue: Int64); overload;
    procedure FillCol(ACol: Integer; AValue: TStrings); overload;
    procedure FillCol(ACol: Integer; AValue: TObject); overload;
    procedure FillCol(ACol: Integer; AValue: Single); overload;
    // Fill matrix row methods     
    procedure FillRow(ARow: Integer; AValue: Boolean); overload;
    procedure FillRow(ARow: Integer; AValue: TDateTime); overload;
    procedure FillRow(ARow: Integer; AValue: Double); overload;
    procedure FillRow(ARow: Integer; AValue: Extended); overload;
    procedure FillRow(ARow: Integer; AValue: String); overload;
    procedure FillRow(ARow: Integer; AValue: Integer); overload;
    procedure FillRow(ARow: Integer; AValue: Int64); overload;
    procedure FillRow(ARow: Integer; AValue: TStrings); overload;
    procedure FillRow(ARow: Integer; AValue: TObject); overload;
    procedure FillRow(ARow: Integer; AValue: Single); overload;
    procedure GetColumnAsStrings(const ACol: Integer; AStrings: TStrings); overload;
    procedure GetColumnAsStrings(const ACol: Integer; AStrings: IcaStringList); overload;
    // Saving and Loading 
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromXML(const AFileName: String);
    procedure SaveToExcel(const AFileName: String);
    procedure SaveToFile(const AFileName: String);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToXML(const AFileName: String);
    // Matrix math 
    procedure AddMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    procedure DivideMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    procedure MultiplyMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    procedure SubtractMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    // Properties 
    property AutoColCount: Boolean read GetAutoColCount write SetAutoColCount;
    property AutoRowCount: Boolean read GetAutoRowCount write SetAutoRowCount;
    property Cells[ACol, ARow: Integer]: TcaCell read GetCell write SetCell;
    property CellIsNumeric[ACol, ARow: Integer]: Boolean read GetCellIsNumeric;
    property ColCount: Integer read GetColCount write SetColCount;
    property ColumnNames: TStrings read GetColumnNames write SetColumnNames;
    property ColumnTypes[ACol: Integer]: TcaCellType read GetColumnType write SetColumnType;
    property DateFormat: String read GetDateFormat write SetDateFormat;
    property FloatFormat: String read GetFloatFormat write SetFloatFormat;
    property IntegerFormat: String read GetIntegerFormat write SetIntegerFormat;
    property MatrixCopy: TcaMatrix read GetMatrixCopy;
    property Name: String read GetName write SetName;
    property OwnsObject[ACol, ARow: Integer]: Boolean read GetOwnsObject write SetOwnsObject;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property SaveUnsorted: Boolean read GetSaveUnsorted write SetSaveUnsorted;
    property SortColumns: TcaIntegerVector read GetSortColumns;
    property SortDirections: TcaIntegerVector read GetSortDirections;
    property Sorted: Boolean read GetSorted write SetSorted;
    property StringSortCaseInsensitive: Boolean read GetStringSortCaseInsensitive write SetStringSortCaseInsensitive;
    property XML: String read GetXML write SetXML;
    property XMLRowContent: String read GetXMLRowContent;
    // Cell access properties 
    property Booleans[ACol, ARow: Integer]: Boolean read GetBoolean write SetBoolean;
    property DateTimes[ACol, ARow: Integer]: TDateTime read GetDateTime write SetDateTime;
    property Doubles[ACol, ARow: Integer]: Double read GetDouble write SetDouble;
    property Extendeds[ACol, ARow: Integer]: Extended read GetExtended write SetExtended;
    property Formulas[ACol, ARow: Integer]: String read GetFormula write SetFormula;
    property Int64s[ACol, ARow: Integer]: Int64 read GetInt64 write SetInt64;
    property Integers[ACol, ARow: Integer]: Integer read GetInteger write SetInteger;
    property Memos[ACol, ARow: Integer]: TStrings read GetMemo write SetMemo;
    property Objects[ACol, ARow: Integer]: TObject read GetObject write SetObject;
    property Selected[ACol, ARow: Integer]: Boolean read GetSelected write SetSelected;
    property Singles[ACol, ARow: Integer]: Single read GetSingle write SetSingle;
    property Strings[ACol, ARow: Integer]: String read GetString write SetString;
    property Variants[ACol, ARow: Integer]: Variant read GetVariant write SetVariant;
    // Get related Events 
    property OnGetBooleanAsString: TcaGetBooleanAsStringEvent read GetOnGetBooleanAsString write SetOnGetBooleanAsString;
    property OnGetDateTimeAsString: TcaGetDateTimeAsStringEvent read GetOnGetDateTimeAsString write SetOnGetDateTimeAsString;
    property OnGetDoubleAsString: TcaGetDoubleAsStringEvent read GetOnGetDoubleAsString write SetOnGetDoubleAsString;
    property OnGetExtendedAsString: TcaGetExtendedAsStringEvent read GetOnGetExtendedAsString write SetOnGetExtendedAsString;
    property OnGetFormulaAsString: TcaGetFormulaAsStringEvent read GetOnGetFormulaAsString write SetOnGetFormulaAsString;
    property OnGetInt64AsString: TcaGetInt64AsStringEvent read GetOnGetInt64AsString write SetOnGetInt64AsString;
    property OnGetIntegerAsString: TcaGetIntegerAsStringEvent read GetOnGetIntegerAsString write SetOnGetIntegerAsString;
    property OnGetMemoAsString: TcaGetMemoAsStringEvent read GetOnGetMemoAsString write SetOnGetMemoAsString;
    property OnGetObjectAsString: TcaGetObjectAsStringEvent read GetOnGetObjectAsString write SetOnGetObjectAsString;
    property OnGetSingleAsString: TcaGetSingleAsStringEvent read GetOnGetSingleAsString write SetOnGetSingleAsString;
    // Set related Events 
    property OnSetBoolean: TcaSetBooleanEvent read GetOnSetBoolean write SetOnSetBoolean;
    property OnSetCellValue: TcaSetCellValueEvent read GetOnSetCellValue write SetOnSetCellValue;
    property OnSetDateTime: TcaSetDateTimeEvent read GetOnSetDateTime write SetOnSetDateTime;
    property OnSetDouble: TcaSetDoubleEvent read GetOnSetDouble write SetOnSetDouble;
    property OnSetExtended: TcaSetExtendedEvent read GetOnSetExtended write SetOnSetExtended;
    property OnSetString: TcaSetStringEvent read GetOnSetString write SetOnSetString;
    property OnSetFormula: TcaSetFormulaEvent read GetOnSetFormula write SetOnSetFormula;
    property OnSetInt64: TcaSetInt64Event read GetOnSetInt64 write SetOnSetInt64;
    property OnSetInteger: TcaSetIntegerEvent read GetOnSetInteger write SetOnSetInteger;
    property OnSetMemo: TcaSetMemoEvent read GetOnSetMemo write SetOnSetMemo;
    property OnSetObject: TcaSetObjectEvent read GetOnSetObject write SetOnSetObject;
    property OnSetSingle: TcaSetSingleEvent read GetOnSetSingle write SetOnSetSingle;
  end;

  //---------------------------------------------------------------------------
  // TcaMatrix                                                                 
  //---------------------------------------------------------------------------

  TcaMatrix = class(TcaInterfacedPersistent, IcaMatrix, IcaCloneable, IcaAssignable, IcaLoggable)
  private
    FAutoColCount: Boolean;
    FAutoRowCount: Boolean;
    FCells: IcaSparseMatrix;
    FColumnNames: TStrings;
    FColumnTypeIndex: Integer;
    FColumnTypes: TcaByteString;
    FConverter: TcaCellConverter;
    FCompRow: Integer;
    FDateFormat: String;
    FFloatFormat: String;
    FIntegerFormat: String;
    FMatrixCopy: TcaMatrix;
    FName: String;
    FSaveUnsorted: Boolean;
    FSortColumns: TcaIntegerVector;
    FSortDirections: TcaIntegerVector;
    FSorted: Boolean;
    FStringSortCaseInsensitive: Boolean;
    FSwapRow: Integer;
    FXMLReadRow: Integer;
    // Get events fields 
    FOnGetBooleanAsString: TcaGetBooleanAsStringEvent;
    FOnGetDateTimeAsString: TcaGetDateTimeAsStringEvent;
    FOnGetDoubleAsString: TcaGetDoubleAsStringEvent;
    FOnGetExtendedAsString: TcaGetExtendedAsStringEvent;
    FOnGetFormulaAsString: TcaGetFormulaAsStringEvent;
    FOnGetInt64AsString: TcaGetInt64AsStringEvent;
    FOnGetIntegerAsString: TcaGetIntegerAsStringEvent;
    FOnGetMemoAsString: TcaGetMemoAsStringEvent;
    FOnGetObjectAsString: TcaGetObjectAsStringEvent;
    FOnGetSingleAsString: TcaGetSingleAsStringEvent;
    // Get events fields 
    FOnSetBoolean: TcaSetBooleanEvent;
    FOnSetCellValue: TcaSetCellValueEvent;
    FOnSetDateTime: TcaSetDateTimeEvent;
    FOnSetDouble: TcaSetDoubleEvent;
    FOnSetExtended: TcaSetExtendedEvent;
    FOnSetString: TcaSetStringEvent;
    FOnSetFormula: TcaSetFormulaEvent;
    FOnSetInt64: TcaSetInt64Event;
    FOnSetInteger: TcaSetIntegerEvent;
    FOnSetMemo: TcaSetMemoEvent;
    FOnSetObject: TcaSetObjectEvent;
    FOnSetSingle: TcaSetSingleEvent;
    // Cell access property methods 
    function GetBoolean(ACol, ARow: Integer): Boolean;
    function GetDateTime(ACol, ARow: Integer): TDateTime;
    function GetDouble(ACol, ARow: Integer): Double;
    function GetExtended(ACol, ARow: Integer): Extended;
    function GetFormula(ACol, ARow: Integer): String;
    function GetInt64(ACol, ARow: Integer): Int64;
    function GetInteger(ACol, ARow: Integer): Integer;
    function GetMemo(ACol, ARow: Integer): TStrings;
    function GetObject(ACol, ARow: Integer): TObject;
    function GetSelected(ACol, ARow: Integer): Boolean;
    function GetSingle(ACol, ARow: Integer): Single;
    function GetString(ACol, ARow: Integer): String;
    function GetVariant(ACol, ARow: Integer): Variant;
    procedure SetBoolean(ACol, ARow: Integer; const Value: Boolean);
    procedure SetDateTime(ACol, ARow: Integer; const Value: TDateTime);
    procedure SetDouble(ACol, ARow: Integer; const Value: Double);
    procedure SetExtended(ACol, ARow: Integer; const Value: Extended);
    procedure SetFormula(ACol, ARow: Integer; const Value: String);
    procedure SetInt64(ACol, ARow: Integer; const Value: Int64);
    procedure SetInteger(ACol, ARow: Integer; const Value: Integer);
    procedure SetMemo(ACol, ARow: Integer; const Value: TStrings);
    procedure SetObject(ACol, ARow: Integer; const Value: TObject);
    procedure SetSelected(ACol, ARow: Integer; const Value: Boolean);
    procedure SetSingle(ACol, ARow: Integer; const Value: Single);
    procedure SetString(ACol, ARow: Integer; const Value: String);
    procedure SetVariant(ACol, ARow: Integer; const Value: Variant);
    // Get event property methods 
    function GetOnGetBooleanAsString: TcaGetBooleanAsStringEvent;
    function GetOnGetDateTimeAsString: TcaGetDateTimeAsStringEvent;
    function GetOnGetDoubleAsString: TcaGetDoubleAsStringEvent;
    function GetOnGetExtendedAsString: TcaGetExtendedAsStringEvent;
    function GetOnGetFormulaAsString: TcaGetFormulaAsStringEvent;
    function GetOnGetInt64AsString: TcaGetInt64AsStringEvent;
    function GetOnGetIntegerAsString: TcaGetIntegerAsStringEvent;
    function GetOnGetMemoAsString: TcaGetMemoAsStringEvent;
    function GetOnGetObjectAsString: TcaGetObjectAsStringEvent;
    function GetOnGetSingleAsString: TcaGetSingleAsStringEvent;
    procedure SetOnGetBooleanAsString(const Value: TcaGetBooleanAsStringEvent);
    procedure SetOnGetDateTimeAsString(const Value: TcaGetDateTimeAsStringEvent);
    procedure SetOnGetDoubleAsString(const Value: TcaGetDoubleAsStringEvent);
    procedure SetOnGetExtendedAsString(const Value: TcaGetExtendedAsStringEvent);
    procedure SetOnGetFormulaAsString(const Value: TcaGetFormulaAsStringEvent);
    procedure SetOnGetInt64AsString(const Value: TcaGetInt64AsStringEvent);
    procedure SetOnGetIntegerAsString(const Value: TcaGetIntegerAsStringEvent);
    procedure SetOnGetMemoAsString(const Value: TcaGetMemoAsStringEvent);
    procedure SetOnGetObjectAsString(const Value: TcaGetObjectAsStringEvent);
    procedure SetOnGetSingleAsString(const Value: TcaGetSingleAsStringEvent);
    // Set event property methods 
    function GetOnSetBoolean: TcaSetBooleanEvent;
    function GetOnSetCellValue: TcaSetCellValueEvent;
    function GetOnSetDateTime: TcaSetDateTimeEvent;
    function GetOnSetDouble: TcaSetDoubleEvent;
    function GetOnSetExtended: TcaSetExtendedEvent;
    function GetOnSetString: TcaSetStringEvent;
    function GetOnSetFormula: TcaSetFormulaEvent;
    function GetOnSetInt64: TcaSetInt64Event;
    function GetOnSetInteger: TcaSetIntegerEvent;
    function GetOnSetMemo: TcaSetMemoEvent;
    function GetOnSetObject: TcaSetObjectEvent;
    function GetOnSetSingle: TcaSetSingleEvent;
    procedure SetOnSetBoolean(const Value: TcaSetBooleanEvent);
    procedure SetOnSetCellValue(const Value: TcaSetCellValueEvent);
    procedure SetOnSetDateTime(const Value: TcaSetDateTimeEvent);
    procedure SetOnSetDouble(const Value: TcaSetDoubleEvent);
    procedure SetOnSetExtended(const Value: TcaSetExtendedEvent);
    procedure SetOnSetString(const Value: TcaSetStringEvent);
    procedure SetOnSetFormula(const Value: TcaSetFormulaEvent);
    procedure SetOnSetInt64(const Value: TcaSetInt64Event);
    procedure SetOnSetInteger(const Value: TcaSetIntegerEvent);
    procedure SetOnSetMemo(const Value: TcaSetMemoEvent);
    procedure SetOnSetObject(const Value: TcaSetObjectEvent);
    procedure SetOnSetSingle(const Value: TcaSetSingleEvent);
    // Saving and loading private methods 
    function LoadBooleanFromStream(Stream: TStream): Boolean;
    function LoadCellTypeFromStream(Stream: TStream): TcaCellType;
    function LoadDateTimeFromStream(Stream: TStream): TDateTime;
    function LoadDoubleFromStream(Stream: TStream): Double;
    function LoadExtendedFromStream(Stream: TStream): Extended;
    function LoadFormulaFromStream(Stream: TStream): String;
    function LoadIntegerFromStream(Stream: TStream): Integer;
    function LoadInt64FromStream(Stream: TStream): Int64;
    function LoadMemoFromStream(Stream: TStream): IcaStringList;
    function LoadSingleFromStream(Stream: TStream): Single;
    function LoadStringFromStream(Stream: TStream): String;
    function ReadHeader(Stream: TStream): Boolean;
    procedure LoadRowFromStream(ARow: Integer; Stream: TStream);
    procedure SaveBooleanToStream(Cell: TcaBooleanCell; Stream: TStream);
    procedure SaveCellTypeToStream(CellType: TcaCellType; Stream: TStream);
    procedure SaveDateTimeToStream(Cell: TcaDateTimeCell; Stream: TStream);
    procedure SaveDoubleToStream(Cell: TcaDoubleCell; Stream: TStream);
    procedure SaveExtendedToStream(Cell: TcaExtendedCell; Stream: TStream);
    procedure SaveFormulaToStream(Cell: TcaFormulaCell; Stream: TStream);
    procedure SaveInt64ToStream(Cell: TcaInt64Cell; Stream: TStream);
    procedure SaveIntegerToStream(Cell: TcaIntegerCell; Stream: TStream);
    procedure SaveMemoToStream(Cell: TcaMemoCell; Stream: TStream);
    procedure SaveSingleToStream(Cell: TcaSingleCell; Stream: TStream);
    procedure SaveStringToStream(Cell: TcaStringCell; Stream: TStream);
    procedure SaveRowToStream(ARow: Integer; Stream: TStream);
    procedure WriteHeader(Stream: TStream);
    // Private methods 
    function CellTypeStrToCellType(const ACellTypeStr: string): TcaCellType;
    function CompareCells(const ACol1, ARow1, ACol2, ARow2: Integer): TcaCompareResult;
    function CompareRows(const ARow1, ARow2: Integer): TcaCompareResult;
    function CreateCell(ACellType: TcaCellType): TcaCell;
    function CreateXmlBuilder: IcaXmlBuilder;
    function GetXMLDocumentName: String;
    procedure AddXMLHeader(AXmlBuilder: IcaXmlBuilder);
    procedure AddXMLFooter(AXmlBuilder: IcaXmlBuilder);
    procedure CheckAutoColCount(ACol: Integer);
    procedure CheckAutoRowCount(ARow: Integer);
    procedure CheckDimensions(AMatrix: TcaMatrix);
    procedure PerformMathOperation(AOperation: TcaMathOperation; AMatrix, AResultMatrix: TcaMatrix);
    procedure ResetSwapRow;
    procedure SaveUnsortedMatrix;
    procedure SwapRows(const ARow1, ARow2: Integer);
    procedure SynchronizeColumnNames;
    // XML reading private methods 
    function BuildXMLAttributesList(const AAttributes: String): IcaStringList;
    function GetXMLColumnFromTag(const ATag: String): Integer;
    procedure RunXMLReader(AXMLReader: IcaXMLReader);
    procedure XMLColCountReceived(const AData: String);
    procedure XMLRowCountReceived(const AData: String);
    procedure XMLColumnNameReceived(const AData: String);
    procedure XMLColumnTypeReceived(const AData: String);
    procedure XMLDocumentNameReceived(const ATag: String);
    procedure XMLEndRowReceived;
    procedure XMLRowDataReceived(const ATag, AData, AAttributes: String);
    // Event handlers 
    procedure XMLTagEvent(Sender: TObject; const ATag, AAttributes: String; ALevel: Integer);
    procedure XMLEndTagEvent(Sender: TObject; const ATag: String; ALevel: Integer);
    procedure XMLDataEvent(Sender: TObject; const ATag, AData, AAttributes: String; ALevel: Integer);
  protected
    // Property methods 
    function GetAutoColCount: Boolean;
    function GetAutoRowCount: Boolean;
    function GetCell(ACol, ARow: Integer): TcaCell;
    function GetCellIsNumeric(ACol, ARow: Integer): Boolean;
    function GetColCount: Integer;
    function GetColumnNames: TStrings;
    function GetColumnType(ACol: Integer): TcaCellType;
    function GetDateFormat: String;
    function GetFloatFormat: String;
    function GetIntegerFormat: String;
    function GetMatrixCopy: TcaMatrix;
    function GetName: String;
    function GetOwnsObject(ACol, ARow: Integer): Boolean;
    function GetRowCount: Integer;
    function GetSaveUnsorted: Boolean;
    function GetSortColumns: TcaIntegerVector;
    function GetSortDirections: TcaIntegerVector;
    function GetSorted: Boolean;
    function GetStringSortCaseInsensitive: Boolean;
    function GetXML: String;
    function GetXMLRowContent: String;
    procedure SetAutoColCount(const Value: Boolean);
    procedure SetAutoRowCount(const Value: Boolean);
    procedure SetCell(ACol, ARow: Integer; const Value: TcaCell);
    procedure SetColCount(const Value: Integer);
    procedure SetColumnNames(const Value: TStrings);
    procedure SetColumnType(ACol: Integer; const Value: TcaCellType);
    procedure SetDateFormat(const Value: String);
    procedure SetFloatFormat(const Value: String);
    procedure SetIntegerFormat(const Value: String);
    procedure SetName(const Value: String);
    procedure SetOwnsObject(ACol, ARow: Integer; const Value: Boolean);
    procedure SetRowCount(const Value: Integer);
    procedure SetSaveUnsorted(const Value: Boolean);
    procedure SetSorted(const Value: Boolean);
    procedure SetStringSortCaseInsensitive(const Value: Boolean);
    procedure SetXML(const Value: String);
    // Protected methods 
    function GetBooleanAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetDateTimeAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetDoubleAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetExtendedAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetFormulaAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetInt64AsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetIntegerAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetMemoAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetObjectAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    function GetSingleAsString(ACol, ARow: Integer; ACell: TcaCell): String;
    procedure ReleaseCell(ACol, ARow: Integer);
    procedure ReleaseCells;
    procedure RestoreUnsortedMatrix;
    procedure Sort;
    procedure SynchronizeDimensions(AMatrix: TcaMatrix); overload;
    procedure SynchronizeDimensions(AMatrix: IcaMatrix); overload;
    // Protected virtual methods 
    function CreateBooleanCell: TcaBooleanCell; virtual;
    function CreateDateTimeCell: TcaDateTimeCell; virtual;
    function CreateDoubleCell: TcaDoubleCell; virtual;
    function CreateExtendedCell: TcaExtendedCell; virtual;
    function CreateFormulaCell: TcaFormulaCell; virtual;
    function CreateInt64Cell: TcaInt64Cell; virtual;
    function CreateIntegerCell: TcaIntegerCell; virtual;
    function CreateMemoCell: TcaMemoCell; virtual;
    function CreateObjectCell: TcaObjectCell; virtual;
    function CreateSingleCell: TcaSingleCell; virtual;
    function CreateStringCell: TcaStringCell; virtual;
    procedure DoBuildColumnNames; virtual;
    // Protected cell math methods 
    procedure DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); virtual;
    procedure DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); virtual;
    procedure DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); virtual;
    procedure DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); virtual;
    // Protected cell access methods 
    procedure DoGetBooleanAsString(ACol, ARow: Integer; ABoolean: Boolean; var AString: String); virtual;
    procedure DoGetDateTimeAsString(ACol, ARow: Integer; ADateTime: TDateTime; var AFormat, AString: String); virtual;
    procedure DoGetDoubleAsString(ACol, ARow: Integer; ADouble: Double; var AFormat, AString: String); virtual;
    procedure DoGetExtendedAsString(ACol, ARow: Integer; AExtended: Extended; var AFormat, AString: String); virtual;
    procedure DoGetFormulaAsString(ACol, ARow: Integer; var AString: String); virtual;
    procedure DoGetInt64AsString(ACol, ARow: Integer; AInt64: Int64; var AFormat, AString: String); virtual;
    procedure DoGetIntegerAsString(ACol, ARow: Integer; AInteger: Integer; var AFormat, AString: String); virtual;
    procedure DoGetMemoAsString(ACol, ARow: Integer; AMemo: TStrings; var AString: String); virtual;
    procedure DoGetObjectAsString(ACol, ARow: Integer; AObject: TObject; var AString: String); virtual;
    procedure DoGetSingleAsString(ACol, ARow: Integer; ASingle: Single; var AFormat, AString: String); virtual;
    // SetCell event trigger methods 
    procedure DoSetCellValue(ACol, ARow: Integer; const ACell: TcaCell);
    procedure DoSetBoolean(ACol, ARow: Integer; const ABoolean: Boolean);
    procedure DoSetDateTime(ACol, ARow: Integer; const ADateTime: TDateTime);
    procedure DoSetDouble(ACol, ARow: Integer; const ADouble: Double);
    procedure DoSetExtended(ACol, ARow: Integer; const AExtended: Extended);
    procedure DoSetString(ACol, ARow: Integer; const AString: string);
    procedure DoSetFormula(ACol, ARow: Integer; const AFormula: string);
    procedure DoSetInt64(ACol, ARow: Integer; const AInt64: Int64);
    procedure DoSetInteger(ACol, ARow: Integer; const AInteger: Integer);
    procedure DoSetMemo(ACol, ARow: Integer; const AMemo: TStrings);
    procedure DoSetObject(ACol, ARow: Integer; const AObject: TObject);
    procedure DoSetSingle(ACol, ARow: Integer; const ASingle: Single);
  public
    // Create/Destroy 
    constructor Create; overload;
    constructor Create(AAutoColCount, AAutoRowCount: Boolean); overload;
    constructor Create(AColCount, ARowCount: Integer); overload;
    constructor Create(AXML: string); overload;
    destructor Destroy; override;
    // IcaLoggable 
    procedure SendToLog(const AMsg: String; AClearLog: Boolean = False);
    // Public methods 
    function AddCol: Integer; virtual;
    function AddRow: Integer; virtual;
    function AsString(ACopyColumnNames: Boolean = True): string; overload;
    function AsString(AColFrom, ARowFrom, AColTo, ARowTo: Integer; ACopyColumnNames: Boolean = True): string; overload;
    function CloneAsObject: TcaInterfacedPersistent; override;
    function FindBooleanInColumn(ABoolean: Boolean; ACol: Integer): Integer;
    function FindDateTimeInColumn(ADateTime: TDateTime; ACol: Integer): Integer;
    function FindDoubleInColumn(ADouble: Double; ACol: Integer): Integer;
    function FindExtendedInColumn(AExtended: Extended; ACol: Integer): Integer;
    function FindFormulaInColumn(const AFormula: String; ACol: Integer): Integer;
    function FindInt64InColumn(AInt64: Int64; ACol: Integer): Integer;
    function FindIntegerInColumn(AInteger: Integer; ACol: Integer): Integer;
    function FindMemoInColumn(AMemo: TStrings; ACol: Integer): Integer;
    function FindObjectInColumn(AObject: TObject; ACol: Integer): Integer;
    function FindSingleInColumn(ASingle: Single; ACol: Integer): Integer;
    function FindStringInColumn(const AString: String; ACol: Integer): Integer;
    procedure AddCols(ACount: Integer);
    procedure AddColumnNames(AColumnNames: array of string);
    procedure AddFromMatrix(Source: TcaMatrix); virtual;
    procedure AddRows(ACount: Integer);
    procedure AddRowValues(AValues: array of Variant);
    procedure AddSortColumn(const ACol: Integer; const ASortDirection: TcaSortDirection);
    procedure Assign(Source: TPersistent); override;  // VCL compatibilty
    procedure AssignFromObject(Source: TPersistent); override;
    procedure AssignCell(Source: TcaMatrix; const SrcCol, SrcRow, ACol, ARow: Integer);
    procedure Clear; overload;
    procedure Clear(AColCount, ARowCount: Integer); overload;
    procedure ClearCell(const ACol, ARow: Integer);
    procedure ClearCol(const ACol: Integer);
    procedure ClearRow(const ARow: Integer);
    procedure ClearSortColumns;
    procedure CopyCol(const AFromCol, AToCol: Integer);
    procedure CopyRow(const AFromRow, AToRow: Integer);
    procedure CopyToClipboard(ACopyColumnNames: Boolean = True); overload;
    procedure CopyToClipboard(AColFrom, ARowFrom, AColTo, ARowTo: Integer; ACopyColumnNames: Boolean = True); overload;
    procedure DeleteCol(const ACol: Integer);
    procedure DeleteRow(const ARow: Integer);
    procedure InsertCol(const ACol: Integer);
    procedure InsertRow(const ARow: Integer);
    procedure MoveRowDown(const ARow: Integer);
    procedure MoveRowUp(const ARow: Integer);
    // Fill matrix methods 
    procedure Fill(AValue: Boolean); overload;
    procedure Fill(AValue: TDateTime); overload;
    procedure Fill(AValue: Double); overload;
    procedure Fill(AValue: Extended); overload;
    procedure Fill(AValue: String); overload;
    procedure Fill(AValue: Integer); overload;
    procedure Fill(AValue: Int64); overload;
    procedure Fill(AValue: TStrings); overload;
    procedure Fill(AValue: TObject); overload;
    procedure Fill(AValue: Single); overload;
    // Fill matrix column methods 
    procedure FillCol(ACol: Integer; AValue: Boolean); overload;
    procedure FillCol(ACol: Integer; AValue: TDateTime); overload;
    procedure FillCol(ACol: Integer; AValue: Double); overload;
    procedure FillCol(ACol: Integer; AValue: Extended); overload;
    procedure FillCol(ACol: Integer; AValue: String); overload;
    procedure FillCol(ACol: Integer; AValue: Integer); overload;
    procedure FillCol(ACol: Integer; AValue: Int64); overload;
    procedure FillCol(ACol: Integer; AValue: TStrings); overload;
    procedure FillCol(ACol: Integer; AValue: TObject); overload;
    procedure FillCol(ACol: Integer; AValue: Single); overload;
    // Fill matrix row methods 
    procedure FillRow(ARow: Integer; AValue: Boolean); overload;
    procedure FillRow(ARow: Integer; AValue: TDateTime); overload;
    procedure FillRow(ARow: Integer; AValue: Double); overload;
    procedure FillRow(ARow: Integer; AValue: Extended); overload;
    procedure FillRow(ARow: Integer; AValue: String); overload;
    procedure FillRow(ARow: Integer; AValue: Integer); overload;
    procedure FillRow(ARow: Integer; AValue: Int64); overload;
    procedure FillRow(ARow: Integer; AValue: TStrings); overload;
    procedure FillRow(ARow: Integer; AValue: TObject); overload;
    procedure FillRow(ARow: Integer; AValue: Single); overload;
    procedure GetColumnAsStrings(const ACol: Integer; AStrings: TStrings); overload;
    procedure GetColumnAsStrings(const ACol: Integer; AStrings: IcaStringList); overload;
    // Saving and Loading 
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromXML(const AFileName: String);
    procedure SaveToExcel(const AFileName: String);
    procedure SaveToFile(const AFileName: String);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToXML(const AFileName: String);
    // Matrix math 
    procedure AddMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    procedure DivideMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    procedure MultiplyMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    procedure SubtractMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
    // Properties 
    property AutoColCount: Boolean read GetAutoColCount write SetAutoColCount;
    property AutoRowCount: Boolean read GetAutoRowCount write SetAutoRowCount;
    property Cells[ACol, ARow: Integer]: TcaCell read GetCell write SetCell;
    property CellIsNumeric[ACol, ARow: Integer]: Boolean read GetCellIsNumeric;
    property ColCount: Integer read GetColCount write SetColCount;
    property ColumnNames: TStrings read GetColumnNames write SetColumnNames;
    property ColumnTypes[ACol: Integer]: TcaCellType read GetColumnType write SetColumnType;
    property DateFormat: String read GetDateFormat write SetDateFormat;
    property FloatFormat: String read GetFloatFormat write SetFloatFormat;
    property IntegerFormat: String read GetIntegerFormat write SetIntegerFormat;
    property MatrixCopy: TcaMatrix read GetMatrixCopy;
    property Name: String read GetName write SetName;
    property OwnsObject[ACol, ARow: Integer]: Boolean read GetOwnsObject write SetOwnsObject;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property SaveUnsorted: Boolean read GetSaveUnsorted write SetSaveUnsorted;
    property SortColumns: TcaIntegerVector read GetSortColumns;
    property SortDirections: TcaIntegerVector read GetSortDirections;
    property Sorted: Boolean read GetSorted write SetSorted;
    property StringSortCaseInsensitive: Boolean read GetStringSortCaseInsensitive write SetStringSortCaseInsensitive;
    property XML: String read GetXML write SetXML;
    property XMLRowContent: String read GetXMLRowContent;
    // Cell access properties 
    property Booleans[ACol, ARow: Integer]: Boolean read GetBoolean write SetBoolean;
    property DateTimes[ACol, ARow: Integer]: TDateTime read GetDateTime write SetDateTime;
    property Doubles[ACol, ARow: Integer]: Double read GetDouble write SetDouble;
    property Extendeds[ACol, ARow: Integer]: Extended read GetExtended write SetExtended;
    property Formulas[ACol, ARow: Integer]: String read GetFormula write SetFormula;
    property Integers[ACol, ARow: Integer]: Integer read GetInteger write SetInteger;
    property Int64s[ACol, ARow: Integer]: Int64 read GetInt64 write SetInt64;
    property Memos[ACol, ARow: Integer]: TStrings read GetMemo write SetMemo;
    property Objects[ACol, ARow: Integer]: TObject read GetObject write SetObject;
    property Selected[ACol, ARow: Integer]: Boolean read GetSelected write SetSelected;
    property Singles[ACol, ARow: Integer]: Single read GetSingle write SetSingle;
    property Strings[ACol, ARow: Integer]: String read GetString write SetString;
    property Variants[ACol, ARow: Integer]: Variant read GetVariant write SetVariant;
    // Get related events 
    property OnGetBooleanAsString: TcaGetBooleanAsStringEvent read GetOnGetBooleanAsString write SetOnGetBooleanAsString;
    property OnGetDateTimeAsString: TcaGetDateTimeAsStringEvent read GetOnGetDateTimeAsString write SetOnGetDateTimeAsString;
    property OnGetDoubleAsString: TcaGetDoubleAsStringEvent read GetOnGetDoubleAsString write SetOnGetDoubleAsString;
    property OnGetExtendedAsString: TcaGetExtendedAsStringEvent read GetOnGetExtendedAsString write SetOnGetExtendedAsString;
    property OnGetFormulaAsString: TcaGetFormulaAsStringEvent read GetOnGetFormulaAsString write SetOnGetFormulaAsString;
    property OnGetInt64AsString: TcaGetInt64AsStringEvent read GetOnGetInt64AsString write SetOnGetInt64AsString;
    property OnGetIntegerAsString: TcaGetIntegerAsStringEvent read GetOnGetIntegerAsString write SetOnGetIntegerAsString;
    property OnGetMemoAsString: TcaGetMemoAsStringEvent read GetOnGetMemoAsString write SetOnGetMemoAsString;
    property OnGetObjectAsString: TcaGetObjectAsStringEvent read GetOnGetObjectAsString write SetOnGetObjectAsString;
    property OnGetSingleAsString: TcaGetSingleAsStringEvent read GetOnGetSingleAsString write SetOnGetSingleAsString;
    // Set related events 
    property OnSetBoolean: TcaSetBooleanEvent read GetOnSetBoolean write SetOnSetBoolean;
    property OnSetCellValue: TcaSetCellValueEvent read GetOnSetCellValue write SetOnSetCellValue;
    property OnSetDateTime: TcaSetDateTimeEvent read GetOnSetDateTime write SetOnSetDateTime;
    property OnSetDouble: TcaSetDoubleEvent read GetOnSetDouble write SetOnSetDouble;
    property OnSetExtended: TcaSetExtendedEvent read GetOnSetExtended write SetOnSetExtended;
    property OnSetString: TcaSetStringEvent read GetOnSetString write SetOnSetString;
    property OnSetFormula: TcaSetFormulaEvent read GetOnSetFormula write SetOnSetFormula;
    property OnSetInt64: TcaSetInt64Event read GetOnSetInt64 write SetOnSetInt64;
    property OnSetInteger: TcaSetIntegerEvent read GetOnSetInteger write SetOnSetInteger;
    property OnSetMemo: TcaSetMemoEvent read GetOnSetMemo write SetOnSetMemo;
    property OnSetObject: TcaSetObjectEvent read GetOnSetObject write SetOnSetObject;
    property OnSetSingle: TcaSetSingleEvent read GetOnSetSingle write SetOnSetSingle;
  end;

  //---------------------------------------------------------------------------
  // IcaBooleanMatrix                                                          
  //---------------------------------------------------------------------------

  IcaBooleanMatrix = interface(IcaMatrix)
  ['{71D3136E-C690-46E2-950D-007F466369BC}']
    property Booleans[ACol, ARow: Integer]: Boolean read GetBoolean write SetBoolean; default;
  end;

  //---------------------------------------------------------------------------
  // TcaBooleanMatrix                                                          
  //---------------------------------------------------------------------------

  TcaBooleanMatrix = class(TcaMatrix, IcaBooleanMatrix)
  private
    function GetBoolean(ACol, ARow: Integer): Boolean;
    procedure SetBoolean(ACol, ARow: Integer; const Value: Boolean);
  public
    property Booleans[ACol, ARow: Integer]: Boolean read GetBoolean write SetBoolean; default;
  end;

  //---------------------------------------------------------------------------
  // IcaDateTimeMatrix                                                         
  //---------------------------------------------------------------------------

  IcaDateTimeMatrix = interface(IcaMatrix)
  ['{575FA229-78BF-458B-AA90-6371B810B931}']
    property DateTimes[ACol, ARow: Integer]: TDateTime read GetDateTime write SetDateTime; default;
  end;

  //---------------------------------------------------------------------------
  // TcaDateTimeMatrix                                                         
  //---------------------------------------------------------------------------

  TcaDateTimeMatrix = class(TcaMatrix, IcaDateTimeMatrix)
  private
    function GetDateTime(ACol, ARow: Integer): TDateTime;
    procedure SetDateTime(ACol, ARow: Integer; const Value: TDateTime);
  protected
    procedure DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
  public
    property DateTimes[ACol, ARow: Integer]: TDateTime read GetDateTime write SetDateTime; default;
  end;

  //---------------------------------------------------------------------------
  // IcaDoubleMatrix                                                           
  //---------------------------------------------------------------------------

  IcaDoubleMatrix = interface(IcaMatrix)
  ['{4943F0C1-92AB-4523-9BE8-F5E3F98F9D33}']
    function GetColTotal(ACol: Integer): Double;
    function GetRowTotal(ARow: Integer): Double;
    // Properties 
    property ColTotal[ACol: Integer]: Double read GetColTotal;
    property Doubles[ACol, ARow: Integer]: Double read GetDouble write SetDouble; default;
    property RowTotal[ARow: Integer]: Double read GetRowTotal;
  end;

  //---------------------------------------------------------------------------
  // TcaDoubleMatrix                                                           
  //---------------------------------------------------------------------------

  TcaDoubleMatrix = class(TcaMatrix, IcaDoubleMatrix)
  protected
    // Protected property methods 
    function GetColTotal(ACol: Integer): Double;
    function GetDouble(ACol, ARow: Integer): Double;
    function GetRowTotal(ARow: Integer): Double;
    procedure SetDouble(ACol, ARow: Integer; const Value: Double);
    // Virtual protected methods 
    procedure DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
  public
    property ColTotal[ACol: Integer]: Double read GetColTotal;
    property Doubles[ACol, ARow: Integer]: Double read GetDouble write SetDouble; default;
    property RowTotal[ARow: Integer]: Double read GetRowTotal;
  end;

  //---------------------------------------------------------------------------
  // IcaExtendedMatrix                                                         
  //---------------------------------------------------------------------------

  IcaExtendedMatrix = interface(IcaMatrix)
  ['{83C0AE06-DE0D-44C0-B879-11563D9CDAD5}']
    // Property methods 
    function GetColTotal(ACol: Integer): Extended;
    function GetRowTotal(ARow: Integer): Extended;
    // Properties 
    property ColTotal[ACol: Integer]: Extended read GetColTotal;
    property Extendeds[ACol, ARow: Integer]: Extended read GetExtended write SetExtended; default;
    property RowTotal[ARow: Integer]: Extended read GetRowTotal;
  end;

  //---------------------------------------------------------------------------
  // TcaExtendedMatrix                                                         
  //---------------------------------------------------------------------------

  TcaExtendedMatrix = class(TcaMatrix, IcaExtendedMatrix)
  private
    function GetColTotal(ACol: Integer): Extended;
    function GetExtended(ACol, ARow: Integer): Extended;
    function GetRowTotal(ARow: Integer): Extended;
    procedure SetExtended(ACol, ARow: Integer; const Value: Extended);
  protected
    procedure DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;        
    procedure DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
  public
    property ColTotal[ACol: Integer]: Extended read GetColTotal;
    property Extendeds[ACol, ARow: Integer]: Extended read GetExtended write SetExtended; default;
    property RowTotal[ARow: Integer]: Extended read GetRowTotal;
  end;

  //---------------------------------------------------------------------------
  // IcaFormulaMatrix                                                          
  //---------------------------------------------------------------------------

  IcaFormulaMatrix = interface(IcaMatrix)
  ['{81FDEBA7-BA51-4B31-A628-171A531A2F4C}']
    property Formulas[ACol, ARow: Integer]: String read GetFormula write SetFormula; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFormulaMatrix                                                          
  //---------------------------------------------------------------------------

  TcaFormulaMatrix = class(TcaMatrix, IcaFormulaMatrix)
  private
    function GetFormula(ACol, ARow: Integer): String;
    procedure SetFormula(ACol, ARow: Integer; const Value: String);
  public
    property Formulas[ACol, ARow: Integer]: String read GetFormula write SetFormula; default;
  end;

  //---------------------------------------------------------------------------
  // IcaInt64Matrix                                                            
  //---------------------------------------------------------------------------

  IcaInt64Matrix = interface(IcaMatrix)
  ['{1D5438FA-6462-46ED-A0AE-06D57D6C7A78}']
    property Int64s[ACol, ARow: Integer]: Int64 read GetInt64 write SetInt64; default;
  end;

  //---------------------------------------------------------------------------
  // TcaInt64Matrix                                                            
  //---------------------------------------------------------------------------

  TcaInt64Matrix = class(TcaMatrix, IcaInt64Matrix)
  private
    function GetInt64(ACol, ARow: Integer): Int64;
    procedure SetInt64(ACol, ARow: Integer; const Value: Int64);
  protected
    procedure DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
  public
    property Int64s[ACol, ARow: Integer]: Int64 read GetInt64 write SetInt64; default;
  end;

  //---------------------------------------------------------------------------
  // IcaIntegerMatrix                                                          
  //---------------------------------------------------------------------------

  IcaIntegerMatrix = interface(IcaMatrix)
  ['{CAA73CFB-2D3C-4C04-AC0E-DB9A133B5324}']
    property Integers[ACol, ARow: Integer]: Integer read GetInteger write SetInteger; default;
  end;

  //---------------------------------------------------------------------------
  // TcaIntegerMatrix                                                          
  //---------------------------------------------------------------------------

  TcaIntegerMatrix = class(TcaMatrix, IcaIntegerMatrix)
  private
    function GetInteger(ACol, ARow: Integer): Integer;
    procedure SetInteger(ACol, ARow: Integer; const Value: Integer);
  protected
    procedure DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;        
    procedure DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
  public
    property Integers[ACol, ARow: Integer]: Integer read GetInteger write SetInteger; default;
  end;

  //---------------------------------------------------------------------------
  // IcaMemoMatrix                                                             
  //---------------------------------------------------------------------------

  IcaMemoMatrix = interface(IcaMatrix)
  ['{C333C14F-A5C6-4892-9A58-409F7BB922AB}']
    property Memos[ACol, ARow: Integer]: TStrings read GetMemo write SetMemo; default;
  end;

  //---------------------------------------------------------------------------
  // TcaMemoMatrix                                                             
  //---------------------------------------------------------------------------

  TcaMemoMatrix = class(TcaMatrix, IcaMemoMatrix)
  private
    function GetMemo(ACol, ARow: Integer): TStrings;
    procedure SetMemo(ACol, ARow: Integer; const Value: TStrings);
  public
    property Memos[ACol, ARow: Integer]: TStrings read GetMemo write SetMemo; default;
  end;

  //---------------------------------------------------------------------------
  // IcaObjectMatrix                                                           
  //---------------------------------------------------------------------------

  IcaObjectMatrix = interface(IcaMatrix)
  ['{43E055D0-FE64-4F04-9C8B-49B039CEC6D3}']
    property Objects[ACol, ARow: Integer]: TObject read GetObject write SetObject; default;
  end;

  //---------------------------------------------------------------------------
  // TcaObjectMatrix                                                           
  //---------------------------------------------------------------------------

  TcaObjectMatrix = class(TcaMatrix, IcaObjectMatrix)
  private
    function GetObject(ACol, ARow: Integer): TObject;
    procedure SetObject(ACol, ARow: Integer; const Value: TObject);
  public
    property Objects[ACol, ARow: Integer]: TObject read GetObject write SetObject; default;
  end;

  //---------------------------------------------------------------------------
  // IcaSelectedMatrix                                                         
  //---------------------------------------------------------------------------

  IcaSelectedMatrix = interface(IcaMatrix)
  ['{85831C34-4B02-4BF1-A931-C94A1FC282C9}']
    property Selected[ACol, ARow: Integer]: Boolean read GetSelected write SetSelected; default;
  end;

  //---------------------------------------------------------------------------
  // TcaSelectedMatrix                                                         
  //---------------------------------------------------------------------------

  TcaSelectedMatrix = class(TcaMatrix, IcaSelectedMatrix)
  private
    function GetSelected(ACol, ARow: Integer): Boolean;
    procedure SetSelected(ACol, ARow: Integer; const Value: Boolean);
  public
    property Selecteds[ACol, ARow: Integer]: Boolean read GetSelected write SetSelected; default;
  end;

  //---------------------------------------------------------------------------
  // IcaSingleMatrix                                                           
  //---------------------------------------------------------------------------

  IcaSingleMatrix = interface(IcaMatrix)
  ['{C83147F3-9B1B-4A01-8D64-C01CF76E1CDF}']
    property Singles[ACol, ARow: Integer]: Single read GetSingle write SetSingle; default;
  end;

  //---------------------------------------------------------------------------
  // TcaSingleMatrix                                                           
  //---------------------------------------------------------------------------

  TcaSingleMatrix = class(TcaMatrix, IcaSingleMatrix)
  private
    function GetSingle(ACol, ARow: Integer): Single;
    procedure SetSingle(ACol, ARow: Integer; const Value: Single);
  protected
    procedure DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
    procedure DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;        
    procedure DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer); override;
  public
    property Singles[ACol, ARow: Integer]: Single read GetSingle write SetSingle; default;
  end;

  //---------------------------------------------------------------------------
  // IcaStringMatrix                                                           
  //---------------------------------------------------------------------------

  IcaStringMatrix = interface(IcaMatrix)
  ['{1A88A3A3-E5DE-40A6-A2FA-27EE0FADE06A}']
    property Strings[ACol, ARow: Integer]: String read GetString write SetString; default;
  end;

  //---------------------------------------------------------------------------
  // TcaStringMatrix                                                           
  //---------------------------------------------------------------------------

  TcaStringMatrix = class(TcaMatrix, IcaStringMatrix)
  private
    function GetString(ACol, ARow: Integer): String;
    procedure SetString(ACol, ARow: Integer; const Value: String);
  public
    property Strings[ACol, ARow: Integer]: String read GetString write SetString; default;
  end;

  //---------------------------------------------------------------------------
  // TcaMatrixList                                                             
  //---------------------------------------------------------------------------

  TcaMatrixList = class(TObject)
  private
    // Private fields 
    FList: TObjectList;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaMatrix;
  public
    // Create/Destroy 
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // Public methods 
    function Add: TcaMatrix;
    function IndexOf(AItem: TcaMatrix): Integer;
    procedure Clear;
    // Properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaMatrix read GetItem; default;
  end;

const
  caIntegerCellTypes: TcaCellTypes = [ctInteger, ctInt64];
  caNumberCellTypes: TcaCellTypes = [ctInteger, ctInt64, ctSingle, ctDouble, ctExtended, ctDateTime];
  caStringCellTypes: TcaCellTypes = [ctString, ctMemo, ctFormula];

implementation

  //---------------------------------------------------------------------------
  // TcaCellConverter                                                          
  //---------------------------------------------------------------------------

constructor TcaCellConverter.Create(AMatrix: TcaMatrix);
begin
  inherited Create;
  FMatrix := AMatrix;
  FEmptyMemo := TStringList.Create;
end;

destructor TcaCellConverter.Destroy;
begin
  FEmptyMemo.Free;
  inherited;
end;

function TcaCellConverter.GetAsObject: TObject;
var
  Cell: TcaCell;
begin
  Result := nil;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := TcaObjectCell(Cell).Value;
      ctInteger:    Result := nil;
      ctInt64:      Result := nil;
      ctSingle:     Result := nil;
      ctDouble:     Result := nil;
      ctExtended:   Result := nil;
      ctString:     Result := nil;
      ctMemo:       Result := TcaMemoCell(Cell).Value;
      ctBoolean:    Result := nil;
      ctDateTime:   Result := nil;
      ctFormula:    Result := nil;
    end;
end;

function TcaCellConverter.GetAsInteger: Integer;
var
  Cell: TcaCell;
  MathUtils: IcaMathUtils;
begin
  MathUtils := Utils as IcaMathUtils;
  Result := 0;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := Integer(TcaObjectCell(Cell).Value);
      ctInteger:    Result := TcaIntegerCell(Cell).Value;
      ctInt64:      Result := TcaInt64Cell(Cell).Value;
      ctSingle:     Result := MathUtils.EquiRound(TcaSingleCell(Cell).Value);
      ctDouble:     Result := MathUtils.EquiRound(TcaDoubleCell(Cell).Value);
      ctExtended:   Result := MathUtils.EquiRound(TcaExtendedCell(Cell).Value);
      ctString:     Result := Utils.Str2Int(TcaStringCell(Cell).Value, 0);
      ctMemo:       Result := TcaMemoCell(Cell).Value.Count;
      ctBoolean:    Result := Ord(TcaBooleanCell(Cell).Value);
      ctDateTime:   Result := MathUtils.Trunc(TcaDateTimeCell(Cell).Value);
      ctFormula:    Result := GetFormulaAsInteger(FMatrix.GetFormulaAsString(FCol, FRow, Cell));
    end;
end;

function TcaCellConverter.GetAsInt64: Int64;
var
  Cell: TcaCell;
  MathUtils: IcaMathUtils;
begin
  MathUtils := Utils as IcaMathUtils;
  Result := 0;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := Integer(TcaObjectCell(Cell).Value);
      ctInteger:    Result := TcaIntegerCell(Cell).Value;
      ctInt64:      Result := TcaInt64Cell(Cell).Value;
      ctSingle:     Result := MathUtils.EquiRound(TcaSingleCell(Cell).Value);
      ctDouble:     Result := MathUtils.EquiRound(TcaDoubleCell(Cell).Value);
      ctExtended:   Result := MathUtils.EquiRound(TcaExtendedCell(Cell).Value);
      ctString:     Result := Utils.Str2Int(TcaStringCell(Cell).Value, 0);
      ctMemo:       Result := TcaMemoCell(Cell).Value.Count;
      ctBoolean:    Result := Ord(TcaBooleanCell(Cell).Value);
      ctDateTime:   Result := MathUtils.Trunc(TcaDateTimeCell(Cell).Value);
      ctFormula:    Result := GetFormulaAsInt64(FMatrix.GetFormulaAsString(FCol, FRow, Cell));
    end;
end;

function TcaCellConverter.GetAsSingle: Single;
var
  Cell: TcaCell;
begin
  Result := 0;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := 0;
      ctInteger:    Result := TcaIntegerCell(Cell).Value;
      ctInt64:      Result := TcaInt64Cell(Cell).Value;
      ctSingle:     Result := TcaSingleCell(Cell).Value;
      ctDouble:     Result := TcaDoubleCell(Cell).Value;
      ctExtended:   Result := TcaExtendedCell(Cell).Value;
      ctString:     Result := Utils.Str2Float(TcaStringCell(Cell).Value, 0);
      ctMemo:       Result := TcaMemoCell(Cell).Value.Count;
      ctBoolean:    Result := Ord(TcaBooleanCell(Cell).Value);
      ctDateTime:   Result := TcaDateTimeCell(Cell).Value;
      ctFormula:    Result := GetFormulaAsSingle(FMatrix.GetFormulaAsString(FCol, FRow, Cell));
    end;
end;

function TcaCellConverter.GetAsDouble: Double;
var
  Cell: TcaCell;
begin
  Result := 0;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := 0;
      ctInteger:    Result := TcaIntegerCell(Cell).Value;
      ctInt64:      Result := TcaInt64Cell(Cell).Value;
      ctSingle:     Result := TcaSingleCell(Cell).Value;
      ctDouble:     Result := TcaDoubleCell(Cell).Value;
      ctExtended:   Result := TcaExtendedCell(Cell).Value;
      ctString:     Result := Utils.Str2Float(TcaStringCell(Cell).Value, 0);
      ctMemo:       Result := TcaMemoCell(Cell).Value.Count;
      ctBoolean:    Result := Ord(TcaBooleanCell(Cell).Value);
      ctDateTime:   Result := TcaDateTimeCell(Cell).Value;
      ctFormula:    Result := GetFormulaAsDouble(FMatrix.GetFormulaAsString(FCol, FRow, Cell));
    end;
end;

function TcaCellConverter.GetAsExtended: Extended;
var
  Cell: TcaCell;
begin
  Result := 0;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := 0;
      ctInteger:    Result := TcaIntegerCell(Cell).Value;
      ctInt64:      Result := TcaInt64Cell(Cell).Value;
      ctSingle:     Result := TcaSingleCell(Cell).Value;
      ctDouble:     Result := TcaDoubleCell(Cell).Value;
      ctExtended:   Result := TcaExtendedCell(Cell).Value;
      ctString:     Result := Utils.Str2Float(TcaStringCell(Cell).Value, 0);
      ctMemo:       Result := TcaMemoCell(Cell).Value.Count;
      ctBoolean:    Result := Ord(TcaBooleanCell(Cell).Value);
      ctDateTime:   Result := TcaDateTimeCell(Cell).Value;
      ctFormula:    Result := GetFormulaAsExtended(FMatrix.GetFormulaAsString(FCol, FRow, Cell));
    end;
end;

function TcaCellConverter.GetAsString: String;
var
  Cell: TcaCell;
begin
  Result := '';
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := FMatrix.GetObjectAsString(FCol, FRow, Cell);
      ctInteger:    Result := FMatrix.GetIntegerAsString(FCol, FRow, Cell);
      ctInt64:      Result := FMatrix.GetInt64AsString(FCol, FRow, Cell);
      ctSingle:     Result := FMatrix.GetSingleAsString(FCol, FRow, Cell);
      ctDouble:     Result := FMatrix.GetDoubleAsString(FCol, FRow, Cell);
      ctExtended:   Result := FMatrix.GetExtendedAsString(FCol, FRow, Cell);
      ctString:     Result := TcaStringCell(Cell).Value;
      ctMemo:       Result := FMatrix.GetMemoAsString(FCol, FRow, Cell);
      ctBoolean:    Result := FMatrix.GetBooleanAsString(FCol, FRow, Cell);
      ctDateTime:   Result := FMatrix.GetDateTimeAsString(FCol, FRow, Cell);
      ctFormula:    Result := FMatrix.GetFormulaAsString(FCol, FRow, Cell);
    end;
end;

function TcaCellConverter.GetAsMemo: TStrings;
var
  ObjectCell: TcaObjectCell;
  Cell: TcaCell;
begin
  Result := FEmptyMemo;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell = nil then
    begin
      Cell := TcaMemoCell.Create;
      AddNewCell(Cell, False, False);
    end;
  case Cell.CellType of
    ctObject:
      begin
        ObjectCell := TcaObjectCell(Cell);
        if ObjectCell.Value is TStrings then
          Result := TStrings(ObjectCell.Value)
        else
          Result := FEmptyMemo;
      end;
    ctInteger:    Result := FEmptyMemo;
    ctInt64:      Result := FEmptyMemo;
    ctSingle:     Result := FEmptyMemo;
    ctDouble:     Result := FEmptyMemo;
    ctExtended:   Result := FEmptyMemo;
    ctString:     Result := TcaStringCell(Cell).AsMemo;
    ctMemo:       Result := TcaMemoCell(Cell).Value;
    ctBoolean:    Result := FEmptyMemo;
    ctDateTime:   Result := FEmptyMemo;
    ctFormula:    Result := TcaStringCell(Cell).AsMemo;
  end;
end;

function TcaCellConverter.GetAsBoolean: Boolean;
var
  Cell: TcaCell;
  MathUtils: IcaMathUtils;
begin
  MathUtils := Utils as IcaMathUtils;
  Result := False;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := TcaObjectCell(Cell).Value <> nil;
      ctInteger:    Result := Boolean(TcaIntegerCell(Cell).Value);
      ctInt64:      Result := Boolean(TcaInt64Cell(Cell).Value);
      ctSingle:     Result := MathUtils.IsNonZero(TcaSingleCell(Cell).Value);
      ctDouble:     Result := MathUtils.IsNonZero(TcaDoubleCell(Cell).Value);
      ctExtended:   Result := MathUtils.IsNonZero(TcaExtendedCell(Cell).Value);
      ctString:     Result := Utils.StringToBoolean(TcaStringCell(Cell).Value);
      ctMemo:       Result := TcaMemoCell(Cell).Value.Count > 0;
      ctBoolean:    Result := TcaBooleanCell(Cell).Value;
      ctDateTime:   Result := TcaDateTimeCell(Cell).Value <> 0;
      ctFormula:    Result := TcaFormulaCell(Cell).Value <> '';
    end;
end;

function TcaCellConverter.GetAsDateTime: TDateTime;
var
  Cell: TcaCell;
begin
  Result := 0;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := 0;
      ctInteger:    Result := Utils.IntegerToDateTime(TcaIntegerCell(Cell).Value);
      ctInt64:      Result := Utils.IntegerToDateTime(TcaInt64Cell(Cell).Value);
      ctSingle:     Result := Utils.ExtendedToDateTime(TcaSingleCell(Cell).Value);
      ctDouble:     Result := Utils.ExtendedToDateTime(TcaDoubleCell(Cell).Value);
      ctExtended:   Result := Utils.ExtendedToDateTime(TcaExtendedCell(Cell).Value);
      ctString:     Result := StrToDateTime(TcaStringCell(Cell).Value);
      ctMemo:       Result := 0;
      ctBoolean:    Result := 0;
      ctDateTime:   Result := TcaDateTimeCell(Cell).Value;
      ctFormula:    Result := 0;
    end;
end;

function TcaCellConverter.GetAsFormula: String;
var
  Cell: TcaCell;
begin
  Result := '<Unknown>';
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := '<Object>';
      ctInteger:    Result := '<Integer>';
      ctInt64:      Result := '<Int64>';
      ctSingle:     Result := '<Single>';
      ctDouble:     Result := '<Double>';
      ctExtended:   Result := '<Extended>';
      ctString:     Result := '<String>';
      ctMemo:       Result := '<Memo>';
      ctBoolean:    Result := '<Boolean>';
      ctDateTime:   Result := '<DateTime>';
      ctFormula:    Result := TcaFormulaCell(Cell).Value;
    end;
end;

procedure TcaCellConverter.ReleaseOldCell(var WasSelected: Boolean);
var
  Cell: TcaCell;
begin
  Cell := FMatrix.Cells[FCol, FRow];
  WasSelected := False;
  if Cell <> nil then
    begin
      WasSelected := Cell.Selected;
      FMatrix.ReleaseCell(FCol, FRow);
    end;
end;

procedure TcaCellConverter.AddNewCell(const ACell: TcaCell; WasSelected: Boolean; IsSetting: Boolean);
begin
  ACell.Selected := WasSelected;
  FMatrix.Cells[FCol, FRow] := ACell;
  if IsSetting then
    FMatrix.DoSetCellValue(FCol, FRow, ACell);
end;

function TcaCellConverter.GetFormulaAsDouble(const AFormula: String): Double;
begin
  Result := 0;
end;

function TcaCellConverter.GetFormulaAsSingle(const AFormula: String): Single;
begin
  Result := 0;
end;

function TcaCellConverter.GetFormulaAsInteger(const AFormula: String): Integer;
begin
  Result := 0;
end;

function TcaCellConverter.GetFormulaAsInt64(const AFormula: String): Int64;
begin
  Result := 0;
end;

function TcaCellConverter.GetFormulaAsExtended(const AFormula: String): Extended;
begin
  Result := 0;
end;

function TcaCellConverter.GetAsVariant: Variant;
var
  Cell: TcaCell;
begin
  Result := Null;
  Cell := FMatrix.Cells[FCol, FRow];
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     Result := FMatrix.GetObjectAsString(FCol, FRow, Cell);
      ctInteger:    Result := TcaIntegerCell(Cell).Value;
      ctInt64:      Result := TcaInt64Cell(Cell).Value;
      ctSingle:     Result := TcaSingleCell(Cell).Value;
      ctDouble:     Result := TcaDoubleCell(Cell).Value;
      ctExtended:   Result := TcaExtendedCell(Cell).Value;
      ctString:     Result := TcaStringCell(Cell).Value;
      ctMemo:       Result := TcaMemoCell(Cell).AsString;
      ctBoolean:    Result := TcaBooleanCell(Cell).Value;
      ctDateTime:   Result := TcaDateTimeCell(Cell).Value;
      ctFormula:    Result := TcaFormulaCell(Cell).Value;
    end;
end;

procedure TcaCellConverter.SetAsObject(const Value: TObject);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateObjectCell;
  TcaObjectCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsInteger(const Value: Integer);
var
  NewCell: TcaIntegerCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateIntegerCell;
  NewCell.Value := Value;
  AddNewCell(NewCell as TcaCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsInt64(const Value: Int64);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateInt64Cell;
  TcaInt64Cell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsSingle(const Value: Single);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateSingleCell;
  TcaSingleCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsDouble(const Value: Double);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateDoubleCell;
  TcaDoubleCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsExtended(const Value: Extended);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateExtendedCell;
  TcaExtendedCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsString(const Value: String);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateStringCell;
  TcaStringCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsMemo(const Value: TStrings);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateMemoCell;
  TcaMemoCell(NewCell).Value.Assign(Value);
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsBoolean(const Value: Boolean);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateBooleanCell;
  TcaBooleanCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsDateTime(const Value: TDateTime);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateDateTimeCell;
  TcaDateTimeCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsFormula(const Value: String);
var
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  NewCell := FMatrix.CreateFormulaCell;
  TcaFormulaCell(NewCell).Value := Value;
  AddNewCell(NewCell, WasSelected, True);
end;

procedure TcaCellConverter.SetAsVariant(const Value: Variant);
var
  CellType: TcaCellType;
  NewCell: TcaCell;
  WasSelected: Boolean;
begin
  ReleaseOldCell(WasSelected);
  CellType := TcaCell.VariantToCellType(Value);
  NewCell := FMatrix.CreateCell(CellType);
  case CellType of
    ctNil:        Pass;
    ctObject:     Pass;
    ctInteger:    TcaIntegerCell(NewCell).Value := Value;
    ctInt64:      TcaInt64Cell(NewCell).Value := Value;
    ctSingle:     TcaSingleCell(NewCell).Value := Value;
    ctDouble:     TcaDoubleCell(NewCell).Value := Value;
    ctExtended:   TcaExtendedCell(NewCell).Value := Value;
    ctString:     TcaStringCell(NewCell).Value := Value;
    ctMemo:       TcaMemoCell(NewCell).AsString := Value;
    ctBoolean:    TcaBooleanCell(NewCell).Value := Value;
    ctDateTime:   TcaDateTimeCell(NewCell).Value := Value;
    ctFormula:    TcaFormulaCell(NewCell).Value := Value;
  end;
  AddNewCell(NewCell, WasSelected, True);
end;

  //----------------------------------------------------------------------------
  // TcaMatrixHeader                                                            
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaMatrixHeader.Create(const AMatrixRef: IInterface);
begin
  inherited Create;
  FMatrix := TcaMatrix(Utils.GetImplementingObject(AMatrixRef));
  FColumnTypes := TcaByteString.Create;
  FMagic := cMatrixMagic;
end;

destructor TcaMatrixHeader.Destroy;
begin
  FColumnTypes.Free;
  inherited;
end;

  // Public methods 

function TcaMatrixHeader.IsValid: Boolean;
begin
  Result := FMagic = cMatrixMagic;
end;

procedure TcaMatrixHeader.Read(Stream: TStream);
begin
  ReadMagic(Stream);
  if IsValid then
    begin
      ReadColumnNames(Stream);
      ReadDimensions(Stream);
      UpdateMatrix;
    end;
end;

procedure TcaMatrixHeader.Write(Stream: TStream);
begin
  UpdateFromMatrix;
  WriteMagic(Stream);
  WriteColumnNames(Stream);
  WriteDimensions(Stream);
end;

procedure TcaMatrixHeader.UpdateFromMatrix;
var
  ACol: Integer;
begin
  FColCount := FMatrix.ColCount;
  FRowCount := FMatrix.RowCount;
  FColumnNames := FMatrix.ColumnNames.Text;
  for ACol := 0 to Pred(FMatrix.ColCount) do
    FColumnTypes.Bytes[ACol] := Byte(FMatrix.ColumnTypes[ACol]);
end;

procedure TcaMatrixHeader.UpdateMatrix;
var
  ACol: Integer;
begin
  FMatrix.ColCount := FColCount;
  FMatrix.RowCount := FRowCount;
  FMatrix.ColumnNames.Text := FColumnNames;
  for ACol := 0 to Pred(FMatrix.ColCount) do
    FMatrix.ColumnTypes[ACol] := TcaCellType(FColumnTypes.Bytes[ACol]);
end;

  // Private methods 

procedure TcaMatrixHeader.ReadColumnNames(Stream: TStream);
begin
  ReadString(Stream, FColumnNames);
end;

procedure TcaMatrixHeader.ReadDimensions(Stream: TStream);
begin
  Stream.ReadBuffer(FColCount, SizeOf(FColCount));
  Stream.ReadBuffer(FRowCount, SizeOf(FRowCount));
end;

procedure TcaMatrixHeader.ReadMagic(Stream: TStream);
begin
  ReadString(Stream, FMagic);
end;

procedure TcaMatrixHeader.ReadString(Stream: TStream; var AString: string);
var
  Size: Integer;
  Buffer: PChar;
begin
  Stream.ReadBuffer(Size, SizeOf(Size));
  GetMem(Buffer, Size);
  try
    Stream.ReadBuffer(Buffer^, Size);
    System.SetString(AString, Buffer, Size);
  finally
    FreeMem(Buffer, Size);
  end;
end;

procedure TcaMatrixHeader.WriteColumnNames(Stream: TStream);
begin
  WriteString(Stream, FColumnNames);
end;

procedure TcaMatrixHeader.WriteDimensions(Stream: TStream);
begin
  Stream.WriteBuffer(FColCount, SizeOf(FColCount));
  Stream.WriteBuffer(FRowCount, SizeOf(FRowCount));
end;

procedure TcaMatrixHeader.WriteMagic(Stream: TStream);
begin
  WriteString(Stream, FMagic);
end;

procedure TcaMatrixHeader.WriteString(Stream: TStream; const AString: string);
var
  StrStream: TStringStream;
  StrSize: Integer;
begin
  StrStream := Auto(TStringStream.Create(AString)).Instance;
  StrSize := StrStream.Size;
  Stream.WriteBuffer(StrSize, SizeOf(StrSize));
  Stream.CopyFrom(StrStream, StrSize);
end;

  //---------------------------------------------------------------------------
  // TcaMatrix                                                                 
  //---------------------------------------------------------------------------

constructor TcaMatrix.Create;
begin
  inherited;
  FColumnNames := TStringList.Create;
  FColumnTypes := TcaByteString.Create;
  FConverter := TcaCellConverter.Create(Self);
  FCells := TcaSparseMatrix.Create;
  ResetSwapRow;
end;

constructor TcaMatrix.Create(AAutoColCount, AAutoRowCount: Boolean);
begin
  Create;
  FAutoColCount := AAutoColCount;
  FAutoRowCount := AAutoRowCount;
end;

constructor TcaMatrix.Create(AColCount, ARowCount: Integer);
begin
  Create;
  SetColCount(AColCount);
  SetRowCount(ARowCount);
end;

constructor TcaMatrix.Create(AXML: string);
begin
  Create;
  SetXML(AXML);
end;

destructor TcaMatrix.Destroy;
begin
  ReleaseCells;
  FMatrixCopy.Free;
  FColumnNames.Free;
  FColumnTypes.Free;
  FSortColumns.Free;
  FSortDirections.Free;
  FConverter.Free;
  inherited;
end;

  // IcaLoggable 

procedure TcaMatrix.SendToLog(const AMsg: String; AClearLog: Boolean = False);
begin
  if AClearLog then Log.Clear;
  if AMsg <> '' then Log.Send(AMsg);
  Log.Send(AsString);
end;

  // Public methods 

function TcaMatrix.AddCol: Integer;
begin
  SetColCount(GetColCount + 1);
  Result := GetColCount - 1;
end;

function TcaMatrix.AddRow: Integer;
begin
  SetRowCount(GetRowCount + 1);
  Result := GetRowCount - 1;
end;

function TcaMatrix.AsString(ACopyColumnNames: Boolean = True): string;
begin
  Result := AsString(0, 0, GetColCount - 1, GetRowCount - 1, ACopyColumnNames);
end;

function TcaMatrix.AsString(AColFrom, ARowFrom, AColTo, ARowTo: Integer; ACopyColumnNames: Boolean = True): string;
var
  ACol: Integer;
  AllDefaultNames: Boolean;
  ARow: Integer;
  ColumnName: String;
  CopyList: TStringList;
  DefColumnName: String;
  RowStr: String;
begin
  CopyList := Auto(TStringList.Create).Instance;
  if ACopyColumnNames and (FColumnNames.Count = ColCount) then
    begin
      RowStr := '';
      AllDefaultNames := True;
      for ACol := AColFrom to AColTo do
        begin
          ColumnName := FColumnNames[ACol];
          DefColumnName := Format('Column%d', [ACol]);
          RowStr := RowStr + ColumnName + #9;
          if ColumnName <> DefColumnName then
            AllDefaultNames := False;
        end;
      if not AllDefaultNames then CopyList.Add(RowStr);
    end;
  for ARow := ARowFrom to ARowTo do
    begin
      RowStr := '';
      for ACol := AColFrom to AColTo do
        RowStr := RowStr + GetString(ACol, ARow) + #9;
      SetLength(RowStr, Length(RowStr) - 1);
      CopyList.Add(RowStr);
    end;
  Result := CopyList.Text;
end;

function TcaMatrix.CloneAsObject: TcaInterfacedPersistent;
begin
  Result := TcaMatrix.Create;
  TcaMatrix(Result).AssignFromObject(Self);
end;

function TcaMatrix.FindBooleanInColumn(ABoolean: Boolean; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetBoolean(ACol, ARow) = ABoolean then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindDateTimeInColumn(ADateTime: TDateTime; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetDateTime(ACol, ARow) = ADateTime then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindDoubleInColumn(ADouble: Double; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetDouble(ACol, ARow) = ADouble then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindExtendedInColumn(AExtended: Extended; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetExtended(ACol, ARow) = AExtended then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindFormulaInColumn(const AFormula: String; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetFormula(ACol, ARow) = AFormula then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindInt64InColumn(AInt64: Int64; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetInt64(ACol, ARow) = AInt64 then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindIntegerInColumn(AInteger, ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetInteger(ACol, ARow) = AInteger then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindMemoInColumn(AMemo: TStrings; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetMemo(ACol, ARow).Equals(AMemo) then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindObjectInColumn(AObject: TObject; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetObject(ACol, ARow) = AObject then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindSingleInColumn(ASingle: Single; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetSingle(ACol, ARow) = ASingle then
      begin
        Result := ARow;
        Break;
      end;
end;

function TcaMatrix.FindStringInColumn(const AString: String; ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := -1;
  for ARow := 0 to Pred(GetRowCount) do
    if GetString(ACol, ARow) = AString then
      begin
        Result := ARow;
        Break;
      end;
end;

procedure TcaMatrix.AddCols(ACount: Integer);
var
  Index: Integer;
begin
  for Index := 1 to ACount do AddCol;
end;

procedure TcaMatrix.AddColumnNames(AColumnNames: array of string);
var
  Index: Integer;
  Offset: Integer;
begin
  SetColCount(GetColCount + Length(AColumnNames));
  Offset := Low(AColumnNames);
  for Index := Low(AColumnNames) to High(AColumnNames) do
    FColumnNames[Index - Offset] := AColumnNames[Index];
end;

procedure TcaMatrix.AddFromMatrix(Source: TcaMatrix);
var
  ACol, ARow, ColLimit, OldRowCount: Integer;
begin
  ColLimit := Min(ColCount, Source.ColCount);
  OldRowCount := RowCount;
  RowCount := RowCount + Source.RowCount;
  for ACol := 0 to ColLimit - 1 do
    for ARow := 0 to Pred(Source.RowCount) do
      AssignCell(Source, ACol, ARow, ACol, OldRowCount + ARow);
end;

procedure TcaMatrix.AddRows(ACount: Integer);
var
  Index: Integer;
begin
  for Index := 1 to ACount do AddRow;
end;

procedure TcaMatrix.AddRowValues(AValues: array of Variant);
var
  CellType: TcaCellType;
  ACol: Integer;
  NewRow: Integer;
  Value: Variant;
begin
  while GetColCount < Length(AValues) do
    AddCol;
  NewRow := AddRow;
  for ACol := Low(AValues) to High(AValues) do
    begin
      Value := AValues[ACol];
      CellType := TcaCell.VariantToCellType(Value);
      case CellType of
        ctNil:        Pass;
        ctInteger:    SetInteger(ACol, NewRow, Value);
        ctInt64:      SetInt64(ACol, NewRow, Value);
        ctSingle:     SetSingle(ACol, NewRow, Value);
        ctDouble:     SetDouble(ACol, NewRow, Value);
        ctString:     SetString(ACol, NewRow, Value);
        ctBoolean:    SetBoolean(ACol, NewRow, Value);
        ctDateTime:   SetDateTime(ACol, NewRow, Value);
      end;
    end;
end;

procedure TcaMatrix.AddSortColumn(const ACol: Integer; const ASortDirection: TcaSortDirection);
begin
  if ASortDirection <> sdNone then
    begin
      if FSortColumns = nil then FSortColumns := TcaIntegerVector.Create;
      if FSortDirections = nil then FSortDirections := TcaIntegerVector.Create;
      if FSortColumns.IndexOf(ACol) = -1 then
        begin
          FSortColumns.Add(ACol);
          FSortDirections.Add(Ord(ASortDirection));
        end;
    end;
end;

procedure TcaMatrix.Assign(Source: TPersistent);
begin
  AssignFromObject(Source);
end;

procedure TcaMatrix.AssignFromObject(Source: TPersistent);
var
  ACol: Integer;
  ARow: Integer;
  SourceMatrix: TcaMatrix;
begin
  if Source is TcaMatrix then
    begin
      SourceMatrix := TcaMatrix(Source);
      SynchronizeDimensions(SourceMatrix);
      for ACol := 0 to Pred(GetColCount) do
        for ARow := 0 to Pred(GetRowCount) do
          AssignCell(SourceMatrix, ACol, ARow, ACol, ARow);
    end
  else
    inherited AssignFromObject(Source);
end;

procedure TcaMatrix.AssignCell(Source: TcaMatrix; const SrcCol, SrcRow, ACol, ARow: Integer);
var
  SourceCell: TcaCell;
begin
  SourceCell := Source.Cells[SrcCol, SrcRow];
  if SourceCell <> nil then
    begin
      case
        SourceCell.CellType of
          ctObject:   SetObject(ACol, ARow, Source.Objects[SrcCol, SrcRow]);
          ctInteger:  SetInteger(ACol, ARow, Source.Integers[SrcCol, SrcRow]);
          ctInt64:    SetInt64(ACol, ARow, Source.Int64s[SrcCol, SrcRow]);
          ctSingle:   SetSingle(ACol, ARow, Source.Singles[SrcCol, SrcRow]);
          ctDouble:   SetDouble(ACol, ARow, Source.Doubles[SrcCol, SrcRow]);
          ctExtended: SetExtended(ACol, ARow, Source.Extendeds[SrcCol, SrcRow]);
          ctString:   SetString(ACol, ARow, Source.Strings[SrcCol, SrcRow]);
          ctMemo:     SetMemo(ACol, ARow, Source.Memos[SrcCol, SrcRow]);
          ctBoolean:  SetBoolean(ACol, ARow, Source.Booleans[SrcCol, SrcRow]);
          ctDateTime: SetDateTime(ACol, ARow, Source.DateTimes[SrcCol, SrcRow]);
          ctFormula:  SetFormula(ACol, ARow, Source.Formulas[SrcCol, SrcRow]);
        end;
      SetSelected(ACol, ARow, Source.Selected[SrcCol, SrcRow]);
    end
  else
    ReleaseCell(ACol, ARow);
end;

procedure TcaMatrix.Clear;
begin
  SetColCount(0);
  SetRowCount(0);
end;

procedure TcaMatrix.Clear(AColCount, ARowCount: Integer);
begin
  Clear;
  SetColCount(AColCount);
  SetRowCount(ARowCount);
end;

procedure TcaMatrix.ClearCell(const ACol, ARow: Integer);
var
  Cell: TcaCell;
begin
  Cell := GetCell(ACol, ARow);
  if Cell <> nil then
    case Cell.CellType of
      ctObject:     SetObject(ACol, ARow, nil);
      ctInteger:    SetInteger(ACol, ARow, 0);
      ctInt64:      SetInt64(ACol, ARow, 0);
      ctSingle:     SetSingle(ACol, ARow, 0);
      ctDouble:     SetDouble(ACol, ARow, 0);
      ctExtended:   SetExtended(ACol, ARow, 0);
      ctString:     SetString(ACol, ARow, '');
      ctMemo:       GetMemo(ACol, ARow).Clear;
      ctBoolean:    SetBoolean(ACol, ARow, False);
      ctDateTime:   SetDateTime(ACol, ARow, 0);
      ctFormula:    SetFormula(ACol, ARow, '');
    end;
end;

procedure TcaMatrix.ClearCol(const ACol: Integer);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    ClearCell(ACol, ARow);
end;

procedure TcaMatrix.ClearRow(const ARow: Integer);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    ClearCell(ACol, ARow);
end;

procedure TcaMatrix.ClearSortColumns;
begin
  if FSortColumns = nil then FSortColumns := TcaIntegerVector.Create;
  if FSortDirections = nil then FSortDirections := TcaIntegerVector.Create;
  FSortColumns.Clear;
  FSortDirections.Clear;
end;

procedure TcaMatrix.CopyCol(const AFromCol, AToCol: Integer);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    AssignCell(Self, AFromCol, ARow, AToCol, ARow);
end;

procedure TcaMatrix.CopyRow(const AFromRow, AToRow: Integer);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    AssignCell(Self, ACol, AFromRow, ACol, AToRow);
end;

procedure TcaMatrix.CopyToClipboard(ACopyColumnNames: Boolean = True);
begin
  CopyToClipboard(0, 0, GetColCount - 1, GetRowCount - 1, ACopyColumnNames);
end;

procedure TcaMatrix.CopyToClipboard(AColFrom, ARowFrom, AColTo, ARowTo: Integer; ACopyColumnNames: Boolean = True);
begin
  Clipboard.AsText := AsString(AColFrom, ARowFrom, AColTo, ARowTo, ACopyColumnNames);
end;

procedure TcaMatrix.DeleteCol(const ACol: Integer);
var
  DelCol: Integer;
begin
  for DelCol := ACol to ColCount - 2 do
    CopyCol(DelCol + 1, DelCol);
  SetColCount(GetColCount - 1);
end;

procedure TcaMatrix.DeleteRow(const ARow: Integer);
var
  DelRow: Integer;
begin
  for DelRow := ARow to RowCount - 2 do
    CopyRow(DelRow + 1, DelRow);
  SetRowCount(GetRowCount - 1);
end;

procedure TcaMatrix.InsertCol(const ACol: Integer);
var
  ColIndex: Integer;
  EndCol: Integer;
begin
  EndCol := AddCol;
  for ColIndex := Pred(EndCol) downto ACol do
    CopyCol(ColIndex, Succ(ColIndex));
  ClearCol(ACol);
end;

procedure TcaMatrix.InsertRow(const ARow: Integer);
var
  EndRow: Integer;
  RowIndex: Integer;
begin
  EndRow := AddRow;
  for RowIndex := Pred(EndRow) downto ARow do
    CopyRow(RowIndex, Succ(RowIndex));
  ClearRow(ARow);
end;

procedure TcaMatrix.MoveRowDown(const ARow: Integer);
begin
  if ARow < Pred(GetRowCount) then
    begin
      FSwapRow := AddRow;
      SwapRows(ARow, Succ(ARow));
      RowCount := RowCount - 1;
      ResetSwapRow;
    end;
end;

procedure TcaMatrix.MoveRowUp(const ARow: Integer);
begin
  if ARow > 0 then
    begin
      FSwapRow := AddRow;
      SwapRows(ARow, Pred(ARow));
      RowCount := RowCount - 1;
      ResetSwapRow;
    end;
end;

procedure TcaMatrix.Sort;

 {} procedure QuickSort(const ALo, AHi: Integer);
 {} var
 {}   Lo, Hi, Mid: Integer;
 {} begin
 {}   Lo := ALo;
 {}   Hi := AHi;
 {}   Mid := (Lo + Hi) div 2;
 {}   CopyRow(Mid, FCompRow);
 {}   repeat
 {}     while (CompareRows(FCompRow, Lo) = TcaCompareResult(sdAscending)) do Inc(Lo);
 {}     while (CompareRows(Hi, FCompRow) = TcaCompareResult(sdAscending)) do Dec(Hi);
 {}     if (Lo <= Hi) then
 {}       begin
 {}         if (Lo <> Hi) then SwapRows(Lo, Hi);
 {}         Inc(Lo);
 {}         Dec(Hi);
 {}       end;
 {}   until Lo > Hi;
 {}   if Hi > ALo then QuickSort(ALo, Hi);
 {}   if Lo < AHi then QuickSort(Lo, AHi);
 {} end;

begin
  if FSortColumns.Count > 0 then
    begin
      FCompRow := AddRow;
      FSwapRow := AddRow;
      QuickSort(0, RowCount - 3);
      RowCount := RowCount - 2;
      ResetSwapRow;
    end;
end;

procedure TcaMatrix.SynchronizeDimensions(AMatrix: TcaMatrix);
begin
  SetColCount(AMatrix.ColCount);
  SetRowCount(AMatrix.RowCount);
end;

procedure TcaMatrix.SynchronizeDimensions(AMatrix: IcaMatrix);
begin
  SetColCount(AMatrix.ColCount);
  SetRowCount(AMatrix.RowCount);
end;

  // Fill matrix methods 

procedure TcaMatrix.Fill(AValue: Extended);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetExtended(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: String);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetString(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: Double);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetDouble(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: Boolean);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetBoolean(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: TDateTime);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetDateTime(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: TObject);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetObject(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: Single);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetSingle(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: TStrings);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetMemo(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: Integer);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetInteger(ACol, ARow, AValue);
end;

procedure TcaMatrix.Fill(AValue: Int64);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetInt64(ACol, ARow, AValue);
end;

  // Fill matrix column methods 

procedure TcaMatrix.FillCol(ACol: Integer; AValue: Extended);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetExtended(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: String);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetString(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: Double);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetDouble(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: Boolean);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetBoolean(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: TDateTime);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetDateTime(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: TObject);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetObject(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: Single);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetSingle(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: TStrings);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetMemo(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol, AValue: Integer);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetInteger(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillCol(ACol: Integer; AValue: Int64);
var
  ARow: Integer;
begin
  for ARow := 0 to Pred(GetRowCount) do
    SetInt64(ACol, ARow, AValue);
end;

  // Fill matrix row methods 

procedure TcaMatrix.FillRow(ARow: Integer; AValue: Extended);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetExtended(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: String);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetString(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: Double);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetDouble(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: Boolean);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetBoolean(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: TDateTime);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetDateTime(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: TObject);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetObject(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: Single);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetSingle(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: TStrings);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetMemo(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow, AValue: Integer);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetInteger(ACol, ARow, AValue);
end;

procedure TcaMatrix.FillRow(ARow: Integer; AValue: Int64);
var
  ACol: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    SetInt64(ACol, ARow, AValue);
end;

procedure TcaMatrix.GetColumnAsStrings(const ACol: Integer; AStrings: TStrings);
var
  ARow: Integer;
begin
  AStrings.Clear;
  for ARow := 0 to Pred(GetRowCount) do
    AStrings.Add(Strings[ACol, ARow]);
end;

procedure TcaMatrix.GetColumnAsStrings(const ACol: Integer; AStrings: IcaStringList);
begin
  GetColumnAsStrings(ACol, AStrings.GetStrings);
end;

procedure TcaMatrix.LoadFromFile(const AFileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TcaMatrix.LoadFromStream(Stream: TStream);
var
  ARow: Integer;
begin
  if ReadHeader(Stream) then
    begin
      for ARow := 0 to Pred(GetRowCount) do
        LoadRowFromStream(ARow, Stream);
    end;
end;

procedure TcaMatrix.LoadFromXML(const AFileName: String);
var
  XMLReader: IcaXMLReader;
begin
  XMLReader := TcaXMLReader.Create;
  XMLReader.LoadFromXML(AFileName);
  RunXMLReader(XMLReader);
end;

procedure TcaMatrix.SaveToExcel(const AFileName: String);
var
  ACol: Integer;
  ARow: Integer;
  DocName: String;
  ExcelFactory: IcaExcelFactory;
begin
  ExcelFactory := TcaExcelFactory.Create;
  if FName <> '' then
    DocName := FName
  else
    DocName := 'Matrix';
  for ACol := 0 to Pred(GetColCount) do
    ExcelFactory.AddHeaderColumn(FColumnNames[ACol]);
  ExcelFactory.WriteHeader;
  for ARow := 0 to Pred(GetRowCount) do
    begin
      for ACol := 0 to Pred(GetColCount) do
        ExcelFactory.AddRowColumn(FColumnNames[ACol], Strings[ACol, ARow]);
      ExcelFactory.WriteRow;
    end;
  ExcelFactory.SaveXLSToFile(AFileName);
end;

procedure TcaMatrix.SaveToFile(const AFileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TcaMatrix.SaveToStream(Stream: TStream);
var
  ARow: Integer;
begin
  WriteHeader(Stream);
  for ARow := 0 to Pred(GetRowCount) do
    SaveRowToStream(ARow, Stream);
end;

procedure TcaMatrix.SaveToXML(const AFileName: String);
var
  XmlBuilder: IcaXmlBuilder;
begin
  XmlBuilder := CreateXmlBuilder;
  XmlBuilder.SaveXMLToFile(AFileName);
end;

  // Matrix math 

procedure TcaMatrix.AddMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
begin
  PerformMathOperation(moAdd, AMatrix, AResultMatrix);
end;

procedure TcaMatrix.DivideMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
begin
  PerformMathOperation(moDivide, AMatrix, AResultMatrix);
end;

procedure TcaMatrix.MultiplyMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
begin
  PerformMathOperation(moMultiply, AMatrix, AResultMatrix);
end;

procedure TcaMatrix.SubtractMatrix(AMatrix: TcaMatrix; AResultMatrix: TcaMatrix = nil);
begin
  PerformMathOperation(moSubtract, AMatrix, AResultMatrix);
end;

procedure TcaMatrix.PerformMathOperation(AOperation: TcaMathOperation; AMatrix, AResultMatrix: TcaMatrix);
var
  ACol: Integer;
  ARow: Integer;
  ResultMatrix: TcaMatrix;
begin
  CheckDimensions(AMatrix);
  if AResultMatrix <> nil then
    begin
      CheckDimensions(AResultMatrix);
      ResultMatrix := AResultMatrix;
    end
  else
    ResultMatrix := Self;
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      begin
        if Cells[ACol, ARow] <> nil then
          begin
            if Cells[ACol, ARow].CellType in [ctInteger, ctInt64, ctSingle, ctDouble, ctExtended, ctDateTime] then
              begin
                case AOperation of
                  moAdd:        DoAddCell(AMatrix, ResultMatrix, ACol, ARow);
                  moDivide:     DoDivideCell(AMatrix, ResultMatrix, ACol, ARow);
                  moMultiply:   DoMultiplyCell(AMatrix, ResultMatrix, ACol, ARow);
                  moSubtract:   DoSubtractCell(AMatrix, ResultMatrix, ACol, ARow)
                end;
              end;
          end;
      end;
end;

  // Protected methods 

function TcaMatrix.GetBooleanAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  ABoolean: Boolean;
  AString: String;
begin
  ABoolean := TcaBooleanCell(ACell).Value;
  AString := '';
  DoGetBooleanAsString(ACol, ARow, ABoolean, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.BooleanToString(ABoolean);
end;

function TcaMatrix.GetDateTimeAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  ADateTime: TDateTime;
  AFormat: String;
  AString: String;
begin
  ADateTime := TcaDateTimeCell(ACell).Value;
  AFormat := FDateFormat;
  AString := '';
  DoGetDateTimeAsString(ACol, ARow, ADateTime, AFormat, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.DateTimeToString(ADateTime, AFormat);
end;

function TcaMatrix.GetDoubleAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  ADouble: Double;
  AFormat: String;
  AString: String;
begin
  ADouble := TcaDoubleCell(ACell).Value;
  AFormat := FFloatFormat;
  AString := '';
  DoGetDoubleAsString(ACol, ARow, ADouble, AFormat, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.DoubleToString(ADouble, AFormat);
end;

function TcaMatrix.GetExtendedAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  AExtended: Extended;
  AFormat: String;
  AString: String;
begin
  AExtended := TcaExtendedCell(ACell).Value;
  AFormat := FFloatFormat;
  AString := '';
  DoGetExtendedAsString(ACol, ARow, AExtended, AFormat, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.ExtendedToString(AExtended, AFormat);
end;

function TcaMatrix.GetFormulaAsString(ACol, ARow: Integer; ACell: TcaCell): String;
begin
  Result := TcaFormulaCell(ACell).Value;
  DoGetFormulaAsString(ACol, ARow, Result);
end;

function TcaMatrix.GetInt64AsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  AInt64: Int64;
  AFormat: String;
  AString: String;
begin
  AInt64 := TcaInt64Cell(ACell).Value;
  AFormat := FIntegerFormat;
  AString := '';
  DoGetInt64AsString(ACol, ARow, AInt64, AFormat, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.Int64ToString(AInt64, AFormat);
end;

function TcaMatrix.GetIntegerAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  AInteger: Integer;
  AFormat: String;
  AString: String;
begin
  AInteger := TcaIntegerCell(ACell).Value;
  AFormat := FIntegerFormat;
  AString := '';
  DoGetIntegerAsString(ACol, ARow, AInteger, AFormat, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.IntegerToString(AInteger, AFormat);
end;

function TcaMatrix.GetMemoAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  AMemo: TStrings;
  AString: String;
begin
  AMemo := TcaMemoCell(ACell).Value;
  AString := '';
  DoGetMemoAsString(ACol, ARow, AMemo, AString);
  if AString <> '' then
    Result := AString
  else
    Result := AMemo.Text;
end;

function TcaMatrix.GetObjectAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  AObject: TObject;
  AString: String;
begin
  AObject := TcaObjectCell(ACell).Value;
  AString := '';
  DoGetObjectAsString(ACol, ARow, AObject, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.ObjectToString(AObject);
end;

function TcaMatrix.GetSingleAsString(ACol, ARow: Integer; ACell: TcaCell): String;
var
  ASingle: Single;
  AFormat: String;
  AString: String;
begin
  ASingle := TcaSingleCell(ACell).Value;
  AFormat := FFloatFormat;
  AString := '';
  DoGetSingleAsString(ACol, ARow, ASingle, AFormat, AString);
  if AString <> '' then
    Result := AString
  else
    Result := Utils.SingleToString(ASingle, AFormat);
end;

procedure TcaMatrix.ReleaseCell(ACol, ARow: Integer);
begin
  FCells.ReleaseElement(ACol, ARow);
end;

procedure TcaMatrix.ReleaseCells;
begin
  // This is a temporary hack, and will go when 
  // the TcaMatrix internals are converted to   
  // an all-object reference basis              
  try
    FCells.ReleaseElements;
  except
    Pass;
  end;
end;

procedure TcaMatrix.RestoreUnsortedMatrix;
begin
  if FMatrixCopy <> nil then
    begin
      Assign(FMatrixCopy);
      FMatrixCopy.Free;
      FMatrixCopy := nil;
    end;
end;

  // Protected virtual methods 

function TcaMatrix.CreateBooleanCell: TcaBooleanCell;
begin
  Result := TcaBooleanCell(CreateCell(ctBoolean));
end;

function TcaMatrix.CreateDateTimeCell: TcaDateTimeCell;
begin
  Result := TcaDateTimeCell(CreateCell(ctDateTime));
end;

function TcaMatrix.CreateDoubleCell: TcaDoubleCell;
begin
  Result := TcaDoubleCell(CreateCell(ctDouble));
end;

function TcaMatrix.CreateExtendedCell: TcaExtendedCell;
begin
  Result := TcaExtendedCell(CreateCell(ctExtended));
end;

function TcaMatrix.CreateFormulaCell: TcaFormulaCell;
begin
  Result := TcaFormulaCell(CreateCell(ctFormula));
end;

function TcaMatrix.CreateInt64Cell: TcaInt64Cell;
begin
  Result := TcaInt64Cell(CreateCell(ctInt64));
end;

function TcaMatrix.CreateIntegerCell: TcaIntegerCell;
begin
  Result := TcaIntegerCell(CreateCell(ctInteger));
end;

function TcaMatrix.CreateMemoCell: TcaMemoCell;
begin
  Result := TcaMemoCell(CreateCell(ctMemo));
end;

// move CellConverter to own unit / rename / and include cell creation methods

function TcaMatrix.CreateObjectCell: TcaObjectCell;
begin
  Result := TcaObjectCell(CreateCell(ctObject));
end;

function TcaMatrix.CreateSingleCell: TcaSingleCell;
begin
  Result := TcaSingleCell(CreateCell(ctSingle));
end;

function TcaMatrix.CreateStringCell: TcaStringCell;
begin
  Result := TcaStringCell(CreateCell(ctString));
end;

procedure TcaMatrix.DoBuildColumnNames;
begin
  FColumnNames.Clear;
end;

  // Protected cell math methods 

procedure TcaMatrix.DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  raise EcaMatrixError.Create('Addition can only be applied to TcaMatrix descendents');
end;

procedure TcaMatrix.DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  raise EcaMatrixError.Create('Division can only be applied to TcaMatrix descendents');
end;

procedure TcaMatrix.DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  raise EcaMatrixError.Create('Multiplication can only be applied to TcaMatrix descendents');
end;

procedure TcaMatrix.DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  raise EcaMatrixError.Create('Subtraction can only be applied to TcaMatrix descendents');
end;

  // Protected cell access methods 

procedure TcaMatrix.DoGetBooleanAsString(ACol, ARow: Integer; ABoolean: Boolean; var AString: String);
begin
  if Assigned(FOnGetBooleanAsString) then
    FOnGetBooleanAsString(Self, ACol, ARow, ABoolean, AString);
end;

procedure TcaMatrix.DoGetDateTimeAsString(ACol, ARow: Integer; ADateTime: TDateTime; var AFormat, AString: String);
begin
  if Assigned(FOnGetDateTimeAsString) then
    FOnGetDateTimeAsString(Self, ACol, ARow, ADateTime, AFormat, AString);
end;

procedure TcaMatrix.DoGetDoubleAsString(ACol, ARow: Integer; ADouble: Double; var AFormat, AString: String);
begin
  if Assigned(FOnGetDoubleAsString) then
    FOnGetDoubleAsString(Self, ACol, ARow, ADouble, AFormat, AString);
end;

procedure TcaMatrix.DoGetExtendedAsString(ACol, ARow: Integer; AExtended: Extended; var AFormat, AString: String);
begin
  if Assigned(FOnGetExtendedAsString) then
    FOnGetExtendedAsString(Self, ACol, ARow, AExtended, AFormat, AString);
end;

procedure TcaMatrix.DoGetFormulaAsString(ACol, ARow: Integer; var AString: String);
begin
  if Assigned(FOnGetFormulaAsString) then
    FOnGetFormulaAsString(Self, ACol, ARow, AString);
end;

procedure TcaMatrix.DoGetInt64AsString(ACol, ARow: Integer; AInt64: Int64; var AFormat, AString: String); 
begin
  if Assigned(FOnGetInt64AsString) then
    FOnGetInt64AsString(Self, ACol, ARow, AInt64, AFormat, AString);
end;

procedure TcaMatrix.DoGetIntegerAsString(ACol, ARow, AInteger: Integer; var AFormat, AString: String);
begin
  if Assigned(FOnGetIntegerAsString) then
    FOnGetIntegerAsString(Self, ACol, ARow, AInteger, AFormat, AString);
end;

procedure TcaMatrix.DoGetMemoAsString(ACol, ARow: Integer; AMemo: TStrings; var AString: String);
begin
  if Assigned(FOnGetMemoAsString) then
    FOnGetMemoAsString(Self, ACol, ARow, AMemo, AString);
end;

procedure TcaMatrix.DoGetObjectAsString(ACol, ARow: Integer; AObject: TObject; var AString: String);
begin
  if Assigned(FOnGetObjectAsString) then
    FOnGetObjectAsString(Self, ACol, ARow, AObject, AString);
end;

procedure TcaMatrix.DoGetSingleAsString(ACol, ARow: Integer; ASingle: Single; var AFormat, AString: String);
begin
  if Assigned(FOnGetSingleAsString) then
    FOnGetSingleAsString(Self, ACol, ARow, ASingle, AFormat, AString);
end;

  // SetCell event trigger methods 

procedure TcaMatrix.DoSetCellValue(ACol, ARow: Integer; const ACell: TcaCell);
begin
  if ACell <> nil then
    begin
      if Assigned(FOnSetCellValue) then FOnSetCellValue(Self, ACol, ARow, ACell);
      case
        ACell.CellType of
          ctObject:     DoSetObject(ACol, ARow, TcaObjectCell(ACell).Value);
          ctInteger:    DoSetInteger(ACol, ARow, TcaIntegerCell(ACell).Value);
          ctInt64:      DoSetInt64(ACol, ARow, TcaInt64Cell(ACell).Value);
          ctSingle:     DoSetSingle(ACol, ARow, TcaSingleCell(ACell).Value);
          ctDouble:     DoSetDouble(ACol, ARow, TcaDoubleCell(ACell).Value);
          ctExtended:   DoSetExtended(ACol, ARow, TcaExtendedCell(ACell).Value);
          ctString:     DoSetString(ACol, ARow, TcaStringCell(ACell).Value);
          ctMemo:       DoSetMemo(ACol, ARow, TcaMemoCell(ACell).Value);
          ctBoolean:    DoSetBoolean(ACol, ARow, TcaBooleanCell(ACell).Value);
          ctDateTime:   DoSetDateTime(ACol, ARow, TcaDateTimeCell(ACell).Value);
          ctFormula:    DoSetFormula(ACol, ARow, TcaFormulaCell(ACell).Value);
      end;
    end;
end;

procedure TcaMatrix.DoSetBoolean(ACol, ARow: Integer; const ABoolean: Boolean);
begin
  if Assigned(FOnSetBoolean) then FOnSetBoolean(Self, ACol, ARow, ABoolean);
end;

procedure TcaMatrix.DoSetDateTime(ACol, ARow: Integer; const ADateTime: TDateTime);
begin
  if Assigned(FOnSetDateTime) then FOnSetDateTime(Self, ACol, ARow, ADateTime);
end;

procedure TcaMatrix.DoSetDouble(ACol, ARow: Integer; const ADouble: Double);
begin
  if Assigned(FOnSetDouble) then FOnSetDouble(Self, ACol, ARow, ADouble);
end;

procedure TcaMatrix.DoSetExtended(ACol, ARow: Integer; const AExtended: Extended);
begin
  if Assigned(FOnSetExtended) then FOnSetExtended(Self, ACol, ARow, AExtended);
end;

procedure TcaMatrix.DoSetString(ACol, ARow: Integer; const AString: string);
begin
  if Assigned(FOnSetString) then FOnSetString(Self, ACol, ARow, AString);
end;

procedure TcaMatrix.DoSetFormula(ACol, ARow: Integer; const AFormula: string);
begin
  if Assigned(FOnSetFormula) then FOnSetFormula(Self, ACol, ARow, AFormula);
end;

procedure TcaMatrix.DoSetInt64(ACol, ARow: Integer; const AInt64: Int64);
begin
  if Assigned(FOnSetInt64) then FOnSetInt64(Self, ACol, ARow, AInt64);
end;

procedure TcaMatrix.DoSetInteger(ACol, ARow: Integer; const AInteger: Integer);
begin
  if Assigned(FOnSetInteger) then FOnSetInteger(Self, ACol, ARow, AInteger);
end;

procedure TcaMatrix.DoSetMemo(ACol, ARow: Integer; const AMemo: TStrings);
begin
  if Assigned(FOnSetMemo) then FOnSetMemo(Self, ACol, ARow, AMemo);
end;

procedure TcaMatrix.DoSetObject(ACol, ARow: Integer; const AObject: TObject);
begin
  if Assigned(FOnSetObject) then FOnSetObject(Self, ACol, ARow, AObject);
end;

procedure TcaMatrix.DoSetSingle(ACol, ARow: Integer; const ASingle: Single);
begin
  if Assigned(FOnSetSingle) then FOnSetSingle(Self, ACol, ARow, ASingle);
end;

  // Saving and loading private methods 

function TcaMatrix.LoadBooleanFromStream(Stream: TStream): Boolean;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadCellTypeFromStream(Stream: TStream): TcaCellType;
var
  CellType: Byte;
begin
  Stream.ReadBuffer(CellType, SizeOf(Byte));
  Result := TcaCellType(CellType);
end;

function TcaMatrix.LoadDateTimeFromStream(Stream: TStream): TDateTime;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadDoubleFromStream(Stream: TStream): Double;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadExtendedFromStream(Stream: TStream): Extended;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadFormulaFromStream(Stream: TStream): String;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadInt64FromStream(Stream: TStream): Int64;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadIntegerFromStream(Stream: TStream): Integer;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadMemoFromStream(Stream: TStream): IcaStringList;
var
  MemoSize: Integer;
  Buffer: PChar;
  ListText: String;
begin
  Stream.ReadBuffer(MemoSize, SizeOf(MemoSize));
  GetMem(Buffer, MemoSize);
  try
    Stream.ReadBuffer(Buffer^, MemoSize);
    System.SetString(ListText, Buffer, MemoSize);
    Result := TcaStringList.Create;
    Result.Text := ListText;
  finally
    FreeMem(Buffer, MemoSize);
  end;
end;

function TcaMatrix.LoadSingleFromStream(Stream: TStream): Single;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TcaMatrix.LoadStringFromStream(Stream: TStream): String;
var
  StrSize: Integer;
  Buffer: PChar;
begin
  Stream.ReadBuffer(StrSize, SizeOf(StrSize));
  GetMem(Buffer, StrSize);
  try
    Stream.ReadBuffer(Buffer^, StrSize);
    System.SetString(Result, Buffer, StrSize);
  finally
    FreeMem(Buffer, StrSize);
  end;
end;

function TcaMatrix.ReadHeader(Stream: TStream): Boolean;
var
  Header: TcaMatrixHeader;
begin
  Header := TcaMatrixHeader.Create(Self);
  try
    Header.Read(Stream);
    Result := Header.IsValid;
  finally
    Header.Free;
  end;
end;

procedure TcaMatrix.LoadRowFromStream(ARow: Integer; Stream: TStream);
var
  ACol: Integer;
  CellMemo: IcaStringList;
begin
  for ACol := 0 to Pred(GetColCount) do
    begin
      case
        LoadCellTypeFromStream(Stream) of
          ctNil:      Cells[ACol, ARow] := nil;
          ctObject:   ;
          ctInteger:  SetInteger(ACol, ARow, LoadIntegerFromStream(Stream));
          ctInt64:    SetInt64(ACol, ARow, LoadInt64FromStream(Stream));
          ctSingle:   SetSingle(ACol, ARow, LoadSingleFromStream(Stream));
          ctDouble:   SetDouble(ACol, ARow, LoadDoubleFromStream(Stream));
          ctExtended: SetExtended(ACol, ARow, LoadExtendedFromStream(Stream));
          ctString:   SetString(ACol, ARow, LoadStringFromStream(Stream));
          ctMemo:     begin
                        CellMemo := LoadMemoFromStream(Stream);
                        SetMemo(ACol, ARow, CellMemo.GetStrings);
                      end;
          ctBoolean:  SetBoolean(ACol, ARow, LoadBooleanFromStream(Stream));
          ctDateTime: SetDateTime(ACol, ARow, LoadDateTimeFromStream(Stream));
          ctFormula:  SetFormula(ACol, ARow, LoadFormulaFromStream(Stream));
        end;
    end;
end;

procedure TcaMatrix.SaveBooleanToStream(Cell: TcaBooleanCell; Stream: TStream);
var
  Value: Boolean;
begin
  SaveCellTypeToStream(ctBoolean, Stream);
  Value := Cell.Value;
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

procedure TcaMatrix.SaveCellTypeToStream(CellType: TcaCellType; Stream: TStream);
var
  CellTypeByte: Byte;
begin
  CellTypeByte := Ord(CellType);
  Stream.WriteBuffer(CellTypeByte, SizeOf(CellTypeByte));
end;

procedure TcaMatrix.SaveDateTimeToStream(Cell: TcaDateTimeCell; Stream: TStream);
var
  Value: TDateTime;
begin
  SaveCellTypeToStream(ctDateTime, Stream);
  Value := Cell.Value;
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

procedure TcaMatrix.SaveDoubleToStream(Cell: TcaDoubleCell; Stream: TStream);
var
  Value: Double;
begin
  SaveCellTypeToStream(ctDouble, Stream);
  Value := Cell.Value;
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

procedure TcaMatrix.SaveExtendedToStream(Cell: TcaExtendedCell; Stream: TStream);
var
  Value: Extended;
begin
  SaveCellTypeToStream(ctExtended, Stream);
  Value := Cell.Value;
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

procedure TcaMatrix.SaveFormulaToStream(Cell: TcaFormulaCell; Stream: TStream);
var
  Size: Integer;
  StrStream: TStringStream;
begin
  SaveCellTypeToStream(ctFormula, Stream);
  StrStream := TStringStream.Create(Cell.Value);
  try
    Size := StrStream.Size;
    Stream.WriteBuffer(Size, SizeOf(Size));
    Stream.CopyFrom(StrStream, Size);
  finally
    StrStream.Free;
  end;
end;

procedure TcaMatrix.SaveInt64ToStream(Cell: TcaInt64Cell; Stream: TStream);
var
  Int64Value: Integer;
begin
  SaveCellTypeToStream(ctInt64, Stream);
  Int64Value := Cell.Value;
  Stream.WriteBuffer(Int64Value, SizeOf(Int64Value));
end;

procedure TcaMatrix.SaveIntegerToStream(Cell: TcaIntegerCell; Stream: TStream);
var
  IntValue: Integer;
begin
  SaveCellTypeToStream(ctInteger, Stream);
  IntValue := Cell.Value;
  Stream.WriteBuffer(IntValue, SizeOf(IntValue));
end;

procedure TcaMatrix.SaveMemoToStream(Cell: TcaMemoCell; Stream: TStream);
var
  MemoSize: Integer;
begin
  SaveCellTypeToStream(ctMemo, Stream);
  MemoSize := Length(TStrings(Cell.Value).Text);
  Stream.WriteBuffer(MemoSize, SizeOf(MemoSize));
  TStrings(Cell.Value).SaveToStream(Stream);
end;

procedure TcaMatrix.SaveRowToStream(ARow: Integer; Stream: TStream);
var
  ACol: Integer;
  Cell: TcaCell;
begin
  for ACol := 0 to Pred(GetColCount) do
    begin
      Cell := Cells[ACol, ARow];
      if Cell <> nil then
        begin
          case
            Cell.CellType of
              ctObject:   ;
              ctInteger:  SaveIntegerToStream(TcaIntegerCell(Cell), Stream);
              ctInt64:    SaveInt64ToStream(TcaInt64Cell(Cell), Stream);
              ctSingle:   SaveSingleToStream(TcaSingleCell(Cell), Stream);
              ctDouble:   SaveDoubleToStream(TcaDoubleCell(Cell), Stream);
              ctExtended: SaveExtendedToStream(TcaExtendedCell(Cell), Stream);
              ctString:   SaveStringToStream(TcaStringCell(Cell), Stream);
              ctMemo:     SaveMemoToStream(TcaMemoCell(Cell), Stream);
              ctBoolean:  SaveBooleanToStream(TcaBooleanCell(Cell), Stream);
              ctDateTime: SaveDateTimeToStream(TcaDateTimeCell(Cell), Stream);
              ctFormula:  SaveFormulaToStream(TcaFormulaCell(Cell), Stream);
            end;
        end
      else
        SaveCellTypeToStream(ctNil, Stream);
    end;
end;

procedure TcaMatrix.SaveSingleToStream(Cell: TcaSingleCell; Stream: TStream);
var
  SingleValue: Single;
begin
  SaveCellTypeToStream(ctSingle, Stream);
  SingleValue := Cell.Value;
  Stream.WriteBuffer(SingleValue, SizeOf(SingleValue));
end;

procedure TcaMatrix.SaveStringToStream(Cell: TcaStringCell; Stream: TStream);
var
  StrSize: Integer;
  StrStream: TStringStream;
begin
  SaveCellTypeToStream(ctString, Stream);
  StrStream := TStringStream.Create(Cell.Value);
  try
    StrSize := StrStream.Size;
    Stream.WriteBuffer(StrSize, SizeOf(StrSize));
    Stream.CopyFrom(StrStream, StrSize);
  finally
    StrStream.Free;
  end;
end;

procedure TcaMatrix.WriteHeader(Stream: TStream);
var
  Header: TcaMatrixHeader;
begin
  Header := TcaMatrixHeader.Create(Self);
  try
    Header.Write(Stream);
  finally
    Header.Free;
  end;
end;

  // Private methods 

function TcaMatrix.CellTypeStrToCellType(const ACellTypeStr: string): TcaCellType;
var
  CellType: TcaCellType;
begin
  Result := ctNil;
  for CellType := Low(TcaCellType) to High(TcaCellType) do
    begin
      if GetEnumName(TypeInfo(TcaCellType), Ord(CellType)) = ACellTypeStr then
        begin
          Result := CellType;
          Break;
        end;
    end
end;

function TcaMatrix.CompareCells(const ACol1, ARow1, ACol2, ARow2: Integer): TcaCompareResult;
var
  Cell1: TcaCell;
  Cell2: TcaCell;

  procedure CompareError;
  begin
    raise EcaMatrixError.Create('Invalid data for sorting');
  end;

begin
  Cell1 := GetCell(ACol1, ARow1);
  Cell2 := GetCell(ACol2, ARow2);
  if (Cell1 = nil) or (Cell2 = nil) then CompareError;
  if Cell1.CellType <> Cell2.CellType then CompareError;
  case Cell1.CellType of
    ctObject:   Result := crEqual;
    ctInteger:  Result := Utils.CompareIntegers(GetInteger(ACol1, ARow1), GetInteger(ACol2, ARow2));
    ctInt64:    Result := Utils.CompareInt64s(GetInt64(ACol1, ARow1), GetInt64(ACol2, ARow2));
    ctSingle:   Result := Utils.CompareSingles(GetSingle(ACol1, ARow1), GetSingle(ACol2, ARow2));
    ctDouble:   Result := Utils.CompareDoubles(GetDouble(ACol1, ARow1), GetDouble(ACol2, ARow2));
    ctExtended: Result := Utils.CompareExtendeds(GetExtended(ACol1, ARow1), GetExtended(ACol2, ARow2));
    ctString:   Result := Utils.CompareStrings(GetString(ACol1, ARow1), GetString(ACol2, ARow2), FStringSortCaseInsensitive);
    ctMemo:     Result := crEqual;
    ctBoolean:  Result := Utils.CompareBooleans(GetBoolean(ACol1, ARow1), GetBoolean(ACol2, ARow2));
    ctDateTime: Result := Utils.CompareDateTimes(GetDateTime(ACol1, ARow1), GetDateTime(ACol2, ARow2));
    ctFormula:  Result := crEqual;
  else
    Result := crUndefined;
  end;
end;

function TcaMatrix.CompareRows(const ARow1, ARow2: Integer): TcaCompareResult;
var
  ColIndex, ACol: Integer;
  CompResult: TcaCompareResult;
begin
  Result := crEqual;
  for ColIndex := 0 to Pred(FSortColumns.Count) do
    begin
      ACol := FSortColumns[ColIndex];
      CompResult := CompareCells(ACol, ARow1, ACol, ARow2);
      if FSortDirections[ColIndex] = Ord(sdDescending) then
        begin
          case CompResult of
            crFirstGreater:   CompResult := crSecondGreater;
            crSecondGreater:  CompResult := crFirstGreater;
            crEqual:          ;
          end;
        end;
      if CompResult <> crEqual then
        begin
          Result := CompResult;
          Break;
        end;
    end;
end;

function TcaMatrix.CreateCell(ACellType: TcaCellType): TcaCell;
begin
  Result := nil;
  case ACellType of
    ctNil:        Pass;
    ctObject:     Result := TcaObjectCell.Create;
    ctInteger:    Result := TcaIntegerCell.Create;
    ctInt64:      Result := TcaInt64Cell.Create;
    ctSingle:     Result := TcaSingleCell.Create;
    ctDouble:     Result := TcaDoubleCell.Create;
    ctExtended:   Result := TcaExtendedCell.Create;
    ctString:     Result := TcaStringCell.Create;
    ctMemo:       Result := TcaMemoCell.Create;
    ctBoolean:    Result := TcaBooleanCell.Create;
    ctDateTime:   Result := TcaDateTimeCell.Create;
    ctFormula:    Result := TcaFormulaCell.Create;
  end;
end;

function TcaMatrix.CreateXmlBuilder: IcaXmlBuilder;
var
  XmlBuilder: IcaXmlBuilder;
begin
  XmlBuilder := TcaXmlBuilder.Create;
  AddXMLHeader(XmlBuilder);
  XmlBuilder.AddText(GetXMLRowContent);
  AddXMLFooter(XmlBuilder);
  Result := XmlBuilder;
end;

function TcaMatrix.GetXMLDocumentName: String;
begin
  if FName <> '' then
    Result := FName
  else
    Result := 'Matrix';
end;

procedure TcaMatrix.AddXMLHeader(AXmlBuilder: IcaXmlBuilder);
var
  ACol: Integer;
  CellType: TcaCellType;
  CellTypeStr: String;
begin
  // Document name 
  AXmlBuilder.AddTag(GetXMLDocumentName);
  AXmlBuilder.WriteValue('ColCount', ColCount);
  AXmlBuilder.WriteValue('RowCount', RowCount);
  // ColumnNames 
  AXmlBuilder.AddTag('ColumnNames');
  for ACol := 0 to Pred(FColumnNames.Count) do
    begin
      AXmlBuilder.AddTag('ColumnName');
      AXmlBuilder.Add(FColumnNames[ACol]);
      AXmlBuilder.EndTag;
    end;
  AXmlBuilder.EndTag;
  // ColumnTypes 
  AXmlBuilder.AddTag('ColumnTypes');
  for ACol := 0 to Pred(GetColCount) do
    begin
      AXmlBuilder.AddTag('ColumnType');
      CellType := GetColumnType(ACol);
      CellTypeStr := GetEnumName(TypeInfo(TcaCellType), Ord(CellType));
      AXmlBuilder.Add(CellTypeStr);
      AXmlBuilder.EndTag;
    end;
  AXmlBuilder.EndTag;
end;

procedure TcaMatrix.AddXMLFooter(AXmlBuilder: IcaXmlBuilder);
begin
  AXmlBuilder.EndTag(GetXMLDocumentName, 0);
end;

procedure TcaMatrix.CheckAutoColCount(ACol: Integer);
begin
  if FAutoColCount then
    SetColCount(Max(GetColCount, ACol + 1));
end;

procedure TcaMatrix.CheckAutoRowCount(ARow: Integer);
begin
  if FAutoRowCount then
    SetRowCount(Max(GetRowCount, ARow + 1));
end;

procedure TcaMatrix.CheckDimensions(AMatrix: TcaMatrix);
begin
  if (AMatrix.ColCount <> GetColCount)
  or (AMatrix.RowCount <> GetRowCount) then
    raise EcaMatrixError.Create('Matrices do not have the same dimensions');
end;

procedure TcaMatrix.ResetSwapRow;
begin
  FSwapRow := -1;
end;

procedure TcaMatrix.SaveUnsortedMatrix;
begin
  if FMatrixCopy = nil then FMatrixCopy := TcaMatrix.Create;
  FMatrixCopy.Assign(Self);
end;

procedure TcaMatrix.SwapRows(const ARow1, ARow2: Integer);
begin
  if FSwapRow < 0 then raise EcaMatrixError.Create('SwapRow has not been assigned');
  CopyRow(ARow1, FSwapRow);
  CopyRow(ARow2, ARow1);
  CopyRow(FSwapRow, ARow2);
end;

procedure TcaMatrix.SynchronizeColumnNames;
begin
  while FColumnNames.Count > GetColCount do
    FColumnNames.Delete(FColumnNames.Count - 1);
  while FColumnNames.Count < GetColCount do
    FColumnNames.Add(Format('Column%d', [FColumnNames.Count + 1]));
end;

  // XML reading private methods 

function TcaMatrix.BuildXMLAttributesList(const AAttributes: String): IcaStringList;
var
  Parser: IcaParser;
  AttributeName: String;
  AttributeValue: String;
begin
  Result := TcaStringList.Create;
  Parser := Utils as IcaParser;
  Parser.Initialize;
  Parser.TokenDelimiters := '="';
  Parser.StringToParse := AAttributes;
  while Parser.HasMoreTokens do
    begin
      AttributeName := Parser.NextToken;
      AttributeValue := Parser.NextToken;
      Result.Add(AttributeName + '=' + AttributeValue);
    end;
end;

function TcaMatrix.GetXMLColumnFromTag(const ATag: String): Integer;
var
  ColumnName: String;
  Index: Integer;
  StrippedTag: String;
begin
  Result := -1;
  StrippedTag := ATag;
  Utils.Replace(StrippedTag, '_', #32);
  for Index := 0 to Pred(FColumnNames.Count) do
    begin
      ColumnName := FColumnNames[Index];
      Utils.Replace(ColumnName, '_', #32);
      if StrippedTag = ColumnName then
        begin
          Result := Index;
          Break;
        end;
    end;
end;

procedure TcaMatrix.RunXMLReader(AXMLReader: IcaXMLReader);
begin
  AXMLReader.OnTag := XMLTagEvent;
  AXMLReader.OnEndTag := XMLEndTagEvent;
  AXMLReader.OnData := XMLDataEvent;
  FColumnTypeIndex := 0;
  FXMLReadRow := 0;
  FColumnNames.Clear;
  AXMLReader.Parse;
end;

procedure TcaMatrix.XMLColCountReceived(const AData: String);
var
  DataAsInt: Integer;
begin
  DataAsInt := Utils.StringToInteger(AData);
  SetColCount(DataAsInt);
  FColumnNames.Clear;
end;

procedure TcaMatrix.XMLRowCountReceived(const AData: String);
var
  DataAsInt: Integer;
begin
  DataAsInt := Utils.StringToInteger(AData);
  SetRowCount(DataAsInt);
end;

procedure TcaMatrix.XMLColumnNameReceived(const AData: String);
begin
  FColumnNames.Add(AData)
end;

procedure TcaMatrix.XMLColumnTypeReceived(const AData: string);
begin
  FColumnTypes.Bytes[FColumnTypeIndex] := Byte(CellTypeStrToCellType(AData));
  Inc(FColumnTypeIndex);
end;

procedure TcaMatrix.XMLDocumentNameReceived(const ATag: String);
begin
  SetName(ATag);
end;

procedure TcaMatrix.XMLEndRowReceived;
begin
  Inc(FXMLReadRow);
end;

procedure TcaMatrix.XMLRowDataReceived(const ATag, AData, AAttributes: String);
var
  ACol: Integer;
  CellMemo: IcaStringList;
  CellType: TcaCellType;
  CellTypeStr: String;
  AttributesList: IcaStringList;
begin
  AttributesList := BuildXMLAttributesList(AAttributes);
  CellTypeStr := AttributesList.Values['CellType'];
  if CellTypeStr <> '' then
    begin
      CellType := TcaCell.StringToCellType(CellTypeStr);
      ACol := GetXMLColumnFromTag(ATag);
      if ACol >= 0 then
        begin
          case CellType of
            ctObject:   Pass;
            ctInteger:  SetInteger(ACol, FXMLReadRow, Utils.StringToInteger(AData));
            ctInt64:    SetInt64(ACol, FXMLReadRow, Utils.StringToInt64(AData));
            ctSingle:   SetSingle(ACol, FXMLReadRow, Utils.StringToSingle(AData));
            ctDouble:   SetDouble(ACol, FXMLReadRow, Utils.StringToDouble(AData));
            ctExtended: SetExtended(ACol, FXMLReadRow, Utils.StringToExtended(AData));
            ctString:   SetString(ACol, FXMLReadRow, AData);
            ctMemo:     begin
                          CellMemo := TcaStringList.Create;
                          // Mime decode memos - all memos are mime encoded as 
                          // this is the only way to produce valid XML if the  
                          // cell itself contains XML                          
                          CellMemo.Text := TaggedMimeDecodeString(AData);
                          SetMemo(ACol, FXMLReadRow, CellMemo.GetStrings);
                        end;
            ctBoolean:  SetBoolean(ACol, FXMLReadRow, Utils.StringToBoolean(AData));
            ctDateTime: SetDateTime(ACol, FXMLReadRow, Utils.StringToDateTime(AData));
            ctFormula:  SetFormula(ACol, FXMLReadRow, AData);
          end;
        end;
    end;
end;

  // Cell access property methods 

function TcaMatrix.GetBoolean(ACol, ARow: Integer): Boolean;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsBoolean;
end;

function TcaMatrix.GetDateTime(ACol, ARow: Integer): TDateTime;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsDateTime;
end;

function TcaMatrix.GetDouble(ACol, ARow: Integer): Double;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsDouble;
end;

function TcaMatrix.GetExtended(ACol, ARow: Integer): Extended;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsExtended;
end;

function TcaMatrix.GetFormula(ACol, ARow: Integer): String;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsFormula;
end;

function TcaMatrix.GetInt64(ACol, ARow: Integer): Int64;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsInt64;
end;

function TcaMatrix.GetInteger(ACol, ARow: Integer): Integer;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsInteger;
end;

function TcaMatrix.GetMemo(ACol, ARow: Integer): TStrings;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsMemo;
end;

function TcaMatrix.GetObject(ACol, ARow: Integer): TObject;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsObject;
end;

function TcaMatrix.GetSelected(ACol, ARow: Integer): Boolean;
var
  Cell: TcaCell;
begin
  Result := False;
  { TODO : Investigate Selected and Refactor }
  Cell := TcaCell(FCells.Items[ACol, ARow]);
  if Cell <> nil then Result := Cell.Selected;
end;

function TcaMatrix.GetSingle(ACol, ARow: Integer): Single;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsSingle;
end;

function TcaMatrix.GetString(ACol, ARow: Integer): String;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsString;
end;

function TcaMatrix.GetVariant(ACol, ARow: Integer): Variant;
begin
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  Result := FConverter.AsVariant;
end;

procedure TcaMatrix.SetBoolean(ACol, ARow: Integer; const Value: Boolean);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsBoolean := Value;
end;

procedure TcaMatrix.SetDateTime(ACol, ARow: Integer; const Value: TDateTime);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsDateTime := Value;
end;

procedure TcaMatrix.SetDouble(ACol, ARow: Integer; const Value: Double);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsDouble := Value;
end;

procedure TcaMatrix.SetExtended(ACol, ARow: Integer; const Value: Extended);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsExtended := Value;
end;

procedure TcaMatrix.SetFormula(ACol, ARow: Integer; const Value: String);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsFormula := Value;
end;

procedure TcaMatrix.SetInt64(ACol, ARow: Integer; const Value: Int64);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsInt64 := Value;
end;

procedure TcaMatrix.SetInteger(ACol, ARow: Integer; const Value: Integer);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsInteger := Value;
end;

procedure TcaMatrix.SetMemo(ACol, ARow: Integer; const Value: TStrings);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsMemo := Value;
end;

procedure TcaMatrix.SetObject(ACol, ARow: Integer; const Value: TObject);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsObject := Value;
end;

procedure TcaMatrix.SetSelected(ACol, ARow: Integer; const Value: Boolean);
var
  Cell: TcaCell;
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  Cell := TcaCell(FCells[ACol, ARow]);
  if Cell = nil then
    begin
      Cell := TcaCell.Create;
      Cell.Selected := Value;
      FCells[ACol, ARow] := Cell;
    end
  else
    Cell.Selected := Value;
end;

procedure TcaMatrix.SetSingle(ACol, ARow: Integer; const Value: Single);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsSingle := Value;
end;

procedure TcaMatrix.SetString(ACol, ARow: Integer; const Value: String);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsString := Value;
end;

procedure TcaMatrix.SetVariant(ACol, ARow: Integer; const Value: Variant);
begin
  CheckAutoColCount(ACol);
  CheckAutoRowCount(ARow);
  FConverter.Col := ACol;
  FConverter.Row := ARow;
  FConverter.AsVariant := Value;
end;

  // Event handlers 

procedure TcaMatrix.XMLDataEvent(Sender: TObject; const ATag, AData, AAttributes: String; ALevel: Integer);
begin
  if ALevel = 1 then
    begin
      if ATag = 'ColCount' then
        XMLColCountReceived(AData);
      if ATag = 'RowCount' then
        XMLRowCountReceived(AData);
    end;
  if ALevel = 2 then
    begin
      if ATag = 'ColumnName' then
        XMLColumnNameReceived(AData)
      else
        begin
          if ATag = 'ColumnType' then
            XMLColumnTypeReceived(AData)
          else
            XMLRowDataReceived(ATag, AData, AAttributes);
        end;
    end;
end;

procedure TcaMatrix.XMLTagEvent(Sender: TObject; const ATag, AAttributes: String; ALevel: Integer);
begin
  if ALevel = 0 then XMLDocumentNameReceived(ATag);
end;

procedure TcaMatrix.XMLEndTagEvent(Sender: TObject; const ATag: String; ALevel: Integer);
begin
  if (ALevel = 1) and (ATag = 'Row') then XMLEndRowReceived;
end;

  // Property methods 

function TcaMatrix.GetAutoColCount: Boolean;
begin
  Result := FAutoColCount;
end;

function TcaMatrix.GetAutoRowCount: Boolean;
begin
  Result := FAutoRowCount;
end;

function TcaMatrix.GetCell(ACol, ARow: Integer): TcaCell;
begin
  Result := TcaCell(FCells[ACol, ARow]);
end;

function TcaMatrix.GetCellIsNumeric(ACol, ARow: Integer): Boolean;
begin
  Result := GetCell(ACol, ARow).CellType in [ctInteger, ctInt64, ctSingle, ctDouble, ctExtended];
end;

function TcaMatrix.GetColCount: Integer;
begin
  Result := FCells.ColCount;
end;

function TcaMatrix.GetColumnNames: TStrings;
begin
  Result := FColumnNames;
end;

function TcaMatrix.GetColumnType(ACol: Integer): TcaCellType;
begin
  Result := TcaCellType(FColumnTypes.Bytes[ACol]);
end;

function TcaMatrix.GetDateFormat: String;
begin
  Result := FDateFormat;
end;

function TcaMatrix.GetFloatFormat: String;
begin
  Result := FFloatFormat;
end;

function TcaMatrix.GetIntegerFormat: String;
begin
  Result := FIntegerFormat;
end;

function TcaMatrix.GetMatrixCopy: TcaMatrix;
begin
  Result := FMatrixCopy;
end;

function TcaMatrix.GetName: String;
begin
  Result := FName;
end;

function TcaMatrix.GetOwnsObject(ACol, ARow: Integer): Boolean;
var
  Cell: TcaCell;
begin
  Result := False;
  Cell := GetCell(ACol, ARow);
  if Cell <> nil then
    if Cell.CellType = ctObject then
      Result := TcaObjectCell(Cell).OwnsObject;
end;

function TcaMatrix.GetRowCount: Integer;
begin
  Result := FCells.RowCount;
end;

function TcaMatrix.GetSaveUnsorted: Boolean;
begin
  Result := FSaveUnsorted;
end;

function TcaMatrix.GetSortColumns: TcaIntegerVector;
begin
  Result := FSortColumns;
end;

function TcaMatrix.GetSortDirections: TcaIntegerVector;
begin
  Result := FSortDirections;
end;

function TcaMatrix.GetSorted: Boolean;
begin
  Result := FSorted;
end;

function TcaMatrix.GetStringSortCaseInsensitive: Boolean;
begin
  Result := FStringSortCaseInsensitive;
end;

function TcaMatrix.GetXML: String;
var
  XmlBuilder: IcaXmlBuilder;
begin
  XmlBuilder := CreateXmlBuilder;
  Result := XmlBuilder.AsText;
end;

function TcaMatrix.GetXMLRowContent: String;
var
  ACol: Integer;
  ARow: Integer;
  Cell: TcaCell;
  XmlBuilder: IcaXmlBuilder;
  CellType: TcaCellType;
  CellTypeStr: String;
  CellStr: String;
  Attribute: String;
begin
  XmlBuilder := TcaXmlBuilder.Create;
  for ARow := 0 to Pred(GetRowCount) do
    begin
      XmlBuilder.AddTag('Row', Format('Number="%d"', [ARow]), 1);
      for ACol := 0 to Pred(GetColCount) do
        begin
          CellStr := GetString(ACol, ARow);
          Cell := GetCell(ACol, ARow);
          if Cell <> nil then
            begin
              CellType := Cell.CellType;
              if CellType = ctMemo then
                // Mime encode memos - this is the only way to produce 
                // valid XML if the cell itself contains XML           
                CellStr := TaggedMimeEncodeString(CellStr);
              CellTypeStr := GetEnumName(TypeInfo(TcaCellType), Ord(CellType));
              Attribute := Format('CellType="%s"', [CellTypeStr]);
            end
          else
            Attribute := '';
          XmlBuilder.AddTag(FColumnNames[ACol], Attribute, 2);
          XmlBuilder.Add(CellStr, 3);
          XmlBuilder.EndTag(FColumnNames[ACol], 2);
        end;
      XmlBuilder.EndTag('Row', 1);
    end;
  Result := XmlBuilder.AsText;
end;

procedure TcaMatrix.SetAutoColCount(const Value: Boolean);
begin
  FAutoColCount := Value;
end;

procedure TcaMatrix.SetAutoRowCount(const Value: Boolean);
begin
  FAutoRowCount := Value;
end;

procedure TcaMatrix.SetCell(ACol, ARow: Integer; const Value: TcaCell);
begin
  FCells[ACol, ARow] := Value;
end;

procedure TcaMatrix.SetColCount(const Value: Integer);
begin
  FCells.ColCount := Value;
  SynchronizeColumnNames;
end;

procedure TcaMatrix.SetColumnNames(const Value: TStrings);
begin
  FColumnNames.Assign(Value);
  SynchronizeColumnNames;
end;

procedure TcaMatrix.SetColumnType(ACol: Integer; const Value: TcaCellType);
begin
  FColumnTypes.Bytes[ACol] := Byte(Value);
end;

procedure TcaMatrix.SetDateFormat(const Value: String);
begin
  FDateFormat := Value;
end;

procedure TcaMatrix.SetFloatFormat(const Value: String);
begin
  FFloatFormat := Value;
end;

procedure TcaMatrix.SetIntegerFormat(const Value: String);
begin
  FIntegerFormat := Value;
end;

procedure TcaMatrix.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TcaMatrix.SetOwnsObject(ACol, ARow: Integer; const Value: Boolean);
var
  Cell: TcaCell;
begin
  Cell := GetCell(ACol, ARow);
  if Cell <> nil then
    if Cell.CellType = ctObject then
      TcaObjectCell(Cell).OwnsObject := Value;
end;

procedure TcaMatrix.SetRowCount(const Value: Integer);
begin
  FCells.RowCount := Value;
end;

procedure TcaMatrix.SetSaveUnsorted(const Value: Boolean);
begin
  FSaveUnsorted := Value;
end;

procedure TcaMatrix.SetSorted(const Value: Boolean);
var
  WasUnSorted: Boolean;
begin
  if FCells.RowCount > 0 then
    begin
      WasUnSorted := not FSorted;
      FSorted := Value;
      if FSorted then
        begin
          if WasUnSorted then SaveUnsortedMatrix;
          Sort;
        end
      else
        begin
          if FSaveUnsorted then
            RestoreUnsortedMatrix;
        end;
    end;
end;

procedure TcaMatrix.SetStringSortCaseInsensitive(const Value: Boolean);
begin
  FStringSortCaseInsensitive := Value;
end;

procedure TcaMatrix.SetXML(const Value: String);
var
  StrStream: TStringStream;
  XMLReader: IcaXMLReader;
begin
  XMLReader := TcaXMLReader.Create;
  StrStream := TStringStream.Create(Value);
  try
    XMLReader.LoadFromStream(StrStream);
  finally
    StrStream.Free;
  end;
  RunXMLReader(XMLReader);
end;

  // Get event property methods 

function TcaMatrix.GetOnGetBooleanAsString: TcaGetBooleanAsStringEvent;
begin
  Result := FOnGetBooleanAsString;
end;

function TcaMatrix.GetOnGetDateTimeAsString: TcaGetDateTimeAsStringEvent;
begin
  Result := FOnGetDateTimeAsString;
end;

function TcaMatrix.GetOnGetDoubleAsString: TcaGetDoubleAsStringEvent;
begin
  Result := FOnGetDoubleAsString;
end;

function TcaMatrix.GetOnGetExtendedAsString: TcaGetExtendedAsStringEvent;
begin
  Result := FOnGetExtendedAsString;
end;

function TcaMatrix.GetOnGetFormulaAsString: TcaGetFormulaAsStringEvent;
begin
  Result := FOnGetFormulaAsString;
end;

function TcaMatrix.GetOnGetInt64AsString: TcaGetInt64AsStringEvent;
begin
  Result := FOnGetInt64AsString;
end;

function TcaMatrix.GetOnGetIntegerAsString: TcaGetIntegerAsStringEvent;
begin
  Result := FOnGetIntegerAsString;
end;

function TcaMatrix.GetOnGetMemoAsString: TcaGetMemoAsStringEvent;
begin
  Result := FOnGetMemoAsString;
end;

function TcaMatrix.GetOnGetObjectAsString: TcaGetObjectAsStringEvent;
begin
  Result := FOnGetObjectAsString;
end;

function TcaMatrix.GetOnGetSingleAsString: TcaGetSingleAsStringEvent;
begin
  Result := FOnGetSingleAsString;
end;

procedure TcaMatrix.SetOnGetBooleanAsString(const Value: TcaGetBooleanAsStringEvent);
begin
  FOnGetBooleanAsString := Value;
end;

procedure TcaMatrix.SetOnGetDateTimeAsString(const Value: TcaGetDateTimeAsStringEvent);
begin
  FOnGetDateTimeAsString := Value;
end;

procedure TcaMatrix.SetOnGetDoubleAsString(const Value: TcaGetDoubleAsStringEvent);
begin
  FOnGetDoubleAsString := Value;
end;

procedure TcaMatrix.SetOnGetExtendedAsString(const Value: TcaGetExtendedAsStringEvent);
begin
  FOnGetExtendedAsString := Value;
end;

procedure TcaMatrix.SetOnGetFormulaAsString(const Value: TcaGetFormulaAsStringEvent);
begin
  FOnGetFormulaAsString := Value;
end;

procedure TcaMatrix.SetOnGetInt64AsString(const Value: TcaGetInt64AsStringEvent);
begin
  FOnGetInt64AsString := Value;
end;

procedure TcaMatrix.SetOnGetIntegerAsString(const Value: TcaGetIntegerAsStringEvent);
begin
  FOnGetIntegerAsString := Value;
end;

procedure TcaMatrix.SetOnGetMemoAsString(const Value: TcaGetMemoAsStringEvent);
begin
  FOnGetMemoAsString := Value;
end;

procedure TcaMatrix.SetOnGetObjectAsString(const Value: TcaGetObjectAsStringEvent);
begin
  FOnGetObjectAsString := Value;
end;

procedure TcaMatrix.SetOnGetSingleAsString(const Value: TcaGetSingleAsStringEvent);
begin
  FOnGetSingleAsString := Value;
end;

  // Set event property methods 

function TcaMatrix.GetOnSetBoolean: TcaSetBooleanEvent;
begin
  Result := FOnSetBoolean;
end;

function TcaMatrix.GetOnSetCellValue: TcaSetCellValueEvent;
begin
  Result := FOnSetCellValue;
end;

function TcaMatrix.GetOnSetDateTime: TcaSetDateTimeEvent;
begin
  Result := FOnSetDateTime;
end;

function TcaMatrix.GetOnSetDouble: TcaSetDoubleEvent;
begin
  Result := FOnSetDouble;
end;

function TcaMatrix.GetOnSetExtended: TcaSetExtendedEvent;
begin
  Result := FOnSetExtended;
end;

function TcaMatrix.GetOnSetFormula: TcaSetFormulaEvent;
begin
  Result := FOnSetFormula;
end;

function TcaMatrix.GetOnSetInt64: TcaSetInt64Event;
begin
  Result := FOnSetInt64;
end;

function TcaMatrix.GetOnSetInteger: TcaSetIntegerEvent;
begin
  Result := FOnSetInteger;
end;

function TcaMatrix.GetOnSetMemo: TcaSetMemoEvent;
begin
  Result := FOnSetMemo;
end;

function TcaMatrix.GetOnSetObject: TcaSetObjectEvent;
begin
  Result := FOnSetObject;
end;

function TcaMatrix.GetOnSetSingle: TcaSetSingleEvent;
begin
  Result := FOnSetSingle;
end;

function TcaMatrix.GetOnSetString: TcaSetStringEvent;
begin
  Result := FOnSetString;
end;

procedure TcaMatrix.SetOnSetBoolean(const Value: TcaSetBooleanEvent);
begin
  FOnSetBoolean := Value;
end;

procedure TcaMatrix.SetOnSetCellValue(const Value: TcaSetCellValueEvent);
begin
  FOnSetCellValue := Value;
end;

procedure TcaMatrix.SetOnSetDateTime(const Value: TcaSetDateTimeEvent);
begin
  FOnSetDateTime := Value;
end;

procedure TcaMatrix.SetOnSetDouble(const Value: TcaSetDoubleEvent);
begin
  FOnSetDouble := Value;
end;

procedure TcaMatrix.SetOnSetExtended(const Value: TcaSetExtendedEvent);
begin
  FOnSetExtended := Value;
end;

procedure TcaMatrix.SetOnSetString(const Value: TcaSetStringEvent);
begin
  FOnSetString := Value;
end;

procedure TcaMatrix.SetOnSetFormula(const Value: TcaSetFormulaEvent);
begin
  FOnSetFormula := Value;
end;

procedure TcaMatrix.SetOnSetInt64(const Value: TcaSetInt64Event);
begin
  FOnSetInt64 := Value;
end;

procedure TcaMatrix.SetOnSetInteger(const Value: TcaSetIntegerEvent);
begin
  FOnSetInteger := Value;
end;

procedure TcaMatrix.SetOnSetMemo(const Value: TcaSetMemoEvent);
begin
  FOnSetMemo := Value;
end;

procedure TcaMatrix.SetOnSetObject(const Value: TcaSetObjectEvent);
begin
  FOnSetObject := Value;
end;

procedure TcaMatrix.SetOnSetSingle(const Value: TcaSetSingleEvent);
begin
  FOnSetSingle := Value;
end;

  //---------------------------------------------------------------------------
  // TcaBooleanMatrix                                                          
  //---------------------------------------------------------------------------

function TcaBooleanMatrix.GetBoolean(ACol, ARow: Integer): Boolean;
begin
  Result := inherited Booleans[ACol, ARow];
end;

procedure TcaBooleanMatrix.SetBoolean(ACol, ARow: Integer; const Value: Boolean);
begin
  inherited Booleans[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaDateTimeMatrix                                                         
  //---------------------------------------------------------------------------

function TcaDateTimeMatrix.GetDateTime(ACol, ARow: Integer): TDateTime;
begin
  Result := inherited DateTimes[ACol, ARow];
end;

procedure TcaDateTimeMatrix.DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.DateTimes[ACol, ARow] := Self.DateTimes[ACol, ARow] + AMatrix.DateTimes[ACol, ARow];
end;

procedure TcaDateTimeMatrix.DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
var
  MathUtils: IcaMathUtils;
  Numerator: TDateTime;
  Divisor: TDateTime;
begin
  MathUtils := Utils as IcaMathUtils;
  Numerator := Self.DateTimes[ACol, ARow];
  Divisor := AMatrix.DateTimes[ACol, ARow];
  ResultMatrix.DateTimes[ACol, ARow] := MathUtils.FloatDiv(Numerator, Divisor, 0);
end;

procedure TcaDateTimeMatrix.DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.DateTimes[ACol, ARow] := Self.DateTimes[ACol, ARow] * AMatrix.DateTimes[ACol, ARow];
end;

procedure TcaDateTimeMatrix.DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.DateTimes[ACol, ARow] := Self.DateTimes[ACol, ARow] - AMatrix.DateTimes[ACol, ARow];
end;

procedure TcaDateTimeMatrix.SetDateTime(ACol, ARow: Integer; const Value: TDateTime);
begin
  inherited DateTimes[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaDoubleMatrix                                                           
  //---------------------------------------------------------------------------

procedure TcaDoubleMatrix.DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Doubles[ACol, ARow] := Self.Doubles[ACol, ARow] + AMatrix.Doubles[ACol, ARow];
end;

procedure TcaDoubleMatrix.DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
var
  MathUtils: IcaMathUtils;
  Numerator: Double;
  Divisor: Double;
begin
  MathUtils := Utils as IcaMathUtils;
  Numerator := Self.Doubles[ACol, ARow];
  Divisor := AMatrix.Doubles[ACol, ARow];
  ResultMatrix.Doubles[ACol, ARow] := MathUtils.FloatDiv(Numerator, Divisor, 0);
end;

procedure TcaDoubleMatrix.DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Doubles[ACol, ARow] := Self.Doubles[ACol, ARow] * AMatrix.Doubles[ACol, ARow];
end;

procedure TcaDoubleMatrix.DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Doubles[ACol, ARow] := Self.Doubles[ACol, ARow] - AMatrix.Doubles[ACol, ARow];
end;

  // Protected property methods 

function TcaDoubleMatrix.GetColTotal(ACol: Integer): Double;
var
  ARow: Integer;
begin
  Result := 0;
  for ARow := 0 to Pred(GetRowCount) do
    Result := Result + GetDouble(ACol, ARow);
end;

function TcaDoubleMatrix.GetDouble(ACol, ARow: Integer): Double;
begin
  Result := inherited Doubles[ACol, ARow];
end;

function TcaDoubleMatrix.GetRowTotal(ARow: Integer): Double;
var
  ACol: Integer;
begin
  Result := 0;
  for ACol := 0 to Pred(GetColCount) do
    Result := Result + GetDouble(ACol, ARow);
end;

procedure TcaDoubleMatrix.SetDouble(ACol, ARow: Integer; const Value: Double);
begin
  inherited Doubles[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaExtendedMatrix                                                         
  //---------------------------------------------------------------------------

function TcaExtendedMatrix.GetExtended(ACol, ARow: Integer): Extended;
begin
  Result := inherited Extendeds[ACol, ARow];
end;

procedure TcaExtendedMatrix.DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Extendeds[ACol, ARow] := Self.Extendeds[ACol, ARow] + AMatrix.Extendeds[ACol, ARow];
end;

procedure TcaExtendedMatrix.DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
var
  MathUtils: IcaMathUtils;
  Numerator: Extended;
  Divisor: Extended;
begin
  MathUtils := Utils as IcaMathUtils;
  Numerator := Self.Extendeds[ACol, ARow];
  Divisor := AMatrix.Extendeds[ACol, ARow];
  ResultMatrix.Extendeds[ACol, ARow] := MathUtils.FloatDiv(Numerator, Divisor, 0);
end;

procedure TcaExtendedMatrix.DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Extendeds[ACol, ARow] := Self.Extendeds[ACol, ARow] * AMatrix.Extendeds[ACol, ARow];
end;

procedure TcaExtendedMatrix.DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Extendeds[ACol, ARow] := Self.Extendeds[ACol, ARow] - AMatrix.Extendeds[ACol, ARow];
end;

function TcaExtendedMatrix.GetColTotal(ACol: Integer): Extended;
var
  ARow: Integer;
begin
  Result := 0;
  for ARow := 0 to Pred(GetRowCount) do
    Result := Result + GetExtended(ACol, ARow);
end;

function TcaExtendedMatrix.GetRowTotal(ARow: Integer): Extended;
var
  ACol: Integer;
begin
  Result := 0;
  for ACol := 0 to Pred(GetColCount) do
    Result := Result + GetExtended(ACol, ARow);
end;

procedure TcaExtendedMatrix.SetExtended(ACol, ARow: Integer; const Value: Extended);
begin
  inherited Extendeds[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFormulaMatrix                                                          
  //---------------------------------------------------------------------------

function TcaFormulaMatrix.GetFormula(ACol, ARow: Integer): String;
begin
  Result := inherited Formulas[ACol, ARow];
end;

procedure TcaFormulaMatrix.SetFormula(ACol, ARow: Integer; const Value: String);
begin
  inherited Formulas[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaInt64Matrix                                                            
  //---------------------------------------------------------------------------

function TcaInt64Matrix.GetInt64(ACol, ARow: Integer): Int64;
begin
  Result := inherited Int64s[ACol, ARow];
end;

procedure TcaInt64Matrix.DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Int64s[ACol, ARow] := Self.Int64s[ACol, ARow] + AMatrix.Int64s[ACol, ARow];
end;

procedure TcaInt64Matrix.DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
var
  MathUtils: IcaMathUtils;
  Numerator: Int64;
  Divisor: Int64;
begin
  MathUtils := Utils as IcaMathUtils;
  Numerator := Self.Int64s[ACol, ARow];
  Divisor := AMatrix.Int64s[ACol, ARow];
  ResultMatrix.Int64s[ACol, ARow] := MathUtils.EquiRound(MathUtils.FloatDiv(Numerator, Divisor, 0));
end;

procedure TcaInt64Matrix.DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Int64s[ACol, ARow] := Self.Int64s[ACol, ARow] * AMatrix.Int64s[ACol, ARow];
end;

procedure TcaInt64Matrix.DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Int64s[ACol, ARow] := Self.Int64s[ACol, ARow] - AMatrix.Int64s[ACol, ARow];
end;

procedure TcaInt64Matrix.SetInt64(ACol, ARow: Integer; const Value: Int64);
begin
  inherited Int64s[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaIntegerMatrix                                                          
  //---------------------------------------------------------------------------

function TcaIntegerMatrix.GetInteger(ACol, ARow: Integer): Integer;
begin
  Result := inherited Integers[ACol, ARow];
end;

procedure TcaIntegerMatrix.DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Integers[ACol, ARow] := Self.Integers[ACol, ARow] + AMatrix.Integers[ACol, ARow];
end;

procedure TcaIntegerMatrix.DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
var
  MathUtils: IcaMathUtils;
  Numerator: Integer;
  Divisor: Integer;
begin
  MathUtils := Utils as IcaMathUtils;
  Numerator := Self.Integers[ACol, ARow];
  Divisor := AMatrix.Integers[ACol, ARow];
  ResultMatrix.Integers[ACol, ARow] := MathUtils.EquiRound(MathUtils.FloatDiv(Numerator, Divisor, 0));
end;

procedure TcaIntegerMatrix.DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Integers[ACol, ARow] := Self.Integers[ACol, ARow] * AMatrix.Integers[ACol, ARow];
end;

procedure TcaIntegerMatrix.DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Integers[ACol, ARow] := Self.Integers[ACol, ARow] - AMatrix.Integers[ACol, ARow];
end;

procedure TcaIntegerMatrix.SetInteger(ACol, ARow: Integer; const Value: Integer);
begin
  inherited Integers[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaMemoMatrix                                                             
  //---------------------------------------------------------------------------

function TcaMemoMatrix.GetMemo(ACol, ARow: Integer): TStrings;
begin
  Result := inherited Memos[ACol, ARow];
end;

procedure TcaMemoMatrix.SetMemo(ACol, ARow: Integer; const Value: TStrings);
begin
  inherited Memos[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaObjectMatrix                                                           
  //---------------------------------------------------------------------------

function TcaObjectMatrix.GetObject(ACol, ARow: Integer): TObject;
begin
  Result := inherited Objects[ACol, ARow];
end;

procedure TcaObjectMatrix.SetObject(ACol, ARow: Integer; const Value: TObject);
begin
  inherited Objects[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaSelectedMatrix                                                         
  //---------------------------------------------------------------------------

function TcaSelectedMatrix.GetSelected(ACol, ARow: Integer): Boolean;
begin
  Result := inherited Selected[ACol, ARow];
end;

procedure TcaSelectedMatrix.SetSelected(ACol, ARow: Integer; const Value: Boolean);
begin
  inherited Selected[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaSingleMatrix                                                           
  //---------------------------------------------------------------------------

function TcaSingleMatrix.GetSingle(ACol, ARow: Integer): Single;
begin
  Result := inherited Singles[ACol, ARow];
end;

procedure TcaSingleMatrix.DoAddCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Singles[ACol, ARow] := Self.Singles[ACol, ARow] + AMatrix.Singles[ACol, ARow];
end;

procedure TcaSingleMatrix.DoDivideCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
var
  MathUtils: IcaMathUtils;
  Numerator: Single;
  Divisor: Single;
begin
  MathUtils := Utils as IcaMathUtils;
  Numerator := Self.Singles[ACol, ARow];
  Divisor := AMatrix.Singles[ACol, ARow];
  ResultMatrix.Singles[ACol, ARow] := MathUtils.FloatDiv(Numerator, Divisor, 0);
end;

procedure TcaSingleMatrix.DoMultiplyCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Singles[ACol, ARow] := Self.Singles[ACol, ARow] * AMatrix.Singles[ACol, ARow];
end;

procedure TcaSingleMatrix.DoSubtractCell(AMatrix, ResultMatrix: TcaMatrix; ACol, ARow: Integer);
begin
  ResultMatrix.Singles[ACol, ARow] := Self.Singles[ACol, ARow] - AMatrix.Singles[ACol, ARow];
end;

procedure TcaSingleMatrix.SetSingle(ACol, ARow: Integer; const Value: Single);
begin
  inherited Singles[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaStringMatrix                                                           
  //---------------------------------------------------------------------------

function TcaStringMatrix.GetString(ACol, ARow: Integer): String;
begin
  Result := inherited Strings[ACol, ARow];
end;

procedure TcaStringMatrix.SetString(ACol, ARow: Integer; const Value: String);
begin
  inherited Strings[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaMatrixList                                                             
  //---------------------------------------------------------------------------

  // Create/Destroy 

procedure TcaMatrixList.AfterConstruction;
begin
  inherited;
  FList := TObjectList.Create(True);
end;

procedure TcaMatrixList.BeforeDestruction;
begin
  inherited;
  FList.Free;
end;

  // Public methods 

function TcaMatrixList.Add: TcaMatrix;
begin
  Result := TcaMatrix.Create;
  FList.Add(Result);
end;

function TcaMatrixList.IndexOf(AItem: TcaMatrix): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TcaMatrixList.Clear;
begin
  FList.Clear;
end;

  // Property methods 

function TcaMatrixList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaMatrixList.GetItem(Index: Integer): TcaMatrix;
begin
  Result := TcaMatrix(FList[Index]);
end;

end.


