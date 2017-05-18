unit caArrays;

{$INCLUDE ca.inc}

interface

uses

  Classes,
  SysUtils;

const
  cByteSize   = SizeOf(Byte);

type

  PWord = ^Word;

  //----------------------------------------------------------------------------
  // TcaBitArray                                                                
  //----------------------------------------------------------------------------

  TcaBitArray = class(TObject)
  private
    // Private fields 
    FArray: PWord;
  protected
  public
    // Create/Destroy 
    constructor Create(ASize: Int64);
    destructor Destroy; override;
  end;

  //----------------------------------------------------------------------------
  // TcaBitMatrix                                                               
  //----------------------------------------------------------------------------

  TcaBitMatrix = class(TObject)
  private
    // Private fields 
    FBitOffset: Byte;
    FCols: Integer;
    FRows: Integer;
    FMatrix: PWord;
    FWordPtr: PWord;
    // Private methods
    procedure UpdateBitPosition(ACol, ARow: Integer);
    procedure Initialize;
    // Property methods 
    function GetBit(ACol, ARow: Integer): Boolean;
    procedure SetBit(ACol, ARow: Integer; const AValue: Boolean);
  protected
  public
    // Create/Destroy 
    constructor Create(ACols, ARows: Integer);
    destructor Destroy; override;
    // Properties
    property Bits[ACol, ARow: Integer]: Boolean read GetBit write SetBit;
  end;

  //----------------------------------------------------------------------------
  // TcaNibbleArray                                                             
  //----------------------------------------------------------------------------

  TcaNibbleArray = class(TObject)
  private
  protected
  public
  end;

implementation

const
  cWordSize = SizeOf(Word);

  //----------------------------------------------------------------------------
  // TcaBitArray                                                                
  //----------------------------------------------------------------------------

constructor TcaBitArray.Create(ASize: Int64);
begin
  inherited Create;
  GetMem(FArray, ASize);
end;

destructor TcaBitArray.Destroy;
begin
  FreeMem(FArray);
  inherited;
end;

  //----------------------------------------------------------------------------
  // TcaBitMatrix                                                               
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaBitMatrix.Create(ACols, ARows: Integer);
begin
  inherited Create;
  FCols := ACols;
  FRows := ARows;
  Initialize;
end;

destructor TcaBitMatrix.Destroy;
begin
  FreeMem(FMatrix);
  inherited;
end;

  // Private methods

procedure TcaBitMatrix.Initialize;
begin
  GetMem(FMatrix, FCols * FRows div cByteSize);
end;

procedure TcaBitMatrix.UpdateBitPosition(ACol, ARow: Integer);
var
  BitPos: Integer;
begin
  BitPos := ARow * FCols + ACol;
  FWordPtr := FMatrix;
  Inc(FWordPtr, BitPos div cWordSize);
  FBitOffset := BitPos mod cWordSize;
end;

  // Property methods 

function TcaBitMatrix.GetBit(ACol, ARow: Integer): Boolean;
begin
  UpdateBitPosition(ACol, ARow);
  Result := (FWordPtr^ and (1 shl FBitOffset)) <> 0;
end;

procedure TcaBitMatrix.SetBit(ACol, ARow: Integer; const AValue: Boolean);
begin
  UpdateBitPosition(ACol, ARow);
  FWordPtr^ := FWordPtr^ or (1 shl FBitOffset);
end;

end.
