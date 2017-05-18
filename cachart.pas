unit caChart;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  Classes,
  SysUtils,
  Controls,
  ExtCtrls,
  Graphics,

  // ca units
  caGraphics,
  caClasses,
  caControls;

type

  TcaAxisType = (axX, axY);

 //---------------------------------------------------------------------------
 // Forward declarations
 //---------------------------------------------------------------------------

  TcaChart = class; 

 //---------------------------------------------------------------------------
 // IcaChartObject
 //---------------------------------------------------------------------------

  IcaChartObject = interface
  ['{C92F24EE-1E71-4AF5-9334-44B3B2943348}']
    // Public methods
    procedure Paint;
  end;

 //---------------------------------------------------------------------------
 // TcaChartObject
 //---------------------------------------------------------------------------

  TcaChartObject = class(TcaInterfacedPersistent, IcaChartObject)
  private
    FChart: TcaChart;
    // Property methods
  protected
    // Virtual methods
    procedure Paint; virtual;
  public
    constructor Create(AChart: TcaChart);
  end;

 //---------------------------------------------------------------------------
 // IcaChartAxis
 //---------------------------------------------------------------------------

  IcaChartAxis = interface
  ['{8628D8B6-5229-41CB-BD85-881DC970D406}']
    // Property methods
    function GetAxisType: TcaAxisType;
    function GetMajorInterval: Double;
    function GetMaxValue: Double;
    function GetMinorInterval: Double;
    function GetMinValue: Double;
    function GetOnChanged: TNotifyEvent;
    procedure SetMajorInterval(const Value: Double);
    procedure SetMaxValue(const Value: Double);
    procedure SetMinorInterval(const Value: Double);
    procedure SetMinValue(const Value: Double);
    procedure SetOnChanged(const Value: TNotifyEvent);
    // Protected methods
    procedure Paint;
    // Properties
    property AxisType: TcaAxisType read GetAxisType;
    property MajorInterval: Double read GetMajorInterval write SetMajorInterval;
    property MaxValue: Double read GetMaxValue write SetMaxValue;
    property MinorInterval: Double read GetMinorInterval write SetMinorInterval;
    property MinValue: Double read GetMinValue write SetMinValue;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

 //---------------------------------------------------------------------------
 // TcaChartAxis
 //---------------------------------------------------------------------------

  TcaChartAxis = class(TcaChartObject, IcaChartAxis)
  private
    FAxisType: TcaAxisType;
    FMajorInterval: Double;
    FMaxValue: Double;
    FMinorInterval: Double;
    FMinValue: Double;
    FOnChanged: TNotifyEvent;
    // Property methods
    function GetAxisType: TcaAxisType;
    function GetMajorInterval: Double;
    function GetMaxValue: Double;
    function GetMinorInterval: Double;
    function GetMinValue: Double;
    function GetOnChanged: TNotifyEvent;
    procedure SetMajorInterval(const Value: Double);
    procedure SetMaxValue(const Value: Double);
    procedure SetMinorInterval(const Value: Double);
    procedure SetMinValue(const Value: Double);
    procedure SetOnChanged(const Value: TNotifyEvent);
    // Private methods
    procedure Changed;
  protected
    procedure DoChanged; virtual;
    procedure Paint; override;
  public
    constructor Create(AChart: TcaChart; AAxisType: TcaAxisType);
    // Public properties
    property AxisType: TcaAxisType read GetAxisType;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  published
    // Published properties
    property MajorInterval: Double read GetMajorInterval write SetMajorInterval;
    property MaxValue: Double read GetMaxValue write SetMaxValue;
    property MinorInterval: Double read GetMinorInterval write SetMinorInterval;
    property MinValue: Double read GetMinValue write SetMinValue;
  end;

 //---------------------------------------------------------------------------
 // IcaChartGrid
 //---------------------------------------------------------------------------

  IcaChartGrid = interface
  ['{F3D16104-1E39-4983-868B-1F1E03B50E21}']
    // Property methods
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetXAxis: TcaChartAxis;
    function GetYAxis: TcaChartAxis;
    procedure SetXAxis(const Value: TcaChartAxis);
    procedure SetYAxis(const Value: TcaChartAxis);
    // Protected methods
    procedure Paint;
    // Properties
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property XAxis: TcaChartAxis read GetXAxis write SetXAxis;
    property YAxis: TcaChartAxis read GetYAxis write SetYAxis;
  end;

 //---------------------------------------------------------------------------
 // TcaChartGrid
 //---------------------------------------------------------------------------

  TcaChartGrid = class(TcaChartObject, IcaChartGrid)
  private
    FHeight: Integer;
    FWidth: Integer;
    FXAxis: TcaChartAxis;
    FYAxis: TcaChartAxis;
    // Property methods
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetXAxis: TcaChartAxis;
    function GetYAxis: TcaChartAxis;
    procedure SetXAxis(const Value: TcaChartAxis);
    procedure SetYAxis(const Value: TcaChartAxis);
  protected
    procedure Paint; override;
  public
    // Properties
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property XAxis: TcaChartAxis read GetXAxis write SetXAxis;
    property YAxis: TcaChartAxis read GetYAxis write SetYAxis;
  end;

 //---------------------------------------------------------------------------
 // IcaChart
 //---------------------------------------------------------------------------

  IcaChart = interface
  ['{D52D84AF-6EC7-43A8-A0A1-2C778FD9CE30}']
    function GetMargin: TcaMargin;
    function GetXAxis: TcaChartAxis;
    function GetYAxis: TcaChartAxis;
    procedure SetMargin(const Value: TcaMargin);
    procedure SetXAxis(const Value: TcaChartAxis);
    procedure SetYAxis(const Value: TcaChartAxis);
    // Properties
    property Margin: TcaMargin read GetMargin write SetMargin;
    property XAxis: TcaChartAxis read GetXAxis write SetXAxis;
    property YAxis: TcaChartAxis read GetYAxis write SetYAxis;
  end;

 //---------------------------------------------------------------------------
 // TcaChart
 //---------------------------------------------------------------------------

  TcaChart = class(TcaPanel, IcaChart)
  private
    FMargin: TcaMargin;
    FGrid: TcaChartGrid;
    FXAxis: TcaChartAxis;
    FYAxis: TcaChartAxis;
    // Property methods
    function GetGrid: TcaChartGrid;
    function GetMargin: TcaMargin;
    function GetXAxis: TcaChartAxis;
    function GetYAxis: TcaChartAxis;
    procedure SetMargin(const Value: TcaMargin);
    procedure SetXAxis(const Value: TcaChartAxis);
    procedure SetYAxis(const Value: TcaChartAxis);
    // Event handlers
    procedure MarginChangedEvent(Sender: TObject);
    procedure AxisChangedEvent(Sender: TObject);
  protected
    procedure Paint; override;
    property Grid: TcaChartGrid read GetGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Margin: TcaMargin read GetMargin write SetMargin;
    property XAxis: TcaChartAxis read GetXAxis write SetXAxis;
    property YAxis: TcaChartAxis read GetYAxis write SetYAxis;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaChartObject
 //---------------------------------------------------------------------------

 // Virtual methods

constructor TcaChartObject.Create(AChart: TcaChart);
begin
  inherited Create;
  FChart := AChart;
end;

procedure TcaChartObject.Paint;
begin
  // Virtual
end;

 //---------------------------------------------------------------------------
 // TcaChartAxis
 //---------------------------------------------------------------------------

constructor TcaChartAxis.Create(AChart: TcaChart; AAxisType: TcaAxisType);
begin
  inherited Create(AChart);
  FAxisType := AAxisType;
end;

 // Protected methods

procedure TcaChartAxis.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TcaChartAxis.Paint;
var
  M: TcaMargin;
  C: TCanvas;
  Grid: TcaChartGrid;
  Range: Double;
  XFrom: Integer;
  XTo: Integer;
  YFrom: Integer;
  YTo: Integer;
  LineValue: Double;
begin
  M := FChart.Margin;
  C := FChart.Canvas;
  Grid := FChart.Grid;
  Range := FMaxValue - FMinValue;
  if (FMinorInterval > 0) and (Round(Range / FMinorInterval) >= 2) then
    begin
      LineValue := FMinorInterval;
      while LineValue < Range do
        begin
          if FAxisType = axX then
            begin
              XFrom := M.Left + Round(Grid.Width * LineValue / Range);
              XTo := XFrom;
              YFrom := M.Top;
              YTo := M.Top + Grid.Height;
            end
          else
            begin
              XFrom := M.Left;
              XTo := M.Left + Grid.Width;
              YFrom := M.Top + Grid.Height - Round(Grid.Height * LineValue / Range);
              YTo := YFrom;
            end;
          C.Pen.Color := clGray;
          C.Pen.Style := psSolid;
          C.MoveTo(XFrom, YFrom);
          C.LineTo(XTo, YTo);
          LineValue := LineValue + FMinorInterval;
        end;
    end;
end;

 // Private methods

procedure TcaChartAxis.Changed;
begin
  DoChanged;
end;

 // Property methods

function TcaChartAxis.GetAxisType: TcaAxisType;
begin
  Result := FAxisType;
end;

function TcaChartAxis.GetMajorInterval: Double;
begin
  Result := FMajorInterval;
end;

function TcaChartAxis.GetMaxValue: Double;
begin
  Result := FMaxValue;
end;

function TcaChartAxis.GetMinorInterval: Double;
begin
  Result := FMinorInterval;
end;

function TcaChartAxis.GetMinValue: Double;
begin
  Result := FMinValue;
end;

function TcaChartAxis.GetOnChanged: TNotifyEvent;
begin
  Result := FOnChanged;
end;

procedure TcaChartAxis.SetMajorInterval(const Value: Double);
begin
  FMajorInterval := Value;
  Changed;
end;

procedure TcaChartAxis.SetMaxValue(const Value: Double);
begin
  FMaxValue := Value;
  Changed;
end;

procedure TcaChartAxis.SetMinorInterval(const Value: Double);
begin
  FMinorInterval := Value;
  Changed;
end;

procedure TcaChartAxis.SetMinValue(const Value: Double);
begin
  FMinValue := Value;
  Changed;  
end;

procedure TcaChartAxis.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

 //---------------------------------------------------------------------------
 // TcaChartGrid
 //---------------------------------------------------------------------------

 // Protected methods

procedure TcaChartGrid.Paint;
var
  M: TcaMargin;
  C: TCanvas;
  R: IcaRect;
begin
  M := FChart.Margin;
  C := FChart.Canvas;
  R := TcaRect.Create(FChart.ClientRect);
  R.Adjust(M.Left, M.Top, -M.Right, -M.Bottom);
  FWidth := R.Right - R.Left;
  FHeight := R.Bottom - R.Top;
  C.Pen.Color := clGray;
  C.Pen.Style := psSolid;
  C.MoveTo(R.Left, R.Top);
  C.LineTo(R.Right, R.Top);
  C.LineTo(R.Right, R.Bottom);
  C.LineTo(R.Left, R.Bottom);
  C.LineTo(R.Left, R.Top);
end;

 // Property methods

function TcaChartGrid.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TcaChartGrid.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TcaChartGrid.GetXAxis: TcaChartAxis;
begin
  Result := FXAxis;
end;

function TcaChartGrid.GetYAxis: TcaChartAxis;
begin
  Result := FYAxis;
end;

procedure TcaChartGrid.SetXAxis(const Value: TcaChartAxis);
begin
  FXAxis := Value;
end;

procedure TcaChartGrid.SetYAxis(const Value: TcaChartAxis);
begin
  FYAxis := Value;
end;

 //---------------------------------------------------------------------------
 // TcaChart
 //---------------------------------------------------------------------------

constructor TcaChart.Create(AOwner: TComponent);
begin
  inherited;
  FMargin := TcaMargin.Create;
  FMargin.OnChanged := MarginChangedEvent;
  FGrid := TcaChartGrid.Create(Self);
  FXAxis := TcaChartAxis.Create(Self, axX);
  FXAxis.OnChanged := AxisChangedEvent;
  FYAxis := TcaChartAxis.Create(Self, axY);
  FYAxis.OnChanged := AxisChangedEvent;
end;

destructor TcaChart.Destroy;
begin
  FMargin.Free;
  FGrid.Free;
  FXAxis.Free;
  FYAxis.Free;
  inherited;
end;

 // Protected methods

procedure TcaChart.Paint;
begin
  inherited;
  FGrid.Paint;
  FXAxis.Paint;
  FYAxis.Paint;
end;

 // Event handlers

procedure TcaChart.AxisChangedEvent(Sender: TObject);
begin
  Invalidate;
end;

procedure TcaChart.MarginChangedEvent(Sender: TObject);
begin
  Invalidate;
end;

 // Property methods

function TcaChart.GetGrid: TcaChartGrid;
begin
  Result := FGrid;
end;

function TcaChart.GetMargin: TcaMargin;
begin
  Result := FMargin;
end;

function TcaChart.GetXAxis: TcaChartAxis;
begin
  Result := FXAxis;
end;

function TcaChart.GetYAxis: TcaChartAxis;
begin
  Result := FYAxis;
end;

procedure TcaChart.SetMargin(const Value: TcaMargin);
begin
  FMargin := Value;
end;

procedure TcaChart.SetXAxis(const Value: TcaChartAxis);
begin
  FXAxis := Value;
end;

procedure TcaChart.SetYAxis(const Value: TcaChartAxis);
begin
  FYAxis := Value;
end;

end.
