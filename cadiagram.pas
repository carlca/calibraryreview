unit caDiagram;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Menus,
  Contnrs,

  // ca units 
  caClasses,
  caUtils,
  caControls,
  caSizeMovePanel;

type

  TcaDiagramEdge = (deLeft, deRight, deTop, deBottom);

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLink                                                            
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagramElement = class;

  TcaDiagramLink = class(TCollectionItem)
  private
    // Property fields 
    FLinkFrom: TcaDiagramElement;
    FLinkTo: TcaDiagramElement;
  protected
    // Protected methods 
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    // Create/Destroy 
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    // Public methods 
    procedure Assign(Source: TPersistent); override;
  published
    // Properties 
    property LinkFrom: TcaDiagramElement read FLinkFrom write FLinkFrom;
    property LinkTo: TcaDiagramElement read FLinkTo write FLinkTo;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLinks                                                           
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagramLinks = class(TOwnedCollection)
  private
    // Property methods
    function GetItem(Index: Integer): TcaDiagramLink;
    procedure SetItem(Index: Integer; Value: TcaDiagramLink);
  protected
    // Protected methods
    procedure Update(Item: TCollectionItem); override;
  public
    // Public methods
    function Add: TcaDiagramLink;
    // Properties
    property Items[Index: Integer]: TcaDiagramLink read GetItem write SetItem; default;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramElement                                                         
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagramElement = class(TcaSizeMovePanel)
  private
    // Property fields 
    FCenterPoint: TPoint;
    FOldLeft: Integer;
    FOldTop: Integer;
    // Private methods 
    procedure UpdateCenterPoint;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    // Protected methods 
    procedure Moved; virtual;
    // Protected properties 
    property OldLeft: Integer read FOldLeft;
    property OldTop: Integer read FOldTop;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Properties 
    property CenterPoint: TPoint read FCenterPoint;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLineNode                                                        
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagramBox = class;

  TcaDiagramLineNode = class(TcaDiagramElement)
  private
    // Property fields 
    FDiagramBox: TcaDiagramBox;
    FLinkedNode: TcaDiagramLineNode;
    // Windows message handlers 
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    // Properties 
    property DiagramBox: TcaDiagramBox read FDiagramBox write FDiagramBox;
    property LinkedNode: TcaDiagramLineNode read FLinkedNode write FLinkedNode;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLineNodeList                                                    
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagram = class;

  TcaDiagramLineNodeList = class(TObject)
  private
    // Private fields 
    FDiagram: TcaDiagram;
    FList: TObjectList;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaDiagramLineNode;
  public
    // Create/Destroy 
    constructor Create(ADiagram: TcaDiagram);
    destructor Destroy; override;
    // Public methods 
    function Add(ADiagramBox: TcaDiagramBox; ALeft, ATop: Integer): TcaDiagramLineNode;
    function IndexOf(AItem: TcaDiagramLineNode): Integer;
    procedure Clear;
    // Properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaDiagramLineNode read GetItem; default;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramBox                                                             
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagramBox = class(TcaDiagramElement)
  private
    // Private fields 
    FDiagram: TcaDiagram;
    // Property fields 
    FNodes: TcaDiagramLineNodeList;
    // Private methods 
    function GetNearestEdge(ANodeX, ANodeY: Integer): TcaDiagramEdge;
    procedure MoveNodes;
  protected
    // Protected methods 
    function MakeEdgePoint: TPoint;
    procedure DblClick; override;
    procedure Moved; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Properties 
    property Nodes: TcaDiagramLineNodeList read FNodes write FNodes;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramController                                                      
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagramController = class(TComponent)
  private
    // Property fields 
    FDiagram: TcaDiagram;
    FFormCanvas: TCanvas;
    FLinks: TcaDiagramLinks;
    FNodeFrom: TcaDiagramLineNode;
    FNodeTo: TcaDiagramLineNode;
    // Property methods 
    procedure SetDiagram(const Value: TcaDiagram);
    procedure SetLinks(const Value: TcaDiagramLinks);
    // Private methods 
    procedure LinkNodes;
    procedure ResetNodePoints;
  protected
    // Protected methods 
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure AddNode(ANode: TcaDiagramLineNode);
  published
    // Published properties 
    property Diagram: TcaDiagram read FDiagram write SetDiagram;
    property Links: TcaDiagramLinks read FLinks write SetLinks;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagram                                                                
  //```````````````````````````````````````````````````````````````````````````

  TcaDiagram = class(TCustomPanel)
  private
    // Property fields 
    FController: TcaDiagramController;
    // Property methods 
    procedure SetController(const Value: TcaDiagramController);
    // Private methods 
    procedure PaintDiagram;
  protected
    // Protected methods 
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Properties 
    property Controller: TcaDiagramController read FController write SetController;
  end;

implementation

uses Types;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLink                                                            
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDiagramLink.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TcaDiagramLink.Destroy;
begin
  inherited;
end;

  // Public methods 

procedure TcaDiagramLink.Assign(Source: TPersistent);
var
  SourceLink: TcaDiagramLink;
begin
  if Source is TcaDiagramLink then
    begin
      SourceLink := TcaDiagramLink(Source);
      Utils.ShallowCopy(SourceLink, Self);
    end
  else
    inherited;
end;

  // Property methods 

function TcaDiagramLink.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
end;

procedure TcaDiagramLink.SetDisplayName(const Value: string);
begin
  inherited;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLinks                                                           
  //```````````````````````````````````````````````````````````````````````````

  // Public methods 

function TcaDiagramLinks.Add: TcaDiagramLink;
begin
  Result := TcaDiagramLink(inherited Add);
end;

  // Protected methods 

procedure TcaDiagramLinks.Update(Item: TCollectionItem);
begin
  inherited;
end;

  // Property methods 

function TcaDiagramLinks.GetItem(Index: Integer): TcaDiagramLink;
begin
  Result := TcaDiagramLink(inherited GetItem(Index));
end;

procedure TcaDiagramLinks.SetItem(Index: Integer; Value: TcaDiagramLink);
begin
  inherited SetItem(Index, Value);
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramElement                                                         
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDiagramElement.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TcaDiagramElement.Destroy;
begin
  inherited;
end;

  // Protected methods 

procedure TcaDiagramElement.Moved;
begin
  UpdateCenterPoint;
  if Parent <> nil then
    Parent.Invalidate;
end;

  // Private methods 

procedure TcaDiagramElement.UpdateCenterPoint;
begin
  FCenterPoint := Point(Left, Top);
  FCenterPoint.X := FCenterPoint.X + (Width div 2);
  FCenterPoint.Y := FCenterPoint.Y + (Height div 2);
end;

  // Windows messgae handlers 

procedure TcaDiagramElement.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  FOldLeft := Left;
  FOldTop := Top;
end;

procedure TcaDiagramElement.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Moved;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLineNode                                                        
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDiagramLineNode.Create(AOwner: TComponent);
begin
  inherited;
  Width := 3;
  Height := 3;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Color := clRed;
  Cursor := crSizeAll;
  Options := [smMoving];
  Frame.Sides := [];
end;

  // Windows message handlers 

procedure TcaDiagramLineNode.WMExitSizeMove(var Msg: TMessage);
var
  EdgePoint: TPoint;
begin
  inherited;
  if Assigned(FDiagramBox) then
    begin
      if  (Left >= (FDiagramBox.Left - 4))
      and (Left <= (FDiagramBox.Left + FDiagramBox.Width))
      and (Top >= (FDiagramBox.Top - 4))
      and (Top <= (FDiagramBox.Top + FDiagramBox.Height)) then
        begin
          EdgePoint := DiagramBox.MakeEdgePoint;
          if Abs(EdgePoint.X - FLinkedNode.Left) < 8 then
            EdgePoint.X := FLinkedNode.Left;
          if Abs(EdgePoint.Y - FLinkedNode.Top) < 8 then
            EdgePoint.Y := FLinkedNode.Top;
          Left := EdgePoint.X;
          Top := EdgePoint.Y;
        end;
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramLineNodeList                                                    
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDiagramLineNodeList.Create(ADiagram: TcaDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FList := TObjectList.Create(False);
end;

destructor TcaDiagramLineNodeList.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Public methods 

function TcaDiagramLineNodeList.Add(ADiagramBox: TcaDiagramBox; ALeft, ATop: Integer): TcaDiagramLineNode;
begin
  Result := TcaDiagramLineNode.Create(nil);
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Parent := FDiagram;
  Result.DiagramBox := ADiagramBox;
  FList.Add(Result);
end;

function TcaDiagramLineNodeList.IndexOf(AItem: TcaDiagramLineNode): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TcaDiagramLineNodeList.Clear;
begin
  FList.Clear;
end;

  // Property methods 

function TcaDiagramLineNodeList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaDiagramLineNodeList.GetItem(Index: Integer): TcaDiagramLineNode;
begin
  Result := TcaDiagramLineNode(FList[Index]);
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramBox                                                             
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDiagramBox.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  SizingPixels := 6;
  Color := clCream;
end;

destructor TcaDiagramBox.Destroy;
begin
  FNodes.Free;
  inherited;
end;

  // Protected methods 

function TcaDiagramBox.MakeEdgePoint: TPoint;
var
  CursorPos: TPoint;
  NearestEdge: TcaDiagramEdge;
  NodeX: Integer;
  NodeY: Integer;
begin
  CursorPos := ScreenToClient(Mouse.CursorPos);
  NodeX := CursorPos.X + Left;
  NodeY := CursorPos.Y + Top;
  NearestEdge := GetNearestEdge(CursorPos.X, CursorPos.Y);
  case NearestEdge of
    deLeft:     NodeX := Left - 3;
    deRight:    NodeX := Left + Width;
    deTop:      NodeY := Top - 3;
    deBottom:   NodeY := Top + Height;
  end;
  Result.X := NodeX;
  Result.Y := NodeY;
end;

procedure TcaDiagramBox.DblClick;
var
  EdgePoint: TPoint;
  Node: TcaDiagramLineNode;
begin
  inherited;
  EdgePoint := MakeEdgePoint;
  Node := FNodes.Add(Self, EdgePoint.X, EdgePoint.Y);
  Node.BringToFront;
  if FDiagram.Controller <> nil then
    FDiagram.Controller.AddNode(Node);
end;

procedure TcaDiagramBox.Moved;
begin
  inherited;
  MoveNodes;
end;

procedure TcaDiagramBox.SetParent(AParent: TWinControl);
begin
  inherited;
  FDiagram := nil;
  if AParent is TcaDiagram then
    FNodes := TcaDiagramLineNodeList.Create(TcaDiagram(AParent));
  FDiagram := TcaDiagram(AParent);
end;

  // Private methods 

function TcaDiagramBox.GetNearestEdge(ANodeX, ANodeY: Integer): TcaDiagramEdge;
var
  LeftRight: TcaDiagramEdge;
  TopBottom: TcaDiagramEdge;
begin
  LeftRight := deLeft;
  if ANodeX > (Width div 2 + 1) then LeftRight := deRight;
  TopBottom := deTop;
  if ANodeY > (Height div 2 + 1) then TopBottom := deBottom;
  if LeftRight = deLeft then
    begin
      if TopBottom = deTop then
        begin
          if ANodeY < ANodeX then
            Result := deTop
          else
            Result := deLeft;
        end
      else
        begin
          if (Height - ANodeY) < ANodeX then
            Result := deBottom
          else
            Result := deLeft;
        end;
    end
  else
    begin
      if TopBottom = deTop then
        begin
          if ANodeY < (Width - ANodeX) then
            Result := deTop
          else
            Result := deRight;
        end
      else
        begin
          if (Height - ANodeY) < (Width - ANodeX) then
            Result := deBottom
          else
            Result := deRight;
        end;
    end;
end;

procedure TcaDiagramBox.MoveNodes;
var
  DeltaX: Integer;
  DeltaY: Integer;
  Index: Integer;
  Node: TcaDiagramLineNode;
begin
  DeltaX := Left - OldLeft;
  DeltaY := Top - FOldTop;
  for Index := 0 to Pred(FNodes.Count) do
    begin
      Node := FNodes[Index];
      Node.Left := Node.Left + DeltaX;
      Node.Top := Node.Top + DeltaY;
      Node.BringToFront;
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagramController                                                      
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDiagramController.Create(AOwner: TComponent);
begin
  inherited;
  FFormCanvas := TCanvas.Create;
  FLinks := TcaDiagramLinks.Create(Self, TcaDiagramLink);
  ResetNodePoints;
end;

destructor TcaDiagramController.Destroy;
begin
  FFormCanvas.Free;
  FLinks.Free;
  inherited;
end;

  // Public methods 

procedure TcaDiagramController.AddNode(ANode: TcaDiagramLineNode);
begin
  if not Assigned(FNodeFrom) then
    FNodeFrom := ANode
  else
    begin
      FNodeTo := ANode;
      LinkNodes;
    end;
end;

  // Protected methods 

procedure TcaDiagramController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDiagram) then
    FDiagram := nil;
end;

  // Private methods 

procedure TcaDiagramController.LinkNodes;
var
  Link: TcaDiagramLink;
begin
  Link := FLinks.Add;
  Link.LinkFrom := FNodeFrom;
  Link.LinkTo := FNodeTo;
  FNodeFrom.LinkedNode := FNodeTo;
  FNodeTo.LinkedNode := FNodeFrom;
  ResetNodePoints;
  FDiagram.Invalidate;
end;

procedure TcaDiagramController.ResetNodePoints;
begin
  FNodeFrom := nil;
  FNodeTo := nil;
end;

  // Property methods 

procedure TcaDiagramController.SetDiagram(const Value: TcaDiagram);
begin
  if Value <> FDiagram then
    begin
      FDiagram := Value;
      if Assigned(FDiagram) then
        FDiagram.Controller := Self;
    end;
end;

procedure TcaDiagramController.SetLinks(const Value: TcaDiagramLinks);
begin
  FLinks.Assign(Value);
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDiagram                                                                
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDiagram.Create(AOwner: TComponent);
begin
  inherited;
  Caption := '';
  DoubleBuffered := True;
end;

destructor TcaDiagram.Destroy;
begin
  inherited;
end;

  // Protected methods 

procedure TcaDiagram.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FController) then
    FController := nil;
end;

procedure TcaDiagram.Paint;
begin
  inherited;
  if Assigned(FController) then
    PaintDiagram;
end;

  // Private methods 

procedure TcaDiagram.PaintDiagram;
var
  ElementFrom: TcaDiagramElement;
  ElementTo: TcaDiagramElement;
  Index: Integer;
  Link: TcaDiagramLink;
begin
  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  for Index := 0 to Pred(FController.Links.Count) do
    begin
      Link := TcaDiagramLink(FController.Links[Index]);
      ElementFrom := Link.LinkFrom;
      ElementTo := Link.LinkTo;
      Canvas.MoveTo(ElementFrom.CenterPoint.X, ElementFrom.CenterPoint.Y);
      Canvas.LineTo(ElementTo.CenterPoint.X, ElementTo.CenterPoint.Y);
    end;
end;

  // Property methods 

procedure TcaDiagram.SetController(const Value: TcaDiagramController);
begin
  if Value <> FController then
    begin
      FController := Value;
      if Assigned(FController) then
        FController.Diagram := Self;
    end;
end;

end.
