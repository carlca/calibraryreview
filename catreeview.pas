unit caTreeView;

{$INCLUDE ca.inc}

interface

uses

  // standard Delphi units...
  SysUtils,
  Classes,
  Windows,
  Messages,
  Graphics,
  Controls,
  ComCtrls,
  CommCtrl,
  ExtCtrls,

  // ca units...
  caClasses,
  caUtils,
  caLog,
  caNodes;

type

  //---------------------------------------------------------------------------
  // TcaTreeNode
  //---------------------------------------------------------------------------

  TcaTreeNode = class(TTreeNode)
  private
    // private members...
    FBold: Boolean;
    FPartChecked: Boolean;
    FFont: TFont;
    FEnabled: Boolean;
    // property methods...
    function GetChecked: Boolean;
    procedure SetBold(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetPartChecked(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetEnabled(const Value: Boolean);
  public
    // lifetime...
    constructor Create(AOwner: TTreeNodes);
    destructor Destroy; override;
    // public properties...
    property Bold: Boolean read FBold write SetBold;
    property Checked: Boolean read GetChecked write SetChecked;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Font: TFont read FFont write SetFont;
    property PartChecked: Boolean read FPartChecked write SetPartChecked;
  end;

  //---------------------------------------------------------------------------
  // TcaTreeView
  //---------------------------------------------------------------------------

  TcaTreeViewNodeAddedEvent = procedure(Sender: TObject; ATreeNode: TcaTreeNode; ANode: TcaNode) of object;

  TcaTreeViewCheckBoxClickEvent = procedure(Sender: TObject; ATreeNode: TcaTreeNode) of object;

  TcaTreeViewNodeHoverEvent = TcaTreeViewCheckBoxClickEvent;


  TcaTreeView = class(TTreeView)
  private
    // private members...
    FNodes: IcaNodes;
    FHoverTimer: TTimer;
    FMousePos: TPoint;
    FPrevMousePos: TPoint;
    // property fields...
    FUseCheckBoxes: Boolean;
    FUseLinkedChecking: Boolean;
    FUserChange: Boolean;
    FUpdatingNodes: Boolean;
    // event property fields...
    FOnNodeAdded: TcaTreeViewNodeAddedEvent;
    FOnCheckBoxClicked: TcaTreeViewCheckBoxClickEvent;
    FUseNodeHoverEvents: Boolean;
    FOnNodeHover: TcaTreeViewNodeHoverEvent;
    // private methods...
    procedure AddTreeNode(ANode: TcaNode; ATreeNode: TcaTreeNode);
    procedure UpdateChildrenCheckState(ATreeNode: TcaTreeNode);
    procedure UpdateParentsCheckState(ATreeNode: TcaTreeNode);
    procedure UpdateNodes;
    // property methods...
    function GetItem(Index: Integer): TcaTreeNode;
    procedure SetUseCheckBoxes(const Value: Boolean);
    procedure SetUseLinkedChecking(const Value: Boolean);
    procedure SetUseNodeHoverEvents(const Value: Boolean);
    // windows message handlers...
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    // event handler - THIS IS A HACK because there seems to be no way to
    //                 use the internal virtual methods...
    procedure CustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure AdvancedCustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    // hover timer event
    procedure HoverTimerEvent(Sender: TObject);
  protected
    // protected virtual methods...
    function CreateNode: TTreeNode; override;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; override;
    procedure Added(Node: TTreeNode); {$IFDEF D7_UP}override{$ELSE}virtual{$ENDIF};
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoAddNode(ANode: TcaNode; var AAccept: Boolean); virtual;
    procedure DoNodeAdded(ATreeNode: TcaTreeNode; ANode: TcaNode); virtual;
    procedure DoCheckBoxClicked; virtual;
    procedure DoNodeHover(ATreeNode: TcaTreeNode); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // protected static methods...
    function GetNodeCheckedState(ATreeNode: TcaTreeNode): Boolean;
    procedure RepaintNode(ATreeNode: TcaTreeNode);
    procedure SetNodeBoldState(ATreeNode: TcaTreeNode; IsBold: Boolean);
    procedure SetNodeCheckedState(ATreeNode: TcaTreeNode; IsChecked: Boolean);
    procedure UpdateNodeCheckLinkStates(ATreeNode: TcaTreeNode);
    // protected properties...
    property UpdatingNodes: Boolean read FUpdatingNodes write FUpdatingNodes;
    property UserChange: Boolean read FUserChange;
  public
    // lifetime...
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // public methods...
    procedure SetNodes(const Value: IcaNodes);
    // public properties...
    property Item[Index: Integer]: TcaTreeNode read GetItem;
    property Items;
  published
    // published properties...
    property UseCheckBoxes: Boolean read FUseCheckBoxes write SetUseCheckBoxes;
    property UseLinkedChecking: Boolean read FUseLinkedChecking write SetUseLinkedChecking;
    property UseNodeHoverEvents: Boolean read FUseNodeHoverEvents write SetUseNodeHoverEvents;
    // event properties...
    property OnNodeAdded: TcaTreeViewNodeAddedEvent read FOnNodeAdded write FOnNodeAdded;
    property OnCheckBoxClicked: TcaTreeViewCheckBoxClickEvent read FOnCheckBoxClicked write FOnCheckBoxClicked;
    property OnNodeHover: TcaTreeViewNodeHoverEvent read FOnNodeHover write FOnNodeHover;
  end;

implementation

//---------------------------------------------------------------------------
// TcaTreeNode;
//---------------------------------------------------------------------------

// lifetime...

constructor TcaTreeNode.Create(AOwner: TTreeNodes);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
end;

destructor TcaTreeNode.Destroy;
begin
  FFont.Free;
  inherited;
end;

// property methods...

function TcaTreeNode.GetChecked: Boolean;
begin
  Result := TcaTreeView(TreeView).GetNodeCheckedState(Self);
end;

procedure TcaTreeNode.SetBold(const Value: Boolean);
begin
  if Value <> FBold then
    begin
      FBold := Value;
      TcaTreeView(TreeView).SetNodeBoldState(Self, FBold);
    end;
end;

procedure TcaTreeNode.SetChecked(const Value: Boolean);
begin
  if Value <> GetChecked then
    begin
      TcaTreeView(TreeView).SetNodeCheckedState(Self, Value);
      if not TcaTreeView(TreeView).UpdatingNodes then
        begin
          TcaTreeView(TreeView).UpdatingNodes := True;
          try
            if not TcaTreeView(TreeView).UserChange then
              TcaTreeView(TreeView).UpdateNodeCheckLinkStates(Self);
          finally
            TcaTreeView(TreeView).UpdatingNodes := False;
          end;
        end;
      if Assigned(Data) then
        TcaNode(Data).Checked := Value;
    end;
end;

procedure TcaTreeNode.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
    begin
      FEnabled := Value;
      TcaTreeView(TreeView).RepaintNode(Self);
    end;
end;

procedure TcaTreeNode.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TcaTreeNode.SetPartChecked(const Value: Boolean);
begin
  if Value <> FPartChecked then
    begin
      FPartChecked := Value;
      TcaTreeView(TreeView).RepaintNode(Self);
    end;
end;

//---------------------------------------------------------------------------
// TcaTreeView                                                               
//---------------------------------------------------------------------------

// lifetime...

constructor TcaTreeView.Create(AOwner: TComponent);
begin
  inherited;
  // HACK - there seems to be no way to use the internal virtual methods...
  Self.OnCustomDrawItem := CustomDrawItemEvent;
  Self.OnAdvancedCustomDrawItem := AdvancedCustomDrawItemEvent;
  FHoverTimer := TTimer.Create(nil);
  FHoverTimer.Interval := 200;
  FHoverTimer.Enabled := False;
end;

destructor TcaTreeView.Destroy;
begin
  FHoverTimer.Free;
  inherited;
end;

// public methods...

procedure TcaTreeView.SetNodes(const Value: IcaNodes);
begin
  FNodes := Value;
  UpdateNodes;
end;

// protected virtual methods...

function TcaTreeView.CreateNode: TTreeNode;
begin
  Result := TcaTreeNode.Create(Items);
  TcaTreeNode(Result).Enabled := True;
end;

function TcaTreeView.IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
begin
  Result := inherited IsCustomDrawn(Target, Stage);
end;

procedure TcaTreeView.Added(Node: TTreeNode);
var
  ANode: TcaNode;
  ParentNode: TcaNode;
  ParentTreeNode: TTreeNode;
begin
  inherited;
  if Assigned(FNodes) then
    begin
      ANode := nil;
      if not FUpdatingNodes then
        begin
          ParentTreeNode := Node.Parent;
          if Assigned(ParentTreeNode) then
            begin
              ParentNode := TcaNode(ParentTreeNode.Data);
              if Assigned(ParentNode) then
                ANode := FNodes.AddSub(ParentNode, Node.Text);
            end
          else
            ANode := FNodes.AddRoot(Node.Text);
          if Assigned(ANode) then
            Node.Data := ANode;
        end;
    end;
end;

procedure TcaTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FUseCheckBoxes then
    Params.Style := Params.Style or TVS_CHECKBOXES;
end;

procedure TcaTreeView.DoAddNode(ANode: TcaNode; var AAccept: Boolean);
begin
  AAccept := True;
end;

procedure TcaTreeView.DoNodeAdded(ATreeNode: TcaTreeNode; ANode: TcaNode);
begin
  if Assigned(FOnNodeAdded) then FOnNodeAdded(Self, ATreeNode, ANode);
end;

procedure TcaTreeView.DoCheckBoxClicked;
begin
  if Assigned(FOnCheckBoxClicked) then
    FOnCheckBoxClicked(Self, TcaTreeNode(Selected));
end;

procedure TcaTreeView.DoNodeHover(ATreeNode: TcaTreeNode);
begin
  if Assigned(FOnNodeHover) then
    FOnNodeHover(Self, ATreeNode);
end;

procedure TcaTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMousePos.X := X;
  FMousePos.Y := Y;
end;

// windows message handlers...

procedure TcaTreeView.WMKeyDown(var Message: TWMKeyDown);
var
  WasChecked: Boolean;
begin
  WasChecked := TcaTreeNode(Selected).Checked;
  inherited;
  if Message.CharCode = $20 then
    begin
      if TcaTreeNode(Selected).Enabled then
        begin
          if FUseLinkedChecking then
            begin
              FUserChange := True;
              UpdateNodeCheckLinkStates(TcaTreeNode(Selected));
              FUserChange := False;
            end;
          DoCheckBoxClicked;
        end
      else
        TcaTreeNode(Selected).Checked := WasChecked;
      Message.Result := 0;
    end;
end;

procedure TcaTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  Node: TcaTreeNode;
  WasChecked: Boolean;
begin
  Node := TcaTreeNode(GetNodeAt(Message.XPos, Message.YPos));
  if Assigned(Node) then
    begin
      WasChecked := Node.Checked;
      inherited;
      if htOnStateIcon in GetHitTestInfoAt(Message.XPos, Message.YPos) then
        begin
          if TcaTreeNode(Selected).Enabled then
            begin
              Node.Focused := True;
              Node.Selected := True;
              if Node.Checked then
                Node.PartChecked := False;
              if FUseLinkedChecking then
                begin
                  FUserChange := True;
                  UpdateNodeCheckLinkStates(TcaTreeNode(Selected));
                  FUserChange := False;
                end;
              DoCheckBoxClicked;
            end
          else
            Node.Checked := WasChecked;
        end;
    end;
end;

// event handlers - THIS IS A HACK because there seems to be no way to
//                  use the internal virtual methods...

procedure TcaTreeView.CustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if UseCheckBoxes then
    begin
      Canvas.Font.Assign(TcaTreeNode(Node).Font);
      if cdsSelected in State then
        begin
          if TcaTreeNode(Node).Enabled then
            begin
              Canvas.Brush.Color := clHighlight;
              Canvas.Font.Color := clHighlightText;
            end
          else
            begin
              Canvas.Brush.Color := clBtnShadow;
              Canvas.Font.Color := clBtnFace;
            end;
        end
      else
        begin
          if not TcaTreeNode(Node).Enabled then
            begin
              Canvas.Brush.Color := clWindow;
              Canvas.Font.Color := clBtnShadow;
            end;
        end;
    end
  else
    DefaultDraw := True;
end;

procedure TcaTreeView.AdvancedCustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  R: TRect;

  procedure DrawUncheckedBox;
  begin
    R := Node.DisplayRect(False);
    R.Left := R.Left + (Node.Level * Indent) + 26;
    R.Right := R.Left + 9;
    R.Top := R.Top + 4;
    R.Bottom := R.Top + 9;
    if TcaTreeNode(Node).PartChecked then
      begin
        if TcaTreeNode(Node).Enabled then
          Canvas.Brush.Color := clGray
        else
          Canvas.Brush.Color := clSilver;
      end
    else
      Canvas.Brush.Color := clWhite;
    Canvas.FillRect(R);
    DefaultDraw := True;
  end;

begin
  if UseCheckBoxes then
    begin
      if (Stage = cdPostPaint) then
        begin
          if not TcaTreeNode(Node).Checked then
            DrawUncheckedBox;
        end;
    end    
  else
    DefaultDraw := True;
end;

// hover timer event

procedure TcaTreeView.HoverTimerEvent(Sender: TObject);
var
  TreeNode: TcaTreeNode;
begin
  if (FMousePos.X = FPrevMousePos.X) and (FMousePos.Y = FPrevMousePos.Y) then
    begin
      TreeNode := TcaTreeNode(GetNodeAt(FMousePos.X, FMousePos.Y));
      if Assigned(TreeNode) then
        DoNodeHover(TreeNode);
    end;
  FPrevMousePos := FMousePos;
end;

// protected static methods...

function TcaTreeView.GetNodeCheckedState(ATreeNode: TcaTreeNode): Boolean;
var
  tvi: TTVItem;
begin
  Result := False;
  if Assigned(ATreeNode) then
    begin
      FillChar(tvi, SizeOf(tvi), 0);
      tvi.hItem := ATreeNode.ItemID;
      tvi.mask := TVIF_HANDLE or TVIF_STATE;
      tvi.stateMask := TVIS_STATEIMAGEMASK;
      TreeView_GetItem(ATreeNode.Handle, tvi);
      Result := Pred(tvi.state shr 12) > 0;
    end;
end;

procedure TcaTreeView.RepaintNode(ATreeNode: TcaTreeNode);
var
  R: TRect;
begin
  while (ATreeNode <> nil) and not ATreeNode.IsVisible do
    ATreeNode := TcaTreeNode(ATreeNode.Parent);
  if ATreeNode <> nil then
    begin
      R := ATreeNode.DisplayRect(False);
      InvalidateRect(Handle, @R, True);
    end;
end;

procedure TcaTreeView.SetNodeBoldState(ATreeNode: TcaTreeNode; IsBold: Boolean);
var
  tvi: TTVItem;
begin
  if Assigned(ATreeNode) then
    begin
      FillChar(tvi, SizeOf(tvi), 0);
      tvi.hItem := ATreeNode.ItemID;
      tvi.mask := TVIF_STATE;
      tvi.stateMask := TVIS_BOLD;
      tvi.state := TVIS_BOLD * Ord(IsBold);
      TreeView_SetItem(ATreeNode.Handle, tvi);
    end;
end;

procedure TcaTreeView.SetNodeCheckedState(ATreeNode: TcaTreeNode; IsChecked: Boolean);
var
  tvi: TTVItem;
const
  StateIndexes: array[Boolean] of Integer = (1, 2);
begin
  if Assigned(ATreeNode) then
    begin
      FillChar(tvi, SizeOf(tvi), 0);
      tvi.hItem := ATreeNode.ItemID;
      tvi.mask := TVIF_HANDLE or TVIF_STATE;
      tvi.stateMask := TVIS_STATEIMAGEMASK;
      tvi.state := IndexToStateImageMask(StateIndexes[IsChecked]);
      TreeView_SetItem(ATreeNode.Handle, tvi);
    end;
end;

procedure TcaTreeView.UpdateNodeCheckLinkStates(ATreeNode: TcaTreeNode);
begin
  Items.BeginUpdate;
  try
    if Assigned(ATreeNode) then
      begin
        UpdateChildrenCheckState(ATreeNode);
        UpdateParentsCheckState(ATreeNode);
      end;
  finally
    Items.EndUpdate;
  end;
end;

// private methods...

procedure TcaTreeView.AddTreeNode(ANode: TcaNode; ATreeNode: TcaTreeNode);
var
  TreeNode: TTreeNode;
  SubNode: TcaNode;
  ShouldAddNode: Boolean;
begin
  if not Assigned(ANode) then
    if FNodes.RootCount > 0 then
      ANode := FNodes.Roots[0];
  if Assigned(ANode) then
    begin
      TreeNode := Items.AddChild(ATreeNode, ANode.Text);
      TreeNode.Data := ANode;
      TreeNode.ImageIndex := 0;
      TreeNode.SelectedIndex := 0;
      DoNodeAdded(TcaTreeNode(TreeNode), ANode);
      SubNode := ANode.FirstSub;
      while Assigned(SubNode) do
        begin
          DoAddNode(SubNode, ShouldAddNode);
          if ShouldAddNode then
            AddTreeNode(SubNode, TcaTreeNode(TreeNode));
          SubNode := SubNode.NextIso;
        end;
    end;
end;

procedure TcaTreeView.UpdateChildrenCheckState(ATreeNode: TcaTreeNode);
var
  SubTreeNode: TcaTreeNode;
begin
  SubTreeNode := TcaTreeNode(ATreeNode.getFirstChild);
  while Assigned(SubTreeNode) and SubTreeNode.HasAsParent(ATreeNode) do
    begin
      SubTreeNode.Checked := ATreeNode.Checked;
      SubTreeNode := TcaTreeNode(SubTreeNode.GetNext);
    end;
end;

procedure TcaTreeView.UpdateParentsCheckState(ATreeNode: TcaTreeNode);
var
  ParentTreeNode: TcaTreeNode;
  SubTreeNode: TcaTreeNode;
  ChildCount: Integer;
  CheckedCount: Integer;
  PartCheckedCount: Integer;
begin
  ParentTreeNode := TcaTreeNode(ATreeNode.Parent);
  while Assigned(ParentTreeNode) do
    begin
      ChildCount := 0;
      CheckedCount := 0;
      PartCheckedCount := 0;
      SubTreeNode := TcaTreeNode(ParentTreeNode.getFirstChild);
      while Assigned(SubTreeNode) do
        begin
          if SubTreeNode.Parent = ParentTreeNode then
            begin
              Inc(ChildCount);
              if SubTreeNode.Checked then
                Inc(CheckedCount);
              if SubTreeNode.PartChecked then
                Inc(PartCheckedCount);
            end;
          SubTreeNode := TcaTreeNode(ParentTreeNode.GetNextChild(SubTreeNode));
        end;
      if ChildCount = 0 then
        begin
          ParentTreeNode.Checked := False;
          ParentTreeNode.PartChecked := False;
        end
      else
        begin
          if CheckedCount = ChildCount then
            begin
              ParentTreeNode.Checked := True;
              ParentTreeNode.PartChecked := False;
            end
          else
            begin
              if (CheckedCount > 0) or (PartCheckedCount > 0) then
                begin
                  ParentTreeNode.Checked := False;
                  ParentTreeNode.PartChecked := True;
                end
              else
                begin
                  ParentTreeNode.Checked := False;
                  ParentTreeNode.PartChecked := False;
                end;
            end;
        end;
      ParentTreeNode := TcaTreeNode(ParentTreeNode.Parent);
    end;  
end;

procedure TcaTreeView.UpdateNodes;
var
  Index: Integer;
  RootNode: TcaNode;
  ShouldAddNode: Boolean;
begin
  if Assigned(FNodes) then
    begin
      Items.BeginUpdate;
      FUpdatingNodes := True;
      try
        Items.Clear;
        for Index := 0 to Pred(FNodes.RootCount) do
          begin
            RootNode := FNodes.Roots[Index];
            DoAddNode(RootNode, ShouldAddNode);
            if ShouldAddNode then
              AddTreeNode(RootNode, nil);
          end;
      finally
        Items.EndUpdate;
        FUpdatingNodes := False;
      end;
    end;
end;

// property methods...

function TcaTreeView.GetItem(Index: Integer): TcaTreeNode;
begin
  Result := TcaTreeNode(Items[Index]);
end;

procedure TcaTreeView.SetUseCheckBoxes(const Value: Boolean);
begin
  if Value <> FUseCheckBoxes then
    begin
      FUseCheckBoxes := Value;
      // need to force the CreateParams call...
      if not (csDesigning in ComponentState) then
        RecreateWnd;
    end;
end;

procedure TcaTreeView.SetUseLinkedChecking(const Value: Boolean);
begin
  if Value <> FUseLinkedChecking then
    begin
      FUseLinkedChecking := Value;
      if FUseLinkedChecking and (not (csDesigning in ComponentState)) then
        UpdateNodeCheckLinkStates(TcaTreeNode(Selected));
    end;
end;

procedure TcaTreeView.SetUseNodeHoverEvents(const Value: Boolean);
begin
  if Value <> FUseNodeHoverEvents then
    begin
      FUseNodeHoverEvents := Value;
      FHoverTimer.OnTimer := HoverTimerEvent;
      FHoverTimer.Enabled := FUseNodeHoverEvents;
    end;  
end;

end.
