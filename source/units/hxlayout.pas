unit hxLayout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  Controls, ExtCtrls,
  hxGlobal;

type
  TLayoutPosition = (lpLeft, lpRight, lpBottom, lpCenter);

  TSplitterController = class
  private
    FParent: TWinControl;
    FPosition: TViewerPosition;
    FControls: TFPList;
    FSplitters: TFPList;
    FControlSize: array of Integer;
    function GetControlCount: Integer;
  protected
    procedure UpdateLayout;
  public
    constructor Create(AParent: TWinControl; APosition: TViewerPosition);
    destructor Destroy; override;
    procedure AddControl(AControl: TControl);
    function Contains(AControl: TControl): Boolean;
    procedure RemoveControl(AControl: TControl);
    procedure LoadFromIni(ini: TCustomIniFile);
    procedure SaveToIni(ini: TCustomIniFile);
    property ControlCount: Integer read GetControlCount;
  end;

  TLayout = class
  private
    FParent: TWinControl;
    FPanels: array[TLayoutPosition] of TPanel;
    FSplitters: array[TViewerPosition] of TSplitter;
    FSplitterControllers: array[TViewerPosition] of TSplitterController;
  protected
    function ViewerToLayoutPosition(AViewerPosition: TViewerPosition): TLayoutPosition;
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    procedure AddCenterControl(AControl: TControl);
    function GetViewerPosition(AControl: TControl): Integer;
    procedure ShowControl(AControl: TControl; APosition: TViewerPosition);
    procedure ShowPanel(APosition: TViewerPosition; Enable: Boolean);
    procedure HideControl(AControl: TControl);

    procedure LoadFromIni(ini: TCustomIniFile);
    procedure SaveToIni(ini: TCustomIniFile);
  end;

implementation

{ TSplitterController }

constructor TSplitterController.Create(AParent: TWinControl;
  APosition: TViewerPosition);
begin
  inherited Create;
  FControls := TFPList.Create;
  FSplitters := TFPList.Create;
  FParent := AParent;
  FPosition := APosition;
end;

destructor TSplitterController.Destroy;
begin
  FControls.Free;
  FSplitters.Free;
  inherited;
end;

procedure TSplitterController.AddControl(AControl: TControl);
var
  i: Integer;
  splitter: TSplitter;
begin
  if (AControl = nil) or (csDestroying in AControl.ComponentState) then
    exit;

  if FControls.IndexOf(AControl) > -1 then
  begin
    UpdateLayout;
    exit;
  end;

  if FControls.Count > 0 then
  begin
    splitter := TSplitter.Create(FParent);
    splitter.Parent := FParent;
    splitter.ResizeStyle := rsPattern;
    FSplitters.Add(splitter);
  end;
  FControls.Add(AControl);
  UpdateLayout;
end;

function TSplitterController.Contains(AControl: TControl): Boolean;
var
  i: Integer;
begin
  for i := 0 to FControls.Count-1 do
    if TControl(FControls[i]) = AControl then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

function TSplitterController.GetControlCount: Integer;
begin
  Result := FControls.Count;
end;

procedure TSplitterController.LoadFromIni(ini: TCustomIniFile);
var
  i: Integer;
  sz: Integer;
  s: String;
begin
  // Controls already must have been loaded into their panels
  // We skip the client-align control at index 0.
  for i := 1 to FControls.Count-1 do
    case FPosition of
      vpLeft:
        begin
          s := Format('LeftPanel_ControlHeight_%d', [i]);
          sz := ini.ReadInteger('Layout', s, -1);
          if sz > -1 then
            TControl(FControls[i]).Height := sz;
        end;
      vpRight:
        begin
          s := Format('RightPanel_ControlHeight_%d', [i]);
          sz := ini.ReadInteger('Layout', s, -1);
          if sz > -1 then
            TControl(FControls[i]).Height := sz;
        end;
      vpBottom:
        begin
          s := Format('BottomPanel_ControlWidth_%d', [i]);
          sz := TControl(FControls[i]).Width;
          if sz > -1 then
            TControl(FControls[i]).Width := sz;
        end;
    end;
end;

procedure TSplitterController.RemoveControl(AControl: TControl);
var
  idx: Integer;
begin
  idx := FControls.IndexOf(AControl);
  if idx = -1 then
    exit;
  FControls.Delete(idx);
  if FSplitters.Count > 0 then
  begin
    TSplitter(FSplitters[0]).Free;
    FSplitters.Delete(0);
  end;
  UpdateLayout;
end;

procedure TSplitterController.SaveToIni(ini: TCustomIniFile);
var
  sz: Integer;
  i: Integer;
  s: String;
begin
  for i := 1 to FControls.Count-1 do
  begin
    case FPosition of
      vpLeft:
        begin
          sz := TControl(FControls[i]).Height;
          s := Format('LeftPanel_ControlHeight_%d', [i]);
        end;
      vpRight:
        begin
          sz := TControl(FControls[i]).Height;
          s := Format('RightPanel_ControlHeight_%d', [i]);
        end;
      vpBottom:
        begin
          sz := TControl(FControls[i]).Width;
          s := Format('BottomPanel_ControlWidth_%d', [i]);
        end;
    end;
    ini.WriteInteger('Layout', s, sz);
  end;
end;

procedure TSplitterController.UpdateLayout;
var
  i: Integer;
begin
  if FControls.Count = 0 then
    exit;

  if FPosition in [vpLeft, vpRight] then
  begin
    for i := 1 to FControls.Count-1 do
    begin
      TControl(FControls[i]).Align := alBottom;
      TControl(FControls[i]).Top := 9999;
      TSplitter(FSplitters[i-1]).Align := alBottom;
      TSplitter(FSplitters[i-1]).Top := TControl(FControls[i]).Top;
    end;
  end else
  begin
    for i := 1 to FControls.Count-1 do
    begin
      TControl(FControls[i]).Align := alRight;
      TControl(FControls[i]).Left := 9999;
      TSplitter(FSplitters[i-1]).Align := alRight;
      TSplitter(FSplitters[i-1]).Left := TControl(FControls[i]).Left;
    end;
  end;
  TControl(FControls[0]).Align := alClient;
end;


{ TLayout }

constructor TLayout.Create(AParent: TWinControl);
var
  LP: TLayoutPosition;
  VP: TViewerPosition;
begin
  inherited Create;
  FParent := AParent;

  for LP in TLayoutPosition do
  begin
    FPanels[LP] := TPanel.Create(AParent);
    FPanels[LP].Caption := '';
    FPanels[LP].BevelOuter := bvNone;
//    FPanels[LP].Color := Random($FFFFFF);  // for debugging
    FPanels[LP].Parent := AParent;
  end;
  FPanels[lpBottom].Parent := FPanels[lpCenter];
  FPanels[lpBottom].Height := FParent.ClientHeight div 3;
  FPanels[lpLeft].Align := alLeft;
  FPanels[lpRight].Align := alRight;
  FPanels[lpCenter].Align := alClient;
  FPanels[lpBottom].Align := alBottom;

  for VP in TViewerPosition do
  begin
    FSplitters[VP] := TSplitter.Create(AParent);
    FSplitters[VP].Parent := AParent;
    FSplitters[VP].ResizeStyle := rsPattern;
  end;
  FSplitters[vpBottom].Parent := FPanels[lpCenter];
  FSplitters[vpBottom].Align := alBottom;
  FSplitters[vpBottom].Top := 0;

  FSplitters[vpLeft].Align := alLeft;
  FSplitters[vpLeft].Left := 9999;

  FSplitters[vpRight].Align := alRight;
  FSplitters[vpRight].Left := 0;

  for VP in TViewerPosition do
  begin
    FSplitterControllers[VP] := TSplitterController.Create(FPanels[TLayoutPosition(VP)], VP);
    ShowPanel(VP, false);
  end;
end;

destructor TLayout.Destroy;
var
  VP: TViewerPosition;
begin
  for VP in TViewerPosition do
    FSplitterControllers[VP].Free;
  inherited;
end;

procedure TLayout.AddCenterControl(AControl: TControl);
begin
  if AControl <> nil then
  begin
    AControl.Parent := FPanels[lpCenter];
    AControl.Align := alClient;
  end;
end;

function TLayout.GetViewerPosition(AControl: TControl): Integer;
var
  VP: TViewerPosition;
begin
  for VP in TViewerPosition do
    if FSplitterControllers[VP].Contains(AControl) then
    begin
      Result := ord(VP);
      exit;
    end;
  Result := -1;
end;

procedure TLayout.ShowControl(AControl: TControl; APosition: TViewerPosition);
var
  LP: TLayoutPosition;
  VP: TViewerPosition;
begin
  if AControl = nil then
    exit;

  HideControl(AControl);

  LP := ViewerToLayoutPosition(APosition);
  AControl.Parent := FPanels[LP];
  AControl.Show;
  FSplitterControllers[APosition].AddControl(AControl);
  ShowPanel(APosition, true);

  for VP in TViewerPosition do
    ShowPanel(VP, FSplitterControllers[VP].ControlCount > 0);
end;

procedure TLayout.HideControl(AControl: TControl);
var
  LP: TLayoutPosition;
  VP: TViewerPosition;
begin
  if AControl = nil then
    exit;
  for VP in TViewerPosition do
  begin
    LP := ViewerToLayoutPosition(VP);
    if AControl.Parent = FPanels[LP] then
    begin
      FSplitterControllers[VP].RemoveControl(AControl);
      AControl.Hide;
      AControl.Parent := FParent;
      break;
    end;
  end;

  for VP in TViewerPosition do
    ShowPanel(VP, FSplitterControllers[VP].ControlCount > 0);
end;

procedure TLayout.ShowPanel(APosition: TViewerPosition; Enable: Boolean);
var
  LP: TLayoutPosition;
begin
  LP := ViewerToLayoutPosition(APosition);
  FPanels[LP].Visible := Enable;
  FSplitters[APosition].Visible := Enable;
end;

function TLayout.ViewerToLayoutPosition(AViewerPosition: TViewerPosition): TLayoutPosition;
begin
  Result := TLayoutPosition(ord(AViewerPosition));
end;

procedure TLayout.LoadFromIni(ini: TCustomIniFile);
var
  VP: TViewerPosition;
begin
  FPanels[lpLeft].Width := ini.ReadInteger('Layout', 'LeftPanel_Width', FPanels[lpLeft].Width);
  FPanels[lpRight].Width := ini.ReadInteger('Layout', 'RightPanel_Width', FPanels[lpRight].Width);
  FPanels[lpBottom].Height := ini.ReadInteger('Layout', 'BottomPanel_Height', FPanels[lpBottom].Height);
  for VP in TViewerPosition do
    FSplitterControllers[VP].LoadFromIni(ini);
end;

procedure TLayout.SaveToIni(ini: TCustomIniFile);
var
  VP: TViewerPosition;
begin
  ini.WriteInteger('Layout', 'LeftPanel_Width', FPanels[lpLeft].Width);
  ini.WriteInteger('Layout', 'RightPanel_Width', FPanels[lpRight].Width);
  ini.WriteInteger('Layout', 'BottomPanel_Height', FPanels[lpBottom].Height);
  for VP in TViewerPosition do
    FSplitterControllers[VP].SaveToIni(ini);
end;

end.

