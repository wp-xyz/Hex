unit hxViewerPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  hxGlobal, hxBasicViewerFrame;

type               (*
  TViewerPanel = class(TPanel)
  private
    FScrollbar: TScrollbar;
  protected
    procedure CreateSplitter;
    procedure PositionSplitter;
    procedure SetAlign(AValue: TAlign); override;
    procedure SetParent(AControl: TWinControl); override;
  end;
                     *)

  TViewerPanel = class(TPanel)
  private
    FViewers: TFPList;
    FSplitters: TFPList;
    FSplitter: TSplitter;
    FPosition: TViewerPosition;
    function GetPosition: TViewerPosition;
    procedure SetAlign(AValue: TAlign); override;
  protected
    procedure AddSplitter(AAlign: TAlign);
    procedure RemoveSplitters;
    procedure SetVisible(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddViewer(AViewer: TBasicViewerFrame);
    procedure DeleteViewer(AViewer: TBasicViewerFrame);
    procedure UpdateLayout;
    property Position: TViewerPosition read GetPosition;
  end;

implementation

const
  MAX_VIEWERS = 3;


{ TViewerPanel }

constructor TViewerPanel.Create(AOwner: TComponent);
begin
  inherited;
  FViewers := TFPList.Create;
  FSplitters := TFPList.Create;
  BevelOuter := bvNone;
  Caption := '';
end;

destructor TViewerPanel.Destroy;
begin
  FViewers.Free;
  FSplitters.Free;
  inherited;
end;

procedure TViewerPanel.AddViewer(AViewer: TBasicViewerFrame);
begin
  if FViewers.Count = MAX_VIEWERS then
    raise Exception.CreateFmt('[TViewerPanel.Add] Only %d viewers supported.', [MAX_VIEWERS]);

  FViewers.Add(AViewer);
  AViewer.Parent := self;
  RemoveSplitters;

  case FViewers.Count of
    1: AViewer.Align := alClient;
    2: if (FPosition = vpLeft) or (FPosition = vpRight) then begin
         TBasicViewerFrame(FViewers[0]).Align := alTop;
         TBasicViewerFrame(FViewers[0]).Height := Height div 2;
         AViewer.Align := alClient;
         AddSplitter(alTop);
       end else begin
         TBasicViewerFrame(FViewers[0]).Align := alLeft;
         TBasicViewerFrame(FViewers[0]).Width := Width div 2;
         AViewer.Align := alClient;
         AddSplitter(alLeft);
       end;
    3: if (FPosition = vpLeft) or (FPosition = vpRight) then
       begin
         TBasicViewerFrame(FViewers[0]).Height := Height div 3;
         AViewer.Align := alBottom;
         AViewer.Height := Height div 3;
         AddSplitter(alTop);
         AddSplitter(alBottom);
       end else begin
         TBasicViewerFrame(FViewers[0]).Width := Width div 3;
         AViewer.Align := alRight;
         AViewer.Width := Width div 3;
         AddSplitter(alLeft);
         AddSplitter(alRight);
       end;
  end;
end;

procedure TViewerPanel.AddSplitter(AAlign: TAlign);
var
  splitter: TSplitter;
begin
  splitter := TSplitter.Create(self);
  splitter.Parent := self;
  splitter.Align := AAlign;
  case AAlign of
    alLeft: splitter.Left := Width;
    alRight: splitter.Left := 0;
    alTop: splitter.Top := Height;
    alBottom: splitter.Top := 0;
  end;
  FSplitters.Add(splitter);
end;

procedure TViewerPanel.DeleteViewer(AViewer: TBasicViewerFrame);
var
  idx: Integer;
begin
  if FViewers = nil then
    exit;

  idx := FViewers.IndexOf(AViewer);
  if idx = -1 then
    exit;

  case FViewers.Count of
    1: FViewers.Delete(0);
    2: begin
         FViewers.Delete(idx);
         TSplitter(FSplitters[0]).Free;
         FSplitters.Delete(0);
         TBasicViewerFrame(FViewers[0]).Align := alClient;
       end;
    3: if AViewer = TBasicViewerFrame(FViewers[0]) then begin
         FViewers.Delete(idx);
         TSplitter(FSplitters[0]).Free;
         FSplitters.Delete(0);
       end else
       if AViewer = TBasicViewerFrame(FViewers[2]) then begin
         FViewers.Delete(idx);
         TSplitter(FSplitters[1]).Free;
         FSplitters.Delete(1);
       end else begin
         FViewers.Delete(idx);
         TSplitter(FSplitters[1]).Free;
         FSplitters.Delete(1);
         TBasicViewerFrame(FViewers[1]).Align := alClient;
       end;
  end;
end;

function TViewerPanel.GetPosition: TViewerPosition;
begin
  case Align of
    alLeft: Result := vpLeft;
    alRight: Result := vpRight;
    alBottom: Result := vpBottom;
    else raise Exception.Create('[TViewerPanel.GetPosition] Unsupported Align value.');
  end;
end;

procedure TViewerPanel.RemoveSplitters;
var
  i: Integer;
begin
  for i:=0 to FSplitters.Count-1 do
    TSplitter(FSplitters[i]).Free;
  FSplitters.Clear;
end;

procedure TViewerPanel.SetAlign(AValue: TAlign);
begin
  if AValue = inherited Align then
    exit;

  inherited SetAlign(AValue);

  if FViewers.Count > 0 then
    raise Exception.Create('[TViewerPanel.SetViewerPosition] Only allowed if empty');

  case Align of
    alLeft: FPosition := vpLeft;
    alRight: FPosition := vpRight;
    alBottom: FPosition := vpBottom;
    else raise Exception.Create('[TViewerPanel.SetAlign] This Align value is not allowed.');
  end;

  if FSplitter = nil then begin
    FSplitter := TSplitter.Create(self);
    FSplitter.Parent := Parent;
  end;
  FSplitter.Align := Align;
  case Align of
    alLeft: FSplitter.Left := Left + Width + 1;
    alRight: FSplitter.Left := Left - 1;
    alTop: FSplitter.Top := Top + Height + 1;
    alBottom: FSplitter.Top := Top - 1;
  end;
end;

procedure TViewerPanel.SetVisible(AValue: Boolean);
begin
   inherited SetVisible(AValue);
   if Visible then begin
     FSplitter.Show;
     case Align of
       alLeft: FSplitter.Left := Left + Width + 1;
       alRight: FSplitter.Left := Left - 1;
       alTop: FSplitter.Top := Top + Height + 1;
       alBottom: FSplitter.Top := Top - 1;
     end;
   end else
     FSplitter.Hide;
end;

procedure TViewerPanel.UpdateLayout;
begin
  Visible := ControlCount > 0;
end;

end.

