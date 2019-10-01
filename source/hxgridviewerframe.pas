unit hxGridViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  MPHexEditor,
  hxHexEditor, hxViewerGrids, hxBasicViewerFrame;

type
  TGridViewerFrame = class(TBasicViewerFrame)
  private
    function GetColWidths(AIndex: Integer): Integer;
    function GetWriteProtected: Boolean;
    procedure SetColWidths(AIndex: Integer; const AValue: Integer);
    procedure SetWriteProtected(const AValue: Boolean);
  protected
    FGrid: TViewerGrid;
    function CreateViewerGrid: TViewerGrid; virtual; abstract;
    function GetDefaultColWidths({%H-}AIndex: Integer): Integer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateData(AHexEditor: THxHexEditor); override;
    property ColWidths[AIndex: Integer]: Integer read GetColWidths write SetColWidths;
    property WriteProtected: Boolean read GetWriteProtected write SetWriteProtected;
  end;


implementation

{$R *.lfm}


{ TGridViewerFrame }

constructor TGridViewerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

{ AIndex is relative to the Grid.Columns, i.e. excluding the FixedCols }
function TGridViewerFrame.GetColWidths(AIndex: Integer): Integer;
begin
  if AIndex < FGrid.ColCount - FGrid.FixedCols then begin
    if Assigned(FGrid) then
      Result := FGrid.Columns[AIndex].Width
    else
      Result := GetDefaultColWidths(AIndex);
  end else
    Result := 100;
end;

function TGridViewerFrame.GetDefaultColWidths(AIndex: Integer): Integer;
begin
  Result := 100;
end;

function TGridViewerFrame.GetWriteProtected: Boolean;
begin
  Result := goEditing in FGrid.Options;
end;

procedure TGridViewerFrame.Loaded;
begin
  inherited;
  if FGrid = nil then
  begin
    FGrid := CreateViewerGrid;
    FGrid.Name := '';
    FGrid.Parent := Self;
    FGrid.Align := alClient;
    FGrid.Options := FGrid.Options + [goEditing, goColSizing, goDrawFocusSelected];
  end;
end;

procedure TGridViewerFrame.SetColWidths(AIndex: Integer;
  const AValue: Integer);
begin
  if Assigned(FGrid) and (AIndex < FGrid.ColCount - FGrid.FixedCols) then
    FGrid.Columns[AIndex].Width := AValue;
end;

procedure TGridViewerFrame.SetWriteProtected(const AValue: Boolean);
begin
  if AValue then
    FGrid.Options := FGrid.Options - [goEditing]
  else
    FGrid.Options := FGrid.Options + [goEditing];
end;

procedure TGridViewerFrame.UpdateData(AHexEditor: THxHexEditor);
begin
  if Assigned(FGrid) then
    FGrid.UpdateData(AHexEditor);
end;

end.

