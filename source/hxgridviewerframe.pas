unit hxGridViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  MPHexEditor,
  hxViewerGrids, hxBasicViewerFrame;

type
  TGridViewerFrame = class(TBasicViewerFrame)
  private
    function GetColWidths(AIndex: Integer): Integer;
    procedure SetColWidths(AIndex: Integer; const AValue: Integer);
  protected
    FGrid: TViewerGrid;
    function CreateViewerGrid: TViewerGrid; virtual; abstract;
    function GetDefaultColWidths({%H-}AIndex: Integer): Integer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateData(AHexEditor: TMPHexEditor); override;
    property ColWidths[AIndex: Integer]: Integer read GetColWidths write SetColWidths;
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

procedure TGridViewerFrame.UpdateData(AHexEditor: TMPHexEditor);
begin
  if Assigned(FGrid) then
    FGrid.UpdateData(AHexEditor);
end;

end.

