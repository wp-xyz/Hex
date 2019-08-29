unit hxBasicViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Grids,
  MPHexEditor,
  hxGlobal, hxViewerGrids;

type

  { TBasicViewerFrame }

  TBasicViewerFrame = class(TFrame)
    lblHeader: TLabel;
  private
    function GetColWidths(AIndex: Integer): Integer;
    procedure SetColWidths(AIndex: Integer; const AValue: Integer);
  protected
    FGrid: TViewerGrid;
    function CreateViewerGrid: TViewerGrid; virtual; abstract;
    function GetDefaultColWidths(AIndex: Integer): Integer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateData(AHexEditor: TMPHexEditor); virtual;
    property ColWidths[AIndex: Integer]: Integer read GetColWidths write SetColWidths;
  end;


implementation

{$R *.lfm}

uses
  TypInfo;


{ TBasicViewerFrame }

constructor TBasicViewerFrame.Create(AOwner: TComponent);
begin
  inherited;
  {
  FGrid := CreateViewerGrid;
  FGrid.Name := '';
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.Options := FGrid.Options + [goEditing, goColSizing, goDrawFocusSelected];
  }
end;

{ AIndex is relative to the Grid.Columns, i.e. excluding the FixedCols }
function TBasicViewerFrame.GetColWidths(AIndex: Integer): Integer;
begin
  if AIndex < FGrid.ColCount - FGrid.FixedCols then begin
    if Assigned(FGrid) then
      Result := FGrid.Columns[AIndex].Width
    else
      Result := GetDefaultColWidths(AIndex);
  end else
    Result := 100;
end;

function TBasicViewerFrame.GetDefaultColWidths(AIndex: Integer): Integer;
begin
  Result := 100;
end;

procedure TBasicViewerFrame.Loaded;
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

procedure TBasicViewerFrame.SetColWidths(AIndex: Integer;
  const AValue: Integer);
begin
  if Assigned(FGrid) and (AIndex < FGrid.ColCount - FGrid.FixedCols) then
    FGrid.Columns[AIndex].Width := AValue;
end;

procedure TBasicViewerFrame.UpdateData(AHexEditor: TMPHexEditor);
begin
  if Assigned(FGrid) then
    FGrid.UpdateData(AHexEditor);
end;

end.

