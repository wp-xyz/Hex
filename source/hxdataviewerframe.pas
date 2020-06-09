unit hxDataViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  MPHexEditor,
  hxGlobal, hxViewerGrids, hxGridViewerFrame;

type

  { TDataViewerGrid }

  TDataViewerGrid = class(TViewerGrid)
  protected
    procedure DefineColumns; override;
    procedure DoUpdateData; override;
    procedure PopulateDataList; override;
    function SelectCell(ACol, ARow: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent; AOwnsData: Boolean); override;
  end;


  { TDataViewerFrame }

  TDataViewerFrame = class(TGridViewerFrame)
  protected
    function CreateViewerGrid: TViewerGrid; override;
    function GetDefaultColWidths(AIndex: Integer): Integer; override;
  end;


implementation

{$R *.lfm}

uses
  hxViewerItems;

{------------------------------------------------------------------------------}
{                              TDataViewerGrid                                  }
{------------------------------------------------------------------------------}

constructor TDataViewerGrid.Create(AOwner: TComponent; AOwnsData: Boolean);
begin
  FDataItemClass := TDataItem;
  inherited Create(AOwner, AOwnsData);
end;

procedure TDataViewerGrid.DefineColumns;
var
  lCol: TGridColumn;
begin
  Columns.BeginUpdate;
  try
    Columns.Clear;

    lCol := Columns.Add;
    lCol.Tag := 0;  // TDataItem property with index 0: DataType
    lCol.Title.Caption := 'Data type';
    lCol.Width := 80;
    lCol.SizePriority := 0;
    lCol.ReadOnly := true;

    lCol := Columns.Add;
    lCol.Tag := 1;  // TDataItem property with index 1: Data size
    lCol.Title.Caption := 'Size';
    lCol.Title.Alignment := taCenter;
    lCol.Alignment := taRightJustify;
    lCol.Width := 40;
    lCol.SizePriority := 0;
    lCol.ReadOnly := true;

    lCol := Columns.Add;
    lCol.Tag := 3;  // TDataItem property with index 3: BigEndian
    lCol.Title.Caption := 'BE';
    lCol.Title.Alignment := taCenter;
    lCol.Width := 24;
    lCol.SizePriority := 0;
    lCol.ButtonStyle := cbsCheckboxColumn;

    lCol := Columns.Add;
    lCol.Tag := -1;  // Value column
    lCol.Title.Caption := 'Value';
    lCol.SizePriority := 1;  // Expand column to fill rest of grid width
    //lCol.ReadOnly := true;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TDataViewerGrid.DoUpdateData;
var
  i: Integer;
  item: TDataItem;
begin
  for i := 0 to FDataList.Count - 1 do
  begin
    item := FDataList[i] as FDataItemClass;
    item.Offset := HexEditor.GetCursorPos;
  end;
  Invalidate;
end;

procedure TDataViewerGrid.PopulateDataList;
begin
  FDataList.Clear;
  if (dtByte in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtByte, 1, HexParams.BigEndian));
  if (dtShortInt in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtShortInt, 1, HexParams.BigEndian));
  if (dtWord in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtWord, 2, HexParams.BigEndian));
  if (dtSmallInt in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtSmallInt, 2, HexParams.BigEndian));
  if (dtLongWord in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtLongWord, 4, HexParams.BigEndian));
  if (dtLongInt in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtLongInt, 4, HexParams.BigEndian));
  if (dtInt64 in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtInt64, 8, HexParams.BigEndian));
  if (dtCurrency in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtCurrency, 8, HexParams.BigEndian));
  if (dtSingle in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtSingle, 4, HexParams.BigEndian));
  if (dtDouble in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtDouble, 8, HexParams.BigEndian));
  if (dtExtended in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtExtended, 10, HexParams.BigEndian));
  if (dtReal48 in HexParams.DataViewerDataTypes) then
    FDataList.Add(TDataItem.Create(dtReal48, 6, HexParams.BigEndian));
end;

function TDataViewerGrid.SelectCell(ACol, ARow: Integer): Boolean;
var
  item: TDataItem;
begin
  Result := inherited SelectCell(ACol, ARow);
  if Result and Assigned(HexEditor) and (ARow >= FixedRows) and (HexEditor.DataSize > 0) then
  begin
    HexEditor.SelStart := HexEditor.GetCursorPos;
    UpdateData(HexEditor);
    item := FDataList[ARow - FixedRows] as TDataItem;
    HexEditor.SelEnd := HexEditor.SelStart + abs(item.DataSize) - 1;
  end;
end;


{------------------------------------------------------------------------------}
{                             TDataViewerFrame                                  }
{------------------------------------------------------------------------------}

function TDataViewerFrame.CreateViewerGrid: TViewerGrid;
begin
  Result := TDataViewerGrid.Create(self, true);
end;

function TDataViewerFrame.GetDefaultColWidths(AIndex: Integer): Integer;
begin
  Result := DefaultHexParams.DataViewerColWidths[AIndex];
end;

end.

