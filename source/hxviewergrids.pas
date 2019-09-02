unit hxViewerGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Grids,
  MPHexEditor,
  hxGlobal, hxViewerItems;

type
  TViewerGrid = class(TCustomDrawGrid)
  private
    FHexEditor: TMPHexEditor;

  protected
    FDataList: TDataList;
    FDataItemClass: TDataItemClass;

    // new methods
    procedure DefineColumns; virtual;
    procedure DoUpdateData; virtual;
    function GetCellText(ACol, ARow: Integer): String; virtual;
    function GetValueAsText(AItem: TDataItem): String;
    procedure PopulateDataList; virtual;
    procedure Prepare; virtual;

    // inherited methods
    procedure DrawTextInCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState); override;
    procedure GetCheckBoxState(const ACol, ARow: Integer;
      var aState:TCheckboxState); override;
    procedure SetCheckboxState(const ACol, ARow: Integer;
      const aState: TCheckboxState); override;

    property HexEditor: TMPHexEditor read FHexEditor;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateData(AHexEditor: TMPHexEditor); virtual;
    property DataItemClass: TDataItemClass read FDataItemClass;
  published

  end;


implementation

uses
  LCLIntf, TypInfo,
  real48utils,
  hxUtils;

constructor TViewerGrid.Create(AOwner: TComponent);
begin
  inherited;
  FixedCols := 0;
  FDataList := TDataList.Create;
  FDataItemClass := TDataItem;
  DefineColumns;
  AutoFillColumns := true;
  Prepare;
end;

destructor TViewerGrid.Destroy;
begin
  FDataList.Free;
  inherited;
end;

procedure TViewerGrid.DefineColumns;
var
  propCount: Integer;
  propList: PPropList;
  lCol: TGridColumn;
  i: Integer;
begin
  propCount := GetPropList(FDataItemClass, propList);
  Columns.BeginUpdate;
  try
    Columns.Clear;
    for i := 0 to propCount - 1 do begin
      lCol := Columns.Add;
      // Each created column carries the index of the property to be displayed in
      // its Tag property.
      lCol.Tag := i;
      lCol.Title.Caption := FDataItemClass.GetHeader(i);
      lCol.Title.Alignment := taCenter;
      if FDataItemClass.IsBoolean(i) then
        lCol.ButtonStyle := cbsCheckboxColumn;
    end;

    // Finally add value column
    lCol := Columns.Add;
    lCol.Tag := -1;
    lCol.Title.Caption := 'Value';
  finally
    Columns.EndUpdate;
    FreeMem(propList);
  end;
end;

procedure TViewerGrid.DoUpdateData;
begin
  // to be overridden
end;

procedure TViewerGrid.DrawTextInCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  s: String;
  h: Integer;
begin
  if not HandleAllocated then
    exit;

  s := GetCellText(ACol, ARow);
  h := Canvas.TextHeight(s);
  InflateRect(ARect, -varCellPadding, -varCellPadding);
  Canvas.TextRect(ARect, ARect.Left, (ARect.Top + ARect.Bottom - h) div 2, s);
end;


function TViewerGrid.GetCellText(ACol, ARow: Integer): String;
var
  lCol: TGridColumn;
  item: TDataItem;
begin
  lCol := Columns[ACol - FixedCols];
  if ARow = 0 then
    Result := FDataItemClass.GetHeader(lCol.Tag)
  else
  begin
    item := FDataList[ARow - FixedRows] as FDataItemClass;
    if lCol.Tag = -1 then
      Result := GetValueAsText(item)
    else
      Result := item.GetText(lCol.Tag);
  end;
end;

procedure TViewerGrid.GetCheckBoxState(const ACol, ARow: Integer;
  var aState:TCheckboxState);
var
  s: String;
begin
  s := GetCellText(ACol, ARow);
  if s = 'BE' then
    AState := cbChecked
  else
    AState := cbUnchecked;
end;

function TViewerGrid.GetValueAsText(AItem: TDataItem): String;
var
  buffer: array of byte;
  dbl: Double;
  len: Integer;
  ws: WideString;
begin
  if AItem = nil then
  begin
    Result := '';
    exit;
  end;

  // Don't read beyond end-of-file
  if AItem.Offset + AItem.DataSize > HexEditor.DataSize then
  begin
    Result := 'N/A';
    exit;
  end;

  if AItem.DataSize <= 0 then
    SetLength(buffer, 32)
  else
    SetLength(buffer, AItem.DataSize);
  HexEditor.ReadBuffer(buffer[0], AItem.Offset, AItem.DataSize);
  try
    case AItem.DataType of
      // *** NUMERIC DATA ***
      dtByte:
        Result := IntToStr(buffer[0]);
      dtShortInt:
        Result := IntToStr(ShortInt(buffer[0]));
      dtWord:
        if AItem.BigEndian then
          Result := IntToStr(BEToN(PWord(@buffer[0])^))
        else
          Result := IntToStr(LEToN(PWord(@buffer[0])^));
      dtSmallInt:
        if AItem.BigEndian then
          Result := IntToStr(BEToN(SmallInt(PWord(@buffer[0])^)))
        else
          Result := IntToStr(LEToN(SmallInt(PWord(@buffer[0])^)));
      dtLongWord:
        if AItem.BigEndian then
          Result := IntToStr(BEToN(PDWord(@buffer[0])^))
        else
          Result := IntToStr(LEToN(PDWord(@buffer[0])^));
      dtLongInt:
        if AItem.BigEndian then
          Result := IntToStr(BEToN(LongInt(PDWord(@buffer[0])^)))
        else
          Result := IntToStr(LEToN(LongInt(PDWord(@buffer[0])^)));
      dtInt64:
        if AItem.BigEndian then
          Result := IntToStr(BEToN(PInt64(@buffer[0])^))
        else
          Result := IntToStr(LEToN(PInt64(@buffer[0])^));
      dtCurrency:
        if AItem.BigEndian then
          Result := CurrToStr(BEToN(PCurrency(@buffer[0])^))
        else
          Result := CurrToStr(LEToN(PCurrency(@buffer[0])^));
      dtSingle:
        if AItem.BigEndian then
          Result := Format('%.15g', [BEToN(PSingle(@buffer[0])^)])
        else
          Result := Format('%.15g', [LEToN(PSingle(@buffer[0])^)]);
      dtDouble:
        if AItem.BigEndian then
          Result := Format('%.15g', [BEToN(PDouble(@buffer[0])^)])
        else
          Result := Format('%.15g', [LEToN(PDouble(@buffer[0])^)]);
      dtExtended:
        if AItem.BigEndian then
          Result := Format('%.15g', [BEToN(PExtended(@buffer[0])^)])
        else
          Result := Format('%.15g', [LEToN(PExtended(@buffer[0])^)]);
      dtReal48:
        begin
          if AItem.BigEndian then
            dbl := BEToN(PReal48(@buffer[0])^)
          else
            dbl := LEToN(PReal48(@buffer[0])^);
          Result := Format('%.15g', [dbl]);
        end;

      // *** ANSI STRINGS ***
      dtShortString:
        // Ansi (1-byte) characters. 1 length byte before character array, i.e.
        // max length 255.
        // For AItem.DataSize = -1, the next record element follows immedialte
        // after the string. Otherwise the next record element begins after
        // AItem.DataSize bytes.
        begin
          len := buffer[0];
          if (len > 0) and ((AItem.DataSize = -1) or (len < AItem.DataSize)) then
          begin
            SetLength(Result, len);
            Move(buffer[1], Result[1], len);
            Result := '''' + Result + '''';
          end else
            Result := '*** STRING TOO LONG ***';
        end;
      dtAnsiString:
        // Ansi (1-byte) characters. 1 length word before character array, i.e.
        // max length = 65535 ("unlimited")
        // Rest identical to dtShortString
        begin
          if AItem.BigEndian then
            len := BEToN(PWord(@buffer[0])^)
          else
            len := LEToN(PWord(@buffer[0])^);
          if (len > 0) and ((AItem.DataSize = -1) or (len < AItem.DataSize - 2)) then
          begin
            SetLength(Result, len);
            HexEditor.ReadBuffer(Result[1], AItem.Offset+2, len);
            Result := '''' + Result + '''';
          end else
            Result := '*** STRING TOO LONG ***';
        end;
      dtPChar:
        begin
          len := -AItem.DataSize;
          SetLength(Result, len);
          HexEditor.ReadBuffer(Result[1], AItem.Offset, len);
          Result := '''' + Result + '''';
        end;
      dtCharArray:
        // Character array, no length information. The next record element
        // begins immediately after the dtCharArray element.
        begin
          SetLength(Result, AItem.DataSize);
          Move(buffer[0], Result[1], AItem.DataSize);
          Result := '''' + Result + '''';
        end;

      // *** WIDE STRINGS ***
      dtWideString:
        begin
          if AItem.BigEndian then
            len := BEToN(PWord(@buffer[0])^)
          else
            len := LEToN(PWord(@buffer[0])^);
          if (len > 0) and ((AItem.DataSize = -1) or (len * 2 < AItem.DataSize - 2)) then
          begin
            SetLength(ws, len);
            HexEditor.ReadBuffer(ws[1], AItem.Offset+2, len);
            // To do: fix big endian of wide string characters
            Result := '''' + UTF8Encode(ws) + '''';
          end else
            Result := '*** STRING TOO LONG ***';
        end;
      dtPWideChar:
        begin
          len := -AItem.DataSize;
          SetLength(ws, len div 2);
          HexEditor.ReadBuffer(ws[1], AItem.Offset, len);
          Result := '''' + UTF8Encode(ws) + '''';
        end;
      dtWideCharArray:
        begin
          SetLength(ws, AItem.DataSize div 2);
          Move(buffer[0], ws[1], AItem.DataSize);
          Result := '''' + UTF8Decode(ws) + '''';
        end;

      else
        Result := '*** UNKNOWN DATA TYPE ***';
    end;
  except
    Result := 'N/A';
  end;
end;

procedure TViewerGrid.Prepare;
begin
  PopulateDataList;
  RowCount := FDataList.Count + FixedRows;
  Invalidate;
end;

procedure TViewerGrid.PopulateDataList;
begin
  // to be overridden: Must add the DataItem objects to the FDataList which will
  // be displayed in the grid as rows.
end;

procedure TViewerGrid.SetCheckboxState(const ACol, ARow: Integer;
  const aState: TCheckboxState);
var
  item: TDataItem;
begin
  item := FDataList[ARow - FixedRows] as FDataItemClass;
  if AState = cbChecked then
    item.BigEndian := true
  else if AState = cbUnchecked then
    item.BigEndian := false;
end;

procedure TViewerGrid.UpdateData(AHexEditor: TMPHexEditor);
begin
  FHexEditor := AHexEditor;
  if Assigned(FHexEditor) then
    DoUpdateData;
end;


end.

