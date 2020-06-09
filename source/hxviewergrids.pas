unit hxViewerGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Grids,
  hxGlobal, hxHexEditor, hxViewerItems;

type
  TViewerGrid = class(TCustomDrawGrid)
  private
    FHexEditor: THxHexEditor;
    FOldEditText: String;
    FEditText: String;
    FOwnsData: Boolean;
    function GetDataCount: Integer;
    procedure SetDataList(AValue: TDataList);

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
    procedure SetValueAsText(AItem: TDataItem; const AText: String);

    // inherited methods
    procedure DrawTextInCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState); override;
    procedure EditorDoResetValue; override;
    procedure GetCheckBoxState(const ACol, ARow: Integer;
      var aState:TCheckboxState); override;
    function GetEditText(ACol, ARow: LongInt): String; override;
    procedure SetCheckboxState(const ACol, ARow: Integer;
      const aState: TCheckboxState); override;
    procedure SetEditText(ACol, ARow: LongInt; const AText: String); override;

  public
    constructor Create(AOwner: TComponent; AOwnsData: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure EditingDone; override;
    procedure UpdateData(AHexEditor: THxHexEditor); virtual;
    property DataCount: Integer read GetDataCount;
    property DataItemClass: TDataItemClass read FDataItemClass;
    property DataList: TDataList read FDataList write SetDatalist;
    property HexEditor: THxHexEditor read FHexEditor;
  published

  end;


implementation

uses
  LCLIntf, Math, TypInfo, Dialogs,
  real48utils,
  hxUtils;

constructor TViewerGrid.Create(AOwner: TComponent; AOwnsData: Boolean);
begin
  inherited Create(AOwner);
  FOwnsData := AOwnsData;
  FixedCols := 0;
  if FOwnsData then
    FDataList := TDataList.Create;
  FDataItemClass := TDataItem;
  DefineColumns;
  AutoFillColumns := true;
  Prepare;
end;

destructor TViewerGrid.Destroy;
begin
  if FOwnsData then
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

procedure TViewerGrid.EditingDone;
var
  item: TDataItem;
begin
  if FOldEditText = FEditText then
    exit;
  item := FDataList[Row - FixedRows] as FDataItemClass;
  SetValueAsText(item, FEditText);
end;

procedure TViewerGrid.EditorDoResetValue;
begin
  FEditText := FOldEditText;
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

function TViewerGrid.GetDataCount: Integer;
begin
  if Assigned(FDataList) then
    Result := FDataList.Count
  else
    Result := 0;
end;

function TViewerGrid.GetEditText(ACol, ARow: LongInt): String;
begin
  Result := GetCellText(ACol, ARow);
  FEditText := Result;
  FOldEditText := Result;
end;

function TViewerGrid.GetValueAsText(AItem: TDataItem): String;
var
  buffer: array of byte;
  dbl: Double;
  len: Integer;
  ws: WideString;
  ext10: TExtended10;
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
       {$IF SizeOf(extended) = 10}
        if AItem.BigEndian then
          Result := Format('%.15g', [BEToN(PExtended(@buffer[0])^)])
        else
          Result := Format('%.15g', [LEToN(PExtended(@buffer[0])^)]);
       {$ELSE}
        begin
          ext10 := PExtended10(@buffer[0])^;
          if AItem.BigEndian then
            dbl := ExtendedToDouble(BEToN(ext10))
          else
            dbl := ExtendedToDouble(LEToN(ext10));
          Result := Format('%.15g', [dbl]);
        end;
       {$IFEND}
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
            if AItem.BigEndian then
              ws := BEToN(ws)
            else
              ws := LEToN(ws);
            Result := '''' + UTF8Encode(ws) + '''';
          end else
            Result := '*** STRING TOO LONG ***';
        end;
      dtPWideChar:
        begin
          len := -AItem.DataSize;
          SetLength(ws, len div 2);
          HexEditor.ReadBuffer(ws[1], AItem.Offset, len);
          if AItem.BigEndian then
            ws := BEToN(ws)
          else
            ws := LEToN(ws);
          Result := '''' + UTF8Encode(ws) + '''';
        end;
      dtWideCharArray:
        begin
          SetLength(ws, AItem.DataSize div 2);
          Move(buffer[0], ws[1], AItem.DataSize);
          if AItem.BigEndian then
            ws := BEToN(ws)
          else
            ws := LEToN(ws);
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
  if FDataList = nil then
    RowCount := FixedRows
  else
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

procedure TViewerGrid.SetDataList(AValue: TDataList);
begin
  if not FOwnsData then begin
    FDataList := AValue;
    if Assigned(FDataList) then
      RowCount := FDataList.Count + FixedRows
    else
      RowCount := FixedRows;
  end;
end;

procedure TViewerGrid.SetEditText(ACol, ARow: LongInt; const AText: String);
begin
  FEditText := AText;
end;

procedure TViewerGrid.SetValueAsText(AItem: TDataItem; const AText: String);
const
  errOK = 0;
  errOutOfRange = 1;
  errNumberReq = 2;
var
  b: Byte;
  i: Integer;
  i64: Int64;
  c: Currency;
  w: Word;
  dw: DWord;
  sng: Single;
  dbl: Double;
  ext: Extended;
  ext10: TExtended10;
  r48: Real48;
  oldInsertMode: Boolean;
  fs: TFormatSettings;
  err: Integer;
begin
  err := errOK;
  oldInsertMode := FHexEditor.InsertMode;
  FHexEditor.InsertMode := false;
  try
    case AItem.DataType of
      dtByte, dtShortInt, dtWord, dtSmallInt, dtLongWord, dtLongInt:
        if TryStrToInt(AText, i) then
          case AItem.DataType of
            dtByte:
              if ((AItem.DataType = dtByte) and (i >= 0) and (i <= 255)) or
                 ((AItem.DataType = dtShortInt) and (i >= -128) and (i <= 127)) then
              begin
                b := byte(i);
                FHexEditor.WriteBuffer(b, AItem.Offset, 1);
              end else
                err := errOutOfRange;
            dtWord:
              if (AItem.DataType = dtWord) and (i >= 0) and (word(i) <= word($FFFF)) then
              begin
                w := word(i);
                if AItem.BigEndian then w := NtoBE(w) else w := NtoLE(w);
                FHexEditor.WriteBuffer(w, AItem.Offset, SizeOf(w));
              end else
                err := errOutOfRange;
            dtSmallInt:
              if (SmallInt(i) >= SmallInt($8000)) and (word(i) <= $7FFF) then
              begin
                w := word(i);
                if AItem.BigEndian then w := NToBE(w) else w := NToLE(w);
                FHexEditor.WriteBuffer(w, AItem.Offset, 2);
              end else
                err := errOutOfRange;
            dtLongWord:
              if (i >= 0) and (LongWord(i) <= LongWord($FFFFFFFF)) then
              begin
                dw := DWord(i);
                if AItem.BigEndian then dw := NtoBE(dw) else dw := NtoLE(dw);
                FHexEditor.WriteBuffer(dw, AItem.Offset, SizeOf(dw));
              end else
                err := errOutOfRange;
            dtLongInt:
              if (LongInt(i) >= LongInt($80000000)) and (LongWord(i) <= $7FFFFFFF) then
              begin
                dw := DWord(i);
                if AItem.BigEndian then dw := NToBE(dw) else dw := NToLE(dw);
                  FHexEditor.WriteBuffer(dw, AItem.Offset, 4);
              end else
                err := errOutOfRange;
          end
        else
          err := errNumberReq;

      dtInt64, dtCurrency:
        begin
          case AItem.DataType of
            dtInt64:
              if not TryStrToInt64(AText, i64) then
                err := errNumberReq;
            dtCurrency:
              if TryStrToCurr(AText, c) then
                Move(c, i64, SizeOf(c))
              else
                err := errNumberReq;
          end;
          if err = errOK then
          begin
            if AItem.BigEndian then i64 := NToBE(i64) else i64 := NToLE(i64);
            FHexEditor.WriteBuffer(i64, AItem.Offset, SizeOf(i64));
          end;
        end;

      dtSingle, dtDouble, dtExtended, dtReal48:
        begin
          fs := DefaultFormatSettings;
          if not TryStrToFloat(AText, ext, fs) then
          begin
            if fs.DecimalSeparator = '.' then
              fs.DecimalSeparator := ','
            else
              fs.DecimalSeparator := '.';
            if not TryStrToFloat(AText, ext, fs) then
              err := errNumberReq;
          end;
          if err = errOK then
            case AItem.DataType of
              dtSingle:
                if (abs(ext) >= MinSingle) and (abs(ext) <= MaxSingle) then
                begin
                  sng := ext;
                  if AItem.BigEndian then sng := hxUtils.NtoBE(sng) else sng := hxUtils.NtoLE(sng);
                  FHexEditor.WriteBuffer(sng, AItem.Offset, SizeOf(sng));
                end else
                  err := errOutOfRange;
              dtDouble:
                if (abs(ext) >= MinDouble) and (abs(ext) <= MaxDouble) then
                begin
                  dbl := ext;
                  if AItem.BigEndian then dbl := NtoBE(dbl) else dbl := NtoLE(dbl);
                  FHexEditor.WriteBuffer(dbl, AItem.Offset, SizeOf(dbl));
                end else
                  err := errOutOfRange;
              dtExtended:
                begin
                  if SizeOf(ext) <> 10 then
                    raise Exception.Create('Cannot write non ISO-extended.');
                  if AItem.BigEndian then ext := NtoBE(ext) else ext := NtoLE(ext);
                  FHexEditor.WriteBuffer(ext, AItem.Offset, SizeOf(ext));
                end;
              dtReal48:
                try
                  r48 := ext;
                  FHexEditor.WriteBuffer(r48, AItem.Offset, SizeOf(r48));
                except
                  err := errOutOfRange;
                end;
            end;
        end;
    end;

    case err of
      errOK: ;
      errNumberReq:
        MessageDlg('Numeric input required.', mtError, [mbOK], 0);
      errOutOfRange:
        MessageDlg('Value out of range.', mtError, [mbOK], 0);
    end;

  finally
    FHexEditor.InsertMode := oldInsertMode;
    FHexEditor.Invalidate;
  end;
end;

procedure TViewerGrid.UpdateData(AHexEditor: THxHexEditor);
begin
  FHexEditor := AHexEditor;
  if Assigned(FHexEditor) then
    DoUpdateData;
end;

end.

