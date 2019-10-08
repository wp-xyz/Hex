unit hxRecordViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ActnList,
  ComCtrls, StdCtrls, ExtCtrls, Contnrs,
  MPHexEditor,
  hxGlobal, hxViewerItems, hxViewerGrids, hxGridViewerFrame;

type

  { TRecordViewerGrid }

  TRecordViewerGrid = class(TViewerGrid)
  private
    FFileName: String;
    FRecordStart: Integer;
    FRecordSize: Integer;
    function GetItem(ARow: Integer): TRecordDataItem;
    procedure SetItem(ARow: Integer; AItem: TRecordDataitem);
  protected
    function CanEditShow: Boolean; override;
    procedure DefineColumns; override;
    function DistanceToZero(AOffset: Integer; IsForWideString: Boolean): Integer;
    procedure DoUpdateData; override;
    function SelectCell(ACol, ARow: Integer): Boolean; override;
    procedure UpdateSelection(ARow: Integer);
  public
    constructor Create(AOwner: TComponent; AOwnsData: Boolean); override;
    destructor Destroy; override;
    procedure AddItem(AItem: TRecordDataItem);
    procedure Advance(ADirection: Integer);
    procedure ClearItems;
    procedure DeleteItem(ARow: Integer);
    procedure MakePascalRecord(AList: TStrings);
    procedure MoveItemDown;
    procedure MoveItemUp;
//    procedure SaveRecordToFile(const AFileName: String);
    property FileName: String read FFileName;
    property RowItems[ARow: Integer]: TRecordDataItem read GetItem write SetItem;
  end;

  { TRecordViewerFrame }

  TRecordViewerFrame = class(TGridViewerFrame)
    acAdd: TAction;
    acEdit: TAction;
    acSaveAs: TAction;
    acDelete: TAction;
    acLoad: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    acSave: TAction;
    acPrevRecord: TAction;
    acNextRecord: TAction;
    acMakePascalRecord: TAction;
    acClear: TAction;
    acDeletePage: TAction;
    acAddPage: TAction;
    acEditPage: TAction;
    acClearPages: TAction;
    ActionList: TActionList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    TabControl: TTabControl;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acAddExecute(Sender: TObject);
    procedure acAddPageExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure acClearPagesExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeletePageExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acEditPageExecute(Sender: TObject);
    procedure acLoadExecute(Sender: TObject);
    procedure acMakePascalRecordExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acNextRecordExecute(Sender: TObject);
    procedure acPrevRecordExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure ActionListUpdate(AnAction: TBasicAction; var {%H-}Handled: Boolean);
    procedure TabControlChange(Sender: TObject);
  private
    FFileName: String;
    FToolButtons: array of TToolButton;  // stores original order of toolbuttons
    FDataLists: TFPObjectList;
  protected
    function CreateViewerGrid: TViewerGrid; override;
    function GetDefaultColWidths(AIndex: Integer): Integer; override;
    procedure Loaded; override;
    procedure LoadRecordsFromFile(const AFileName: String);
    function RecordViewerGrid: TRecordViewerGrid; inline;
    procedure RestoreToolButtons;
    procedure SaveRecordsToFile(const AFileName: String);
    procedure SetParent(AValue: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectDataList(AIndex: Integer);
    procedure UpdateIconSet; override;
  end;


implementation

{$R *.lfm}

uses
  TypInfo, Math, IniFiles,
  hxDataModule, hxHexEditor, hxRecordEditorForm, hxPascalRecordForm;

{------------------------------------------------------------------------------}
{                           TRecordViewerGrid                                  }
{------------------------------------------------------------------------------}

constructor TRecordViewerGrid.Create(AOwner: TComponent; AOwnsData: Boolean);
begin
  FDataItemClass := TRecordDataItem;
  inherited Create(AOwner, AOwnsData);
end;

destructor TRecordViewerGrid.Destroy;
begin
  HexEditor.SelCount := 0;
  HexEditor.SecondSelStart := -1;
  HexEditor.SecondSelEnd := -1;
  inherited;
end;

procedure TRecordViewerGrid.AddItem(AItem: TRecordDataItem);
begin
  FDataList.Add(AItem);
  RowCount := RowCount + 1;
  UpdateData(HexEditor);
end;

procedure TRecordViewerGrid.Advance(ADirection: Integer);
var
  P : Integer;
begin
  P := HexEditor.GetCursorPos;
  inc(P, ADirection * FRecordSize);
  if (P < 0) or (P >= HexEditor.DataSize) then
    exit;
  HexEditor.SelStart := P;
  UpdateData(HexEditor);
  UpdateSelection(Row);
end;

function TRecordViewerGrid.CanEditShow: Boolean;
var
  item: TDataItem;
  c: TGridColumn;
begin
  Result := inherited;
  // do not allow editing of string data values
  c := ColumnFromGridColumn(Col);
  item := FDataList[Row - FixedRows] as FDataItemClass;
  if (item.DataType in StringDataTypes) and (c.Index = Columns.Count-1) then
    Result := false;
end;

{ Property indexes of TRecordDataItem:
  0=DataType, 1=DataSize, 2=Offset, 3=BigEndian, 4=Name }
procedure TRecordViewerGrid.DefineColumns;
var
  lCol: TGridColumn;
begin
  Columns.BeginUpdate;
  try
    Columns.Clear;

    lCol := Columns.Add;
    lCol.Tag := 4;  // TRecordItem property with index 4: Name
    lCol.Title.Caption := 'Name';
    lCol.Width := 120;
    lCol.SizePriority := 0;
    lCol.ReadOnly := true;

    lCol := Columns.Add;
    lCol.Tag := 0;  // inherited TDataItem property with index 0: DataType
    lCol.Title.Caption := 'Data type';
    lCol.Width := 80;
    lCol.SizePriority := 0;
    lCol.ReadOnly := true;

    lCol := Columns.Add;
    lCol.Tag := 1;  // inherited TDataItem property with index 1: Data size
    lCol.Title.Caption := 'Size';
    lCol.Alignment := taRightJustify;
    lCol.Width := 24;
    lCol.SizePriority := 0;
    lCol.ReadOnly := true;

    lCol := Columns.Add;
    lCol.Tag := -1;  // Value column
    lCol.Title.Caption := 'Value';
    lCol.Width := 100;
    lCol.SizePriority := 1;  // Expand column to fill rest of grid width
    //lCol.ReadOnly := true;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TRecordViewerGrid.ClearItems;
begin
  FDataList.Clear;
  RowCount := FixedRows;
  HexEditor.SelEnd := -1;
  HexEditor.SecondSelStart := -1;
  HexEditor.SecondSelEnd := -1;
  UpdateData(HexEditor);
end;

procedure TRecordViewerGrid.DeleteItem(ARow: Integer);
begin
  FDataList.Delete(ARow - FixedRows);
  RowCount := RowCount - 1;
  UpdateData(HexEditor);
end;

function TRecordViewerGrid.DistanceToZero(AOffset: Integer; IsForWideString: Boolean): Integer;
const
  BUFSIZE = 1024;
var
  buf: array of byte;
  n: Integer;
  P: PChar;
  Pend: PChar;
  Pw: PWideChar;
  PwEnd: PWideChar;
begin
  Result := 0;
  repeat
    if AOffset + BUFSIZE < HexEditor.DataSize then
      n := BUFSIZE
    else
      n := HexEditor.DataSize - AOffset;
    SetLength(buf, n);
    HexEditor.ReadBuffer(buf[0], AOffset, n);
    if IsForWideString then begin
      Pw := PWideChar(@buf[0]);
      PwEnd := Pw + (n + 1) div 2;
      while Pw < PwEnd do
      begin
        if (Pw^ = #0) then
          exit;
        inc(Pw);
        inc(Result, 2);
      end;
    end else
    begin
      P := PChar(@buf[0]);
      PEnd := P + n + 1;
      while P < PEnd do
      begin
        if P^ = #0 then
          exit;
        inc(P);
        inc(Result);
      end;
    end;
    inc(AOffset, BUFSIZE);
  until (AOffset > HexEditor.DataSize);
end;

procedure TRecordViewerGrid.DoUpdateData;
var
  i: Integer;
  item: TDataItem;
  P: Integer;
  b: Byte = 0;
  w: Word = 0;
  n: Integer;
begin
  FRecordStart := HexEditor.GetCursorPos;
  if FDataList = nil then
    exit;

  P := FRecordStart;
  for i := 0 to FDataList.Count - 1 do
  begin
    item := FDataList[i] as FDataItemClass;
    item.Offset := P;
    n := item.DataSize;
    // negative DataSize means: Retrieve the datasize from the record itself.
    if (item.DataSize < 0) then
      case item.DataType of
        dtShortString:
          begin
            HexEditor.ReadBuffer(b, P, 1);
            n := b + 1;
          end;
        dtAnsiString:
          begin
            HexEditor.ReadBuffer(w, P, 2);
            if item.BigEndian then
              n := BEToN(w) + 2
            else
              n := LEToN(w) + 2;
          end;
        dtWideString:
          begin
            HexEditor.ReadBuffer(w, P, 2);
            if item.BigEndian then
              n := BEToN(w) * 2 + 2
            else
              n := LEToN(w) * 2 + 2;
          end;
        dtPChar:
          begin
            n := -(DistanceToZero(item.Offset, false) + 1);
          end;
        dtPWideChar:
          begin
            n := -(DistanceToZero(item.Offset, true) + 2) div 2;
          end;
      end;
    item.DataSize := n;
    inc(P, n);
  end;
  FRecordSize := P - FRecordStart;
  Invalidate;
end;

function TRecordViewerGrid.GetItem(ARow: Integer): TRecordDataItem;
begin
  Result := FDataList[ARow - FixedRows] as TRecordDataItem;
end;
                      (*
procedure TRecordViewerGrid.LoadRecordFromFile(const AFileName: String);
var
  i, i0: Integer;
  lines, parts: TStringList;
  item: TRecordDataItem;
  dt: TDataType;  // DataType
  ds: Integer;    // DataSize
  endian: Boolean;
  sep: String;
begin
  FDataList.Clear;

  FFileName := AFileName;

  lines := TStringList.Create;
  parts := TStringList.Create;
  try
    lines.LoadFromFile(AFileName);
    if lines.Count = 0 then
      raise EHexError.CreateFmt('Empty file "%s"', [AFileName]);

    i0 := 0;
    while (i0 < lines.Count) and ((lines[i0] = '') or (lines[i0][1] = ';')) do
      inc(i0);

    if i0 >= lines.Count then
      raise EHexError.CreateFmt('No contents in file "%s"', [AFileName]);

    if pos(DATA_FIELD_SEPARATOR, lines[i0]) > 0 then
      sep := DATA_FIELD_SEPARATOR
    else if pos(',', lines[i0]) > 0 then
      sep := ','
    else if pos(';', lines[i0]) > 0 then
      sep := ';'
    else if pos(#9, lines[i0]) > 0 then
      sep := #9
    else
      raise EHexError.CreateFmt('Unknown field separator in "%s"', [AfileName]);

    parts.Delimiter := sep[1];
    parts.StrictDelimiter := true;

    for i := i0 to lines.Count - 1 do
    begin
      if lines[i] = '' then
        Continue;

      parts.DelimitedText := lines[i];
      if parts.Count < 3 then
        raise EHexError.Create('Invalid file structure, line "' +  lines[i] + '".');
      if parts[2] = 'BE' then
        endian := true
      else if parts[2] = 'LE' then
        endian := false
      else
        raise EHexError.Create('Invalid file structure, line "' + lines[i] + '".');

      dt := TDataType(GetEnumValue(TypeInfo(TDataType), parts[1]));
      if not InRange(ord(dt), ord(Low(TDataType)), ord(High(TDataType))) then
        raise EHexError.Create('Unknown data type, line "' + lines[i] + '".');

      if dt in StringDatatypes then
        ds := StrToInt(parts[3])
      else
        ds := DataTypeSizes[dt];

      item := TRecordDataItem.Create(parts[0], dt, ds, endian);
      FDataList.Add(item);
    end;

  finally
    parts.Free;
    lines.Free;
  end;
  RowCount := FDataList.Count + FixedRows;
  UpdateData(HexEditor);
end;             *)

procedure TRecordViewerGrid.MakePascalRecord(AList: TStrings);
const
  SPACE = '  ';
  LONG_SPACE = '    ';
var
  i: Integer;
  item: TRecordDataItem;
begin
  AList.BeginUpdate;
  try
    AList.Add('type');
    AList.Add(SPACE + '%s = packed record');
    for i:=0 to FDataList.Count-1 do begin
      item := FDataList[i] as TRecordDataItem;
      case item.DataType of
        dtShortString:
          AList.Add(LONG_SPACE + item.Name + ': String[' + IntToStr(item.DataSize-1) + '];');
        dtAnsiString:
          begin
            AList.Add(LONG_SPACE + item.Name + 'Len: Word;');
            AList.Add(LONG_SPACE + item.Name + ': String;');
          end;
        dtCharArray:
          AList.Add(LONG_SPACE + item.Name + ': Array[0..' + IntToStr(item.DataSize-1) + '] of Char;');
        dtWideString:
          begin
            AList.Add(LONG_SPACE + item.Name + 'Len: Word;');
            AList.Add(LONG_SPACE + item.Name + ': WideString;');
          end;
        dtWideCharArray:
          AList.Add(LONG_SPACE + item.Name + ': Array[0..' + IntToStr(item.DataSize div 2 - 1) + '] of WideChar;');
        else
          AList.Add(LONG_SPACE + item.Name + ': ' + DataTypeNames[item.Datatype] + ';');
      end;
    end;
    AList.Add(SPACE + 'end;');
  finally
    AList.EndUpdate;
  end;
end;

procedure TRecordViewerGrid.MoveItemDown;
begin
  if Row = RowCount - 1 then
    exit;
  FDataList.Exchange(Row - FixedRows, Row - FixedRows + 1);
  Row := Row + 1;
  UpdateData(HexEditor);
end;

procedure TRecordViewerGrid.MoveItemUp;
begin
  if Row = FixedRows then
    exit;
  FDataList.Exchange(Row - FixedRows, Row - FixedRows - 1);
  Row := Row - 1;
  UpdateData(HexEditor);
end;
                      (*
procedure TRecordViewerGrid.SaveRecordToFile(const AFileName: String);
var
  i: integer;
  L: TStringList;
  Ls: TStringList;
  item: TRecordDataItem;
begin
  FFileName := AFileName;

  L := TStringList.Create;
  Ls := TStringList.Create;
  try
    Ls.Delimiter := DATA_FIELD_SEPARATOR;
    for i := 0 to FDataList.Count - 1 do
    begin
      item := FDataList[i] as TRecordDataItem;
      Ls.Clear;
      Ls.Add(item.Name);
      Ls.Add(GetEnumName(TypeInfo(TDataType), Integer(item.DataType)));
      Ls.Add(BigEndianStr[item.BigEndian]);
      if item.DataType in StringDataTypes then
        Ls.Add(IntToStr(item.DataSize));
      L.Add(Ls.DelimitedText);
    end;
    L.SaveToFile(AFileName);
  finally
    Ls.Free;
    L.Free;
  end;
end;
*)
function TRecordViewerGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  if Result and Assigned(HexEditor) and (HexEditor.DataSize > 0) then
  begin
    HexEditor.SelStart := HexEditor.GetCursorPos;
    UpdateData(HexEditor);
    UpdateSelection(ARow);
  end;
end;

procedure TRecordViewerGrid.SetItem(ARow: Integer; AItem: TRecordDataItem);
begin
  (FDataList[ARow - FixedRows] as TRecordDataItem).Assign(AItem);
  UpdateData(HexEditor);
end;

procedure TRecordViewerGrid.UpdateSelection(ARow: Integer);
var
  P: Integer;
  item: TDataItem;
begin
  P := FRecordStart + FRecordSize - 1;
  if P >= HexEditor.DataSize then
    exit;
  HexEditor.SelEnd := P;
  if (HexEditor is THxHexEditor) and (ARow >= FixedRows) then
  begin
    item := FDataList[ARow - FixedRows] as TDataItem;
    THxHexEditor(HexEditor).SecondSelStart := item.Offset;
    THxHexEditor(HexEditor).SecondSelEnd := item.Offset + abs(item.DataSize) - 1;
  end;
end;


{------------------------------------------------------------------------------}
{                            TRecordViewerFrame                                }
{------------------------------------------------------------------------------}

constructor TRecordViewerFrame.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  if FDataLists = nil then
    FDataLists := TFPObjectList.Create;

  SetLength(FToolButtons, Toolbar.ButtonCount);
  for i:=0 to Toolbar.ButtonCount-1 do
    FToolButtons[i] := Toolbar.Buttons[i];
end;

destructor TRecordViewerFrame.Destroy;
begin
  FDataLists.Free;
  inherited;
end;

procedure TRecordViewerFrame.acAddExecute(Sender: TObject);
var
  item: TRecordDataItem = nil;
begin
  if RecordEditor('New record element', item) then
  begin
    RecordViewerGrid.AddItem(item);
  end;
end;

procedure TRecordViewerFrame.acAddPageExecute(Sender: TObject);
var
  idx: Integer;
  lName: String;
begin
  lName := '';
  if InputQuery('New set of record elements', 'Name', lName) then
  begin
    if lName = '' then begin
      MessageDlg('Specify a name for the new set of record elements.', mtError, [mbOK], 0);
      exit;
    end;
    idx := TabControl.Tabs.Add(lName);
    FDataLists.Add(TDataList.Create);
    SelectDataList(idx);
  end;
end;

procedure TRecordViewerFrame.acClearExecute(Sender: TObject);
begin
  if MessageDlg('Do you really want to delete all record elements?',
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
  RecordViewerGrid.ClearItems;
end;

procedure TRecordViewerFrame.acClearPagesExecute(Sender: TObject);
begin
  TabControl.Tabs.Clear;
  FDataLists.Clear;

  TabControl.Tabs.Add('no name');
  FDataLists.Add(TDataList.Create);

  SelectDataList(0);
end;

procedure TRecordViewerFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg('Do you really want to delete this record element?',
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;

  RecordViewerGrid.DeleteItem(RecordViewerGrid.Row);
end;

procedure TRecordViewerFrame.acDeletePageExecute(Sender: TObject);
var
  idx: Integer;
begin
  idx := TabControl.TabIndex;
  if (idx < 0) or (idx >= TabControl.Tabs.Count) then
    exit;
  if MessageDlg('Do you really want to delete this set of record elements?',
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
  FDataLists.Delete(idx);
  TabControl.Tabs.Delete(idx);
  if idx = TabControl.Tabs.Count then
    idx := TabControl.Tabs.Count - 1;
  SelectDataList(idx);
end;

procedure TRecordViewerFrame.acEditExecute(Sender: TObject);
var
  item: TRecordDataItem;
  r: Integer;
begin
  r := FGrid.Row;
  item := RecordViewerGrid.RowItems[r];
  if RecordEditor('Edit record element', item) then
  begin
    RecordViewerGrid.RowItems[r] := item;
  end;
end;

procedure TRecordViewerFrame.acEditPageExecute(Sender: TObject);
var
  lName: String;
begin
  lName := TabControl.Tabs[TabControl.TabIndex];
  if InputQuery('Edit name of record element list', 'Name', lName) then
    TabControl.Tabs[TabControl.TabIndex] := lName;
end;

procedure TRecordViewerFrame.acLoadExecute(Sender: TObject);
begin
  with OpenDialog do
  begin
    InitialDir := ExtractFileDir(FileName);
    FileName := ExtractFileName(FileName);
    if Execute then
      LoadRecordsFromFile(FileName);
  end;
end;

procedure TRecordViewerFrame.acMakePascalRecordExecute(Sender: TObject);
var
  F: TPascalRecordForm;
begin
  F := TPascalRecordForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    RecordViewerGrid.MakePascalRecord(F.Code);
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TRecordViewerFrame.acMoveDownExecute(Sender: TObject);
begin
  RecordViewerGrid.MoveItemDown;
end;

procedure TRecordViewerFrame.acMoveUpExecute(Sender: TObject);
begin
  RecordViewerGrid.MoveItemUp;
end;

procedure TRecordViewerFrame.acNextRecordExecute(Sender: TObject);
begin
  RecordViewerGrid.Advance(+1);
end;

procedure TRecordViewerFrame.acPrevRecordExecute(Sender: TObject);
begin
  RecordViewerGrid.Advance(-1);
end;

procedure TRecordViewerFrame.acSaveAsExecute(Sender: TObject);
begin
  with SaveDialog do
  begin
    InitialDir := ExtractFileDir(FileName);
    FileName := ExtractFileName(FileName);
    if Execute then
      SaveRecordsToFile(FileName);
  end;
end;

procedure TRecordViewerFrame.acSaveExecute(Sender: TObject);
begin
  if RecordViewerGrid.FileName = '' then
    acSaveAsExecute(nil)
  else
    SaveRecordsToFile(RecordViewerGrid.FileName);
end;

procedure TRecordViewerFrame.ActionListUpdate(AnAction: TBasicAction;
  var Handled: Boolean);
var
  hasData: Boolean;
begin
  hasData := (FDataLists.Count > 0) and (FGrid.DataCount > 0);
  if (AnAction = acEdit) or (AnAction = acDelete) or (AnAction = acClear) or
     (AnAction = acSave) or (AnAction = acSaveAs) or
     (AnAction = acPrevRecord) or (AnAction = acNextRecord) or
     (AnAction = acMakePascalRecord) or
     (AnAction = acDeletePage) or (AnAction = acClearPages)
  then
    TAction(AnAction).Enabled := hasData
  else if AnAction = acMoveUp then
    acMoveUp.Enabled := FGrid.Row > FGrid.FixedRows
  else if AnAction = acMoveDown then
    acMoveDown.Enabled := hasData and (FGrid.Row < FGrid.RowCount-1)
  else if AnAction = acEditPage then
    acEditPage.Enabled := FDataLists.Count > 0;
end;

function TRecordViewerFrame.CreateViewerGrid: TViewerGrid;
begin
  Result := TRecordViewerGrid.Create(self, false);
end;

function TRecordViewerFrame.GetDefaultColWidths(AIndex: Integer): Integer;
begin
  Result := DefaultHexParams.RecordViewerColWidths[AIndex];
end;

procedure TRecordViewerFrame.Loaded;
begin
  inherited;
  FGrid.Parent := TabControl;
  FGrid.DataList := TDataList.Create;
  if FDataLists = nil then FDataLists := TFPObjectList.Create;
  FDataLists.Add(FGrid.DataList);
end;

procedure TRecordViewerFrame.LoadRecordsFromFile(const AFileName: String);
var
  i, i0, j: Integer;
  sections, elements, parts: TStringList;
  item: TRecordDataItem;
  dt: TDataType;  // DataType
  ds: Integer;    // DataSize
  endian: Boolean;
  s, sep: String;
  lDataList: TDataList;
  ini: TIniFile;
begin
  TabControl.Tabs.Clear;
  FDataLists.Clear;

  FFileName := AFileName;

  ini := TMemIniFile.Create(AFileName);
  try
    sections := TStringList.Create;
    elements := TStringList.Create;
    parts := TStringList.Create;
    try
      ini.ReadSections(sections);
      for j := 0 to sections.Count - 1 do begin
        ini.ReadSection(sections[j], elements);
        if elements.Count = 0 then
          Continue;
        lDataList := TDataList.Create;
        TabControl.Tabs.Add(sections[j]);
        FDataLists.Add(lDataList);

        for i := 0 to elements.Count - 1 do
        begin
          s := ini.ReadString(sections[j], elements[i], '');
          if i = 0 then
          begin
            if pos(DATA_FIELD_SEPARATOR, s) > 0 then
              sep := DATA_FIELD_SEPARATOR
            else if pos(',', s) > 0 then
              sep := ','
            else if pos('|', s) > 0 then
              sep := '|'
            else if pos(#9, s) > 0 then
              sep := #9
            else
              raise EHexError.CreateFmt('Unknown field separator in "%s", line "%s=%s"', [AFileName, elements[i], s]);
            parts.Delimiter := sep[1];
            parts.StrictDelimiter := true;
          end;

          parts.DelimitedText := s;
          if parts.Count < 2 then
            raise EHexError.CreateFmt('Invalid file structure, line "%s=%s.', [elements[i],s]);
          if parts[1] = 'BE' then
            endian := true
          else if parts[1] = 'LE' then
            endian := false
          else
            raise EHexError.CreateFmt('Invalid file structure, line "%s=%s".', [elements[i], s]);

          dt := TDataType(GetEnumValue(TypeInfo(TDataType), parts[0]));
          if not InRange(ord(dt), ord(Low(TDataType)), ord(High(TDataType))) then
            raise EHexError.CreateFmt('Unknown data type, line "%s=%s".', [elements[i], s]);

          if dt in StringDatatypes then
            ds := StrToInt(parts[2])
          else
            ds := DataTypeSizes[dt];

          item := TRecordDataItem.Create(elements[i], dt, ds, endian);
          lDataList.Add(item);
        end;
      end;
    finally
      parts.Free;
      elements.Free;
      sections.Free;
    end;
  finally
    ini.Free;
  end;
  SelectDataList(0);
end;

function TRecordViewerFrame.RecordViewerGrid: TRecordViewerGrid;
begin
  Result := (FGrid as TRecordViewerGrid);
end;

procedure TRecordViewerFrame.RestoreToolButtons;
var
  i: Integer;
begin
  case Toolbar.Align of
    alLeft, alRight:
      for i := High(FToolButtons) downto 0 do
      begin
        if FToolButtons[i].Style = tbsDivider then
          FToolButtons[i].Height := 5;
        Toolbar.ButtonList.Exchange(i, 0);
      end;
    alTop, alBottom:
      for i := High(FToolButtons) downto 0 do
      begin
        if FToolButtons[i].Style = tbsDivider then
          FToolButtons[i].Width := 5;
        Toolbar.ButtonList.Exchange(i, 0);
      end;
  end;
end;

procedure TRecordViewerFrame.SaveRecordsToFile(const AFileName: String);
var
  ini: TCustomIniFile;
  i, j: Integer;
  lDataList: TDataList;
  item: TRecordDataItem;
  L: TStrings;
begin
  ini := TMemIniFile.Create(AFileName);
  L := TStringList.Create;
  try
    L.Delimiter := DATA_FIELD_SEPARATOR;
    L.StrictDelimiter := true;
    for j := 0 to TabControl.Tabs.Count-1 do
    begin
      lDataList := TDataList(FDataLists[j]);
      for i := 0 to lDataList.Count - 1 do begin
        L.Clear;
        item := lDataList[i] as TRecordDataItem;
        L.Add(GetEnumName(TypeInfo(TDataType), Integer(item.DataType)));
        L.Add(BigEndianStr[item.BigEndian]);
        if item.DataType in StringDataTypes then
          L.Add(IntToStr(item.Datasize));
        ini.WriteString(TabControl.Tabs[j], item.Name, L.DelimitedText);
      end;
    end;
  finally
    L.Free;
    ini.Free;
  end;
end;
{


var
  i: integer;
  L: TStringList;
  Ls: TStringList;
  item: TRecordDataItem;
begin
  FFileName := AFileName;

  L := TStringList.Create;
  Ls := TStringList.Create;
  try
    Ls.Delimiter := DATA_FIELD_SEPARATOR;
    for i := 0 to FDataList.Count - 1 do
    begin
      item := FDataList[i] as TRecordDataItem;
      Ls.Clear;
      Ls.Add(item.Name);
      Ls.Add(GetEnumName(TypeInfo(TDataType), Integer(item.DataType)));
      Ls.Add(BigEndianStr[item.BigEndian]);
      if item.DataType in StringDataTypes then
        Ls.Add(IntToStr(item.DataSize));
      L.Add(Ls.DelimitedText);
    end;
    L.SaveToFile(AFileName);
  finally
    Ls.Free;
    L.Free;
  end;
end;
}

procedure TRecordViewerFrame.SelectDataList(AIndex: Integer);
begin
  if (AIndex > -1) and (FDataLists.Count > 0) then
    FGrid.DataList := TDataList(FDataLists[AIndex])
  else
    FGrid.DataList := nil;
  TabControl.TabIndex := AIndex;
  UpdateData(FGrid.HexEditor);
end;

procedure TRecordViewerFrame.SetParent(AValue: TWinControl);
begin
  inherited SetParent(AValue);
  if (Parent <> nil) then begin
    case Parent.Align of
      alLeft, alRight:
        Toolbar.Align := alTop;
      alTop, alBottom:
        Toolbar.Align := alLeft;
    end;
    ToolBar.AutoSize := true;
    RestoreToolButtons;
  end;
end;

procedure TRecordViewerFrame.TabControlChange(Sender: TObject);
begin
  SelectDataList(TabControl.TabIndex);
end;

procedure TRecordViewerFrame.UpdateIconSet;
begin
  ActionList.Images := CommonData.Images;
  Toolbar.Images := CommonData.Images;
end;


end.

