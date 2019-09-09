// todo: Implement "text-is-hex" (i.e. search string can be hex chars)
// todo: Fix searching for widestring

unit hxSearchReplaceDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, Buttons,
  hxGlobal, hxHexEditor;

type
  TSearchReplaceMode = (srmSearch, srmReplace);

  { TSearchReplaceForm }

  TSearchReplaceForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    bvSeparator: TBevel;
    bvSearchCenter: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    btnSearchReplace: TBitBtn;
    btnClose: TBitBtn;
    btnRepeat: TBitBtn;
    cbIgnoreCase: TCheckBox;
    cbConfirmReplace: TCheckBox;
    cbSearchTextIsHex: TCheckBox;
    cbReplaceTextIsHex: TCheckBox;
    cmbReplaceDataTypes: TComboBox;
    cmbSearchFor: TComboBox;
    cmbSearchDataTypes: TComboBox;
    cmbReplaceBy: TComboBox;
    gbReplace: TGroupBox;
    gbSearchOptions: TGroupBox;
    gbSearch: TGroupBox;
    gbReplaceOptions: TGroupBox;
    lblReplaceDataTypes: TLabel;
    lblHexRepalceSequence: TLabel;
    ButtonPanel: TPanel;
    txtSearchSequence: TEdit;
    lblHexSearchSequence: TLabel;
    lblSearchDataTypes: TLabel;
    MainPanel: TPanel;
    rgSearchStart: TRadioGroup;
    txtReplaceSequence: TEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure cmbSearchDataTypesChange(Sender: TObject);
    procedure cmbSearchForChange(Sender: TObject);
    procedure Execute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgSearchStartClick(Sender: TObject);
    procedure txtSearchSequenceChange(Sender: TObject);
  private
    FHexEditor: THxHexEditor;
    FSearchBuffer: TBytes;
    FReplaceBuffer: TBytes;
    FBigEndian: Boolean;
    FMode: TSearchReplaceMode;
    procedure SetMode(AValue: TSearchReplaceMode);
  protected
    procedure AddToHistory(ACombo: TCombobox);
    procedure ControlsToParams;
    procedure EnableControls(Found: Boolean);
    procedure PrepareBuffer(const AExpression: String; ADataType: TDataType;
      ABytesDisplay: TControl; out ABuffer: TBytes);
    procedure SelectReplaceDataType(AValue: TDataType);
    procedure SelectSearchDataType(AValue: TDataType);
    function SelectedReplaceDataType: TDataType;
    function SelectedSearchDataType: TDataType;
    procedure TryPrepareBuffer(const AExpression: String; ADataType: TDataType;
      ABytesDisplay: TControl; out ABuffer: TBytes);
    procedure UpdateIconSet;
    function ValidData(out AControl: TWinControl; out AMsg: String): Boolean;

    procedure ReadIni;
    procedure WriteIni;

  public
    property BigEndian: Boolean read FBigEndian write FBigEndian;
    property HexEditor: THxHexEditor read FHexEditor write FHexEditor;
    property Mode: TSearchReplaceMode read FMode write SetMode;

  end;

var
  SearchReplaceForm: TSearchReplaceForm;

implementation

{$R *.lfm}

uses
  IniFiles,
  Real48Utils,
  hxStrings, hxDatamodule, hxUtils;


{ TSearchReplaceForm }

procedure TSearchReplaceForm.AddToHistory(ACombo: TCombobox);
var
  idx: Integer;
begin
  idx := ACombo.Items.IndexOf(ACombo.Text);
  if idx = -1 then begin
    ACombo.Items.Insert(0, ACombo.Text);
    while ACombo.Items.Count > MAX_SEARCH_HISTORY do
      ACombo.Items.Delete(ACombo.Items.Count-1);
  end else
    ACombo.Items.Exchange(0, idx);
end;

procedure TSearchReplaceForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TSearchReplaceForm.cmbSearchDataTypesChange(Sender: TObject);
begin
  if (Sender = cmbSearchDataTypes) and (cmbSearchDataTypes.ItemIndex <> -1) then
    TryPrepareBuffer(cmbSearchFor.Text, SelectedSearchDataType, txtSearchSequence, FSearchBuffer)
  else
  if (Sender = cmbReplaceDataTypes) and (cmbReplaceDataTypes.ItemIndex <> -1) then
    TryPrepareBuffer(cmbReplaceBy.Text, SelectedReplaceDataType, txtReplaceSequence, FReplacebuffer);
  EnableControls(false);
end;

procedure TSearchReplaceForm.cmbSearchForChange(Sender: TObject);
begin
  if (Sender = cmbSearchFor) and (cmbSearchDataTypes.ItemIndex <> -1) then
    TryPrepareBuffer(cmbSearchFor.Text, SelectedSearchDataType, txtSearchSequence, FSearchBuffer)
  else
  if (Sender = cmbReplaceBy) and (cmbReplaceDataTypes.ItemIndex <> -1) then
    TryPrepareBuffer(cmbReplaceBy.Text, SelectedReplaceDataType, txtReplaceSequence, FReplacebuffer);
  EnableControls(false);
end;

procedure TSearchReplaceForm.ControlsToParams;
begin
  SearchReplaceParams.SearchExpression := cmbSearchFor.Text;
  SearchReplaceParams.SearchStart := TSearchStart(rgSearchStart.ItemIndex);
  SearchReplaceParams.SearchDataType := SelectedSearchDataType;
  SearchReplaceParams.IgnoreCase := cbIgnoreCase.Checked;
  SearchReplaceParams.SearchTextIsHex := cbSearchTextIsHex.Checked;

  if FMode = srmReplace then
  begin
    SearchReplaceParams.ReplaceExpression := cmbReplaceBy.Text;
    SearchReplaceParams.ReplaceDataType := SelectedReplaceDataType;
    SearchReplaceParams.ReplaceTextIsHex := cbReplaceTextIsHex.Checked;
    SearchReplaceParams.ReplaceConfirmation := cbConfirmReplace.Checked;
  end;
end;

procedure TSearchReplaceForm.EnableControls(Found: Boolean);
begin
  btnSearchReplace.Enabled := not Found;
  btnRepeat.Enabled := Found;
  cbIgnoreCase.Enabled := SelectedSearchDataType in [dtCharArray, dtWideCharArray];
  cbSearchTextIsHex.Enabled := cbignoreCase.Enabled;
end;

procedure TSearchReplaceForm.Execute(Sender: TObject);
var
  foundAt: Integer;
  startPos: Integer;
  s: String;
  bufferIsText: Boolean;
  C: TWinControl;
  msg: String;
  canReplace: Boolean;
begin
  if not ValidData(C, msg) then begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    exit;
  end;

  try
    ControlsToParams;
    PrepareBuffer(
      SearchReplaceParams.SearchExpression,
      SearchReplaceParams.SearchDataType,
      txtSearchSequence,
      FSearchBuffer
    );
    AddToHistory(cmbSearchFor);
    if FMode = srmReplace then
    begin
      PrepareBuffer(
        SearchReplaceParams.ReplaceExpression,
        SearchReplaceParams.ReplaceDataType,
        txtReplaceSequence,
        FReplaceBuffer
      );
      AddToHistory(cmbReplaceBy);
    end;
  except
    on E: Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      ModalResult := mrNone;
      exit;
    end;
  end;

  with SearchReplaceParams do
  begin
    if Sender = btnRepeat then
      startPos := FHexEditor.GetCursorPos + 1
    else
    if (SearchStart = ssCursor) then
      startPos := FHexEditor.GetCursorPos
    else
      startPos := 0;

    // Prepare string to be found for HexEditor
    SetLength(s, Length(FSearchBuffer));
    Move(FSearchBuffer[0], s[1], Length(s));
    bufferIsText := SearchDataType in [dtCharArray, dtWideCharArray];
    s := FHexEditor.PrepareFindReplaceData(s, IgnoreCase, bufferIsText);

    // Search
    foundAt := FHexEditor.Find(PChar(s), Length(s), startPos, FHexEditor.DataSize-1, IgnoreCase);

    if foundAt = -1 then begin
      // not found
      EnableControls(false);
      MessageDlg(SNotFound, mtInformation, [mbOK], 0)
    end else
    begin
      // found
      FHexEditor.SelStart := foundAt;
      FHexEditor.SelEnd := foundAt + Length(FSearchBuffer) - 1;

      // Replace
      if FMode = srmReplace then
      begin
        canReplace := not SearchReplaceParams.ReplaceConfirmation or
          (MessageDlg('Do you really want to replace this byte sequence?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes);
        if canReplace then
        begin
          if Length(FReplaceBuffer) = 0 then
            FHexEditor.DeleteSelection
          else
          begin
            SetLength(s, Length(FReplaceBuffer));
            Move(FReplaceBuffer[0], s[1], Length(s));
            FHexEditor.ReplaceSelection(PChar(s), Length(s), '', false);
            FHexEditor.SelEnd := foundAt + Length(FReplaceBuffer) - 1;
          end;
        end;
      end;

      EnableControls(true);
    end;
  end;
end;

procedure TSearchReplaceForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if CanClose then
    try
      WriteIni;
    except
    end;
end;

procedure TSearchReplaceForm.FormCreate(Sender: TObject);
begin
  bvSearchCenter.Shape := bsSpacer;

  cmbSearchFor.DropDownCount := DROP_DOWN_COUNT;
  cmbSearchDataTypes.DropDownCount := DROP_DOWN_COUNT;
  cmbReplaceBy.DropDownCount := DROP_DOWN_COUNT;
  cmbReplaceDataTypes.DropDownCount := DROP_DOWN_COUNT;

  SetMode(srmSearch);

  ReadIni;
end;

procedure TSearchReplaceForm.FormShow(Sender: TObject);
begin
  RgSearchStart.Controls[1].BorderSpacing.Bottom := 6;
  RgSearchStart.Controls[0].BorderSpacing.Left := 12;

  // Improve layout
  rgSearchStart.AutoSize := false;
  rgSearchStart.AnchorSideRight.Control := bvSearchCenter;
  rgSearchStart.Anchors := rgSearchStart.Anchors + [akRight];
  gbSearchOptions.AutoSize := false;
  gbSearchOptions.Anchors := gbSearchOptions.Anchors + [akRight];
  gbSearchOptions.AnchorSideLeft.Control := bvSearchCenter;
  gbSearchOptions.AnchorSideRight.Control := gbSearch;
  gbSearchOptions.BorderSpacing.Left := 0;
  gbReplace.Width := gbSearch.Width;
  gbReplace.Visible := FMode = srmReplace;
  AutoSize := true;

  UpdateIconSet;
end;

procedure TSearchReplaceForm.PrepareBuffer(const AExpression: String; ADataType: TDataType;
  ABytesDisplay: TControl; out ABuffer: TBytes);
var
  ws: WideString;
  idx: Integer;
begin
  if AExpression = '' then
  begin
    ABuffer := nil;
    exit;
  end;

  try
    case ADataType of
      dtCharArray:
        begin
          SetLength(ABuffer, Length(AExpression));
          Move(AExpression[1], ABuffer[0], Length(ABuffer));
        end;
      dtWideCharArray:
        begin
          ws := UTF8Decode(AExpression);
          if FBigEndian then
            ws := NtoBE(ws)
          else
            ws := NtoLE(ws);
          SetLength(ABuffer, Length(ws)*2);
          Move(ws[1], ABuffer[0], Length(ABuffer));
        end;
      dtByte:
        begin
          SetLength(ABuffer, 1);
          ABuffer[0] := StrToInt(AExpression);
        end;
      dtShortInt:
        begin
          SetLength(ABuffer, 1);
          ABuffer[0] := ShortInt(StrToInt(AExpression));
        end;
      dtWord:
        ABuffer := WordToBytes(Word(StrToInt(AExpression)), FBigEndian);
      dtSmallInt:
        ABuffer := WordToBytes(SmallInt(StrToInt(AExpression)), FBigEndian);
      dtLongWord:
        ABuffer := DWordToBytes(LongWord(StrToInt(AExpression)), FBigEndian);
      dtLongInt:
        ABuffer := DWordToBytes(LongInt(StrToInt(AExpression)), FBigEndian);
      dtInt64:
        ABuffer := Int64ToBytes(StrToInt(AExpression), FBigEndian);
      dtCurrency:
        ABuffer := CurrToBytes(StrToCurr(AExpression), FBigEndian);
      dtSingle:
        ABuffer := SingleToBytes(Single(StrToFloat(AExpression)), FBigEndian);
      dtDouble:
        ABuffer := DoubleToBytes(Double(StrToFloat(AExpression)), FBigEndian);
      dtExtended:
        ABuffer := ExtendedToBytes(Extended(StrToFloat(AExpression)), FBigEndian);
      dtReal48:
        ABuffer := Real48ToBytes(Double2Real(Double(StrToFloat(AExpression))), FBigEndian);
    end;
    ABytesDisplay.Caption := BytesToHex(ABuffer);

  except
    on E:Exception do begin
      ABytesDisplay.Caption := E.Message;
      raise;
    end;
  end;
end;

procedure TSearchReplaceForm.ReadIni;
var
  ini: TCustomIniFile;
  n, i: Integer;
  s: String;
  key: String = INI_SEARCH_REPLACE;
begin
  ini := CreateIniFile;
  try
    ReadFormFromIni(ini, self, key, true);

    cbIgnoreCase.Checked := ini.ReadBool(key, 'IgnoreCase',
      SearchReplaceParams.IgnoreCase);
    cbSearchTextIsHex.Checked := ini.ReadBool(key, 'SearchTextIsHex',
      SearchReplaceParams.SearchTextIsHex);

    n := ini.ReadInteger(key, 'SearchHistoryCount', 0);
    for i:= 1 to n do
    begin
      s := ini.ReadString(key, Format('SearchHistoryItem%d', [i]), '');
      if (s <> '') then
        cmbSearchFor.Items.Add(s);
    end;

    if FMode = srmReplace then
    begin
      cbReplaceTextIsHex.Checked := ini.ReadBool(key, 'ReplaceTextIsHex',
        SearchReplaceParams.ReplaceTextIsHex);
      cbConfirmReplace.Checked := ini.ReadBool(key, 'Confirm',
        SearchReplaceParams.ReplaceConfirmation);
      n := ini.ReadInteger(key, 'ReplaceHistoryCount', 0);
      for i:= 1 to n do
      begin
        s := ini.ReadString(key, Format('ReplaceHistoryItem%d', [i]), '');
        if (s <> '') then
          cmbReplaceBy.Items.Add(s);
      end;
    end;
  finally
    ini.Free;
  end;
end;

procedure TSearchReplaceForm.rgSearchStartClick(Sender: TObject);
begin
  EnableControls(false);
end;

procedure TSearchReplaceForm.SelectReplaceDataType(AValue: TDataType);
begin
  with cmbReplaceDataTypes do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(AValue)));
  cmbSearchDataTypesChange(nil);
end;

procedure TSearchReplaceForm.SelectSearchDataType(AValue: TDataType);
begin
  with cmbSearchDataTypes do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(AValue)));
  cmbSearchDataTypesChange(nil);
end;

function TSearchReplaceForm.SelectedReplaceDataType: TDataType;
begin
  with cmbReplaceDataTypes do
    Result := TDataType(PtrInt(Items.Objects[ItemIndex]));
end;

function TSearchReplaceForm.SelectedSearchDataType: TDataType;
begin
  with cmbSearchDataTypes do
    Result := TDataType(PtrInt(Items.Objects[ItemIndex]));
end;

procedure TSearchReplaceForm.SetMode(AValue: TSearchReplaceMode);
const
  DATA_TYPES: array[0..13] of TDataType = (
    dtCharArray, dtWideCharArray, dtByte, dtShortInt, dtWord, dtSmallInt,
    dtLongWord, dtLongInt, dtInt64, dtCurrency, dtSingle, dtDouble,
    dtExtended, dtReal48
  );
begin
  FMode := AValue;
//  MainCenterBevel.Visible := (FMode = srmReplace);
  gbReplace.Visible := (FMode = srmReplace);

  case FMode of
    srmSearch:
      begin
        Caption := 'Search';
        btnSearchReplace.Caption := 'Search';
        btnSearchReplace.ImageIndex := IMG_INDEX_FIND;
      end;
    srmReplace:
      begin
        Caption := 'Replace';
        btnSearchReplace.Caption := 'Replace';
        btnSearchReplace.ImageIndex := IMG_INDEX_REPLACE;
      end;
  end;

  cmbSearchFor.Text := SearchReplaceParams.SearchExpression;
  CreateDataTypeList(TStringList(cmbSearchDataTypes.Items), DATA_TYPES);
  SelectSearchDataType(SearchReplaceParams.SearchDataType);
  rgSearchStart.ItemIndex := ord(SearchReplaceParams.SearchStart);
  cbIgnoreCase.Checked := SearchReplaceParams.IgnoreCase;
  cbSearchTextIsHex.Checked := SearchReplaceParams.SearchTextIsHex;
  if cmbSearchDataTypes.ItemIndex > -1 then
    TryPrepareBuffer(cmbSearchFor.Text, SelectedSearchDataType, txtSearchSequence, FSearchBuffer);

  if FMode = srmReplace then
  begin
    cmbReplaceBy.Text := SearchReplaceParams.SearchExpression;
    CreateDataTypeList(TStringList(cmbReplaceDataTypes.Items), DATA_TYPES);
    SelectReplaceDataType(SearchReplaceParams.ReplaceDataType);
    cbReplaceTextIsHex.Checked := SearchReplaceParams.ReplaceTextIsHex;
    cbConfirmReplace.Checked := SearchReplaceParams.ReplaceConfirmation;
    if cmbReplaceDataTypes.ItemIndex > -1 then
      TryPrepareBuffer(cmbReplaceBy.Text, SelectedReplaceDataType, txtReplaceSequence, FReplaceBuffer);
  end;
end;

procedure TSearchReplaceForm.TryPrepareBuffer(const AExpression: String;
  ADataType: TDataType; ABytesDisplay: TControl; out ABuffer: TBytes);
begin
  try
    PrepareBuffer(AExpression, ADataType, ABytesDisplay, ABuffer);
  except
  end;
end;

procedure TSearchReplaceForm.txtSearchSequenceChange(Sender: TObject);
begin
  (*
  TryPrepareBuffer;
  if SelectedSearchDataType in [dtCharArray, dtWideCharArray] then
  begin
    cbIgnoreCase.Enabled := true;
    cbSearchTextIsHex.Enabled := true;
  end;
  *)
end;

procedure TSearchReplaceForm.UpdateIconSet;
begin
   btnSearchReplace.Images := CommonData.Images;
   btnRepeat.Images := CommonData.Images;
   btnClose.Images := CommonData.Images;
end;

function TSearchReplaceForm.ValidData(out AControl: TWinControl; out AMsg: String): Boolean;
begin
  Result := false;
  if cmbSearchFor.Text = '' then
  begin
    AMsg := 'Please specifiy data to be searched for.';
    AControl := cmbSearchFor;
    exit;
  end;
  if cmbSearchDataTypes.ItemIndex = -1 then
  begin
    AMsg := 'Type of search data must be specified.';
    AControl := cmbSearchDataTypes;
    exit;
  end;

  if FMode = srmReplace then
  begin
    if (cmbReplaceBy.Text <> '') and (cmbReplaceDataTypes.ItemIndex = -1) then
    begin
      AMsg := 'Type of replacement data must be specified.';
      AControl := cmbReplaceDataTypes;
      exit;
    end;
  end;

  Result := true;
end;

procedure TSearchReplaceForm.WriteIni;
var
  ini: TCustomIniFile;
  i: Integer;
  key: String = INI_SEARCH_REPLACE;
begin
  ini := CreateIniFile;
  try
    ini.EraseSection(key);

    WriteFormToIni(ini, self, key);
    SearchReplaceParams.IgnoreCase := cbIgnoreCase.Checked;
    SearchReplaceParams.SearchTextIsHex := cbSearchTextIsHex.Checked;
    ini.WriteBool(key, 'IgnoreCase', SearchReplaceParams.IgnoreCase);
    ini.WriteBool(key, 'SearchTextIsHex', SearchReplaceParams.SearchTextIsHex);
    ini.WriteInteger(key, 'SearchHistoryCount', cmbSearchFor.Items.Count);
    for i := 0 to cmbSearchFor.Items.Count - 1 do
      ini.WriteString(key, Format('SearchHistoryItem%d', [i+1]), cmbSearchFor.Items[i]);

    if FMode = srmReplace then
    begin
      SearchReplaceParams.ReplaceTextIsHex := cbReplaceTextIsHex.Checked;
      SearchReplaceParams.ReplaceConfirmation := cbConfirmReplace.Checked;
      ini.WriteBool(key, 'ReplaceTextIsHex', SearchReplaceParams.ReplaceTextIsHex);
      ini.WriteBool(key, 'Confirm', SearchReplaceParams.ReplaceConfirmation);
      ini.WriteInteger(key, 'ReplaceHistoryCount', cmbReplaceBy.Items.Count);
      for i := 0 to cmbReplaceBy.Items.Count - 1 do
        ini.WriteString(key, Format('ReplaceHistoryItem%d', [i+1]), cmbReplaceBy.Items[i]);
    end;

  finally
    ini.Free;
  end;
end;


end.

