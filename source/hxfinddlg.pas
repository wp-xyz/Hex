// todo: Implement "text-is-hex" (i.e. search string can be hex chars)
// todo: Fix searching for widestring

unit hxFindDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, Buttons,
  MPHexEditor,
  hxGlobal;

type

  { TFindForm }

  TFindForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    btnSearch: TBitBtn;
    btnClose: TBitBtn;
    btnRepeat: TBitBtn;
    cbIgnoreCase: TCheckBox;
    cbTextIsHex: TCheckBox;
    cmbSearchFor: TComboBox;
    cmbDataTypes: TComboBox;
    gbSearchOptions: TGroupBox;
    Panel1: TPanel;
    txtSearchSequence: TEdit;
    lblHexSearchSequence: TLabel;
    lblDataTypes: TLabel;
    lblSearchFor: TLabel;
    MainPanel: TPanel;
    rgSearchStart: TRadioGroup;
    procedure btnCloseClick(Sender: TObject);
    procedure cmbDataTypesChange(Sender: TObject);
    procedure cmbSearchForChange(Sender: TObject);
    procedure ExecSearch(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgSearchStartClick(Sender: TObject);
    procedure txtSearchSequenceChange(Sender: TObject);
  private
    FHexEditor: TMPHexEditor;
    FBuffer: TBytes;
    FBigEndian: Boolean;
    procedure AddToHistory;
    procedure EnableControls(Found: Boolean);
    procedure PrepareBuffer;
    procedure SelectDataType(AValue: TDataType);
    function SelectedDataType: TDataType;
    procedure TryPrepareBuffer;
    function ValidData(out AControl: TWinControl; out AMsg: String): Boolean;

    procedure ReadIni;
    procedure WriteIni;

  public
    property BigEndian: Boolean read FBigEndian write FBigEndian;
    property HexEditor: TMPHexEditor read FHexEditor write FHexEditor;

  end;

var
  FindForm: TFindForm;

implementation

{$R *.lfm}

uses
  IniFiles,
  Real48Utils,
  hxStrings, hxDatamodule, hxUtils;


{ TFindForm }

procedure TFindForm.AddToHistory;
var
  idx: Integer;
begin
  idx := cmbSearchFor.Items.IndexOf(cmbSearchFor.Text);
  if idx = -1 then begin
    cmbSearchFor.Items.Insert(0, cmbSearchFor.Text);
    while cmbSearchFor.Items.Count > MAX_SEARCH_HISTORY do
      cmbSearchFor.Items.Delete(cmbSearchFor.Items.Count-1);
  end else
    cmbSearchFor.Items.Exchange(0, idx);
end;

procedure TFindForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFindForm.cmbDataTypesChange(Sender: TObject);
begin
  TryPrepareBuffer;
  EnableControls(false);
end;

procedure TFindForm.cmbSearchForChange(Sender: TObject);
begin
  TryPrepareBuffer;
  EnableControls(false);
end;

procedure TFindForm.EnableControls(Found: Boolean);
begin
  btnSearch.Enabled := not Found;
  btnRepeat.Enabled := Found;
  cbIgnoreCase.Enabled := SelectedDataType in [dtCharArray, dtWideCharArray];
  cbTextIsHex.Enabled := cbignoreCase.Enabled;
end;

procedure TFindForm.ExecSearch(Sender: TObject);
var
  foundAt: Integer;
  startPos: Integer;
  findStr: String;
  ignoreCase: Boolean;
  bufferIsText: Boolean;
  C: TWinControl;
  msg: String;
begin
  if not ValidData(C, msg) then begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    exit;
  end;

  try
    FindParams.FindExpression := cmbSearchFor.Text;
    FindParams.SearchStart := TSearchStart(rgSearchStart.ItemIndex);
    PrepareBuffer;
    FindParams.FindDataType := SelectedDataType;
    FindParams.IgnoreCase := cbIgnoreCase.Checked;
    FindParams.TextIsHex := cbTextIsHex.Checked;
    AddToHistory;
  except
    on E: Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      ModalResult := mrNone;
      exit;
    end;
  end;

  with FindParams do
  begin
    if Sender = btnRepeat then
      startPos := FHexEditor.GetCursorPos + 1
    else
    if (SearchStart = ssCursor) then
      startPos := FHexEditor.GetCursorPos
    else
      startPos := 0;

    // Prepare string to be found for HexEditor
    SetLength(findStr, Length(FBuffer));
    Move(FBuffer[0], findStr[1], Length(findStr));
    ignoreCase := FindParams.IgnoreCase;
    bufferIsText := FindParams.FindDataType in [dtCharArray, dtWideCharArray];
    findStr := FHexEditor.PrepareFindReplaceData(findStr, ignoreCase, bufferIsText);

    // Search
    foundAt := FHexEditor.Find(PChar(findStr), Length(findStr), startPos, FHexEditor.DataSize-1, ignoreCase);

    if foundAt = -1 then begin
      EnableControls(false);
      MessageDlg(SNotFound, mtInformation, [mbOK], 0)
    end else
    begin
      FHexEditor.SelStart := foundAt;
      FHexEditor.SelEnd := foundAt + Length(FBuffer) - 1;
      EnableControls(true);
    end;
  end;
end;

procedure TFindForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteIni;
end;

procedure TFindForm.FormCreate(Sender: TObject);
begin
  cmbSearchFor.DropDownCount := DROP_DOWN_COUNT;
  cmbDataTypes.DropDownCount := DROP_DOWN_COUNT;

  ReadIni;

  cmbSearchFor.Text := FindParams.FindExpression;
  CreateDataTypeList(TStringList(cmbDataTypes.Items), [
    dtCharArray, dtWideCharArray, dtByte, dtShortInt, dtWord, dtSmallInt,
    dtLongWord, dtLongInt, dtInt64, dtCurrency, dtSingle, dtDouble,
    dtExtended, dtReal48
  ]);
  SelectDataType(FindParams.FindDataType);
  rgSearchStart.ItemIndex := ord(FindParams.SearchStart);
  cbIgnoreCase.Checked := FindParams.IgnoreCase;
  cbTextIsHex.Checked := FindParams.TextIsHex;

  TryPrepareBuffer;
end;

procedure TFindForm.FormShow(Sender: TObject);
var
  w, h: Integer;
begin
  RgSearchStart.Controls[1].BorderSpacing.Bottom := 6;
  RgSearchStart.Controls[0].BorderSpacing.Left := 12;

  AutoSize := true;
  h := Height;
  AutoSize := false;
  Constraints.MinHeight := h;
end;

procedure TFindForm.PrepareBuffer;
var
  s: string;
  ws: WideString;
  idx: Integer;
begin
  idx := cmbDataTypes.ItemIndex;
  if idx = -1 then
    exit;
  if cmbSearchFor.Text = '' then
    exit;

  s := cmbSearchFor.Text;
  try
    case SelectedDataType of
      dtCharArray:
        begin
          SetLength(FBuffer, Length(s));
          Move(s[1], FBuffer[0], Length(FBuffer));
        end;
      dtWideCharArray:
        begin
          ws := UTF8Decode(s);
          if FBigEndian then
            ws := NtoBE(ws)
          else
            ws := NtoLE(ws);
          SetLength(FBuffer, Length(ws)*2);
          Move(ws[1], FBuffer[0], Length(FBuffer));
        end;
      dtByte:
        begin
          SetLength(FBuffer, 1);
          FBuffer[0] := StrToInt(s);
        end;
      dtShortInt:
        begin
          SetLength(FBuffer, 1);
          FBuffer[0] := ShortInt(StrToInt(s));
        end;
      dtWord:
        FBuffer := WordToBytes(Word(StrToInt(s)), FBigEndian);
      dtSmallInt:
        FBuffer := WordToBytes(SmallInt(StrToInt(s)), FBigEndian);
      dtLongWord:
        FBuffer := DWordToBytes(LongWord(StrToInt(s)), FBigEndian);
      dtLongInt:
        FBuffer := DWordToBytes(LongInt(StrToInt(s)), FBigEndian);
      dtInt64:
        FBuffer := Int64ToBytes(StrToInt(s), FBigEndian);
      dtCurrency:
        FBuffer := CurrToBytes(StrToCurr(s), FBigEndian);
      dtSingle:
        FBuffer := SingleToBytes(Single(StrToFloat(s)), FBigEndian);
      dtDouble:
        FBuffer := DoubleToBytes(Double(StrToFloat(s)), FBigEndian);
      dtExtended:
        FBuffer := ExtendedToBytes(Extended(StrToFloat(s)), FBigEndian);
      dtReal48:
        FBuffer := Real48ToBytes(Double2Real(Double(StrToFloat(s))), FBigEndian);
    end;
    txtSearchSequence.Text := BytesToHex(FBuffer);
  except
    on E:Exception do begin
      txtSearchSequence.Text := E.Message;
      raise;
    end;
  end;
end;

procedure TFindForm.ReadIni;
var
  ini: TCustomIniFile;
  n, i: Integer;
  s: String;
begin
  ini := CreateIniFile;
  try
    ReadFormFromIni(ini, self, INI_SEARCH);
    cbIgnoreCase.Checked := ini.ReadBool(INI_SEARCH, 'IgnoreCase', FindParams.IgnoreCase);
    cbTextIsHex.Checked := ini.ReadBool(INI_SEARCH, 'TextIsHex', FindParams.TextIsHex);
    n := ini.ReadInteger(INI_SEARCH, 'HistoryCount', 0);
    for i:= 1 to n do
    begin
      s := ini.ReadString(INI_SEARCH, Format('HistoryItem%d', [i]), '');
      if (s <> '') then
        cmbSearchFor.Items.Add(s);
    end;
  finally
    ini.Free;
  end;
end;

procedure TFindForm.rgSearchStartClick(Sender: TObject);
begin
  EnableControls(false);
end;

(*
procedure TFindForm.SaveHistory;
var
  ini: TCustomIniFile;
  i: integer;
begin
  ini := CreateIniFile;
  try
    ini.WriteInteger(INI_SEARCH, 'HistoryCount', cmbSearchFor.Items.Count);
    for i := 0 to cmbSearchFor.Items.Count - 1 do
      ini.WriteString(INI_SEARCH, Format('HistoryItem%d', [i+1]), cmbSearchFor.Items[i]);
  finally
    ini.Free;
  end;
end;                                             *)

procedure TFindForm.SelectDataType(AValue: TDataType);
var
  idx: Integer;
begin
  idx := cmbDataTypes.Items.IndexOfObject(TObject(PtrInt(AValue)));
  cmbDataTypes.ItemIndex := idx;
  cmbDataTypesChange(nil);
end;

function TFindForm.SelectedDataType: TDataType;
begin
  Result := TDataType(PtrInt(cmbDataTypes.Items.Objects[cmbDataTypes.ItemIndex]));
end;

procedure TFindForm.TryPrepareBuffer;
begin
  try
    PrepareBuffer;
  except
  end;
end;

procedure TFindForm.txtSearchSequenceChange(Sender: TObject);
begin
  TryPrepareBuffer;
  if SelectedDataType in [dtCharArray, dtWideCharArray] then
  begin
    cbIgnoreCase.Enabled := true;
    cbTextIsHex.Enabled := true;
  end;
end;

function TFindForm.ValidData(out AControl: TWinControl; out AMsg: String): Boolean;
begin
  Result := false;
  if cmbSearchFor.Text = '' then
  begin
    AMsg := 'Please specifiy data to be searched for.';
    AControl := cmbSearchFor;
    exit;
  end;
  if cmbDataTypes.ItemIndex = -1 then
  begin
    AMsg := 'Type of search data must be specified.';
    AControl := cmbDataTypes;
    exit;
  end;
  Result := true;
end;

procedure TFindForm.WriteIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := CreateIniFile;
  try
    ini.EraseSection(INI_SEARCH);
    WriteFormToIni(ini, self, INI_SEARCH);
    FindParams.IgnoreCase := cbIgnoreCase.Checked;
    FindParams.TextIsHex := cbTextIsHex.Checked;
    ini.WriteBool(INI_SEARCH, 'IgnoreCase', FindParams.IgnoreCase);
    ini.WriteBool(INI_SEARCH, 'TextIsHex', FindParams.TextIsHex);
    ini.WriteInteger(INI_SEARCH, 'HistoryCount', cmbSearchFor.Items.Count);
    for i := 0 to cmbSearchFor.Items.Count - 1 do
      ini.WriteString(INI_SEARCH, Format('HistoryItem%d', [i+1]), cmbSearchFor.Items[i]);
  finally
    ini.Free;
  end;
end;


end.

