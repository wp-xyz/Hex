unit hxSettingsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  StdCtrls, ExtCtrls,
  hxGlobal;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnRestoreDefaults: TButton;
    ButtonPanel: TButtonPanel;
    cbBytesPerRow: TComboBox;
    cbBytesPerColumn: TComboBox;
    cbOffsetDisplayBase: TComboBox;
    cbHexPrefix: TComboBox;
    cbRecordViewerVisible: TCheckBox;
    cbObjectViewerVisible: TCheckBox;
    cbViewOnly: TCheckBox;
    cbWriteProtected: TCheckBox;
    cbAllowInsertMode: TCheckBox;
    cbHexLowercase: TCheckBox;
    clbActiveFieldBackground: TColorButton;
    clbChangedBackground: TColorButton;
    clbChangedForeground: TColorButton;
    clbEvenColumnForeground: TColorButton;
    clbOddColumnForeground: TColorButton;
    clbCharFieldForeground: TColorButton;
    clbOffsetBackground: TColorButton;
    clbCurrentOffsetBackground: TColorButton;
    clbOffsetForeground: TColorButton;
    clbCurrentOffsetForeground: TColorButton;
    cmbObjectViewerPosition: TComboBox;
    cmbRulerNumberBase: TComboBox;
    cbRulerVisible: TCheckBox;
    cbDataViewerVisible: TCheckBox;
    cmbDataViewerPosition: TComboBox;
    cbDataViewerByte: TCheckBox;
    cbDataViewerWord: TCheckBox;
    cbDataViewerCurrency: TCheckBox;
    cbDataViewerReal48: TCheckBox;
    cbDataViewerShortInt: TCheckBox;
    cbDataViewerSmallInt: TCheckBox;
    cbDataViewerLongWord: TCheckBox;
    cbDataViewerLongInt: TCheckBox;
    cbDataViewerInt64: TCheckBox;
    cbDataViewerSingle: TCheckBox;
    cbDataViewerDouble: TCheckBox;
    cbDataViewerExtended: TCheckBox;
    clbBackground: TColorButton;
    cmbRecordViewerPosition: TComboBox;
    edMaskChar: TEdit;
    gbDataViewer: TGroupBox;
    gbRecordViewer: TGroupBox;
    gbDataViewerDataTypes: TGroupBox;
    gbObjectViewer: TGroupBox;
    lblMaskChar: TLabel;
    lblCurrentOffsetColor: TLabel;
    lblChangedColor: TLabel;
    lblEvenColumnForegroundColor: TLabel;
    lblOddColumnForegroundColor: TLabel;
    lblCharFieldForegroundColor: TLabel;
    lblWindowColor: TLabel;
    lblOffsetColor: TLabel;
    lblForeground: TLabel;
    lblBackground: TLabel;
    lblRulerNumberbase: TLabel;
    lblRuler: TLabel;
    lblBytesPerRow: TLabel;
    lblBytesPerColumn: TLabel;
    lblOffsetDisplayBase: TLabel;
    lblHexIndicator: TLabel;
    lblActiveFieldBackground: TLabel;
    PageControl: TPageControl;
    pgColors: TTabSheet;
    pgFormat: TTabSheet;
    pgViewer: TTabSheet;
    pgEditor: TTabSheet;
    rgByteOrder: TRadioGroup;
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure cbDataViewerVisibleChange(Sender: TObject);
    procedure cbObjectViewerVisibleChange(Sender: TObject);
    procedure cbRecordViewerVisibleChange(Sender: TObject);
    procedure cbViewOnlyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDataTypeCheckBoxes : array[dtFirstNumericDataType..dtLastNumericDataType] of TCheckbox;
    procedure SetColorData(const AParams: THexParams);
    procedure SetEditorData(const AParams: THexParams);
    procedure SetFormatData(const AParams: THexParams);
    procedure SetViewerData(const AParams: THexParams);

  public
    procedure ParamsFromControls(var AParams: THexParams);
    procedure ParamsToControls(const AParams: THexParams);

  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

uses
  hxDataModule;

procedure TSettingsForm.btnRestoreDefaultsClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := PageControl.ActivePageIndex;
  ParamsToControls(DefaultHexParams);
  PageControl.ActivePageIndex := idx;
end;

procedure TSettingsForm.cbDataViewerVisibleChange(Sender: TObject);
var
  dt: TDataType;
begin
  cmbDataViewerPosition.Enabled := cbDataViewerVisible.Checked;
  for dt := dtFirstNumericDataType to dtLastNumericDataType do
    FDataTypeCheckboxes[dt].Enabled := cbDataViewerVisible.Checked;
end;

procedure TSettingsForm.cbObjectViewerVisibleChange(Sender: TObject);
begin
  cmbObjectViewerPosition.Enabled := cbObjectViewerVisible.Checked;
end;

procedure TSettingsForm.cbRecordViewerVisibleChange(Sender: TObject);
begin
  cmbRecordViewerPosition.Enabled := cbRecordViewerVisible.Checked;
end;

procedure TSettingsForm.cbViewOnlyChange(Sender: TObject);
begin
  cbWriteProtected.Enabled := not cbViewOnly.Checked;
  cbAllowInsertMode.Enabled := not cbViewOnly.Checked;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  with CommonData.Images do
  begin
    GetBitmap(IMG_INDEX_OK, ButtonPanel.OKButton.Glyph);          // OK icon
    GetBitmap(IMG_INDEX_CANCEL, ButtonPanel.CancelButton.Glyph);  // Cancel icon
  end;

  FDataTypeCheckboxes[dtByte] := cbDataViewerByte;
  FDataTypeCheckboxes[dtShortInt] := cbDataViewerShortInt;
  FDataTypeCheckboxes[dtWord] := cbDataViewerWord;
  FDataTypeCheckboxes[dtSmallInt] := cbDataViewerSmallInt;
  FDataTypeCheckboxes[dtLongWord] := cbDataViewerLongWord;
  FDataTypeCheckboxes[dtLongInt] := cbDataViewerLongInt;
  FDataTypeCheckboxes[dtInt64] := cbDataViewerInt64;
  FDataTypeCheckboxes[dtCurrency] := cbDataViewerCurrency;
  FDataTypeCheckboxes[dtSingle] := cbDataViewerSingle;
  FDataTypeCheckboxes[dtDouble] := cbDataViewerDouble;
  FDataTypeCheckboxes[dtExtended] := cbDataViewerExtended;
  FDataTypeCheckboxes[dtReal48] := cbDataViewerReal48;

  rgByteOrder.Controls[1].BorderSpacing.Bottom := 6;
end;

procedure TSettingsForm.ParamsFromControls(var AParams: THexParams);
var
  dt: TDataType;
  i: integer;
  dir: string;
  cmd: string;
begin
  AParams := HexParams;
  with AParams do
  begin
    { PageControl }
    SettingsPageIndex := PageControl.ActivePage.PageIndex;

    { Editor }
    ViewOnly := cbViewOnly.Checked;
    WriteProtected := CbWriteProtected.Checked;
    AllowInsertMode := cbAllowInsertMode.Checked;
    BigEndian := rgByteOrder.ItemIndex = 1;
//    Translation := TTranslationType(RgTranslation.ItemIndex);

    { Format }
    BytesPerRow := StrToInt(cbBytesPerRow.Text);
    BytesPerColumn := StrToInt(cbBytesPerColumn.Text);
    OffsetDisplayBase := TOffsetDisplayBase(cbOffsetDisplayBase.ItemIndex);
    if cbHexPrefix.ItemIndex = 0 then
      OffsetDisplayHexPrefix := ''
    else
      OffsetDisplayHexPrefix := copy(cbHexPrefix.Text, 1, pos(' ', cbHexPrefix.Text) - 1);

    RulerVisible := cbRulerVisible.Checked;
    RulerNumberBase := TOffsetDisplayBase(cmbRulerNumberBase.ItemIndex+1);
    if edMaskChar.Text = '' then
      MaskChar := ' '
    else
      MaskChar := edMaskChar.Text[1];

    HexLowerCase := cbHexLowercase.Checked;

    { Colors }
    BackgroundColor := clbBackground.ButtonColor;
    ActiveFieldBackgroundColor := clbActiveFieldBackground.ButtonColor;
    OffsetBackgroundColor := clbOffsetBackground.ButtonColor;
    OffsetForegroundColor := clbOffsetForeground.ButtonColor;
    CurrentOffsetBackgroundColor := clbCurrentOffsetBackground.ButtonColor;
    CurrentOffsetForegroundColor := clbCurrentOffsetForeground.ButtonColor;
    EvenColumnForegroundColor := clbEvenColumnForeground.ButtonColor;
    OddColumnForegroundColor := clbOddColumnForeground.ButtonColor;
    ChangedBackgroundColor := clbChangedBackground.ButtonColor;
    ChangedForegroundColor := clbchangedForeground.ButtonColor;
    CharFieldForegroundColor := clbCharFieldForeground.ButtonColor;
    {
    PositionTextColor := BtnPositionTextColor.SelectionColor;
    PositionbackgroundColor := BtnPositionBackgroundColor.SelectionColor;
    CursorFrameColor := BtnCursorFrameColor.SelectionColor;

    EditorFontName := FontInfo.Font.Name;
    EditorFontSize := FontInfo.Font.Size;
    EditorFontStyle := FontInfo.Font.Style;
    }

    { NumViewer }
    DataViewerVisible := cbDataViewerVisible.Checked;
    DataViewerPosition := TViewerPosition(cmbDataViewerPosition.ItemIndex);
    DataViewerDataTypes := [];
    for dt := dtFirstNumericDataType to dtLastNumericDataType do
      if FDataTypeCheckBoxes[dt].Checked then Include(DataViewerDataTypes, dt);
    cbDataViewerVisibleChange(nil);

    { ObjectViewer }
    ObjectViewerVisible := cbObjectViewerVisible.Checked;
    ObjectViewerPosition := TViewerPosition(cmbObjectViewerPosition.ItemIndex);
    cbObjectViewerVisibleChange(nil);

    { RecordViewer }
    RecordViewerVisible := cbRecordViewerVisible.Checked;
    RecordViewerPosition := TViewerPosition(cmbRecordViewerPosition.ItemIndex);
    cbRecordViewerVisibleChange(nil);

    {ViewerVisible[vtStdViewer] := cbDataViewerVisible.Checked;
    ViewerVisible[vtRecordViewer] := CbRecordViewerVisible.Checked;
    for i:=1 to 4 do begin
      if FStdViewerPosRadioButtons[i].Checked
        then ViewerPos[vtStdViewer] := FStdViewerPosRadioButtons[i].Tag;
      if FRecordViewerPosRadioButtons[i].Checked
        then ViewerPos[vtRecordViewer] := FRecordViewerPosRadioButtons[i].Tag;
    end;
    KeepViewerLayout := CbKeepViewerLayout.Checked;

    RipperActive := CbRipperActive.Checked;
    RipperImmediateShow := CbRipperImmediateShow.Checked;

    with WinParams do begin
      cmd := Format('"%s" "%%1"', [ParamStr(0)]);
      AddToContextMenu := CbAddToContextMenu.Checked;
      if AddToContextmenu
        then AddToExplorerContextmenu('*', 'Hex-Editor', cmd)
        else AddToExplorerContextmenu('*', 'Hex-Editor', '');
(*
      dir := ExtractFileDir(ParamStr(0));
      cmd := dir+'\HexReg.exe';
      try
        if AddToContextMenu
          then RunApp(cmd, ' /enable',  dir)
          else RunApp(cmd, ' /disable', dir);
      except
      end;
*)

      AddToSendTo := CbAddToSendToMenu.Checked;
      AddToSendToMenu(AddToSendTo);

      AllowDragAndDrop := CbAllowDragAndDrop.Checked;
      SingleInstance := CbSingleInstance.Checked;
    end;
    }

    //CbStdViewerVisibleClick(self);
  end;
end;

procedure TSettingsForm.ParamsToControls(const AParams: THexParams);
var
  i : integer;
begin
  with AParams do
  begin
    for i := 0 to PageControl.PageCount-1 do
      if PageControl.Pages[i].PageIndex = SettingsPageIndex then
      begin
        PageControl.ActivePage := PageControl.Pages[i];
        break;
      end;

    for i:=0 to PageControl.ActivePage.ControlCount-1 do
      if PageControl.ActivePage.Controls[i] is TWinControl then
      begin
        ActiveControl := PageControl.ActivePage.Controls[i] as TWinControl;
        break;
      end;
  end;

  SetEditorData(AParams);
  SetFormatData(AParams);
  SetColorData(AParams);
  SetViewerData(AParams);
//  SetWinData(WinParams);
end;

procedure TSettingsForm.SetColorData(const AParams: THexParams);
begin
  with AParams do begin
    clbBackground.ButtonColor := BackgroundColor;
    clbActiveFieldBackground.ButtonColor := ActiveFieldBackgroundColor;
    clbOffsetBackground.ButtonColor := OffsetBackgroundColor;
    clbOffsetForeground.ButtonColor := OffsetForegroundColor;
    clbCurrentOffsetBackground.ButtonColor := CurrentOffsetBackgroundColor;
    clbCurrentOffsetForeground.ButtonColor := CurrentOffsetForegroundColor;
    clbEvenColumnForeground.ButtonColor := EvenColumnForegroundColor;
    clbOddColumnForeground.ButtonColor := OddColumnForegroundColor;
    clbChangedBackground.ButtonColor := ChangedBackgroundColor;
    clbChangedForeground.ButtonColor := ChangedForegroundColor;
    clbCharFieldForeground.ButtonColor := CharfieldForegroundColor;
  end;
end;

procedure TSettingsForm.SetEditorData(const AParams: THexParams);
begin
  with AParams do begin
    cbViewOnly.Checked := ViewOnly;
    cbWriteProtected.Checked := WriteProtected;
    cbAllowInsertMode.Checked := AllowInsertMode;
    if BigEndian then rgByteOrder.ItemIndex := 1 else rgByteOrder.ItemIndex := 0;
  end;
  cbViewOnlyChange(nil);
end;

procedure TSettingsForm.SetFormatData(const AParams: THexParams);
var
  i: integer;
begin
  with AParams do begin
    //RgTranslation.ItemIndex := ord(Translation);

    for i:=0 to cbBytesPerRow.Items.Count-1 do begin
      if StrToInt(cbBytesPerRow.Items[i]) = BytesPerRow then
      begin
        cbBytesPerRow.ItemIndex := i;
        break;
      end;
    end;

    for i:=0 to cbBytesPerColumn.Items.Count-1 do begin
      if StrToInt(cbbytesPerColumn.Items[i]) = BytesPerColumn then
      begin
        cbBytesPerColumn.ItemIndex := i;
        break;
      end;
    end;

    cbOffsetDisplayBase.ItemIndex := ord(OffsetDisplayBase);

    if OffsetDisplayHexPrefix = '' then
      cbHexPrefix.ItemIndex := 0  // 'none'
    else
      for i := 0 to cbHexPrefix.Items.Count-1 do
        if Pos(OffsetDisplayHexPrefix, cbHexPrefix.Items[i]) = 1 then
        begin
          cbHexPrefix.ItemIndex := i;
          break;
        end;

    cbRulerVisible.Checked := RulerVisible;
    cmbRulerNumberBase.ItemIndex := ord(RulerNumberBase) - 1;
    edMaskChar.Text := String(MaskChar);

    cbHexLowercase.Checked := HexLowerCase;


    (*
    FontInfo.Font.Name := EditorFontName;
    FontInfo.Font.Size := EditorFontSize;
    FontInfo.Font.Style := EditorFontStyle;
    *)
  end;
end;

procedure TSettingsForm.SetViewerData(const AParams: THexParams);
var
  dt: TDataType;
  i: integer;
begin
  with AParams do
  begin
    cbDataViewerVisible.Checked := DataViewerVisible;
    cmbDataViewerPosition.ItemIndex := ord(DataViewerPosition);
    for dt := dtFirstNumericDataType to dtLastNumericDataType do
      FDataTypeCheckboxes[dt].Checked := dt in DataViewerDataTypes;

    cbObjectViewerVisible.Checked := ObjectViewerVisible;
    cmbObjectViewerPosition.ItemIndex := ord(ObjectViewerPosition);

    cbRecordViewerVisible.Checked := RecordViewerVisible;
    cmbRecordViewerPosition.ItemIndex := ord(RecordViewerPosition);
  end;
end;

end.

