unit hxSettingsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, StdCtrls, ExtCtrls,
  hxGlobal, hxHexEditor;

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
    cbDrawGutter3D: TCheckBox;
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
    cmbFontSize: TComboBox;
    cmbFontName: TComboBox;
    edMaskChar: TEdit;
    gbDataViewer: TGroupBox;
    gbRecordViewer: TGroupBox;
    gbDataViewerDataTypes: TGroupBox;
    gbObjectViewer: TGroupBox;
    gbSampleHexEditor: TGroupBox;
    gbIcons: TGroupBox;
    lblFont: TLabel;
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
    pbOffice: TPaintBox;
    pbSimpleSmall: TPaintBox;
    pgColors: TTabSheet;
    pgFormat: TTabSheet;
    pgViewer: TTabSheet;
    pgEditor: TTabSheet;
    rbOfficeIconSet: TRadioButton;
    rbSimpleSmallIconSet: TRadioButton;
    rgByteOrder: TRadioGroup;
    pgGUI: TTabSheet;
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure cbOffsetDisplayBaseChange(Sender: TObject);
    procedure cbDataViewerVisibleChange(Sender: TObject);
    procedure cbObjectViewerVisibleChange(Sender: TObject);
    procedure cbRecordViewerVisibleChange(Sender: TObject);
    procedure cbViewOnlyChange(Sender: TObject);
    procedure ColorChanged(Sender: TObject);
    procedure FormatChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pbOfficePaint(Sender: TObject);
    procedure pbSimpleSmallPaint(Sender: TObject);
  private
    FDataTypeCheckBoxes : array[dtFirstNumericDataType..dtLastNumericDataType] of TCheckbox;
    FSampleHexEditor: THxHexEditor;
    procedure DrawIcons(APaintbox: TPaintbox; AImages: TImageList);
    procedure PrepareSampleHexEditor;
    procedure SetEditorData(const AParams: THexParams);
    procedure SetFormatData(const AParams: THexParams);
    procedure SetViewerData(const AParams: THexParams);
    function ValidData(out AControl: TWinControl; out AMsg: String): boolean;

  public
    procedure ColorsFromControls(var AParams: TColorParams);
    procedure ColorsToControls(const AParams: TColorParams);

    procedure GuiParamsFromControls(var AParams: TGuiParams);
    procedure GuiParamsToControls(const AParams: TGuiParams);

    procedure ParamsFromControls(var AParams: THexParams);
    procedure ParamsToControls(const AParams: THexParams);

  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

uses
  hxDataModule, hxUtils;

procedure TSettingsForm.btnRestoreDefaultsClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := PageControl.ActivePageIndex;
  if PageControl.ActivePage = pgColors then
    ColorsToControls(DefaultColorParams)
  else
    ParamsToControls(DefaultHexParams);
  PageControl.ActivePageIndex := idx;
end;

procedure TSettingsForm.FormatChanged(Sender: TObject);
var
  params: THexParams;
begin
  ParamsFromControls(params);
  ApplyParamsToHexEditor(params, FSampleHexEditor);
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

procedure TSettingsForm.cbOffsetDisplayBaseChange(Sender: TObject);
begin
  FormatChanged(Sender);
  cbHexPrefix.Enabled := cbOffsetDisplayBase.ItemIndex = 2;
  lblHexIndicator.Enabled := cbHexPrefix.Enabled;
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

procedure TSettingsForm.ColorChanged(Sender: TObject);
var
  colors: TColorParams;
begin
  ColorsFromControls(colors);
  ApplyColorsToHexEditor(colors, FSampleHexEditor);
end;

procedure TSettingsForm.ColorsFromControls(var AParams: TColorParams);
var
  dt: TDataType;
  i: integer;
  dir: string;
  cmd: string;
begin
  AParams := ColorParams;
  with AParams do
  begin
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
  end;
end;

procedure TSettingsForm.ColorsToControls(const AParams: TColorParams);
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
  ApplyColorsToHexEditor(AParams, FSampleHexEditor);
end;

procedure TSettingsForm.DrawIcons(APaintbox: TPaintbox; AImages: TImageList);
const
  MARGIN = 2;
var
  i: Integer;
  ppi: Integer;
  w: Integer;
  x: Integer;
begin
  ppi := Font.PixelsPerInch;
  w := AImages.WidthForPPI[AImages.Width, ppi];

  APaintbox.Canvas.Brush.Color := clWindow;
  APaintbox.Canvas.FillRect(0, 0, APaintbox.Width, APaintbox.Height);
  x := MARGIN;
  for i:=0 to AImages.Count-1 do
    if x + w + MARGIN <= APaintbox.Width then
    begin
      AImages.Draw(APaintbox.Canvas, x, 0, i);
      inc(x, w + MARGIN);
    end else
      break;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
const
  FONT_SIZES: array[0..14] of PtrInt = (6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 24);
var
  i: PtrInt;
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

  cmbFontName.Items.Assign(Screen.Fonts);
  cmbFontSize.Items.Clear;
  for i in FONT_SIZES do
    cmbFontSize.Items.AddObject(IntToStr(i) + ' pt', TObject(i));

  PrepareSampleHexEditor;

  pbOffice.Height := CommonData.Images.WidthForPPI[CommonData.Images.Width, Font.PixelsPerInch];
  pbSimpleSmall.Height := pbOffice.Height;
end;

procedure TSettingsForm.GuiParamsFromControls(var AParams: TGuiParams);
begin
  if rbOfficeIconSet.Checked then
    AParams.IconSet := isOffice;
  if rbSimpleSmallIconSet.Checked then
    AParams.IconSet := isSimpleSmall;
end;

procedure TSettingsForm.GuiParamsToControls(const AParams: TGuiParams);
begin
  case AParams.IconSet of
    isOffice: rbOfficeIconSet.Checked := true;
    isSimpleSmall: rbSimpleSmallIconSet.Checked := true;
  end;
end;

procedure TSettingsForm.OKButtonClick(Sender: TObject);
var
  C: TWinControl;
  msg: String;
begin
  if not ValidData(C, msg) then begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

procedure TSettingsForm.PageControlChange(Sender: TObject);
begin
  if (PageControl.ActivePage = pgColors) or (PageControl.ActivePage = pgFormat) then
  begin
    gbSampleHexEditor.Parent := PageControl.ActivePage;
    gbSampleHexEditor.AnchorSideLeft.Control := PageControl.ActivePage;
    gbSampleHexEditor.AnchorSideRight.Control := PageControl.ActivePage;
  end;
end;

procedure TSettingsForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  gbSampleHexEditor.AnchorSideLeft.Control := nil;
  gbSampleHexEditor.AnchorSideRight.Control := nil;
end;

procedure TSettingsForm.ParamsFromControls(var AParams: THexParams);
var
  dt: TDataType;
  i: integer;
  s: String;
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

    FontName := cmbFontName.Text;
    FontSize := PtrInt(cmbFontSize.Items.Objects[cmbFontSize.ItemIndex]);
    HexLowerCase := cbHexLowercase.Checked;
    DrawGutter3D := cbDrawGutter3D.Checked;

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
  stream: TStream;
begin
  with AParams do
  begin
    for i := 0 to PageControl.PageCount-1 do
      if PageControl.Pages[i].PageIndex = SettingsPageIndex then
      begin
        PageControl.ActivePage := PageControl.Pages[i];
        PageControlChange(nil);
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
  SetViewerData(AParams);
  //  SetWinData(WinParams);

  ApplyParamsToHexEditor(AParams, FSampleHexEditor);
end;

procedure TSettingsForm.pbOfficePaint(Sender: TObject);
begin
  DrawIcons(pbOffice, CommonData.Images_Office);
end;

procedure TSettingsForm.pbSimpleSmallPaint(Sender: TObject);
begin
  DrawIcons(pbSimpleSmall, CommonData.Images_SimpleSmall);
end;

procedure TSettingsForm.PrepareSampleHexEditor;
var
  stream: TStream;
begin
  FSampleHexEditor.Free;
  FSampleHexEditor := THxHexEditor.Create(self);
  FSampleHexEditor.ParentFont := false;
  FSampleHexEditor.Parent := gbSampleHexEditor;
  FSampleHexEditor.Align := alClient;
  FSampleHexEditor.BorderSpacing.Around := 8;

  // Load some dummy data into the sample hex editor.
  stream := TStringStream.Create(#$1B#$FF#$08#$00'Here is some sample text loaded into our demo HexEditor.'#$0D#$0A#09#$AE);
  try
    FSampleHexEditor.LoadFromStream(stream);
  finally
    stream.Free;
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

    cmbFontName.Text := FontName;
    cmbFontSize.ItemIndex := cmbFontSize.Items.IndexOfObject(TObject(PtrInt(FontSize)));
    cbHexLowercase.Checked := HexLowerCase;
    cbDrawGutter3D.Checked := DrawGutter3D;
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

function TSettingsForm.ValidData(out AControl: TWinControl;
  out AMsg: String): Boolean;
begin
  Result := false;
  if cmbFontName.Text = '' then
  begin
    AControl := cmbFontName;
    AMsg := 'No font selected.';
    exit;
  end;
  if cmbFontSize.ItemIndex = -1 then
  begin
    AControl := cmbFontSize;
    AMsg := 'No font size selected.';
    exit;
  end;
  Result := true;
end;

end.

