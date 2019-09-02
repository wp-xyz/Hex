unit hxRecordEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  ExtCtrls, Spin,
  hxGlobal, hxViewerItems;

type

  { TRecordEditorForm }

  TRecordEditorForm = class(TForm)
    ButtonPanel: TButtonPanel;
    edName: TEdit;
    gbCharCount: TGroupBox;
    lblCharCount: TLabel;
    lblSizeInfo: TLabel;
    lblName: TLabel;
    MainPanel: TPanel;
    rbUnlimited: TRadioButton;
    rbAtMost: TRadioButton;
    rgDataTypes: TRadioGroup;
    seCharCount: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure rgDataTypesClick(Sender: TObject);
    procedure seCharCountChange(Sender: TObject);
  private
    procedure ControlsToDataItem(var AItem: TRecordDataItem);
    procedure DataItemToControls(AItem: TRecordDataItem);
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean;

  public

  end;

var
  RecordEditorForm: TRecordEditorForm;

function RecordEditor(const ACaption: String; var AItem: TRecordDataItem): Boolean;

implementation

{$R *.lfm}

uses
  hxDataModule;

function RecordEditor(const ACaption: String; var AItem: TRecordDataItem): Boolean;
var
  F: TRecordEditorForm;
begin
  F := TRecordEditorForm.Create(nil);
  try
    F.Caption := ACaption;
    F.DataItemToControls(AItem);
    if F.ShowModal = mrOK then
    begin
      F.ControlsToDataItem(AItem);
      Result := true;
    end else
      Result := false;
  finally
    F.Free;
  end;
end;


{ TRecordEditorForm }

procedure TRecordEditorForm.ControlsToDataItem(var AItem: TRecordDataItem);
var
  dt: TDataType;
  ds: Integer;
begin
  dt := TDataType(rgDataTypes.ItemIndex);
  case dt of
    dtShortString, dtAnsiString, dtPChar:
      if rbUnlimited.Checked then
        ds := -1
      else
        ds := seCharCount.Value + 1;
    dtWideString, dtPWideChar:
      if rbUnlimited.Checked then
        ds := -1
      else
        ds := (seCharCount.Value + 1) * 2;
    dtCharArray:
      ds := seCharCount.Value;
    dtWideCharArray:
      ds := seCharCount.Value * 2;
    else
      ds := DataTypeSizes[dt];
  end;

  if AItem = nil then
    AItem := TRecordDataItem.Create(edName.Text, dt, ds, HexParams.BigEndian)
  else
  begin
    AItem.Name := edName.Text;
    AItem.DataType := dt;
    AItem.DataSize := ds;
  end;
end;

procedure TRecordEditorForm.DataItemToControls(AItem: TRecordDataItem);
begin
  if AItem = nil then
  begin
    edName.Text := '';
    rgDataTypes.ItemIndex := -1;
    seCharCount.Value := -1;
  end else
  begin
    edName.Text := AItem.Name;
    rgDataTypes.ItemIndex := ord(AItem.DataType);
    if AItem.DataSize = -1 then
    begin
      rbUnlimited.Checked := true;
      seCharCount.Value := -1;
    end else
    begin
      rbAtMost.Checked := true;
      case AItem.DataType of
        dtShortString,
        dtAnsiString,
        dtPChar:
          seCharCount.Value := AItem.DataSize - 1;
        dtCharArray:
          seCharCount.Value := AItem.DataSize;
        dtWideString,
        dtPWideChar:
          seCharCount.Value := (AItem.DataSize - 2) div 2;
        dtWideCharArray:
          seCharCount.value := AItem.DataSize div 2;
        else
          seCharCount.Value := -1;
      end;
    end;
  end;
end;

procedure TRecordEditorForm.FormCreate(Sender: TObject);
var
  dt: TDataType;
begin
  with CommonData.Images do
  begin
    GetBitmap(IMG_INDEX_OK, ButtonPanel.OKButton.Glyph);          // OK icon
    GetBitmap(IMG_INDEX_CANCEL, ButtonPanel.CancelButton.Glyph);  // Cancel icon
  end;

  rgDataTypes.Controls[4].BorderSpacing.Bottom := 8;
  lblSizeInfo.Caption := '';
end;

procedure TRecordEditorForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  H := Height;
  AutoSize := false;
  Height := H;
  rgDataTypesClick(nil);
end;

procedure TRecordEditorForm.OKButtonClick(Sender: TObject);
var
  msg: String;
  C: TWinControl;
begin
  if not ValidData(msg, C) then
  begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

procedure TRecordEditorForm.rgDataTypesClick(Sender: TObject);
var
  dt: TDataType;
begin
  dt := TDataType(rgdataTypes.ItemIndex);
  case dt of
    dtShortString, dtAnsiString:
      lblSizeInfo.Caption := 'w/o length byte';
    dtPChar:
      lblSizeInfo.Caption := 'w/o terminating zero byte';
    dtCharArray,
    dtWideCharArray:
      lblSizeInfo.Caption := 'all characters';
    dtWideString:
      lblSizeInfo.Caption := 'w/o length word';
    dtPWideChar:
      lblSizeInfo.Caption := 'w/o terminating zero word';
  end;
  gbCharCount.Visible := dt in StringDataTypes;
  rbUnlimited.Visible := not (dt in [dtCharArray, dtWideCharArray]);
  rbAtMost.Visible := rbUnlimited.Visible;
end;

procedure TRecordEditorForm.seCharCountChange(Sender: TObject);
begin
  rbAtMost.Checked := true;
end;

function TRecordEditorForm.ValidData(out AMsg: String;
  out AControl: TWinControl): Boolean;
var
  dt: TDataType;
begin
  Result := false;
  if (edName.Text = '') then
  begin
    AMsg := 'The record element must have a name';
    AControl := edName;
    exit;
  end;

  if rgDataTypes.ItemIndex = -1 then
  begin
    AMsg := 'No data type selected.';
    AControl := rgDataTypes;
    exit;
  end;

  dt := TDataType(rgDataTypes.ItemIndex);
  if (dt In StringDataTypes) then
  begin
    if (not rbUnlimited.Checked) and (not rbAtMost.Checked) then
    begin
      AMsg := 'Size not specified';
      AControl := rbAtMost;
      exit;
    end;

    if rbAtMost.Checked and (seCharCount.Value < 1) then
    begin
      AMsg := 'Size not specified.';
      AControl := seCharCount;
      exit;
    end;
  end;

  Result := true;
end;

end.
