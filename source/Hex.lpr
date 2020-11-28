program Hex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, hxMain, hxHexEditorFrame, MPHexEditor, mrumanager, MPHexEditorEx,
  hxDataModule, hxViewerItems, hxViewerGrids, hxBasicViewerFrame,
  hxDataViewerFrame, hxRecordViewerFrame, hxObjectViewerFrame, hxSettingsDlg,
  hxGotoDlg, hxRecordEditorForm, hxGridViewerFrame, hxAbout, hxPascalRecordForm,
  hxSearchReplaceDlg, hxHexEditor;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCommonData, CommonData);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

