program Hex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, hxMain, hxHexEditorFrame, MPHexEditor, mrumanager,
  hxDataModule, hxViewerItems, hxViewerGrids, hxViewerPanel,
  hxBasicViewerFrame, hxNumViewerFrame, hxRecordViewerFrame, hxObjectViewerFrame,
  hxSettingsDlg, hxGotoDlg, hxRecordEditorForm, hxGridViewerFrame;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCommonData, CommonData);
  Application.Run;
end.

