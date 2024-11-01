program hex_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UtilsTests,
  hxUtils;//, MPHexEditor,
  //MPHexEditorEx;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

