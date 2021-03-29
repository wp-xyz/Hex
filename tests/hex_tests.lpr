program hex_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UtilsTests, MPHexEditor, MPHexEditorEx;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

