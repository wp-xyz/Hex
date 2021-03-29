unit UtilsTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TUtilsTests = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ExtendedTest;
  end;

implementation

uses
  hxUtils;

type
  TExtendedTestCase = record
    Bytes: TExtended10;
    DblValue: Double;
  end;

procedure TUtilsTests.ExtendedTest;
const
  TestCases: array[0..3] of TExtendedTestCase = (
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FF, $3F); DblValue:  1.0),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $00, $40); DblValue:  2.0),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FD, $3F); DblValue:  0.25),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FD, $BF); DblValue: -0.25)
  );
var
  i: Integer;
  dbl: Double;
begin
  for i := Low(TestCases) to High(TestCases) do
  begin
    dbl := ExtendedToDouble(TestCases[i].Bytes);
    CheckEquals(TestCases[i].DblValue, dbl, Format('Test #%d mismatch', [i]));
  end;
end;

procedure TUtilsTests.SetUp;
begin
  //
end;

procedure TUtilsTests.TearDown;
begin
  //
end;

initialization
  RegisterTest(TUtilsTests);

end.

