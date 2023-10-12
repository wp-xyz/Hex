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
    AsString: String;
  end;

procedure TUtilsTests.ExtendedTest;
const
  EPS = 1E-8;
  TestCases: array[0..12] of TExtendedTestCase = (
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FF, $3F); AsString: '1'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $00, $40); AsString: '2'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FD, $3F); AsString: '0.25'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FD, $BF); AsString: '-0.25'),
    (Bytes: ($A1, $A6, $FE, $D9, $6B, $39, $06, $8F, $F3, $59); AsString: '1.2345E2000'),
    (Bytes: ($CA, $B1, $B3, $79, $08, $40, $94, $AE, $0B, $26); AsString: '1.2345E-2000'),
    (Bytes: ($A1, $A6, $FE, $D9, $6B, $39, $06, $8F, $F3, $D9); AsString: '-1.2345E2000'),
    (Bytes: ($CA, $B1, $B3, $79, $08, $40, $94, $AE, $0B, $A6); AsString: '-1.2345E-2000'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00); AsString: '0'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $00, $00, $80); AsString: '-0'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FF, $7F); AsString: '+Inf'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $80, $FF, $FF); AsString: '-Inf'),
    (Bytes: ($00, $00, $00, $00, $00, $00, $00, $C0, $FF, $FF); AsString: 'Nan')
  );
var
  i, p: Integer;
  actual: String;
  expected: String;
  actualMant, expectedMant: Double;
  actualExp, expectedExp: Integer;
  decSep: Char;
begin
  decSep := Formatsettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  for i := Low(TestCases) to High(TestCases) do
  begin
    expected := TestCases[i].AsString;
    actual := ExtendedToString(TestCases[i].Bytes);

    if (expected = '+Inf') or (expected = '-Inf') or (expected = 'Nan') then
      CheckEquals(expected, actual, Format('Test #%d mismatch: +Inf case', [i]))
    else
    begin
      // Do not compare directly because there may be small rounding errors
      p := pos('E', actual);
      if p > 0 then
      begin
        actualMant := StrToFloat(copy(actual, 1, p-1));
        actualExp := StrToInt(copy(actual, p+1, Length(actual)));
      end else
      begin
        actualMant := StrToFloat(actual);
        actualExp := 0;
      end;

      p := pos('E', expected);
      if p > 0 then
      begin
        expectedMant := StrToFloat(copy(expected, 1, p-1));
        expectedExp := StrToInt(copy(expected, p+1, Length(expected)));
      end else
      begin
        expectedMant := StrToFloat(expected);
        expectedExp := 0;
      end;
      CheckEquals(expectedMant, actualmant, EPS, Format('Test #%d mismatch in mantissa', [i]));
      CheckEquals(expectedExp, actualExp, Format('Test %d mismatich in exponent', [i]));
    end;
  end;
  FormatSettings.DecimalSeparator := decSep;
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

