 { Unit with utilities for the Hex application that are independent of the GUI }

 unit hxUtils_NonGui;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, FileInfo,
  hxGlobal;

type
  TExtended10 = packed array[0..9] of byte;     // covers the case that "extended" is not 10 bytes
  PExtended10 = ^TExtended10;

// Conversion
function IntToOctal(AValue: Integer): String;
function BEToN(AValue: Single): Single; overload;
function BEToN(AValue: Double): Double; overload;
function BEToN(AValue: Real48): Real48; overload;
function BEToN(AValue: Currency): Currency; overload;
function BEToN(AValue: WideString): WideString; overload;
function LEToN(AValue: Single): Single; overload;
function LEToN(AValue: Double): Double; overload;
function LEToN(AValue: Real48): Real48; overload;
function LEToN(AValue: Currency): Currency; overload;
function LEToN(AValue: WideString): WideString; overload;
function NtoBE(AValue: Single): Single; overload;
function NtoBE(AValue: Double): Double; overload;
function NtoBE(AValue: Real48): Real48; overload;
function NtoBE(AValue: Currency): Currency; overload;
function NToBE(AValue: WideString): WideString; overload;
function NtoLE(AValue: Single): Single; overload;
function NtoLE(AValue: Double): Double; overload;
function NtoLE(AValue: Real48): Real48; overload;
function NtoLE(AValue: Currency): Currency; overload;
function NToLE(AValue: WideString): WideString; overload;

{$IF SizeOf(Extended) = 10}
function BEToN(AValue: Extended): Extended; overload;
function LEToN(AValue: Extended): Extended; overload;
function NtoBE(AValue: Extended): Extended; overload;
function NtoLE(AValue: Extended): Extended; overload;
{$ELSE}
// replacement when extended <> 10 bytes
function BEToN(AValue: TExtended10): TExtended10; overload;
function LEToN(AValue: TExtended10): TExtended10; overload;
function NtoBE(AValue: TExtended10): TExtended10; overload;
function NtoLE(AValue: TExtended10): TExtended10; overload;
{$IFEND}
function ExtendedToString(x: TExtended10): String;

// Data types
procedure CreateDataTypeList(AList: TStrings; const ADataTypes: Array of TDataType);
function ByteToBytes(AValue: byte): TBytes;
function WordToBytes(AValue: word; BigEndian: Boolean): TBytes;
function DWordToBytes(AValue: DWord; BigEndian: Boolean): TBytes;
function Int64ToBytes(AValue: DWord; BigEndian: Boolean): TBytes;
function CurrToBytes(AValue: Currency; BigEndian: Boolean): TBytes;
function SingleToBytes(AValue: single; BigEndian: Boolean): TBytes;
function DoubleToBytes(AValue: double; BigEndian: Boolean): TBytes;
function ExtendedToBytes(AValue: Extended; BigEndian: Boolean): TBytes;
function ExtendedToBytes(AValue: TExtended10; BigEndian: Boolean): TBytes;  // replacement when extended <> 10 bytes
function Real48ToBytes(AValue: Real48; BigEndian: Boolean): TBytes;

function BytesToHex(AValue: TBytes): String;

// misc
procedure Exchange(var A, B: Integer);
procedure EnsureOrder(var A, B: Integer);
function GetVersionStr: String;

implementation

{==============================================================================}
{  Conversion                                                                  }
{==============================================================================}

function IntToOctal(AValue: integer): String;
// https://stackoverflow.com/questions/16778861/base-converter-binary-to-octal
const
  tt: array[0..7] of char = ('0', '1', '2', '3', '4', '5', '6', '7');
var
  tempval: integer;
begin
  Result := '';
  tempval := AValue;
  if tempval = 0 then
    Result := '0'
  else
    while (tempval <> 0) do
    begin
      Result := tt[(tempval and $7)] + Result;
      tempval := tempval shr 3;
    end;
end;

procedure SwapBytes(var AValue; NumBytes: Integer);
var
  b: array[0..15] of byte absolute AValue;
  i, j: Integer;
  tmp: byte;
begin
  j := NumBytes - 1;
  for i := 0 to pred(NumBytes div 2) do
  begin
    tmp := b[i];
    b[i] := b[j];
    b[j] := tmp;
    dec(j);
  end;
end;

procedure BEtoN_Helper(var AValue; NumBytes: Integer);
begin
  {$IFNDEF ENDIAN_BIG}
  SwapBytes(AValue, NumBytes);
  {$ENDIF}
end;

procedure LEtoN_Helper(var {%H-}AValue; {%H-}NumBytes: Integer);
begin
  {$IFDEF ENDIAN_BIG}
  SwapBytes(AValue, NumBytes);
  {$ENDIF}
end;

procedure NtoBE_Helper(var AValue; NumBytes: Integer);
begin
  {$IFNDEF ENDIAN_BIG}
  SwapBytes(AValue, NumBytes);
  {$ENDIF}
end;

procedure NtoLE_Helper(var {%H-}AValue; {%H-}NumBytes: Integer);
begin
  {$IFDEF ENDIAN_BIG}
  SwapBytes(AValue, NumBytes);
  {$ENDIF}
end;

function BEToN(AValue: Single): Single; overload;
begin
  Result := AValue;
  BEtoN_Helper(Result, SizeOf(Single));
end;

function BEToN(AValue: Double): Double; overload;
begin
  Result := AValue;
  BEToN_Helper(Result, SizeOf(Double));
end;

{$IF SizeOf(Extended) = 10}
function BEToN(AValue: Extended): Extended; overload;
begin
  Result := AValue;
  BEToN_Helper(Result, SizeOf(Extended));
end;
{$ELSE}
function BEToN(AValue: TExtended10): TExtended10; overload;
begin
  Result := AValue;
  BEToN_Helper(Result, Sizeof(TExtended10));
end;
{$IFEND}

function BEToN(AValue: Real48): Real48; overload;
begin
  Result := AValue;
  BEToN_Helper(Result, SizeOf(Real48));
end;

function BEToN(AValue: Currency): Currency; overload;
begin
  Result := AValue;
  BEToN_Helper(Result, SizeOf(Currency));
end;

function BEToN(AValue: WideString): WideString; overload;
{$IFDEF ENDIAN_BIG}
begin
  Result := AValue;
end;
{$ELSE}
var
  i: Integer;
  w: Word;
begin
  Result := '';
  SetLength(Result, Length(AValue));
  for i := 1 to Length(AValue) do begin
    w := PWord(@AValue[i])^;
    Swap(w);
    Result[i] := WideChar(w);
  end;
end;
{$ENDIF}

function LEToN(AValue: Single): Single; overload;
begin
  Result := AValue;
  LEToN_Helper(Result, SizeOf(Single));
end;

function LEToN(AValue: Double): Double; overload;
begin
  Result := AValue;
  LEtoN_Helper(Result, SizeOf(Double));
end;

{$IF SizeOf(Extended) = 10}
function LEToN(AValue: Extended): Extended; overload;
begin
  Result := AValue;
  LEtoN_Helper(Result, SizeOf(Extended));
end;
{$ELSE}
function LEToN(AValue: TExtended10): TExtended10; overload;
begin
  Result := AValue;
  LEToN_Helper(Result, SizeOf(TExtended10));
end;
{$IFEND}

function LEToN(AValue: Real48): Real48; overload;
begin
  Result := AValue;
  LEtoN_Helper(Result, SizeOf(Real48));
end;

function LEToN(AValue: Currency): Currency; overload;
begin
  Result := AValue;
  LEtoN_Helper(Result, SizeOf(Currency));
end;

function LEToN(AValue: WideString): WideString; overload;
{$IFDEF ENDIAN_LITTLE}
begin
  Result := AValue;
end;
{$ELSE}
var
  i: Integer;
  w: Word;
begin
  SetLength(Result, Length(AValue));
  for i := 1 to Length(AValue) do begin
    w := PWord(@AValue[i])^;
    Swap(w);
    Result[i] := WideChar(w);
  end;
end;
{$ENDIF}

function NtoBE(AValue: Single): Single;
begin
  Result := AValue;
  NtoBE_Helper(AValue, SizeOf(Single));
end;

function NtoBE(AValue: Double): Double;
begin
  Result := AValue;
  NtoBE_Helper(AValue, SizeOf(Double));
end;

{$IF SizeOf(Extended) = 10}
function NtoBE(AValue: Extended): Extended;
begin
  Result := AValue;
  NtoBE_Helper(AValue, SizeOf(Extended));
end;
{$ELSE}
function NToBE(AValue: TExtended10): TExtended10;
begin
  Result := AValue;
  NtoBE_HElper(AValue, SizeOf(TExtended10));
end;
{$IFEND}

function NtoBE(AValue: Real48): Real48;
begin
  Result := AValue;
  NtoBE_HElper(AValue, SizeOf(Real48));
end;

function NtoBE(AValue: Currency): Currency;
begin
  Result := AValue;
  NtoBE_Helper(AValue, SizeOf(Currency));
end;

function NToBE(AValue: WideString): WideString;
{$IFDEF ENDIAN_BIG}
begin
  Result := AValue;
end;
{$ELSE}
var
  i: Integer;
  w: Word;
begin
  Result := '';
  SetLength(Result, Length(AValue));
  for i := 1 to Length(AValue) do
  begin
    w := PWord(@AValue[i])^;
    Swap(w);
    Result[i] := WideChar(w);
  end;
end;
{$ENDIF}

function NtoLE(AValue: Single): Single;
begin
  Result := AValue;
  NtoLE_Helper(AValue, SizeOf(Single));
end;

function NtoLE(AValue: Double): Double;
begin
  Result := AValue;
  NtoLE_Helper(AValue, SizeOf(Double));
end;

{$IF SizeOf(Extended) = 10}
function NtoLE(AValue: Extended): Extended;
begin
  Result := AValue;
  NtoLE_Helper(AValue, SizeOf(Extended));
end;
{$ELSE}
function NtoLE(AValue: TExtended10): TExtended10;
begin
  Result := AValue;
  NtoLE_Helper(AValue, SizeOf(TExtended10));
end;
{$IFEND}

function NtoLE(AValue: Real48): Real48;
begin
  Result := AValue;
  NtoLE_HElper(AValue, SizeOf(Real48));
end;

function NtoLE(AValue: Currency): Currency;
begin
  Result := AValue;
  NtoLE_Helper(AValue, SizeOf(Currency));
end;

function NToLE(AValue: WideString): WideString;
{$IFDEF ENDIAN_LITTLE}
begin
  Result := AValue;
end;
{$ELSE}
var
  i: Integer;
  w: Word;
begin
  SetLength(Result, Length(AValue);
  for i := 1 to Length(AValue) do
  begin
    w := PWord(@AValue[i])^;
    Swap(w);
    Result[i] := WideChar(w);
  end;
end;
{$ENDIF}

{ Is needed in cases such as Win-64bit where "extended" maps to "double". }

function ExtendedToString(x: TExtended10): String;
type
  TExtendedRec = packed record
    Significand: QWord;
    Exponent: Word;
  end;
var
  xr: TExtendedRec absolute x;
  sgn: Integer;
  exponent: Integer;
  mantissa: Extended;
  exp10, mant10: Double;
  tmp: QWord; //Int64;
  factor: Int64;
  i: Integer;
  signBit: Word;
  exponentBits: Word;
  sigBits63_62: byte;
  sigBits61_0: Int64;
  dbl: Double;
begin
  Result := 'N/A';

  signBit := xr.Exponent and $8000;
  exponentBits := xr.Exponent and $7FFF;

  // Get sign: highest bit
  if signBit = 0 then
    sgn := +1
  else
    sgn := -1;

  // Special cases: NaN, Infinity, NegInfinity
  if exponentBits = $7FFF then  // all bits in exponent part set
  begin
    sigBits63_62 := (xr.Significand and $C000000000000000) shr (4*15 + 2);
    sigBits61_0 := (xr.Significand and $3000000000000000);
    case sigBits63_62 of
      0: if sigBits61_0 = 0 then dbl := sgn * Infinity else exit;
      1: exit;
      2: if sigBits61_0 = 0 then dbl := sgn * Infinity else exit;
      3: if sigBits61_0 = 0 then dbl := NaN else exit;
    end;
    Result := FloatToStr(dbl);
    exit;
  end;

  // Special case zero:
  if (xr.Significand = 0) and (exponentBits = 0) then
  begin
    if sgn = -1 then
      Result := '-0'
    else
      Result := '0';
    exit;
  end;

  // Get exponent: 15 bit, offset by 16383
  exponent := exponentBits - 16383;

  // Get mantissa
  mantissa := 0.0;
  tmp := xr.Significand;
  factor := 1;
  for i := 0 to 63 do
  begin
    if tmp and $8000000000000000 <> 0 then
      mantissa := mantissa + 1.0 / factor;
    tmp := tmp shl 1;
    factor := factor shl 1;
  end;

  // Product
  if (exponent >= -1022) and (exponent <= 1023) then
  begin
    dbl := sgn * mantissa * IntPower(2.0, exponent);
    Result := Format('%.15g', [dbl]);
  end else
  begin
    // Convert m*2^exponent to m10*10^exp10
    exp10 := exponent * log10(2.0);
    mant10 := sgn * mantissa * Power(10, frac(exp10));
    Result := Format('%.15g', [mant10]) + 'E' + IntToStr(trunc(exp10));
  end;
end;


{==============================================================================}
{    Data types                                                                }
{==============================================================================}

procedure CreateDataTypeList(AList: TStrings; const ADataTypes: array of TDataType);
var
  dt: TDataType;
  s: String;
begin
  if AList = nil then
    raise Exception.Create('[CreateDataTypeList] Parameter AList cannot be nil.');

  for dt in ADataTypes do begin
    if DataTypeDescriptions[dt] = '' then
      s := DataTypeNames[dt]
    else
      s := Format('%s (%s)', [DataTypeNames[dt], DataTypeDescriptions[dt]]);
    AList.AddObject(s, TObject(PtrInt(dt)))
  end;
end;

function NumberToBytes(P: PByte; ANumBytes: Integer; BigEndian: Boolean): TBytes;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, ANumBytes);
  if BigEndian then
    for i := 0 to ANumBytes - 1 do
      Result[ANumBytes - 1 - i] := P^
  else
    Move(P^, Result[0], ANumBytes);
end;

function ByteToBytes(AValue: Byte): TBytes;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := AValue;
end;

function WordToBytes(AValue: word; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, 2, BigEndian);
end;

function DWordToBytes(AValue: DWord; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, 4, BigEndian);
end;

function Int64ToBytes(AValue: DWord; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, 8, BigEndian);
end;

function CurrToBytes(AValue: Currency; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, 8, BigEndian);
end;

function SingleToBytes(AValue: single; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, 4, BigEndian);
end;

function DoubleToBytes(AValue: double; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, 8, BigEndian);
end;

function ExtendedToBytes(AValue: Extended; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, SizeOf(Extended), BigEndian);
end;

function ExtendedToBytes(AValue: TExtended10; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, SizeOf(TExtended10), BigEndian);
end;

function Real48ToBytes(AValue: Real48; BigEndian: Boolean): TBytes;
begin
  Result := NumberToBytes(@AValue, 6, BigEndian);
end;

function BytesToHex(AValue: TBytes): string;
const
  HexChars = '0123456789ABCDEF';
var
  i, j: integer;
  b: byte;
begin
  Result := '';
  if AValue = nil then
    exit;

  j := 1;
  SetLength(Result, Length(AValue) * 3);
  for i := 0 to Length(AValue)-1 do begin
    b := AValue[i];
    Result[j    ] := HexChars[(b shr 4) + 1];
    Result[j + 1] := HexChars[(b and 15) + 1];
    Result[j + 2] := ' ';
    inc(j, 3);
  end;
  SetLength(Result, Length(Result) - 1);
end;


{==============================================================================}
{                              Misc                                            }
{==============================================================================}

procedure Exchange(var A, B: Integer);
var
  C: Integer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure EnsureOrder(var A, B: Integer);
begin
  if A > B then Exchange(A, B);
end;

function GetVersionStr: String;
var
  ver: TProgramVersion;
begin
  GetProgramVersion(ver);
  Result := Format('v%d.%d.%d', [ver.Major, ver.Minor, ver.Revision]);
end;


end.

