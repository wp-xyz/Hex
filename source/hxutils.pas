unit hxUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IniFiles, Forms,
  hxGlobal, hxHexEditor;

type
  TExtended10 = packed array[0..9] of byte;     // covers that case that "extended" is not 10 bytes
  PExtended10 = ^TExtended10;

// Ini file
function CreateIniFile : TCustomIniFile;
function GetIniFileName: String;

procedure ReadFormFromIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: String;
  APositionOnly: Boolean = false);
procedure ReadColorsFromIni(AIniFile: TCustomIniFile; ASection: String);
procedure ReadGuiParamsFromIni(AIniFile: TCustomIniFile; ASection: String);
procedure ReadParamsFromIni(AIniFile: TCustomIniFile; ASection: String);

procedure WriteFormToIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: String);
procedure WriteColorsToIni(AIniFile: TCustomIniFile; ASection: String);
procedure WriteGuiParamsToIni(AIniFile: TCustomIniFile; ASection: String);
procedure WriteParamsToIni(AIniFile: TCustomIniFile; ASection: String);

procedure ApplyColorsToHexEditor(const AParams: TColorParams; AHexEditor: THxHexEditor);
procedure ApplyParamsToHexEditor(const AParams: THexParams; AHexEditor: THxHexEditor);

// Simple dialogs
function Confirm(const AMsg: string): boolean;
procedure ErrorMsg(const AMsg: string);

// Exception
procedure ErrorFmt(const AMsg: string; AParams: Array of const);

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

function ExtendedToDouble(x: TExtended10): Double;

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

uses
  LCLType, Math, TypInfo, LazFileUtils, FileInfo, Dialogs;


{==============================================================================}
{  Ini file                                                                    }
{==============================================================================}

function CreateIniFile : TCustomIniFile;
var
  fn: String;
  dir: String;
begin
  fn := GetIniFileName;
  dir := ExtractFileDir(fn);
  ForceDirectories(dir);
  result := TMemIniFile.Create(fn);
end;

function GetIniFileName: String;
begin
  Result :=
    AppendPathDelim(GetAppConfigDir(false)) +
    ChangeFileExt(ExtractFileName(Application.ExeName), '.cfg');
end;

procedure ReadColorsFromIni(AIniFile: TCustomIniFile; ASection: String);
begin
  with ColorParams do
  begin
    BackgroundColor := TColor(AIniFile.ReadInteger(ASection,
      'BackgroundColor', Integer(BackgroundColor)));
    ActiveFieldBackgroundColor := TColor(AIniFile.ReadInteger(ASection,
      'ActiveFieldBackgroundColor', Integer(ActiveFieldBackgroundColor)));
    OffsetBackgroundColor := TColor(AIniFile.ReadInteger(ASection,
      'OffsetBackgroundColor', Integer(OffsetBackgroundColor)));
    OffsetForegroundColor := TColor(AIniFile.ReadInteger(ASection,
      'OffsetForegroundColor', Integer(OffsetForegroundColor)));
    CurrentOffsetBackgroundColor := TColor(AIniFile.ReadInteger(ASection,
      'CurrentOffsetBackgroundColor', Integer(CurrentOffsetBackgroundColor)));
    CurrentOffsetForegroundColor := TColor(AIniFile.ReadInteger(ASection,
      'CurrentOffsetForegroundColor', Integer(CurrentOffsetForegroundColor)));
    ChangedBackgroundColor := TColor(AIniFile.ReadInteger(ASection,
      'ChangedBackgroundColor', Integer(ChangedBackgroundColor)));
    ChangedForegroundColor := TColor(AIniFile.ReadInteger(ASection,
      'ChangedForegroundColor', Integer(ChangedForegroundColor)));
    EvenColumnForeGroundColor := TColor(AIniFile.ReadInteger(ASection,
      'EvenColumnForegroundColor', Integer(EvenColumnForegroundColor)));
    OddColumnForegroundColor := TColor(AIniFile.ReadInteger(ASection,
      'OddColumnForegroundColor', Integer(OddColumnForegroundColor)));
    CharFieldForegroundColor := TColor(AIniFile.ReadInteger(ASection,
      'CharFieldForegroundColor', Integer(CharFieldForegroundColor)));
  end;
end;

procedure ReadFormFromIni(AIniFile: TCustomIniFile; AForm: TForm;
  ASection: String; APositionOnly: Boolean = false);
var
  s: String;
  ws: TWindowState;
  L, T, W, H: integer;
begin
  with AIniFile do
  begin
    s := ReadString(ASection, 'WindowState', '');
    if s = '' then
      ws := AForm.WindowState
    else
      ws := TWindowState(GetEnumValue(TypeInfo(TWindowState), s));
    AForm.WindowState := ws;
    if ws = wsNormal then
    begin
      L := ReadInteger(ASection, 'Left', AForm.Left);
      T := ReadInteger(ASection, 'Top',  AForm.Top);
      if APositionOnly then
      begin
        W := AForm.Width;
        H := AForm.Height;
      end else
      begin
        W := ReadInteger(ASection, 'Width',  AForm.Width);
        H := ReadInteger(ASection, 'Height', AForm.Height);
      end;
      AForm.Position := poDesigned;
      AForm.BoundsRect := Rect(L, T, L + W, T + H);
      AForm.MakeFullyVisible;
    end;
  end;
end;

procedure ReadGUIParamsFromIni(AIniFile: TCustomIniFile; ASection: String);
var
  s: String;
begin
  with AIniFile do
  begin
    s := ReadString(ASection, 'IconSet', '');
    if s <> '' then
      GuiParams.IconSet := TIconSet(GetEnumValue(TypeInfo(TIconSet), s));
  end;
end;

procedure ReadParamsFromIni(AIniFile: TCustomIniFile; ASection: String);
var
  i: Integer;
  s: String;
  sa: TStringArray;
  dt: TDataType;
begin
  with HexParams do
  begin
    ViewOnly := AIniFile.ReadBool(ASection, 'ViewOnly', ViewOnly);
    WriteProtected := AIniFile.ReadBool(ASection, 'WriteProtected', WriteProtected);
    AllowInsertMode := AIniFile.ReadBool(ASection, 'AllowInsertMode', AllowInsertMode);
    InsertMode := AIniFile.ReadBool(ASection, 'InsertMode', InsertMode);
    BigEndian := AIniFile.ReadBool(ASection, 'BigEndian', BigEndian);

    s := AIniFile.ReadString(ASection, 'OffsetDisplay.Base', '');
    if s <> '' then
      OffsetDisplayBase := TOffsetDisplayBase(GetEnumValue(TypeInfo(TOffsetDisplayBase), s));
    OffsetDisplayHexPrefix := AIniFile.ReadString(ASection, 'OffsetDisplay.HexPrefix', OffsetDisplayHexPrefix);
    RulerVisible := AIniFile.ReadBool(ASection, 'Ruler.Visible', RulerVisible);
    s := AIniFile.ReadString('Params', 'Ruler.NumberBase', '');
    if s <> '' then
      RulerNumberBase := TOffsetDisplayBase(GetEnumValue(TypeInfo(TOffsetDisplayBase), s));
    BytesPerRow := AIniFile.ReadInteger(ASection, 'BytesPerRow', BytesPerRow);
    BytesPerColumn := AIniFile.ReadInteger(ASection, 'BytesPerColumn', BytesPerColumn);
    MaskChar := char(AIniFile.ReadInteger(ASection, 'MaskChar', ord(MaskChar)));
    FontName := AIniFile.ReadString(ASection, 'FontName', FontName);
    FontSize := AIniFile.ReadInteger(ASection, 'FontSize', FontSize);
    HexLowercase := AIniFile.ReadBool(ASection, 'HexLowercase', HexLowercase);
    DrawGutter3D := AIniFile.ReadBool(ASection, 'DrawGutter3D', DrawGutter3D);

    { Data Viewer }

    DataViewerVisible := AIniFile.ReadBool(ASection, 'DataViewer.Visible', DataViewerVisible);
    s := AIniFile.ReadString(ASection, 'DataViewer.Position', '');
    if s <> '' then
      DataViewerPosition := TViewerPosition(GetEnumValue(TypeInfo(TViewerPosition), s));
    s := UpperCase(AIniFile.ReadString(ASection, 'DataViewer.DataTypes', ''));
    if s <> '' then
    begin
      s := ';' + s + ';';
      DataViewerDataTypes := [];
      for dt := dtFirstNumericDataType to dtLastNumericDataType do
        if pos(';' + UpperCase(DataTypeNames[dt]) + ';', s) <> 0 then
          Include(DataViewerDataTypes, dt);
    end;
    s := AIniFile.ReadString(ASection, 'DataViewer.ColWidths', '');
    if s <> '' then
    begin
      sa := s.Split(',');
      for i:=0 to High(sa) do
        if i <= High(DataViewerColWidths) then
          TryStrToInt(sa[i], DataViewerColWidths[i]);
    end;

    { Object viewer }

    ObjectViewerVisible := AIniFile.ReadBool(ASection, 'ObjectViewer.Visible', ObjectViewerVisible);
    s := AIniFile.ReadString(ASection, 'ObjectViewer.Position', '');
    if s <> '' then
      ObjectViewerPosition := TViewerPosition(GetEnumValue(TypeInfo(TViewerPosition), s));

    { RecordViewer }

    RecordViewerVisible := AIniFile.ReadBool(ASection, 'RecordViewer.Visible', RecordViewerVisible);
    s := AIniFile.ReadString(ASection, 'RecordViewer.Position', '');
    if s <> '' then
      RecordViewerPosition := TViewerPosition(GetEnumValue(TypeInfo(TViewerPosition), s));

    s := AIniFile.ReadString(ASection, 'RecordViewer.ColWidths', '');
    if s <> '' then
    begin
      sa := s.Split(',');
      for i:=0 to High(sa) do
        if i <= High(RecordViewerColWidths) then
          TryStrToInt(sa[i], RecordViewerColWidths[i]);
    end;

    SettingsPageIndex := AIniFile.ReadInteger(ASection, 'SettingsPageIndex', SettingsPageIndex);
  end;
end;

procedure WriteColorsToIni(AIniFile: TCustomIniFile; ASection: String);
var
  s: String;
  dt: TDataType;
  i: Integer;
begin
  with ColorParams do
  begin
    AIniFile.EraseSection(ASection);

    AIniFile.WriteInteger(ASection, 'BackgroundColor',
      Integer(BackgroundColor));
    AIniFile.WriteInteger(ASection, 'ActiveFieldBackgroundColor',
      Integer(ActiveFieldBackgroundColor));
    AIniFile.WriteInteger(ASection, 'OffsetBackgroundColor',
      Integer(OffsetBackgroundColor));
    AIniFile.WriteInteger(ASection, 'OffsetForegroundColor',
      Integer(OffsetForegroundColor));
    AIniFile.WriteInteger(ASection, 'CurrentOffsetBackgroundColor',
      Integer(CurrentOffsetBackgroundColor));
    AIniFile.WriteInteger(ASection, 'CurrentOffsetForegroundColor',
      Integer(CurrentOffsetForegroundColor));
    AIniFile.WriteInteger(ASection, 'ChangedBackgroundColor',
      Integer(ChangedBackgroundColor));
    AIniFile.WriteInteger(ASection, 'ChangedForegroundColor',
      Integer(ChangedForegroundColor));
    AIniFile.WriteInteger(ASection, 'EvenColumnForegroundColor',
      Integer(EvenColumnForegroundColor));
    AIniFile.WriteInteger(ASection, 'OddColumnForegroundColor',
      Integer(OddColumnForegroundColor));
    AIniFile.WriteInteger(ASection, 'CharFieldForegroundColor',
      Integer(CharFieldForegroundColor));
  end;
end;

procedure WriteFormToIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: string);
begin
  with AIniFile do
  begin
    EraseSection(ASection);

    WriteString(ASection, 'WindowState', GetEnumName(TypeInfo(TWindowState), integer(AForm.WindowState)));
    if AForm.WindowState = wsNormal then
    begin
      WriteInteger(ASection, 'Left',   AForm.Left);
      WriteInteger(ASection, 'Top',    AForm.Top);
      WriteInteger(ASection, 'Width',  AForm.Width);
      WriteInteger(ASection, 'Height', AForm.Height);
    end;
  end;
end;

procedure WriteGuiParamsToIni(AIniFile: TCustomIniFile; ASection: String);
var
  s: String;
begin
  with GUIParams do
  begin
    AIniFile.EraseSection(ASection);
    s := GetEnumName(TypeInfo(TIconSet), Integer(IconSet));
    AIniFile.WriteString(ASection, 'IconSet', s);
  end;
end;

procedure WriteParamsToIni(AIniFile: TCustomIniFile; ASection: String);
var
  s: String;
  dt: TDataType;
  i: Integer;
begin
  with HexParams do
  begin
    AIniFile.EraseSection(ASection);

    AIniFile.WriteInteger(ASection, 'SettingsPageIndex',
      SettingsPageIndex);

    AIniFile.WriteBool(ASection, 'ViewOnly',
      ViewOnly);
    AIniFile.WriteBool(ASection, 'WriteProtected',
      WriteProtected);
    AIniFile.WriteBool(ASection, 'AllowInsertMode',
      AllowInsertMode);
    AIniFile.WriteBool(ASection, 'InsertMode',
      InsertMode);
    AIniFile.WriteBool(ASection, 'BigEndian',
      BigEndian);
    AIniFile.WriteString(ASection, 'OffsetDisplay.Base',
      GetEnumName(TypeInfo(TOffsetDisplayBase), integer(OffsetDisplayBase)));
    AIniFile.WriteString(ASection, 'OffsetDisplay.HexPrefix',
      OffsetDisplayHexPrefix);
    AIniFile.WriteBool(ASection, 'Ruler.Visible',
      RulerVisible);
    AIniFile.WriteString(ASection, 'Ruler.NumberBase',
      GetEnumName(TypeInfo(TOffsetDisplayBase), integer(RulerNumberBase)));
    AIniFile.WriteInteger(ASection, 'BytesPerRow',
      BytesPerRow);
    AIniFile.WriteInteger(ASection, 'BytesPerColumn',
      BytesPerColumn);
    AIniFile.WriteInteger(ASection, 'MaskChar', ord(MaskChar));

    AIniFile.WriteString(ASection, 'FontName',
      FontName);
    AIniFile.WriteInteger(ASection, 'FontSize',
      FontSize);
    AIniFile.WriteBool(ASection, 'HexLowercase',
      HexLowercase);
    AIniFile.WriteBool(ASection, 'DrawGutter3D',
      DrawGutter3D);


    { Data viewer }

    AIniFile.WriteBool(ASection, 'DataViewer.Visible',
      DataViewerVisible);
    AIniFile.WriteString(ASection, 'DataViewer.Position',
      GetEnumName(TypeInfo(TViewerPosition), integer(DataViewerPosition)));
    s := '';
    for dt := dtFirstNumericDataType to dtLastNumericDataType do
      if dt in DataViewerDataTypes then s := s + DataTypeNames[dt] + ';';
    if s <> '' then Delete(s, Length(s), 1);
    AIniFile.WriteString(ASection, 'DataViewer.DataTypes', s);
    s := IntToStr(HexParams.DataViewerColWidths[0]);
    for i := 1 to High(TDataViewerColWidths) do
      s := s + ',' + IntToStr(HexParams.DataViewerColWidths[i]);
    AIniFile.WriteString(ASection, 'DataViewer.ColWidths', s);

    { Object viewer }

    AIniFile.WriteBool(ASection, 'ObjectViewer.Visible',
      ObjectViewerVisible);
    AIniFile.WriteString(ASection, 'ObjectViewer.Position',
      GetEnumName(TypeInfo(TViewerPosition), integer(ObjectViewerPosition)));

    { Record viewer }

    AIniFile.WriteBool(ASection, 'RecordViewer.Visible',
      RecordViewerVisible);
    AIniFile.WriteString(ASection, 'RecordViewer.Position',
      GetEnumName(TypeInfo(TViewerPosition), integer(RecordViewerPosition)));
    s := IntToStr(HexParams.RecordViewerColWidths[0]);
    for i := 1 to High(TRecordViewerColWidths) do
      s := s + ',' + IntToStr(HexParams.RecordViewerColWidths[i]);
    AIniFile.WriteString(ASection, 'RecordViewer.ColWidths', s);
  end;
end;

procedure ApplyColorsToHexEditor(const AParams: TColorParams;
  AHexEditor: THxHexEditor);
var
  i: Integer;
begin
  if Assigned(AHexEditor) then
  begin
    AHexEditor.Colors.Background := AParams.BackgroundColor;
    AHexEditor.Colors.ActiveFieldBackground := AParams.ActiveFieldBackgroundColor;
    AHexEditor.Colors.OffsetBackground := AParams.OffsetBackgroundColor;
    AHexEditor.Colors.Offset := AParams.OffsetForegroundColor;
    AHexEditor.Colors.CurrentOffsetBackground := AParams.CurrentOffsetBackgroundColor;
    AHexEditor.Colors.CurrentOffset := AParams.CurrentOffsetForegroundColor;
    AHexEditor.Colors.EvenColumn := AParams.EvenColumnForegroundColor;
    AHexEditor.Colors.OddColumn := AParams.OddColumnForegroundColor;
    AHexEditor.Colors.ChangedBackground := AParams.ChangedBackgroundColor;
    AHexEditor.Colors.ChangedText := AParams.ChangedForegroundColor;
    AHexEditor.Font.Color := AParams.CharFieldForegroundColor;
  end;
end;

procedure ApplyParamsToHexEditor(const AParams: THexParams;
  AHexEditor: THxHexEditor);
begin
  if Assigned(AHexEditor) then
  begin
    AHexEditor.ReadOnlyView := AParams.ViewOnly;
    AHexEditor.ReadOnlyFile := AParams.WriteProtected;
    AHexEditor.AllowInsertMode := AParams.AllowInsertMode;
    AHexEditor.InsertMode := AParams.InsertMode;
    // To do: Big endian

    AHexEditor.OffsetFormat := AParams.GetOffsetFormat;
    AHexEditor.ShowRuler := AParams.RulerVisible;
    case AParams.RulerNumberBase of
      odbDec: AHexEditor.RulerNumberBase := 10;
      odbHex: AHexEditor.RulerNumberBase := 16;
      odbOct: AHexEditor.RulerNumberBase := 8;
    end;
    AHexEditor.BytesPerRow := AParams.BytesPerRow;
    AHexEditor.BytesPerColumn := AParams.BytesPerColumn;
    AHexEditor.MaskChar := AParams.MaskChar;

    AHexEditor.Font.Name := AParams.FontName;
    AHexEditor.Font.Size := AParams.FontSize;
    AHexEditor.HexLowercase := AParams.HexLowerCase;
    AHexEditor.DrawGutter3D := AParams.DrawGutter3D;
  end;
end;

{==============================================================================}
{  Dialogs                                                                     }
{==============================================================================}

function Confirm(const AMsg: string): boolean;
begin
  Result := MessageDlg(AMsg, mtConfirmation, mbYesNoCancel, 0) = idYes;
end;

procedure ErrorMsg(const AMsg: string);
begin
  MessageDlg(AMsg, mtError, [mbOK], 0);
end;


{==============================================================================}
{  Exception                                                                   }
{==============================================================================}

procedure ErrorFmt(const AMsg: string; AParams: Array of const);
begin
  raise EHexError.CreateFmt(AMsg, AParams);
end;


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

procedure LEtoN_Helper(var AValue; NumBytes: Integer);
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

procedure NtoLE_Helper(var AValue; NumBytes: Integer);
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

{ Is needed in cases like Win-64bit where "extended" maps to "double". }

function ExtendedToDouble(x: TExtended10): Double;
type
  TExtendedRec = packed record
    Significand: Int64;
    Exponent: Word;
  end;
var
  xr: TExtendedRec absolute x;
  sgn: Integer;
  exponent: Integer;
  mantissa: Extended;
  tmp: Int64;
  factor: Int64;
  i: Integer;
  signBit: Word;
  exponentBits: Word;
  sigBits63_62: byte;
  sigBits61_0: Int64;
begin
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
      0: if sigBits61_0 = 0 then Result := sgn * Infinity else Result := NaN;
      1: Result := NaN;
      2: if sigBits61_0 = 0 then Result := sgn * Infinity else Result := NaN;
      3: Result := NaN;
    end;
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
  Result := sgn * mantissa * IntPower(2.0, exponent);
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
  SetLength(Result, ANumBytes);
  if BigEndian then
    for i := 0 to ANumBytes - 1 do
      Result[ANumBytes - 1 - i] := P^
  else
    Move(P^, Result[0], ANumBytes);
end;

function ByteToBytes(AValue: Byte): TBytes;
begin
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

