unit hxUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IniFiles, Forms,
  hxGlobal;

// Ini file
function CreateIniFile : TCustomIniFile;
function GetIniFileName: String;
procedure ReadFormFromIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: String);
procedure WriteFormToIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: String);

// Configuration parameters
procedure ReadParamsFromIni(AIniFile: TCustomIniFile; ASection: String);
procedure WriteParamsToIni(AIniFile: TCustomIniFile; ASection: String);

// Simple dialogs
function Confirm(const AMsg: string): boolean;
procedure ErrorMsg(const AMsg: string);

// Exception
procedure ErrorFmt(const AMsg: string; AParams: Array of const);

// Conversion
function IntToOctal(AValue: Integer): String;
function BEToN(AValue: Single): Single; overload;
function BEToN(AValue: Double): Double; overload;
function BEToN(AValue: Extended): Extended; overload;
function BEToN(AValue: Real48): Real48; overload;
function BEToN(AValue: Currency): Currency; overload;
function BEToN(AValue: WideString): WideString; overload;
function LEToN(AValue: Single): Single; overload;
function LEToN(AValue: Double): Double; overload;
function LEToN(AValue: Extended): Extended; overload;
function LEToN(AValue: Real48): Real48; overload;
function LEToN(AValue: Currency): Currency; overload;
function LEToN(AValue: WideString): WideString; overload;


implementation

uses
  LCLType, TypInfo, LazFileUtils, Dialogs;


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

procedure ReadFormFromIni(AIniFile: TCustomIniFile; AForm: TForm;
  ASection: String);
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
      W := ReadInteger(ASection, 'Width',  AForm.Width);
      H := ReadInteger(ASection, 'Height', AForm.Height);
      AForm.BoundsRect := Rect(L, T, L + W, T + H);
      AForm.MakeFullyVisible;
    end;
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
    ViewOnly := AIniFile.ReadBool('Params', 'ViewOnly', ViewOnly);
    WriteProtected := AIniFile.ReadBool('Params', 'WriteProtected', WriteProtected);
    AllowInsertMode := AIniFile.ReadBool('Params', 'AllowInsertMode', AllowInsertMode);
    InsertMode := AIniFile.ReadBool('Params', 'InsertMode', InsertMode);
    BigEndian := AIniFile.ReadBool('Params', 'BigEndian', BigEndian);

    s := AIniFile.ReadString('Params', 'OffsetDisplay.Base', '');
    if s <> '' then
      OffsetDisplayBase := TOffsetDisplayBase(GetEnumValue(TypeInfo(TOffsetDisplayBase), s));
    OffsetDisplayHexPrefix := AIniFile.ReadString('Params', 'OffsetDisplay.HexPrefix', OffsetDisplayHexPrefix);
    RulerVisible := AIniFile.ReadBool('Params', 'Ruler.Visible', RulerVisible);
    s := AIniFile.ReadString('Params', 'Ruler.NumberBase', '');
    if s <> '' then
      RulerNumberBase := TOffsetDisplayBase(GetEnumValue(TypeInfo(TOffsetDisplayBase), s));
    BytesPerRow := AIniFile.ReadInteger('Params', 'BytesPerRow', BytesPerRow);
    BytesPerColumn := AIniFile.ReadInteger('Params', 'BytesPerColumn', BytesPerColumn);
    HexLowercase := AIniFile.ReadBool('Params', 'HexLowercase', HexLowercase);

    BackgroundColor := TColor(AIniFile.ReadInteger('Params',
      'BackgroundColor', Integer(BackgroundColor)));
    ActiveFieldBackgroundColor := TColor(AIniFile.ReadInteger('Params',
      'ActiveFieldBackgroundColor', Integer(ActiveFieldBackgroundColor)));
    OffsetBackgroundColor := TColor(AIniFile.ReadInteger('Params',
      'OffsetBackgroundColor', Integer(OffsetBackgroundColor)));
    OffsetForegroundColor := TColor(AIniFile.ReadInteger('Params',
      'OffsetForegroundColor', Integer(OffsetForegroundColor)));
    CurrentOffsetBackgroundColor := TColor(AIniFile.ReadInteger('Params',
      'CurrentOffsetBackgroundColor', Integer(CurrentOffsetBackgroundColor)));
    CurrentOffsetForegroundColor := TColor(AIniFile.ReadInteger('Params',
      'CurrentOffsetForegroundColor', Integer(CurrentOffsetForegroundColor)));
    ChangedBackgroundColor := TColor(AIniFile.ReadInteger('Params',
      'ChangedBackgroundColor', Integer(ChangedBackgroundColor)));
    ChangedForegroundColor := TColor(AIniFile.ReadInteger('Params',
      'ChangedForegroundColor', Integer(ChangedForegroundColor)));
    EvenColumnForeGroundColor := TColor(AIniFile.ReadInteger('Params',
      'EvenColumnForegroundColor', Integer(EvenColumnForegroundColor)));
    OddColumnForegroundColor := TColor(AIniFile.ReadInteger('Params',
      'OddColumnForegroundColor', Integer(OddColumnForegroundColor)));
    CharFieldForegroundColor := TColor(AIniFile.ReadInteger('Params',
      'CharFieldForegroundColor', Integer(CharFieldForegroundColor)));

(*
    LeftPanelWidth := AIniFile.ReadInteger('Params', 'LeftPanelWidth', LeftPanelWidth);
    RightPanelWidth := AIniFile.ReadInteger('Params', 'RightPanelWidth', RightPanelWidth);
    BottomPanelHeight := AIniFile.ReadInteger('Params', 'BottomPanelHeight', BottomPanelHeight);
*)

    NumViewerVisible := AIniFile.ReadBool('Params', 'NumViewer.Visible', NumViewerVisible);
    s := AIniFile.ReadString('Params', 'NumViewer.Position', '');
    if s <> '' then
      NumViewerPosition := TViewerPosition(GetEnumValue(TypeInfo(TViewerPosition), s));
    //NumViewerHeight := AIniFile.ReadInteger('Params', 'NumViewer.Height', NumViewerHeight);
    //NumViewerWidth := AIniFile.ReadInteger('Params', 'NumViewer.Width', NumViewerWidth);

    s := UpperCase(AIniFile.ReadString('Params', 'NumViewer.DataTypes', ''));
    if s <> '' then
    begin
      s := ';' + s + ';';
      NumViewerDataTypes := [];
      for dt := dtFirstNumericDataType to dtLastNumericDataType do
        if pos(';' + UpperCase(DataTypeNames[dt]) + ';', s) <> 0 then
          Include(NumViewerDataTypes, dt);
    end;

    s := AIniFile.ReadString('Params', 'NumViewer.ColWidths', '');
    if s <> '' then
    begin
      sa := s.Split(',');
      for i:=0 to High(sa) do
        if i <= High(NumViewerColWidths) then
          TryStrToInt(sa[i], NumViewerColWidths[i]);
    end;

    ObjectViewerVisible := AIniFile.ReadBool('Params', 'ObjectViewer.Visible', ObjectViewerVisible);
    s := AIniFile.ReadString('Params', 'ObjectViewer.Position', '');
    if s <> '' then
      ObjectViewerPosition := TViewerPosition(GetEnumValue(TypeInfo(TViewerPosition), s));

    RecordViewerVisible := AIniFile.ReadBool('Params', 'RecordViewer.Visible', RecordViewerVisible);
    s := AIniFile.ReadString('Params', 'RecordViewer.Position', '');
    if s <> '' then
      RecordViewerPosition := TViewerPosition(GetEnumValue(TypeInfo(TViewerPosition), s));

    s := AIniFile.ReadString('Params', 'RecordViewer.ColWidths', '');
    if s <> '' then
    begin
      sa := s.Split(',');
      for i:=0 to High(sa) do
        if i <= High(RecordViewerColWidths) then
          TryStrToInt(sa[i], RecordViewerColWidths[i]);
    end;

    SettingsPageIndex := AIniFile.ReadInteger('Params', 'SettingsPageIndex', SettingsPageIndex);
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

procedure WriteParamsToIni(AIniFile: TCustomIniFile; ASection: String);
var
  s: String;
  dt: TDataType;
  i: Integer;
begin
  with HexParams do
  begin
    AIniFile.EraseSection('Params');

    AIniFile.WriteBool('Params', 'ViewOnly',
      ViewOnly);
    AIniFile.WriteBool('Params', 'WriteProtected',
      WriteProtected);
    AIniFile.WriteBool('Params', 'AllowInsertMode',
      AllowInsertMode);
    AIniFile.WriteBool('Params', 'InsertMode',
      InsertMode);
    AIniFile.WriteBool('Params', 'BigEndian',
      BigEndian);

    AIniFile.WriteString('Params', 'OffsetDisplay.Base',
      GetEnumName(TypeInfo(TOffsetDisplayBase), integer(OffsetDisplayBase)));
    AIniFile.WriteString('Params', 'OffsetDisplay.HexPrefix',
      OffsetDisplayHexPrefix);
    AIniFile.WriteBool('Params', 'Ruler.Visible',
      RulerVisible);
    AIniFile.WriteString('Params', 'Ruler.NumberBase',
      GetEnumName(TypeInfo(TOffsetDisplayBase), integer(RulerNumberBase)));
    AIniFile.WriteInteger('Params', 'BytesPerRow',
      BytesPerRow);
    AIniFile.WriteInteger('Params', 'BytesPerColumn',
      BytesPerColumn);
    AIniFile.WriteBool('Params', 'HexLowercase',
      HexLowercase);

    AIniFile.WriteInteger('Params', 'BackgroundColor',
      Integer(BackgroundColor));
    AIniFile.WriteInteger('Params', 'ActiveFieldBackgroundColor',
      Integer(ActiveFieldBackgroundColor));
    AIniFile.WriteInteger('Params', 'OffsetBackgroundColor',
      Integer(OffsetBackgroundColor));
    AIniFile.WriteInteger('Params', 'OffsetForegroundColor',
      Integer(OffsetForegroundColor));
    AIniFile.WriteInteger('Params', 'CurrentOffsetBackgroundColor',
      Integer(CurrentOffsetBackgroundColor));
    AIniFile.WriteInteger('Params', 'CurrentOffsetForegroundColor',
      Integer(CurrentOffsetForegroundColor));
    AIniFile.WriteInteger('Params', 'ChangedBackgroundColor',
      Integer(ChangedBackgroundColor));
    AIniFile.WriteInteger('Params', 'ChangedForegroundColor',
      Integer(ChangedForegroundColor));
    AIniFile.WriteInteger('Params', 'EvenColumnForegroundColor',
      Integer(EvenColumnForegroundColor));
    AIniFile.WriteInteger('Params', 'OddColumnForegroundColor',
      Integer(OddColumnForegroundColor));
    AIniFile.WriteInteger('Params', 'CharFieldForegroundColor',
      Integer(CharFieldForegroundColor));

  (*
    AIniFile.WriteInteger('Params', 'LeftPanelWidth', LeftPaneLWidth);
    AIniFile.WriteInteger('Params', 'RightPanelWidth', RightPanelWidth);
    AIniFile.WriteInteger('Params', 'BottomPanelHeight', BottomPanelHeight);
*)

    AIniFile.WriteBool('Params', 'NumViewer.Visible',
      NumViewerVisible);
    AIniFile.WriteString('Params', 'NumViewer.Position',
      GetEnumName(TypeInfo(TViewerPosition), integer(NumViewerPosition)));
    {
    if NumViewerPosition in [vpLeft, vpRight] then
      AIniFile.WriteInteger('Params', 'NumViewer.Width', NumViewerWidth);
    if NumViewerPosition in [vpBottom] then
      AIniFile.WriteInteger('Params', 'NumViewer.Height', NumViewerHeight);
      }
    s := '';
    for dt := dtFirstNumericDataType to dtLastNumericDataType do
      if dt in NumViewerDataTypes then s := s + DataTypeNames[dt] + ';';
    if s <> '' then Delete(s, Length(s), 1);
    AIniFile.WriteString('Params', 'NumViewer.DataTypes', s);
    s := IntToStr(HexParams.NumViewerColWidths[0]);
    for i := 1 to High(TNumViewerColWidths) do
      s := s + ',' + IntToStr(HexParams.NumViewerColWidths[i]);
    AIniFile.WriteString('Params', 'NumViewer.ColWidths', s);

    AIniFile.WriteBool('Params', 'ObjectViewer.Visible',
      ObjectViewerVisible);
    AIniFile.WriteString('Params', 'ObjectViewer.Position',
      GetEnumName(TypeInfo(TViewerPosition), integer(ObjectViewerPosition)));

    AIniFile.WriteBool('Params', 'RecordViewer.Visible',
      RecordViewerVisible);
    AIniFile.WriteString('Params', 'RecordViewer.Position',
      GetEnumName(TypeInfo(TViewerPosition), integer(RecordViewerPosition)));
    {
    if RecordViewerPosition in [vpLeft, vpRight] then
      AIniFile.WriteInteger('Params', 'RecordViewer.Width', RecordViewerWidth);
    if RecordViewerPosition in [vpBottom] then
      AIniFile.WriteInteger('Params', 'RecordViewer.Height', RecordViewerHeight);
    }

    s := IntToStr(HexParams.RecordViewerColWidths[0]);
    for i := 1 to High(TRecordViewerColWidths) do
      s := s + ',' + IntToStr(HexParams.RecordViewerColWidths[i]);
    AIniFile.WriteString('Params', 'RecordViewer.ColWidths', s);

    AIniFile.WriteInteger('Params', 'SettingsPageIndex',
      SettingsPageIndex);
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

function BEToN(AValue: Single): Single; overload;
{$IFDEF ENDIAN_BIG}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..3] of Byte;
var
  i: Integer;
begin
  for i:=0 to 3 do TData(Result)[i] := TData(AValue)[3-i];
end;
{$ENDIF}

function BEToN(AValue: Double): Double; overload;
{$IFDEF ENDIAN_BIG}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..7] of Byte;
var
  i: Integer;
begin
  for i:=0 to 7 do TData(Result)[i] := TData(AValue)[7-i];
end;
{$ENDIF}

function BEToN(AValue: Extended): Extended; overload;
{$IFDEF ENDIAN_BIG}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..9] of Byte;
var
  i: Integer;
begin
  for i:=0 to 9 do TData(Result)[i] := TData(AValue)[9-i];
end;
{$ENDIF}

function BEToN(AValue: Real48): Real48; overload;
{$IFDEF ENDIAN_BIG}
begin
  Result := AValue;
end;
{$ELSE}
var
  i: Integer;
begin
  for i:=0 to 5 do Result[5-i] := AValue[i];
end;
{$ENDIF}

function BEToN(AValue: Currency): Currency; overload;
{$IFDEF ENDIAN_BIG}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..7] of Byte;
var
  i: Integer;
begin
  for i:=0 to 7 do TData(Result)[7-i] := TData(AValue)[i];
end;
{$ENDIF}

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
  for i := 1 to Length(AValue) do begin
    w := PWord(@AValue[i])^;
    Swap(w);
    AValue[i] := WideChar(w);
  end;
end;
{$ENDIF}

function LEToN(AValue: Single): Single; overload;
{$IFDEF ENDIAN_LITTLE}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..3] of byte;
var
  i: Integer;
begin
  for i:=0 to 5 do TData(Result)[3-i] := TData(Value)[i];
end;
{$ENDIF}

function LEToN(AValue: Double): Double; overload;
{$IFDEF ENDIAN_LITTLE}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..7] of byte;
var
  i: Integer;
begin
  for i:=0 to 7 do TData(Result)[7-i] := TData(Value)[i];
end;
{$ENDIF}

function LEToN(AValue: Extended): Extended; overload;
{$IFDEF ENDIAN_LITTLE}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..9] of Byte;
var
  i: Integer;
begin
  for i:=0 to 9 do TData(Result)[9-i] := TData(Value)[i];
end;
{$ENDIF}

function LEToN(AValue: Real48): Real48; overload;
{$IFDEF ENDIAN_LITTLE}
begin
  Result := AValue;
end;
{$ELSE}
var
  i: Integer;
begin
  for i:=0 to 5 do Result[5-i] := Value[i];
end;
{$ENDIF}

function LEToN(AValue: Currency): Currency; overload;
{$IFDEF ENDIAN_LITTLE}
begin
  Result := AValue;
end;
{$ELSE}
type
  TData = array[0..7] of Byte;
var
  i: Integer;
begin
  for i:=0 to 7 do TData(Result)[7-i] := TData(Value)[i];
end;
{$ENDIF}

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
  for i := 1 to Length(AValue) do begin
    w := PWord(@AValue[i])^;
    Swap(w);
    AValue[i] := WideChar(w);
  end;
end;
{$ENDIF}

end.

