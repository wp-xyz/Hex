unit hxGlobal;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics;

type
  TOffsetDisplayBase = (odbNone, odbDec, odbHex, odbOct);
  TViewerPosition = (vpLeft, vpRight, vpBottom);

  TDataType = (
    // Integer types
    dtByte, dtShortInt, dtWord, dtSmallInt, dtLongWord, dtLongInt, dtInt64,
    dtCurrency,
    // Floating point types
    dtSingle, dtDouble, dtExtended, dtReal48,
    // Ansi strings
    dtShortString,
    dtAnsiString,
    dtPChar,
    dtCharArray,
    // Wide strings
    dtWideString,
    dtPWideChar,
    dtWideCharArray
  );
  (*
    // Ansi strings (1 byte per char)
    dtFixedString,     // fixed size allocated, 1 length byte + Chars, rest undefined
    dtFixedPChar,      // fixed size allocated, Chars + zero byte, rest undefined
    dtFixedCharArray,  // fixed size allocated, completely filled by Chars
    dtAnsiString,      // unlimited size, 2 length bytes + Chars
    dtPChar,           // unlimited size, Chars terminated by zero byte
    // Unicode strings (2 bytes per char)
    dtFixedWideString,    // fixed size allocated, 1 length word + WideChars
    dtFixedPWideChar,     // fixed size allocated, WideChars + zero word
    dtFixedWideCharArray, // fixed size allocated, completely filled by WideChars
    dtWideString,         // unlimited size, 1 length word + WideChars
    dtPWideChar           // unlimited size, WideChars terminated by zero word
  );         *)
  TDataTypes = set of TDataType;

  PReal48 = ^real48;

  TColWidths = array of Integer;
  TNumViewerColWidths = array[0..2] of Integer;      // Does no contain Value column
  TRecordViewerColWidths = array[0..2] of Integer;   // Does no contain Value column

  EHexError = class(Exception);

const
  StringDataTypes = [
    dtShortString, dtAnsiString, dtPChar, dtCharArray,
    dtWideString, dtPWideChar, dtWideCharArray
  ];

  AllNumericDataTypes = [
    dtByte, dtShortInt, dtWord, dtSmallInt, dtLongWord, dtLongInt,
    dtInt64, dtCurrency, dtSingle, dtDouble, dtExtended, dtReal48
  ];

  dtFirstNumericDataType = dtByte;
  dtLastNumericDataType  = dtReal48;
  dtFirstStringDataType  = dtShortString;
  dtLastStringDataType   = dtWideCharArray;

  DataTypeNames: array[TDataType] of string = (
    'Byte', 'ShortInt', 'Word', 'SmallInt', 'LongWord', 'LongInt', 'Int64',
    'Currency',
    'Single', 'Double', 'Extended', 'Real48',
    'ShortString', 'AnsiString', 'PChar', 'Char array',
    'WideString', 'PWideChar', 'WideChar array'
  );

  DataTypeSizes: array[TDataType] of Integer = (
    1, 1, 2, 2, 4, 4, 8,
    8,
    4, 8, 10, 6,
    0, 0, 0, 0,     // 0 means: size is stored in the TDataType
    0, 0, 0
  );

  DATA_FIELD_SEPARATOR = '|';

  BigEndianStr: array[Boolean] of String = (
    'LE', 'BE'
  );

  // Viewer column IDs
  vcValue = 0;
  vcDataType = 1;
  vcDataSize = 2;
  vcBigEndian = 3;
  vcName = 4;

  // Index into image list for specific icons
  IMG_INDEX_OK = 5;
  IMG_INDEX_CANCEL = 6;

type
  TStatusbarItem = (sbPos, sbSel, sbSize);
  TStatusbarItems = set of TStatusbarItem;

  THexParams = record
    ViewOnly: Boolean;
    WriteProtected: Boolean;
    AllowInsertMode: Boolean;
    InsertMode: Boolean;
    BigEndian: Boolean;

    OffsetDisplayBase: TOffsetDisplayBase;
    OffsetDisplayHexPrefix: String;
    RulerVisible: Boolean;
    RulerNumberBase: TOffsetDisplayBase;
    BytesPerRow: Integer;
    BytesPerColumn: Integer;

    BackgroundColor: TColor;
    ActiveFieldBackgroundColor: TColor;
    OffsetBackgroundColor: TColor;
    OffsetForegroundColor: TColor;
    CurrentOffsetBackgroundColor: TColor;
    CurrentOffsetForegroundColor: TColor;
    EvenColumnForegroundColor: TColor;
    OddColumnForegroundColor: TColor;
    ChangedBackgroundColor: TColor;
    ChangedForegroundColor: TColor;
    CharFieldForegroundColor: TColor;

    ShowStatusBar: Boolean;
    StatusbarItems : TStatusbarItems;
    StatusbarPosDisplay : TOffsetDisplayBase;
    StatusbarSelDisplay : TOffsetDisplayBase;

    LeftPanelWidth: Integer;
    RightPanelWidth: Integer;
    BottomPanelHeight: Integer;

    NumViewerVisible: Boolean;
    NumViewerPosition: TViewerPosition;
    NumViewerDataTypes: TDataTypes;
    NumViewerColWidths: TNumViewerColWidths;

    ObjectViewerVisible: Boolean;
    ObjectViewerPosition: TViewerPosition;

    RecordViewerVisible: Boolean;
    RecordViewerPosition: TViewerPosition;
    RecordViewerColWidths: TRecordViewerColWidths;

    SettingsPageIndex: Integer;

    function GetOffsetFormat: String;
  end;

var
  HexParams: THexParams = (
    ViewOnly: true;
    WriteProtected: true;
    AllowInsertMode: false;
    InsertMode: false;
    BigEndian: false;

    OffsetDisplayBase: odbHex;
    OffsetDisplayHexPrefix: '$';
    RulerVisible: true;
    RulerNumberBase: odbHex;
    BytesPerRow: 16;
    BytesPerColumn: 1;

    BackgroundColor: clWindow;
    ActiveFieldBackgroundColor: clWindow;
    OffsetBackgroundColor: clBtnFace;
    OffsetForegroundColor: clWindowText;
    CurrentOffsetBackgroundColor: clBtnShadow;
    CurrentOffsetForegroundColor: clBtnHighlight;
    EvenColumnForegroundColor: clNavy;
    OddColumnForegroundColor: clBlue;
    ChangedBackgroundColor: $00A8FFFF;
    ChangedForegroundColor: clMaroon;
    CharFieldForegroundColor: clWindowText;

    ShowStatusBar: true;
    StatusbarItems: [sbPos, sbSel, sbSize];
    StatusbarPosDisplay: odbDec;
    StatusbarSelDisplay: odbDec;

    LeftPanelWidth: 250;
    RightPanelWidth: 250;
    BottomPanelHeight: 250;

    NumViewerVisible: true;
    NumViewerPosition: vpRight;
    NumViewerDataTypes: AllNumericDataTypes;
    NumViewerColWidths: (70, 40, 24);

    ObjectViewerVisible: false;
    ObjectViewerPosition: vpRight;

    RecordViewerVisible: false;
    RecordViewerPosition: vpBottom;
    RecordViewerColWidths: (100, 70, 40);

    SettingsPageIndex: 0
  );

  DefaultHexParams: THexParams;

type
  TGotoParams = record
    PosAbs: integer;
    PosRel: integer;
    JumpAbs: boolean;
  end;

var
  GotoParams: TGotoParams = (
    PosAbs: 0;
    PosRel: 0;
    JumpAbs: true
  );

implementation

function THexParams.GetOffsetFormat: String;
begin
  case OffsetDisplayBase of
    odbHex: Result := '-!10:' + OffsetDisplayHexPrefix + '|';
    odbDec: Result := '1!0a:|';
    odbOct: Result := '-!08:&|';
  end;
end;

initialization
  DefaultHexParams := HexParams;

end.

