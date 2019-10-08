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
  TDataViewerColWidths = array[0..2] of Integer;     // Does no contain Value column
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

  SearchableDataTypes = [dtCharArray, dtWideCharArray] + AllNumericDataTypes;

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

  DataTypeDescriptions: array[TDataType] of string = (
    '8-bit unsigned Integer', '8-bit signed Integer',
    '16-bit unsigned Integer', '16-bit signed Integer',
    '32-bit unsigned Integer', '32-bit signed Integer',
    '64-bit signed Integer',
    '64-bit currency',
    '4-byte Float', '8-byte Float', '10-byte Float', '6-byte Pascal Float',
    'String of 1-byte characters, 1 length byte',
    'String of 1-byte characters, 2 length bytes',
    'Zero-terminated string of 1-byte characters',
    'Array of 1-byte characters',
    'String of 2-byte characters, 2 length bytes',
    'Zero-terminated string of 2-byte characters',
    'Array of 2-byte characters'
  );

  DataTypeSizes: array[TDataType] of Integer = (
    1, 1, 2, 2, 4, 4, 8,
    8,
    4, 8, 10, 6,
    0, 0, 0, 0,     // 0 means: size is stored in the TDataType
    0, 0, 0
  );

  DATA_FIELD_SEPARATOR = ';';

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
  IMG_INDEX_EXIT = 1;
  IMG_INDEX_OK = 5;
  IMG_INDEX_CANCEL = 6;
  IMG_INDEX_FIND = 18;
  IMG_INDEX_REPLACE = 19;

  // Ini file sections
  INI_MAINFORM = 'MainForm';
  INI_COLORS = 'Colors';
  INI_PARAMS = 'Params';
  INI_SEARCH_REPLACE = 'SearchReplace';
  INI_GUI = 'UserInterface';

  MAX_SEARCH_HISTORY = 10;
  DROP_DOWN_COUNT = 32;

  // List all GroupIndex value of TActions here to avoid collisions
  GROUP_INDEX_VIEW_OFFSET = 20;

  // Action tags
  TAG_FIND_OBJECTS = 5000;

  TAG_SET_BOOKMARK = 7000;
  TAG_GOTO_BOOKMARK = 7100;
  TAG_CLEAR_BOOKMARK = 7200;


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
    MaskChar: Char;

    FontName: String;
    FontSize: Integer;
    HexLowerCase: Boolean;
    DrawGutter3D: Boolean;

    ShowStatusBar: Boolean;
    StatusbarItems : TStatusbarItems;
    StatusbarPosDisplay : TOffsetDisplayBase;
    StatusbarSelDisplay : TOffsetDisplayBase;

    DataViewerVisible: Boolean;
    DataViewerPosition: TViewerPosition;
    DataViewerDataTypes: TDataTypes;
    DataViewerColWidths: TDataViewerColWidths;

    ObjectViewerVisible: Boolean;
    ObjectViewerPosition: TViewerPosition;

    RecordViewerVisible: Boolean;
    RecordViewerPosition: TViewerPosition;
    RecordViewerColWidths: TRecordViewerColWidths;

    SettingsPageIndex: Integer;

    function GetOffsetFormat: String;
    function GetOffsetFormat(ADisplayBase: Integer): String;
  end;

  TColorParams = record
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
    MaskChar: '.';

    FontName: 'Courier New';
    FontSize: 9;
    HexLowercase: false;
    DrawGutter3D: false;

    ShowStatusBar: true;
    StatusbarItems: [sbPos, sbSel, sbSize];
    StatusbarPosDisplay: odbDec;
    StatusbarSelDisplay: odbDec;

    DataViewerVisible: true;
    DataViewerPosition: vpRight;
    DataViewerDataTypes: AllNumericDataTypes;
    DataViewerColWidths: (70, 40, 24);

    ObjectViewerVisible: false;
    ObjectViewerPosition: vpRight;

    RecordViewerVisible: false;
    RecordViewerPosition: vpBottom;
    RecordViewerColWidths: (100, 70, 40);

    SettingsPageIndex: 0
  );
  DefaultHexParams: THexParams;

  ColorParams: TColorParams = (
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
    CharFieldForegroundColor: clWindowText
  );
  DefaultColorParams: TColorParams;

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

type
  TSearchStart = (ssBOF, ssCursor);

  TSearchReplaceParams = record
    SearchExpression: string;
    SearchDataType: TDataType;
    SearchStart: TSearchStart;
    IgnoreCase: Boolean;
    SearchTextIsHex: Boolean;
    ReplaceExpression: String;
    ReplaceDataType: TDataType;
    ReplaceTextIsHex: Boolean;
    ReplaceConfirmation: Boolean;
  end;
  PSearchReplaceParams = ^TSearchReplaceParams;

var
  SearchReplaceParams: TSearchReplaceParams = (
    SearchExpression: '';
    SearchDataType: dtCharArray;
    SearchStart: ssBOF;
    IgnoreCase: true;
    SearchTextIsHex: false;
    ReplaceExpression: '';
    ReplaceDataType: dtCharArray;
    ReplaceTextIsHex: false;
    ReplaceConfirmation: true;
  );

type
  TIconSet = (isOffice, isSimpleSmall);

  TGuiParams = record
    IconSet: TIconSet;
  end;

var
  GuiParams: TGuiParams = (
    IconSet: isOffice
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

function THexParams.GetOffsetFormat(ADisplayBase: Integer): String;
begin
  case ADisplayBase of
    16: Result := '-!10:' + OffsetDisplayHexPrefix + '|';
    10: Result := '1!0a:|';
     8: Result := '-!08:&|';
    else raise Exception.CreateFmt('DisplayBase %d not supported.', [ADisplayBase]);
  end;
end;


initialization
  DefaultHexParams := HexParams;
  DefaultColorParams := ColorParams;

end.

