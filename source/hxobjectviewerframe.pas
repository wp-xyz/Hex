unit hxObjectViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  hxBasicViewerFrame, hxHexEditor;

type
  TCheckUserAbortEvent = procedure (Sender:TObject; var Aborted:boolean) of object;

  TExtractor = class
  private
    FHexEditor: THxHexEditor;
    FOffset: integer;
    FSize: integer;
    FTag: integer;
    FLastResult: boolean;
    FSignatures: TStringArray;
    FFileExt: string;
    FInfo: string;
    FEmbeddedClass: TClass;
    FOnCheckUserAbort: TCheckUserAbortEvent;
    function GetFirstFileExt: String;
  protected
    function CreateView({%H-}AOwner: TWinControl; AOffset: Integer;
      out AInfo: String): TControl; virtual;
    function FindView(AParent: TWinControl): TControl;
    procedure HideView(AParent: TWinControl);
  public
    constructor Create(AEmbeddedClass: TClass; AFileExt: String;
      const ASignatures: TStringArray); virtual;
    function CanExtract(AHexEditor: THxHexEditor; AOffset: integer; out ASize: Integer): boolean;
    function CheckAndShow(AHexEditor: THxHexEditor; AOffset: integer; AParent: TWinControl): TControl;
    function ExtractorFilter: String;
    function Find(AHexEditor: THxHexEditor; AStart, AEnd: integer): integer;
    procedure Reset;
    procedure SaveToStream(AStream: TStream); virtual;
    property FirstFileExt: string read GetFirstFileExt;
    { class methods }
    class function CheckSignature(AHexEditor: THxHexEditor; AOffset: Integer;
      AEmbeddedClass: TClass; const ASignature: String): boolean; virtual;
    class function FindOffset(AHexEditor: THxHexEditor; AOffset: Integer;
      AEmbeddedClass: TClass; const ASignatures: TStringArray): Integer;
    { properties }
    property Info: string read FInfo;
    property Signatures: TStringArray read FSignatures;
    property Size: integer read FSize;
    property OnCheckUserAbort: TCheckUserAbortEvent read FOnCheckUserAbort write FOnCheckUserAbort;
  end;

  TExtractorClass = class of TExtractor;

  TGraphicExtractor = class(TExtractor)
  protected
    class function CheckSignature(AHexEditor: THxHexEditor; AOffset: Integer;
      AEmbeddedClass: TClass; const ASignature: String): Boolean; override;
    function CreateInfo(APicture: TPicture): String; virtual;
    function CreateView(AOwner: TWinControl; AOffset: Integer;
      out AInfo: String): TControl; override;
  end;

  TIconExtractor = class(TExtractor)
  protected
    class function CheckSignature(AHexEditor: THxHexEditor; AOffset: Integer;
      AEmbeddedClass: TClass; const ASignature: String): Boolean; override;
    function CreateInfo(AIcon: TIcon): String;
    function CreateView(AOwner: TWinControl; AOffset: Integer;
      out AInfo: String): TControl; override;
  end;

  { ------ }

  { TObjectViewerFrame }

  TObjectViewerFrame = class(TBasicViewerFrame)
    mmoInfo: TMemo;
    ScrollBox: TScrollBox;
    Splitter1: TSplitter;
  private
    FExtractor: TExtractor;
    FExtractorControl: TControl;
    FLockUpdate: Integer;
    function GetAtObject: Boolean;

  public
    destructor Destroy; override;
    function FindObject(AIndex: Integer; AHexEditor: THxHexEditor; AOffset: Integer): Boolean;
    procedure UpdateData(AHexEditor: THxHexEditor); override;
    property AtObject: Boolean read GetAtObject;
    property Extractor: TExtractor read FExtractor;
  end;

function RegisterExtractor(AClass: TExtractorClass; AEmbeddedClass: TClass;
  AExt: String; const ASignatures: array of string): integer; overload;

function CanExtract(AHexEditor: THxHexEditor; AOffset: Integer): TExtractor;
function CreateExtractor(AExt: string): TExtractor; overload;
function CreateExtractor(AIndex: Integer): TExtractor; overload;
function FindOffsetForExtractor(AIndex: Integer; AHexEditor: THxHexEditor; AOffset: Integer): Integer;
function NumExtractors: integer;
function RegisteredExtension(AIndex: Integer): String;
function RegisteredExtractorSignature(AIndex: Integer): Boolean;


implementation

{$R *.lfm}

uses
  hxStrings, hxUtils;

const
  PIXEL_FORMATS: array[TPixelFormat] of string = (
    'Device', '1 bit', '4 bit', '8 bit', '15 bit',
    '16 bit', '24 bit', '32 bit', 'Custom'
  );

{ Extractor list }

type
  TExtractorItem = class
    FExtractorClass: TExtractorClass;
    FEmbeddedClass: TClass;
    FSignatures: TStringArray;
    FExtensions: String;
    function CreateExtractor: TExtractor;
    function FirstExt: String;
    function MatchesExt(AExt: String): Boolean;
  end;

  function TExtractorItem.CreateExtractor: TExtractor;
  begin
    Result := FExtractorClass.Create(FEmbeddedClass, FExtensions, FSignatures);
  end;

  function TExtractorItem.FirstExt: String;
  var
    sa: TStringArray;
  begin
    sa := FExtensions.Split('|');
    if Length(sa) > 0 then
      Result := sa[0]
    else
      Result := '';
  end;

  function TExtractorItem.MatchesExt(AExt: String): Boolean;
  var
    sa: TStringArray;
    ext: String;
  begin
    sa := FExtensions.Split('|');
    for ext in sa do
      if ext = AExt then begin
        Result := true;
        exit;
      end;
    Result := false;
  end;

{ ---- }

type
  TExtractorList = class(TFPList)
  private
    function GetItem(AIndex: integer): TExtractorItem;
  public
    destructor Destroy; override;
    property Items[AIndex: integer]: TExtractorItem read GetItem; default;
  end;

var
  RegisteredExtractors: TExtractorList = nil;

destructor TExtractorList.Destroy;
var
  i: integer;
begin
  for i := Count-1 downto 0 do Items[i].Free;
  inherited Destroy;
end;

function TExtractorList.GetItem(AIndex: integer): TExtractorItem;
begin
  Result := TExtractorItem(inherited Items[AIndex]);
end;

{ ---------- }

function RegisterExtractor(AClass: TExtractorClass; AEmbeddedClass: TClass;
  AExt: String; const  ASignatures: array of String): integer;
var
  item: TExtractorItem;
  i: Integer;
begin
  item := TExtractorItem.Create;
  item.FExtractorClass := AClass;
  item.FEmbeddedClass := AEmbeddedClass;
  item.FExtensions := AExt;
  SetLength(item.FSignatures, Length(ASignatures));
  for i := 0 to High(ASignatures) do
    item.FSignatures[i] := ASignatures[i];
  Result := RegisteredExtractors.Add(item);
end;

function CanExtract(AHexEditor: THxHexEditor; AOffset: Integer): TExtractor;
var
  i, j: integer;
  item: TExtractorItem;
begin
  for i := 0 to RegisteredExtractors.Count-1 do
  begin
    item := RegisteredExtractors[i];
    for j := 0 to High(item.FSignatures) do
      if item.FExtractorClass.CheckSignature(AHexEditor, AOffset, item.FEmbeddedClass, item.FSignatures[j]) then
      begin
        Result := item.CreateExtractor;
        exit;
      end;
  end;
  Result := nil;
end;

function CreateExtractor(AExt: string): TExtractor;
var
  i: integer;
  item: TExtractorItem;
begin
  if (AExt <> '') then
  begin
    while AExt[1] = '.' do
      Delete(AExt, 1, 1);

    if AExt = '' then
      for i := 0 to RegisteredExtractors.Count-1 do
      begin
        item := RegisteredExtractors[i];
        if item.MatchesExt(AExt) then
        begin
          Result := item.CreateExtractor;
          exit;
        end;
      end;
  end;
  Result := nil;
end;

function CreateExtractor(AIndex: Integer): TExtractor;
begin
  if (AIndex >= 0) and (AIndex < RegisteredExtractors.Count) then
    Result := RegisteredExtractors[AIndex].CreateExtractor
  else
    Result := nil;
end;

function FindOffsetForExtractor(AIndex: Integer; AHexEditor: THxHexEditor;
  AOffset: Integer): Integer;
var
  item: TExtractorItem;
begin
  item := RegisteredExtractors[AIndex];
  Result := item.FExtractorClass.FindOffset(AHexEditor, AOffset, item.FEmbeddedClass, item.FSignatures);
end;

function NumExtractors: Integer;
begin
  Result := RegisteredExtractors.Count;
end;

function RegisteredExtension(AIndex: Integer): String;
var
  item: TExtractorItem;
begin
  item := RegisteredExtractors[AIndex];
  Result := item.FirstExt;
end;

function RegisteredExtractorSignature(AIndex: Integer): Boolean;
var
  item: TExtractorItem;
begin
  item := RegisteredExtractors[AIndex];
  if Assigned(item) then
    Result := Length(item.FSignatures) > 0
  else
    Result := false;
end;



{ TExtractor basic class }

constructor TExtractor.Create(AEmbeddedClass: TClass;
  AFileExt: String; const ASignatures: TStringArray);
begin
  inherited Create;
  FEmbeddedClass := AEmbeddedClass;
  FSignatures := ASignatures;
  FFileExt := AFileExt;
end;

{ Checks whether an embedded object exists at the given offset in the HexEditor }
function TExtractor.CanExtract(AHexEditor: THxHexEditor; AOffset: integer;
  out ASize: Integer): Boolean;
var
  C: TControl;
  P: Integer;
  lInfo: String;
  sig: String;
  found: Boolean;
begin
  Result := false;
  if Assigned(AHexEditor) and Assigned(AHexEditor.DataStorage) and
     (AOffset >= 0) then
  begin
    found := false;
    for sig in FSignatures do
      if CheckSignature(AHexEditor, AOffset, FEmbeddedClass, sig) then
      begin
        found := true;
        break;
      end;
    if not found then
      exit;

    AHexEditor.Seek(AOffset, soFromBeginning);
    AHexEditor.DataStorage.Position := AOffset;
    try
      P := AHexEditor.GetCursorPos;
      C := CreateView(nil, AOffset, lInfo);   // Moves the stream pos to the end of the object
      if Assigned(C) then
      begin
        ASize := FHexEditor.DataStorage.Position - AOffset;
        Result := true;
      end else
        ASize := -1;
    finally
      C.Free;
      FHexEditor.DataStorage.Position := P;
      FHexEditor.Seek(P, soFromBeginning);
    end;
  end;
end;

{ Possibly an embedded object exists at position "AOffset" of the HexEditor stream.
  CreateView tries to extract this object.
  When this is successful a component matching the embedded object is created.
  If not, the function returns nil.
  "AOwner" is the Owner (and parent) of the component to be created.
  MUST BE OVERRIDDEN FOR EVERY EXTRACTOR TYPE. }
function TExtractor.CreateView(AOwner: TWinControl; AOffset: Integer;
  out AInfo: String): TControl;
begin
  Result := nil;
  AInfo := '';
end;

{ Main method to be called from outside.
  Checks wheter an extractable embedded object exists at the "AOffset" of then
  "AHexEditor".
  If yes, the viewing component is created which fits to the
  type of the embedded object; the component is inserted as a child of "AParent".
  If not, the component is searched in "AParent" and destroyed. The function
  returns nil in this case. }
function TExtractor.CheckAndShow(AHexEditor: THxHexEditor; AOffset: integer;
  AParent: TWinControl): TControl;
var
  sig: String;
  found: Boolean;
begin
  Result := nil;

  FHexEditor := AHexEditor;
  FOffset := AOffset;
  if Assigned(FHexEditor) then
  begin
    found := false;
    for sig in FSignatures do
      if CheckSignature(FHexEditor, FOffset, FEmbeddedClass, sig) then
      begin
        found := true;
        break;
      end;
    if not found then
      exit;

    try
      HideView(AParent);
      Result := CreateView(AParent, FOffset, FInfo);
      if Assigned(Result) then
      begin
        FSize := FHexEditor.DataStorage.Position - FOffset;
        //FHexEditor.Seek(FOffset, soFromBeginning);  // Do not restore old stream pos because this will erase the selection needed by SelectObject
        Result.Parent := AParent;
        Result.Tag := FTag;
        FLastResult := true;
        exit;
      end;
    except
      FreeAndNil(Result);
    end;
  end;
  FHexEditor := nil;
  FOffset := -1;
  FSize := 0;
  FLastResult := false;

  if not FLastResult then
  begin
    HideView(AParent);
    if not Assigned(Result) then
      FLastResult := false;
  end else
    Result := FindView(AParent);
end;

{ Checks whether the signature (a specific byte sequence) of the embedded object
  handled by the Extractor is found at the current offset of the hex editor.
  Example: The signature of bmp files is "BM"
  When the function result is true the calling method "CheckAndShow" tries to
  display the found object (which still may fail).
  The signature is provided as a parameter when the Extractor is created.
  MUST BE OVERRIDDEN IF A DIFFERENT CHECKING METHOD IS REQUIRED. }
class function TExtractor.CheckSignature(AHexEditor: THxHexEditor;
  AOffset: Integer; AEmbeddedClass: TClass; const ASignature: String): Boolean;
var
  s: string;
  n: integer;
  P: Integer;
begin
  Result := false;
  n := Length(ASignature);
  if (n > 0) and Assigned(AHexEditor) and
     (AOffset >= 0) and (AOffset <= AHexEditor.DataSize - n) then
  begin
    SetLength(s, n);
    AHexEditor.ReadBuffer(s[1], AOffset, n);
    Result := (ASignature = s);
  end;
end;

function TExtractor.ExtractorFilter: String;
var
  sa: TStringArray;
  i: Integer;
begin
  sa := Lowercase(FFileExt).Split('|');
  Result := Format(SExtractorFilterMask, [sa[0], sa[0], sa[0]]);
  for i := 1 to High(sa) do
    Result := Result + ';' + Format(SExtractorFilterMask, [sa[i], sa[i], sa[i]]);
end;

{ Searches the signature of the embedded object known to the Extractor in the
  specified HexEditor between offset positions AStart and AEnd. Returns the
  offset of the first byte of the embedded object if the search was successful.
  Otherwise the function returns -1.
  The user can abort the search (via event "OnCheckUserAbort"), and the
  function returns -2 in this case. }
function TExtractor.Find(AHexEditor: THxHexEditor; AStart, AEnd: Integer): Integer;
var
  p: integer;
  crs: TCursor;
  aborted: boolean;
  s: string;
  sig: String;
begin
  Result := -1;
  if AHexEditor = nil then
    exit;

  crs := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    if (AEnd >= AHexEditor.DataSize) or (AEnd = -1) then
      AEnd := AHexEditor.DataSize-1;
    if AStart < 0 then
      AStart := 0;
    EnsureOrder(AStart, AEnd);

    for sig in FSignatures do
    begin
      p := AStart;
      while (p <= AEnd - Length(sig)) and (p <> -1) do
      begin
        s := sig;
        if s <> '' then
        begin
          s := AHexEditor.PrepareFindReplaceData(s, false, false);
          p := AHexEditor.Find(PChar(s), Length(s), p, AEnd, false);
          if p = -1 then
            Continue;
        end;

        if CheckSignature(AHexEditor, p, FEmbeddedClass, sig) then
        begin
          Result := p;
          exit;
        end;

        Application.ProcessMessages;
        aborted := false;
        if Assigned(FOnCheckUserAbort) then
          FOnCheckUserAbort(Self, aborted);
        if aborted then begin
          Result := -2;
          exit;
        end;

        inc(p);
      end;
    end;
  finally
    Screen.Cursor := crs;
  end;
end;

class function TExtractor.FindOffset(AHexEditor: THxHexEditor; AOffset: Integer;
  AEmbeddedClass: TClass; const ASignatures: TStringArray): Integer;
var
  s: String;
  i: Integer;
begin
  for i := 0 to High(ASignatures) do
  begin
    // Prepare and execute search for the embedded object's signature
    s := AHexEditor.PrepareFindReplaceData(ASignatures[i], false, false);
    Result := AHexEditor.Find(PChar(s), Length(s), AOffset, AHexEditor.DataSize-1, false);
    // Found
    if Result <> -1 then
    begin
      if CheckSignature(AHexEditor, Result, AEmbeddedClass, s) then
        exit;
    end;
  end;
  // No embedded object found
  Result := -1;
end;

{ Searches for the viewing component of the Extractor in the specified parent.
  The component has a specific tag ("FTag"). }
function TExtractor.FindView(AParent: TWinControl): TControl;
var
  i: integer;
begin
  if Assigned(AParent) then begin
    for i := 0 to AParent.ControlCount-1 do
      if AParent.Controls[i].Tag = FTag then
      begin
        Result := AParent.Controls[i];
        exit;
      end;
  end;
  Result := nil;
end;

function TExtractor.GetFirstFileExt: String;
var
  sa: TStringArray;
begin
  sa := FFileExt.Split('|');
  if Length(sa) > 0 then
    Result := sa[0]
  else
    Result := '';
end;

{ Checks whether the viewing component of the Extractor already exists inside
  the specified parent and removes it. }
procedure TExtractor.HideView(AParent: TWinControl);
var
  i: integer;
  C: TControl;
begin
  if Assigned(AParent) then
    for i := 0 to AParent.ControlCount-1 do
    begin
      C := AParent.Controls[i];
      if C.Tag = FTag then
      begin
        AParent.RemoveControl(C);
        C.Free;
        FInfo := '';
        FSize := -1;
        exit;
      end;
    end;
end;

procedure TExtractor.Reset;
begin
  FHexEditor := nil;
  FOffset := -1;
  FInfo := '';
end;

procedure TExtractor.SaveToStream(AStream: TStream);
var
  P: Integer;
begin
  if Assigned(FHexEditor) then
  begin
    P := FHexEditor.DataStorage.Position;
    try
      FHexEditor.DataStorage.Position := FOffset;
      AStream.CopyFrom(FHexEditor.DataStorage, FSize);
    finally
      FHexEditor.DataStorage.Position := P;
    end;
  end;
end;


{ TGraphicExtractor }

class function TGraphicExtractor.CheckSignature(AHexEditor: THxHexEditor;
  AOffset: Integer; AEmbeddedClass: TClass; const ASignature: String): Boolean;
begin
  Result := inherited CheckSignature(AHexEditor, AOffset, AEmbeddedClass, ASignature);
  { Do not call GraphicClass.IsStreamFormatSupported because it would usually
    do nothing more than our CheckSignature. }
end;

function TGraphicExtractor.CreateView(AOwner: TWinControl; AOffset: Integer;
  out AInfo: String): TControl;
var
  pic: TPicture;
begin
  pic := TPicture.Create;
  try
    FHexEditor.DataStorage.Position := AOffset;
    pic.LoadFromStream(FHexEditor.DataStorage);
    Result := TImage.Create(AOwner);
    with (Result as TImage) do
    begin
      Picture.Assign(pic);
      Align := alClient;
      Center := true;
      Proportional := true;
      AInfo := CreateInfo(pic);
    end;
  finally
    // do not reset the stream position here. Position is needed for size determination.
    pic.Free;
  end;
end;

function TGraphicExtractor.CreateInfo(APicture: TPicture): String;
begin
  Result :=
    Copy(FEmbeddedClass.ClassName, 2, MaxInt) + ' / ' +
    Format('%d x %d', [APicture.Width, APicture.Height]);
  if APicture.Graphic is TRasterImage then
    Result := Result + ' / '  +
      PIXEL_FORMATS[TRasterImage(APicture.Graphic).PixelFormat];
end;


{ TIconExtractor }

class function TIconExtractor.CheckSignature(AHexEditor: THxHexEditor;
  AOffset: Integer; AEmbeddedClass: TClass; const ASignature: String): Boolean;
type
  TIconDirectory = packed record
    Width: Byte;             // Image width, 0 means: 256
    Height: Byte;            // Image height, 0 means: 256
    PaletteSize: Byte;       // 0 when no palette is used
    Reserved: byte;          // 0
    ColorPlanes: Word;       // 0 or 1
    BitsPerPixel: Word;
    ImageDataSize: DWord;    // Image data size in bytes
    OffsetToImage: DWord;
  end;
var
  P: Integer;
  n, m: Word;
  i: Integer;
  directory: TIconDirectory;
  totalSize: Integer;
begin
  Result := inherited CheckSignature(AHexEditor, AOffset, AEmbeddedClass, ASignature);
  if Result then begin
    // Read the icon images directory and do a plausibility check
    P := AOffset + Length(ASignature);
    // Read number of images;
    AHexEditor.ReadBuffer(n, P, 2);
    if n = 0 then
      exit(false);
    n := LEToN(n);
    totalSize := 0;
    inc(P, 2);
    for i := 1 to n do begin
      AHexEditor.ReadBuffer(directory, P, SizeOf(directory));
      if directory.Reserved <> 0 then
        exit(false);
      if not (LEToN(directory.BitsPerPixel) in [1, 4, 8, 16, 24, 32]) then
        exit(false);
      if not (LEToN(directory.ColorPlanes) in [0, 1]) then
        exit(false);
      totalSize := totalSize + LEToN(directory.ImageDataSize);
      if totalSize > AHexEditor.DataSize then
        exit(false);
      if LEToN(directory.OffsetToImage) > totalSize then
        exit(false);
      inc(P, SizeOf(directory));
    end;
  end;
end;

function TIconExtractor.CreateInfo(AIcon: TIcon): String;
var
  L: TStrings;
  i: Integer;
begin
  L := TStringList.Create;
  try
    L.Add('Icon');
    for i:=0 to AIcon.Count-1 do begin
      AIcon.Current := i;
      L.Add(Format('%d x %d, %s', [AIcon.Width, AIcon.Height, PIXEL_FORMATS[AIcon.PixelFormat]]));
    end;
    Result := L.Text;
    while Result[Length(Result)] in [#13, #10] do
      Delete(Result, Length(Result), 1);
  finally
    AIcon.Current := 0;
    L.Free;
  end;
end;

function TIconExtractor.CreateView(AOwner: TWinControl; AOffset: Integer;
  out AInfo: String): TControl;
var
  ico: TIcon;
  i: Integer;
begin
  ico := TIcon.Create;
  try
    FHexEditor.DataStorage.Position := AOffset;
    ico.LoadFromStream(FHexEditor.DataStorage);
    Result := TImage.Create(AOwner);
    with (Result as TImage) do
    begin
      Picture.Assign(ico);
      Align := alClient;
      Center := true;
      Proportional := true;
      AInfo := CreateInfo(ico);
    end;
  finally
    FHexEditor.DataStorage.Position := AOffset;
    ico.Free;
  end;
end;


{ TObjectViewerFrame }

destructor TObjectViewerFrame.Destroy;
begin
  FreeAndNil(FExtractor);
  inherited;
end;

function TObjectViewerFrame.FindObject(AIndex: Integer;
  AHexEditor: THxHexEditor; AOffset: Integer): Boolean;
var
  P: Integer;
  s: String;
begin
  inherited;

  P := FindOffsetForExtractor(AIndex, AHexEditor, AOffset);
  if P >= 0 then
  begin
    AHexEditor.Seek(P, soFromBeginning);
    Result := true;
  end else
  begin
    Result := false;
    AHexEditor.SelStart := AHexEditor.GetCursorPos;
    AHexEditor.SelEnd := -1;
    if P = -1 then
      s := Format(SObjectNotFound, [RegisteredExtension(AIndex)])
    else if P = -2 then
      s := Format(SExtractorSearchUserAbort, [RegisteredExtension(AIndex)]);
    MessageDlg(s, mtInformation, [mbOK], 0);
  end;
end;

function TObjectViewerFrame.GetAtObject: Boolean;
begin
  Result := Assigned(FExtractorControl);
end;

procedure TObjectViewerFrame.UpdateData(AHexEditor: THxHexEditor);
var
  P: Integer;
begin
  if FLockUpdate > 0 then
    exit;

  P := AHexEditor.GetCursorPos;
  if (AHexEditor.SelStart > AHexEditor.SelEnd) and (P = AHexEditor.SelStart) then
    P := AHexEditor.SelEnd;

  FreeAndNil(FExtractor);
  FExtractor := CanExtract(AHexEditor, P);       // creates a new extractor
  if FExtractor <> nil then
  begin
    FExtractorControl := FExtractor.CheckAndShow(AHexEditor, P, ScrollBox);
    mmoInfo.Lines.Text := FExtractor.Info;
  end else begin
    FreeAndNil(FExtractorControl);
    //lblInfo.Hide;
    mmoInfo.Lines.Text := '(no object)';
  end;
end;


initialization
  RegisteredExtractors := TExtractorList.Create;
  RegisterExtractor(TGraphicExtractor, TBitmap, 'bmp',
    ['BM']);
  RegisterExtractor(TGraphicExtractor, TGifImage, 'gif',
    ['GIF']);
  RegisterExtractor(TIconExtractor, TIcon, 'ico',
    [#0#0#1#0]);
  RegisterExtractor(TGraphicExtractor, TJpegImage, 'jpg|jpeg|jfe',
    [#$FF#$D8#$FF#$E0#$00#$10'JFIF'#0, #$FF#$D8#$FF#$E1]);
  RegisterExtractor(TGraphicExtractor, TPortableAnyMapGraphic, 'pnm|pbm|pgm|ppm',
    ['P1','P2','P3','P4','P5','P6']);
  RegisterExtractor(TGraphicExtractor, TPortableNetworkGraphic, 'png',
    [#137'PNG'#13#10#26#10]);
  RegisterExtractor(TGraphicExtractor, TTiffImage, 'tif|tiff',
    ['II'#42#00, 'MM'#00#42]);
  RegisterExtractor(TGraphicExtractor, TPixMap, 'xpm',
    ['/* XPM */']);

  {
  RegisterRipper(TPcxRipper, rtGraphics, 'PCX', #10,
    TPcxGraphic);

  RegisterRipper(TWavRipper, rtMediaPlayer, 'WAV', 'RIFF');
  RegisterRipper(TAviRipper, rtMediaPlayer, 'AVI', 'RIFF');
  }

finalization
  RegisteredExtractors.Free;


end.

