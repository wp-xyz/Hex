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
    FSignature: string;
    FFileExt: string;
    FInfo: string;
    FEmbeddedClass: TClass;
    FOnCheckUserAbort: TCheckUserAbortEvent;
    function GetFirstFileExt: String;
  protected
    class function CheckSignature(AHexEditor: THxHexEditor; AEmbeddedClass: TClass;
      const ASignature: String): boolean; virtual;
    function CreateView({%H-}AOwner: TWinControl; out AInfo: String): TControl; virtual;
    function FindView(AParent: TWinControl): TControl;
    procedure HideView(AParent: TWinControl);
  public
    constructor Create(AEmbeddedClass: TClass; AFileExt, ASignature: string); virtual;
    function CanExtract(AHexEditor: THxHexEditor; AOffset: integer): boolean;
    function CheckAndShow(AHexEditor: THxHexEditor; AOffset: integer; AParent: TWinControl): TControl;
    function ExtractorFilter: String;
    function Find(AHexEditor: THxHexEditor; AStart, AEnd: integer): integer;
    procedure Reset;
    procedure SaveToStream(AStream: TStream); virtual;
    property FirstFileExt: string read GetFirstFileExt;
    property Info: string read FInfo;
    property Signature: string read FSignature;
    property Size: integer read FSize;
    property OnCheckUserAbort: TCheckUserAbortEvent read FOnCheckUserAbort write FOnCheckUserAbort;
  end;

  TExtractorClass = class of TExtractor;

  TGraphicExtractor = class(TExtractor)
  protected
    class function CheckSignature(AHexEditor: THxHexEditor; AEmbeddedClass: TClass;
      const ASignature: String): Boolean; override;
    function CreateInfo(APicture: TPicture): String; virtual;
    function CreateView(AOwner: TWinControl; out AInfo: String): TControl; override;
  end;

  TIconExtractor = class(TExtractor)
  protected
    class function CheckSignature(AHexEditor: THxHexEditor; AEmbeddedClass: TClass;
      const ASignature: String): Boolean; override;
    function CreateInfo(AIcon: TIcon): String;
    function CreateView(AOwner: TWinControl; out AInfo: String): TControl; override;
  end;

  { ------ }

  { TObjectViewerFrame }

  TObjectViewerFrame = class(TBasicViewerFrame)
    lblInfo: TLabel;
    ScrollBox: TScrollBox;
  private
    FExtractor: TExtractor;
    FExtractorControl: TControl;
    FLockUpdate: Integer;

  public
    destructor Destroy; override;
    procedure UpdateData(AHexEditor: THxHexEditor); override;
    property Extractor: TExtractor read FExtractor;
  end;

function RegisterExtractor(AClass: TExtractorClass; AEmbeddedClass: TClass;
  AExt, ASignature: string): integer; overload;

function CanExtract(AHexEditor: THxHexEditor): TExtractor;
function CreateExtractor(AExt: string): TExtractor; overload;
function NumExtractors: integer;

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
    FSignature: String;
    FExtensions: String;
    function CreateExtractor: TExtractor;
    function MatchesExt(AExt: String): Boolean;
  end;

  function TExtractorItem.CreateExtractor: TExtractor;
  begin
    Result := FExtractorClass.Create(FEmbeddedClass, FExtensions, FSignature);
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
  AExt, ASignature: string): integer;
var
  item: TExtractorItem;
begin
  item := TExtractorItem.Create;
  item.FExtractorClass := AClass;
  item.FEmbeddedClass := AEmbeddedClass;
  item.FExtensions := AExt;
  item.FSignature := ASignature;
  Result := RegisteredExtractors.Add(item);
end;
                         (*
function ExtractAndDisplay(AHexEditor: THxHexEditor; AOffset: Integer;
  AParent: TWinControl; var AControl: TControl): TExtractor;
var
  i: integer;
  Ex: TExtractor;
begin
  Result := nil;
  for i:=0 to RegisteredExtractors.Count-1 do
  begin
    Ex := RegisteredExtractors[i];
    AControl := Ex.CheckAndShow(AHexEditor, AOffset, AParent);
    if Assigned(AControl) then
    begin
      Result := Ex;
      exit;
    end;
  end;
end;
*)

function CanExtract(AHexEditor: THxHexEditor): TExtractor;
var
  i: integer;
  item: TExtractorItem;
begin
  for i := 0 to RegisteredExtractors.Count-1 do
  begin
    item := RegisteredExtractors[i];
    if item.FExtractorClass.CheckSignature(AHexEditor, item.FEmbeddedClass, item.FSignature) then
    begin
      Result := item.CreateExtractor;
      exit;
    end;
  end;
  Result := nil;
end;
                       {
function ExtractorFilter(AExtractor: TExtractor): string;
var
  i: integer;
  item: TExtractorItem;
  Ex: TExtractor;
begin
  Result := '';                           (*
  for i := 0 to RegisteredExtractors.Count-1 do
  begin
    item := RegisteredExtractors[i];
    if (item.FExtractorClass = AExtractor.Class) and (item.FSignature = AExtractor.Signature)
    if AExtractor = Ex then
    begin
      Result := Format(SExtractorFilterMask,
        [LowerCase(Ex.FileExt), LowerCase(Ex.FileExt), LowerCase(Ex.FileExt)]);
      exit;
    end;
  end;
  *)
end;                    }

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
                               (*
function Extractor(AIndex: Integer): TExtractor;
begin
  if (AIndex >= 0) and (AIndex < RegisteredExtractors.Count) then
    Result := RegisteredExtractors[AIndex]
  else
    Result := nil;
end;                             *)

function NumExtractors: Integer;
begin
  Result := RegisteredExtractors.Count;
end;


{ TExtractor basic class }

constructor TExtractor.Create(AEmbeddedClass: TClass;
  AFileExt, ASignature: string);
begin
  inherited Create;
  FEmbeddedClass := AEmbeddedClass;
  FSignature := ASignature;
  FFileExt := AFileExt;
end;

{ Checks whether an embedded object exists at the given offset in the HexEditor }
function TExtractor.CanExtract(AHexEditor: THxHexEditor; AOffset: integer): Boolean;
var
  C: TControl;
  P: Integer;
  lInfo: String;
begin
  Result := false;
  if Assigned(AHexEditor) and Assigned(AHexEditor.DataStorage) and (AOffset >= 0) then
    Result := CheckSignature(AHexEditor, FEmbeddedClass, FSignature)
  else
    Result := false;
  (*
  begin
    AHexEditor.Seek(AOffset, soFromBeginning);
    AHexEditor.DataStorage.Position := AOffset;
    if CheckSignature(AHexEditor, FEmbeddedClass, FSignature) then
    begin
      try
        P := AHexEditor.GetCursorPos;
        FHexEditor := AHexEditor;
        FOffset := AOffset;
        FSize := ObjectSize(AHexEditor);
        if FSize = -1 then begin
          C := CreateView(nil, lInfo);   // Moves the stream pos to the end of the object
          if Assigned(C) then
          begin
            FSize := FHexEditor.DataStorage.Position - FOffset;
            Result := true;
          end else
            FSize := -1;
        end;
      finally
        C.Free;
        FHexEditor.DataStorage.Position := P;
        FHexEditor.Seek(P, soFromBeginning);
      end;
    end;
  end;
  *)
end;

{ Possibly an embedded object exists at the "FOffset" of the HexEditor stream.
  CreateView tries to extract this object.
  When this is successful a component matching the embedded object is created.
  If not, the function returns nil.
  "AOwner" is the Owner (and parent) of the component to be created.
  MUST BE OVERRIDDEN FOR EVERY EXTRACTOR TYPE. }
function TExtractor.CreateView(AOwner: TWinControl; out AInfo: String): TControl;
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
begin
  Result := nil;

  FHexEditor := AHexEditor;
  FOffset := AOffset;
  if Assigned(FHexEditor) then
  begin
    AHexEditor.Seek(FOffset, soFromBeginning);
    if CheckSignature(FHexEditor, FEmbeddedClass, FSignature) then
      try
        HideView(AParent);
        Result := CreateView(AParent, FInfo);
        if Assigned(Result) then
        begin
          FSize := FHexEditor.DataStorage.Position - FOffset;
          FHexEditor.Seek(FOffset, soFromBeginning);  // Restore old stream pos
          //HideView(AParent);
          Result.Parent := AParent;
          Result.Tag := FTag;
          FLastResult := true;
          exit;
        end;
      except
        FHexEditor.Seek(FOffset, soFromBeginning);  // Restore stream position
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
  AEmbeddedClass: TClass; const ASignature: String): Boolean;
var
  s: string;
  n: integer;
  P: Integer;
begin
  Result := false;
  P := AHexEditor.GetCursorPos;  //DataStorage.Position;
  try
    AHexEditor.DataStorage.Position := P;
    n := Length(ASignature);
    if (n > 0) and Assigned(AHexEditor) and
       (P >= 0) and (P <= AHexEditor.DataSize - n) then
    begin
      SetLength(s, n);
      AHexEditor.ReadBuffer(s[1], P, n);
      Result := (ASignature = s);
    end;
  finally
    AHexEditor.DataStorage.Position := P;
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

    p := AStart;
    while (p <= AEnd - Length(FSignature)) and (p <> -1) do
    begin
      s := FSignature;
      if s <> '' then
      begin
        p := AHexEditor.Find(PChar(s), Length(s), p, AEnd, false);
        if p = -1 then
        begin
          Result := -1;
          exit;
        end;
      end;
      if CheckSignature(AHexEditor, FEmbeddedClass, FSignature) then
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
  finally
    Screen.Cursor := crs;
  end;
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
  AEmbeddedClass: TClass; const ASignature: String): Boolean;
var
  lGraphicclass: TGraphicClass;
begin
  Result := inherited CheckSignature(AHexEditor, AEmbeddedClass, ASignature);
  if Result or (ASignature = '') then begin
    lGraphicClass := TGraphicClass(AEmbeddedClass);
    Result := lGraphicClass.IsStreamFormatSupported(AHexEditor.DataStorage);
  end;
end;

function TGraphicExtractor.CreateView(AOwner: TWinControl;
  out AInfo: String): TControl;
var
  pic: TPicture;
begin
  pic := TPicture.Create;
  try
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
  AEmbeddedClass: TClass; const ASignature: String): Boolean;
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
  dir: TIconDirectory;
  totalSize: Integer;
begin
  Result := inherited CheckSignature(AHexEditor, AEmbeddedClass, ASignature);
  if Result then begin
    // Read the icon images directory and do a plausibility check
    P := AHexEditor.DataStorage.Position;
    try
      AHexEditor.DataStorage.Position := P + Length(ASignature);
      // Read number of images
      n := LEToN(AHexEditor.DataStorage.ReadWord);
      if n = 0 then
        exit(false);
      totalSize := 0;
      for i:=1 to n do begin
        AHexEditor.DataStorage.Read(dir, SizeOf(dir));
        if dir.Reserved <> 0 then exit(false);
        if not (LEToN(dir.BitsPerPixel) in [1, 4, 8, 16, 24, 32]) then
          exit(false);
        if not (LEToN(dir.ColorPlanes) in [0, 1]) then
          exit(false);
        totalSize := totalSize + LEToN(dir.ImageDataSize);
        if totalSize > AHexEditor.DataSize then
          exit(false);
        if LEToN(dir.OffsetToImage) > totalSize then
          exit(false);
      end;
    finally
      AHexEditor.DataStorage.Position := P;
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

function TIconExtractor.CreateView(AOwner: TWinControl; out AInfo: String): TControl;
var
  ico: TIcon;
  i: Integer;
begin
  ico := TIcon.Create;
  try
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
    ico.Free;
  end;
end;


{ TObjectViewerFrame }

destructor TObjectViewerFrame.Destroy;
begin
  FreeAndNil(FExtractor);
  inherited;
end;

procedure TObjectViewerFrame.UpdateData(AHexEditor: THxHexEditor);
begin
  if FLockUpdate > 0 then
    exit;

  FreeAndNil(FExtractor);
  FExtractor := CanExtract(AHexEditor);       // creates a new extractor
  if FExtractor <> nil then
  begin
    FExtractorControl := FExtractor.CheckAndShow(AHexEditor, AHexEditor.GetCursorPos, ScrollBox);
    lblInfo.Caption := FExtractor.Info;
  end else begin
    FreeAndNil(FExtractorControl);
    lblInfo.caption := '';
  end;
end;


initialization
  RegisteredExtractors := TExtractorList.Create;
  RegisterExtractor(TGraphicExtractor, TBitmap, 'bmp', 'BM');
  RegisterExtractor(TGraphicExtractor, TGifImage, 'gif', 'GIF');
  RegisterExtractor(TIconExtractor, TIcon, 'ico', #0#0#1#0);
  RegisterExtractor(TGraphicExtractor, TJpegImage, 'jpg|jpeg|jfe', #$FF#$D8);
  RegisterExtractor(TGraphicExtractor, TPortableNetworkGraphic, 'png', #137'PNG'#13#10#26#10);
  RegisterExtractor(TGraphicExtractor, TTiffImage, 'tif|tiff', '');

  {
  RegisterRipper(TPcxRipper, rtGraphics, 'PCX', #10,
    TPcxGraphic);

  RegisterRipper(TWavRipper, rtMediaPlayer, 'WAV', 'RIFF');
  RegisterRipper(TAviRipper, rtMediaPlayer, 'AVI', 'RIFF');
  }

finalization
  RegisteredExtractors.Free;


end.

