unit hxHexEditorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  Forms, Controls, ComCtrls, ExtCtrls, Dialogs, Menus, ActnList,
  hxGlobal, hxUtils_NonGUI, hxUtils, hxHexEditor, hxLayout,
  hxBasicViewerFrame, hxDataViewerFrame, hxRecordViewerFrame, hxObjectViewerFrame;

type

  { THexEditorFrame }

  THexEditorFrame = class(TFrame)
    SaveDialog: TSaveDialog;
  private
    FLayout: TLayout;
    FHexEditor: THxHexEditor;
    FDataViewer: TDataViewerFrame;
    FRecordViewer: TRecordViewerFrame;
    FObjectViewer: TObjectViewerFrame;
    FStatusbarItems: TStatusbarItems;
    FStatusbarPosDisplay: TOffsetDisplayBase;
    FStatusbarSelDisplay: TOffsetDisplayBase;
    FObjectSaveDir: String;
    FOnChange: TNotifyEvent;
    FOnUpdateStatusBar: TNotifyEvent;

    FExtractorAborted: Boolean;
    FExtractorIndex: Integer;

    function GetFileName: String;

    function GetOffsetDisplayBase(AOffsetFormat: String): TOffsetDisplayBase;
    function GetOffsetDisplayHexPrefix(AOffsetFormat: String): String;

    function GetShowDataViewer: Boolean;
    procedure SetShowDataViewer(AValue: Boolean);
    function GetDataViewerPosition: TViewerPosition;
    procedure SetDataViewerPosition(AValue: TViewerPosition);

    function GetShowObjectViewer: Boolean;
    procedure SetShowObjectViewer(AValue: Boolean);
    function GetObjectViewerPosition: TViewerPosition;
    procedure SetObjectViewerPosition(AValue: TViewerPosition);

    function GetShowRecordViewer: Boolean;
    procedure SetShowRecordViewer(AValue: Boolean);
    function GetRecordViewerPosition: TViewerPosition;
    procedure SetRecordViewerPosition(AValue: TViewerPosition);

    procedure SetOnChange(AValue: TNotifyEvent);

  protected
    procedure CheckUserAbortHandler(Sender: TObject; var Aborted: Boolean);
    procedure CreateHexEditor;
    procedure CreateDataViewer;
    procedure CreateRecordViewer;
    procedure CreateObjectViewer;
    procedure DoUpdateStatusBar;
    function GetViewerPosition(AViewer: TBasicViewerFrame): TViewerPosition;
    function GetWriteProtected: Boolean;
    procedure HexEditorChanged(Sender: TObject);
    procedure SetParent(AParent: TWinControl); override;
    procedure SetViewerPosition(AViewer: TBasicViewerFrame; AValue: TViewerPosition);
    procedure SetWriteProtected(const AValue: Boolean);

    procedure LoadFromIni;
    procedure SaveToIni;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActiveColors(var AParams: TColorParams);
    procedure ActiveHexParams(var AParams: THexParams);
    procedure ApplyHexParams(const AParams: THexParams);
    function CanSaveFileAs(const AFileName: String): Boolean;
    procedure ExportObject;
    procedure FindDlg;
    procedure FindObjectHandler(Sender: TObject);
    procedure InsertMode(const AEnable: Boolean);
    procedure JumpToPosition(APosition: Integer);
    procedure OpenFile(const AFileName: string; WriteProtected: boolean);
    procedure ReplaceDlg;
    procedure SaveFile;
    procedure SaveFileAs(const AFileName: string);
    procedure SaveSelectionToFile(const AFileName: string);
    procedure SelectObject;
    procedure UpdateCaption;
    procedure UpdateIconSet;
    procedure UpdateStatusBar(AStatusBar: TStatusBar);
    procedure UpdateStatusBarPanelWidths(AStatusBar: TStatusBar);

    property Caption;
    property ExtractorIndex: Integer
      read FExtractorIndex;
    property FileName: String
      read GetFileName;
    property HexEditor: THxHexEditor
      read FHexEditor;
    property DataViewerPosition: TViewerPosition
      read GetDataViewerPosition write SetDataViewerPosition;
    property ObjectViewer: TObjectViewerFrame
      read FObjectViewer;
    property ObjectViewerPosition: TViewerPosition
      read GetObjectViewerPosition write SetObjectViewerPosition;
    property RecordViewerPosition: TViewerPosition
      read GetRecordViewerPosition write SetRecordViewerPosition;
    property ShowDataViewer: Boolean
      read GetShowDataViewer write SetShowDataViewer;
    property ShowObjectViewer: Boolean
      read GetShowObjectViewer write SetShowObjectViewer;
    property ShowRecordViewer: Boolean
      read GetShowRecordViewer write SetShowRecordViewer;
    property WriteProtected: Boolean
      read GetWriteProtected write SetWriteProtected;
    property OnChange: TNotifyEvent
      read FOnChange write SetOnChange;
    property OnUpdateStatusBar: TNotifyEvent
      read FOnUpdateStatusBar write FOnUpdateStatusBar;

  end;


implementation

{$R *.lfm}

uses
  StrUtils, IniFiles, Math,
  hxStrings, hxSearchReplaceDlg;

constructor THexEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FLayout := TLayout.Create(Self);
  FExtractorIndex := -1;
end;

destructor THexEditorFrame.Destroy;
begin
  SaveToIni;
  FLayout.Free;
  inherited;
end;

procedure THexEditorFrame.ActiveColors(var AParams: TColorParams);
begin
  if IsDarkMode then
    AParams := ColorParams[smDarkMode]
  else
    AParams := ColorParams[smLightMode];
  if Assigned(HexEditor) then
  begin
    AParams.BackgroundColor := HexEditor.Colors.Background;
    AParams.ActiveFieldBackgroundColor := HexEditor.Colors.ActiveFieldBackground;
    AParams.OffsetBackgroundColor := HexEditor.Colors.OffsetBackground;
    AParams.OffsetForegroundColor := HexEditor.Colors.Offset;
    AParams.CurrentOffsetBackgroundColor := HexEditor.Colors.CurrentOffsetBackground;
    AParams.CurrentOffsetForegroundColor := HexEditor.Colors.CurrentOffset;
    AParams.EvenColumnForegroundColor := HexEditor.Colors.EvenColumn;
    AParams.OddColumnForegroundColor := HexEditor.Colors.OddColumn;
    AParams.ChangedBackgroundColor := HexEditor.Colors.ChangedBackground;
    AParams.ChangedForegroundColor := HexEditor.Colors.ChangedText;
    AParams.CharFieldForegroundColor := HexEditor.Font.Color;
  end;
end;

procedure THexEditorFrame.ActiveHexParams(var AParams: THexParams);
var
  i: Integer;
begin
  AParams := HexParams;

  if Assigned(HexEditor) then
  begin
    AParams.ViewOnly := HexEditor.ReadOnlyView;
    AParams.WriteProtected := HexEditor.ReadOnlyFile;
    AParams.AllowInsertMode := HexEditor.AllowInsertMode;
    AParams.InsertMode := HexEditor.InsertMode;

    AParams.RulerVisible := HexEditor.ShowRuler;
    AParams.RulerNumberBase := GetOffsetDisplayBase(IntToStr(HexEditor.RulerNumberBase));
    AParams.OffsetDisplayBase := GetOffsetDisplayBase(HexEditor.OffsetFormat);
    AParams.OffsetDisplayHexPrefix := GetOffsetDisplayHexPrefix(HexEditor.OffsetFormat);
    AParams.BytesPerRow := HexEditor.BytesPerRow;
    AParams.BytesPerColumn := HexEditor.BytesPerColumn;
    AParams.MaskChar := HexEditor.MaskChar;

    AParams.FontName := HexEditor.Font.Name;
    AParams.FontSize := HexEditor.Font.Size;
    AParams.HexLowercase := HexEditor.HexLowercase;
    AParams.DrawGutter3D := HexEditor.DrawGutter3D;
  end;

  AParams.StatusbarItems := FStatusbarItems;
  AParams.StatusbarPosDisplay := FStatusbarPosDisplay;
  AParams.StatusbarSelDisplay := FStatusbarSelDisplay;

  if Assigned(FDataViewer) then
  begin
    AParams.DataViewerVisible := GetShowDataViewer;
    AParams.DataViewerPosition := GetDataViewerPosition;
    for i:=0 to High(AParams.DataViewerColWidths) do
      AParams.DataViewerColWidths[i] := FDataViewer.ColWidths[i];
  end;

  if Assigned(FObjectViewer) then
  begin
    AParams.ObjectViewerVisible := GetShowObjectViewer;
    AParams.ObjectViewerPosition := GetObjectViewerPosition;
    AParams.ObjectViewerInfoHeight := FObjectViewer.InfoHeight;
  end;

  if Assigned(FRecordViewer) then
  begin
    AParams.RecordViewerVisible := GetShowRecordViewer;
    AParams.RecordViewerPosition := GetRecordViewerPosition;
    for i:=0 to High(AParams.RecordViewerColWidths) do
      AParams.RecordViewerColWidths[i] := FRecordViewer.ColWidths[i];
  end;
end;

procedure THexEditorFrame.ApplyHexParams(const AParams: THexParams);
var
  i: Integer;
begin
  ApplyParamsToHexEditor(AParams, HexEditor);

  FStatusbarItems := AParams.StatusbarItems;
  FStatusbarPosDisplay := AParams.StatusbarPosDisplay;
  FStatusbarSelDisplay := AParams.StatusbarSelDisplay;

  ShowDataViewer := AParams.DataViewerVisible;         // this creates the NumViewer
  if Assigned(FDataViewer) then
  begin
    FDataViewer.UpdateDataList;
    SetDataViewerPosition(AParams.DataViewerPosition);
    for i := 0 to High(AParams.DataViewerColWidths) do
      FDataViewer.ColWidths[i] := AParams.DataViewerColWidths[i];
  end;

  ShowObjectViewer := AParams.ObjectViewerVisible;   // this creates the ObjectViewer
  if Assigned(FObjectViewer) then
  begin
    SetObjectViewerPosition(AParams.ObjectViewerPosition);
    FObjectViewer.InfoHeight := AParams.ObjectViewerInfoHeight;
  end;

  ShowRecordViewer := AParams.RecordViewerVisible;   // this creates the RecordViewer
  if Assigned(FRecordViewer) then
  begin
    FRecordViewer.UpdateDataList;
    SetRecordViewerPosition(AParams.RecordViewerPosition);
    for i := 0 to High(AParams.RecordViewerColWidths) do
      FRecordViewer.ColWidths[i] := AParams.RecordViewerColWidths[i];
  end;
end;

function THexEditorFrame.CanSaveFileAs(const AFileName: String): Boolean;
begin
  Result := not ((AFileName = GetFileName) and HexEditor.ReadOnlyFile);
end;

procedure THexEditorFrame.CheckUserAbortHandler(Sender: TObject;
  var Aborted: boolean);
begin
  Aborted := FExtractorAborted;
end;

procedure THexEditorFrame.CreateHexEditor;
begin
  FHexEditor := THxHexEditor.Create(self);
  FHexEditor.Align := alClient;
  FHexEditor.DrawGutter3D := false;
  FHexEditor.WantTabs := false;
  FHexEditor.OnChange := @HexEditorChanged;
  FHexEditor.OnSelectionChanged := @HexEditorChanged;
  FLayout.AddCenterControl(FHexEditor);

  ApplyParamsToHexEditor(HexParams, FHexEditor);

  FHexEditor.ReadOnlyView := true;
  FHexEditor.ReadOnlyFile := true;

//  CommonData.BookmarkImages.GetFullBitmap(FHexEditor.BookmarkBitmap);
end;

procedure THexEditorFrame.CreateDataViewer;
begin
  FDataViewer.Free;
  FDataViewer := TDataViewerFrame.Create(self);
  FDataViewer.Name := '';
end;

procedure THexEditorFrame.CreateRecordViewer;
begin
  FRecordViewer.Free;
  FRecordViewer := TRecordViewerFrame.Create(self);
  FRecordViewer.Name := '';
end;

procedure THexEditorFrame.CreateObjectViewer;
begin
  FObjectViewer.Free;
  FObjectViewer := TObjectViewerFrame.Create(self);
  FObjectViewer.Name := '';
end;

procedure THexEditorFrame.ExportObject;
var
  stream: TStream;
  ex: TExtractor;
  lSize: Integer;
begin
  if FObjectViewer = nil then
    exit;

  ex := FObjectViewer.Extractor;

  if (not ShowObjectViewer) or (not Assigned(ex)) or
     (not ex.CanExtract(FHexEditor, FHexEditor.GetCursorPos, lSize))
  then
    exit;

  with SaveDialog do begin
    InitialDir := FObjectSaveDir;
    DefaultExt := Lowercase(Format('*.%s', [ex.FirstFileExt]));
    Filter := ex.ExtractorFilter;
    FileName := '';
    if Execute then begin
      Application.ProcessMessages;
      stream := TFileStream.Create(FileName, fmCreate + fmShareDenyNone);
      try
        ex.SaveToStream(stream);
        FObjectSaveDir := ExtractFileDir(FileName);
      finally
        stream.Free;
      end;
    end;
  end;
end;

procedure THexEditorFrame.DoUpdateStatusBar;
begin
  if Assigned(FOnUpdateStatusBar) then
    FOnUpdateStatusBar(self);
end;

procedure THexEditorFrame.FindDlg;
begin
  if (FHexEditor = nil) or (FHexEditor.DataSize < 1) then
    Exit;

  if SearchReplaceForm = nil then begin
    SearchReplaceForm := TSearchReplaceForm.Create(Application);
    SearchReplaceForm.FormStyle := fsStayOnTop;
  end;
  SearchReplaceForm.HexEditor := FHexEditor;
  SearchReplaceForm.BigEndian := HexParams.BigEndian;
  SearchReplaceForm.Mode := srmSearch;
  SearchReplaceForm.Show;
end;

procedure THexEditorFrame.FindObjectHandler(Sender: TObject);
var
  idx: Integer;
begin
  if Assigned(FObjectViewer) then
  begin
    idx := (Sender as TComponent).Tag - TAG_FIND_OBJECTS - 1;
    if (idx < 0) and (FExtractorIndex >= 0) then  // "Find next"
      FObjectViewer.FindObject(FExtractorIndex, HexEditor, HexEditor.GetCursorPos+1)
    else
    if (idx >= 0) and FObjectViewer.FindObject(idx, HexEditor, 0) then
      FExtractorIndex := idx
    else
      FExtractorIndex := -1;
  end;
end;

function THexEditorFrame.GetFileName: String;
begin
  if Assigned(FHexEditor) then
    Result := FHexEditor.FileName
  else
    Result := '';
end;

function THexEditorFrame.GetOffsetDisplayBase(AOffsetFormat: String): TOffsetDisplayBase;
var
  p1, p2: Integer;
  s: String;
begin
  p1 := Pos('!', AOffsetFormat);
  p2 := Pos(':', AOffsetFormat);
  if (p1 = 0) and (p2 = 0) then
    s := AOffsetFormat
  else
    s := '$' + copy(AOffsetFormat, p1+1, p2-p1-1);
  case StrToInt(s) of
    16: Result := odbHex;
    10: Result := odbDec;
     8: Result := odbOct;
  end;
end;

function THexEditorFrame.GetDataViewerPosition: TViewerPosition;
begin
  Result := GetViewerPosition(FDataViewer);
end;

function THexEditorFrame.GetObjectViewerPosition: TViewerPosition;
begin
  Result := GetViewerPosition(FObjectViewer);
end;

function THexEditorFrame.GetOffsetDisplayHexPrefix(AOffsetFormat: String): String;
var
  p1, p2: Integer;
begin
  p1 := Pos(':', AOffsetFormat);
  p2 := Pos('|', AOffsetFormat);
  Result := Copy(AOffsetFormat, p1+1, p2-p1-1);
end;

function THexEditorFrame.GetShowDataViewer: Boolean;
begin
  Result := (FDataViewer <> nil) and FDataViewer.Visible;
end;

function THexEditorFrame.GetShowObjectViewer: Boolean;
begin
  Result := (FObjectViewer <> nil) and FObjectViewer.Visible;
end;

function THexEditorFrame.GetRecordViewerPosition: TViewerPosition;
begin
  Result := GetViewerPosition(FRecordViewer);
end;

function THexEditorFrame.GetShowRecordViewer: Boolean;
begin
  Result := (FRecordViewer <> nil) and FRecordViewer.Visible;
end;

function THexEditorFrame.GetViewerPosition(AViewer: TBasicViewerFrame): TViewerPosition;
var
  VP: Integer;
begin
  VP := FLayout.GetViewerPosition(AViewer);
  if VP > -1 then
    Result := TViewerPosition(VP)
  else
    raise Exception.Create('Viewer not found.');
end;

function THexEditorFrame.GetWriteProtected: Boolean;
begin
  Result := Assigned(FHexEditor) and FHexEditor.ReadOnlyView;
end;

procedure THexEditorFrame.HexEditorChanged(Sender: TObject);
begin
  DoUpdateStatusBar;
  if Assigned(FDataViewer) then
    FDataViewer.UpdateData(FHexEditor);
  if Assigned(FObjectViewer) then
    FObjectViewer.UpdateData(FHexEditor);
  if Assigned(FRecordViewer) then
    FRecordViewer.UpdateData(FHexEditor);
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure THexEditorFrame.InsertMode(const AEnable: Boolean);
begin
  if Assigned(FHexEditor) then
  begin
    FHexEditor.InsertMode := AEnable;
    DoUpdateStatusBar;
  end;
end;

procedure THexEditorFrame.JumpToPosition(APosition: Integer);
var
  ok : boolean;
begin
  if not Assigned(HexEditor) then
    exit;

  ok := true;
  if APosition < 0 then
  begin
    ok := Confirm(SGotoPastBOF);
    if ok then APosition := 0;
  end
  else
  if APosition > HexEditor.DataSize then
  begin
    ok := Confirm(SGotoPastEOF);
    if ok then
      APosition := HexEditor.DataSize - 1;
  end;

  if ok then
    HexEditor.Seek(APosition, soFromBeginning);
end;

procedure THexEditorFrame.LoadFromIni;
var
  ini: TCustomIniFile;
begin
  if Assigned(FLayout) then
  begin
    ini := CreateIniFile;
    try
      FLayout.LoadFromIni(ini);
    finally
      ini.Free;
    end;
  end;
end;

procedure THexEditorFrame.OpenFile(const AFileName: string; WriteProtected: boolean);
begin
  if Assigned(HexEditor) then
  begin
    HexEditor.LoadFromFile(AFileName);
    HexEditor.ReadOnlyFile := WriteProtected;
    UpdateCaption;
  end;
end;

procedure THexEditorFrame.ReplaceDlg;
begin
  if (FHexEditor = nil) or (FHexEditor.DataSize < 1) then
    Exit;

  if SearchReplaceForm = nil then begin
    SearchReplaceForm := TSearchReplaceForm.Create(Application);
    SearchReplaceForm.FormStyle := fsStayOnTop;
  end;
  SearchReplaceForm.HexEditor := FHexEditor;
  SearchReplaceForm.BigEndian := HexParams.BigEndian;
  SearchReplaceForm.Mode := srmReplace;
  SearchReplaceForm.Show;
end;

procedure THexEditorFrame.SaveFile;
begin
  if Assigned(HexEditor) then
    SaveFileAs(HexEditor.FileName);
end;

procedure THexEditorFrame.SaveFileAs(const AFileName: string);
begin
  if Assigned(HexEditor) then
  begin
    if not CanSaveFileAs(AFileName) then
    begin
      MessageDlg(Format(SReadOnlyFile, [AFileName]), mtError, [mbOK], 0);
      exit;
    end;

    try
      HexEditor.SaveToFile(AFileName);
      UpdateCaption;
    except
      on E: Exception do
        ErrorFmt(SErrorSavingFile + LineEnding + E.Message, [AFileName]);
    end;
  end;
end;

procedure THexEditorFrame.SaveSelectionToFile(const AFileName: String);
var
  stream: TFileStream;
begin
  if Assigned(FHexEditor) then
  begin
    if FHexEditor.SelCount = 0 then
      exit;
    stream := TFileStream.Create(AFileName, fmCreate);
    try
      FHexEditor.SaveRangeToStream(stream, FHexEditor.SelStart, FHexEditor.SelCount);
    finally
      stream.Free;
    end;
  end;
end;

procedure THexEditorFrame.SelectObject;
var
  P: Integer;
  lSize: Integer;
begin
  if ShowObjectViewer and Assigned(FObjectViewer.Extractor) then
  begin
    P := FHexEditor.GetCursorPos;
    if FObjectViewer.Extractor.CanExtract(FHexEditor, P, lSize) then
    begin
      FHexEditor.SelStart := P;
      FHexEditor.SelEnd := P + lSize - 1;
    end;
  end;
end;

procedure THexEditorFrame.SetDataViewerPosition(AValue: TViewerPosition);
begin
  SetViewerPosition(FDataViewer, AValue);
end;

procedure THexEditorFrame.SetObjectViewerPosition(AValue: TViewerPosition);
begin
  SetViewerPosition(FObjectViewer, AValue);
end;

procedure THexEditorFrame.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  if Assigned(FHexEditor) then FHexEditor.OnChange := FOnChange;
end;

procedure THexEditorFrame.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent <> nil) and (FHexEditor = nil) then
    CreateHexEditor;
  ApplyHexParams(HexParams);
  ApplyColorsToHexEditor(ColorParams[GetScreenMode], FHexEditor);
  if AParent <> nil then
    LoadFromIni;
  if HexEditor.CanFocus then
    HexEditor.SetFocus;
end;

procedure THexEditorFrame.SetRecordViewerPosition(AValue: TViewerPosition);
begin
  SetViewerPosition(FRecordViewer, AValue);
end;

procedure THexEditorFrame.SetShowDataViewer(AValue: Boolean);
begin
  if AValue then
  begin
    // Show number viewer
    if Assigned(FDataViewer) then
      exit;
    CreateDataViewer;
    FLayout.ShowControl(FDataViewer, HexParams.DataViewerPosition);
    FDataViewer.UpdateData(HexEditor);
    HexParams.DataViewerVisible := true;
  end else
  begin
    // Hide number viewer
    if not Assigned(FDataViewer) then
      exit;
    FLayout.HideControl(FDataViewer);
    FreeAndNil(FDataViewer);
    HexParams.DataViewerVisible := false;
  end;
end;

procedure THexEditorFrame.SetShowObjectViewer(AValue: Boolean);
begin
  if AValue then
  begin
    // Show object viewer
    if Assigned(FObjectViewer) then
      exit;
    CreateObjectViewer;
    FLayout.ShowControl(FObjectViewer, HexParams.ObjectViewerPosition);
    FObjectViewer.InfoHeight := HexParams.ObjectViewerInfoHeight;
    FObjectViewer.UpdateData(HexEditor);
    HexParams.ObjectViewerVisible := true;
  end else
  begin
    // Hide object viewer
    if not Assigned(FObjectViewer) then
      exit;
    FLayout.HideControl(FObjectViewer);
    FreeAndNil(FObjectViewer);
    HexParams.ObjectViewerVisible := false;
  end;
end;

procedure THexEditorFrame.SetShowRecordViewer(AValue: Boolean);
begin
  if AValue then
  begin
    // Show record viewer
    if Assigned(FRecordViewer) then
      exit;
    CreateRecordViewer;
    FLayout.ShowControl(FRecordViewer, HexParams.RecordViewerPosition);
    FRecordViewer.UpdateData(HexEditor);
    HexParams.RecordViewerVisible := true;
  end else
  begin
    // Hide record viewer
    if not Assigned(FRecordViewer) then
      exit;
    FLayout.HideControl(FRecordViewer);
    FreeAndNil(FRecordViewer);
    HexParams.RecordViewerVisible := false;
  end;
end;

procedure THexEditorFrame.SetViewerPosition(AViewer: TBasicViewerFrame;
  AValue: TViewerPosition);
begin
  FLayout.ShowControl(AViewer, AValue);
end;

procedure THexEditorFrame.SetWriteProtected(const AValue: Boolean);
begin
  if Assigned(FHexEditor) then
    FHexEditor.ReadOnlyView := AValue;
end;

procedure THexEditorFrame.UpdateCaption;
begin
  if Assigned(FHexEditor) then
  begin
    Caption := ExtractFilename(FHexEditor.FileName);
    if FHexEditor.Modified then
      Caption := '* ' + Caption //Format(SWriteProtectedCaption, [Caption]);
  end else
    Caption := SEmptyCaption;
end;

procedure THexEditorFrame.UpdateIconSet;
begin
  if Assigned(FDataViewer) then
    FDataViewer.UpdateIconSet;
  if Assigned(FRecordViewer) then
    FRecordViewer.UpdateIconSet;
  if Assigned(FObjectViewer) then
    FObjectViewer.UpdateIconSet;
end;

procedure THexEditorFrame.UpdateStatusbar(AStatusBar: TStatusBar);
// Panel 0: Modified           (width=40)
// Panel 1: ReadOnly           (      30)
// Panel 2: Insert / Overwrite (      40)
// Panel 3: Position           (      80)
// Panel 4: Markierung         (      180)
// Panel 5: Dateigröße         (      120)
var
  p: integer;
  i1, i2, n: integer;
  s: string;
  hexprefix: String;
begin
  inherited;
  if Assigned(HexEditor) then
  begin
    hexprefix := GetOffsetDisplayHexPrefix(HexEditor.OffsetFormat);

    AStatusbar.Panels[0].Text := IfThen(HexEditor.Modified, 'MOD', '');
    if HexEditor.ReadOnlyView then
      AStatusBar.Panels[1].Text := 'R/O'
    else
      AStatusbar.Panels[1].Text := IfThen(HexEditor.InsertMode, 'INS', 'OVW');

    p := 2;
    if sbPos in FStatusbarItems then
    begin
      case FStatusbarPosDisplay of
        odbDec: s := Format('%.0n', [1.0*HexEditor.GetCursorPos]);
        odbHex: s := Format('%s%x', [hexprefix, HexEditor.GetCursorPos]);
        odbOct: s := Format('&%s', [IntToOctal(HexEditor.GetCursorPos)]);
      end;
      s := Format(SMaskPos, [s]);
      AStatusbar.Panels[p].Text := s;
      inc(p);
    end;

    if sbSel in FStatusbarItems then
    begin
      if HexEditor.SelCount <> 0 then
      begin
        i1 := Min(HexEditor.SelStart, HexEditor.SelEnd);
        i2 := Max(HexEditor.SelStart, HexEditor.SelEnd);
        n := HexEditor.SelCount;
        case FStatusbarSelDisplay of
          odbDec: s := Format('%.0n ... %.0n (%.0n)', [1.0*i1, 1.0*i2, 1.0*n]);
          odbHex: s := Format('%0:s%1:x ... %0:s%2:x (%0:s%3:x)', [hexPrefix, i1, i2, n]);
          odbOct: s := Format('&%s ... &%s (&%s)', [IntToOctal(i1), IntToOctal(i2), IntToOctal(n)]);
        end;
      end else
        s := '';
      AStatusBar.Panels[p].Text := s;
      inc(p);
    end;

    if sbSize in FStatusbarItems then
    begin
      s := Format('%.0n', [1.0 * HexEditor.DataSize]);
      AStatusBar.Panels[p].Text := Format(SMaskSize, [s]);
      inc(p);
    end;
  end;
end;

procedure THexEditorFrame.UpdateStatusbarPanelWidths(AStatusBar: TStatusBar);
var
  n: integer;
  s: String;
  hexPrefix: String;
begin
  AStatusbar.Canvas.Font.Assign(AStatusbar.Font);
  hexprefix := GetOffsetDisplayHexPrefix(HexEditor.OffsetFormat);

  AStatusBar.Panels.Clear;

  // "Modified" flag
  AStatusBar.Panels.Add.Width := 40;

  // "ReadOnly" flag
  AStatusBar.Panels.Add.Width := 30;

  // Position
  if (sbPos in FStatusBarItems) then
    if Assigned(HexEditor) then
    begin
      case FStatusBarPosDisplay of
        odbDec: s := Format('%.0n', [1.0*HexEditor.DataSize]);
        odbHex: s := Format('%s%x', [hexprefix, HexEditor.DataSize]);
        odbOct: s := Format('&%s', [IntToOctal(HexEditor.DataSize)]);
      end;
      s := Format(SMaskPos, [s]);
      AStatusBar.Panels.Add.Width := AStatusbar.Canvas.TextWidth(s) + 10;
    end else
      AStatusBar.Panels.Add.Width := 120;

  // Selection
  if sbSel in FStatusbarItems then
    if Assigned(FHexEditor) then
    begin
      n := HexEditor.DataSize;
      case FStatusbarSelDisplay of
        odbDec: s := Format('%.0n ... %.0n (%.0n)', [1.0*n, 1.0*n, 1.0*n]);
        odbHex: s := Format('%0:s%1:x ... %0:s%2:x (%0:s%3:x)', [hexPrefix, n, n, n]);
        odbOct: s := Format('&%s ... &%s (&%s)', [IntToOctal(n), IntToOctal(n), IntToOctal(n)]);
      end;
      AStatusbar.Panels.Add.Width := AStatusbar.Canvas.TextWidth(s) + 10;
    end else
      AStatusbar.Panels.Add.Width := 250;

  // Data size
  if sbSize in FStatusbarItems then
    if Assigned(HexEditor) then
    begin
      s := Format('%.0n', [1.0 * HexEditor.DataSize]);
      s := Format(SMaskSize, [s]);
      AStatusBar.Panels.Add.Width := AStatusBar.Canvas.TextWidth(s) + 10;
    end else
      AStatusbar.Panels.Add.Width := 150;

  // Hint texts
  AStatusBar.Panels.Add.Width := 200;
end;

procedure THexEditorFrame.SaveToIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIniFile;
  try
    FLayout.SaveToIni(ini);
  finally
    ini.Free;
  end;
end;

end.

