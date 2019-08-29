unit hxHexEditorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  MPHexEditor,
  hxGlobal, hxNumViewerFrame, hxRecordViewerFrame;

type

  { THexEditorFrame }

  THexEditorFrame = class(TFrame)
    StatusBar: TStatusBar;
  private
    FHexEditor: TMPHexEditor;
    FNumViewer: TNumViewerFrame;
    FNumViewerSplitter: TSplitter;
    FRecordViewer: TRecordViewerFrame;
    FRecordViewerSplitter: TSplitter;
    FStatusbarItems: TStatusbarItems;
    FStatusbarPosDisplay: TOffsetDisplayBase;
    FStatusbarSelDisplay: TOffsetDisplayBase;
    FOnChange: TNotifyEvent;
    function GetFileName: String;

    function GetOffsetDisplayBase(AOffsetFormat: String): TOffsetDisplayBase;
    function GetOffsetDisplayHexPrefix(AOffsetFormat: String): String;

    function GetShowNumViewer: Boolean;
    procedure SetShowNumViewer(AValue: Boolean);
    function GetNumViewerPosition: TViewerPosition;
    procedure SetNumViewerPosition(AValue: TViewerPosition);

    function GetShowRecordViewer: Boolean;
    procedure SetShowRecordViewer(AValue: Boolean);
    function GetRecordViewerPosition: TViewerPosition;
    procedure SetRecordViewerPosition(AValue: TViewerPosition);

    procedure SetOnChange(AValue: TNotifyEvent);

  protected
    procedure CreateHexEditor;
    procedure CreateNumViewer;
    procedure CreateRecordViewer;
    procedure FixViewerSplitterPosition(AViewer: TControl; ASplitter: TSplitter);
    procedure HexEditorChanged(Sender: TObject);
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateStatusBarPanelWidths;

  public
    procedure ActiveParams(var AParams: THexParams);
    procedure ApplyParams(const AParams: THexParams);
    function CanSaveFileAs(const AFileName: String): Boolean;
    procedure InsertMode(const AEnable: Boolean);
    procedure JumpToPosition(APosition: Integer);
    procedure OpenFile(const AFileName: string; WriteProtected: boolean);
    procedure SaveFile;
    procedure SaveFileAs(const AFileName: string);
    procedure UpdateCaption;
    procedure UpdateStatusBar;

    property Caption;
    property FileName: String
      read GetFileName;
    property HexEditor: TMPHexEditor
      read FHexEditor;
    property NumViewerPosition: TViewerPosition
      read GetNumViewerPosition write SetNumViewerPosition;
    property RecordViewerPosition: TViewerPosition
      read GetRecordViewerPosition write SetRecordViewerPosition;
    property ShowNumViewer: Boolean
      read GetShowNumViewer write SetShowNumViewer;
    property ShowRecordViewer: Boolean
      read GetShowRecordViewer write SetShowRecordViewer;
    property OnChange: TNotifyEvent
      read FOnChange write SetOnChange;

  end;

implementation

{$R *.lfm}

uses
  StrUtils,
  hxStrings, hxUtils;

procedure THexEditorFrame.ActiveParams(var AParams: THexParams);
var
  i: Integer;
begin
  AParams := HexParams;

  if Assigned(HexEditor) then begin
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

  AParams.ShowStatusBar := StatusBar.Visible;
  AParams.StatusbarItems := FStatusbarItems;
  AParams.StatusbarPosDisplay := FStatusbarPosDisplay;
  AParams.StatusbarSelDisplay := FStatusbarSelDisplay;

  if Assigned(FNumViewer) then begin
    AParams.NumViewerVisible := GetShowNumViewer;
    AParams.NumViewerPosition := GetNumViewerPosition;
    if FNumViewer.Align in [alLeft, alRight] then
      AParams.NumViewerWidth := FNumViewer.Width
    else if FNumViewer.Align in [alBottom, alTop] then
      AParams.NumViewerHeight := FNumViewer.Height;
    for i:=0 to High(AParams.NumViewerColWidths) do
      AParams.NumViewerColWidths[i] := FNumViewer.ColWidths[i];
  end;

  if Assigned(FRecordViewer) then begin
    AParams.RecordViewerVisible := GetShowRecordViewer;
    AParams.RecordViewerPosition := GetRecordViewerPosition;
    if FRecordViewer.Align in [alLeft, alRight] then
      AParams.RecordViewerWidth := FRecordViewer.Width
    else if FRecordViewer.Align in [alBottom, alTop] then
      AParams.RecordViewerHeight := FRecordViewer.Height;
    for i:=0 to High(AParams.RecordViewerColWidths) do
      AParams.RecordViewerColWidths[i] := FRecordViewer.ColWidths[i];
  end;
end;

procedure THexEditorFrame.ApplyParams(const AParams: THexParams);
var
  i: Integer;
begin
  if Assigned(HexEditor) then begin
    HexEditor.ReadOnlyView := AParams.ViewOnly;
    HexEditor.ReadOnlyFile := AParams.WriteProtected;
    HexEditor.AllowInsertMode := AParams.AllowInsertMode;
    HexEditor.InsertMode := AParams.InsertMode;
    // To do: Big endian

    HexEditor.OffsetFormat := AParams.GetOffsetFormat;
    HexEditor.ShowRuler := AParams.RulerVisible;
    case AParams.RulerNumberBase of
      odbDec: HexEditor.RulerNumberBase := 10;
      odbHex: HexEditor.RulerNumberBase := 16;
      odbOct: HexEditor.RulerNumberBase := 8;
    end;
    HexEditor.BytesPerRow := AParams.BytesPerRow;
    HexEditor.BytesPerColumn := AParams.BytesPerColumn;

    HexEditor.Colors.Background := AParams.BackgroundColor;
    HexEditor.Colors.ActiveFieldBackground := AParams.ActiveFieldBackgroundColor;
    HexEditor.Colors.OffsetBackground := AParams.OffsetBackgroundColor;
    HexEditor.Colors.Offset := AParams.OffsetForegroundColor;
    HexEditor.Colors.CurrentOffsetBackground := AParams.CurrentOffsetBackgroundColor;
    HexEditor.Colors.CurrentOffset := AParams.CurrentOffsetForegroundColor;
    HexEditor.Colors.EvenColumn := AParams.EvenColumnForegroundColor;
    HexEditor.Colors.OddColumn := AParams.OddColumnForegroundColor;
    HexEditor.Colors.ChangedBackground := AParams.ChangedBackgroundColor;
    HexEditor.Colors.ChangedText := AParams.ChangedForegroundColor;
    HexEditor.Font.Color := AParams.CharFieldForegroundColor;
  end;

  StatusBar.Visible := AParams.ShowStatusBar;
  FStatusbarItems := AParams.StatusbarItems;
  FStatusbarPosDisplay := AParams.StatusbarPosDisplay;
  FStatusbarSelDisplay := AParams.StatusbarSelDisplay;

  ShowNumViewer := AParams.NumViewerVisible;
  if Assigned(FNumViewer) then begin
    FNumViewer.Width := AParams.NumViewerWidth;
    FNumViewer.Height := AParams.NumViewerHeight;
    SetNumViewerPosition(AParams.NumViewerPosition);
    for i := 0 to High(AParams.NumViewerColWidths) do
      FNumViewer.ColWidths[i] := AParams.NumViewerColWidths[i];
  end;

  ShowRecordViewer := AParams.RecordViewerVisible;
  if Assigned(FRecordViewer) then begin
    SetRecordViewerPosition(AParams.RecordViewerPosition);
    if FRecordViewer.Align in [alLeft, alRight] then
      FRecordViewer.Width := AParams.RecordViewerWidth
    else if FRecordViewer.Align in [alBottom, alTop] then
      FRecordViewer.Height := AParams.RecordViewerHeight;
    for i := 0 to High(AParams.RecordViewerColWidths) do
      FRecordViewer.ColWidths[i] := AParams.RecordViewerColWidths[i];
  end;

  UpdateStatusbarPanelWidths;
end;

function THexEditorFrame.CanSaveFileAs(const AFileName: String): Boolean;
begin
  Result := not ((AFileName = GetFileName) and HexEditor.ReadOnlyFile);
end;

procedure THexEditorFrame.CreateHexEditor;
begin
  FHexEditor := TMPHexEditor.Create(self);
  FHexEditor.Parent := self;
  FHexEditor.Align := alClient;
  FHexEditor.BytesPerColumn := HexParams.BytesPerColumn;
  FHexEditor.BytesPerRow := HexParams.BytesPerRow;
  FHexEditor.DrawGutter3D := false;
  FHexEditor.Font.Size := 9;
  FHexEditor.GraySelectionIfNotFocused := true;
  FHexEditor.OffsetFormat := HexParams.GetOffsetFormat;
  FHexEditor.ReadOnlyView := true;
  FHexEditor.ReadOnlyFile := true;
  FHexEditor.RulerNumberBase := 10;
  FHexEditor.ShowRuler := HexParams.RulerVisible;
  FHexEditor.WantTabs := false;
  FHexEditor.OnChange := @HexEditorChanged;
  FHexEditor.OnSelectionChanged := @HexEditorChanged;

//  CommonData.BookmarkImages.GetFullBitmap(FHexEditor.BookmarkBitmap);
end;

procedure THexEditorFrame.CreateNumViewer;
begin
  FNumViewer.Free;
  FNumViewer := TNumViewerFrame.Create(self);
  FNumViewer.Name := '';
  FNumViewer.Parent := Self;
  FNumViewer.Width := HexParams.NumViewerWidth;
  FNumViewer.Align := alRight;
  FNumViewerSplitter := TSplitter.Create(self);
  FNumViewerSplitter.Parent := self;
  FNumViewerSplitter.Align := FNumViewer.Align;
end;

procedure THexEditorFrame.CreateRecordViewer;
begin
  FRecordViewer.Free;
  FRecordViewer := TRecordViewerFrame.Create(self);
  FRecordViewer.Name := '';
  FRecordViewer.Parent := Self;
  FRecordViewer.Width := HexParams.RecordViewerWidth;
  FRecordViewer.Align := alRight;
  FRecordViewerSplitter := TSplitter.Create(self);
  FRecordViewerSplitter.Parent := self;
  FRecordViewerSplitter.Align := FRecordViewer.Align;
end;

procedure THexEditorFrame.FixViewerSplitterPosition(AViewer: TControl;
  ASplitter: TSplitter);
begin
  case ASplitter.Align of
    alLeft:
      ASplitter.Left := AViewer.Left + AViewer.Width + 1;
    alRight:
      ASplitter.Left := AViewer.Left - 1;
    alBottom:
      ASplitter.top :=  AViewer.Top - 1;
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

function THexEditorFrame.GetNumViewerPosition: TViewerPosition;
begin
  if FNumViewer.Align = alLeft then
    Result := vpLeft
  else if FNumViewer.Align = alRight then
    Result := vpRight
  else
    Result := vpBottom;
end;

function THexEditorFrame.GetOffsetDisplayHexPrefix(AOffsetFormat: String): String;
var
  p1, p2: Integer;
begin
  p1 := Pos(':', AOffsetFormat);
  p2 := Pos('|', AOffsetFormat);
  Result := Copy(AOffsetFormat, p1+1, p2-p1-1);
end;

function THexEditorFrame.GetShowNumViewer: Boolean;
begin
  Result := (FNumViewer <> nil) and FNumViewer.Visible;
end;

function THexEditorFrame.GetRecordViewerPosition: TViewerPosition;
begin
  if FRecordViewer.Align = alLeft then
    Result := vpLeft
  else if FRecordViewer.Align = alRight then
    Result := vpRight
  else
    Result := vpBottom;
end;

function THexEditorFrame.GetShowRecordViewer: Boolean;
begin
  Result := (FRecordViewer <> nil) and FRecordViewer.Visible;
end;

procedure THexEditorFrame.HexEditorChanged(Sender: TObject);
begin
  UpdateStatusBar;
  if Assigned(FNumViewer) then
    FNumViewer.UpdateData(FHexEditor);
  if Assigned(FRecordViewer) then
    FRecordViewer.UpdateData(FHexEditor);
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure THexEditorFrame.InsertMode(const AEnable: Boolean);
begin
  if Assigned(FHexEditor) then begin
    FHexEditor.InsertMode := AEnable;
    UpdateStatusBar;
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

procedure THexEditorFrame.OpenFile(const AFileName: string; WriteProtected: boolean);
begin
  if Assigned(HexEditor) then begin
    HexEditor.LoadFromFile(AFileName);
    HexEditor.ReadOnlyFile := WriteProtected;
    UpdateCaption;
    UpdateStatusbarPanelWidths;
    (*
    EnableActions(true);
    AdjustWidth;
    ico := TIcon.Create;
    try
      idx := CommonData.SystemImages.GetImageIndex(AFileName, false, false, []);
      CommonData.SystemImages.GetIcon(idx, ico);
      Icon.Assign(ico);
    finally
      ico.Free;
    end;
    *)
  end;
end;

procedure THexEditorFrame.SaveFile;
begin
  if Assigned(HexEditor) then
    SaveFileAs(HexEditor.FileName);
end;

procedure THexEditorFrame.SaveFileAs(const AFileName: string);
begin
  if Assigned(HexEditor) then begin
    if not CanSaveFileAs(AFileName) then begin
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
  ApplyParams(HexParams);
  if HexEditor.CanFocus then HexEditor.SetFocus;
end;

procedure THexEditorFrame.SetRecordViewerPosition(AValue: TViewerPosition);
begin
  case FRecordViewer.Align of
    alLeft, alRight:
      HexParams.RecordViewerWidth := FRecordViewer.Width;
    alBottom, alTop:
      HexParams.RecordViewerHeight := FRecordViewer.Height;
  end;

  case AValue of
    vpLeft:
      begin
        FRecordViewer.Align := alLeft;
        FRecordViewer.Width := HexParams.RecordViewerWidth;
        FRecordViewer.Toolbar.Align := alTop;
      end;
    vpRight:
      begin
        FRecordViewer.Align := alRight;
        FRecordViewer.Width := HexParams.RecordViewerWidth;
        FRecordViewer.ToolBar.Align := alTop;
      end;
    vpBottom :
      begin
        FRecordViewer.Align := alBottom;
        FRecordViewer.Height := HexParams.RecordViewerHeight;
        FRecordViewer.ToolBar.Align := alLeft;
      end;
  end;
  FRecordViewer.ToolBar.AutoSize := true;
  FRecordViewerSplitter.Align := FRecordViewer.Align;
  FixViewerSplitterPosition(FRecordViewer, FRecordViewerSplitter);
  StatusBar.Top := Height * 2;
end;

procedure THexEditorFrame.SetShowRecordViewer(AValue: Boolean);
begin
  if AValue and (FRecordViewer = nil) then
    CreateRecordViewer;
  if FRecordViewer <> nil then begin
    FRecordViewer.Visible := AValue;
    FRecordViewerSplitter.Visible := AValue;
    FixViewerSplitterPosition(FRecordViewer, FRecordViewerSplitter);
    if AValue then
      FRecordViewer.UpdateData(HexEditor);
  end;
end;

procedure THexEditorFrame.SetShowNumViewer(AValue: Boolean);
begin
  if AValue and (FNumViewer = nil) then
    CreateNumViewer;
  if FNumViewer <> nil then begin
    FNumViewer.Visible := AValue;
    FNumViewerSplitter.Visible := AValue;
    FixViewerSplitterPosition(FNumViewer, FNumViewerSplitter);
    if AValue then
      FNumViewer.UpdateData(HexEditor);
  end;
end;

procedure THexEditorFrame.SetNumViewerPosition(AValue: TViewerPosition);
begin
  case FNumViewer.Align of
    alLeft, alRight:
      HexParams.NumViewerWidth := FNumViewer.Width;
    alBottom:
      HexParams.NumViewerHeight := FNumViewer.Height;
  end;

  case AValue of
    vpLeft:
      begin
        FNumViewer.Align := alLeft;
        FNumViewer.Width := HexParams.NumViewerWidth;
      end;
    vpRight:
      begin
        FNumViewer.Align := alRight;
        FNumViewer.Width := HexParams.NumViewerWidth;
      end;
    vpBottom :
      begin
        FNumViewer.Align := alBottom;
        FNumViewer.Height := HexParams.NumViewerHeight;
      end;
  end;
  FNumViewerSplitter.Align := FNumViewer.Align;
  FixViewerSplitterPosition(FNumViewer, FNumViewerSplitter);
  StatusBar.Top := Height * 2;
end;

procedure THexEditorFrame.UpdateCaption;
begin
  if Assigned(FHexEditor) then begin
    Caption := ExtractFilename(FHexEditor.FileName);
    if FHexEditor.Modified then
      Caption := '* ' + Caption //Format(SWriteProtectedCaption, [Caption]);
  end else
    Caption := SEmptyCaption;
end;

procedure THexEditorFrame.UpdateStatusbar;
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
  if Assigned(HexEditor) then begin
    hexprefix := GetOffsetDisplayHexPrefix(HexEditor.OffsetFormat);

    Statusbar.Panels[0].Text := IfThen(HexEditor.Modified, 'MOD', '');
    if HexEditor.ReadOnlyView then
      StatusBar.Panels[1].Text := 'R/O'
    else
      Statusbar.Panels[1].Text := IfThen(HexEditor.InsertMode, 'INS', 'OVW');

    p := 2;
    if sbPos in FStatusbarItems then
    begin
      case FStatusbarPosDisplay of
        odbDec: s := Format('%.0n', [1.0*HexEditor.GetCursorPos]);
        odbHex: s := Format('%s%x', [hexprefix, HexEditor.GetCursorPos]);
        odbOct: s := Format('&%s', [IntToOctal(HexEditor.GetCursorPos)]);
      end;
      s := Format(SMaskPos, [s]);
      Statusbar.Panels[p].Text := s;
      inc(p);
    end;

    if sbSel in FStatusbarItems then begin
      if HexEditor.SelCount <> 0 then begin
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
      StatusBar.Panels[p].Text := s;
      inc(p);
    end;

    if sbSize in FStatusbarItems then begin
      s := Format('%.0n', [1.0 * HexEditor.DataSize]);
      StatusBar.Panels[p].Text := Format(SMaskSize, [s]);
      inc(p);
    end;

    while p < Statusbar.Panels.Count do begin
      StatusBar.Panels[p].Text := '';
      inc(p);
    end;
  end;
end;

procedure THexEditorFrame.UpdateStatusbarPanelWidths;
var
  p, n: integer;
  s: String;
  hexPrefix: String;
begin
  Statusbar.Canvas.Font.Assign(Statusbar.Font);
  hexprefix := GetOffsetDisplayHexPrefix(HexEditor.OffsetFormat);

  p := 2;
  if sbPos in FStatusbarItems then
  begin
    if Assigned(HexEditor) then begin
      case FStatusbarPosDisplay of
        odbDec: s := Format('%.0n', [1.0*HexEditor.DataSize]);
        odbHex: s := Format('%s%x', [hexprefix, HexEditor.DataSize]);
        odbOct: s := Format('&%s', [IntToOctal(HexEditor.DataSize)]);
      end;
      s := Format(SMaskPos, [s]);
      Statusbar.Panels[p].Width := Statusbar.Canvas.TextWidth(s) + 10;
    end else
      Statusbar.Panels[p].Width := 120;
    inc(p);
  end;

  if sbSel in FStatusbarItems then begin
    if Assigned(FHexEditor) then begin
      n := HexEditor.DataSize;
      case FStatusbarSelDisplay of
        odbDec: s := Format('%.0n ... %.0n (%.0n)', [1.0*n, 1.0*n, 1.0*n]);
        odbHex: s := Format('%0:s%1:x ... %0:s%2:x (%0:s%3:x)', [hexPrefix, n, n, n]);
        odbOct: s := Format('&%s ... &%s (&%s)', [IntToOctal(n), IntToOctal(n), IntToOctal(n)]);
      end;
      Statusbar.Panels[p].Width := Statusbar.Canvas.TextWidth(s) + 10;
    end else
      Statusbar.Panels[p].Width := 250;
    inc(p);
  end;

  if sbSize in FStatusbarItems then begin
    if Assigned(HexEditor) then begin
      s := Format('%.0n', [1.0 * HexEditor.DataSize]);
      s := Format(SMaskSize, [s]);
      StatusBar.Panels[p].Width := StatusBar.Canvas.TextWidth(s) + 10;
    end else
      Statusbar.Panels[p].Width := 150;
  end;
end;


end.
