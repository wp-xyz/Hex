unit hxMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  StdCtrls, ComCtrls, mrumanager,
  hxGlobal, hxHexEditor, hxHexEditorFrame;

type

  { TMainForm }

  TMainForm = class(TForm)
    acFileNew: TAction;
    acFileOpen: TAction;
    acFileQuit: TAction;
    acShowStatusbar: TAction;
    acShowToolbar: TAction;
    acShowStdViewer: TAction;
    acCfgSettings: TAction;
    acFileClose: TAction;
    acFileCloseAll: TAction;
    acFileSave: TAction;
    acFileSaveAs: TAction;
    acSetBookmark0: TAction;
    acSetBookmark1: TAction;
    acSetBookmark2: TAction;
    acSetBookmark3: TAction;
    acSetBookmark4: TAction;
    acSetBookmark5: TAction;
    acSetBookmark6: TAction;
    acSetBookmark7: TAction;
    acSetBookmark8: TAction;
    acSetBookmark9: TAction;
    acGotoBookmark0: TAction;
    acGotoBookmark9: TAction;
    acGotoBookmark1: TAction;
    acGotoBookmark2: TAction;
    acGotoBookmark3: TAction;
    acGotoBookmark4: TAction;
    acGotoBookmark5: TAction;
    acGotoBookmark6: TAction;
    acGotoBookmark7: TAction;
    acGotoBookmark8: TAction;
    acClearBookmark0: TAction;
    acClearBookmark9: TAction;
    acClearBookmark1: TAction;
    acClearBookmark2: TAction;
    acClearBookmark3: TAction;
    acClearBookmark4: TAction;
    acClearBookmark5: TAction;
    acClearBookmark6: TAction;
    acClearBookmark7: TAction;
    acClearBookmark8: TAction;
    acGoTo: TAction;
    acEditInsertMode: TAction;
    acEditOverWriteMode: TAction;
    acEditEditingForbidden: TAction;
    acEditEditingAllowed: TAction;
    acGoToRepeat: TAction;
    acGotoBackward: TAction;
    acAbout: TAction;
    acEditFind: TAction;
    acEditReplace: TAction;
    acEditUndo: TAction;
    acEditRedo: TAction;
    acEditSelectAll: TAction;
    acEditCut: TAction;
    acEditCopy: TAction;
    acEditPaste: TAction;
    acObjectsSelect: TAction;
    acObjectsExport: TAction;
    acObjectsFindNext: TAction;
    acViewOffsetHex: TAction;
    acViewOffsetDec: TAction;
    acViewOffsetOctal: TAction;
    ActionList: TActionList;
    CoolBar1: TCoolBar;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    N1: TMenuItem;
    mnuObjectsExport: TMenuItem;
    mnuObjectsSelect: TMenuItem;
    mnuObjects: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    mnuEditRepalce: TMenuItem;
    mnuEditInsert: TMenuItem;
    mnuEditModeSeparator: TMenuItem;
    mnuEditOverwrite: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    mnuEditFind: TMenuItem;
    MenuItem4: TMenuItem;
    mnuEditMode: TMenuItem;
    mnuEditingAllowed: TMenuItem;
    mnuEditingForbidden: TMenuItem;
    mnuEdit: TMenuItem;
    mnuGoto: TMenuItem;
    mnuClearBookmark5: TMenuItem;
    mnuClearBookmark6: TMenuItem;
    mnuClearBookmark7: TMenuItem;
    mnuClearBookmark8: TMenuItem;
    mnuClearBookmark9: TMenuItem;
    mnuClearBookmark0: TMenuItem;
    mnuClearBookmark1: TMenuItem;
    mnuClearBookmark2: TMenuItem;
    mnuClearBookmark3: TMenuItem;
    mnuClearBookmark4: TMenuItem;
    mnuSetBookmark5: TMenuItem;
    mnuSetBookmark6: TMenuItem;
    mnuSetBookmark7: TMenuItem;
    mnuSetBookmark8: TMenuItem;
    mnuSetBookmark9: TMenuItem;
    mnuSetBookmark0: TMenuItem;
    mnuGotoBookmark1: TMenuItem;
    mnuGotoBookmark2: TMenuItem;
    mnuGotoBookmark3: TMenuItem;
    mnuGotoBookmark4: TMenuItem;
    MenuItem2: TMenuItem;
    mnuGotoBookmark5: TMenuItem;
    mnuGotoBookmark6: TMenuItem;
    mnuGotoBookmark7: TMenuItem;
    mnuGotoBookmark8: TMenuItem;
    mnuGotoBookmark9: TMenuItem;
    mnuGotoBookmark0: TMenuItem;
    MenuItem3: TMenuItem;
    mnuSetBookmark1: TMenuItem;
    mnuSetBookmark2: TMenuItem;
    mnuSetBookmark3: TMenuItem;
    mnuSetBookmark4: TMenuItem;
    mnuClearBookmarks: TMenuItem;
    mnuGotoBookmarks: TMenuItem;
    mnuSetBookmarks: TMenuItem;
    mnuNavigation: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuFileSave: TMenuItem;
    MenuItem5: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuFileCloseAll: TMenuItem;
    MenuItem9: TMenuItem;
    mnuGlobalSettings: TMenuItem;
    mnuConfig: TMenuItem;
    mnuView: TMenuItem;
    mnuRecentlyOpened: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileQuit: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    RecentFilesPopup: TPopupMenu;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    tbFileOpen: TToolButton;
    tbFileQuit: TToolButton;
    tbSettings: TToolButton;
    tbDivider1: TToolButton;
    tbSave: TToolButton;
    tbDivider2: TToolButton;
    tbFind: TToolButton;
    tbDivider3: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure acAboutExecute(Sender: TObject);
    procedure acBookmarkClear(Sender: TObject);
    procedure acBookmarkGoto(Sender: TObject);
    procedure acBookmarkSet(Sender: TObject);
    procedure acCfgSettingsExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditFindExecute(Sender: TObject);
    procedure acEditMode(Sender: TObject);
    procedure acEditInsertOverwriteModeExecute(Sender: TObject);
    procedure acEditEditingForbiddenExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditRedoExecute(Sender: TObject);
    procedure acEditReplaceExecute(Sender: TObject);
    procedure acEditSelectAllExecute(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure acFileCloseAllExecute(Sender: TObject);
    procedure acFileCloseExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileQuitExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acGoToExecute(Sender: TObject);
    procedure acGoToRepeatExecute(Sender: TObject);
    procedure acObjectsExportExecute(Sender: TObject);
    procedure acObjectsFindNextExecute(Sender: TObject);
    procedure acObjectsSelectExecute(Sender: TObject);
    procedure acShowStatusbarExecute(Sender: TObject);
    procedure acShowToolbarExecute(Sender: TObject);
    procedure ActionListUpdate({%H-}AAction: TBasicAction; var {%H-}Handled: Boolean);
    procedure acViewOffsetFormatHandler(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: Array of String);
    procedure FormShow(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure StatusBarHint(Sender: TObject);
  private
    FCurrentHexEditor: THxHexEditor;
    FRecentFilesManager: TMRUMenuManager;

    procedure AppendToObjectsMenu(AParentMenu: TMenuItem);
    procedure ApplyParams(const AParams: THexParams);
    procedure EvalCmdLine;
    procedure FindObjectHandler(Sender: TObject);
    function GetActiveHexEditorFrame: THexEditorFrame;
    function GetHexEditorFrame(APageIndex: Integer): THexEditorFrame;
    procedure HexEditorChanged(Sender: TObject);
    procedure HexEditorUpdateStatusBar(Sender: TObject);

    procedure RecentFileHandler(Sender: TObject; const AFileName: String);
    procedure ShowStatusbar(AEnable: boolean);
    procedure ShowNumViewer(AEnable: Boolean);
    procedure ShowToolbar(AEnable: boolean);
    procedure UpdateCaption;
    procedure UpdateCmds;
    procedure UpdateCurrentHexEditor;
    procedure UpdateIconSet;

    procedure ReadIni;
    procedure WriteIni;

  public
    procedure CreateEditor(const AFileName: String; WriteProtected: Boolean);

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IniFiles, Math,
  MPHexEditor,
  hxStrings, hxUtils, {%H-}hxDataModule, hxObjectViewerFrame,
  hxSettingsDlg, hxGotoDlg, hxAbout;

const
  PROG_NAME = 'Hex';


{ TMainForm }

procedure TMainForm.acAboutExecute(Sender: TObject);
var
  F: TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.acBookmarkClear(Sender: TObject);
var
  i, idx: integer;
  ac: TAction;
  bm: TMPHBookmark;
begin
  inherited;
  if Assigned(FCurrentHexEditor) then
  begin
    bm.MPosition := -1;
    idx := (Sender as TAction).Tag - TAG_CLEAR_BOOKMARK;
    FCurrentHexEditor.Bookmark[idx] := bm;
    for i := 0 to ActionList.ActionCount - 1 do begin
      ac := TAction(ActionList[i]);
      if ac.Tag - TAG_SET_BOOKMARK = idx then
        ac.Checked := false;
      if ac.Tag - TAG_GOTO_BOOKMARK = idx then
        ac.Enabled := false;
      if ac.Tag - TAG_CLEAR_BOOKMARK = idx then
        ac.Enabled := false;
    end;
  end;
end;

procedure TMainForm.acBookmarkGoto(Sender:TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.GotoBookmark((Sender as TAction).Tag - TAG_GOTO_BOOKMARK);
end;

procedure TMainForm.acBookmarkSet(Sender: TObject);
var
  i : integer;
  ac : TAction;
  bm: TMPHBookmark;
  idx: Integer;
begin
  if Assigned(FCurrentHexEditor) then
  begin
    idx := (Sender as TAction).Tag - TAG_SET_BOOKMARK;
    bm.mPosition := FCurrentHexEditor.GetCursorPos; //SelStart;
    bm.mInCharField := FCurrentHexEditor.InCharField;
    FCurrentHexEditor.Bookmark[idx] := bm;
    (Sender as TAction).Checked := true;
    for i := 0 to ActionList.ActionCount - 1 do begin
      ac := TAction(ActionList[i]);
      if ac.Tag - TAG_SET_BOOKMARK = idx then
        ac.Checked := true;
      if ac.Tag - TAG_GOTO_BOOKMARK = idx then
        ac.Enabled := true;
      if ac.Tag - TAG_CLEAR_BOOKMARK = idx then
        ac.Enabled := true;
    end;
  end;
end;

procedure TMainForm.acCfgSettingsExecute(Sender: TObject);
var
  D: TSettingsForm;
  F: THexEditorFrame;
  params: THexParams;
  colors: TColorParams;
begin
  F := GetActiveHexEditorFrame;
  D := TSettingsForm.Create(nil);
  try
    params := HexParams;
    colors := ColorParams;
    if Assigned(F) then begin
      F.ActiveHexParams(params{%H-});
      F.ActiveColors(colors{%H-});
    end;

    D.ParamsToControls(params);
    D.ColorsToControls(colors);
    D.GuiParamsToControls(GUIParams);

    if D.ShowModal = mrOK then
    begin
      D.ParamsFromControls(params);
      D.ColorsFromControls(colors);
      HexParams.SettingsPageIndex := params.SettingsPageIndex;
      if Assigned(F) then begin
        F.ApplyHexParams(params);
        ApplyColorsToHexEditor(colors, F.HexEditor);
        F.UpdateIconSet;
      end else begin
        HexParams := params;
        ColorParams := colors;
      end;
      D.GuiParamsFromControls(GUIParams);
      UpdateIconSet;
    end;
  finally
    D.Free;
  end;
end;

procedure TMainForm.acEditCopyExecute(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.CBCopy;
end;

procedure TMainForm.acEditCutExecute(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.CBCut;
end;

procedure TMainForm.acEditInsertOverwriteModeExecute(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    F.InsertMode(acEditInsertMode.Checked or not acEditOverwriteMode.Checked);
end;

procedure TMainForm.acEditMode(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    F.WriteProtected := acEditEditingForbidden.Checked
    {
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.ReadOnlyView := acEditEditingForbidden.Checked
    }
  else
    HexParams.ViewOnly := acEditEditingForbidden.Checked;
end;

procedure TMainForm.acEditPasteExecute(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.CBPaste;
end;

procedure TMainForm.acEditRedoExecute(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.Redo;
end;

procedure TMainForm.acEditReplaceExecute(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) and Assigned(F.HexEditor) then
    F.ReplaceDlg;
end;

procedure TMainForm.acEditSelectAllExecute(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.SelectAll;
end;

procedure TMainForm.acEditUndoExecute(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.Undo;
end;

procedure TMainForm.acEditEditingForbiddenExecute(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    FCurrentHexEditor.ReadOnlyView := acEditEditingForbidden.Checked;
end;

procedure TMainForm.acEditFindExecute(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) and Assigned(F.HexEditor) then
    F.FindDlg;
end;

procedure TMainForm.acFileCloseAllExecute(Sender: TObject);
var
  i: Integer;
  F: THexEditorFrame;
  res: Integer;
begin
  for i := PageControl.PageCount-1 downto 0 do
  begin
    F := GetHexEditorFrame(i);
    if F.HexEditor.Modified then
    begin
      PageControl.ActivePageIndex := i;
      PageControlChange(nil);
      res := MessageDlg(SCloseModified, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      case res of
        mrYes:
          with SaveDialog do
          begin
            InitialDir := ExtractFileDir(F.Filename);
            FileName := ExtractFileName(F.Filename);
            DefaultExt := '';
            Filter := 'All files (*.*)|*.*';
            if Execute then
            begin
              Application.ProcessMessages;
              F.SaveFileAs(FileName);
            end else
              Continue;
          end;
        mrNo:
          Continue;
        mrCancel:
          Break;
      end;
    end;
    F.Free;
    PageControl.Pages[i].Free;
  end;

  if PageControl.PageCount = 0 then
    PageControl.Hide;

  UpdateCurrentHexEditor;
  UpdateCmds;
  UpdateCaption;
end;

procedure TMainForm.acFileCloseExecute(Sender: TObject);
var
  idx: Integer;
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then begin
    if F.HexEditor.Modified then begin
      if MessageDlg(SCloseModified, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        exit;
      with SaveDialog do
      begin
        InitialDir := ExtractFileDir(F.Filename);
        FileName := ExtractFileName(F.Filename);
        DefaultExt := '';
        Filter := 'All files (*.*)|*.*';
        if Execute then
        begin
          Application.ProcessMessages;
          F.SaveFileAs(FileName);
        end else
          exit;
      end;
    end;
    F.Free;
    idx := PageControl.ActivePageIndex;
    PageControl.ActivePage.Free;
    if idx < PageControl.PageCount then
      PageControl.ActivePageIndex := idx
    else
    if PageControl.PageCount > 0 then
      PageControl.ActivePageIndex := PageControl.PageCount - 1;
    if PageControl.PageCount > 0 then
      PageControlChange(nil)
    else
      PageControl.Hide;
    UpdateCurrentHexEditor;
    UpdateCmds;
    UpdateCaption;
  end;
end;

procedure TMainForm.acFileNewExecute(Sender: TObject);
begin
  CreateEditor('', false);
end;

procedure TMainForm.acFileOpenExecute(Sender: TObject);
var
  i : integer;
begin
  with OpenDialog do begin
    FileName := '*.*';
    Filter := 'All files (*.*)|*.*';
    DefaultExt := '';
    if Execute then
    begin
      Application.ProcessMessages;
      for i:=0 to Files.Count-1 do
        CreateEditor(Files[i], ofReadOnly in Options);
    end;
  end;
end;

procedure TMainForm.acFileQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acFileSaveAsExecute(Sender: TObject);
var
  F: THexEditorFrame;
begin
  inherited;
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
  begin
    with SaveDialog do
    begin
      InitialDir := ExtractFileDir(F.Filename);
      FileName := ExtractFileName(F.Filename);
      DefaultExt := '';
      Filter := 'All files (*.*)|*.*';
      if Execute then
      begin
        Application.ProcessMessages;
        F.SaveFileAs(FileName);
        HexEditorChanged(self);  // Remove "modified" tag from PageControl tab
      end;
    end;
  end;
end;

procedure TMainForm.acFileSaveExecute(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    F.SaveFile;
end;

procedure TMainForm.acGoToExecute(Sender: TObject);
var
  F: THexEditorFrame;
  dlg: TGotoForm;
  n: Integer;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
  begin
    if (F.HexEditor = nil) or (not F.HexEditor.HasFile) then
      exit;

    dlg := TGotoForm.Create(nil);
    try
      if dlg.ShowModal = mrOK then
      begin
        if GotoParams.JumpAbs then
          n := GotoParams.PosAbs
        else
          n := F.HexEditor.GetCursorPos + GotoParams.PosRel;
        F.JumpToPosition(n);
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TMainForm.acGoToRepeatExecute(Sender: TObject);
var
  F: THexEditorFrame;
  n: integer;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) and Assigned(F.HexEditor) then
  begin
    if Sender = acGotoRepeat then      // jump forward
    begin
      if GotoParams.JumpAbs then
        n := GotoParams.PosAbs
      else
        n := F.HexEditor.GetCursorPos + GotoParams.PosRel
    end
    else
    if Sender = acGotoBackward then    // jump backward
    begin
      if GotoParams.JumpAbs then
        exit;
      n := F.HexEditor.GetCursorPos - GotoParams.PosRel
    end
    else
      exit;

    F.JumpToPosition(n);
  end;
end;

procedure TMainForm.acObjectsExportExecute(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    F.ExportObject;
end;

procedure TMainForm.acObjectsFindNextExecute(Sender: TObject);
begin
  FindObjectHandler(acObjectsFindNext);
end;

procedure TMainForm.acObjectsSelectExecute(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    F.SelectObject;
end;

procedure TMainForm.acShowStatusbarExecute(Sender: TObject);
begin
  ShowStatusbar(acShowStatusbar.Checked);
end;

procedure TMainForm.acShowToolbarExecute(Sender: TObject);
begin
  ShowToolbar(acShowToolbar.Checked);
end;

procedure TMainForm.ActionListUpdate(AAction: TBasicAction; var Handled: Boolean
  );
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    HexEditorUpdateStatusBar(F);
  UpdateCmds;
end;

procedure TMainForm.acViewOffsetFormatHandler(Sender: TObject);
begin
  if Assigned(FCurrentHexEditor) then
    with FCurrentHexEditor do
    begin
      RulerNumberBase := (Sender as TAction).Tag;
      FCurrentHexEditor.OffsetFormat := HexParams.GetOffsetFormat(RulerNumberBase);
    end;
end;

{ Attaches the "Search ..." entries to the "Objects" menu, after the item
  identified by TAG_OBJECTS }
procedure TMainForm.AppendToObjectsMenu(AParentMenu: TMenuItem);

  function CreateExtractorAction(AIndex: Integer; AFileExt: string): TAction;
  begin
    Result := TAction.Create(Self);
    with Result do begin
      ActionList := Self.ActionList;
      Caption := Format('Find %s', [AFileExt]);
      //Enabled := false;
      Tag := TAG_FIND_OBJECTS + AIndex + 1;
      Hint := Format('Find embedded object (%s)', [AFileExt]);
      OnExecute := @FindObjectHandler;
    end;
  end;

  function FindPrevItem: integer;
  var
    i : integer;
  begin
    Result := -1;
    for i := 0 to AParentMenu.Count - 1 do
    begin
      if AParentMenu[i].Tag = TAG_FIND_OBJECTS then
      begin
        Result := i + 1;
        exit;
      end;
    end;
  end;

var
  i, i0: integer;
  item: TMenuItem;
begin
  i0 := 0;

  for i := 0 to NumExtractors - 1 do
  begin
    if not RegisteredExtractorSignature(i) then
      Continue;
    item := TMenuItem.Create(self);
    with item do
    begin
      Action := CreateExtractorAction(i, RegisteredExtension(i));
    end;
    AParentMenu.Insert(i0, item);
    inc(i0);
  end;
end;

procedure TMainForm.ApplyParams(const AParams: THexParams);
var
  i: Integer;
  F: THexEditorFrame;
begin
  for i := 0 to PageControl.PageCount-1 do
  begin
    F := GetHexEditorFrame(i);
    F.ApplyHexParams(AParams);
  end;
end;

procedure TMainForm.CreateEditor(const AFileName: String; WriteProtected: Boolean);

  function CountEmpty: integer;
  var
    i, n: integer;
    ef: THexEditorFrame;
  begin
    n := 0;
    for i:=0 to PageControl.PageCount-1 do
    begin
      ef := GetHexEditorFrame(i);
      if ef.HexEditor.HasFile then
        inc(n);
    end;
    Result := n;
  end;

var
  F: THexEditorFrame;
  c: TCursor;
  page: TTabSheet;
begin
  if not FileExists(AFileName) then
  begin
    MessageDlg(Format('File "%s" not found.', [AFileName]), mtError, [mbOK], 0);
    exit;
  end;

  c := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    page := PageControl.AddTabSheet;
    F := THexEditorFrame.Create(self);
    F.Name := '';
    F.Parent := page;
    F.Align := alClient;
    if AFileName <> '' then
    begin
      F.OpenFile(AFileName, WriteProtected);
      F.WriteProtected := not acEditEditingAllowed.Checked;
      FRecentFilesManager.AddToRecent(AFileName);
    end else
      F.Caption := Format(SNoName, [CountEmpty]);
    F.UpdateStatusbarPanelWidths(StatusBar);
    F.OnChange := @HexEditorChanged;
    F.OnUpdateStatusBar := @HexEditorUpdateStatusBar;
    F.ApplyHexParams(HexParams);
    ApplyColorsToHexEditor(ColorParams, F.HexEditor);
    page.Caption := F.Caption;
    PageControl.ActivePage := page;
    PageControl.Show;
    FCurrentHexEditor := F.HexEditor;
    UpdateCaption;
    UpdateCmds;
  finally
    Screen.Cursor := c;
  end;
end;

procedure TMainForm.EvalCmdLine;
var
  s: string;
  i: integer;
begin
  for i:=1 to ParamCount do
  begin
    s := UpperCase(ParamStr(i));
    if (s[1]='/') or (s[1]='-') then
    begin
      //
    end else
      if FileExists(ParamStr(i)) then CreateEditor(ParamStr(i), HexParams.WriteProtected);
  end;
end;

procedure TMainForm.FindObjectHandler(Sender: TObject);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
  begin
    F.FindObjectHandler(Sender);
    UpdateCmds;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := true;
  try
    WriteIni;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cInputQueryEditSizePercents := 0; // Reduce width of InputQuery dialogs

  FRecentFilesManager := TMRUMenuManager.Create(self);
  FRecentFilesManager.MenuCaptionMask := '%0:x - %s';
  FRecentFilesManager.MenuItem := mnuRecentlyOpened;
  FRecentFilesManager.PopupMenu := RecentFilesPopup;
  FRecentFilesManager.OnRecentFile := @RecentFileHandler;
  FRecentFilesManager.IniFileName := GetIniFileName;
  FRecentFilesManager.IniSection := 'RecentFiles';

  AppendToObjectsMenu(mnuObjects);

  ReadIni;
  UpdateCmds;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: Array of String);
var
  fn: String;
begin
  for fn in FileNames do
     CreateEditor(fn, HexParams.WriteProtected);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  EvalCmdLine;
end;

function TMainForm.GetActiveHexEditorFrame: THexEditorFrame;
begin
  if PageControl.ActivePage = nil then
    Result := nil
  else
    Result := GetHexEditorFrame(PageControl.ActivePageIndex);
end;

function TMainForm.GetHexEditorFrame(APageIndex: Integer): THexEditorFrame;
begin
  if PageControl.Pages[APageIndex].Controls[0] is THexEditorFrame then
    Result := THexEditorFrame(PageControl.Pages[APageIndex].Controls[0])
  else
    Result := nil;
end;

procedure TMainForm.HexEditorChanged(Sender: TObject);
var
  F: THexEditorFrame;
  i: Integer;
begin
  F := GetActiveHexEditorFrame;
  F.UpdateCaption;
  for i:=0 to PageControl.PageCount-1 do
    if GetHexEditorFrame(i) = F then
    begin
      PageControl.Pages[i].Caption := F.Caption;
      break;
    end;
end;

procedure TMainForm.HexEditorUpdateStatusBar(Sender: TObject);
begin
  (Sender as THexEditorFrame).UpdateStatusBar(StatusBar);
end;

procedure TMainForm.PageControlChange(Sender: TObject);
var
  F: THexEditorFrame;
begin
  UpdateCurrentHexEditor;
  UpdateCaption;
  F := GetActiveHexEditorFrame;
  if Assigned(F) then begin
    F.UpdateStatusBarPanelWidths(StatusBar);
  end;
end;

procedure TMainForm.ReadIni;
var
  ini : TCustomIniFile;
begin
  ini := CreateIniFile;
  try
    ReadFormFromIni(ini, self, INI_MAINFORM);
    ShowToolbar(ini.ReadBool(INI_MAINFORM, 'ShowToolbar', acShowToolbar.Checked));
    ShowStatusbar(ini.ReadBool(INI_MAINFORM, 'ShowStatusbar', AcShowStatusbar.Checked));

    ReadParamsFromIni(ini, INI_PARAMS);
    if HexParams.ViewOnly then
      AcEditEditingForbidden.Checked := true
    else
      AcEditEditingAllowed.Checked := true;
    if HexParams.InsertMode then
      AcEditInsertMode.Checked := true
    else
      AcEditOverwriteMode.Checked := true;

    ReadColorsFromIni(ini, INI_COLORS);

    ReadGuiParamsFromIni(ini, INI_GUI);
    UpdateIconSet;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.RecentFileHandler(Sender: TObject;
  const AFileName: String);
begin
  CreateEditor(AFileName, HexParams.WriteProtected);
end;

procedure TMainForm.ShowNumViewer(AEnable: Boolean);
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    F.ShowDataViewer := AEnable
  else
    HexParams.DataViewerVisible := AEnable;
end;

procedure TMainForm.ShowStatusbar(AEnable: boolean);
begin
  StatusBar.Visible := AEnable;
  AcShowstatusbar.Checked := AEnable;
  ApplyParams(HexParams);
end;

procedure TMainForm.ShowToolbar(AEnable: boolean);
begin
  Toolbar.Visible := AEnable;
  acShowToolbar.Checked := AEnable;
end;

procedure TMainForm.StatusBarHint(Sender: TObject);
var
  txt: String;
begin
  txt := GetLongHint(Application.Hint);
  with StatusBar do
  begin
    Panels[Panels.Count-1].Text := txt;
    Refresh;
  end;
end;

procedure TMainForm.UpdateCaption;
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    Caption := Format('%s - %s', [PROG_NAME, F.Caption])
  else
    Caption := PROG_NAME;
end;

procedure TMainForm.UpdateCmds;
var
  F: THexEditorFrame;
  i: Integer;
  ac: TAction;
  hasData: Boolean;
  atObj: Boolean;
  hasEx: Boolean;
begin
  F := GetActiveHexEditorFrame;
  hasData := Assigned(F) and Assigned(F.HexEditor) and (F.HexEditor.DataSize > 0);
  atObj := hasData and Assigned(F.Objectviewer) and F.ObjectViewer.AtObject;
  hasEx := hasData and Assigned(F.ObjectViewer) and (F.ExtractorIndex >= 0);

  acFileClose.Enabled := hasData;
  acFileCloseAll.Enabled := hasData;
  acFileSave.Enabled := hasdata;
  acFileSaveAs.Enabled := hasData;
  acEditUndo.Enabled := hasData and F.HexEditor.CanUndo;
  acEditRedo.Enabled := hasData and F.HexEditor.CanRedo;
  acEditCut.Enabled := hasData and F.HexEditor.CanCut;
  acEditCopy.Enabled := hasData and F.HexEditor.CanCopy;
  acEditPaste.Enabled := hasData and F.HexEditor.CanPaste;
  acEditFind.Enabled := hasData;
  acEditReplace.Enabled := hasData;
  acEditSelectAll.Enabled := hasData;

  if (F = nil) then
  begin
    if (not HexParams.ViewOnly) and HexParams.AllowInsertMode then
      acEditInsertMode.Enabled := true
    else
      acEditOverwriteMode.Enabled := true;
  end
  else if Assigned(F.HexEditor) then
  begin
    if F.HexEditor.InsertMode then
      acEditInsertMode.Checked := true
    else
      acEditOverwriteMode.Checked := true;
    if (not F.HexEditor.ReadOnlyView) and F.HexEditor.AllowInsertMode then
      acEditInsertMode.Enabled := true
    else
      acEditOverwriteMode.Enabled := true;
  end;
  acEditOverWriteMode.Enabled := acEditInsertMode.Enabled;

  for i := 0 to ActionList.ActionCount-1 do
  begin
    ac := TAction(ActionList[i]);
    if InRange(ac.tag, TAG_SET_BOOKMARK, TAG_SET_BOOKMARK + 10) then
      ac.Enabled := hasData;
    if InRange(ac.Tag, TAG_GOTO_BOOKMARK, TAG_GOTO_BOOKMARK + 10) then
      ac.Enabled := hasData and (F.HexEditor.Bookmark[ac.Tag - TAG_GOTO_BOOKMARK].mPosition <> -1);
    if InRange(ac.Tag, TAG_CLEAR_BOOKMARK, TAG_CLEAR_BOOKMARK + 10) then
      ac.Enabled := hasData and (F.HexEditor.Bookmark[ac.Tag - TAG_CLEAR_BOOKMARK].mPosition <> -1);

    if InRange(ac.Tag, TAG_FIND_OBJECTS, TAG_FIND_OBJECTS + NumExtractors) then
      ac.Enabled := hasData;
  end;

  acObjectsFindNext.Enabled := hasEx;
  acObjectsSelect.Enabled := atObj;
  acObjectsExport.Enabled := atObj;
end;

procedure TMainForm.UpdateCurrentHexEditor;
var
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if Assigned(F) then
    FCurrentHexEditor := F.HexEditor
  else begin
    FCurrentHexEditor := nil;
    while StatusBar.Panels.Count > 1 do
      StatusBar.Panels.Delete(0);
  end;
end;

procedure TMainForm.UpdateIconSet;
var
  i: Integer;
  F: THexEditorFrame;
begin
  if CommonData = nil then
    exit;

  ActionList.Images := CommonData.Images;
  MainMenu.Images := CommonData.Images;
  Toolbar.Images := CommonData.Images;

  for i := 0 to PageControl.PageCount-1 do begin
    F := GetHexEditorFrame(i);
    F.UpdateIconSet;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini : TCustomIniFile;
  F: THexEditorFrame;
begin
  F := GetActiveHexEditorFrame;
  if F <> nil then begin
    F.ActiveHexParams(HexParams);
    F.ActiveColors(ColorParams);
  end;

  ini := CreateIniFile;
  try
    WriteFormToIni(ini, self, INI_MAINFORM);
    ini.WriteBool(INI_MAINFORM, 'ShowToolbar', acShowToolbar.Checked);
    ini.WriteBool(INI_MAINFORM, 'ShowStatusbar', acShowStatusbar.Checked);
    WriteParamsToIni(ini, INI_PARAMS);
    WriteColorsToIni(ini, INI_COLORS);
    WriteGuiParamsToIni(ini, INI_GUI);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

end.

