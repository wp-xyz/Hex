unit hxGotoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TGotoForm }

  TGotoForm = class(TForm)
    Bevel1: TBevel;
    ButtonPanel: TButtonPanel;
    edAbs: TEdit;
    edRel: TEdit;
    lblRelPrefix: TLabel;
    lblAbsPos: TLabel;
    lblAbsPrefix: TLabel;
    lblRelPos: TLabel;
    PageControl: TPageControl;
    pgAbsolute: TTabSheet;
    pgRelative: TTabSheet;
    rbForward: TRadioButton;
    rbBackward: TRadioButton;
    procedure edRelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    function PageValid: Boolean;
    procedure SetPosAbs(AValue: Integer);
    procedure SetPosRel(AValue: Integer);

  public

  end;

var
  GotoForm: TGotoForm;

implementation

{$R *.lfm}

uses
  hxStrings, hxGlobal, hxDataModule;

{ TGotoForm }

procedure TGotoForm.edRelChange(Sender: TObject);
var
  s: string;
begin
  if edRel.Text <> '' then
  begin
    s := Trim(edRel.Text[1]);
    rbBackward.Checked := s[1] = '-';
  end;
end;

procedure TGotoForm.FormCreate(Sender: TObject);
begin
  with CommonData.Images do
  begin
    GetBitmap(IMG_INDEX_OK, ButtonPanel.OKButton.Glyph);          // OK icon
    GetBitmap(IMG_INDEX_CANCEL, ButtonPanel.CancelButton.Glyph);  // Cancel icon
  end;

  SetPosAbs(GotoParams.PosAbs);
  SetPosRel(GotoParams.PosRel);
  if GotoParams.JumpAbs then
  begin
    PageControl.ActivePage := PgAbsolute;
    ActiveControl := EdAbs;
  end else
  begin
    PageControl.ActivePage := PgRelative;
    ActiveControl := EdRel;
  end;
end;

procedure TGotoForm.OKButtonClick(Sender: TObject);
begin
  if PageValid then
  begin
    with GotoParams do
    begin
      PosAbs := StrToInt(edAbs.Text);
      PosRel := StrToInt(edRel.Text);
      if rbBackward.Checked then
        PosRel := -abs(PosRel);
      JumpAbs := (PageControl.ActivePage = pgAbsolute);
    end;
  end
  else
    ModalResult := mrNone;
end;

procedure TGotoForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := PageValid;
end;

function TGotoForm.PageValid: boolean;
var
  n: Integer;
begin
  Result := false;
  if PageControl.ActivePage = pgAbsolute then
  begin
    if not TryStrToInt(edAbs.Text, n) then
    begin
      MessageDlg(Format(SNoValidNumber, [edAbs.Text]), mtError, [mbOK], 0);
      edAbs.SetFocus;
      exit;
    end;
  end
  else
  if PageControl.ActivePage = pgRelative then
  begin
    if not TryStrToInt(edRel.Text, n) then
    begin
      MessageDlg(Format(SNoValidNumber, [edRel.Text]), mtError, [mbOK], 0);
      edRel.SetFocus;
      exit;
    end;
  end;
  Result := true;
end;


procedure TGotoForm.SetPosAbs(AValue: Integer);
begin
  EdAbs.Text := IntToStr(AValue);
end;

procedure TGotoForm.SetPosRel(AValue: Integer);
begin
  EdRel.Text := IntToStr(abs(AValue));
  rbBackward.Checked := AValue < 0;
  rbForward.Checked := AValue >= 0;
end;

end.

