unit hxAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons,
  hxGlobal, hxUtils_NonGUI, hxUtils;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblOMultiPanelComponent: TLabel;
    lblIcons: TLabel;
    lblMarkus: TLabel;
    lblMPHexEditor: TLabel;
    lblHexEditor: TLabel;
    lblAcknowledgements: TLabel;
    lblCompiler: TLabel;
    lblFPC: TLabel;
    lblLazarus: TLabel;
    lblIDE: TLabel;
    lblHex: TLabel;
    lblMichal: TLabel;
    lblOndrej: TLabel;
    lblIcons8: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure lblURLMouseEnter(Sender: TObject);
    procedure lblURLMouseLeave(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, Types;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
var
  mode: TScreenMode;
begin
  with Image1 do
  begin
    Picture.Assign(Application.Icon);
    Picture.Icon.Current := Picture.Icon.GetBestIndexForSize(Size(Width, Height));
  end;
  lblVersion.Caption := 'Version: ' + GetVersionStr();
  mode := GetScreenMode;
  lblFPC.Font.Color := LINK_COLOR[mode];
  lblLazarus.Font.Color := LINK_COLOR[mode];
  lblMichal.Font.Color := LINK_COLOR[mode];
  lblOndrej.Font.Color := LINK_COLOR[mode];
  lblIcons8.Font.Color := LINK_COLOR[mode];
end;

procedure TAboutForm.lblURLClick(Sender: TObject);
begin
  if Sender is TLabel then
    OpenURL(TLabel(Sender).Hint);
end;

procedure TAboutForm.lblURLMouseEnter(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style + [fsUnderline];
end;

procedure TAboutForm.lblURLMouseLeave(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style - [fsUnderline];
end;

end.

