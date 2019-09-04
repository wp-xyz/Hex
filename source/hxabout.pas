unit hxAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Image1: TImage;
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
  LCLIntf,
  hxGlobal, hxDataModule;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Image1.Picture.Assign(Application.Icon);
  Image1.Picture.Icon.Current := 0;

  with CommonData.Images do
  begin
    GetBitmap(IMG_INDEX_OK, ButtonPanel.OKButton.Glyph);          // OK icon
  end;
end;

procedure TAboutForm.lblURLClick(Sender: TObject);
begin
  if Sender = lblFPC then
    OpenURL('https://www.freepascal.org/')
  else if Sender = lblLazarus then
    OpenURL('https://www.lazarus-ide.org/')
  else if Sender = lblIcons8 then
    OpenURL('http://www.icons8.com')
  else if Sender = lblOndrej then
    OpenURL('http://www.kluug.net/omultipanel.php')
  else if Sender = lblMichal then
    OpenURL('https://github.com/michalgw/mphexeditor');
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

