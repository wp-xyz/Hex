unit hxAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
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
  LCLIntf,
  hxGlobal;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Image1.Picture.Assign(Application.Icon);
  Image1.Picture.Icon.Current := 0;
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

