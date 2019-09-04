unit hxPascalRecordForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  SynEdit, SynHighlighterPas;

type

  { TPascalRecordForm }

  TPascalRecordForm = class(TForm)
    btnCopyToClipboard: TBitBtn;
    btnOK: TBitBtn;
    edRecordName: TEdit;
    lblRecordName: TLabel;
    SynEdit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure edRecordNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCode: TStrings;
    procedure SetCode(AList: TStrings);
  public
    property Code: TStrings read FCode write SetCode;
  end;

var
  PascalRecordForm: TPascalRecordForm;

implementation

{$R *.lfm}

uses
  Clipbrd,
  hxDataModule;

{ TPascalRecordForm }

procedure TPascalRecordForm.btnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := SynEdit.Lines.Text;
end;

procedure TPascalRecordForm.edRecordNameChange(Sender: TObject);
begin
  SynEdit.Lines.Assign(Code);
  if edRecordName.Text <> '' then
    SynEdit.Lines[1] := Format(SynEdit.Lines[1], [edRecordName.Text]);
end;

procedure TPascalRecordForm.FormCreate(Sender: TObject);
begin
  FCode := TStringList.Create;
end;

procedure TPascalRecordForm.FormDestroy(Sender: TObject);
begin
  FCode.Free;
end;

procedure TPascalRecordForm.FormShow(Sender: TObject);
begin
  edRecordNameChange(nil);
end;

procedure TPascalRecordForm.SetCode(AList: TStrings);
begin
  FCode.Assign(AList);
end;

end.

