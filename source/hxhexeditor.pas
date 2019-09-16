unit hxHexEditor;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, MPHexEditor, MPHexEditorEx;

type
  THxHexEditor = class(TMPHexEditorEx)
  private
    FSecondSelStart: Integer;
    FSecondSelEnd: Integer;
    procedure SetSecondSelStart(AValue: Integer);
    procedure SetSecondSelEnd(AValue: Integer);
    procedure DrawCellHandler(Sender: TObject; ACanvas: TCanvas; ACol, ARow: Integer;
      var AText: String; ARect: TRect; var ADefaultDraw: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    property DataStorage;
    property SecondSelStart: Integer read FSecondSelStart write SetSecondSelStart;
    property SecondSelEnd: Integer read FSecondSelEnd write SetSecondSelEnd;
  end;

implementation

constructor THxHexEditor.Create(AOWner: TComponent);
begin
  inherited;
  FSecondSelStart := -1;
  FSecondSelEnd := -1;
  OnDrawCell := @DrawCellHandler;
end;

procedure THxHexEditor.DrawCellHandler(Sender: TObject; ACanvas: TCanvas;
  ACol, ARow: Integer; var AText: String; ARect: TRect;
  var ADefaultDraw: Boolean);
var
  P: Integer;
begin
  ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline, fsBold];
  if (ShowRuler and (ARow = 0)) or (ACol = 0) then
    exit;
  P := GetPosAtCursor(ACol, ARow);
  if (P >= 0) and (P >= SecondSelStart) and (P <= SecondSelEnd) then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderLine, fsBold];
end;

procedure THxHexEditor.SetSecondSelStart(AValue: Integer);
begin
  if FSecondSelStart = AValue then
    exit;

  FSecondSelStart := AValue;
  InvalidateGrid;
end;

procedure THxHexEditor.SetSecondSelEnd(AValue: Integer);
begin
  if FSecondSelEnd = AValue then
    exit;

  FSecondSelEnd := AValue;
  InvalidateGrid;
end;

end.

