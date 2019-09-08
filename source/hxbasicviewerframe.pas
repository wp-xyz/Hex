unit hxBasicViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  MPHexEditor;

type

  { TBasicViewerFrame }

  TBasicViewerFrame = class(TFrame)
    lblHeader: TLabel;
  public
    procedure UpdateData({%H-}AHexEditor: TMPHexEditor); virtual;
    procedure UpdateIconSet; virtual;
  end;



implementation

{$R *.lfm}


{ TBasicViewerFrame }

procedure TBasicViewerFrame.UpdateData(AHexEditor: TMPHexEditor);
begin
  // to be overridden...
end;

procedure TbasicViewerFrame.UpdateIconSet;
begin
  // to be overridden...
end;

end.

