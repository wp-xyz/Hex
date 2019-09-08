unit hxDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, hxGlobal;

type

  { TCommonData }

  TCommonData = class(TDataModule)
    Images_Office: TImageList;
    Images_SimpleSmall: TImageList;
  private
    function GetImages: TImageList;

  public
    property Images: TImageList read GetImages;

  end;

var
  CommonData: TCommonData;

implementation

{$R *.lfm}

function TCommonData.GetImages: TImageList;
begin
  case GuiParams.IconSet of
    isOffice      : Result := Images_Office;
    isSimpleSmall : Result := Images_SimpleSmall;
  end;
end;

end.

