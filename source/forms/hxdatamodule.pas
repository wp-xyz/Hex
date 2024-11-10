unit hxDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  exImgList,
  hxGlobal, hxUtils;

type

  { TCommonData }

  TCommonData = class(TDataModule)
    Images_Office: TImageList;
    Images_SimpleSmall: TImageList;
  private
    FImages_SimpleSmall_DarkMode: TImageList;
    function GetImages: TImageList;
    function GetImages_SimpleSmall_DarkMode: TImageList;

  public
    property Images: TImageList read GetImages;
    property Images_SimpleSmall_DarkMode: TImageList read GetImages_SimpleSmall_DarkMode;

  end;

var
  CommonData: TCommonData;

implementation

{$R *.lfm}

function TCommonData.GetImages: TImageList;
begin
  case GuiParams.IconSet of
    isOffice:
      Result := Images_Office;
    isSimpleSmall:
      if IsDarkMode then
        Result := Images_SimpleSmall_DarkMode
      else
        Result := Images_SimpleSmall;
  end;
end;

function TCommonData.GetImages_SimpleSmall_DarkMode: TImageList;
begin
  if FImages_SimpleSmall_DarkMode = nil then
    FImages_SimpleSmall_DarkMode := CreateRecoloredImageList(Images_SimpleSmall, $FFFFFF, [$FFFFFF], [14], self);
  Result := FImages_SimpleSmall_DarkMode;
end;

end.

