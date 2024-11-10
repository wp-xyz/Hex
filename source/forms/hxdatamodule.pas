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
    FDisabledImages_SimpleSmall_DarkMode: TImageList;
    function GetDisabledImages: TImageList;
    function GetDisabledImages_SimpleSmall_DarkMode: TImageList;
    function GetImages: TImageList;
    function GetImages_SimpleSmall_DarkMode: TImageList;

  public
    property Images: TImageList read GetImages;
    property Images_SimpleSmall_DarkMode: TImageList read GetImages_SimpleSmall_DarkMode;
    property DisabledImages: TImageList read GetDisabledImages;

  end;

var
  CommonData: TCommonData;

implementation

{$R *.lfm}

function TCommonData.GetDisabledImages: TImageList;
begin
  case GuiParams.IconSet of
    isOffice:
      Result := nil;
    isSimpleSmall:
      if IsDarkMode then
        Result := GetDisabledImages_SimpleSmall_DarkMode
      else
        Result := nil;
  end;
end;

function TCommonData.GetDisabledImages_SimpleSmall_DarkMode: TImageList;
begin
  if FDisabledImages_SimpleSmall_DarkMode = nil then
    FDisabledImages_SimpleSmall_DarkMode := CreateRecoloredImageList(Images_SimpleSmall, $606060, [$FFFFFF], [14], self);
  Result := FDisabledImages_SimpleSmall_DarkMode;
end;

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
    FImages_SimpleSmall_DarkMode := CreateRecoloredImageList(Images_SimpleSmall, $CFCFCF, [$FFFFFF], [14], self);
  Result := FImages_SimpleSmall_DarkMode;
end;

end.

