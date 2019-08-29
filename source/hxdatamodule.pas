unit hxDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TCommonData }

  TCommonData = class(TDataModule)
    Images: TImageList;
  private

  public

  end;

var
  CommonData: TCommonData;

implementation

{$R *.lfm}

end.

