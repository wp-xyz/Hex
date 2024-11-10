unit ExImgList;

{$mode objfpc}{$H+}

interface

uses
  LCLVersion, Classes, SysUtils, Graphics, GraphUtil, Controls, ImgList;

{$IF LCL_FullVersion < 2030000}
procedure BitmapGrayscale(ABitmap: TCustomBitmap; RedFilter, GreenFilter, BlueFilter: Single);
{$IFEND}

function CreateDisabledImageList(AImageList: TCustomImageList; AOwner: TComponent): TImageList;

function CreateRecoloredImageList(AImageList: TCustomImageList; ANewColor: TColor;
  AOwner: TComponent): TImageList;
function CreateRecoloredImageList(AImageList: TCustomImageList; ANewColor: TColor;
  const ATransparentColors: Array of TColor; const ASkippedIndex: array of Integer;
  AOwner: TComponent): TImageList;

procedure SetImageListColor(AImageList: TCustomImageList; ANewColor: TColor);
procedure SetImageListColor(AImageList: TCustomImageList; ANewColor: TColor;
  const ATransparentColors: Array of TColor; const ASkippedIndex: array of Integer);

implementation

uses
  fpImage, fpCanvas, IntfGraphics;

{$IF LCL_FullVersion < 2030000}
procedure BitmapGrayscale(ABitmap: TCustomBitmap; RedFilter, GreenFilter, BlueFilter: Single);
var
  IntfImg: TLazIntfImage = nil;
  x, y: Integer;
  TempColor: TFPColor;
  Gray: Word;
  sum: Single;
begin
  // Normalize filter factors to avoid word overflow.
  sum := RedFilter + GreenFilter + BlueFilter;
  if sum = 0.0 then
    exit;
  RedFilter := RedFilter / sum;
  GreenFilter := GreenFilter / sum;
  BlueFilter := BlueFilter / sum;

  IntfImg := ABitmap.CreateIntfImage;
  try
    IntfImg.BeginUpdate;
    try
      for y := 0 to IntfImg.Height - 1 do
        for x := 0 to IntfImg.Width - 1 do
        begin
          TempColor := IntfImg.Colors[x, y];
          Gray := word(Round(TempColor.Red * RedFilter + TempColor.Green * GreenFilter + TempColor.Blue * BlueFilter));
          TempColor.Red := Gray;
          TempColor.Green := Gray;
          TempColor.Blue := Gray;
          IntfImg.Colors[x, y] := TempColor;
        end;
    finally
      IntfImg.EndUpdate;
    end;
    ABitmap.LoadFromIntfImage(IntfImg);
  finally
    IntfImg.Free;
  end;
end;
{$IFEND}

{ Creates a new image list as copy of AImageList and converts its images to
  grayscale. Intended to be used by TToolbar.DisabledImages. Avoids some
  drawing artefacts in the auto-generated disabled images due to poorly
  supported alpha channel in the built-in routines. }
function CreateDisabledImageList(AImageList: TCustomImageList; AOwner: TComponent): TImageList;
var
  i: Integer;
  bmp: TCustomBitmap;
  Resolution: TCustomImageListResolution;
begin
  if AImageList = nil then
  begin
    Result := nil;
    exit;
  end;

  Result := TImageList.Create(AOwner);
  AImageList.AssignTo(Result);
  Result.Scaled := AImageList.Scaled;

  bmp := TBitmap.Create;
  Result.BeginUpdate;
  try
    for i := 0 to Result.Count - 1 do
    begin
      for Resolution in Result.Resolutions do
      begin
        Resolution.GetBitmap(i, bmp);
        BitmapGrayScale(bmp, 0.30, 0.59, 0.11);
        Resolution.ImageList.Replace(i, bmp, nil, False);
      end;
    end;
  finally
    Result.EndUpdate;
    bmp.Free;
  end;
end;

{ Creates a new images list as copy of AImagelist and changes all pixels with
  alpha > 0 to the specified new color.
  Primarily intended to change the color of monochrome outline icons. }
function CreateRecoloredImageList(AImageList: TCustomImageList; ANewColor: TColor;
  AOwner: TComponent): TImageList;
begin
  Result := CreateRecoloredImageList(AImageList, ANewColor, [], [], AOwner);
end;

{ Creates a new images list as copy of AImagelist and changes all pixels with
  alpha > 0 to the specified new color and sets the alpha of all pixels with
  a color in then ATransparentColors array to 0, i.e. makes them alpha-transparent.
  Icons with ImageIndex in the ASkipIndex array are not handled.
  Primarily intended to change the color of monochrome outline icons. }
function CreateRecoloredImageList(AImageList: TCustomImageList; ANewColor: TColor;
  const ATransparentColors: array of TColor; const ASkippedIndex: array of Integer;
  AOwner: TComponent): TImageList;
begin
  Result := TImageList.Create(AOwner);
  AImageList.AssignTo(Result);
  Result.Scaled := AImageList.Scaled;
  SetImagelistColor(Result, ANewColor, ATransparentColors, ASkippedIndex);
end;

{ Replaces the color of all pixels with alpha > 0 by the given new color.
  Intended to change the color of monochrome outline icons. }
procedure SetImageListColor(AImageList: TCustomImageList; ANewColor: TColor);
begin
  SetImageListColor(AImageList, ANewColor, [], []);
end;

{ Replaces the color of all pixels with alpha > 0 by the given new color.
  Intended to change the color of monochrome outline icons. }
procedure SetImageListColor(AImageList: TCustomImageList; ANewColor: TColor;
  const ATransparentColors: array of TColor; const ASkippedIndex: array of Integer);
var
  i, x, y: Integer;
  IntfImg: TLazIntfImage = nil;
  newColor: TFPColor;
  tmpColor: TFPColor;
  transparentColors: Array of TFPColor;
  bmp: TCustomBitmap;
  Resolution: TCustomImageListResolution;

  function IsTransparent(AColor: TFPColor): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to High(transparentColors) do
      if transparentColors[i] = AColor then
      begin
        Result := true;
        exit;
      end;
    Result := false;
  end;

  function IsSkipped(AIndex: Integer): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to High(ASkippedIndex) do
      if ASkippedIndex[i] = AIndex then
      begin
        Result := true;
        exit;
      end;
    Result := false;
  end;

begin
  if AImageList = nil then
    exit;

  SetLength(transparentColors, Length(ATransparentColors));
  for i := 0 to High(transparentColors) do
    transparentColors[i] := TColorToFPColor(ATransparentColors[i]);

  bmp := TBitmap.Create;
  AImageList.BeginUpdate;
  try
    newColor := TColorToFPColor(ANewColor);
    for i := 0 to AImageList.Count - 1 do
    begin
      if  IsSkipped(i) then
        Continue;
      for Resolution in AImageList.Resolutions do
      begin
        Resolution.GetBitmap(i, bmp);
        IntfImg := bmp.CreateIntfImage;
        try
          IntfImg.BeginUpdate;
          try
            for y := 0 to IntfImg.Height - 1 do
              for x := 0 to IntfImg.Width - 1 do
              begin
                tmpColor := IntfImg.Colors[x, y];
                if tmpColor.Alpha > 0 then
                begin
                  if IsTransparent(tmpColor) then
                    tmpColor.Alpha := 0
                  else
                  begin
                    tmpColor.Red := newColor.Red;
                    tmpColor.Green := newColor.Green;
                    tmpColor.Blue := newColor.Blue;
                  end;
                  IntfImg.Colors[x, y] := tmpColor;
                end;
              end;
          finally
            IntfImg.EndUpdate;
          end;
          bmp.LoadFromIntfImage(IntfImg);
          Resolution.ImageList.Replace(i, bmp, nil, False);
        finally
          IntfImg.Free;
        end;
      end;
    end;
  finally
    AImageList.EndUpdate;
    bmp.Free;
  end;
end;

end.

