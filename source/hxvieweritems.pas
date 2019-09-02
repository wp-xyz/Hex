unit hxViewerItems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  hxGlobal;

type
  TDataItem = class
  private
    FDataType: TDataType;
    FDataSize: Integer;
    FBigEndian: Boolean;
    FOffset: Integer;
  public
    constructor Create(ADataType: TDataType; ADataSize: Integer; ABigEndian: Boolean);
    procedure Assign(const ASource: TDataItem); virtual;
    class function GetHeader(AIndex: Integer): String; virtual;
    function GetText(AIndex: Integer): String; virtual;
//    function GetValueAsString(var Buffer): String; virtual;
    class function IsBoolean(AIndex: Integer): Boolean; virtual;
  published
    property DataType: TDataType read FDataType write FDataType;
    property Offset: Integer read FOffset write FOffset;
    property DataSize: Integer read FDataSize write FDataSize;
    property BigEndian: Boolean read FBigEndian write FBigEndian;
  end;

  TDataItemClass = class of TDataItem;

  TRecordDataItem = class(TDataItem)
  private
    FName: String;
  public
    constructor Create(AName: String; ADataType: TDataType;
      ADataSize: Integer; ABigEndian: Boolean);
    procedure Assign(const ASource: TDataItem); override;
    class function GetHeader(AIndex: Integer): String; override;
    function GetText(AIndex: Integer): String; override;
  published
    property Name: String read FName write FName;
  end;

  TDataList = class(TFPObjectList);

implementation

uses
  StrUtils;

{ TDataItem }

constructor TDataItem.Create(ADataType: TDataType; ADataSize: Integer;
  ABigEndian: Boolean);
begin
  DataType := ADataType;
  DataSize := ADataSize;
  BigEndian := ABigEndian;
end;

procedure TDataItem.Assign(const ASource: TDataItem);
begin
  FDataType := ASource.DataType;
  FDataSize := ASource.DataSize;
  FBigEndian := ASource.BigEndian;
end;

{ AIndex = 0 --> DataType , 1 --> DataSize, 2--> BigEndian }
class function TDataItem.GetHeader(AIndex: Integer): String;
begin
  case AIndex of
   -1: Result := 'Value';
    0: Result := 'Data type'; //DataTypeNames[FDataType];
    1: Result := 'Size';
    2: Result := 'Offset';
    3: Result := 'BE';
  end;
end;

function TDataItem.GetText(AIndex: Integer): String;
begin
  case AIndex of
    0: Result := DataTypeNames[FDataType];
    1: Result := IntToStr(FDataSize);
    2: Result := IntToStr(FOffset);
    3: Result := IfThen(BigEndian, 'BE', 'LE');
  end;
end;

class function TDataItem.IsBoolean(AIndex: Integer): Boolean;
begin
  Result := AIndex = 3;  // 3 = index of BigEndian field
end;


{ TRecordDataItem }

constructor TRecordDataItem.Create(AName: String; ADataType: TDataType;
  ADataSize: Integer; ABigEndian: Boolean);
begin
  inherited Create(ADataType, ADataSize, ABigEndian);
  FName := AName;
end;

procedure TRecordDataItem.Assign(const ASource: TDataItem);
begin
  if ASource is TRecordDataItem then
    FName := TRecordDataItem(ASource).Name;
  inherited Assign(ASource);
end;

class function TRecordDataItem.GetHeader(AIndex: Integer): String;
begin
  if AIndex = 4 then
    Result := 'Name'
  else
    Result := inherited;
end;

{ Record layout: 0=data type, 1=data size, 2=offset, 3=BigEndian, 4=name }
function TRecordDataItem.GetText(AIndex: Integer): String;
begin
  if AIndex = 4 then
    Result := FName
  else
    Result := inherited;
end;

end.

