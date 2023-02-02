unit LA.Types.Tracker;

interface

uses
  System.SysUtils,
  System.Generics.Collections, System.Generics.Defaults,
  SynCrossPlatformJSON;


type
  /// <summary>
  ///   Прототип Тега
  /// </summary>
  TLATagPrototype = class
  private
    {$REGION 'Fields'}
    FSID: string;
    FName: string;
    FUN: string;
    FKind: Integer;
    FID: Integer;
    {$ENDREGION}
  public
    procedure Init(v: Variant);

    property ID: Integer read FID write FID;
    property SID: string read FSID write FSID;
    property Name: string read FName write FName;
    property UN: string read FUN write FUN;
    property Kind: Integer read FKind write FKind;
  end;
  TLATagPrototypes = TObjectDictionary<Integer,TLATagPrototype>;


  /// <summary>
  ///   Тег
  /// </summary>
  TLATag = class
  private
    {$REGION 'Fields'}
    FProto: TLATagPrototype;
    FValue: Variant;
    FAddr: string;
    FSID: string;
//    FText: string;
    FStatus: string;
    {$ENDREGION}
  public
    procedure FromJson(const aJson: string;  aPrototypes: TLATagPrototypes);
    procedure Init(v: Variant; aPrototypes: TLATagPrototypes);

    // прототип тега
    property Proto: TLATagPrototype read FProto write FProto;

    // строковый идентификатор для поиска тега
    property SID: string read FSID write FSID;
    // адрес тега в Мониторинге для получения данных
    property Addr: string read FAddr write FAddr;

    // числовое значение тега
    property Value: Variant read FValue write FValue;
//    // текстовое представление значения
//    property Text: string read FText write FText;
    // наименование ошибки, если она есть (пусто, если ошибки нет)
    property Status: string read FStatus write FStatus;
  end;
  TLATags = TObjectDictionary<string,TLATag>;

  /// <summary>
  ///   Стандартизированные имена тегов трекера
  /// </summary>
  TLATagNamesRec = record
  const
    Time = 'Time';

    Lat = 'Lat';
    Lon = 'Lon';
    Alt = 'Height';
    Sat = 'Sat';
    Course = 'Curs';
    Speed = 'Speed';

    Distance = 'Distance';
    DistanceSpeed1TO = 'DistanceSpeed1TO';
    DistanceSpeed1Trip = 'DistanceSpeed1Trip';

    Ignition = 'Ignition';
    Movement = 'Movement';

    FuelLevel = 'FuelLevel';
    FuelLevel0 = 'FuelLevel0';
    FL1 = 'FL1';
    FL2 = 'FL2';

    ExternalVoltage = 'ExternalVoltage';
  end;




implementation

{ TTagPrototype }

procedure TLATagPrototype.Init(v: Variant);
begin
  FID := v.id;
  FSID := v.sid;
  FName := v.name;
  FUN := v.un;
  FKind := v.kind;
end;

{ TTag }
(*
  {
    "sid": "Lon",
    "addr": "48669",
    "proto": 599
  }
*)
procedure TLATag.FromJson(const aJson: string; aPrototypes: TLATagPrototypes);
var
  vd: TJSONVariantData;
begin
  vd.Init(aJson);
  SID := vd.Value['sid'];
  Addr := vd.Value['addr'];
  if Assigned(aPrototypes) then
    aPrototypes.TryGetValue(vd.Value['proto'], FProto);
end;

procedure TLATag.Init(v: Variant; aPrototypes: TLATagPrototypes);
begin
  FSID := v.sid;
  FAddr := v.addr;
  if Assigned(aPrototypes) then
    aPrototypes.TryGetValue(v.proto, FProto);
end;


end.
