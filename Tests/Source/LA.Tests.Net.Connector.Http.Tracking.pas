unit LA.Tests.Net.Connector.Http.Tracking;

interface

uses
  DUnitX.TestFramework,
  LA.Net.Connector,
  LA.Net.Connector.Http, LA.Net.Connector.Intf;

type
  [TestFixture]
  TTest_TDCHttpConnector = class
  private
    FConnector: TLAHttpTrackingConnection;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test;

  end;


implementation

uses
  LA.Tests.Net.Consts,
  LA.Types.Monitoring;


{ TTest_TDCHttpConnector }

procedure TTest_TDCHttpConnector.Setup;
begin
  FConnector := TLAHttpTrackingConnection.Create(nil);
  FConnector.UserName := cUserName;
  FConnector.Password := cPassword;
  FConnector.Address := cHttpAddr;//'localhost:89' ;
end;

procedure TTest_TDCHttpConnector.TearDown;
begin
  FConnector.Free;
end;

procedure TTest_TDCHttpConnector.Test;
var
  r: Variant;
begin
  FConnector.Connect;
  // '[{"id":174,"name":"Demo","group":0}]'
  r := FConnector.GetClients;
(*
'{"prototypes":{"_600":{"id":600,"sid":"Height","name":"Высота","un":"м","kind":0},"_599":{"id":599,"sid":"Lon","name":"Долгота","un":"°","kind":0},"_604":{"id":604,"sid":"Ignition","name":"Зажигание","un":"","kind":1},"_602":{"id":602,"sid":"Curs","name":"Курс","un":"°","kind":0},"_645":{"id":645,"sid":"Route","name":"Маршрут (задание)","un":"","kind":1},"_644":{"id":644,"sid":"Location","name":"Местоположение","un":"","kind":1},"_621":{"id":621,"sid":"ExternalVoltage","name":"Напряжение питания","un":"В","kind":0},"_603":{"id":603,"sid":"Distance","name":"Пробег","un":"км","kind":2},"_601":{"id":601,"sid":"Speed","name":"Скорость","un":"км/ч","kind":0},"_598":{"id":598,"sid":"Lat","name":"Широта","un":"°","kind":0},"_624":{"id":624,"sid":"DistanceRace","name":"Пробег в рейсе","un":"км","kind":2},"_623":{"id":623,"sid":"DistanceTO","name":"Пробег с ТО","un":"км","kind":2},"_641":{"id":641,"sid":"ExtEngineTemp","name":"Температура двигателя","un":"°C","kind":0},"_642":{"id":642,"sid":"ExtEngineRPM","name":"Обороты двигателя","un":"об/мин","kind":0},"_692":{"id":692,"sid":"FuelLevelPercentage","name":"Уровень топлива, %","un":"%","kind":0},"_693":{"id":693,"sid":"DoorStatus","name":"Статус дверей","un":"","kind":1},"_700":{"id":700,"sid":"FuelCounter","name":"Счетчик топлива","un":"л","kind":2}},"devices":[{"id":3475,"name":"N  1 (ВЕ9806АЕ) - Lanos 356173062574491","group":0,"tags":{"Height":{"sid":"Height","addr":"41180","proto":600},"Lon":{"sid":"Lon","addr":"41179","proto":599},"Ignition":{"sid":"Ignition","addr":"41185","proto":604},"Curs":{"sid":"Curs","addr":"41182","proto":602},"Route":{"sid":"Route","addr":"50178","proto":645},"Location":{"sid":"Location","addr":"41232","proto":644},"ExternalVoltage":{"sid":"ExternalVoltage","addr":"41186","proto":621},"Distance":{"sid":"Distance","addr":"41187","proto":603},"Speed":{"sid":"Speed","addr":"41181","proto":601},"Lat":{"sid":"Lat","addr":"41178","proto":598}}},{"id":3476,"name":"N  2","group":0,"tags":{"Height":{"sid":"Height","addr":"48670","proto":600},"Lon":{"sid":"Lon","addr":"48669","proto":599},"Ignition":{"sid":"Ignition","addr":"48678","proto":604},"Curs":{"sid":"Curs","addr":"48672","proto":602},"ExternalVoltage":{"sid":"ExternalVoltage","addr":"48679","proto":621},"Distance":{"sid":"Distance","addr":"48675","proto":603},"DistanceRace":{"sid":"DistanceRace","addr":"48677","proto":624},"DistanceTO":{"sid":"DistanceTO","addr":"48676","proto":623},"Speed":{"sid":"Speed","addr":"48671","proto":601},"Lat":{"sid":"Lat","addr":"48668","proto":598}}},{"id":3477,"name":"N  3 Kangoo (Оптимал) FMB630","group":0,"tags":{"EngineTemperature":{"sid":"EngineTemperature","addr":"49137","proto":641},"EngineRPM":{"sid":"EngineRPM","addr":"49052","proto":642},"FuelLevelPercentage":{"sid":"FuelLevelPercentage","addr":"49054","proto":692},"DoorStatus":{"si...
*)
  r := FConnector.GetDevices([]);
  Assert.IsNotEmpty(r);
(*
'{"_3475":{"id":3475,"status":"","tags":{}},"_3476":{"id":3476,"status":"","tags":{}},"_3477":{"id":3477,"status":"","tags":{}}}'
*)
  r := FConnector.GetDevicesData([]);
end;

end.
