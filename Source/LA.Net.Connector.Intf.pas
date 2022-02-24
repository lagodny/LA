unit LA.Net.Connector.Intf;

interface

uses
  LA.Types.Monitoring;

type
  /// интерфейс подключения к серверу
  IDCConnector = interface
  ['{54ABD7AB-27A5-42A0-AE14-506242BB01F8}']
    procedure Connect;
    procedure Disconnect;
  end;

  /// интерфейс взаимодействия с подсистемой Мониторинга
  IDCMonitoring = interface
  ['{EA050BF1-4603-4279-9D4E-E9FABE0360A0}']
//    function SensorValue(const SID: String): String;
//    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
//    function GetSensorsData(const IDs: TSIDArr): TDataRecExtArr;
    function SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string;
  end;

  /// интерфейс взаимодействия с подсистемой Отслеживания трекеров
  IDCTracking = interface

  end;


implementation

end.
