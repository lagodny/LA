unit LA.Net.Connector.Intf;

interface

uses
  LA.Types.Monitoring;

type
  /// интерфейс доступа к серверу
  IDCConnector = interface
  ['{54ABD7AB-27A5-42A0-AE14-506242BB01F8}']
    procedure Connect;
    procedure Disconnect;

    function SensorValue(const SID: String): String;
    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
  end;


implementation

end.
