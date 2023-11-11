unit LA.Data.Link.Sensor.Intf;

interface

uses
  LA.Data.Link.Intf;

type
  ILASensorLink = interface(ILADataLink)
    function GetValue: Double;
    function GetText: string;
    function GetStatus: string;
    function GetTimestamp: TDateTime;

    procedure SetValue(const Value: Double);
    procedure SetText(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTimestamp(const Value: TDateTime);

    // числовое значение, необходимо для проверок на допустимые значения
    property Value: Double read GetValue write SetValue;
    // текстовое представление для отображения справочной информации и специального форматирования (date, hex, bin..)
    property Text: string read GetText write SetText;
    // статус датчика, текстовое описание ошибок, если они есть, если ошибок нет, то пустая строка
    property Status: string read GetStatus write SetStatus;
    // момент получения данных с датчика на сервере
    property Timestamp: TDateTime read GetTimestamp write SetTimestamp;
  end;

implementation

end.
