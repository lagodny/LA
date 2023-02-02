unit LA.Data.Link.Intf;

interface

type

  /// <summary>
  ///  Линк (адаптер) к наблюдателю
  ///  может возвращать ID - адрес наблюдателя, необходимый для запроса данных по этому адресу
  ///  полученные данные устанавливаются через SetData
  ///  уведомление наблюдателя выполняется через Notify
  /// </summary>
  ILADataLink = interface(IInvokable)
    /// <summary>
    ///   получить идентификатор, например адрес датчика или трекера
    /// </summary>
    function GetID: string;
    procedure SetID(const aID: string);
    /// <summary>
    ///   установить новые данные
    /// </summary>
    procedure SetData(const aData: string);
    function GetData: string;
    /// <summary>
    ///   уведомить наблюдателя
    /// </summary>
    procedure Notify;

    property ID: string read GetID write SetID;
    property Data: string read GetData write SetData;

  end;


implementation

end.
