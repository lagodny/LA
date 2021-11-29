unit LA.Data.Link.Intf;

interface

type

  /// <summary>
  ///  Линк (адаптер) к наблюдателю
  ///  может возвращать ID - адрес наблюдателя, необходимый для запроса данных по этому адресу
  ///  полученные данные устанавливаются через SetData
  ///  уведомление наблюдателя выполняется через Notify
  /// </summary>
  IDCLink = interface(IInvokable)
    /// <summary>
    ///   получить идентификатор, например адрес датчика или трекера
    /// </summary>
    function GetID: string;
    /// <summary>
    ///   установить новые данные
    /// </summary>
    procedure SetData(const aData: string);
    /// <summary>
    ///   уведомить наблюдателя
    /// </summary>
    procedure Notify;
  end;


implementation

end.
