unit LA.Data.Updater.Intf;

interface

type
  /// <summary>
  ///  Объект, за которым можно наблюдать
  ///  к нему можно подключаться/отключаться и он может сообщать, что что-то изменилось
  ///  aLink - это специальный объект, который поддерживает интерфейс IDCObserverLink,
  ///  создается в момент подключения и знает как уведомить наблюдателя
  ///  (содержит в себе ссылку на наблюдателя)
  /// </summary>
  IDCObservable<T> = interface(IInvokable)
    procedure Attach(const aLink: T);
    procedure Detach(const aLink: T);
    procedure Notify;
  end;

implementation

end.
