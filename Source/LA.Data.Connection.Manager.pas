unit LA.Data.Connection.Manager;

interface

uses
  System.Classes;

type
  TLAConnectionData = class(TPersistent)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;


  /// предназначен для управления подлкючениями к серверам Мониторинга
  ///  - для каждого подключения:
  ///     - хранилище лукапов: поиск по имени таблицы
  ///     - кеш иерархии
  ///     - очередь задач
  TLAConnectionManager = class(TComponent)

  end;

implementation

end.
