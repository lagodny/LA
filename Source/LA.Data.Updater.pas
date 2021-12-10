unit LA.Data.Updater;

{
  *****************************************************************************
  - TDataUpdater - компонент, который содержит
    - список линков к наблюдателям (Датчики, Трекеры и прочее: линк - это
      по сути адаптер к наблюдателю, который знает как и что у наблюдателя
      нужно изменить при изменении объекта наоблюдения)
    - подключение к серверу Мониторинга
    - поток, который периодически выполняет запросы к серверу и уведомляет наблюдаетелей об изменениях
  - Наблюдатели должны подписываться на уведомления, при этом создается линк - TDCLink

  *****************************************************************************
}


interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  LA.Data.Source.Intf,
  LA.Data.Source,
  LA.Data.Link,
  LA.Threads, LA.Net.Connector, LA.Types.Monitoring;

type

  /// <summary>
  ///  Объект, который получает данные из Мониторинга и уведомляет подписчиков
  /// </summary>
  TDataUpdater = class(TLADataSource)
  private
    const
      MinInterval = 100;
      DefInterval = 1000;
    type
      // поток для получения данных с сервера Мониторинга
      TDataUpdateThread = class(TDCIntervalThread)
      private
        FUpdater: TDataUpdater;
        FIDs: TSIDArr;
        procedure InitIDs;
      protected
        procedure DoUpdate;
        procedure Initialize; override;
        procedure ProcessTimer; override;
      public
        constructor Create(CreateSuspended: Boolean; aUpdater: TDataUpdater; aInterval: Int64); overload;
      end;
  private
    FLock: TMREWSync;
    FLinks: TObjectList<TLALink>;
    FLinksChanged: Boolean;
    FThread: TDataUpdateThread;
    FInterval: Int64;
    FConnector: TDCCustomConnector;
    FOnUpdate: TNotifyEvent;

    // управление потоком обновления
    procedure Start;
    procedure Stop;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);

    procedure SetInterval(const Value: Int64);
    procedure SetConnector(const Value: TDCCustomConnector);
  protected
    procedure DoNotify(const aLink: TLALink); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // подключение, отключение, уведомление наблюдателей
    procedure Attach(const aLink: TLALink);
    procedure Detach(const aLink: TLALink);
    procedure Notify;

    // разбор ответа сервера
    procedure ProcessServerResponce(const aResponce: string);

    // список линков, в которых есть ссылки на наблюдателей (отсортирован по ID)
    // разрешаем линки с одинаковым ID, но в запросе оставляем только первый
    property Links: TObjectList<TLALink> read FLinks;
  published
    // подключение к серверу Мониторинга
    property Connector: TDCCustomConnector read FConnector write SetConnector;
    // включать будем по необходимости
    property Active: Boolean read GetActive write SetActive stored False;
    // период опроса сервера, мс
    property Interval: Int64 read FInterval write SetInterval;
    // дополнительное событие, которое вызывается после обновления линков
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;



implementation

uses
  System.Math;


procedure TDataUpdater.Attach(const aLink: TLALink);
var
  aInsertIndex: Integer;
begin
  FLock.BeginWrite;
  try
    if FLinks.Count = 0 then
      FLinks.Add(aLink)
    else
    begin
      FLinks.BinarySearch(aLink, aInsertIndex);
      FLinks.Insert(aInsertIndex, aLink);
    end;

    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

constructor TDataUpdater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := DefInterval;
  FLock := TMREWSync.Create;

  FLinks := TObjectList<TLALink>.Create(TDelegatedComparer<TLALink>.Create(
    function (const aLeft, aRight: TLALink): Integer
    begin
      Result := CompareStr(aLeft.GetID, aRight.GetID);
    end)
  , True);
end;

destructor TDataUpdater.Destroy;
begin
  Active := False;
  FLinks.Free;
  FLock.Free;
  inherited;
end;

procedure TDataUpdater.Detach(const aLink: TLALink);
begin
  FLock.BeginWrite;
  try
    FLinks.Remove(aLink);
    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDataUpdater.DoNotify(const aLink: TLALink);
begin
  aLink.Notify;
end;

//procedure TDataUpdater.DoThreadTerminated(aSender: TObject);
//begin
////  FreeAndNil(FThread);
//  FThread := nil;
//end;

function TDataUpdater.GetActive: Boolean;
begin
  Result := Assigned(FThread);
end;

procedure TDataUpdater.Notify;
var
  aLink: TLALink;
begin
  FLock.BeginRead;
  try
    for aLink in FLinks do
      DoNotify(aLink);
  finally
    FLock.EndRead;
  end;
end;

procedure TDataUpdater.ProcessServerResponce(const aResponce: string);
const
  cDataDelimiter = #13;
var
  p1, p2: Integer;
  i, aLinkIndex, aLinkMaxIndex: Integer;
begin
  if FLinksChanged then
    Exit;

  aLinkMaxIndex := FLinks.Count - 1;
  if aLinkMaxIndex < 0 then
    Exit;

  p1 := 1;
  aLinkIndex := 0;

  FLock.BeginRead;
  try
    for i := 1 to Length(aResponce) do
    begin
      if aResponce[i] = cDataDelimiter then
      begin
        p2 := i;
        FLinks[aLinkIndex].SetData(Copy(aResponce, p1, p2 - p1));
        p1 := p2 + 1;
        if aLinkIndex = aLinkMaxIndex then
          Break;
      end;
    end;

  finally
    FLock.EndRead;
  end;
end;

procedure TDataUpdater.SetActive(const Value: boolean);
begin
  if Value then
    Start
  else
    Stop;
end;

procedure TDataUpdater.SetConnector(const Value: TDCCustomConnector);
begin
  // нельзя изменить коннeктор, во время работы потока
  if Active then
    Exit;

  FConnector := Value;
end;

procedure TDataUpdater.SetInterval(const Value: Int64);
begin
  if FInterval <> Value then
  begin
    FInterval := Max(Value, MinInterval);
    if Active then
      FThread.Interval := FInterval;
  end;
end;

procedure TDataUpdater.Start;
begin
  if Active then
    Exit;

  // поток не может завершиться сам ни при каких условиях,
  // только Stop корректно завершает и очищает поток
  FThread := TDataUpdateThread.Create(True, Self, Interval);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

procedure TDataUpdater.Stop;
begin
  if not Active then
    Exit;

  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
  FThread := nil;
end;

{ TDataUpdater.TDataUpdateThread }

constructor TDataUpdater.TDataUpdateThread.Create(CreateSuspended: Boolean; aUpdater: TDataUpdater; aInterval: Int64);
begin
  inherited CreateInterval(CreateSuspended, aInterval);
  FUpdater := aUpdater;
  FEvent.SetEvent;
end;

procedure TDataUpdater.TDataUpdateThread.DoUpdate;
begin
  FUpdater.OnUpdate(FUpdater);
end;

procedure TDataUpdater.TDataUpdateThread.InitIDs;
begin
  FUpdater.FLock.BeginRead;
  try
    SetLength(FIDs, FUpdater.FLinks.Count);
    for var i := 0 to FUpdater.FLinks.Count - 1 do
      FIDs[i] := FUpdater.FLinks[i].GetID;
  finally
    FUpdater.FLock.EndRead;
  end;
end;

procedure TDataUpdater.TDataUpdateThread.Initialize;
begin
  inherited;
  FUpdater.FLinksChanged := True;
end;

procedure TDataUpdater.TDataUpdateThread.ProcessTimer;
var
  r: string;
  aLinkIndex, aDataIndex, aDataCount: Integer;
begin
  try
    if FUpdater.FLinksChanged then
    begin
      InitIDs;
      FUpdater.FLinksChanged := False;
    end;

    /// получаем данные с сервера
    ///  в случае ошибки, отключаемся и выходим - в слудующей инерации повторим попытку подключения и запрос данных
    try
      if not FUpdater.Connector.Connected then
        FUpdater.Connector.Connect;
      r := FUpdater.Connector.SensorsDataAsText(FIDs, True);
      // если запрос выполнен без ошибок, то нет необходимости повторно передавать IDs (сервер их запомнил)
      SetLength(FIDs, 0);
    except
      on e: Exception do
      begin
        // в случае ошибки, нужно будет заново подключаться к серверу и передавать ID запрашиваемых датчиков
        FUpdater.Connector.Disconnect;
        FUpdater.FLinksChanged := True;
        Exit;
      end;
    end;

    /// обрабатываем результат запроса
    FUpdater.ProcessServerResponce(r);

    /// уведомляем подписчиков
    Queue(FUpdater.Notify);

    /// в OnUpdate можем выполнить дополнительные действия с новыми данными
    if Assigned(FUpdater.OnUpdate) then
      Queue(DoUpdate);
  except
    on e: Exception do
      ;
  end;
end;


end.
