unit LA.Data.Updater;

{
  *****************************************************************************
  TLADataUpdater - компонент наследник TLADataSource, выступает в роли
                   источника данных для подписанных на него объектов (наблюдателей)
    - знает о подключении к серверу Мониторинга (TLACustomConnector)
    - выполняет периодический сбор данных в потоке и уведомляет об изменениях
      линки (TLADataLink) через метод Notity
  *****************************************************************************
}


interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  LA.Data.Source.Intf,
  LA.Data.Source,
  LA.Threads, LA.Net.Connector, LA.Types.Monitoring;

type

  /// <summary>
  ///  Объект, который периодически запрашивает данные с серввера и уведомляет подписчиков
  /// </summary>
  TLADataUpdater = class(TLADataSource)
  private
    const
      MinInterval = 100;
      DefInterval = 1000;
    type
      // поток для получения данных с сервера Мониторинга
      TLADataUpdateThread = class(TLAIntervalThread)
      private
        FUpdater: TLADataUpdater;
        FIDs: TSIDArr;
        procedure InitIDs;
      protected
        procedure DoLinksUpdated;
        procedure Initialize; override;
        procedure Process; override;
      public
        constructor Create(CreateSuspended: Boolean; aUpdater: TLADataUpdater; aInterval: Int64); overload;
      end;
  private
    FThread: TLADataUpdateThread;
    FInterval: Int64;
    FConnector: TLACustomConnector;
    FOnLinksUpdated: TNotifyEvent;
    FOnException: TGetStrProc;

    // управление потоком обновления
    procedure Start;
    procedure Stop;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);

    procedure SetInterval(const Value: Int64);
    procedure SetConnector(const Value: TLACustomConnector);

//    // специализированные методы для разного типа линков
//    procedure ProcessSensorsResponse(const aResponse: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  protected
    // получить данные с сервера
    function GetDataFromServer(const IDs: TSIDArr): string; virtual; abstract;
    // разбор ответа сервера
    procedure ProcessServerResponse(const aResponse: string); virtual; abstract;
    // обработка ошибки получения данных
    procedure ProcessServerException(e: Exception); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // подключение к серверу Мониторинга
    property Connector: TLACustomConnector read FConnector write SetConnector;
    // включать будем по необходимости
    property Active: Boolean read GetActive write SetActive stored False;
    // период опроса сервера, мс
    property Interval: Int64 read FInterval write SetInterval default DefInterval;
    // дополнительное событие, которое вызывается после обновления линков
    property OnLinksUpdated: TNotifyEvent read FOnLinksUpdated write FOnLinksUpdated;
    // вызывается при ошибке во время обновления
    property OnException: TGetStrProc read FOnException write FOnException;
  end;



implementation

uses
  System.Math,
  LA.Data.Link.Sensor;

constructor TLADataUpdater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := DefInterval;
end;

destructor TLADataUpdater.Destroy;
begin
  Active := False;
  inherited;
end;

function TLADataUpdater.GetActive: Boolean;
begin
  Result := Assigned(FThread);
end;

//function TLADataUpdater.GetDataFromServer(const IDs: TSIDArr): string;
//begin
//  if Links.Count = 0 then
//    Exit('');
//
//  if Links[0].ClassType = TLASensorLink then
//    Result := Connector.SensorsDataAsText(IDs, True);
//  { TODO : добавить обработку ответа по трекерам }
//end;

procedure TLADataUpdater.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = Connector) then
    begin
      Stop;
      Connector := nil;
    end;
  end;
end;

//procedure TLADataUpdater.ProcessSensorsResponse(const aResponse: string);
//const
//  cValueDelimiter = ';';
//  cDataDelimiter = #13;
//var
//  p1, p2: Integer;
//  aLineID: string;
//  i, aLinkIndex, aLinkMaxIndex: Integer;
//begin
////  if FLinksChanged then
////    Exit;
//
//  aLinkMaxIndex := FLinks.Count - 1;
//  if aLinkMaxIndex < 0 then
//    Exit;
//
//  p1 := 1;
//  aLinkIndex := 0;
//
//  FLock.BeginRead;
//  try
//    /// из строки вида:
//    ///  id1;value1;status1;moment1$13
//    ///  id2;value2;status2;moment2$13
//    ///  вырезаем строку
//    ///  и эту строку сохраняем в Link с ID = id строки
//    ///  пропускать строки у которых id = ''
//    i := 1;
//    while i <= Length(aResponse) do
//    begin
//      // нашли конец значения и aLineID еще не найден
//      if (aResponse[i] = cValueDelimiter) and (aLineID = '') then
//      begin
//        aLineID := Copy(aResponse, p1, i - p1);
//        // строки с пустым ID пропускаем
//        if aLineID = '' then
//        begin
//          while (i <= Length(aResponse)) and (aResponse[i] <> cDataDelimiter)  do
//            Inc(i);
//          Inc(i);
//          p1 := i;
//          Continue;
//        end;
//
//        // ищем Link для этого ID
//        while (aLinkIndex <= aLinkMaxIndex) and (FLinks[aLinkIndex].ID <> aLineID) do
//          Inc(aLinkIndex);
//
//        // не нашли Линк - выходим (нет необходимости разбирать оставшуюся часть)
//        if aLinkIndex > aLinkMaxIndex then
//          Break;
//      end
//
//      // нашли конец строки
//      else if aResponse[i] = cDataDelimiter then
//      begin
//        p2 := i;
//        FLinks[aLinkIndex].Data := Copy(aResponse, p1, p2 - p1);
//        p1 := p2 + 1;
//        // сбрасываем, чтобы иницировать из следующей строки
//        aLineID := '';
//        // переходим к следующему линку
//        Inc(aLinkIndex);
//      end;
//
//      //
//      Inc(i);
//    end;
//
//  finally
//    FLock.EndRead;
//  end;
//end;

//procedure TLADataUpdater.ProcessServerResponse(const aResponse: string);
//begin
//  if Links.Count = 0 then
//    Exit;
//
//  if Links[0].ClassType = TLASensorLink then
//    ProcessSensorsResponse(aResponse);
//  { TODO : добавить обработку ответа по трекерам }
//end;

procedure TLADataUpdater.SetActive(const Value: boolean);
begin
  if Value then
    Start
  else
    Stop;
end;

procedure TLADataUpdater.SetConnector(const Value: TLACustomConnector);
begin
  // нельзя изменить коннeктор, во время работы потока
  if Active then
    Exit;

  FConnector := Value;
end;

procedure TLADataUpdater.SetInterval(const Value: Int64);
begin
  if FInterval <> Value then
  begin
    FInterval := Max(Value, MinInterval);
    if Active then
      FThread.Interval := FInterval;
  end;
end;

procedure TLADataUpdater.Start;
begin
  if Active then
    Exit;

  // поток не может завершиться сам ни при каких условиях,
  // только Stop корректно завершает и очищает поток
  FThread := TLADataUpdateThread.Create(True, Self, Interval);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

procedure TLADataUpdater.Stop;
begin
  if not Active then
    Exit;

  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
  FThread := nil;
end;

{ TLADataUpdater.TDataUpdateThread }

constructor TLADataUpdater.TLADataUpdateThread.Create(CreateSuspended: Boolean; aUpdater: TLADataUpdater; aInterval: Int64);
begin
  inherited CreateInterval(CreateSuspended, aInterval);
  FUpdater := aUpdater;
  FEvent.SetEvent;
end;

procedure TLADataUpdater.TLADataUpdateThread.DoLinksUpdated;
begin
  if Assigned(FUpdater) and Assigned(FUpdater.OnLinksUpdated) then
    FUpdater.OnLinksUpdated(FUpdater);
end;

procedure TLADataUpdater.TLADataUpdateThread.InitIDs;
begin
  FUpdater.FLock.BeginRead;
  try
    SetLength(FIDs, FUpdater.FLinks.Count);
    for var i := 0 to FUpdater.FLinks.Count - 1 do
      FIDs[i] := FUpdater.FLinks[i].ID;
  finally
    FUpdater.FLock.EndRead;
  end;
end;

procedure TLADataUpdater.TLADataUpdateThread.Initialize;
begin
  inherited;
  FUpdater.FLinksChanged := True;
end;

procedure TLADataUpdater.TLADataUpdateThread.Process;
var
  r: string;
begin
  try
    if FUpdater.FLinksChanged then
    begin
      InitIDs;
      FUpdater.FLinksChanged := False;
    end;

    /// получаем данные с сервера
    ///  в случае ошибки, отключаемся и выходим - в следующей итерации повторим попытку подключения и запрос данных
    try
      if not FUpdater.Connector.Connected then
      begin
        FUpdater.Connector.Connect;
        FUpdater.Connector.InitServerCache;
      end;
      //r := FUpdater.Connector.SensorsDataAsText(FIDs, True);
      r := FUpdater.GetDataFromServer(FIDs);
      // если запрос выполнен без ошибок, то нет необходимости повторно передавать IDs (сервер их запомнил)
      SetLength(FIDs, 0);
    except
      on e: Exception do
      begin
        FUpdater.ProcessServerException(e);

        if Assigned(FUpdater.OnException) then
          Queue(nil, procedure
            begin
              FUpdater.OnException(e.Message);
            end);

        // в случае ошибки, нужно будет заново подключаться к серверу и передавать ID запрашиваемых датчиков
        FUpdater.Connector.Disconnect;
        FUpdater.FLinksChanged := True;
        Queue(FUpdater.Notify);
        Exit;
      end;
    end;

    /// обрабатываем результат запроса
    FUpdater.ProcessServerResponse(r);

    /// уведомляем подписчиков
    Queue(FUpdater.Notify);

    /// в OnUpdate можем выполнить дополнительные действия с новыми данными
    if Assigned(FUpdater.OnLinksUpdated) then
      Queue(DoLinksUpdated);
  except
    on e: Exception do
      if Assigned(FUpdater.OnException) then
        Queue(nil, procedure
          begin
            FUpdater.OnException(e.Message);
          end);
  end;
end;


end.
