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
  LA.Data.Updater.Intf,
  LA.Data.Link,
  LA.Threads, LA.Net.Connector, LA.Types.Monitoring;

type

  /// <summary>
  ///  Объект, который получает данные из Мониторинга и уведомляет подписчиков
  /// </summary>
  TDataUpdater = class(TComponent, IDCObservable<TDCLink>)
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
    FLinks: TObjectList<TDCLink>;
    FLinksChanged: Boolean;
    FThread: TDataUpdateThread;
    FInterval: Int64;
    FConnector: TDCCustomConnector;
    FOnUpdate: TNotifyEvent;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure Start;
    procedure Stop;
    procedure SetInterval(const Value: Int64);
    procedure DoThreadTerminated(aSender: TObject);
    procedure SetConnector(const Value: TDCCustomConnector);
  protected
    procedure DoNotify(const aLink: TDCLink); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // подключение, отключение, уведомление наблюдателей
    procedure Attach(const aLink: TDCLink);
    procedure Detach(const aLink: TDCLink);
    procedure Notify;

    // разбор ответа сервера
    procedure ProcessServerResponce(const aResponce: string);

    // список линков, в которых есть ссылки на наблюдателей (отсортирован по ID)
    // разрешаем линки с одинаковым ID, но в запросе оставляем только первый
    property Links: TObjectList<TDCLink> read FLinks;
  published
    // подключение к серверу Мониторинга
    property Connector: TDCCustomConnector read FConnector write SetConnector;
    // включать будем по необходимости
    property Active: Boolean read GetActive write SetActive stored False;
    // период опроса сервера, мс
    property Interval: Int64 read FInterval write SetInterval;

    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;



implementation

uses
  System.Math;


procedure TDataUpdater.Attach(const aLink: TDCLink);
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

  FLinks := TObjectList<TDCLink>.Create(TDelegatedComparer<TDCLink>.Create(
    function (const aLeft, aRight: TDCLink): Integer
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

procedure TDataUpdater.Detach(const aLink: TDCLink);
begin
  FLock.BeginWrite;
  try
    FLinks.Remove(aLink);
    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDataUpdater.DoNotify(const aLink: TDCLink);
begin
  aLink.Notify;
end;

procedure TDataUpdater.DoThreadTerminated(aSender: TObject);
begin
//  FreeAndNil(FThread);
  FThread := nil;
end;

function TDataUpdater.GetActive: Boolean;
begin
  Result := Assigned(FThread);
end;

procedure TDataUpdater.Notify;
var
  aLink: TDCLink;
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
  // нельзя изменить коннктор, во время работы потока
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

  FThread := TDataUpdateThread.Create(True, Self, Interval);
  FThread.FreeOnTerminate := True;
  FThread.OnTerminate := DoThreadTerminated;
  FThread.Start;
end;

procedure TDataUpdater.Stop;
begin
  if not Active then
    Exit;

  FThread.Terminate;
  FThread := nil;
  //FThread.WaitFor;
end;

{ TDataUpdater.TDataUpdateThread }

constructor TDataUpdater.TDataUpdateThread.Create(CreateSuspended: Boolean; aUpdater: TDataUpdater; aInterval: Int64);
begin
  inherited CreateInterval(CreateSuspended, aInterval);
  FUpdater := aUpdater;
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
//  r: TDataRecExtArr;
  r: string;
  aLinkIndex, aDataIndex, aDataCount: Integer;
begin
  if FUpdater.FLinksChanged then
  begin
    InitIDs;
    FUpdater.FLinksChanged := False;
  end;

  r := FUpdater.Connector.SensorsDataAsText(FIDs);
  // в следующий запрос ID не передаем, они будут взяты из кеша сервера
  SetLength(FIDs, 0);

  FUpdater.ProcessServerResponce(r);

  Queue(FUpdater.Notify);

//  // запрашиваем данные с сервера
//  r := FUpdater.Connector.GetSensorsData(FIDs);
//  aDataCount := Length(r);
//  if aDataCount > 0 then
//  begin
//    aDataIndex := 0;
//    // обновляем линки
//    FUpdater.FLock.BeginRead;
//    try
//      /// линки отсортированы в порядке возрастания ID
//      ///  результат приходит в таком же порядке, но за время отработки запроса
//      ///  линки могли измениться (порядок не изменился)
//      ///  значит если ID текущего линка отличается он ID текущего ответа, то
//      ///  нужно выполнить поиск
//      for aLinkIndex := 0 to FUpdater.FLinks.Count - 1 do
//      begin
//        case CompareStr(FUpdater.Links[aLinkIndex].GetID, r[aDataIndex].SID) of
//          -1: // <
//          begin
//            // переходим к следующему линку
//            Continue;
//          end;
//
//          0: // =
//          begin
//            // нашли - устанавливаем новые данные
//            FUpdater.Links[aLinkIndex].SetData(r[aDataIndex].v);
//          end;
//
//          1: // >
//          begin
//            // выполняем поиск в массиве данных
//            while (aDataIndex < aDataCount) and (CompareStr(FUpdater.Links[aLinkIndex].GetID, r[aDataIndex].SID) = 1) do
//              Inc(aDataIndex);
//
//            // прерываем цикл, если данных больше нет
//            if aDataIndex >= aDataCount then
//              Break;
//
//            // нашли - устанавливаем новые данные
//            if CompareStr(FUpdater.Links[aLinkIndex].GetID, r[aDataIndex].SID) = 0 then
//              FUpdater.Links[aLinkIndex].SetData(r[aDataIndex].v);
//          end;
//        end;
//      end;
//    finally
//      FUpdater.FLock.EndRead;
//    end;
//
//    // уведомляем наблюдателей
//    Queue(FUpdater.Notify);
//  end;


  if Assigned(FUpdater.OnUpdate) then
    Queue(DoUpdate);
end;


end.
