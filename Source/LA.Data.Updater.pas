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
  System.Classes, System.SysUtils,
  System.Generics.Collections,
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
        FIDs: TIDArr;
      protected
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

    // формирование строки запрашиваемых адресов
    function GetRequestedAddresses: string;
    // разбор ответа сервера
    procedure ProcessServerResponce(const aResponce: string);

    // список линков, в которых есть ссылки на наблюдателей
    property Links: TObjectList<TDCLink> read FLinks;
  published
    // подключение к серверу Мониторинга
    property Connector: TDCCustomConnector read FConnector write SetConnector;
    // включать будем по необходимости
    property Active: Boolean read GetActive write SetActive stored False;
    // период опроса сервера, мс
    property Interval: Int64 read FInterval write SetInterval;
  end;



implementation

uses
  System.Math;


procedure TDataUpdater.Attach(const aLink: TDCLink);
begin
  FLock.BeginWrite;
  try
    FLinks.Add(aLink);
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
  FLinks := TObjectList<TDCLink>.Create;
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
  FreeAndNil(FThread);
end;

function TDataUpdater.GetActive: Boolean;
begin
  Result := Assigned(FThread);
end;

function TDataUpdater.GetRequestedAddresses: string;
var
  aLink: TDCLink;
begin
  Result := '';
  FLock.BeginRead;
  try
    for aLink in FLinks do
      Result := Result + aLink.GetID + ';';
  finally
    FLock.EndRead;
  end;
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
          Exit;
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
  FThread.OnTerminate := DoThreadTerminated;
  FThread.Start;
end;

procedure TDataUpdater.Stop;
begin
  if Active then
    Exit;

  FThread.Terminate;
  FThread.WaitFor;
end;

{ TDataUpdater.TDataUpdateThread }

constructor TDataUpdater.TDataUpdateThread.Create(CreateSuspended: Boolean; aUpdater: TDataUpdater; aInterval: Int64);
begin
  inherited Create(CreateSuspended, aInterval);
  FUpdater := aUpdater;
end;

procedure TDataUpdater.TDataUpdateThread.ProcessTimer;
begin
  // запрашиваем данные с сервера
  FUpdater.Connector.GroupSensorDataExtByID(FIDs);
  // обновляем линки
  // уведомляем наблюдателей
end;


end.
