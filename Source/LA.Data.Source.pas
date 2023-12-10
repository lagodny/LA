unit LA.Data.Source;

interface

uses
  System.Classes,
  System.Generics.Collections, System.Generics.Defaults,
  System.SyncObjs,
  System.SysUtils, System.DateUtils,
  LA.Data.Source.Intf,
  LA.Data.Link.Intf;

resourcestring
  sIncorrectDataLinkType = 'You have to use another DataSource for this type Link';

type
  ELADataLinkException = class(Exception)
  end;

  TLADataLink = class;
  TLADataSource = class;
  TLADataLinkList = TObjectList<TLADataLink>;

//  TLANotifyEventKind = (
//    nekDataChanged,
//    nekValueChanged,
//    nekPropChanged
//  );



  /// <summary>
  ///   Линк к источнику данных.
  ///   Содержит в себе информацию об источнике (DataSource) и адресе (ID) данных
  ///   В компонентах должен быть определен как свойство доступное для чтения и записи,
  ///   чтобы оно сохранялось/восстанавливалось из dfm/fmx файлов
  /// </summary>
  TLADataLink = class(TInterfacedPersistent, ILADataLink) //, IDCObservable<TLADataLink> )
  private
    [Weak] FOwner: TPersistent;
    [Weak] FLinks: TLADataLinkList;
    FID: string;
    FData: string;
    FDataSource: TLADataSource;
    FIsNeedNotify: Boolean;
    FIsNeedEncodeData: Boolean;
    FLastNotifyTime: TDateTime;
    FOnDataChange: TNotifyEvent;
    FOnOwnerNotify: TNotifyEvent;
    FUpdateCount: Integer;
    FNotifyConstantly: Boolean;
    procedure SetNotifyConstantly(const Value: Boolean);
  protected
    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;


    function GetID: string; virtual;
    procedure SetID(const Value: string); virtual;

    function GetData: string; virtual;
    /// может быть вызвана в другом потоке, поэтому не должна взаимодействовать другими элементами
    procedure SetData(const Value: string); virtual;

    function GetDataSource: TLADataSource; virtual;
    procedure SetDataSource(const Value: TLADataSource); virtual;
    /// разбирает строку данных
    ///  нужно переопределить для датчика, для трекера и т.д. чтобы иметь доступ к
    ///  значению, моменту времени и статусу
    procedure EncodeData; virtual;

    procedure DoNeedNotify;
  public
    constructor Create(const AOwner: TPersistent); virtual;
    destructor Destroy; override;
    /// компонент владелец должен уведомить об удалении компонента вызвав эту процедуру
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Clear; virtual;
    // событие изменения данных для компонента владельца
    property OnOwnerNotify: TNotifyEvent read FOnOwnerNotify write FOnOwnerNotify;
  public
    /// владелец - должен быть указан при создании линка
    property Owner: TPersistent read FOwner;

    // должна быть вызвана в основном потоке, именно в ней нужно обновлять элементы интерфейса
    procedure Notify; overload; virtual;

    // список линков, не управляет временим их жизни
    property Links: TLADataLinkList read FLinks;

  published
    // адрес данных на сервере (ID датчика или трекера)
    property ID: string read GetID write SetID;

    /// строка данных полученная с сервера для указанного ID
    ///  должна быть разобрана в процедуре EncodeData
    property Data: string read GetData write SetData;
    /// уведомлять при каждом получении данных, независимо от того изменились они или нет
    ///  важно для рисования графиков в режиме реального времени
    property NotifyConstantly: Boolean read FNotifyConstantly write SetNotifyConstantly;

    // объект, который обеспечивает получение данных
    // должно быть очищено при удалении соответствующего компонента
    property DataSource: TLADataSource read GetDataSource write SetDataSource;

    /// событие изменения данных для разработчика
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
  end;


  /// <summary>
  ///  Источник данных. Реализует шаблон Наблюдатель
  /// </summary>
  TLADataSource = class(TComponent, IDCObservable<TLADataLink>)
  protected
    FLock: TMREWSync;
    FLinks: TLADataLinkList;
    FLinksChanged: Boolean;
  protected
    // подключение, отключение, уведомление наблюдателей
    procedure Attach(const aLink: TLADataLink); virtual;
    procedure Detach(const aLink: TLADataLink); virtual;
    procedure Notify; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectLinksFromDataSource(aDataSource: TLADataSource); virtual;

    // список линков, не управляет временим их жизни
    property Links: TLADataLinkList read FLinks;
  end;

implementation

{ TLADataSource }

procedure TLADataSource.Attach(const aLink: TLADataLink);
var
  aInsertIndex: Integer;
begin
  FLock.BeginWrite;
  try
    if FLinks.Count = 0 then
      FLinks.Add(aLink)
    else
    begin
      // мы можем работать с линками только одного класса, поэтому
      // проверяем, что новый линк того же класса, что и уже добавленные ранее
      if aLink.ClassType <> FLinks[0].ClassType then
        raise Exception.Create(sIncorrectDataLinkType);

      FLinks.BinarySearch(aLink, aInsertIndex);
      FLinks.Insert(aInsertIndex, aLink);
    end;

    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TLADataSource.ConnectLinksFromDataSource(aDataSource: TLADataSource);
begin
  for var i := aDataSource.Links.Count - 1 downto 0 do
    aDataSource.Links[i].DataSource := Self;
end;

constructor TLADataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TMREWSync.Create;

  FLinks := TObjectList<TLADataLink>.Create(TDelegatedComparer<TLADataLink>.Create(
    function (const aLeft, aRight: TLADataLink): Integer
    begin
      // линки должны быть отсортированы в порядке возрастания ID, чтобы ускорить разбор ответа сервера
      Result := CompareStr(aLeft.ID, aRight.ID);
      // линки могут иметь одинаковые ID, тогда сравниваем их адреса в памяти
      if Result = 0 then
        Result := Integer(aLeft) - Integer(aRight);
    end)
  , False);
end;

destructor TLADataSource.Destroy;
begin
  for var aLink in FLinks do
    aLink.FDataSource := nil;

  FLinks.Free;
  FLock.Free;
  inherited;
end;

procedure TLADataSource.Detach(const aLink: TLADataLink);
begin
  FLock.BeginWrite;
  try
    FLinks.Remove(aLink);
    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TLADataSource.Notify;
var
  aLink: TLADataLink;
begin
  FLock.BeginRead;
  try
    for aLink in FLinks do
      aLink.Notify;
  finally
    FLock.EndRead;
  end;
end;

{ TLADataLink }

procedure TLADataLink.AssignTo(Dest: TPersistent);
begin
  if Dest is TLADataLink then
  begin
    var aDest: TLADataLink := TLADataLink(Dest);
    aDest.ID := ID;
    aDest.DataSource := DataSource;
    aDest.Data := Data;
    aDest.OnOwnerNotify := OnOwnerNotify;
    aDest.OnDataChange := OnDataChange;
  end
  else
    inherited;
end;

//procedure TLADataLink.Attach(const aLink: TLADataLink);
//begin
//  FLinks.Add(aLink)
//end;

procedure TLADataLink.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLADataLink.Clear;
begin
  DataSource := nil;
end;

constructor TLADataLink.Create(const AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FLinks := TLADataLinkList.Create;
end;

destructor TLADataLink.Destroy;
begin
  DataSource := nil;
  FLinks.Free;
  inherited;
end;

procedure TLADataLink.DoNeedNotify;
begin
  FIsNeedNotify := True;
  if FUpdateCount = 0 then
    Notify;
end;

//procedure TLADataLink.Detach(const aLink: TLADataLink);
//begin
//  FLinks.Remove(aLink);
//end;

procedure TLADataLink.EncodeData;
begin
  // наследники должны разобрать поле Data
  FIsNeedEncodeData := False;
end;

procedure TLADataLink.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Notify;
end;

function TLADataLink.GetData: string;
begin
  Result := FData;
end;

function TLADataLink.GetDataSource: TLADataSource;
begin
  Result := FDataSource;
end;

function TLADataLink.GetID: string;
begin
  Result := FID;
end;

function TLADataLink.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TLADataLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if (AComponent = DataSource) then
      DataSource := nil
  end;
end;

procedure TLADataLink.Notify;
begin
  if NotifyConstantly or FIsNeedNotify or (SecondsBetween(Now, FLastNotifyTime) > 10) then
  begin
    if FIsNeedEncodeData then
      EncodeData;
    // уведомляем компонент-владелец
    if Assigned(FOnOwnerNotify) then
      OnOwnerNotify(Self);
    // отрабатываем пользовательский код
    if Assigned(FOnDataChange) then
      OnDataChange(Self);

    FIsNeedNotify := False;
    FLastNotifyTime := Now;
  end;
end;

procedure TLADataLink.SetData(const Value: string);
begin
  if Value <> FData then
  begin
    FData := Value;
    FIsNeedEncodeData := True;
    FIsNeedNotify := True;
  end;
end;

procedure TLADataLink.SetDataSource(const Value: TLADataSource);
begin
  if FDataSource <> Value then
  begin
    // отключаемся от старого
    if Assigned(FDataSource) then
      FDataSource.Detach(Self);

    // подключаемся к новому
    if Assigned(Value) then
      Value.Attach(Self);

    FDataSource := Value;
  end;
end;

procedure TLADataLink.SetID(const Value: string);
begin
  if FID <> Value then
  begin
    // нужно уведомить
    if Assigned(DataSource) then
    begin
      // удаляем ссылку на линк со старым ID
      DataSource.Detach(Self);
      // добавляем ссылку на линк с новым ID
      FID := Value;
      DataSource.Attach(Self);
    end
    else
      FID := Value;
  end;
end;

procedure TLADataLink.SetNotifyConstantly(const Value: Boolean);
begin
  FNotifyConstantly := Value;
end;

end.
