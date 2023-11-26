unit LA.Data.Connection.Manager;

interface

uses
  System.Classes,
  System.Generics.Defaults, System.Generics.Collections,
  LA.Data.Sensor.Updater,
  LA.Net.Connector,
  LA.Data.History.Viewer,
  LA.Data.Lookup;

type
  /// подключение к серверу Мониторинра
  TLAConnectionItem = class(TCollectionItem)
  private
    FName: string;
    FConnector: TLACustomConnector;
    FUpdater: TLASensorUpdater;
    FLookups: TObjectDictionary<string, TLALookupList>;
    procedure SetConnector(const Value: TLACustomConnector);
    procedure SetUpdater(const Value: TLASensorUpdater);
    procedure SetName(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    /// получить текстовое представление значения датчика
    function Lookup(const aKey, aTableName: string): string;
    /// источники справочной информации создаются и заполняются автоматически при необходимости
    property Lookups: TObjectDictionary<string, TLALookupList> read FLookups;
  published
    property Name: string read FName write SetName;

    property Connector: TLACustomConnector read FConnector write SetConnector;
    property Updater: TLASensorUpdater read FUpdater write SetUpdater;
  end;

  /// коллекция подключений
  TLAConnectionCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TLAConnectionItem;
    procedure SetItem(Index: Integer; const Value: TLAConnectionItem);
  public
    property Items[Index: Integer]: TLAConnectionItem read GetItem write SetItem; default;
  end;

  /// Глобальный объект управления доступом к серверам Мониторинга
  ///  должен быть в единственном экземпляре
  ///  содержит коллекцию подключений, каждое подключение должно иметь уникальное наименование и содержать ссылки на Коннектор и Апдейтер
  ///  Реализует доступ к Справочной информации (Lookup) для Датчиков без необходимости явного указания LookupList-ов
  TLAConnectionManager = class(TComponent)
  private
    class var
      FDefaultConnectionManager: TLAConnectionManager;
  private
    FItems: TLAConnectionCollection;
    FHistoryViewer: TLAHistoryViewer;
    procedure SetItems(const Value: TLAConnectionCollection);
    procedure SetHistoryViewer(const Value: TLAHistoryViewer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class constructor Create;
    /// в проекте должен быть всего один экземпляр
    class property DefInstance: TLAConnectionManager read FDefaultConnectionManager;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;

    function IndexOf(const aName: string): Integer;
    function ConnectionByName(const aName: string): TLAConnectionItem;
    function ConnectionByUpdater(const aUpdater: TLASensorUpdater): TLAConnectionItem;

    function Lookup(const aKey, aTableName: string; aUpdater: TLASensorUpdater): string;
  published
    property Items: TLAConnectionCollection read FItems write SetItems;
    property HistoryViewer: TLAHistoryViewer read FHistoryViewer write SetHistoryViewer;
  end;



implementation

uses
  System.SysUtils;

{ TLAConnectionManager }

function TLAConnectionManager.ConnectionByName(const aName: string): TLAConnectionItem;
begin
  Result := nil;
  for var i := 0 to Items.Count - 1 do
    if SameText(aName, Items[i].Name) then
      Exit(Items[i]);
end;

function TLAConnectionManager.ConnectionByUpdater(const aUpdater: TLASensorUpdater): TLAConnectionItem;
begin
  Result := nil;
  for var i := 0 to Items.Count - 1 do
    if Items[i].Updater = aUpdater then
      Exit(Items[i]);
end;

class constructor TLAConnectionManager.Create;
begin
  FDefaultConnectionManager := nil;
end;

constructor TLAConnectionManager.Create(AOnwer: TComponent);
begin
  inherited;
  FDefaultConnectionManager := Self;
  FItems := TLAConnectionCollection.Create(Self, TLAConnectionItem);
end;

destructor TLAConnectionManager.Destroy;
begin
  FItems.Free;
  inherited;
  FDefaultConnectionManager := nil;
end;

function TLAConnectionManager.IndexOf(const aName: string): Integer;
begin
  Result := -1;
  for var i := 0 to Items.Count - 1 do
    if SameText(aName, Items[i].Name) then
      Exit(i);
end;


function TLAConnectionManager.Lookup(const aKey, aTableName: string; aUpdater: TLASensorUpdater): string;
var
  aConnection: TLAConnectionItem;
begin
  aConnection := ConnectionByUpdater(aUpdater);
  if Assigned(aConnection) then
    Result := aConnection.Lookup(aKey, aTableName)
  else
    Result := aKey;
end;

procedure TLAConnectionManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = HistoryViewer then
      HistoryViewer := nil

    else if (AComponent is TLACustomConnector) then
    begin
      for var i := 0 to Items.Count - 1 do
        if Items[i].Connector = AComponent then
          Items[i].Connector := nil;
    end

    else if (AComponent is TLASensorUpdater) then
    begin
      for var i := 0 to Items.Count - 1 do
        if Items[i].Updater = AComponent then
          Items[i].Updater := nil;
    end;
  end;
end;

procedure TLAConnectionManager.SetHistoryViewer(const Value: TLAHistoryViewer);
begin
  FHistoryViewer := Value;
end;

procedure TLAConnectionManager.SetItems(const Value: TLAConnectionCollection);
begin
  FItems.Assign(Value);
end;

{ TLAConnectionCollection }

function TLAConnectionCollection.GetItem(Index: Integer): TLAConnectionItem;
begin
  Result := TLAConnectionItem(inherited Items[Index]);
end;

procedure TLAConnectionCollection.SetItem(Index: Integer; const Value: TLAConnectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TLAConnectionItem }

procedure TLAConnectionItem.AssignTo(Dest: TPersistent);
var
  aDest: TLAConnectionItem;
begin
  if Dest is TLAConnectionItem then
  begin
    aDest := TLAConnectionItem(Dest);
    aDest.Name := Name;
    aDest.Connector := Connector;
    aDest.Updater := Updater;

    // нужно ли создавать?
    aDest.Lookups.Clear;
    for var aLookup in Lookups.Values do
    begin
      var aNewLookup := TLALookupList.Create(nil);
      aNewLookup.TableName := aLookup.TableName;
      aDest.Lookups.Add(aNewLookup.TableName, aNewLookup);
    end;
  end
  else
    inherited;
end;

constructor TLAConnectionItem.Create(Collection: TCollection);
begin
  inherited;
  FLookups := TObjectDictionary<string, TLALookupList>.Create([doOwnsValues]);
end;

destructor TLAConnectionItem.Destroy;
begin
  FLookups.Free;
  inherited;
end;

function TLAConnectionItem.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else if Assigned(Connector) then
    Result := Connector.Address
  else
    Result := Format('Connection%d', [ID]);
end;

function TLAConnectionItem.Lookup(const aKey, aTableName: string): string;
var
  aLookup: TLALookupList;
begin
  // LookupList будет создан, если такого еще нет
  if not FLookups.TryGetValue(aTableName, aLookup) then
  begin
    aLookup := TLALookupList.Create(nil);
    aLookup.TableName := aTableName;
    FLookups.Add(aTableName, aLookup);
  end;
  // если найти не получится, вернем ключ
  if aLookup.Lookup(aKey, Result, Connector) < 0 then
    Result := aKey;
end;

procedure TLAConnectionItem.SetConnector(const Value: TLACustomConnector);
begin
  if FConnector <> Value then
  begin
    FConnector := Value;
    Changed(False);
  end;
end;

procedure TLAConnectionItem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    Changed(False);
  end;
end;

procedure TLAConnectionItem.SetUpdater(const Value: TLASensorUpdater);
begin
  if FUpdater <> Value then
  begin
    FUpdater := Value;
    Changed(False);
  end;
end;

end.
