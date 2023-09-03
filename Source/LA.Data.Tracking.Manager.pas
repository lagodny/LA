unit LA.Data.Tracking.Manager;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections, System.Generics.Defaults,
  SynCrossPlatformJSON,
  LA.Net.Connector.Http,
  LA.Data.Updater,
  LA.Data.Link.Tracker,
  LA.Types.Tracker,
  LA.Data.Tracking.Model;


type
  TLATrackingManager = class(TComponent)
  private
    {$REGION 'Fields'}
    FConnector: TLAHttpTrackingConnection;
    FUpdater: TLADataUpdater;
    FTagPrototypes: TLATagPrototypes;
    FClients: TClients;
    FGroups: TGroups;
    FDevices: TDevices;
    procedure SetConnector(const Value: TLAHttpTrackingConnection);
    procedure SetUpdater(const Value: TLADataUpdater);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    {$ENDREGION}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property TagPrototypes: TLATagPrototypes read FTagPrototypes;

    property Clients: TClients read FClients;
    property Groups: TGroups read FGroups;
    property Devices: TDevices read FDevices;

    procedure InitClients;
    procedure InitDevices;

    class function ExtractExceptionMessage(const aJSON: string): string;

  published
    property Connector: TLAHttpTrackingConnection read FConnector write SetConnector;
    property Updater: TLADataUpdater read FUpdater write SetUpdater;

    property Connected: Boolean read GetConnected write SetConnected;
  end;

implementation

{ TLATrackerManager }

constructor TLATrackingManager.Create(AOwner: TComponent);
begin
  inherited;
  FClients := TClients.Create;
  FGroups := TGroups.Create;
  FDevices := TDevices.Create([doOwnsValues]);
  FTagPrototypes := TLATagPrototypes.Create([doOwnsValues]);
end;

destructor TLATrackingManager.Destroy;
begin
  FTagPrototypes.Free;
  FDevices.Free;
  FGroups.Free;
  FClients.Free;
  inherited;
end;

class function TLATrackingManager.ExtractExceptionMessage(const aJSON: string): string;
var
  v: TJSONVariantData;
  arr: TArray<string>;
begin
  var p := Pos(#13#10, aJSON);
  if p = 0 then
    Result := aJSON
  else
  begin

    v.Init(Copy(aJSON, p + 2, Length(aJSON)));
    v.Init(v.Value['error']);
    v.Init(v.Value['Exception']);
    Result := v.Value['Exception'];
  end;
end;

function TLATrackingManager.GetConnected: Boolean;
begin
  Result := Connector.Connected;
end;

procedure TLATrackingManager.InitClients;
var
  v: TJSONVariantData;
  aClient: TClient;
begin
  Assert(Assigned(Connector), 'Connector not specified');
  // [{"id":174,"name":"Demo","group":0}]
  v.Init(Connector.GetClients);
  Clients.Clear;
  for var i := 0 to v.Count - 1 do
  begin
    aClient := TClient.Create;
    aClient.ID := v.Item[i].id;
    aClient.Name := v.Item[i].name;
    Clients.Add(aClient);
  end;
end;

procedure TLATrackingManager.InitDevices;
var
  i: Integer;
  v, p, t: TJSONVariantData;
  aClient: TClient;
  aProto: TLATagPrototype;
  aDevice: TDevice;
  //aTracker: TLATrackerLink;
begin
  Assert(Assigned(Connector), 'Connector not specified');
  // [{"id":174,"name":"Demo","group":0}]
  v.Init(Connector.GetDevices([]));

//  TagPrototypes.Clear;
//  p.Init(v.Value['prototypes']);
//  for i := 0 to p.Count - 1 do
//  begin
//    aProto := TLATagPrototype.Create;
//    aProto.Init(p.Values[i]);
//    TagPrototypes.Add(aProto.ID, aProto);
//  end;

  // прототипы не очищаем, а только добавляем, если такого еще нет (на них возможны ссылки линков)
  p.Init(v.Value['prototypes']);
  for i := 0 to p.Count - 1 do
  begin
    if not TagPrototypes.TryGetValue(p.Values[i].id, aProto) then
    begin
      aProto := TLATagPrototype.Create;
      aProto.Init(p.Values[i]);
      TagPrototypes.Add(aProto.ID, aProto);
    end;
  end;

  Devices.Clear;
  t.Init(v.Value['devices']);
  for i := 0 to t.Count - 1 do
  begin
    aDevice := TDevice.Create;
    aDevice.Init(t.Values[i], TagPrototypes);
    aDevice.Link.DataSource := Updater;
    Devices.Add(aDevice.ID, aDevice)
  end;
end;

procedure TLATrackingManager.SetConnected(const Value: Boolean);
begin
  if not Value then
    Updater.Active := False;
  Connector.Connected := Value;
end;

procedure TLATrackingManager.SetConnector(const Value: TLAHttpTrackingConnection);
begin
  FConnector := Value;
end;

procedure TLATrackingManager.SetUpdater(const Value: TLADataUpdater);
begin
  FUpdater := Value;
end;

end.
