unit LA.Data.Tracker.Updater;

interface

uses
  System.SysUtils, System.Variants,
  LA.Types.Monitoring,
  LA.Net.Connector.Utils,
  LA.Data.Source,
  LA.Data.Updater;

type
  TLATrackerUpdater = class(TLADataUpdater)
  protected
    function GetDataFromServer(const IDs: TSIDArr): string; override;
    procedure ProcessServerResponse(const aResponse: string); override;
  public
    procedure Attach(const aLink: TLADataLink); override;
  end;


implementation

uses
  SynCrossPlatformJSON,
  LA.Net.Connector.Http,
  LA.Data.Link.Tracker;


{ TLATrackerUpdater }

procedure TLATrackerUpdater.Attach(const aLink: TLADataLink);
begin
  // можем работать только с Трекерами
  if not (aLink is TLATrackerLink) then
    raise Exception.Create(sIncorrectDataLinkType);

  inherited;
end;

function TLATrackerUpdater.GetDataFromServer(const IDs: TSIDArr): string;
begin
  Result := (Connector as TLAHttpTrackingConnection).GetDevicesData(TLANetConnectorUtils.SIDArr2IDDynArr(IDs));
end;

{$REGION 'Sample to process'}
  (*
  {
    "_3475": {
      "id": 3475,
      "status": "no data",
      "tags": {
        "Height": "105",
        "Lon": "105",
        "Ignition": "105",
        "Curs": "105",
        "Route": "105",
        "Location": "105",
        "ExternalVoltage": "105",
        "Distance": "105",
        "Speed": "105",
        "Lat": "105"
      }
    },
    "_3476": {
      "id": 3476,
      "status": "no data",
      "tags": {
        "Height": "30.2116966",
        "Lon": "30.2116966",
        "Ignition": "30.2116966",
        "Curs": "30.2116966",
        "ExternalVoltage": "30.2116966",
        "Distance": "30.2116966",
        "DistanceRace": "30.2116966",
        "DistanceTO": "30.2116966",
        "Speed": "30.2116966",
        "Lat": "30.2116966"
      }
    },
    "_3477": {
      "id": 3477,
      "status": "no data",
      "tags": {
        "EngineTemperature": "0",
        "EngineRPM": "0",
        "FuelLevelPercentage": "0",
        "DoorStatus": "0",
        "ExternalVoltage": "0",
        "FuelCounter": "0",
        "Height": "0",
        "Lon": "0",
        "Ignition": "0",
        "Curs": "0",
        "Route": "0",
        "Distance": "0",
        "DistanceRace": "0",
        "DistanceTO": "0",
        "Speed": "0",
        "Lat": "0"
      }
    }
  }
  *)
{$ENDREGION}
procedure TLATrackerUpdater.ProcessServerResponse(const aResponse: string);
var
  vd: TJSONVariantData;
  v: Variant;
begin
  vd.Init(aResponse);

  FLock.BeginRead;
  try
    for var i := 0 to Links.Count - 1 do
    begin
      v := vd.Value[TLANetConnectorUtils.ID2Ident(Links[i].ID)];
      if VarIsEmpty(v) then
        Continue;

      Links[i].Data := v;
    end;
  finally
    FLock.EndRead;
  end;
end;

end.
