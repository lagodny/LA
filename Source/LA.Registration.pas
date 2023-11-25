unit LA.Registration;

interface

procedure Register;

implementation

uses
  System.Classes,
  Data.Bind.Components,
  LA.Data.Sensor, LA.Data.Link.Sensor,
  LA.Net.Connector.Http,
  LA.Net.Connector.Tcp,
  LA.Data.Source, LA.Data.Updater,
  LA.Data.Sensor.Updater,
  LA.Data.History.Viewer;

procedure Register;
begin
  RegisterComponents('LA', [
    TLASensorList, TLASensor,
    TLAHttpConnector, TLATCPConnector,
    TLASensorUpdater, TLAHistoryViewer
  ]);

  RegisterClasses([TLADataLink, TLASensorLink]);
  RegisterClasses([TLASensorList, TLASensor]);

  RegisterNoIcon([TLASensor]);
end;

initialization
  Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TLASensor), 'Value', 'DFM');

finalization
  Data.Bind.Components.UnregisterObservableMember(TArray<TClass>.Create(TLASensor));


end.
