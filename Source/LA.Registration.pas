unit LA.Registration;

interface

procedure Register;

implementation

uses
  System.Classes,
  Data.Bind.Components,
  LA.Data.Sensor, LA.Data.Link.Sensor,
  LA.Net.Connector.Http,
  LA.Data.Source, LA.Data.Updater,
  LA.Data.Sensor.Updater;

procedure Register;
begin
  RegisterComponents('LA', [TLASensorList, TLASensor, TLAHttpConnector, TLASensorUpdater]);

  RegisterClasses([TLADataLink, TLASensorLink]);
  RegisterClasses([TLASensorList, TLASensor]);

  RegisterNoIcon([TLASensor]);
end;

initialization
  Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TLASensor), 'Value', 'DFM');

finalization
  Data.Bind.Components.UnregisterObservableMember(TArray<TClass>.Create(TLASensor));


end.
