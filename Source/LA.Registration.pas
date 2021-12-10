unit LA.Registration;

interface

procedure Register;

implementation

uses
  System.Classes,
  Data.Bind.Components,
  LA.Data.Sensor,
  LA.Net.Connector.Http,
  LA.Data.Updater;

procedure Register;
begin
  RegisterComponents('LA', [TLASensorList, TLASensor, TDCHttpConnector, TDataUpdater]);
  RegisterClass(TLASensorList);
  RegisterClass(TLASensor);
  RegisterNoIcon([TLASensor]);
end;

initialization
  Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TLASensor), 'Value', 'DFM');

finalization
  Data.Bind.Components.UnregisterObservableMember(TArray<TClass>.Create(TLASensor));


end.
