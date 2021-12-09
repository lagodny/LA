unit LA.Registration;

interface

procedure Register;

implementation

uses
  System.Classes,
  LA.Data.Sensor,
  LA.Net.Connector.Http,
  LA.Data.Updater;

procedure Register;
begin
  RegisterComponents('LA', [TDCSensor, TDCHttpConnector, TDataUpdater]);
end;

end.
