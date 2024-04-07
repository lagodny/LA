unit DM.Data;

interface

uses
  System.SysUtils, System.Classes, LA.Data.History.Viewer, LA.Net.Connector, LA.Net.Connector.Tcp, LA.Data.Source, LA.Data.Updater,
  LA.Data.Sensor.Updater, LA.Data.Connection.Manager;

type
  TDMData = class(TDataModule)
    Updater: TLASensorUpdater;
    Connector: TLATCPConnector;
    HisroryViewer: TLAHistoryViewer;
    Manager: TLAConnectionManager;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMData: TDMData;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
