unit DM.DataController;

interface

uses
  System.SysUtils, System.Classes, LA.Data.History.Viewer, LA.Net.Connector, LA.Net.Connector.Tcp, LA.Data.Source, LA.Data.Updater,
  LA.Data.Sensor.Updater, LA.Data.Connection.Manager;

type
  TDataController = class(TDataModule)
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
  DataController: TDataController;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
