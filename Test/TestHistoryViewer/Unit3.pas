unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, LA.Data.History.Viewer, LA.Net.Connector.Tcp, LA.Net.Connector,
  LA.Net.Connector.Http, LA.Data.Source, LA.Data.Updater, LA.Data.Sensor.Updater, FMX.Controls.Presentation, FMX.StdCtrls, LA.FMX.StdCtrls,
  FMX.Objects, LA.Data.Connection.Manager, FMX.Layouts;

type
  TForm3 = class(TForm)
    HTTPSensorUpdater: TLASensorUpdater;
    LAHttpConnector1: TLAHttpConnector;
    LATCPConnector1: TLATCPConnector;
    TCPSensorUpdater: TLASensorUpdater;
    LAHistoryViewer1: TLAHistoryViewer;
    LAConnectionManager1: TLAConnectionManager;
    LALabel3: TLALabel;
    LALabel1: TLALabel;
    Text1: TText;
    Text2: TText;
    LALabel2: TLALabel;
    LALabel4: TLALabel;
    TrackBar1: TTrackBar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    LALabel5: TLALabel;
    LALabel6: TLALabel;
    procedure TrackBar1Tracking(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
  begin
    LAHistoryViewer1.ConnectLinksFromDataSource(HTTPSensorUpdater);
    LAHistoryViewer1.ConnectLinksFromDataSource(TCPSensorUpdater);
    LAHistoryViewer1.Active := True;

    TrackBar1.Max := LAHistoryViewer1.Date2;
    TrackBar1.Min := LAHistoryViewer1.Date1;
    TrackBar1.Value := TrackBar1.Min;
  end
  else
  begin
    LAHistoryViewer1.Active := False;
    LAHistoryViewer1.DisconnectLinks;
  end;
end;

procedure TForm3.CheckBox2Change(Sender: TObject);
begin
  HTTPSensorUpdater.Active := CheckBox2.IsChecked;
  TCPSensorUpdater.Active := CheckBox2.IsChecked;
  if CheckBox2.IsChecked then
  begin
    HTTPSensorUpdater.InitItems;
    TCPSensorUpdater.InitItems;
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  LAHTTPConnector1.Address := 'https://dc.tdc.org.ua';
  LATCPConnector1.Address := 'tdc.org.ua:5152';
end;

procedure TForm3.TrackBar1Tracking(Sender: TObject);
begin
  LAHistoryViewer1.CurrentMoment := TrackBar1.Value;
end;

end.
