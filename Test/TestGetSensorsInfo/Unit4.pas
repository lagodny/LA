unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Edit, LA.Net.Connector.Tcp, LA.Net.Connector, LA.Net.Connector.Http, LA.Types.Monitoring, FMX.Layouts, LA.FMX.StdCtrls, LA.Data.Source,
  LA.Data.Updater, LA.Data.Sensor.Updater, LA.Data.History.Viewer;

type
  TForm4 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    LAHttpConnector1: TLAHttpConnector;
    LATCPConnector1: TLATCPConnector;
    Button2: TButton;
    Layout1: TLayout;
    LASensorUpdater1: TLASensorUpdater;
    LALabel1: TLALabel;
    LALabel2: TLALabel;
    LALabel3: TLALabel;
    LALabel4: TLALabel;
    LALabel5: TLALabel;
    LAHistoryViewer1: TLAHistoryViewer;
    TrackBar1: TTrackBar;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TrackBar1Tracking(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses Unit1;

procedure TForm4.Button1Click(Sender: TObject);
var
  aIDs: TSIDArr;
begin
  var a := Edit1.Text.Split([';']);
  SetLength(aIDs, Length(a));
  for var i := 0 to High(a) do
    aIDs[i] := a[i];

//  Memo1.Lines.Add(LAHttpConnector1.GetSensorsInfo(aIDs));
  Memo1.Lines.Add(LATCPConnector1.GetSensorsInfo(aIDs));


end;

procedure TForm4.Button2Click(Sender: TObject);
begin
//  LASensorUpdater1.Active := true;
  LASensorUpdater1.InitItems;

end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  LAHistoryViewer1.ConnectLinksFromDataSource(LASensorUpdater1);
  LAHistoryViewer1.Active := True;
  TrackBar1.Max := LAHistoryViewer1.Date2;
  TrackBar1.Min := LAHistoryViewer1.Date1;
end;

procedure TForm4.TrackBar1Tracking(Sender: TObject);
begin
  LAHistoryViewer1.CurrentMoment := TrackBar1.Value;
end;

end.
