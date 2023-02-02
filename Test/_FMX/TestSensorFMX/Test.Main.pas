unit Test.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  LA.Net.Connector.Http,
  LA.Data.Sensor,
  LA.Data.Source, LA.Data.Sensor.Updater;

type
  TMainForm = class(TForm)
    bCreateObjects: TButton;
    bCreateChild: TButton;
    procedure bCreateObjectsClick(Sender: TObject);
    procedure bCreateChildClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure CreateObjects;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses Test.Child1;

{ TForm4 }

procedure TMainForm.bCreateChildClick(Sender: TObject);
begin
  TChildForm.Create(Self).Show;
end;

procedure TMainForm.bCreateObjectsClick(Sender: TObject);
begin
  CreateObjects;
end;

procedure TMainForm.CreateObjects;
begin
  var aConnection := TLAHttpConnector.Create(Self);
  aConnection.Address := 'https://dc.tdc.org.ua';
  aConnection.UserName := 'demo';
  aConnection.Password := 'demo';

  var aSource := TLASensorUpdater.Create(Self);
  aSource.Connector := aConnection;

  var aSensor := TLASensor.Create(Self);
  aSensor.Link.DataSource := aSource;
  aSensor.Link.ID := '1';

  //aSource.Active := True;

end;

end.
