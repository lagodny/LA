unit Test.SensorList.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, LA.Data.Sensor, Data.Bind.EngExt, Vcl.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Data.Bind.Components, Vcl.StdCtrls, LA.Data.Source, LA.Data.Updater, LA.Net.Connector, LA.Net.Connector.Http,
  Vcl.Bind.Editors;

type
  TForm2 = class(TForm)
    LASensorList1: TLASensorList;
    LASensor1: TLASensor;
    Label1: TLabel;
    BindingsList1: TBindingsList;
    LinkControlToPropertyCaption: TLinkControlToProperty;
    DCHttpConnector1: TDCHttpConnector;
    DataUpdater1: TDataUpdater;
    CheckBox1: TCheckBox;
    LinkControlToPropertyActive: TLinkControlToProperty;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  LASensorList1.DataSource := DataUpdater1;
end;

end.
