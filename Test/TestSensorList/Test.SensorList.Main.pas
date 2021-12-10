unit Test.SensorList.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, LA.Data.Sensor, Data.Bind.EngExt, Vcl.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Data.Bind.Components;

type
  TForm2 = class(TForm)
    LASensorList1: TLASensorList;
    BindingsList1: TBindingsList;
    BindLink1: TBindLink;
    LASensor1: TLASensor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
