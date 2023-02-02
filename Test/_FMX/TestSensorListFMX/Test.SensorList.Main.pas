unit Test.SensorList.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  LA.Data.Source, LA.Data.Updater, LA.Net.Connector, LA.Net.Connector.Http,
  LA.Data.Sensor, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Data.Bind.Components, FMX.Objects,
  Fmx.Bind.Editors, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm3 = class(TForm)
    LASensorList1: TLASensorList;
    DCHttpConnector1: TLAHttpConnector;
    DataUpdater1: TLADataUpdater;
    LASensor1: TLASensor;
    LASensor2: TLASensor;
    LASensor3: TLASensor;
    Text1: TText;
    BindingsList1: TBindingsList;
    Text2: TText;
    Text3: TText;
    LinkControlToPropertyText3: TLinkControlToProperty;
    CheckBox1: TCheckBox;
    LinkControlToPropertyActive: TLinkControlToProperty;
    LinkControlToPropertyText: TLinkControlToProperty;
    LinkControlToPropertyText2: TLinkControlToProperty;
    LASensor4: TLASensor;
    LASensor5: TLASensor;
    LASensor6: TLASensor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

end.
