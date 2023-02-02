unit LA.Test.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Data.Bind.GenData, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListView, Data.Bind.Controls, FMX.Layouts, Fmx.Bind.Navigator,
  LA.Data.Source, LA.Data.Updater, LA.Net.Connector, LA.Net.Connector.Http, LA.Data.Sensor, Fmx.Bind.GenData;

type
  TForm5 = class(TForm)
    PrototypeBindSource1: TPrototypeBindSource;
    ListView1: TListView;
    EditValue: TEdit;
    LabelValue: TLabel;
    LinkControlToFieldValue: TLinkControlToField;
    BindingsList1: TBindingsList;
    EditStatus: TEdit;
    LabelStatus: TLabel;
    LinkControlToFieldStatus: TLinkControlToField;
    LabelMoment: TLabel;
    LabelMoment2: TLabel;
    LinkControlToFieldMoment: TLinkControlToField;
    NavigatorPrototypeBindSource1: TBindNavigator;
    DCHttpConnector1: TLAHttpConnector;
    DataUpdater1: TLADataUpdater;
    LASensorList1: TLASensorList;
    LASensor1: TLASensor;
    LASensor2: TLASensor;
    LASensor3: TLASensor;
    chActive: TCheckBox;
    LinkControlToPropertyActive: TLinkControlToProperty;
    Button1: TButton;
    LinkListControlToField1: TLinkListControlToField;
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
    procedure DataUpdater1Update(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DoOnIdle(Sender: TObject; var Done: Boolean);
  public
    FIsIdle: Boolean;
    FNeedRefresh: Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure InitListView;
    procedure RefreshListView;
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.iPhone47in.fmx IOS}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TForm5.Button1Click(Sender: TObject);
var
  aSensor: TLASensor;
begin
//  for var i := 30328 to 30328 + 20 do
  for var i := 1 to 1 + 2000 do
  begin
    aSensor := TLASensor.Create(Self);
    aSensor.ID := i.ToString;
    aSensor.Name := 'Sensor' + i.ToString;
    LASensorList1.AddSensor(aSensor);
  end;
//  PrototypeBindSource1.Refresh;
InitListView;
end;

constructor TForm5.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TForm5.DataUpdater1Update(Sender: TObject);
begin
//  (PrototypeBindSource1.InternalAdapter as TListBindSourceAdapter<TLASensor>).Current
//  if FIsIdle then
//  FNeedRefresh := True;
//    PrototypeBindSource1.Refresh;
//  PrototypeBindSource1.Refresh;
  RefreshListView;
end;

procedure TForm5.DoOnIdle(Sender: TObject; var Done: Boolean);
begin
  if FNeedRefresh then
  begin
    //PrototypeBindSource1.Refresh;
    FNeedRefresh := False;
  end;

  Done := True;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  Application.OnIdle := DoOnIdle;
  InitListView;
end;

procedure TForm5.InitListView;
begin
  ListView1.BeginUpdate;
  try
    ListView1.Items.Clear;
    for var i := 0 to LASensorList1.Sensors.Count - 1 do
    begin
      ListView1.Items.Add; // Items[i].Text := LASensorList1.Senosrs[i].Data;
      Listview1.Items.AppearanceItem[i].Data['Name'] := LASensorList1.Sensors[i].Name;
      Listview1.Items.AppearanceItem[i].Data['Value'] := LASensorList1.Sensors[i].Value;
    end;
  finally
    ListView1.EndUpdate;
  end;
end;

procedure TForm5.PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
begin
  //
  ABindSourceAdapter := TListBindSourceAdapter<TLASensor>.Create(Self,LASensorList1.Sensors, False);
  //ABindSourceAdapter.Refresh;
end;

procedure TForm5.RefreshListView;
begin
  ListView1.BeginUpdate;
  try
    for var i := 0 to ListView1.Items.Count - 1 do
    begin
      Listview1.Items.AppearanceItem[i].Data['Value'] := LASensorList1.Sensors[i].Value;
    end;
  finally
    ListView1.EndUpdate;
  end;
end;

end.
