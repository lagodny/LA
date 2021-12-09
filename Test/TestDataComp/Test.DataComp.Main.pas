unit Test.DataComp.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, LA.Data.Sensor, LA.Data.Updater, LA.Net.Connector, LA.Net.Connector.Http,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Data.Bind.Components, Vcl.StdCtrls,
  Vcl.Bind.Editors;

type
  TDataCompMainForm = class(TForm)
    DCHttpConnector1: TDCHttpConnector;
    DataUpdater1: TDataUpdater;
    DCSensor1: TDCSensor;
    Label1: TLabel;
    BindingsList1: TBindingsList;
    Edit1: TEdit;
    bEd2Label: TButton;
    bAddLink: TButton;
    chActive: TCheckBox;
    LinkControlToPropertyActive: TLinkControlToProperty;
    Edit2: TEdit;
    procedure bEd2LabelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure bAddLinkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataCompMainForm: TDataCompMainForm;

implementation

uses
  LA.Data.Link.Sensor;

{$R *.dfm}

procedure TDataCompMainForm.bEd2LabelClick(Sender: TObject);
begin
  DCSensor1.Value := Edit1.Text;
end;

procedure TDataCompMainForm.bAddLinkClick(Sender: TObject);
begin
  DataUpdater1.Attach(TDCSensorLink.Create(DCSensor1));
end;

procedure TDataCompMainForm.Edit1Change(Sender: TObject);
begin
  DCSensor1.Value := Edit1.Text;
end;

end.
