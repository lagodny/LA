unit Test.Child1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  LA.Data.Sensor, LA.Data.Source, LA.Data.Updater, LA.Net.Connector,
  LA.Net.Connector.Http, FMX.Objects, LA.FMX.Objects, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Ani, LA.FMX.StdCtrls, LA.Data.Sensor.Updater;

type
  TChildForm = class(TForm)
    Text1: TText;
    DCHttpConnector1: TLAHttpConnector;
    LASensorList1: TLASensorList;
    LASensor1: TLASensor;
    CheckBox1: TCheckBox;
    BindingsList1: TBindingsList;
    LinkControlToPropertyText: TLinkControlToProperty;
    LAText1: TLAText;
    LAText2: TLAText;
    LAText3: TLAText;
    Label1: TLabel;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Label2: TLabel;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    LAText4: TLAText;
    Rectangle5: TRectangle;
    LAText5: TLAText;
    Rectangle6: TRectangle;
    LAText6: TLAText;
    BitmapAnimation1: TBitmapAnimation;
    LALabel1: TLALabel;
    StyleBook1: TStyleBook;
    Button1: TButton;
    LASensorUpdater1: TLASensorUpdater;
    LinkControlToPropertyActive: TLinkControlToProperty;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LAText5LinkDataChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TChildForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TChildForm.FormCreate(Sender: TObject);
begin
  //TButton.Create(nil)
end;

procedure TChildForm.LAText5LinkDataChange(Sender: TObject);
begin
  Caption := LAText5.Link.Text + '.';
end;

end.
