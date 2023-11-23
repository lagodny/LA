unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Edit, LA.Net.Connector.Tcp, LA.Net.Connector, LA.Net.Connector.Http, LA.Types.Monitoring;

type
  TForm4 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    LAHttpConnector1: TLAHttpConnector;
    LATCPConnector1: TLATCPConnector;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

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

end.
