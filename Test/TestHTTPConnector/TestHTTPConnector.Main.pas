unit TestHTTPConnector.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo,
  LA.DC.HTTPConnector, FMX.Edit;

type
  TMainForm = class(TForm)
    mLog: TMemo;
    Layout1: TLayout;
    bRun: TButton;
    bCreate: TButton;
    bConnect: TButton;
    bDisconnect: TButton;
    bDestroy: TButton;
    Edit1: TEdit;
    bSensorValue: TButton;
    Edit2: TEdit;
    bGroupSensorValue: TButton;
    procedure bCreateClick(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure bDisconnectClick(Sender: TObject);
    procedure bDestroyClick(Sender: TObject);
    procedure bSensorValueClick(Sender: TObject);
    procedure bGroupSensorValueClick(Sender: TObject);
  private
    FConnector: TDCHTTPConnector;
    procedure AddToLog(const aMessage: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  LA.DC.mORMotClient;

{$R *.fmx}

procedure TMainForm.AddToLog(const aMessage: string);
begin
  mLog.Lines.Add(FormatDateTime('hh:mm:ss:zzz', Now) + Chr(vkTab) + aMessage);
end;

procedure TMainForm.bConnectClick(Sender: TObject);
begin
  AddToLog('Connect...');
  FConnector.Connect;
  AddToLog('Connected');
  AddToLog(FConnector.Address);
end;

procedure TMainForm.bCreateClick(Sender: TObject);
begin
  FConnector := TDCHTTPConnector.Create(Self);
  FConnector.UserName := 'Лагодный';
  FConnector.Password := '314';
  FConnector.Address := 'dc.tdc.org.ua:80';
end;

procedure TMainForm.bDestroyClick(Sender: TObject);
begin
  FConnector.Free;
end;

procedure TMainForm.bDisconnectClick(Sender: TObject);
begin
  FConnector.Disconnect;
end;

procedure TMainForm.bGroupSensorValueClick(Sender: TObject);
var
  r: TValArr;
  i: Integer;
begin
  r := FConnector.GroupSensorValueByID([1,2,3,4]);
  for i := Low(r) to High(r) do
    AddToLog(r[i]);
end;

procedure TMainForm.bSensorValueClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 10 do
    AddToLog(FConnector.SensorValue(Edit1.Text));
end;

end.
