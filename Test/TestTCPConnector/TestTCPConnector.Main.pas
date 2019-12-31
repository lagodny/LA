unit TestTCPConnector.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Layouts, LA.DC.TCPConnector;

type
  TMainForm = class(TForm)
    mLog: TMemo;
    Layout1: TLayout;
    bRun: TButton;
    bCreate: TButton;
    bConnect: TButton;
    bDisconnect: TButton;
    bDestroy: TButton;
    bSensorValue: TButton;
    bGroupSensorValue: TButton;
    procedure bRunClick(Sender: TObject);
    procedure bCreateClick(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure bDisconnectClick(Sender: TObject);
    procedure bDestroyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bSensorValueClick(Sender: TObject);
  private
    FConnector: TDCTCPConnector;
  public
    procedure RunTest;
    procedure AddToLog(const aMessage: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.AddToLog(const aMessage: string);
begin
  mLog.Lines.Add(FormatDateTime('hh:mm:ss:zzz', Now) + Chr(vkTab) + aMessage);
end;

procedure TMainForm.bConnectClick(Sender: TObject);
begin
  AddToLog('Connect');
  FConnector.Connect;
  AddToLog('Connected');
  AddToLog(Format('Address: %s', [FConnector.Address]));
end;

procedure TMainForm.bCreateClick(Sender: TObject);
begin
  FConnector := TDCTCPConnector.Create(nil);
  //FConnector.Address := '';
  //FConnector.Address := 'test;tdc.org.ua:5152';
  FConnector.Address := 'test:5555;tdc.org.ua:5152';
  FConnector.UserName := 'Lagodny';
  FConnector.Password := '314';
  FConnector.Description := 'Test TDCTCPConnector';
  FConnector.Encrypt := True;
  FConnector.CompressionLevel := 9;
end;

procedure TMainForm.bDestroyClick(Sender: TObject);
begin
  FreeAndNil(FConnector);
end;

procedure TMainForm.bDisconnectClick(Sender: TObject);
begin
  FConnector.Disconnect;
end;

procedure TMainForm.bRunClick(Sender: TObject);
begin
  RunTest;
end;

procedure TMainForm.bSensorValueClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to 10 do
    AddToLog(FConnector.SensorValue('1'));

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FConnector.Free;
end;

procedure TMainForm.RunTest;
var
  aConnector: TDCTCPConnector;
begin
  aConnector := TDCTCPConnector.Create(nil);
  try
    aConnector.Address := 'test:5555;tdc.org.ua:5152';
    aConnector.UserName := 'Lagodny';
    aConnector.Password := '314';
    aConnector.Encrypt := True;
    AddToLog('Connect');
    aConnector.Connect;
    AddToLog('Connected');
    AddToLog(Format('ServerVer: %d', [aConnector.ServerVer]));
    aConnector.Disconnect;
  finally
    aConnector.Free;
  end;
end;


end.
