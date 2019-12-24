program TestTCPConnector;

uses
  System.StartUpCopy,
  FMX.Forms,
  TestTCPConnector.Main in 'TestTCPConnector.Main.pas' {MainForm},
  LA.DC.TCPConnector in '..\..\Source\LA.DC.TCPConnector.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
