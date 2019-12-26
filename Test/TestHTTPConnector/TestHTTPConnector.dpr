program TestHTTPConnector;

uses
  System.StartUpCopy,
  FMX.Forms,
  TestHTTPConnector.Main in 'TestHTTPConnector.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
