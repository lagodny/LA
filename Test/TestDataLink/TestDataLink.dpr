program TestDataLink;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  LA.Threads in '..\..\Source\LA.Threads.pas',
  LA.Data.Sensor in '..\..\Source\LA.Data.Sensor.pas',
  LA.Data.Link in '..\..\Source\LA.Data.Link.pas',
  LA.Data.Link.Sensor in '..\..\Source\LA.Data.Link.Sensor.pas',
  LA.Data.Link.Tracker in '..\..\Source\LA.Data.Link.Tracker.pas',
  LA.Data.Tracker in '..\..\Source\LA.Data.Tracker.pas',
  LA.Data.Updater in '..\..\Source\LA.Data.Updater.pas',
  LA.Net.Connector in '..\..\Source\LA.Net.Connector.pas',
  LA.Data.Updater.Intf in '..\..\Source\LA.Data.Updater.Intf.pas',
  LA.Data.Link.Intf in '..\..\Source\LA.Data.Link.Intf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
