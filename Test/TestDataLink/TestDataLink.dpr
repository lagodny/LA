program TestDataLink;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  LA.Data.Classes in '..\..\Source\LA.Data.Classes.pas',
  LA.Threads in '..\..\Source\LA.Threads.pas',
  LA.Data.Sensor in '..\..\Source\LA.Data.Sensor.pas',
  LA.Data.Link in '..\..\Source\LA.Data.Link.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
