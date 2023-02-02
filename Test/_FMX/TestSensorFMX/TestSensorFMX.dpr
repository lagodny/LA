program TestSensorFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Test.Main in 'Test.Main.pas' {MainForm},
  Test.Child1 in 'Test.Child1.pas' {ChildForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
