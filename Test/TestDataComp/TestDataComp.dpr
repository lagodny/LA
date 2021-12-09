program TestDataComp;

uses
  Vcl.Forms,
  Test.DataComp.Main in 'Test.DataComp.Main.pas' {DataCompMainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataCompMainForm, DataCompMainForm);
  Application.Run;
end.
