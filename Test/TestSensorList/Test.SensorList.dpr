program Test.SensorList;

uses
  Vcl.Forms,
  Test.SensorList.Main in 'Test.SensorList.Main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
