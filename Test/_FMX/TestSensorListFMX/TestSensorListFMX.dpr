program TestSensorListFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Test.SensorList.Main in 'Test.SensorList.Main.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
