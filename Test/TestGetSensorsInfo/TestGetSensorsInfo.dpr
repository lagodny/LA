program TestGetSensorsInfo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  Unit1 in 'Unit1.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
