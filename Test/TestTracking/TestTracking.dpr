program TestTracking;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  uDM in 'uDM.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
