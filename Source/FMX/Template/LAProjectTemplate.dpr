program LAProjectTemplate;

uses
  System.StartUpCopy,
  FMX.Forms,
  LA.Project.Form.Main in 'LA.Project.Form.Main.pas' {Main},
  LA.Project.DM.Data in 'LA.Project.DM.Data.pas' {DMData: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
