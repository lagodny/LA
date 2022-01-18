program TestBindSourceAdapter;

uses
  System.StartUpCopy,
  FMX.Forms,
  LA.Test.Main in 'LA.Test.Main.pas' {Form5},
  FMX.ListView in 'FMX.ListView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
