program MDTheme;

uses
  System.StartUpCopy,
  FMX.Forms,
  MD.Main in 'MD.Main.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
