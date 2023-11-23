program TestViewFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Tess.FMX.View in 'Tess.FMX.View.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
