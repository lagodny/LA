program LAStyle;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  LA.Style.Main in 'LA.Style.Main.pas' {Form9},
  LA.Style.Second in 'LA.Style.Second.pas' {Form10},
  LA.Style.Frame in 'LA.Style.Frame.pas' {TestFrame: TFrame},
  LA.Style.Frame2 in 'LA.Style.Frame2.pas' {Frame2: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
