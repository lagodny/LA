program LAProjectTemplate;

uses
  System.StartUpCopy,
  FMX.Forms,
  DM.DataController in 'Source\DM\DM.DataController.pas' {DataController: TDataModule},
  App.Events in 'Source\App\App.Events.pas',
  App.State in 'Source\App\App.State.pas',
  App.State.Controller in 'Source\App\App.State.Controller.pas',
  App.Forms.Main in 'Source\App.Forms.Main.pas' {MainForm},
  DM.Common in 'Source\DM\DM.Common.pas' {DMCommon: TDataModule},
  Data.Settings in 'Source\Data\Data.Settings.pas',
  Frame.Base in 'Source\Frames\Custom\Frame.Base.pas' {BaseFrame: TFrame},
  Frame.Toolbar in 'Source\Frames\Custom\Frame.Toolbar.pas' {ToolbarFrame: TFrame},
  Frame.Settings in 'Source\Frames\Frame.Settings.pas' {SettingsFrame: TFrame},
  Frame.LongRunButton in 'Source\Frames\Lib\Frame.LongRunButton.pas' {LongRunButtonFrame: TFrame},
  Frame.Login in 'Source\Frames\Frame.Login.pas',
  Frame.Home in 'Source\Frames\Frame.Home.pas' {HomeFrame: TFrame},
  App.Consts in 'Source\App\App.Consts.pas',
  Form.Test in 'Source\Forms\Form.Test.pas' {TestForm};

{$R *.res}
{$R *.dkl_const.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
