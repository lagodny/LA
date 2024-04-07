program LAProjectTemplate_Forms_Colors;

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
  Form.Base in 'Source\Forms\Custom\Form.Base.pas' {BaseForm: TFrame},
  Form.Toolbar in 'Source\Forms\Custom\Form.Toolbar.pas' {ToolbarForm: TFrame},
  Form.Settings in 'Source\Forms\Form.Settings.pas' {SettingsForm: TFrame},
  Frame.LongRunButton in 'Source\Frames\Lib\Frame.LongRunButton.pas' {LongRunButtonFrame: TFrame},
  Form.Login in 'Source\Forms\Form.Login.pas' {LoginForm},
  Form.Home in 'Source\Forms\Form.Home.pas' {HomeForm: TFrame},
  App.Consts in 'Source\App\App.Consts.pas',
  Form.Test in 'Source\Forms\Test\Form.Test.pas' {TestForm};

{$R *.res}
{$R *.dkl_const.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
