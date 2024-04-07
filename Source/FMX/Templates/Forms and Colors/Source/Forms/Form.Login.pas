unit Form.Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation,
  FMX.Edit, Frame.LongRunButton, FMX.Layouts,
  FMX.Platform, FMX.VirtualKeyboard,
  iPub.FMX.SystemBars,
  DKLang, Form.Base;

type
  TLoginForm = class(TBaseForm)
    laContent: TLayout;
    ePassword: TEdit;
    LockImage: TImage;
    eLogin: TEdit;
    UserImage: TImage;
    laFooter: TLayout;
    laLogo: TLayout;
    LogoBackgroundCircle: TCircle;
    LogoImage: TImage;
    lng: TDKLanguageController;
    Text1: TText;
    Layout1: TLayout;
    paBackground: TPanel;
    bAuthenticate: TLongRunButtonFrame;
    tNoAccount: TText;
    tSignUp: TText;
    sbBack: TSpeedButton;
    Layout2: TLayout;
    chRememberMe: TCheckBox;
    procedure ePasswordKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure bAuthenticateButton1Click(Sender: TObject);
    procedure Text2Click(Sender: TObject);
    procedure Text1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure LogoBackgroundCircleClick(Sender: TObject);
    procedure tSignUpClick(Sender: TObject);
    procedure sbBackClick(Sender: TObject);
  private
    procedure StartWithoutLogin;
    procedure Login;
  public
    constructor Create(AOwner: TComponent); override;

//    function Back: Boolean; override;

    procedure OrientationChanged(const ASender: TObject; const AMessage: TMessage); override;
  end;

implementation

uses
  NX.Horizon,
  UI.Utils,
  App.Events,
  Data.Settings,
  DM.DataController,
  App.State.Controller, App.State,
  DM.Common;


{$R *.fmx}


procedure TLoginForm.bAuthenticateButton1Click(Sender: TObject);
begin
  Login;
end;

procedure TLoginForm.Button1Click(Sender: TObject);
begin
  StartWithoutLogin;
end;

constructor TLoginForm.Create(AOwner: TComponent);
begin
  inherited;
  chRememberMe.IsChecked := Settings.AutoLogon;
  eLogin.Text := Settings.UserName;
  if eLogin.Text <> '' then
    ePassword.Text := Settings.Password
  else
    ePassword.Text := '';
end;

procedure TLoginForm.ePasswordKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
    Login;
end;

procedure TLoginForm.FrameClick(Sender: TObject);
begin
  HideKeyboard;
end;

procedure TLoginForm.Login;
begin

//  Enabled := False;

  bAuthenticate.RunAsync(
    procedure
    begin
      DataController.Manager.Connected := False;
      DataController.Manager.UserName := eLogin.Text;
      DataController.Manager.Password := ePassword.Text;

      //DataController.Manager.Connector.Address := Settings.Addr;
      if (DataController.Manager.Items.Count > 0) and Assigned(DataController.Manager.Items[0].Connector) then
        DataController.Manager.Items[0].Connector.Address := Settings.Addr;

      DataController.Manager.Connected := True;
    end,
    procedure
    begin
      Settings.UserName := eLogin.Text;
      Settings.Password := ePassword.Text;
      Settings.AutoLogon := chRememberMe.IsChecked;
      Settings.DoChange;
      Settings.SaveSettings;

      bAuthenticate.HideIndicator;
///      Enabled := True;

      DataController.Manager.Active := True;
      AppStateController.AppState := asHome;
    end,
    procedure (const aMsg: string)
    begin
      bAuthenticate.HideIndicator;
//      Enabled := True;

      ShowMessage(aMsg);
    end
    );
end;

procedure TLoginForm.LogoBackgroundCircleClick(Sender: TObject);
begin
  HideKeyboard;
end;

procedure TLoginForm.OrientationChanged(const ASender: TObject; const AMessage: TMessage);
begin
  inherited;
  //Exit;

  var ScreenService: IFMXScreenService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenService)) then
  begin
    if ScreenService.GetScreenOrientation in [TScreenOrientation.Portrait, TScreenOrientation.InvertedPortrait] then
      laLogo.Visible := True
    else
      laLogo.Visible := False;
  end;
end;

procedure TLoginForm.sbBackClick(Sender: TObject);
begin
  AppStateController.AppState := asSettings;
end;

procedure TLoginForm.StartWithoutLogin;
begin
  Settings.UserName := '';
  Settings.Password := '';
  Settings.DoChange;
  Settings.SaveSettings;
  AppStateController.AppState := asHome;
end;

procedure TLoginForm.Text1Click(Sender: TObject);
begin
  OpenURL('https://scs.co.ua/#contact');
end;

procedure TLoginForm.Text2Click(Sender: TObject);
begin
  StartWithoutLogin;
end;

procedure TLoginForm.tSignUpClick(Sender: TObject);
begin
//  AppStateController.AppState := asSignUp;
end;

end.
