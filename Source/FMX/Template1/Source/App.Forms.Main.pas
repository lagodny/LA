unit App.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ActnList, FMX.Edit, FMX.SearchBox,
  FMX.MultiView, FMX.Platform,
  FMX.ListBox, FMX.Layouts, FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation,
  SubjectStand, FrameStand,
  Grijjy.ErrorReporting,
  NX.Horizon,
  DKLangStorage, DKLang,
  App.Consts, App.Events, FormStand;

type
  TMainForm = class(TForm)
    MainStyleBook: TStyleBook;
    StandsBook: TStyleBook;
    FrameStand1: TFrameStand;
    act: TActionList;
    aHome: TAction;
    aNotifications: TAction;
    aSettings: TAction;
    mvMain: TMultiView;
    loHead: TLayout;
    Rectangle1: TRectangle;
    laAppTitle: TLabel;
    laVersion: TLabel;
    loFooter: TLayout;
    lVersion: TLabel;
    loContent: TLayout;
    loFixed: TLayout;
    bLogout: TCornerButton;
    Line1: TLine;
    loScrolable: TLayout;
    lbMenu: TListBox;
    ListBoxItem2: TListBoxItem;
    bMap: TCornerButton;
    ListBoxItem3: TListBoxItem;
    bNotifications: TCornerButton;
    ListBoxItem5: TListBoxItem;
    bSettings: TCornerButton;
    SearchBox1: TSearchBox;
    VertScrollBox: TVertScrollBox;
    loMainScrolable: TLayout;
    ListBoxItem4: TListBoxItem;
    aTest: TAction;
    CornerButton1: TCornerButton;
    loMainContent: TLayout;
    loMainClient: TLayout;
    aExit: TAction;
    aLogout: TAction;
    lngController: TDKLanguageController;
    lngStorage: TDKLTranslationsStorage;
    aLogin: TAction;
    FormStand1: TFormStand;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure aHomeExecute(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormFocusChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure aExitExecute(Sender: TObject);
    procedure aLogoutExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure lngControllerLanguageChanged(Sender: TObject);
    procedure aLoginExecute(Sender: TObject);
  private
    /// подписываемся на системные события
    FOrientationChangedMessageId: Integer;
    procedure OrientationChanged(const ASender: TObject; const AMessage: TMessage);

    procedure InitMessageHandlers;
    procedure DeinitMessageHandlers;

    procedure HandleExceptionReport(const Sender: TObject; const M: TMessage);
  private
    FEventService: IFMXApplicationEventService;
    function HandleApplicationEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  private
    FBackPressedTime: TDateTime;
    // работа с прокруткой экрана, когда клавиатура закрывает элемент ввода
    FKBBounds: TRectF;
    FNeedOffset: Boolean;
    procedure CalcContentBoundsProc(Sender: TObject; var ContentBounds: TRectF);
    procedure RestorePosition;
    procedure UpdateKBBounds(Sender: TObject);
  private
    // делаем прозрачный статус бар
    procedure SetNoLimit;
    procedure SystemBarsInsetsChange(Sender: TObject);
  private
    // обработчик команд
    FMapViewCommandSubscription: INxEventSubscription;
    procedure OnMainViewCommand(const aEvent: IMainViewCommand);

    // работа с фреймами
    procedure HideAll(const AExceptions: TArray<TClass>);
    function SetupView<T: TForm>(const aCloseExceptions, aHideExceptions: TArray<TClass>; aParent: TFmxObject;
      aAction: TAction; aProc: TProc<TObject> = nil; aEnableMainMV: Boolean = False): TFormInfo<T>;
  private
    // обработка состояния кнопок навигации
    procedure UpdatePressed(aTag: Integer);
    // обработка изменения насторек
    procedure DoSettingsChanged(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Jni.App,
//  Androidapi.Jni.Os,
//  Androidapi.Jni.javatypes,
  Androidapi.Helpers,
//  Androidapi.Jni.GraphicsContentViewText,
  LA.FMX.Android.JNI.PowerManager,
{$ENDIF}
{$IFDEF iOS}
  iOSapi.UIKit,
{$ENDIF}
  iPub.FMX.SystemBars,

  System.Math, System.DateUtils,
  DW.OSLog,
  UI.Toast,
  LA.Version,
  LA.Log,
//  LA.FMX.UI.Toast,
  DM.Common,
  DM.DataController,
  Data.Settings,
  App.State, App.State.Controller,
  Form.Base, Form.Toolbar,
  Form.Login,
  Form.Home,
//  SCS.Frame.Objects, SCS.Frame.Map, SCS.Frame.MainMenu, SCS.Frame.Report,
//  SCS.Frame.Interval, SCS.Frame.ObjectEditor,
  Form.Settings;
//  SCS.Frame.Settings.Sender, SCS.Frame.SignUp, SCS.Frame.AddObject, SCS.Frame.ShareDevice;

{$R *.fmx}

procedure TMainForm.aExitExecute(Sender: TObject);
begin
  {$IFDEF ANDROID}
  TAndroidHelper.Activity.finish;
//  SharedActivity.finish;
  {$ENDIF}
end;

procedure TMainForm.aLoginExecute(Sender: TObject);
begin
  AppStateController.AppState := asLogin;
end;

procedure TMainForm.aLogoutExecute(Sender: TObject);
begin
  DataController.Manager.Connected := False;
  AppStateController.AppState := asLogin;
end;

procedure TMainForm.aHomeExecute(Sender: TObject);
begin
  mvMain.HideMaster;
  AppStateController.AppState := TAppState.asHome;
end;

procedure TMainForm.aSettingsExecute(Sender: TObject);
begin
  AppStateController.AppState := TAppState.asSettings;
end;

procedure TMainForm.CalcContentBoundsProc(Sender: TObject; var ContentBounds: TRectF);
begin
  if FNeedOffset and (FKBBounds.Top > 0) then
  begin
    ContentBounds.Bottom := Max(ContentBounds.Bottom, 2 * ClientHeight - FKBBounds.Top);
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
//  Settings.LoadSettings;
//
//  DataController.Manager.Connector.Address := Settings.Addr;
//
//
//  for var aFormInfo in FrameStand1.FrameInfos.Values do
//  begin
//    if (aFormInfo.Frame is TBaseForm) then
//      TBaseForm(aFormInfo.Frame).LoadSettings;
//  end;
//  Settings.DoChange;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TDCLog.Active := True;

  /// обработка исключений
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, HandleExceptionReport);

  DMCommon := TDMCommon.Create(Self);
  DataController := TDataController.Create(Self);

  InitMessageHandlers;

  lngControllerLanguageChanged(Self);

  VertScrollBox.AniCalculations.TouchTracking := [];
  VertScrollBox.OnCalcContentBounds := CalcContentBoundsProc;

  // подписываемся на команды
  FMapViewCommandSubscription := NxHorizon.Instance.Subscribe<IMainViewCommand>(MainAsync, OnMainViewCommand);

  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicatiOnEventService, FEventService) then
  begin
    FEventService.SetApplicationEventHandler(HandleApplicationEvent);
  end;
  Settings.OnChange := DoSettingsChanged;

  // логин
  if Settings.AutoLogon then
  begin
    try
      DataController.Manager.Connected := False;
      DataController.Manager.UserName := Settings.UserName;
      DataController.Manager.Password := Settings.Password;

      if (DataController.Manager.Items.Count > 0) and Assigned(DataController.Manager.Items[0].Connector) then
        DataController.Manager.Items[0].Connector.Address := Settings.Addr;

      DataController.Manager.Connected := True;
//      DataController.Manager.InitClients;
//      DataController.Manager.InitDevices;
      DataController.Manager.Active := True;

      AppStateController.AppState := asHome;
    except
      on e: Exception do
        AppStateController.AppState := TAppState.asLogin;
    end;
  end
  else
    AppStateController.AppState := TAppState.asLogin;

//{$IFDEF iOS}
//  var UIApp : UIApplication := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
//  UIApp.setIdleTimerDisabled(True);
//{$ENDIF}
//{$IFDEF ANDROID}
//  AcquireWakeLock;
//{$ENDIF}
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  try
    for var aFormInfo in FormStand1.FormInfos.Values do
    begin
      if (aFormInfo.Form is TBaseForm) then
        if aFormInfo.IsVisible then
          TBaseForm(aFormInfo.Form).SaveSettings;
    end;

    Settings.SaveSettings;
  except
    on e: Exception do
      TDCLog.WriteToLogFmt('TMainForm.FormDeactivate: error: %s', [e.Message]);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FormStand1.CloseAll;

  DeinitMessageHandlers;
  NxHorizon.Instance.WaitAndUnsubscribe(FMapViewCommandSubscription);

//{$IFDEF iOS}
//  var UIApp : UIApplication := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
//  UIApp.setIdleTimerDisabled(False);
//{$ENDIF}
//{$IFDEF ANDROID}
//  ReleaseWakeLock;
//{$ENDIF}
end;

procedure TMainForm.FormFocusChanged(Sender: TObject);
begin
  UpdateKBBounds(Sender);
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    // проверяем главное меню
    if mvMain.IsShowed then
    begin
      mvMain.HideMaster;
      Key := 0;
      KeyChar := #0;
      Exit;
    end;

    // проверяем фреймы
    var f: TBaseForm;
    for var aFormInfo in FormStand1.FormInfos.Values do
    begin
      if aFormInfo.IsVisible and (aFormInfo.Form is TBaseForm) then
      begin
        f := TBaseForm(aFormInfo.Form);
        if f.Back then
        begin
          Key := 0;
          KeyChar := #0;
          Exit;
        end;
      end;
    end;

    // переходим в предыдущее состояние, если оно есть (если нет - выходим)
    if AppStateController.Back then
    begin
      // отработали корректно
      Key := 0;
      KeyChar := #0;
    end
    else
    begin
      { DONE : добавить запрос пользователю на выход из приложения }
      if SecondsBetween(Now, FBackPressedTime) > 2 then
      begin
        //
        Key := 0;
        KeyChar := #0;
        FBackPressedTime := Now;
        Toast(DKLangConstW('SConfirmExit'));
      end
      else
      begin
        // выходим
      end;
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetNoLimit;

  Settings.LoadSettings;

  //DataController.Manager.Connector.Address := Settings.Addr;
  if (DataController.Manager.Items.Count > 0) and Assigned(DataController.Manager.Items[0].Connector) then
    DataController.Manager.Items[0].Connector.Address := Settings.Addr;

  for var aFormInfo in FormStand1.FormInfos.Values do
  begin
    if (aFormInfo.Form is TBaseForm) then
      TBaseForm(aFormInfo.Form).LoadSettings;
  end;
  Settings.DoChange;
end;

procedure TMainForm.FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  FKBBounds.Create(0, 0, 0, 0);
  FNeedOffset := False;
  RestorePosition;
end;

procedure TMainForm.FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  FKBBounds := TRectF.Create(Bounds);
  FKBBounds.TopLeft := ScreenToClient(FKBBounds.TopLeft);
  FKBBounds.BottomRight := ScreenToClient(FKBBounds.BottomRight);
  UpdateKBBounds(Sender);
end;

function TMainForm.HandleApplicationEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  for var aFormInfo in FormStand1.FormInfos.Values do
  begin
    if (aFormInfo.Form is TBaseForm) then
      TBaseForm(aFormInfo.Form).HandleApplicationEvent(AAppEvent, AContext);
  end;

  case AAppEvent of
    TApplicationEvent.FinishedLaunching:
    begin
      TOSLog.d('FinishedLaunching');
    end;

    TApplicationEvent.BecameActive:
    begin
      TOSLog.d('BecameActive');
      FormActivate(Self);
    end;

    TApplicationEvent.WillBecomeInactive:
    begin
      TOSLog.d('WillBecomeInactive');
      FormDeactivate(Self);
    end;

    TApplicationEvent.EnteredBackground:
    begin
      TOSLog.d('EnteredBackground - start');

      //Settings.LocationManager.CurrentAppState := casBackground;
      DataController.Manager.IdleMode := True;

      {$IFDEF iOS}
      var UIApp : UIApplication := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
      UIApp.setIdleTimerDisabled(False);
      {$ENDIF}
      {$IFDEF ANDROID}
      ReleaseWakeLock;
      {$ENDIF}
      TOSLog.d('EnteredBackground - done');
    end;

    TApplicationEvent.WillBecomeForeground:
    begin
      TOSLog.d('WillBecomeForeground - start');

      //Settings.LocationManager.CurrentAppState := casForeground;
      if Settings.UserName <> '' then
      begin
        DataController.Manager.IdleMode := False;
      end;

      {$IFDEF iOS}
      var UIApp : UIApplication := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
      UIApp.setIdleTimerDisabled(True);
      {$ENDIF}
      {$IFDEF ANDROID}
      AcquireWakeLock;
      {$ENDIF}
      TOSLog.d('WillBecomeForeground - done');
    end;

    TApplicationEvent.WillTerminate:
    begin
      TOSLog.d('WillTerminate');
      Settings.SaveSettings;
    end;

    TApplicationEvent.LowMemory: ;
    TApplicationEvent.TimeChange: ;
    TApplicationEvent.OpenURL: ;
  end;

  Result := True;
end;

procedure TMainForm.HandleExceptionReport(const Sender: TObject; const M: TMessage);
var
  Report: IgoExceptionReport;
begin
  Assert(M is TgoExceptionReportMessage);
  Report := TgoExceptionReportMessage(M).Report;
  // сохраняем в лог
  TDCLog.WriteToLog(Report.Report);
end;

procedure TMainForm.HideAll(const AExceptions: TArray<TClass>);
var
  LFormInfo: TFormInfo<TForm>;
  LFormInfos: TArray<TFormInfo<TForm>>;
  LConsiderExceptions: Boolean;
begin
  LFormInfos := FormStand1.FormInfos.Values.ToArray;
  LConsiderExceptions := Length(AExceptions) > 0;

  for LFormInfo in LFormInfos do
  begin
    if (not LConsiderExceptions) or not ClassInArray(LFormInfo.Form, AExceptions) then
      LFormInfo.Hide;
  end;
end;

procedure TMainForm.InitMessageHandlers;
begin
  FOrientationChangedMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChanged);
end;

procedure TMainForm.lngControllerLanguageChanged(Sender: TObject);
begin
  laVersion.Text := DKLangConstW('SVersion') + ' ' + GetVersion;
  laAppTitle.Text := DKLangConstW('SAppTitle');
  Caption := laAppTitle.Text;
//  DKLangConstW('SMessageCaption');
end;

procedure TMainForm.OnMainViewCommand(const aEvent: IMainViewCommand);
begin
  case aEvent.Value.Command of
    mcShowLogin:
      SetupView<TLoginForm>([], [], loMainScrolable, nil, nil, False);
    mcHome:
      SetupView<THomeForm>([], [], loMainContent, aHome, nil, True);

//      SetupView<TLoginForm>([], [], loMainContent, nil, nil, False);
//    mcSignUp:
//      SetupView<TSingUpFrame>([], [], loMainScrolable, nil, nil, False);
//
//    mcShowMainMenu:
//      SetupView<TMainMenuFrame>([TMapFrame, TObjectsFrame], [], loMainContent, nil, nil, True);
//
//    mcShowMap:
//    begin
//      SetupView<TMapFrame>([TMapFrame, TObjectsFrame], [], loMainContent, aMap, nil, True);
//      if Assigned(aEvent.Value.Proc) then
//        aEvent.Value.Proc(nil);
//
//    end;
//
//    mcShowReport:
//    begin
//      SetupView<TReportFrame>([TMapFrame, TObjectsFrame], [], loMainContent, nil, nil, True);
//      if Assigned(aEvent.Value.Proc) then
//        aEvent.Value.Proc(nil);
//    end;
//
//    mcShowObjects:
//      SetupView<TObjectsFrame>([TMapFrame, TObjectsFrame], [], loMainClient, aObjects, nil, True);
//
//    mcShowObjectEditor:
//    begin
//      SetupView<TObjectEditorFrame>([TMapFrame, TObjectsFrame], [], loMainContent, nil, nil, False);
//      if Assigned(aEvent.Value.Proc) then
//        aEvent.Value.Proc(nil);
//    end;
//
//    mcShowAddObject:
//    begin
//      SetupView<TAddObjectFrame>([TMapFrame, TObjectsFrame], [], loMainContent, nil, nil, False);
//      if Assigned(aEvent.Value.Proc) then
//        aEvent.Value.Proc(nil);
//    end;
//
//    mcShowShareObject:
//    begin
//      SetupView<TShareDeviceFrame>([TMapFrame, TObjectsFrame], [], loMainContent, nil, nil, False);
//      if Assigned(aEvent.Value.Proc) then
//        aEvent.Value.Proc(nil);
//    end;


    mcShowInterval:
    begin
//      TDCLog.WriteToLog('mcShowInterval');
//      SetupView<TSelectIntervalDlg>([TMapFrame, TObjectsFrame, TReportFrame], [], loMainContent, nil, aEvent.Value.Proc, False);
//      TDCLog.WriteToLog('mcShowInterval done');
    end;

    mcShowSettings:
    begin
      SetupView<TSettingsForm>([THomeForm], [], loMainContent, aSettings, nil, True);
    end;

//    mcShowTest:
//      //TTestBottomSheet.Create(Self).Show;
//      TTestToolbarForm.Create(Self).Show;

  end;
end;

procedure TMainForm.OrientationChanged(const ASender: TObject; const AMessage: TMessage);
begin
  for var aFormInfo in FormStand1.FormInfos.Values do
  begin
    if (aFormInfo.Form is TBaseForm) then
      if aFormInfo.IsVisible then
        TBaseForm(aFormInfo.Form).OrientationChanged(ASender, AMessage);
  end;
end;

procedure TMainForm.DeinitMessageHandlers;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, FOrientationChangedMessageId);
end;

procedure TMainForm.DoSettingsChanged(Sender: TObject);
begin
//  ShowMessage('Settings changed');
  LangManager.LanguageID := Settings.Language;
  if Settings.UserName = '' then
    bLogout.Action := aLogin
  else
    bLogout.Action := aLogout;
end;

procedure TMainForm.RestorePosition;
begin
  VertScrollBox.ViewportPosition := PointF(VertScrollBox.ViewportPosition.X, 0);
  loMainScrolable.Align := TAlignLayout.Client;
  VertScrollBox.RealignContent;
end;

procedure TMainForm.SetNoLimit;
begin
//{$IFDEF ANDROID}
//  TAndroidHelper.Activity.getWindow.setFlags(
//    TJWindowManager_LayoutParams.JavaClass.FLAG_LAYOUT_NO_LIMITS,
//    TJWindowManager_LayoutParams.JavaClass.FLAG_LAYOUT_NO_LIMITS);
//  TAndroidHelper.Activity.getWindow.
//{$ENDIF}

  SystemBars.OnInsetsChange := SystemBarsInsetsChange;

  SystemBars.StatusBarBackgroundColor := $00000000;
  SystemBars.NavigationBarBackgroundColor := $00000000;

  SystemBars.Visibility := TipFormSystemBars.TVisibilityMode.VisibleAndOverlap;
//  Padding.Rect := SystemBars.Insets;
//  ShowMessage('test2: ' + SystemBars.Insets.top.ToString
//    + ', '  + SystemBars.Insets.Bottom.ToString
//    + ', '  + SystemBars.Insets.Left.ToString
//    + ', '  + SystemBars.Insets.Right.ToString
//   );
end;

function TMainForm.SetupView<T>(const aCloseExceptions, aHideExceptions: TArray<TClass>; aParent: TFmxObject;
      //aScrolable: Boolean;
      aAction: TAction; aProc: TProc<TObject> = nil; aEnableMainMV: Boolean = False): TFormInfo<T>;
var
  aIsNew: Boolean;
begin
//  if aScrolable then
//    MainLayout.Parent := VertScrollBox
//  else
//    MainLayout.Parent := Self;

//  VertScrollBox.Visible := aScrolable;
//  MainLayout.BringToFront;
  mvMain.MasterButton := nil;
  if mvMain.Visible then
    mvMain.HideMaster;
  mvMain.Enabled := aEnableMainMV;
//  mvMain.Parent := loMainContent;

  VertScrollBox.Visible := aParent = loMainScrolable;

  HideAll(aHideExceptions + [T]);

  FormStand1.CloseAllExcept(aCloseExceptions + [T]);
  Result := FormStand1.FormInfo<T>;
  if Result = nil then
  begin
    aIsNew := True;
    Result := FormStand1.New<T>(aParent, '');// 'fader');
  end
  else
    aIsNew := False;

  if Result.Form is TBaseForm then
  begin
    TBaseForm(Result.Form).Proc := aProc;
    TBaseForm(Result.Form).ApplySystemBarsInsets(SystemBars);
  end;

  if Result.Form is TToolbarForm then
  begin
//    mvMain.Parent := TToolbarForm(Result.Frame); // loContent;

    if TToolbarForm(Result.Form).sbDrawer.Visible then
      mvMain.MasterButton := TToolbarForm(Result.Form).sbDrawer
    else
      mvMain.MasterButton := nil;

    mvMain.HideMaster;
    mvMain.BringToFront;

//    TToolbarForm(Result.Frame).loTop.Margins.Top := SystemBars.Insets.Bottom;
  end;

  // подсветим кнопку соответствующую текущему состоянию приложения и спрячем панель навигации
  if Assigned(aAction) then
    UpdatePressed(aAction.Tag);
//  mvMain.HideMaster;
//
  mvMain.BringToFront;

  Result.Show;

  if aIsNew and (Result.Form is TBaseForm) then
    TBaseForm(Result.Form).DoAfterCreate;

end;

procedure TMainForm.SystemBarsInsetsChange(Sender: TObject);
begin
//  Padding.Rect := SystemBars.Insets;
  mvMain.Margins.Top := SystemBars.Insets.Top;
  mvMain.Margins.Bottom := SystemBars.Insets.Bottom;

  for var aFormInfo in FormStand1.FormInfos.Values do
  begin
    if (aFormInfo.Form is TBaseForm) then
      if aFormInfo.IsVisible then
        TBaseForm(aFormInfo.Form).ApplySystemBarsInsets(SystemBars);
  end;

//  Padding.Left := SystemBars.Insets.Left;
//  Padding.Right := SystemBars.Insets.Right;
//  ShowMessage(SystemBars.Insets.Top.ToString);
//  ShowMessage(Padding.Top.ToString);
end;

procedure TMainForm.UpdateKBBounds(Sender: TObject);
var
  LFocused: TControl;
  LFocusRect: TRectF;
begin
  FNeedOffset := False;
  if Assigned(Focused) and (FKBBounds <> NullRect) then
  begin
    LFocused := TControl(Focused.GetObject);
    LFocusRect := LFocused.AbsoluteRect;
    LFocusRect.Offset(VertScrollBox.ViewportPosition);
    if (LFocusRect.IntersectsWith(TRectF.Create(FKBBounds))) and (LFocusRect.Bottom > FKBBounds.Top) then
    begin
      FNeedOffset := True;
      loMainScrolable.Align := TAlignLayout.Horizontal;
      VertScrollBox.RealignContent;
      Application.ProcessMessages;
      VertScrollBox.ViewportPosition := PointF(VertScrollBox.ViewportPosition.X, LFocusRect.Bottom - FKBBounds.Top + SystemBars.Insets.Top); // + 50);
    end;
  end;
  if not FNeedOffset then
    RestorePosition;
end;

procedure TMainForm.UpdatePressed(aTag: Integer);
begin
  for var i := 0 to lbMenu.Count - 1 do
    for var j := 0 to lbMenu.ListItems[i].ControlsCount - 1 do
      if lbMenu.ListItems[i].Controls[j] is TCustomButton then
      begin
        var b := TCustomButton(lbMenu.ListItems[i].Controls[j]);
        if not Assigned(b.Action) then
          Continue;

        if (b.Action.Tag = aTag) then
        begin
          b.IsPressed := True;
          TAction(b.Action).ImageIndex := b.Action.Tag;
        end
        else
        begin
          b.IsPressed := False;
          TAction(b.Action).ImageIndex := b.Action.Tag + 1;
        end;
      end;
end;

end.
