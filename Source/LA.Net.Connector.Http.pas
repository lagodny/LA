unit LA.Net.Connector.Http;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  // IdGlobal, IdTCPClient, IdException,
  SynCrossPlatformSpecific,
  SynCrossPlatformREST,
  LA.Net.Connector, LA.Types.Monitoring, LA.Net.Connector.Intf,
  LA.Net.DC.Client;
// LA.DC.mORMotClient;

const
  cHttp = 'http';
  cHttps = 'https';
  cDefHttpPort = '80';
  cDefHttpsPort = '443';

type
  TLAHttpAddr = record
    Https: Boolean;
    Host, Port: string;
    function InitFrom(const aAddr: string): TLAHttpAddr;
  end;

  TLAHttpConnector = class(TLACustomConnector, IDCMonitoring)
  private
    FClient: TSQLRestClientHTTP;
    // сервисы создаются по мере необходимости
    FSignUp: IDCSignUp;
    FSession: IDCSession;
    FMonitoring: IMonitoring;

    FEncrypt: Boolean;
    FCompressionLevel: Integer;
    FHttps: Boolean;
    FProxyName: string;
    FProxyByPass: string;
    FConnectTimeOut: Integer;
    FSendTimeOut: Integer;
    FReadTimeOut: Integer;
    procedure SetHttps(const Value: Boolean);
    procedure SetProxyByPass(const Value: string);
    procedure SetProxyName(const Value: string);

    function GetSignUp: IDCSignUp;
    function GetSession: IDCSession;
    function GetMonitoring: IMonitoring;
  protected
    function GetEncrypt: Boolean; override;
    function GetCompressionLevel: Integer; override;
    function GetConnectTimeOut: Integer; override;
    function GetReadTimeOut: Integer; override;
    function GetSendTimeOut: Integer; override;

    procedure SetEncrypt(const Value: Boolean); override;
    procedure SetCompressionLevel(const Value: Integer); override;
    procedure SetReadTimeOut(const Value: Integer); override;
    procedure SetConnectTimeOut(const Value: Integer); override;
    procedure SetSendTimeOut(const Value: Integer); override;

    function GetConnected: Boolean; override;
    procedure TryConnectTo(const aAddrLine: string); override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;

    procedure DoAuthorize; override;

    procedure DoServicesConnect; override;
    procedure DoServicesDisconnect; override;

    procedure DoLog(const aText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // все эти методы используют блокировки (safe)
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Authorize; override;

    // взаимодействие с Мониторингом
    function SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string; override;

    // регистрация
    procedure RequestSignUp(const Login: String; const EMail: String; const Password: String);

    // сервисы
    property SignUp: IDCSignUp read GetSignUp;
    property Session: IDCSession read GetSession;
    property Monitoring: IMonitoring read GetMonitoring;
  published
    property Https: Boolean read FHttps write SetHttps;
    property ProxyName: string read FProxyName write SetProxyName;
    property ProxyByPass: string read FProxyByPass write SetProxyByPass;
    property SendTimeOut: Integer read FSendTimeOut write SetSendTimeOut;
  end;

  TLAHttpTrackingConnection = class(TLAHttpConnector, IDCTracking)
  private
    FTracking: ITracking;
    function GetTracking: ITracking;
  protected
    procedure DoServicesConnect; override;
    procedure DoServicesDisconnect; override;
  public
    procedure InitServerCache; override;

    function GetClients: Variant;
    function GetDevices(const Clients: TIDDynArray): Variant;
    function GetDevicesData(const Devices: TIDDynArray): Variant;

    function GetTrack(const DeviceID: TID; const Date1, Date2: Int64): Variant;
    function GetReport(const DeviceID: TID; const Date1, Date2: Int64): Variant;

    procedure SetTagValue(const DeviceID: TID; const TagSID: string; const Value: Variant);
    procedure SetDevice(const Device: Variant);

    procedure CreateDevice(const aName, aLogin, aPhone, aProto: string);
    procedure ShareDevice(const aDeviceID: TID; const aLogin: string; const aRight: string);
    // сервисы
    property Tracking: ITracking read GetTracking;
  end;

implementation

uses
  LA.Log,
  LA.Utils.System;

{ TDCHTTPConnector }

procedure TLAHttpConnector.Authorize;
begin
  Lock;
  try
    DoAuthorize;
  finally
    Unlock;
  end;
end;

procedure TLAHttpConnector.Connect;
begin
  Lock;
  try
    DoConnect;
  finally
    Unlock;
  end;
end;

constructor TLAHttpConnector.Create(AOwner: TComponent);
begin
  inherited;
  FConnectTimeOut := 5000;
  FReadTimeOut := 10000;
  FSendTimeOut := 5000;
end;

destructor TLAHttpConnector.Destroy;
begin
  DoDisconnect;
  inherited;
end;

procedure TLAHttpConnector.Disconnect;
begin
  Lock;
  try
    DoDisconnect;
  finally
    Unlock;
  end;
end;

procedure TLAHttpConnector.DoAuthorize;
begin
  TDCLog.WriteToLog('DoAuthorize');
  if not Connected then
    DoConnect;

  FClient.SetUser(TSQLRestServerAuthenticationDefault, UserName, Password);
  FAuthorized := True;

  // можно получить хешированный пароль пользователя и хранить его, а не исходный (Client.Authentication.User.PasswordHashHexa)
  if Assigned(OnAuthorize) then
    OnAuthorize(Self);

  DoServicesConnect;

  // добавляем на сервер информации о нашем подключении
  Session.SetSessionInfo(TLASystemUtils.ProgramFullSpec);

  TDCLog.WriteToLog('DoAuthorize - done');
end;

procedure TLAHttpConnector.DoConnect;
begin
  TDCLog.WriteToLog('DoConnect');
  TryConnect;

  if Assigned(OnConnect) then
    OnConnect(Self);
  TDCLog.WriteToLog('DoConnect - done');
end;

procedure TLAHttpConnector.DoDisconnect;
begin
  TDCLog.WriteToLog('DoDisconnect');
  try
    FAuthorized := False;

    if Assigned(FClient) then
    begin
      try
        // сначала удаляем клиента, это очистит ссылки в сервисах и они не будут пытаться обращаться к серверу
        FreeAndNil(FClient);
        DoServicesDisconnect;
      except
        on e: Exception do
          TDCLog.WriteToLogFmt('DoDisconnect: error: %s', [e.Message]);
      end;
//
//      try
//        DoServicesDisconnect;
//      except
//        on e: Exception do
//          TDCLog.WriteToLogFmt('DoDisconnect: DoServicesDisconnect: error: %s', [e.Message]);
//      end;
//      FreeAndNil(FClient);

      if Assigned(OnDisconnect) then
        OnDisconnect(Self);
    end;
  except
    on e: Exception do
      TDCLog.WriteToLogFmt('DoDisconnect: error: %s', [e.Message]);
  end;
  TDCLog.WriteToLog('DoDisconnect - done');
end;

procedure TLAHttpConnector.DoLog(const aText: string);
begin
  TDCLog.WriteToLog(aText);
end;

procedure TLAHttpConnector.DoServicesConnect;
begin
  inherited;
//  FSession := TServiceDCSession.Create(FClient);
//  FMonitoring := TServiceMonitoring.Create(FClient);
end;

procedure TLAHttpConnector.DoServicesDisconnect;
begin
  inherited;
  FSignUp := nil;
  FMonitoring := nil;
  FSession := nil;
end;

function TLAHttpConnector.GetCompressionLevel: Integer;
begin
  Result := FCompressionLevel;
end;

function TLAHttpConnector.GetConnected: Boolean;
begin
  Result := Assigned(FClient);
end;

function TLAHttpConnector.GetConnectTimeOut: Integer;
begin
  Result := FConnectTimeOut;
end;

function TLAHttpConnector.GetEncrypt: Boolean;
begin
  Result := FEncrypt;
end;

function TLAHttpConnector.GetMonitoring: IMonitoring;
begin
  if not Assigned(FMonitoring) then
    FMonitoring := TServiceMonitoring.Create(FClient);
  Result := FMonitoring;
end;

function TLAHttpConnector.GetReadTimeOut: Integer;
begin
  Result := FReadTimeOut;
end;

function TLAHttpConnector.GetSendTimeOut: Integer;
begin
  Result := FSendTimeOut;
end;

function TLAHttpConnector.GetSession: IDCSession;
begin
  if not Assigned(FSession) then
    FSession := TServiceDCSession.Create(FClient);
  Result := FSession;
end;

function TLAHttpConnector.GetSignUp: IDCSignUp;
begin
  if not Assigned(FSignUp) then
    FSignUp := TServiceDCSignUp.Create(FClient);
  Result := FSignUp;
end;

procedure TLAHttpConnector.RequestSignUp(const Login, EMail, Password: String);
begin
  Lock;
  try
    // проверяем только подключение (авторизация не нужна)
    CheckConnected;
    SignUp.RequestSignUp(Login, EMail, Password);
  finally
    Unlock;
  end;
end;

function TLAHttpConnector.SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string;
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Result := Monitoring.SensorsDataAsText(IDs, aUseCache);
  finally
    Unlock;
  end;
end;

procedure TLAHttpConnector.SetCompressionLevel(const Value: Integer);
begin
  if FCompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.SetConnectTimeOut(const Value: Integer);
begin
  if FConnectTimeOut <> Value then
  begin
    FConnectTimeOut := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.SetEncrypt(const Value: Boolean);
begin
  if FEncrypt <> Value then
  begin
    FEncrypt := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.SetHttps(const Value: Boolean);
begin
  if FHttps <> Value then
  begin
    FHttps := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.SetProxyByPass(const Value: string);
begin
  if FProxyByPass <> Value then
  begin
    FProxyByPass := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.SetProxyName(const Value: string);
begin
  if FProxyName <> Value then
  begin
    FProxyName := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.SetReadTimeOut(const Value: Integer);
begin
  if FReadTimeOut <> Value then
  begin
    FReadTimeOut := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.SetSendTimeOut(const Value: Integer);
begin
  if FSendTimeOut <> Value then
  begin
    FSendTimeOut := Value;
    DoPropChanged;
  end;
end;

procedure TLAHttpConnector.TryConnectTo(const aAddrLine: string);
var
  aAddrRec: TLAHttpAddr;
begin
  aAddrRec.InitFrom(aAddrLine);

  DoDisconnect;

  // проверяем возможность подключения без авторизации
  FClient := GetClientNoUser(aAddrRec.Host, { UserName, Password, } StrToInt(aAddrRec.Port), SERVER_ROOT, aAddrRec.Https, ProxyName,
    ProxyByPass, SendTimeOut, ReadTimeout, ConnectTimeout);
  // включаем логирование
  FClient.OnLog := DoLog;
  FClient.LogLevel := [
    sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError,
    sllEnter, sllLeave,
    sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    sllServiceCall, sllServiceReturn, sllUserAuth,
    sllCustom1, sllCustom2, sllCustom3, sllCustom4,
    sllNewRun, sllDDDError, sllDDDInfo
  ];
  FClient.OnLog := DoLog;

  // FClient.SetUser(TSQLRestServerAuthenticationDefault, UserName, Password);

  //DoServicesConnect;
  //FSignUp := TServiceDCSignUp.Create(FClient);
end;

{ TDCHttpAddr }

function TLAHttpAddr.InitFrom(const aAddr: string): TLAHttpAddr;
var
  aParams: TStrings;
begin
  aParams := TStringList.Create;
  try
    aParams.LineBreak := ':';
    aParams.Text := aAddr;

    if aParams.Count = 3 then
    begin
      // https://dc.tdc.org.ua:443
      Https := SameText(aParams[0], cHttps);
      Host := StringReplace(aParams[1], '//', '', [rfReplaceAll]);
      Port := aParams[2];
    end

    else if aParams.Count = 2 then
    begin
      // http://dc.tdc.org.ua
      if SameText(aParams[0], cHttp) then
      begin
        Https := False;
        Host := StringReplace(aParams[1], '//', '', [rfReplaceAll]);
        Port := cDefHttpPort;
      end

      // https://dc.tdc.org.ua
      else if SameText(aParams[0], cHttps) then
      begin
        Https := True;
        Host := StringReplace(aParams[1], '//', '', [rfReplaceAll]);
        Port := cDefHttpsPort;
      end

      // dc.tdc.org.ua:80
      else
      begin
        Https := False;
        Host := aParams[0];
        Port := aParams[1];
      end
    end

    // dc.tdc.org.ua
    else if aParams.Count = 1 then
    begin
      Https := False;
      Host := aParams[0];
      Port := cDefHttpPort;
    end

    else
      raise EDCConnectorBadAddress.CreateFmt(sResAddressIsBadFmt, [aAddr]);

  finally
    aParams.Free;
  end;

end;

{ TDCHttpTrackingConnection }

procedure TLAHttpTrackingConnection.CreateDevice(const aName, aLogin, aPhone, aProto: string);
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Tracking.CreateDevice(aName, aLogin, aPhone, aProto);
  finally
    Unlock;
  end;
end;

procedure TLAHttpTrackingConnection.DoServicesConnect;
begin
  inherited;
//  if not Assigned(FTracking) then
//    FTracking := TServiceTracking.Create(FClient);
end;

procedure TLAHttpTrackingConnection.DoServicesDisconnect;
begin
  inherited;
  FTracking := nil;
end;

function TLAHttpTrackingConnection.GetClients: Variant;
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Result := Tracking.GetClients;
  finally
    Unlock;
  end;
end;

function TLAHttpTrackingConnection.GetDevices(const Clients: TIDDynArray): Variant;
begin
  TDCLog.WriteToLog('GetDevices');
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Result := Tracking.GetDevices(Clients);
  finally
    Unlock;
  end;
  TDCLog.WriteToLog('GetDevices - done');
end;

function TLAHttpTrackingConnection.GetDevicesData(const Devices: TIDDynArray): Variant;
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Result := Tracking.GetDevicesData(Devices);
  finally
    Unlock;
  end;
end;

function TLAHttpTrackingConnection.GetReport(const DeviceID: TID; const Date1, Date2: Int64): Variant;
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Result := Tracking.GetReport(DeviceID, Date1, Date2);
  finally
    Unlock;
  end;
end;

function TLAHttpTrackingConnection.GetTrack(const DeviceID: TID; const Date1, Date2: Int64): Variant;
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Result := Tracking.GetTrack(DeviceID, Date1, Date2);
  finally
    Unlock;
  end;
end;

function TLAHttpTrackingConnection.GetTracking: ITracking;
begin
  if not Assigned(FTracking) then
    FTracking := TServiceTracking.Create(FClient);
  Result := FTracking;
end;

procedure TLAHttpTrackingConnection.InitServerCache;
begin
  GetDevices([]);
end;

procedure TLAHttpTrackingConnection.SetDevice(const Device: Variant);
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Tracking.SetDevice(Device);
  finally
    Unlock;
  end;
end;

procedure TLAHttpTrackingConnection.SetTagValue(const DeviceID: TID; const TagSID: string; const Value: Variant);
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Tracking.SetTagValue(DeviceID, TagSID, Value);
  finally
    Unlock;
  end;
end;

procedure TLAHttpTrackingConnection.ShareDevice(const aDeviceID: TID; const aLogin, aRight: string);
begin
  Lock;
  try
    CheckConnected;
    CheckAuthorized;
    Tracking.ShareDevice(aDeviceID, aLogin, aRight);
  finally
    Unlock;
  end;
end;

end.
