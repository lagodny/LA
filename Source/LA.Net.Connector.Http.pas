unit LA.Net.Connector.Http;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  //IdGlobal, IdTCPClient, IdException,
  SynCrossPlatformREST,
  LA.Net.Connector, LA.Types.Monitoring, LA.Net.Connector.Intf,
  LA.Net.DC.Client;
  //LA.DC.mORMotClient;

const
  cHttp = 'http';
  cHttps = 'https';
  cDefHttpPort = '80';
  cDefHttpsPort = '443';

type
  TDCHttpAddr = record
    Https: Boolean;
    Host, Port: string;
    function InitFrom(const aAddr: string): TDCHttpAddr;
  end;

  TDCHttpConnector = class(TDCCustomConnector, IDCMonitoring)
  private
    FClient: TSQLRestClientHTTP;
    FMonitoring: IMonitoring;
    FEncrypt: boolean;
    FCompressionLevel: Integer;
    FHttps: boolean;
    FProxyName: string;
    FProxyByPass: string;
    FConnectTimeOut: Integer;
    FSendTimeOut: Integer;
    FReadTimeOut: Integer;
    procedure SetHttps(const Value: boolean);
    procedure SetProxyByPass(const Value: string);
    procedure SetProxyName(const Value: string);
    procedure SetSendTimeOut(const Value: Integer);
  protected
    function GetEncrypt: boolean; override;
    function GetCompressionLevel: Integer; override;
    function GetConnectTimeOut: Integer; override;
    function GetReadTimeOut: Integer; override;

    procedure SetEncrypt(const Value: boolean); override;
    procedure SetCompressionLevel(const Value: Integer); override;
    procedure SetReadTimeOut(const Value: Integer); override;
    procedure SetConnectTimeOut(const Value: Integer); override;

    function GetConnected: Boolean; override;

    procedure TryConnectTo(const aAddrLine: string); override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;

    procedure DoServicesConnect; override;
    procedure DoServicesDisconnect; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;

    // взаимодействие с Мониторингом
    function SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string; override;
  published
    property Https: boolean read FHttps write SetHttps;
    property ProxyName: string read FProxyName write SetProxyName;
    property ProxyByPass: string read FProxyByPass write SetProxyByPass;
    property SendTimeOut: Integer read FSendTimeout write SetSendTimeOut;
  end;

  TDCHttpTrackingConnection = class(TDCHttpConnector, IDCTracking)
  private
    FTracking: ITracking;
  protected
    procedure DoServicesConnect; override;
    procedure DoServicesDisconnect; override;
  public
    function GetClients: Variant;
    function GetDevices(const Clients: TIDDynArray): Variant;
    function GetDevicesData(const Devices: TIDDynArray): Variant;
  end;



implementation

{ TDCHTTPConnector }

procedure TDCHttpConnector.Connect;
begin
  DoConnect;
end;

constructor TDCHttpConnector.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TDCHttpConnector.Destroy;
begin
  DoDisconnect;
  inherited;
end;

procedure TDCHttpConnector.Disconnect;
begin
  DoDisconnect;
end;

procedure TDCHttpConnector.DoConnect;
begin
  TryConnect;

  if Assigned(OnConnect) then
    OnConnect(Self);
end;

procedure TDCHttpConnector.DoDisconnect;
begin
  ClientLock.Enter;
  try
    if Assigned(FClient) then
    begin
      //FMonitoring := nil;
      DoServicesDisconnect;
      FreeAndNil(FClient);
      if Assigned(OnDisconnect) then
        OnDisconnect(Self);
    end;
  finally
    ClientLock.Leave;
  end;
end;

procedure TDCHttpConnector.DoServicesConnect;
begin
  inherited;
  FMonitoring :=  TServiceMonitoring.Create(FClient);
end;

procedure TDCHttpConnector.DoServicesDisconnect;
begin
  inherited;
  FMonitoring := nil;
end;

function TDCHttpConnector.GetCompressionLevel: Integer;
begin
  Result := FCompressionLevel;
end;

function TDCHttpConnector.GetConnected: Boolean;
begin
  ClientLock.Enter;
  try
    Result := Assigned(FClient);
  finally
    ClientLock.Leave;
  end;
end;

function TDCHttpConnector.GetConnectTimeOut: Integer;
begin
  Result := FConnectTimeOut;
end;

function TDCHttpConnector.GetEncrypt: boolean;
begin
  Result := FEncrypt;
end;

function TDCHttpConnector.GetReadTimeOut: Integer;
begin
  Result := FReadTimeOut;
end;

function TDCHttpConnector.SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string;
begin
  if not Connected then
    Connect;

  ClientLock.Enter;
  try
    if Assigned(FMonitoring) then
      Result := FMonitoring.SensorsDataAsText(IDs, aUseCache);
  finally
    ClientLock.Leave;
  end;
end;

procedure TDCHttpConnector.SetCompressionLevel(const Value: Integer);
begin
  if FCompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.SetConnectTimeOut(const Value: Integer);
begin
  if FConnectTimeOut <> Value then
  begin
    FConnectTimeOut := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.SetEncrypt(const Value: boolean);
begin
  if FEncrypt <> Value then
  begin
    FEncrypt := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.SetHttps(const Value: boolean);
begin
  if FHttps <> Value then
  begin
    FHttps := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.SetProxyByPass(const Value: string);
begin
  if FProxyByPass <>Value then
  begin
    FProxyByPass := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.SetProxyName(const Value: string);
begin
  if FProxyName <> Value then
  begin
    FProxyName := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.SetReadTimeOut(const Value: Integer);
begin
  if FReadTimeOut <> Value then
  begin
    FReadTimeOut := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.SetSendTimeOut(const Value: Integer);
begin
  if FSendTimeOut <> Value then
  begin
    FSendTimeOut := Value;
    DoPropChanged;
  end;
end;

procedure TDCHttpConnector.TryConnectTo(const aAddrLine: string);
var
  aAddrRec: TDCHttpAddr;
begin
  aAddrRec.InitFrom(aAddrLine);

  DoDisconnect;

  ClientLock.Enter;
  try
    FClient := GetClient(aAddrRec.Host, UserName, Password, StrToInt(aAddrRec.Port), SERVER_ROOT, aAddrRec.Https,
      ProxyName, ProxyByPass,
      SendTimeOut, ReadTimeout, ConnectTimeout);
    FClient.SetUser(TSQLRestServerAuthenticationDefault, UserName, Password);

    DoServicesConnect;
//    FMonitoring :=  TServiceMonitoring.Create(FClient);
  finally
    ClientLock.Leave;
  end;
end;

{ TDCHttpAddr }

function TDCHttpAddr.InitFrom(const aAddr: string): TDCHttpAddr;
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
      HTTPs := SameText(aParams[0], cHTTPs);
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

procedure TDCHttpTrackingConnection.DoServicesConnect;
begin
  inherited;
  FTracking :=  TServiceTracking.Create(FClient);
end;

procedure TDCHttpTrackingConnection.DoServicesDisconnect;
begin
  inherited;
  FTracking := nil;
end;

function TDCHttpTrackingConnection.GetClients: Variant;
begin
  if not Connected then
    Connect;

  ClientLock.Enter;
  try
    if Assigned(FTracking) then
      Result := FTracking.GetClients;
  finally
    ClientLock.Leave;
  end;
end;


function TDCHttpTrackingConnection.GetDevices(const Clients: TIDDynArray): Variant;
begin
  if not Connected then
    Connect;

  ClientLock.Enter;
  try
    if Assigned(FTracking) then
      Result := FTracking.GetDevices(Clients);
  finally
    ClientLock.Leave;
  end;
end;

function TDCHttpTrackingConnection.GetDevicesData(const Devices: TIDDynArray): Variant;
begin
  if not Connected then
    Connect;

  ClientLock.Enter;
  try
    if Assigned(FTracking) then
      Result := FTracking.GetDevicesData(Devices);
  finally
    ClientLock.Leave;
  end;
end;

end.

