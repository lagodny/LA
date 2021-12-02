unit LA.Net.Connector.Http;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  //IdGlobal, IdTCPClient, IdException,
  SynCrossPlatformREST,
  LA.Net.Connector, LA.Types.Monitoring,
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

  TDCHttpConnector = class(TDCCustomConnector)
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
    function GroupSensorValueByID(const IDs: TIDArr): TValArr;
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
//    procedure TryConnectTo(const aHost: string; const aPort: Integer); override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;

    function SensorValue(const SID: String): String; override;
    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr; override;
    function GetSensorsData(const IDs: TSIDArr): TDataRecExtArr; override;
    function SensorsDataAsText(const IDs: TSIDArr): string; override;

  published
    property Https: boolean read FHttps write SetHttps;
    property ProxyName: string read FProxyName write SetProxyName;
    property ProxyByPass: string read FProxyByPass write SetProxyByPass;
    property SendTimeOut: Integer read FSendTimeout write SetSendTimeOut;
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
end;

procedure TDCHttpConnector.DoDisconnect;
begin
  FMonitoring := nil;
  FreeAndNil(FClient);
end;

function TDCHttpConnector.GetCompressionLevel: Integer;
begin
  Result := FCompressionLevel;
end;

function TDCHttpConnector.GetConnected: Boolean;
begin
  Result := Assigned(FClient);
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

function TDCHttpConnector.GetSensorsData(const IDs: TSIDArr): TDataRecExtArr;
begin
  if not Connected then
    Connect;
  Result := FMonitoring.GroupSensorDataExt(IDs);
end;

function TDCHttpConnector.GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
begin
  if not Connected then
    Connect;
  Result := FMonitoring.GroupSensorDataExtByID(IDs);
end;

function TDCHttpConnector.GroupSensorValueByID(const IDs: TIDArr): TValArr;
begin
  if not Connected then
    Connect;
  Result := FMonitoring.GroupSensorValueByID(IDs);
end;

function TDCHttpConnector.SensorsDataAsText(const IDs: TSIDArr): string;
begin
  if not Connected then
    Connect;
  Result := FMonitoring.SensorsDataAsText(IDs);
end;

function TDCHttpConnector.SensorValue(const SID: String): String;
begin
  if not Connected then
    Connect;
  try
    Result := FMonitoring.SensorValue(SID);
  except
    on e: EServiceException do
    begin
      DoDisconnect;
      DoConnect;
      Result := FMonitoring.SensorValue(SID);
    end;
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
  DoDisconnect;
  aAddrRec.InitFrom(aAddrLine);

  FClient := GetClient(aAddrRec.Host, UserName, Password, StrToInt(aAddrRec.Port), SERVER_ROOT, aAddrRec.Https,
    ProxyName, ProxyByPass,
    SendTimeOut, ReadTimeout, ConnectTimeout);
  FClient.SetUser(TSQLRestServerAuthenticationDefault, UserName, Password);
  FMonitoring :=  TServiceMonitoring.Create(FClient);
end;

//procedure TDCHttpConnector.TryConnectTo(const aHost: string; const aPort: Integer);
//begin
//  DoDisconnect;
//
//  FClient := GetClient(aHost, UserName, Password, aPort, SERVER_ROOT, HTTPs,
//    ProxyName, ProxyByPass,
//    SendTimeOut, ReadTimeout, ConnectTimeout);
//  FClient.SetUser(TSQLRestServerAuthenticationDefault, UserName, Password);
//  FMonitoring :=  TServiceMonitoring.Create(FClient);
//end;

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

end.

