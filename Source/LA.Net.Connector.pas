{*******************************************************}
{                                                       }
{       LA DC Components                                }
{                                                       }
{       Copyright (C) 2021 LA                           }
{                                                       }
{*******************************************************}

unit LA.Net.Connector;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  LA.Net.Connector.Intf, LA.Types.Monitoring;

resourcestring
  sResAddressIsEmpty = 'Address is empty. The format of Address should be: host1:port1;host2:port2 etc';
  sResAddressIsBadFmt = 'Check Address (%s). The format of Address should be: host1:port1;host2:port2 etc';


const
  cDefConnectTimeout = 2000;
  cDefReadTimeout = 60000;

  cDefCompressionLevel = 4;
  cDefEncrypt = False;

  cDefPort = 5555;

type

  // эти исключения не нарушают работы программы
  EDCNotInhibitException = class(Exception);

  EDCConnectorException = class(EDCNotInhibitException);
  EDCConnectorBadAddress = class(EDCConnectorException);
  EDCConnectorCommandException = class(EDCConnectorException);
  EDCConnectorUnknownAnswerException = class(EDCConnectorException);
  EDCConnectorOperationCanceledException = class(EDCConnectorException);

  /// класс подключения к серверу Мониторинга
  ///  его наследники реализуют различные протоколы взаимодейтвия с сервером
  TDCCustomConnector = class(TComponent, IDCConnector)
  private
    FClientLock: TCriticalSection;
    FAddress: string;
    FUserName: string;
    FPassword: string;
    FDescription: string;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    procedure SetAddress(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetDescription(const Value: string);
  protected
    function GetEncrypt: boolean; virtual; abstract;
    function GetCompressionLevel: Integer; virtual; abstract;
    function GetConnectTimeOut: Integer; virtual; abstract;
    function GetReadTimeOut: Integer; virtual; abstract;

    procedure SetEncrypt(const Value: boolean); virtual; abstract;
    procedure SetCompressionLevel(const Value: Integer); virtual; abstract;
    procedure SetReadTimeOut(const Value: Integer); virtual; abstract;
    procedure SetConnectTimeOut(const Value: Integer); virtual; abstract;

    function GetConnected: Boolean; virtual; abstract;
    procedure SetConnected(const Value: Boolean); virtual;

    // вызывается при изменении параметров
    procedure DoPropChanged; virtual;

    /// пытаемся подключиться по указанному адресу
    ///  если подключение невозможно вызываем исключение
    procedure TryConnectTo(const aAddrLine: string); virtual; abstract;

    /// перебираем все возможные варианты
    ///  если подключение невозможно вызываем исключение (из последнего варианта)
    procedure TryConnect; virtual;

    /// проверяем подключение перед вызовом методов сервера
    ///  если подключения нет, то пытаемся подключиться
    procedure CheckConnection; virtual;

    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;

    procedure DoServicesConnect; virtual;
    procedure DoServicesDisconnect; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    function SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string; virtual; abstract;

    property ClientLock: TCriticalSection read FClientLock;
  published
    /// параметры подключения в формате Host:Port
    ///  через точку с запятой (;) можно добавить альтернативые адреса: host1:port1;host2:port2;host3:port3
    property Address: string read FAddress write SetAddress;

    // ожидание подключения, мс
    property ConnectTimeOut: Integer read GetConnectTimeOut write SetConnectTimeOut default cDefConnectTimeout;
    // ожидание отклика на команду, мс
    property ReadTimeOut: Integer read GetReadTimeOut write SetReadTimeOut default cDefReadTimeout;

    // уровень сжатия (0 - без сжатия ... 9 - максимальное сжатие)
    property CompressionLevel: Integer read GetCompressionLevel write SetCompressionLevel default cDefCompressionLevel;
    // шифрование
    property Encrypt: boolean read GetEncrypt write SetEncrypt default cDefEncrypt;

    // имя пользователя
    property UserName: string read FUserName write SetUserName;
    // пароль
    property Password: string read FPassword write SetPassword;

    // состояние подключения
    property Connected: Boolean read GetConnected write SetConnected;

    /// как представиться серверу
    property Description: string read FDescription write SetDescription;

    /// события
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

implementation

{ TDCCustomConnector }
procedure TDCCustomConnector.CheckConnection;
begin
  if not Connected then
    DoConnect;
end;

constructor TDCCustomConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientLock := TCriticalSection.Create;
end;

destructor TDCCustomConnector.Destroy;
begin
  FClientLock.Free;
  inherited;
end;

procedure TDCCustomConnector.DoPropChanged;
begin
  if Connected then
    Disconnect;
end;

procedure TDCCustomConnector.DoServicesConnect;
begin

end;

procedure TDCCustomConnector.DoServicesDisconnect;
begin

end;

procedure TDCCustomConnector.SetAddress(const Value: string);
begin
  if Address <> Value then
  begin
    FAddress := Value;
    DoPropChanged;
  end;
end;

procedure TDCCustomConnector.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    if Value then
      Connect
    else
      Disconnect;
  end;
end;

procedure TDCCustomConnector.SetDescription(const Value: string);
begin
  if Description <> Value then
  begin
    FDescription := Value;
    DoPropChanged;
  end;
end;

procedure TDCCustomConnector.SetPassword(const Value: string);
begin
  if Password <> Value then
  begin
    FPassword := Value;
    DoPropChanged;
  end;
end;

procedure TDCCustomConnector.SetUserName(const Value: string);
begin
  if UserName <> Value then
  begin
    FUserName := Value;
    DoPropChanged;
  end;
end;

procedure TDCCustomConnector.TryConnect;
var
  i: Integer;
  aAddressList: TStringList;
begin
  if Address = '' then
    raise EDCConnectorBadAddress.Create(sResAddressIsEmpty);

  aAddressList := TStringList.Create;
  try
    aAddressList.LineBreak := ';';
    aAddressList.Text := Address;
    for i := 0 to aAddressList.Count - 1 do
    begin
      try
        TryConnectTo(aAddressList[i]);
        /// подключение прошло успешно
        ///  передвинем успешные параметры подключения в начало списка для более быстрого переподключения
        if i <> 0 then
        begin
          aAddressList.Move(i, 0);
          FAddress := aAddressList.Text;
        end;
        /// уходим
        Exit;
      except
        on Exception do
        begin
          /// если мы дошли до последнего варианта и так и не смогли подключиться,
          ///  то поднимаем последнее исключение, иначе продолжаем перебор
          if i = aAddressList.Count - 1 then
            raise
        end;
      end;
    end;
  finally
    aAddressList.Free;
  end;
end;

end.
