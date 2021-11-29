unit LA.Net.Connector.Tcp;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  IdGlobal, IdTCPClient, IdException,
  LA.Net.Connector, LA.Net.Intercept.Tcp;

type
  TDCTCPConnector = class(TDCCustomConnector)
  private
    FLock: TCriticalSection;
    FClient: TIdTCPClient;
    FIntercept: TDCTCPIntercept;

    FEncrypt: Boolean;
    FCompressionLevel: Integer;

    FServerFS: TFormatSettings;

    FServerVer: Integer;
    FServerEnableMessage: Boolean;
    FServerSupportingProtocols: string;
    FServerOffsetFromUTC: TDateTime;

    FServerSettingsIsLoaded: Boolean;

    FLanguage: string;
    FProtocolVersion: Integer;
    FEnableMessage: Boolean;
    FClientOffsetFromUTC: TDateTime;


    function LockClient(const aMessage: string = ''): TIdTCPClient;
    procedure UnLockClient(const aMessage: string = '');

    function GenerateCryptKey(const aCharCount: Integer): RawByteString;

    procedure UpdateEncrypted(const aLock: Boolean);
    procedure UpdateComressionLevel(const aLock: Boolean);

    procedure ClearIncomingData;

    procedure SendCommand(const aCommand: string);
    procedure SendCommandFmt(const aCommand: string; const Args: array of TVarRec);

    function ReadLn: string;

    function ExtractValue(var aValues: string; var aValue: string; var aErrorCode: integer; var aErrorStr: string; var aMoment: TDateTime): Boolean;



    procedure DoCommand(const aCommand: string);
    procedure DoCommandFmt(const aCommand: string; const Args: array of TVarRec);
    procedure CheckCommandResult;

    // обработка ошибок TCP
    function ProcessTCPException(const e: EIdException): Boolean;

    procedure LockAndDoCommand(const aCommand: string);
    procedure LockAndDoCommandFmt(const aCommand: string; const Args: array of TVarRec);

    function LockDoCommandReadLn(const aCommand: string): string;
    function LockDoCommandReadLnFmt(const aCommand: string; const Args: array of TVarRec): string;

    function LockAndGetStringsCommand(const aCommand: string): string;


    procedure GetServerSettings;
    procedure SetConnectionParams;

    procedure SetLanguage(const Value: string);
    procedure SetEnableMessage(const Value: Boolean);
    procedure SetProtocolVersion(const Value: Integer);

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

    procedure Authorize(const aUser, aPassword: string); virtual;

    /// пытаемс€ подключитьс€ по указанному адресу
    ///  если подключение невозможно вызываем исключение
    procedure TryConnectTo(const aHost: string; const aPort: Integer); override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;

    property Intercept: TDCTCPIntercept read FIntercept;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// методы потокобезопасны (используют критическую секцию)
    procedure Connect; override;
    procedure Disconnect; override;

    /// методы работы с сервером


    property ServerFS: TFormatSettings read FServerFS;
    property ServerVer: Integer read FServerVer;
    property ServerOffsetFromUTC: TDateTime read FServerOffsetFromUTC;
    property ClientOffsetFromUTC: TDateTime read FClientOffsetFromUTC;

    /// реализаци€ интерфейса IMonitoring
    function SensorValue(const SID: String): String; override;


  published
    /// верси€ протокола
    property ProtocolVersion: Integer read FProtocolVersion write SetProtocolVersion default 30;
    /// сервер может отправл€ть сообщени€ через это подключение
    property EnableMessage: Boolean read FEnableMessage write SetEnableMessage default True;
    /// €зык, на котором сервер будет слать информационные сообщени€ (ошибки и т.д.)
    property Language: string read FLanguage write SetLanguage;

  end;

implementation

uses
  System.Character,
  flcStdTypes, flcCipherRSA,
  LA.Utils.Str, LA.Utils.System,
  LA.Log;

const
  sOk = 'ok';       // not localize
  sError = 'error'; // not localize
  sNoCommandHandler = 'no command handler'; // not localize

  sUnknownAnswer = 'Unknown answer : %s';
  sBadParamCount = 'ѕолучено некорректное количество параметров %d из %d.';



{ TDCTCPConnector }

procedure TDCTCPConnector.Authorize(const aUser, aPassword: string);
begin
  DoCommandFmt('Login %s;%s;1', [aUser, TDCStrUtils.StrToHex(aPassword, '')]);
  ReadLn; // вычитываем приветствие
end;

procedure TDCTCPConnector.CheckCommandResult;
var
  aStatus: string;
begin
  aStatus := ReadLn;
  if aStatus = sError then
    raise EDCConnectorCommandException.Create(ReadLn)
  else if aStatus <> sOk then
    raise EDCConnectorUnknownAnswerException.Create('Ќестандартый ответ на команду');
end;

procedure TDCTCPConnector.ClearIncomingData;
begin
  while not FClient.IOHandler.InputBufferIsEmpty do
  begin
    FClient.IOHandler.InputBuffer.Clear;
    FClient.IOHandler.CheckForDataOnSource(1);
  end;
end;

procedure TDCTCPConnector.Connect;
begin
  LockClient('Connect');
  try
    DoConnect;
  finally
    UnLockClient('Connect');
  end;
end;

constructor TDCTCPConnector.Create(AOwner: TComponent);
begin
  inherited;

  FLock := TCriticalSection.Create;
  FIntercept := TDCTCPIntercept.Create(nil);

  FClient := TIdTCPClient.Create(nil);
  FClient.ConnectTimeout := cDefConnectTimeout;
  FClient.ReadTimeout := cDefReadTimeout;
  FClient.Host := '';
  FClient.Port := cDefPort;

  FClient.Intercept := FIntercept;

  FProtocolVersion := 30;
  FEnableMessage := True;
  FLanguage := 'ru';
end;

destructor TDCTCPConnector.Destroy;
begin
  FClient.Free;
  FIntercept.Free;
  FLock.Free;
  inherited;
end;

procedure TDCTCPConnector.Disconnect;
begin
  LockClient('Connect');
  try
    DoDisconnect;
  finally
    UnLockClient('Connect');
  end;
end;

procedure TDCTCPConnector.DoCommand(const aCommand: string);
begin
  SendCommand(aCommand);
  CheckCommandResult;
end;

procedure TDCTCPConnector.DoCommandFmt(const aCommand: string; const Args: array of TVarRec);
begin
  SendCommandFmt(aCommand, Args);
  CheckCommandResult;
end;

procedure TDCTCPConnector.DoConnect;
begin
  if not FClient.Connected then
    TryConnect
  else
  begin
    try
      // проверим насколько хорош этот коннект
      FClient.CheckForGracefulDisconnect(True);
    except
      // подключаемс€ по новой, если подключени€ нет
      TryConnect;
    end;
  end;
end;

procedure TDCTCPConnector.DoDisconnect;
begin
  FClient.Disconnect;
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(OnDisconnect) then
      OnDisconnect(Self);
  end;

end;

function TDCTCPConnector.ExtractValue(var aValues, aValue: string; var aErrorCode: integer; var aErrorStr: string;
  var aMoment: TDateTime): Boolean;
var
  i, p1: integer;
  s: string;
  aState: (sValue, sErrorCode, sErrorStr, sMoment, sEOL);
  aStrLength: integer;
begin
  // разбираем текст вида:
  //
  // Value;ErrorCode;ErrorStr;Moment<EOL>
  // Value;ErrorCode;ErrorStr;Moment<EOL>
  // <EOL>
  // ...
  // Value;ErrorCode;ErrorStr;Moment<EOL>
  //
  // удал€ем из испходного текста разобранную строку

  i := 1;
  aStrLength := Length(aValues);

  Assert(aStrLength > 0, 'ѕолучена пуста€ строка');

  Result := aValues[i] <> #13;
  if Result then
  begin
    p1 := 1;
    aState := sValue;

    while aState <> sEOL do
    begin
      // EOL = CR + LF  (#13 + #10)

      if (i > aStrLength) or aValues[i].IsInArray([';', #13]) then
      //(CharInSet(aValues[i], [';', #13])) then
      begin
        s := Copy(aValues, p1, i - p1);
        p1 := i + 1;
        case aState of
          sValue:       // значение
            aValue := s;
          sErrorCode:   // код ошибки
            aErrorCode := StrToIntDef(s, 0);
          sErrorStr:    // ошибка
            aErrorStr := s;
          sMoment:      // момент времени
            aMoment := StrToDateTimeDef(s, aMoment); // DateToClient(aMoment), OpcFS);
        end;
        Inc(aState);
      end;
      Inc(i);
    end;
    aValues := Copy(aValues, i + 1 {LF}, aStrLength);
  end
  else
    aValues := Copy(aValues, i + 2 {CRLF}, aStrLength);


end;

function TDCTCPConnector.GenerateCryptKey(const aCharCount: Integer): RawByteString;
var
  i: integer;
begin
  Randomize;

  Result := '';
  for i := 1 to aCharCount do
    Result := Result + ByteChar(Random(256));
end;

function TDCTCPConnector.GetCompressionLevel: Integer;
begin
  Result := FCompressionLevel;
end;

function TDCTCPConnector.GetConnected: Boolean;
begin
  Result := FClient.Connected;
end;

function TDCTCPConnector.GetConnectTimeOut: Integer;
begin
  Result := FClient.ConnectTimeout;
end;

function TDCTCPConnector.GetEncrypt: boolean;
begin
  Result := FEncrypt;
end;

function TDCTCPConnector.GetReadTimeOut: Integer;
begin
  Result := FClient.ReadTimeout;
end;

procedure TDCTCPConnector.GetServerSettings;
var
  aCount: Integer;
  s: TStrings;
  i: Integer;
begin
  try
	  DoCommand('GetServerSettings');
	  aCount := StrToInt(ReadLn);

	  s := TStringList.Create;
	  try
	    for i := 1 to aCount do
        s.Add(ReadLn);

      with FServerFS do
      begin
        ThousandSeparator := s.Values['ThousandSeparator'][low(string)];
        DecimalSeparator := s.Values['DecimalSeparator'][low(string)];
        TimeSeparator := s.Values['TimeSeparator'][low(string)];
        ListSeparator := s.Values['ListSeparator'][low(string)];

        CurrencyString := s.Values['CurrencyString'];
        ShortDateFormat := s.Values['ShortDateFormat'];
        LongDateFormat := s.Values['LongDateFormat'];
        TimeAMString := s.Values['TimeAMString'];
        TimePMString := s.Values['TimePMString'];
        ShortTimeFormat := s.Values['ShortTimeFormat'];
        LongTimeFormat := s.Values['LongTimeFormat'];

        DateSeparator := s.Values['DateSeparator'][low(string)];
      end;

      FServerSettingsIsLoaded := True;

	    FServerVer := StrToInt(s.Values['ServerVer']);
	    FServerEnableMessage := StrToBool(s.Values['EnableMessage']);
	    FServerSupportingProtocols := s.Values['SupportingProtocols'];
      FServerOffsetFromUTC := StrToTimeDef(s.Values['OffsetFromUTC'], ClientOffsetFromUTC);
	  finally
	    s.Free;
	  end;

  except
    on e: EIdException do
      if ProcessTCPException(e) then
        raise;
  end;
end;

procedure TDCTCPConnector.LockAndDoCommand(const aCommand: string);
begin
  LockClient('LockAndDoCommand: ' + aCommand);
  try
    try
	    DoConnect;
	    DoCommand(aCommand);
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
    end;
  finally
    UnLockClient('LockAndDoCommand: ' + aCommand);
  end;
end;

procedure TDCTCPConnector.LockAndDoCommandFmt(const aCommand: string; const Args: array of TVarRec);
begin
  LockAndDoCommand(Format(aCommand, Args));
end;

function TDCTCPConnector.LockAndGetStringsCommand(const aCommand: string): string;
var
  aByteCount: integer;
begin
  // в протоколе V30 многие команды получают в ответ список строк
  // строки разделены симовлом EOL = CR+LF
  // формат ответа на такие команды:
  // ok<EOL>
  // <LineCount><EOL> - количество строк
  // <ByteCount><EOL> - количество байт начина€ с первого символа первой строки и заканчива€ LF последней
  // —трока 1<EOL>
  // —трока 2<EOL>
  // ...
  // —трока N<EOL>

  Result := '';
  LockClient('LockAndGetStringsCommand: ' + aCommand);
  try
    try
      DoConnect;
      DoCommand(aCommand);
      ReadLn;                           // количество строк данных
      aByteCount := StrToInt(ReadLn);   // количество байт данных

      // читаем данные
      Result := FClient.IOHandler.ReadString(aByteCount);
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
    end;
  finally
    UnLockClient('LockAndGetStringsCommand: ' + aCommand);
  end;
end;

function TDCTCPConnector.LockClient(const aMessage: string): TIdTCPClient;
begin
  //OPCLog.WriteToLogFmt('%d: LockClient %s', [GetCurrentThreadId, aMessage]);
  FLock.Enter;
  Result := FClient;
  //OPCLog.WriteToLogFmt('%d: LockClient OK. %s', [GetCurrentThreadId, aMessage]);
end;

function TDCTCPConnector.LockDoCommandReadLn(const aCommand: string): string;
begin
  LockClient('LockDoCommandReadLn');
  try
    try
	    DoConnect;
	    DoCommand(aCommand);
      Result := ReadLn;
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
    end;
  finally
    UnLockClient('LockDoCommandReadLn');
  end;
end;

function TDCTCPConnector.LockDoCommandReadLnFmt(const aCommand: string; const Args: array of TVarRec): string;
begin
  LockClient('LockDoCommandReadLnFmt');
  try
    try
	    DoConnect;
	    DoCommandFmt(aCommand, Args);
      Result := ReadLn;
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
    end;
  finally
    UnLockClient('LockDoCommandReadLnFmt');
  end;
end;

function TDCTCPConnector.ProcessTCPException(const e: EIdException): Boolean;
begin
  Result := True;
  TDCLog.WriteToLog(e.Message);
  DoDisconnect;
end;

function TDCTCPConnector.ReadLn: string;
begin
  Result := FClient.IOHandler.ReadLn;
end;

procedure TDCTCPConnector.SendCommand(const aCommand: string);
begin
  ClearIncomingData;
  FClient.IOHandler.WriteLn(aCommand);
end;

procedure TDCTCPConnector.SendCommandFmt(const aCommand: string; const Args: array of TVarRec);
begin
  SendCommand(Format(aCommand, Args));
end;

function TDCTCPConnector.SensorValue(const SID: String): String;
var
  aStr: String;
  aErrorCode: integer;
  aErrorStr: string;
  aMoment: TDateTime;
begin
  LockClient('SensorValue');
  try
    try
      DoConnect;
      DoCommandFmt('GetValue %s', [SID]);
      aStr := ReadLn;
      ExtractValue(aStr, Result, aErrorCode, aErrorStr, aMoment);
      //Moment := DateToClient(Moment);
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
    end;
  finally
    UnLockClient('SensorValue');
  end;
end;

procedure TDCTCPConnector.SetCompressionLevel(const Value: Integer);
begin
  if CompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    DoPropChanged;
    //UpdateComressionLevel(True);
  end;
end;

procedure TDCTCPConnector.SetConnectionParams;
const
  cStringEncoding = '';  //'UTF8';
  { TODO : проверить, почему не работает передача списка пользователей, если задан UTF8 }
  //cStringEncoding = 'UTF8';
begin
  try
	  DoCommandFmt('SetConnectionParams '+
	    'ProtocolVersion=%d;'+
	    //'CompressionLevel=%d;'+
	    'EnableMessage=%d;'+
	    'Description=%s;'+
	    'SystemLogin=%s;'+
	    'HostName=%s;'+
	    'MaxLineLength=%d;'+
      'Language=%s;'+
      'StringEncoding=%s',
	    [ ProtocolVersion,
	      //CompressionLevel,
	      Ord(EnableMessage),
	      Description,
	      TDCSystemUtils.GetLocalUserName,
	      TDCSystemUtils.GetComputerName,
	      FClient.IOHandler.MaxLineLength,
        Language,
        cStringEncoding
	      ]
	    );

      if (ServerVer >= 3) and (cStringEncoding = 'UTF8') then
        FClient.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
  except
    on e: EIdException do
      if ProcessTCPException(e) then
        raise;
  end;
end;

procedure TDCTCPConnector.SetConnectTimeOut(const Value: Integer);
begin
  if ConnectTimeOut <> Value then
  begin
    FClient.ConnectTimeout := Value;
    DoPropChanged;
  end;
end;

procedure TDCTCPConnector.SetEnableMessage(const Value: Boolean);
begin
  if EnableMessage <> Value then
  begin
    FEnableMessage := Value;
    DoPropChanged;
  end;
end;

procedure TDCTCPConnector.SetEncrypt(const Value: boolean);
begin
  if Encrypt <> Value then
  begin
    FEncrypt := Value;
    DoPropChanged;
    //UpdateEncrypted(True);
  end;
end;


procedure TDCTCPConnector.SetLanguage(const Value: string);
begin
  if Language <> Value then
  begin
    FLanguage := Value;
    DoPropChanged;
  end;
end;

procedure TDCTCPConnector.SetProtocolVersion(const Value: Integer);
begin
  if ProtocolVersion <> Value then
  begin
    FProtocolVersion := Value;
    DoPropChanged;
  end;
end;

procedure TDCTCPConnector.SetReadTimeOut(const Value: Integer);
begin
  if ReadTimeOut <> Value then
  begin
    FClient.ReadTimeOut := Value;
    DoPropChanged;
  end;

end;

procedure TDCTCPConnector.TryConnectTo(const aHost: string; const aPort: Integer);
begin
  // отключаем шифрование и сжатие
  Intercept.CryptKey := '';
  Intercept.CompressionLevel := 0;

  // устанавливаем соединение
  FClient.Disconnect;
  FClient.Host := aHost;
  FClient.Port := aPort;
  FClient.Connect;
  // провер€ем наличие соединени€
  FClient.CheckForGracefulDisconnect(true);
  // устанавливаем параметры кодировани€ строк и максимальную длину строки
  if Assigned(FClient.IOHandler) then
  begin
    FClient.IOHandler.DefStringEncoding := IndyTextEncoding_OSDefault;
    FClient.IOHandler.MaxLineLength := 100 * (16 * 1024);
  end;

  // получаем параметры сервера
  GetServerSettings;
  // передаем на сервер информацию о подключении
  SetConnectionParams;

  // устанавливаем шифрование и сжатие
  if Encrypt then
    UpdateEncrypted(False);
  if CompressionLevel > 0 then
    UpdateComressionLevel(False);

  // авторизуемс€
  if UserName <> '' then
  begin
    try
      Authorize(UserName, Password);
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
      on e: Exception do
        ;
    end;
  end;
end;

procedure TDCTCPConnector.UnLockClient(const aMessage: string);
begin
  //OPCLog.WriteToLogFmt('%d: UnLockClient %s', [GetCurrentThreadId, aMessage]);
  FLock.Leave;
  //OPCLog.WriteToLogFmt('%d: UnLockClient OK. %s', [GetCurrentThreadId, aMessage]);
end;

procedure TDCTCPConnector.UpdateComressionLevel(const aLock: Boolean);
begin
  if Connected then
  begin
    if aLock then
      LockClient('UpdateComressionLevel');
    try
      try
	      DoCommandFmt('SetConnectionParams CompressionLevel=%d', [CompressionLevel]);
      except
        on e: EIdException do
          if ProcessTCPException(e) then
            raise;
      end;
    finally
      if aLock then
        UnLockClient('UpdateComressionLevel');
    end;
  end;

  FIntercept.CompressionLevel := CompressionLevel;
end;

procedure TDCTCPConnector.UpdateEncrypted(const aLock: Boolean);
var
  aCode: RawByteString;
  aCryptKey: RawByteString;
  aModulus, aExponent: string;
  aPub: TRSAPublicKey;
begin
  if Encrypt then
    aCryptKey := GenerateCryptKey(16)
  else
    aCryptKey := '';

  if Connected then
  begin
    if aLock then
      LockClient('UpdateEncrypted');

    try
      try
        // нова€ верси€ RSA
        DoCommand('GetPublicKey2');
        aModulus := ReadLn;
        aExponent := ReadLn;

        RSAPublicKeyInit(aPub);
        RSAPublicKeyAssignHex(aPub, 256, aModulus, aExponent);
        aCode := RSAEncryptStr(rsaetRSAES_PKCS1, aPub, aCryptKey);
        RSAPublicKeyFinalise(aPub);

        DoCommandFmt('SetCryptKey2 %s', [TDCStrUtils.StrToHex(aCode, '')]);
      except
        on e: EIdException do
          if ProcessTCPException(e) then
            raise;
      end;
    finally
      if aLock then
        UnLockClient('UpdateEncrypted');
    end;
  end;
  FIntercept.CryptKey := aCryptKey;
end;

end.
