unit LA.Net.Connector.Tcp;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  IdGlobal, IdTCPClient, IdException,
  LA.Net.Connector, LA.Types.Monitoring,
  LA.Net.Intercept.Tcp;

type
  TDCTcpAddr = record
    Host, Port: string;
    function InitFrom(const aAddr: string): TDCTcpAddr;
  end;

  TLATCPConnector = class(TLACustomConnector)
  private
    FLock: TCriticalSection;
    FClient: TIdTCPClient;
    FIntercept: TLATCPIntercept;

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

//    function ExtractValue(var aValues: string; var aValue: string; var aErrorCode: integer; var aErrorStr: string; var aMoment: TDateTime): Boolean;



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
    function GetSendTimeOut: Integer; override;

    function GetConnectTimeOut: Integer; override;
    function GetReadTimeOut: Integer; override;

    procedure SetEncrypt(const Value: boolean); override;
    procedure SetCompressionLevel(const Value: Integer); override;

    procedure SetReadTimeOut(const Value: Integer); override;
    procedure SetConnectTimeOut(const Value: Integer); override;
    procedure SetSendTimeOut(const Value: Integer); override;

    function GetConnected: Boolean; override;

    /// пытаемся подключиться по указанному адресу
    ///  если подключение невозможно вызываем исключение
    procedure TryConnectTo(const aAddrLine: string); override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;

    procedure DoAuthorize; override;


    property Intercept: TLATCPIntercept read FIntercept;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// методы потокобезопасны (используют критическую секцию)
    procedure Connect; override;
    procedure Disconnect; override;

    procedure Authorize; override;

    /// методы работы с сервером
    function SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string; override;
    procedure SensorHistoryStream(aStream: TStream; const SID: string; const FromDate, ToDate: Int64;
      const GetValue, GetStatus, GetUser: Boolean; const CalcLeft, CalcRight: Boolean); override;
    function GetSensorsInfo(const IDs: TSIDArr): Variant; override;
    function GetLookup(const aName: string): string; override;


    property ServerFS: TFormatSettings read FServerFS;
    property ServerVer: Integer read FServerVer;
    property ServerOffsetFromUTC: TDateTime read FServerOffsetFromUTC;
    property ClientOffsetFromUTC: TDateTime read FClientOffsetFromUTC;

//    /// реализация интерфейса IMonitoring
//    function SensorValue(const SID: String): String; override;
//    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr; override;


  published
    /// версия протокола
    property ProtocolVersion: Integer read FProtocolVersion write SetProtocolVersion default 30;
    /// сервер может отправлять сообщения через это подключение
    property EnableMessage: Boolean read FEnableMessage write SetEnableMessage default True;
    /// язык, на котором сервер будет слать информационные сообщения (ошибки и т.д.)
    property Language: string read FLanguage write SetLanguage;

  end;

implementation

uses
  System.Character,
  System.DateUtils,
  flcStdTypes, flcCipherRSA,
  LA.Utils.Str, LA.Utils.System,
  LA.Log;

const
  sOk = 'ok';       // not localize
  sError = 'error'; // not localize
  sNoCommandHandler = 'no command handler'; // not localize

  sUnknownAnswer = 'Unknown answer : %s';
  sBadParamCount = 'Получено некорректное количество параметров %d из %d.';



{ TDCTCPConnector }

procedure TLATCPConnector.Authorize; //(const aUser, aPassword: string);
begin
  Lock;
  try
    DoAuthorize;
  finally
    Unlock;
  end;
end;

procedure TLATCPConnector.CheckCommandResult;
var
  aStatus: string;
begin
  aStatus := ReadLn;
  if aStatus = sError then
    raise EDCConnectorCommandException.Create(ReadLn)
  else if aStatus <> sOk then
    raise EDCConnectorUnknownAnswerException.Create('Нестандартый ответ на команду');
end;

procedure TLATCPConnector.ClearIncomingData;
begin
  while not FClient.IOHandler.InputBufferIsEmpty do
  begin
    FClient.IOHandler.InputBuffer.Clear;
    FClient.IOHandler.CheckForDataOnSource(1);
  end;
end;

procedure TLATCPConnector.Connect;
begin
  LockClient('Connect');
  try
    DoConnect;
  finally
    UnLockClient('Connect');
  end;
end;

constructor TLATCPConnector.Create(AOwner: TComponent);
begin
  inherited;

  FLock := TCriticalSection.Create;
  FIntercept := TLATCPIntercept.Create(nil);

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

destructor TLATCPConnector.Destroy;
begin
  FClient.Free;
  FIntercept.Free;
  FLock.Free;
  inherited;
end;

procedure TLATCPConnector.Disconnect;
begin
  LockClient('Connect');
  try
    DoDisconnect;
  finally
    UnLockClient('Connect');
  end;
end;

procedure TLATCPConnector.DoAuthorize;
begin
  DoCommandFmt('Login %s;%s;1', [UserName, TLAStrUtils.StrToHex(Password, '')]);
  ReadLn; // вычитываем приветствие
end;

procedure TLATCPConnector.DoCommand(const aCommand: string);
begin
  SendCommand(aCommand);
  CheckCommandResult;
end;

procedure TLATCPConnector.DoCommandFmt(const aCommand: string; const Args: array of TVarRec);
begin
  SendCommandFmt(aCommand, Args);
  CheckCommandResult;
end;

procedure TLATCPConnector.DoConnect;
begin
  if not FClient.Connected then
    TryConnect
  else
  begin
    try
      // проверим насколько хорош этот коннект
      FClient.CheckForGracefulDisconnect(True);
    except
      // подключаемся по новой, если подключения нет
      TryConnect;
    end;
  end;
end;

procedure TLATCPConnector.DoDisconnect;
begin
  FClient.Disconnect;
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(OnDisconnect) then
      OnDisconnect(Self);
  end;

end;

//function TLATCPConnector.ExtractValue(var aValues, aValue: string; var aErrorCode: integer; var aErrorStr: string;
//  var aMoment: TDateTime): Boolean;
//var
//  i, p1: integer;
//  s: string;
//  aState: (sValue, sErrorCode, sErrorStr, sMoment, sEOL);
//  aStrLength: integer;
//begin
//  // разбираем текст вида:
//  //
//  // Value;ErrorCode;ErrorStr;Moment<EOL>
//  // Value;ErrorCode;ErrorStr;Moment<EOL>
//  // <EOL>
//  // ...
//  // Value;ErrorCode;ErrorStr;Moment<EOL>
//  //
//  // удаляем из испходного текста разобранную строку
//
//  i := 1;
//  aStrLength := Length(aValues);
//
//  Assert(aStrLength > 0, 'Получена пустая строка');
//
//  Result := aValues[i] <> #13;
//  if Result then
//  begin
//    p1 := 1;
//    aState := sValue;
//
//    while aState <> sEOL do
//    begin
//      // EOL = CR + LF  (#13 + #10)
//
//      if (i > aStrLength) or aValues[i].IsInArray([';', #13]) then
//      //(CharInSet(aValues[i], [';', #13])) then
//      begin
//        s := Copy(aValues, p1, i - p1);
//        p1 := i + 1;
//        case aState of
//          sValue:       // значение
//            aValue := s;
//          sErrorCode:   // код ошибки
//            aErrorCode := StrToIntDef(s, 0);
//          sErrorStr:    // ошибка
//            aErrorStr := s;
//          sMoment:      // момент времени
//            aMoment := StrToDateTimeDef(s, aMoment); // DateToClient(aMoment), OpcFS);
//        end;
//        Inc(aState);
//      end;
//      Inc(i);
//    end;
//    aValues := Copy(aValues, i + 1 {LF}, aStrLength);
//  end
//  else
//    aValues := Copy(aValues, i + 2 {CRLF}, aStrLength);
//
//
//end;

function TLATCPConnector.GenerateCryptKey(const aCharCount: Integer): RawByteString;
var
  i: integer;
begin
  Randomize;

  Result := '';
  for i := 1 to aCharCount do
    Result := Result + ByteChar(Random(256));
end;

function TLATCPConnector.GetCompressionLevel: Integer;
begin
  Result := FCompressionLevel;
end;

function TLATCPConnector.GetConnected: Boolean;
begin
  Result := FClient.Connected;
end;

function TLATCPConnector.GetConnectTimeOut: Integer;
begin
  Result := FClient.ConnectTimeout;
end;

function TLATCPConnector.GetEncrypt: boolean;
begin
  Result := FEncrypt;
end;

function TLATCPConnector.GetReadTimeOut: Integer;
begin
  Result := FClient.ReadTimeout;
end;

function TLATCPConnector.GetLookup(const aName: string): string;
begin
  Result := LockAndGetStringsCommand(Format('GetLookup %s', [aName]));
end;

function TLATCPConnector.GetSendTimeOut: Integer;
begin
  Result := 0;
end;

function TLATCPConnector.GetSensorsInfo(const IDs: TSIDArr): Variant;
begin
  Result := LockDoCommandReadLnFmt('GetSensorsInfo %s', [String.Join(';', IDs)]);
end;

procedure TLATCPConnector.GetServerSettings;
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

//function TDCTCPConnector.GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
//begin
//
//end;

procedure TLATCPConnector.LockAndDoCommand(const aCommand: string);
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

procedure TLATCPConnector.LockAndDoCommandFmt(const aCommand: string; const Args: array of TVarRec);
begin
  LockAndDoCommand(Format(aCommand, Args));
end;

function TLATCPConnector.LockAndGetStringsCommand(const aCommand: string): string;
var
  aByteCount: integer;
begin
  // в протоколе V30 многие команды получают в ответ список строк
  // строки разделены симовлом EOL = CR+LF
  // формат ответа на такие команды:
  // ok<EOL>
  // <LineCount><EOL> - количество строк
  // <ByteCount><EOL> - количество байт начиная с первого символа первой строки и заканчивая LF последней
  // Строка 1<EOL>
  // Строка 2<EOL>
  // ...
  // Строка N<EOL>

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

function TLATCPConnector.LockClient(const aMessage: string): TIdTCPClient;
begin
  //OPCLog.WriteToLogFmt('%d: LockClient %s', [GetCurrentThreadId, aMessage]);
  FLock.Enter;
  Result := FClient;
  //OPCLog.WriteToLogFmt('%d: LockClient OK. %s', [GetCurrentThreadId, aMessage]);
end;

function TLATCPConnector.LockDoCommandReadLn(const aCommand: string): string;
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

function TLATCPConnector.LockDoCommandReadLnFmt(const aCommand: string; const Args: array of TVarRec): string;
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

function TLATCPConnector.ProcessTCPException(const e: EIdException): Boolean;
begin
  Result := True;
  TDCLog.WriteToLog(e.Message);
  DoDisconnect;
end;

function TLATCPConnector.ReadLn: string;
begin
  Result := FClient.IOHandler.ReadLn;
end;

procedure TLATCPConnector.SendCommand(const aCommand: string);
begin
  ClearIncomingData;
  FClient.IOHandler.WriteLn(aCommand);
end;

procedure TLATCPConnector.SendCommandFmt(const aCommand: string; const Args: array of TVarRec);
begin
  SendCommand(Format(aCommand, Args));
end;

procedure TLATCPConnector.SensorHistoryStream(aStream: TStream; const SID: string; const FromDate, ToDate: Int64; const GetValue, GetStatus, GetUser,
  CalcLeft, CalcRight: Boolean);
begin
  Assert(Assigned(aStream), 'Не создан поток');

  Lock;
  try
    try
      DoConnect;

      DoCommand('GetHistory ' +
        SID + ';' +
//        DateTimeToStr(UnixToDateTime(FromDate, True)) + ';' +
//        DateTimeToStr(UnixToDateTime(ToDate, True)) + ';' +
        FromDate.ToString + ';' +
        ToDate.ToString + ';' +
        BoolToStr(GetStatus) + ';' +
        BoolToStr(GetValue) + ';' +
        BoolToStr(GetUser) + ';' +
        BoolToStr(CalcLeft) + ';' +
        BoolToStr(CalcRight) + ';' +
        BoolToStr(True) // время UTC
        );

      FClient.IOHandler.ReadStream(aStream);

//	    // переводим даты в наше смещение
//      HistoryDateToClient(aStream, aDataKindSet);

      aStream.Position := 0;
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
    end;
  finally
    UnLock;
  end;

end;

function TLATCPConnector.SensorsDataAsText(const IDs: TSIDArr; aUseCache: Boolean): string;
begin
  Result := LockAndGetStringsCommand(Format('GetValues2 %s', [String.Join(';', IDs)]));
end;

//function TDCTCPConnector.SensorValue(const SID: String): String;
//var
//  aStr: String;
//  aErrorCode: integer;
//  aErrorStr: string;
//  aMoment: TDateTime;
//begin
//  LockClient('SensorValue');
//  try
//    try
//      DoConnect;
//      DoCommandFmt('GetValue %s', [SID]);
//      aStr := ReadLn;
//      ExtractValue(aStr, Result, aErrorCode, aErrorStr, aMoment);
//      //Moment := DateToClient(Moment);
//    except
//      on e: EIdException do
//        if ProcessTCPException(e) then
//          raise;
//    end;
//  finally
//    UnLockClient('SensorValue');
//  end;
//end;

procedure TLATCPConnector.SetCompressionLevel(const Value: Integer);
begin
  if CompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    DoPropChanged;
    //UpdateComressionLevel(True);
  end;
end;

procedure TLATCPConnector.SetConnectionParams;
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
      'StringEncoding=%s;'+
      'ValueDataSize=%d',
	    [ ProtocolVersion,
	      //CompressionLevel,
	      Ord(EnableMessage),
	      Description,
	      TLASystemUtils.GetLocalUserName,
	      TLASystemUtils.GetComputerName,
	      FClient.IOHandler.MaxLineLength,
        Language,
        cStringEncoding,
        8
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

procedure TLATCPConnector.SetConnectTimeOut(const Value: Integer);
begin
  if ConnectTimeOut <> Value then
  begin
    FClient.ConnectTimeout := Value;
    DoPropChanged;
  end;
end;

procedure TLATCPConnector.SetEnableMessage(const Value: Boolean);
begin
  if EnableMessage <> Value then
  begin
    FEnableMessage := Value;
    DoPropChanged;
  end;
end;

procedure TLATCPConnector.SetEncrypt(const Value: boolean);
begin
  if Encrypt <> Value then
  begin
    FEncrypt := Value;
    DoPropChanged;
    //UpdateEncrypted(True);
  end;
end;


procedure TLATCPConnector.SetLanguage(const Value: string);
begin
  if Language <> Value then
  begin
    FLanguage := Value;
    DoPropChanged;
  end;
end;

procedure TLATCPConnector.SetProtocolVersion(const Value: Integer);
begin
  if ProtocolVersion <> Value then
  begin
    FProtocolVersion := Value;
    DoPropChanged;
  end;
end;

procedure TLATCPConnector.SetReadTimeOut(const Value: Integer);
begin
  if ReadTimeOut <> Value then
  begin
    FClient.ReadTimeOut := Value;
    DoPropChanged;
  end;

end;

procedure TLATCPConnector.SetSendTimeOut(const Value: Integer);
begin

end;

procedure TLATCPConnector.TryConnectTo(const aAddrLine: string);
var
  aAddrRec: TDCTcpAddr;
begin
  // отключаем шифрование и сжатие
  Intercept.CryptKey := '';
  Intercept.CompressionLevel := 0;

  // устанавливаем соединение
  FClient.Disconnect;

  aAddrRec.InitFrom(aAddrLine);
  FClient.Host := aAddrRec.Host;
  FClient.Port := StrToInt(aAddrRec.Port);

  FClient.Connect;
  // проверяем наличие соединения
  FClient.CheckForGracefulDisconnect(true);
  // устанавливаем параметры кодирования строк и максимальную длину строки
  if Assigned(FClient.IOHandler) then
  begin
    FClient.IOHandler.DefStringEncoding := IndyTextEncoding_OSDefault;
    FClient.IOHandler.MaxLineLength := 1000 * (16 * 1024);
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

  // авторизуемся
  if UserName <> '' then
  begin
    try
      DoAuthorize; //(UserName, Password);
    except
      on e: EIdException do
        if ProcessTCPException(e) then
          raise;
      on e: Exception do
        ;
    end;
  end;
end;

//procedure TDCTCPConnector.TryConnectTo(const aHost: string; const aPort: Integer);
//begin
//  // отключаем шифрование и сжатие
//  Intercept.CryptKey := '';
//  Intercept.CompressionLevel := 0;
//
//  // устанавливаем соединение
//  FClient.Disconnect;
//  FClient.Host := aHost;
//  FClient.Port := aPort;
//  FClient.Connect;
//  // проверяем наличие соединения
//  FClient.CheckForGracefulDisconnect(true);
//  // устанавливаем параметры кодирования строк и максимальную длину строки
//  if Assigned(FClient.IOHandler) then
//  begin
//    FClient.IOHandler.DefStringEncoding := IndyTextEncoding_OSDefault;
//    FClient.IOHandler.MaxLineLength := 100 * (16 * 1024);
//  end;
//
//  // получаем параметры сервера
//  GetServerSettings;
//  // передаем на сервер информацию о подключении
//  SetConnectionParams;
//
//  // устанавливаем шифрование и сжатие
//  if Encrypt then
//    UpdateEncrypted(False);
//  if CompressionLevel > 0 then
//    UpdateComressionLevel(False);
//
//  // авторизуемся
//  if UserName <> '' then
//  begin
//    try
//      Authorize(UserName, Password);
//    except
//      on e: EIdException do
//        if ProcessTCPException(e) then
//          raise;
//      on e: Exception do
//        ;
//    end;
//  end;
//end;

procedure TLATCPConnector.UnLockClient(const aMessage: string);
begin
  //OPCLog.WriteToLogFmt('%d: UnLockClient %s', [GetCurrentThreadId, aMessage]);
  FLock.Leave;
  //OPCLog.WriteToLogFmt('%d: UnLockClient OK. %s', [GetCurrentThreadId, aMessage]);
end;

procedure TLATCPConnector.UpdateComressionLevel(const aLock: Boolean);
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

procedure TLATCPConnector.UpdateEncrypted(const aLock: Boolean);
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
        // новая версия RSA
        DoCommand('GetPublicKey2');
        aModulus := ReadLn;
        aExponent := ReadLn;

        RSAPublicKeyInit(aPub);
        RSAPublicKeyAssignHex(aPub, 256, aModulus, aExponent);
        aCode := RSAEncryptStr(rsaetRSAES_PKCS1, aPub, aCryptKey);
        RSAPublicKeyFinalise(aPub);

        DoCommandFmt('SetCryptKey2 %s', [TLAStrUtils.StrToHex(aCode, '')]);
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

{ TDCTcpAddr }

function TDCTcpAddr.InitFrom(const aAddr: string): TDCTcpAddr;
var
  aParams: TStrings;
begin
  aParams := TStringList.Create;
  try
    aParams.LineBreak := ':';
    aParams.Text := aAddr;

    if aParams.Count = 2 then
    begin
      // dc.tdc.org.ua:5152
      Host := aParams[0];
      Port := aParams[1];
    end
    else
      raise EDCConnectorBadAddress.CreateFmt(sResAddressIsBadFmt, [aAddr]);
  finally
    aParams.Free;
  end;
end;

end.
