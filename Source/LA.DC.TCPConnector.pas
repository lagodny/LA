unit LA.DC.TCPConnector;

interface

uses
  System.Classes, System.SyncObjs,
  IdTCPClient, IdException,
  LA.DC.CustomConnector, LA.DC.TCPIntercept;

type
  TDCTCPConnector = class(TDCCustomConnector)
  private
    FLock: TCriticalSection;
    FClient: TIdTCPClient;
    FIntercept: TDCTCPIntercept;

    FEncrypt: boolean;
    FCompressionLevel: Integer;

    function LockClient(const aMessage: string = ''): TIdTCPClient;
    procedure UnLockClient(const aMessage: string = '');

    function GenerateCryptKey(const aCharCount: Integer): RawByteString;

    procedure UpdateEncrypted(const aLock: Boolean);
    procedure UpdateComressionLevel(const aLock: Boolean);

    procedure ClearIncomingData;

    procedure SendCommand(aCommand: string);
    procedure SendCommandFmt(aCommand: string; const Args: array of TVarRec);

    function ReadLn: string;


    procedure DoCommand(aCommand: string);
    procedure DoCommandFmt(aCommand: string; const Args: array of TVarRec);
    procedure CheckCommandResult;

  protected
    function GetEncrypt: boolean; override;
    function GetCompressionLevel: Integer; override;
    function GetConnectTimeOut: Integer; override;
    function GetReadTimeOut: Integer; override;

    procedure SetEncrypt(const Value: boolean); override;
    procedure SetCompressionLevel(const Value: Integer); override;
    procedure SetReadTimeOut(const Value: Integer); override;
    procedure SetConnectTimeOut(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  flcStdTypes, flcCipherRSA;

const
  sOk = 'ok';       // not localize
  sError = 'error'; // not localize
  sNoCommandHandler = 'no command handler'; // not localize

  sUnknownAnswer = 'Unknown answer : %s';
  sBadParamCount = 'Получено некорректное количество параметров %d из %d.';



{ TDCTCPConnector }

procedure TDCTCPConnector.CheckCommandResult;
var
  aStatus: string;
begin
  aStatus := ReadLn;
  if aStatus = sError then
    raise EDCConnectorCommandException.Create(ReadLn)
  else if aStatus <> sOk then
    raise EDCConnectorUnknownAnswerException.Create('Нестандартый ответ на команду');
end;

procedure TDCTCPConnector.ClearIncomingData;
begin
  while not FClient.IOHandler.InputBufferIsEmpty do
  begin
    FClient.IOHandler.InputBuffer.Clear;
    FClient.IOHandler.CheckForDataOnSource(1);
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
end;

destructor TDCTCPConnector.Destroy;
begin
  FClient.Free;
  FIntercept.Free;
  FLock.Free;
  inherited;
end;

procedure TDCTCPConnector.DoCommand(aCommand: string);
begin
  SendCommand(aCommand);
  CheckCommandResult;
end;

procedure TDCTCPConnector.DoCommandFmt(aCommand: string; const Args: array of TVarRec);
begin
  SendCommandFmt(aCommand, Args);
  CheckCommandResult;
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

function TDCTCPConnector.LockClient(const aMessage: string): TIdTCPClient;
begin
  //OPCLog.WriteToLogFmt('%d: LockClient %s', [GetCurrentThreadId, aMessage]);
  FLock.Enter;
  Result := FClient;
  //OPCLog.WriteToLogFmt('%d: LockClient OK. %s', [GetCurrentThreadId, aMessage]);
end;

function TDCTCPConnector.ReadLn: string;
begin
  Result := FClient.IOHandler.ReadLn;
end;

procedure TDCTCPConnector.SendCommand(aCommand: string);
begin
  ClearIncomingData;
  FClient.IOHandler.WriteLn(aCommand);
end;

procedure TDCTCPConnector.SendCommandFmt(aCommand: string; const Args: array of TVarRec);
begin
  SendCommand(Format(aCommand, Args));
end;

procedure TDCTCPConnector.SetCompressionLevel(const Value: Integer);
begin
  inherited;

end;

procedure TDCTCPConnector.SetConnectTimeOut(const Value: Integer);
begin
  if ConnectTimeOut <> Value then
  begin
    inherited;
    FClient.ConnectTimeout := Value;
  end;
end;

procedure TDCTCPConnector.SetEncrypt(const Value: boolean);
begin
  if Encrypt <> Value then
  begin
    FEncrypt := Value;
    UpdateEncrypted(True);
  end;
end;


procedure TDCTCPConnector.SetReadTimeOut(const Value: Integer);
begin
  if ReadTimeOut <> Value then
  begin
    inherited;
    FClient.ReadTimeOut := Value;
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
//  if Encrypt then
//    aCryptKey := GenerateCryptKey(16)
//  else
//    aCryptKey := '';
//
//  if Connected then
//  begin
//    if aLock then
//      LockClient('UpdateEncrypted');
//
//    try
//      try
//        if FServerVer >= 4 then
//        begin
//          // новая версия RSA
//          DoCommand('GetPublicKey2');
//          aModulus := ReadLn;
//          aExponent := ReadLn;
//
//          RSAPublicKeyInit(aPub);
//          RSAPublicKeyAssignHex(aPub, 256, aModulus, aExponent);
//          aCode := RSAEncryptStr(rsaetPKCS1, aPub, aCryptKey);
//          RSAPublicKeyFinalise(aPub);
//
//          DoCommandFmt('SetCryptKey2 %s', [StrToHex(aCode, '')]);
//        end
//        else
//        begin
//          // для старых серверов шифрование будет ОТКЛЮЧЕНО
//          aCryptKey := '';
//
////          DoCommand('GetPublicKey');
////          Base10StringToFGInt(ReadLn, aRSA_e);
////          Base10StringToFGInt(ReadLn, aRSA_n);
////
////          FGIntRSA.RSAEncrypt(aCryptKey, aRSA_e, aRSA_n, aCode);
////
////  	      DoCommandFmt('SetCryptKey %s', [StrToHex(aCode, '')]);
//        end;
//      except
//        on e: EIdException do
//          if ProcessTCPException(e) then
//            raise;
//      end;
//    finally
//      if aLock then
//        UnLock('UpdateEncrypted');
//    end;
//  end;
//  Intercept.CryptKey := aCryptKey;
end;

end.
