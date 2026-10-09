unit LA.Mail;

interface

uses
  Classes,
  System.SysUtils,
  IdMessage, IdSMTP, IdText, IdAttachmentFile,
  IdSSLOpenSSL, IdExplicitTLSClientServerBase;

resourcestring
  sNoRecipients = 'No recipients';

//procedure SendMail(
//  const aHost: string; aPort: Integer;
//  const aUsername, aPassword, aSubject, aContent, aFileNames, aRecipients: string; const aFrom: string = '');

procedure SendMailTLS(
  aHost: string; aPort: Integer;
  aAuth: Integer; aTLS: Boolean;
  const aUsername, aPassword, aSubject, aContent, aFileNames, aRecipients: string; aFrom: string);


implementation

uses
  aOPCLog;

procedure SendMailTLS(
  aHost: string; aPort: Integer;
  aAuth: Integer; aTLS: Boolean;
  const aUsername, aPassword, aSubject, aContent, aFileNames, aRecipients: string; aFrom: string);
var
  aSMTP: TIdSMTP;
  aSSL: TIdSSLIOHandlerSocketOpenSSL;
  aIdMessage: TIdMessage;
  i: Integer;
  aText: TIdText;
  aFiles: TStrings;
begin
  if aRecipients = '' then
    raise Exception.Create(sNoRecipients);

  aFiles := TStringList.Create;
  aSMTP := TIdSMTP.Create(nil);
  aIdMessage := TIdMessage.Create(nil);
  try
    aSMTP.Host := aHost;
    aSMTP.Port := aPort;
    aSMTP.AuthType := TIdSMTPAuthenticationType(aAuth);
    aSMTP.Username := aUsername;
    aSMTP.Password := aPassword;

    aSSL := nil;
    if aTLS then
    begin
      aSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      aSSL.DefaultPort := 0;
      aSSL.SSLOptions.Method := sslvTLSv1;
      aSSL.SSLOptions.Mode := sslmClient;

      aSMTP.IOHandler := aSSL;
      aSMTP.UseTLS := utUseExplicitTLS;
    end;


    aSMTP.ConnectTimeout := 10000;
    aSMTP.ReadTimeout := 10000;

    aFiles.LineBreak := ';';
    aFiles.Text := aFileNames;
    try
      OPCLog.WriteToLogFmt('Підключаємось %s:%d...',[aSMTP.Host, aSMTP.Port]);
      aSMTP.Connect;

      //aIdMessage.OnInitializeISO := OnInitISO;

      //aIdMessage.ContentType := 'text/html; charset=windows-1251';
      aIdMessage.ContentType := 'multipart/mixed; charset=windows-1251';
      //aIdMessage.ContentType := 'multipart/alternative; charset=windows-1251';
      aIdMessage.ContentTransferEncoding := '8bit';

      aIdMessage.Subject := aSubject; //'X-Ray (E1) Shift Report';
      aIdMessage.From.Text := aFrom;

      aIdMessage.Recipients.EMailAddresses := aRecipients;
      //aIdMessage.Body.Text := aContent;
      aIdMessage.Date := Now;

      aText := TIdText.Create(aIdMessage.MessageParts);
      aText.ContentType := 'text/html; charset=windows-1251';
      aText.Body.Text := aContent;

      // прикрепляем файлы
      for i := 0 to aFiles.Count - 1 do
      begin
        if aFiles.Strings[i] <> '' then
        begin
          TIdAttachmentFile.Create(aIdMessage.MessageParts, aFiles.Strings[i]);
        end;
      end;

      OPCLog.WriteToLog('Надсилаємо листа...');
      aSMTP.Send(aIdMessage);

      OPCLog.WriteToLog('ОK.');
      aSMTP.Disconnect;
    except
      on e: Exception do
      begin
        OPCLog.WriteToLog(e.ClassName + ' : ' + e.Message);
        raise;
      end;
    end;

  finally
    FreeAndNil(aFiles);
    FreeAndNil(aText);
    FreeAndNil(aIdMessage);
    FreeAndNil(aSSL);
    FreeAndNil(aSMTP);
  end;
end;

type
  TDumyObject = class
  public
    procedure IdSMTP1FailedRecipient(Sender: TObject; const AAddress, ACode, AText: string; var VContinue: Boolean);
  end;


procedure SendMail(const aHost: string; aPort: Integer;
  const aUsername, aPassword, aSubject, aContent, aFileNames, aRecipients: string; const aFrom: string = '');
var
  aSMTP: TIdSMTP;
  aIdMessage: TIdMessage;
  i: Integer;
  aText: TIdText;
  aFiles: TStrings;
  aDumy: TDumyObject;
begin
  if aRecipients = '' then
    raise Exception.Create(sNoRecipients);

  aDumy := TDumyObject.Create;
  aFiles := TStringList.Create;
  aSMTP := TIdSMTP.Create(nil);
  aIdMessage := TIdMessage.Create(nil);
  try
    aSMTP.Host := aHost;
    aSMTP.Port := aPort;
    aSMTP.AuthType := satDefault;  //satNone;// satDefault;
    aSMTP.Username := aUsername;
    aSMTP.Password := aPassword;

    aSMTP.ConnectTimeout := 10000;
    aSMTP.ReadTimeout := 10000;

    aSMTP.OnFailedRecipient := aDumy.IdSMTP1FailedRecipient;

    aFiles.LineBreak := ';';
    aFiles.Text := aFileNames;
    try
      OPCLog.WriteToLogFmt('Поключаемся %s:%d...',[aSMTP.Host, aSMTP.Port]);
      aSMTP.Connect;

      //aIdMessage.OnInitializeISO := OnInitISO;

      //aIdMessage.ContentType := 'text/html; charset=windows-1251';
      aIdMessage.ContentType := 'multipart/mixed; charset=windows-1251';
      //aIdMessage.ContentType := 'multipart/alternative; charset=windows-1251';
      aIdMessage.ContentTransferEncoding := '8bit';

      aIdMessage.Subject := aSubject; //'X-Ray (E1) Shift Report';
//      aIdMessage.From.Text := aUsername;
      aIdMessage.From.Text := aFrom;

      aIdMessage.Recipients.EMailAddresses := aRecipients;
      //aIdMessage.Body.Text := aContent;
      aIdMessage.Date := Now;

      aText := TIdText.Create(aIdMessage.MessageParts);
      aText.ContentType := 'text/html; charset=windows-1251';
      aText.Body.Text := aContent;

      // прикрепляем файлы
      for i := 0 to aFiles.Count - 1 do
      begin
        if aFiles.Strings[i] <> '' then
        begin
          TIdAttachmentFile.Create(aIdMessage.MessageParts, aFiles.Strings[i]);
        end;
      end;

      OPCLog.WriteToLog('Отправляем сообщение...');
      aSMTP.Send(aIdMessage);

      OPCLog.WriteToLog('ОK.');
      aSMTP.Disconnect;
    except
      on e: Exception do
      begin
        OPCLog.WriteToLog(e.ClassName + ' : ' + e.Message);
        raise;
      end;
    end;

  finally
    //aSSL.Free;
    aFiles.Free;
    aIdMessage.Free;
    aSMTP.Free;
    aDumy.Free;
  end;
end;



{ TDumyObject }

procedure TDumyObject.IdSMTP1FailedRecipient(Sender: TObject; const AAddress, ACode, AText: string; var VContinue: Boolean);
begin
  VContinue := True;
end;

end.
