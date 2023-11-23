unit LA.Log;

interface

uses
  System.Classes, System.SyncObjs;

type
  // класс для ведения лога
  TDCLog = class
  private
    class var FActive: Boolean;
    class procedure SetActive(const Value: Boolean); static; type
    TDCLogImpl = class
    private
      FLock: TCriticalSection;
      FLogFileName: string;
      FMaxSize: Integer;
      function GetLogFileName: string;
      function GetDefLogFileName: string;
    public
      constructor Create;
      destructor Destroy; override;

      procedure WriteToLog(const aMessage: string);
      procedure WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);

      property MaxSize: Integer read FMaxSize write FMaxSize;
      property LogFileName: string read GetLogFileName write FLogFileName;
    end;

    class var FLog: TDCLogImpl;
    class function GetLogImpl: TDCLogImpl;

    class constructor Create;
    class destructor Destroy;
  public
    class procedure WriteToLog(const aMessage: string);
    class procedure WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);
    class property Active: Boolean read FActive write SetActive;
  end;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Jni.Os,
  Androidapi.Jni.javatypes,
  Androidapi.Helpers,
{$ENDIF}
{$IFDEF IOS}
  // iOSapi.UIKit,
  Macapi.CoreFoundation, iOSApi.Foundation,
{$ENDIF}
  System.Types, System.SysUtils,
  System.Permissions,
  System.IOUtils;

{ TDCLog }

class constructor TDCLog.Create;
begin
  FLog := nil;
  FActive := True;
end;

class destructor TDCLog.Destroy;
begin
  TDCLog.FLog.Free;
end;

class function TDCLog.GetLogImpl: TDCLogImpl;
begin
  if not Assigned(FLog) then
    FLog := TDCLogImpl.Create;
  Result := FLog;
end;

class procedure TDCLog.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

class procedure TDCLog.WriteToLog(const aMessage: string);
begin
  GetLogImpl.WriteToLog(aMessage);
end;

class procedure TDCLog.WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);
begin
  GetLogImpl.WriteToLogFmt(aMessage, Args);
end;

{ TDCLog.TDCLogImpl }

constructor TDCLog.TDCLogImpl.Create;
begin
  FLock := TCriticalSection.Create;
  FMaxSize := 5 * 1024 * 1024;
end;

destructor TDCLog.TDCLogImpl.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TDCLog.TDCLogImpl.GetDefLogFileName: string;
var
  aPath, aAppName: string;
begin
{$IFDEF ANDROID}
  var
    cPermissions: TArray<string> := [JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE),
      JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)];
  PermissionsService.RequestPermissions(cPermissions,
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if Length(AGrantResults) = 2 then
      begin
        if (AGrantResults[0] = TPermissionStatus.Granted) and (AGrantResults[1] = TPermissionStatus.Granted) then
        begin
          aPath := TPath.GetSharedDocumentsPath;
          aAppName := JStringToString(SharedActivityContext.getPackageName);
        end;
      end;
    end);
{$ENDIF}
{$IFDEF IOS}
  aPath := TPath.GetLibraryPath;
  aAppName := TNSString.Wrap(CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle, kCFBundleIdentifierKey)).UTF8String;;
{$ENDIF}
{$IFDEF MSWINDOWS}
  aPath := ExtractFilePath(ParamStr(0));
  aAppName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  if SameText(aAppName, 'bds') then
  begin
    aPath := TPath.GetTempPath;
    aAppName := 'la';
  end;
//  aPath := 'c:\1\';
//  aAppName := 'la' + ExtractFilePath(ParamStr(0)) + '_' + ChangeFileExt(ExtractFileName(ParamStr(0)), '');
//  aAppName := StringReplace(aAppName, '\', '_', [rfReplaceAll]);
//  aAppName := StringReplace(aAppName, ':', '_', [rfReplaceAll]);
{$ENDIF}

  Result := TPath.Combine(aPath, aAppName + '.log');
end;

function TDCLog.TDCLogImpl.GetLogFileName: string;
begin
  if FLogFileName = '' then
    FLogFileName := GetDefLogFileName;
  Result := FLogFileName;
end;

procedure TDCLog.TDCLogImpl.WriteToLog(const aMessage: string);
var
  aMsg: string;
  LogFileName1: string;
  LogFileStream: TFileStream;

  Encoding: TEncoding;
  Buffer, Preamble: TBytes;
begin
  if not Active then
    Exit;

  // Exit;

  LogFileStream := nil;
  Encoding := TEncoding.Default;

  if Assigned(FLock) then
    FLock.Enter;
  try
    try
      try
        LogFileStream := TFileStream.Create(LogFileName, fmOpenWrite + fmShareDenyNone);
        LogFileStream.Position := LogFileStream.Size;
      except
        LogFileStream := TFileStream.Create(LogFileName, fmCreate);
        FreeAndNil(LogFileStream);

        LogFileStream := TFileStream.Create(LogFileName, fmOpenWrite + fmShareDenyNone);
        Preamble := Encoding.GetPreamble;
        if Length(Preamble) > 0 then
          LogFileStream.WriteBuffer(Preamble[0], Length(Preamble));
      end;

      if (LogFileStream.Size div 1024) > MaxSize then
      begin
        FreeAndNil(LogFileStream);

        LogFileName1 := LogFileName + '1';
        if FileExists(LogFileName1) then
          System.SysUtils.DeleteFile(LogFileName1);

        RenameFile(LogFileName, LogFileName1);

        LogFileStream := TFileStream.Create(LogFileName, fmCreate);
        FreeAndNil(LogFileStream);

        LogFileStream := TFileStream.Create(LogFileName, fmOpenWrite + fmShareDenyNone);
        Preamble := Encoding.GetPreamble;
        if Length(Preamble) > 0 then
          LogFileStream.WriteBuffer(Preamble[0], Length(Preamble));
      end;

      if aMessage <> '' then
      begin

        aMsg := FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', Now) + #09 +
        // UserName          + #09 +
        // LoginName         + #09 +
        // ComputerName      + #09 +
        // IPAdress          + #09 +
        // aShift            +
        // IfThen(aShowAll, aMessage, RemoveNonprintingSymbols(aMessage)) +
          aMessage + #13#10;
      end
      else
        aMsg := #13#10;

      Buffer := Encoding.GetBytes(aMsg);
      LogFileStream.Write(Buffer[0], Length(Buffer));

    except
      FreeAndNil(LogFileStream);
    end;
  finally
    FreeAndNil(LogFileStream);
    if Assigned(FLock) then
      FLock.Leave;
  end;
end;

procedure TDCLog.TDCLogImpl.WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);
begin
  WriteToLog(Format(aMessage, Args));
end;

end.
