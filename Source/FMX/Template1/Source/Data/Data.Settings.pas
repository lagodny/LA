unit Data.Settings;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Types, System.Permissions,
  System.IOUtils, System.SysUtils,
  System.JSON, System.JSON.Builders,
  FMX.Platform,
  DKL_LanguageCodes;

const
  { TODO : нужно исправить в соответствии с фактическим наименованием приложения }
  cAppName = 'LAProjectTemplate';

type
  TSettings = class
  private
    class var FInstance: TSettings;
  private
    FAddr: string;
    FUserName: string;
    FPassword: string;
    FLanguage: Integer;
    FSettingsFileName: string;
    FSettingsPath: string;
    FIntervalKind: Integer;
    FIntervalShift: Double;
    FIntervalDate1: TDateTime;
    FIntervalDate2: TDateTime;
    FOnChange: TNotifyEvent;

    FAutoLogon: Boolean;
  public
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create;
    destructor Destroy; override;

//    function GetPlatformPath: string;
//    function GetSettingsFileName: string;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure DoChange;

    property SettingsPath: string read FSettingsPath;
    property SettingsFileName: string read FSettingsFileName;

    property Addr: string read FAddr write FAddr;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property AutoLogon: Boolean read FAutoLogon write FAutoLogon;

    property Language: Integer read FLanguage write FLanguage;

    property IntervalKind: Integer read FIntervalKind write FIntervalKind;
    property IntervalShift: Double read FIntervalShift write FIntervalShift;
    property IntervalDate1: TDateTime read FIntervalDate1 write FIntervalDate1;
    property IntervalDate2: TDateTime read FIntervalDate2 write FIntervalDate2;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  function Settings: TSettings;

implementation

uses
  FMX.Dialogs,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.Jni.Os,
  Androidapi.Jni.javatypes,
  Androidapi.Helpers,
{$ENDIF}
  DW.Permissions.Helpers,
//{$IFDEF MACOS}
//  Macapi.Foundation,
//{$ENDIF}
  LA.Interval;


const
  SETTINGS_FILENAME = cAppName +'.settings';
  //LOCATION_DATA_FILENAME = 'Location.dat';

function Settings: TSettings;
begin
  if not Assigned(TSettings.FInstance) then
  begin
    TSettings.FInstance := TSettings.Create;
    TSettings.FInstance.LoadSettings;
  end;
  Result := TSettings.FInstance;
end;

function GetLang: string;
var
  LocaleService: IFMXLocaleService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService,
    IInterface(LocaleService)) then
  begin
    Result := LocaleService.GetCurrentLangID();
  end
  else
  begin
    {$IFDEF MSWINDOWS}
    var buffer: MarshaledString;
    var UserLCID: LCID;
    var BufLen: Integer;

    UserLCID := GetUserDefaultLCID;
    BufLen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
    buffer := StrAlloc(BufLen);
    if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, BufLen) <> 0
    then
      Result := buffer
    else
      Result := 'en';
    StrDispose(buffer);
    {$ENDIF}
//    {$IFDEF MACOS}
//    var Languages: NSArray;
//    Languages := TNSLocale.OCClass.preferredLanguages;
//    Result := TNSString.Wrap(Languages.objectAtIndex(0)).UTF8String;
//    {$ENDIF}
  end;
end;

//function GetOSLangID: String;
//{$IFDEF MACOS} var
//  Languages: NSArray;
//begin
//  Languages := TNSLocale.OCClass.preferredLanguages;
//  Result := TNSString.Wrap(Languages.objectAtIndex(0)).UTF8String;
//  {$ENDIF}
//  {$IFDEF ANDROID} var
//    LocServ: IFMXLocaleService;
//  begin
//    if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService,
//      IInterface(LocServ)) then
//      Result := LocServ.GetCurrentLangID; {$ENDIF}
//  {$IFDEF MSWINDOWS} var
//      buffer: MarshaledString;
//      UserLCID: LCID;
//      BufLen: Integer;
//    begin // defaults
//      UserLCID := GetUserDefaultLCID;
//      BufLen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
//      buffer := StrAlloc(BufLen);
//      if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, BufLen) <> 0
//      then
//        Result := buffer
//      else
//        Result := 'en';
//      StrDispose(buffer);
//    {$ENDIF}
//  end; { code }


{ TSettings }

constructor TSettings.Create;
begin
//  FLocationManager := TLocationManager.Create;

  FSettingsPath := '';
  FSettingsFileName := '';

{$IFDEF ANDROID}
  var cPermissions: TArray<string> := [
      JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE),
      JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)
//      , JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)
//      , JStringToString(TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION)
    ];
  PermissionsService.RequestPermissions(
    cPermissions,
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if AGrantResults.AreAllGranted then
      begin
        if (AGrantResults[0] = TPermissionStatus.Granted) and
          (AGrantResults[1] = TPermissionStatus.Granted) then
        begin
          FSettingsPath := TPath.Combine(TPath.GetDocumentsPath, cAppName);
//          FSettingsPath := TPath.Combine(TPath.GetSharedDocumentsPath, cAppName);
          FSettingsFileName := TPath.Combine(SettingsPath, SETTINGS_FILENAME);
        end;
      end;
    end);
{$ELSE}
  FSettingsPath := TPath.Combine(TPath.GetLibraryPath, cAppName);
  FSettingsFileName := TPath.Combine(SettingsPath, SETTINGS_FILENAME);
{$ENDIF}



////  Addr := 'localhost:89';

//  Addr := '192.168.0.104:89';
//  Addr := '192.168.126.1:89';
//  Addr := '192.168.126.1:5152';
//  Addr := 'tdc.org.ua:5152';
  Addr := '193.109.249.118:5152';
//  Addr := 'https://dc.tdc.org.ua'; //:443';
//  Addr := 'https://owa.tdc.org.ua'; //:443';
  UserName := '';
  Password := '';
  AutoLogon := False;

//  Addr := '192.168.126.1:89';
//  UserName := 'Лагодный';
//  Password := '314';
//  UserName := 'Prometey';
//  Password := '2312';

  IntervalKind := Ord(ikToday);
  IntervalShift := 12;
  IntervalDate1 := Now - 1;
  IntervalDate2 := Now;

  Language := GetLangIdFromCultureCode(GetLang);
  if (Language > 0) and (Language < 1024) then
    Language := 1024 + Language;

//  ShowMessage(Language.ToString + ' : ' + GetLang);
end;


destructor TSettings.Destroy;
begin
  inherited;
end;

procedure TSettings.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

class constructor TSettings.Create;
begin
  FInstance := nil;
end;

class destructor TSettings.Destroy;
begin
  FInstance.Free;
  FInstance := nil;
end;

//function TSettings.GetSettingsFileName: string;
//begin
//  Result := TPath.Combine(GetPlatformPath, SETTINGS_FILENAME);
//end;
//
//function TSettings.GetPlatformPath: string;
//begin
//{$IFDEF MSWINDOWS}
//  Result := TPath.GetLibraryPath;
//{$ELSE}
//  Result := TPath.GetDocumentsPath;
//{$ENDIF}
//end;

procedure TSettings.LoadSettings;
var
  aValue: TJSONValue;
  aJSON: TJSONObject;
  aVal: TJSONValue;
  aObj: TJSONObject;
//  aArr: TJSONArray;
  aData: string;
begin
  if SettingsFileName = '' then
    Exit;

  if not TFile.Exists(SettingsFileName) then
    Exit;

  try

    aData := TFile.ReadAllText(SettingsFileName, TEncoding.UTF8);
    aValue := TJSONObject.ParseJSONValue(aData);
    try
      if not (aValue is TJSONObject) then
        Exit;

      aJSON := TJSONObject(aValue);

      aVal := aJson.GetValue('connection');
      if Assigned(aVal) and (aVal is TJSONObject) then
      begin
        aObj := aVal as TJSONObject;
        Addr := aObj.GetValue<string>('addr', Addr);
      end;

      aVal := aJson.GetValue('login');
      if Assigned(aVal) and (aVal is TJSONObject) then
      begin
        aObj := aVal as TJSONObject;
        UserName := aObj.GetValue<string>('username', UserName);
        Password := aObj.GetValue<string>('password', Password);
        AutoLogon := aObj.GetValue<Boolean>('autologon', AutoLogon);
      end;

      aVal := aJson.GetValue('interval');
      if Assigned(aVal) and (aVal is TJSONObject) then
      begin
        aObj := aVal as TJSONObject;
        try
          TLAInterval.LastInterval.Kind := TLAIntervalKind(aObj.GetValue<Integer>('kind', Ord(TLAInterval.LastInterval.Kind)));
          TLAInterval.LastInterval.Shift := aObj.GetValue<Double>('shift', TLAInterval.LastInterval.Shift);
          if TLAInterval.LastInterval.Kind = ikAbsolute then
            TLAInterval.LastInterval.SetInterval(
              aObj.GetValue<Double>('date1', TLAInterval.LastInterval.Date1),
              aObj.GetValue<Double>('date2', TLAInterval.LastInterval.Date2)
            );
        except
          on e: Exception do
          begin
            TLAInterval.LastInterval.Kind := ikLastNHours;
            TLAInterval.LastInterval.Shift := 12;
          end;
        end;
      end;

      aVal := aJson.GetValue('language');
      if Assigned(aVal) and (aVal is TJSONObject) then
      begin
        aObj := aVal as TJSONObject;
        Language := aObj.GetValue<Integer>('id', 0);
      end;

    finally
      aValue.Free;
    end;
    DoChange;
  except
    on e: Exception do
      ;
  end;
end;

procedure TSettings.SaveSettings;
var
  aJson, aObj: TJSONObject;
//  aArr: TJSONArray;
begin
  if SettingsFileName = '' then
    Exit;

  try
    aJson := TJSONObject.Create;
    try
      aObj := TJSONObject.Create;
      aObj.AddPair('addr', Addr);
      aJson.AddPair('connection', aObj);

      aObj := TJSONObject.Create;
      aObj.AddPair('username', UserName);
      aObj.AddPair('password', Password);
      aObj.AddPair('autologon', AutoLogon);
      aJson.AddPair('login', aObj);

//      aArr := TJSONArray.Create;
//      for var i := 0 to High(FMapDevices) do
//        aArr.Add(FMapDevices[i]);

      aObj := TJSONObject.Create;
      aObj.AddPair('kind', Ord(TLAInterval.LastInterval.Kind));
      aObj.AddPair('shift', TLAInterval.LastInterval.Shift);
      aObj.AddPair('date1', TLAInterval.LastInterval.Date1);
      aObj.AddPair('date2', TLAInterval.LastInterval.Date2);
      aJson.AddPair('interval', aObj);

      aObj := TJSONObject.Create;
      aObj.AddPair('id', Language);
      aJson.AddPair('language', aObj);

      ForceDirectories(SettingsPath);
      TFile.WriteAllText(SettingsFileName, aJson.ToJSON([]), TEncoding.UTF8);
    finally
      aJson.Free;
    end;
  except
    on e: Exception do
      ;
  end;
end;

end.
