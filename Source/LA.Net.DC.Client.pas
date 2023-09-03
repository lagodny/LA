/// remote access to a mORMot server using SynCrossPlatform* units
// - retrieved from http://localhost:89/DC/wrapper/CrossPlatform/mORMotClient.pas
// at 2019-12-26 09:36:41 using "CrossPlatform.pas.mustache" template
unit LA.Net.DC.Client;

interface

uses
  SynCrossPlatformJSON, SynCrossPlatformSpecific, SynCrossPlatformREST,
  LA.Types.Monitoring;

//type // define some record types, used as properties below
//  TDataRec = record
//    t: Int64;
//    v: string;
//  end;
//
//  TDataRecExt = record
//    SID: string;
//    t: Int64;
//    v: string;
//    e: string;
//  end;
//
//  THistoryRec = record
//    t: Int64;
//    v: string;
//  end;
//
//  THistoryRecExt = record
//    t: Int64;
//    v: string;
//    e: string;
//  end;
//
//type // define some dynamic array types, used as properties below
//  TSIDArr = array of RawUTF8;
//  TValArr = array of RawUTF8;
//  TDataRecArr = array of TDataRec;
//  TDataRecExtArr = array of TDataRecExt;
//  TIDArr = array of Int64;
//  THistoryRecArr = array of THistoryRec;
//  THistoryRecExtArr = array of THistoryRecExt;

type
  /// service implemented by TServiceMonitoring
  // - you can access this service as such:
  // !var aMonitoring: IMonitoring;
  // !begin
  // !   aMonitoring := TServiceMonitoring.Create(aClient);
  // !   // now you can use aMonitoring methods
  // !...
  IMonitoring = interface(IServiceAbstract)
    ['{27BA80C4-482F-4908-8EED-5D361C0EE532}']
    function DateTimeToMoment(const aDateTime: TDateTime): Int64;
    function MomentToDateTime(const aMoment: Int64): TDateTime;
    function SensorValue(const SID: String): String;
    function SensorData(const SID: String): TDataRec;
    function SensorDataExt(const SID: String): TDataRecExt;
    function GroupSensorValue(const SIDs: TSIDArr): TValArr;
    function GroupSensorData(const SIDs: TSIDArr): TDataRecArr;
    function GroupSensorDataExt(const SIDs: TSIDArr): TDataRecExtArr;
    function SensorValueByID(const ID: TID): String;
    function SensorDataByID(const ID: TID): TDataRec;
    function SensorDataExtByID(const ID: TID): TDataRecExt;
    function GroupSensorValueByID(const IDs: TIDArr): TValArr;
    function GroupSensorDataByID(const IDs: TIDArr): TDataRecArr;
    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
    function SensorDataAsText(const SID: String): String;
    function SensorsDataAsText(const SIDs: TSIDArr; const aUseCache: Boolean): String;
    function SensorHistory(const SID: String; const FromDate: Int64; const ToDate: Int64): THistoryRecArr;
    function SensorHistoryExt(const SID: String; const FromDate: Int64; const ToDate: Int64): THistoryRecExtArr;
    function CloneGroup(const aGroupID: TID; const aCloneChildren: Boolean; const aCloneSensors: Boolean; const aCloneDevices: Boolean; const aCount: Integer): Integer;
    procedure InitSensorsByID(const IDs: TIntegerDynArray);
    procedure InitGroupsByID(const IDs: TIntegerDynArray);
    procedure InitDevicesByID(const IDs: TIntegerDynArray);
  end;

  /// implements IMonitoring from http://localhost:89/DC/Monitoring
  // - this service will run in sicClientDriven mode
  TServiceMonitoring = class(TServiceClientAbstractClientDriven,IMonitoring)
  public
    constructor Create(aClient: TSQLRestClientURI); override;
    destructor Destroy; override;

    function DateTimeToMoment(const aDateTime: TDateTime): Int64;
    function MomentToDateTime(const aMoment: Int64): TDateTime;
    function SensorValue(const SID: String): String;
    function SensorData(const SID: String): TDataRec;
    function SensorDataExt(const SID: String): TDataRecExt;
    function GroupSensorValue(const SIDs: TSIDArr): TValArr;
    function GroupSensorData(const SIDs: TSIDArr): TDataRecArr;
    function GroupSensorDataExt(const SIDs: TSIDArr): TDataRecExtArr;
    function SensorValueByID(const ID: TID): String;
    function SensorDataByID(const ID: TID): TDataRec;
    function SensorDataExtByID(const ID: TID): TDataRecExt;
    function GroupSensorValueByID(const IDs: TIDArr): TValArr;
    function GroupSensorDataByID(const IDs: TIDArr): TDataRecArr;
    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
    function SensorDataAsText(const SID: String): String;
    function SensorsDataAsText(const SIDs: TSIDArr; const aUseCache: Boolean): String;
    function SensorHistory(const SID: String; const FromDate: Int64; const ToDate: Int64): THistoryRecArr;
    function SensorHistoryExt(const SID: String; const FromDate: Int64; const ToDate: Int64): THistoryRecExtArr;
    function CloneGroup(const aGroupID: TID; const aCloneChildren: Boolean; const aCloneSensors: Boolean; const aCloneDevices: Boolean; const aCount: Integer): Integer;
    procedure InitSensorsByID(const IDs: TIntegerDynArray);
    procedure InitGroupsByID(const IDs: TIntegerDynArray);
    procedure InitDevicesByID(const IDs: TIntegerDynArray);
  end;

  /// service implemented by TServiceDCSession
  // - you can access this service as such:
  // !var aDCSession: IDCSession;
  // !begin
  // !   aDCSession := TServiceDCSession.Create(aClient);
  // !   // now you can use aDCSession methods
  // !...
  IDCSession = interface(IServiceAbstract)
    ['{9073D395-66DE-4FFE-B242-D3755E9BA223}']
    procedure SetSessionInfo(const Client: String);
  end;

  /// implements IDCSession from http://localhost:89/DC/DCSession
  // - this service will run in sicClientDriven mode
  TServiceDCSession = class(TServiceClientAbstractClientDriven,IDCSession)
  public
    constructor Create(aClient: TSQLRestClientURI); override;
    procedure SetSessionInfo(const Client: String);
  end;


  /// service implemented by TServiceTracking
  // - you can access this service as such:
  // !var aTracking: ITracking;
  // !begin
  // !   aTracking := TServiceTracking.Create(aClient);
  // !   // now you can use aTracking methods
  // !...

  TTrackingTagRec = record
    sid: String;
    name: String;
    addr: String;
    un: String;
    kind: Integer;
  end;

  TTrackingObjectRec = record
    id: Integer;
    name: String;
    groupId: Integer;
    tags: array of TTrackingTagRec;
  end;

  TTrackingObjects = array of TTrackingObjectRec;


  /// service implemented by TServiceTracking
  // - you can access this service as such:
  // !var aTracking: ITracking;
  // !begin
  // !   aTracking := TServiceTracking.Create(aClient);
  // !   // now you can use aTracking methods
  // !...
  ITracking = interface(IServiceAbstract)
    ['{0287BC71-0194-4F9D-8D6F-3CFEE7C6C371}']
    function GetClients(): Variant;
    function GetDevices(const Clients: TIDDynArray): Variant;
    function GetDevicesData(const Devices: TIDDynArray): Variant;

    function GetTrack(const DeviceID: TID; const Date1, Date2: Int64): Variant;
    function GetReport(const DeviceID: TID; const Date1, Date2: Int64): Variant;

    procedure SetDevice(const Device: Variant);
    procedure SetTagValue(const DeviceID: TID; const TagSID: string; const Value: Variant);

    procedure CreateDevice(const aName, aLogin, aPhone, aProto: string);
    procedure ShareDevice(const aDeviceID: TID; const aLogin: string; const aRight: string);
  end;

  /// implements ITracking from http://localhost:89/DC/Tracking
  // - this service will run in sicClientDriven mode
  TServiceTracking = class(TServiceClientAbstractClientDriven,ITracking)
  public
    constructor Create(aClient: TSQLRestClientURI); override;
    destructor Destroy; override;

    function GetClients(): Variant;
    function GetDevices(const Clients: TIDDynArray): Variant;
    function GetDevicesData(const Devices: TIDDynArray): Variant;

    function GetTrack(const DeviceID: TID; const Date1, Date2: Int64): Variant;
    function GetReport(const DeviceID: TID; const Date1, Date2: Int64): Variant;

    procedure SetDevice(const Device: Variant);
    procedure SetTagValue(const DeviceID: TID; const TagSID: string; const Value: Variant);

    procedure CreateDevice(const aName, aLogin, aPhone, aProto: string);
    procedure ShareDevice(const aDeviceID: TID; const aLogin: string; const aRight: string);
  end;

  /// service implemented by TServiceDCSignUp
  // - you can access this service as such:
  // !var aDCSignUp: IDCSignUp;
  // !begin
  // !   aDCSignUp := TServiceDCSignUp.Create(aClient);
  // !   // now you can use aDCSignUp methods
  // !...
  IDCSignUp = interface(IServiceAbstract)
    ['{7F476721-43BC-4D8E-9C3F-A3935284BDB6}']
    function Help(): String;
    procedure RequestSignUp(const Login: String; const EMail: String; const Password: String);
    function ConfirmSignUp(const Key: String): THttpBody;
    procedure RequestResetPassword(const Login: String; const EMail: String);
    procedure ConfirmResetPassword(const Key: String);
  end;

  /// implements IDCSignUp from http://localhost:89/DC/DCSignUp
  // - this service will run in sicClientDriven mode
  TServiceDCSignUp = class(TServiceClientAbstractClientDriven,IDCSignUp)
  public
    constructor Create(aClient: TSQLRestClientURI); override;
    function Help(): String;
    procedure RequestSignUp(const Login: String; const EMail: String; const Password: String);
    function ConfirmSignUp(const Key: String): THttpBody;
    procedure RequestResetPassword(const Login: String; const EMail: String);
    procedure ConfirmResetPassword(const Key: String);
  end;




const
  /// the server port, corresponding to http://localhost:89
  SERVER_PORT = 89;
  /// the server model root name, corresponding to http://localhost:89
  SERVER_ROOT = 'DC';
  cSendTimeout = 10000;
  cReceiveTimeout = 60000;
  cConnectionTimeOut = 2000;

/// return the database Model corresponding to this server
//function GetModel(const aRoot: string=SERVER_ROOT): TSQLModel;

/// create a TSQLRestClientHTTP instance and connect to the server
// - it will use by default port 89 over root 'DC', corresponding
// to http://localhost:89/DC
// - secure connection will be established via TSQLRestServerAuthenticationDefault
// with the supplied credentials - on connection or authentication error,
// this function will raise a corresponding exception
function GetClient(const aServerAddress, aUserName, aPassword: string;
  aServerPort: integer = SERVER_PORT; const aServerRoot: string = SERVER_ROOT;
  aHttps: boolean = false; const aProxyName: string = ''; const aProxyByPass: string = '';
  aSendTimeout: Cardinal = cSendTimeout; aReceiveTimeout: Cardinal = cReceiveTimeout;
  aConnectionTimeOut: Cardinal = cConnectionTimeOut): TSQLRestClientHTTP;

function GetClientNoUser(const aServerAddress: string; aServerPort: integer = SERVER_PORT;
  const aServerRoot: string = SERVER_ROOT;
  aHttps: boolean = false; const aProxyName: string = ''; const aProxyByPass: string = '';
  aSendTimeout: Cardinal = cSendTimeout; aReceiveTimeout: Cardinal = cReceiveTimeout;
  aConnectionTimeOut: Cardinal = cConnectionTimeOut): TSQLRestClientHTTP;


// publish some low-level helpers for variant conversion
// - used internally: you should not need those functions in your end-user code
function Variant2TDataRec(_variant: variant): TDataRec;

function TDataRec2Variant(const _record: TDataRec): variant;

function Variant2TDataRecExt(_variant: variant): TDataRecExt;

function TDataRecExt2Variant(const _record: TDataRecExt): variant;

function Variant2THistoryRec(_variant: variant): THistoryRec;

function THistoryRec2Variant(const _record: THistoryRec): variant;

function Variant2THistoryRecExt(_variant: variant): THistoryRecExt;

function THistoryRecExt2Variant(const _record: THistoryRecExt): variant;

function Variant2TSIDArr(const _variant: variant): TSIDArr;

function TSIDArr2Variant(const _array: TSIDArr): variant;

function Variant2TValArr(const _variant: variant): TValArr;

function TValArr2Variant(const _array: TValArr): variant;

function Variant2TDataRecArr(const _variant: variant): TDataRecArr;

function TDataRecArr2Variant(const _array: TDataRecArr): variant;

function Variant2TDataRecExtArr(const _variant: variant): TDataRecExtArr;

function TDataRecExtArr2Variant(const _array: TDataRecExtArr): variant;

function Variant2TIDArr(const _variant: variant): TIDArr;

function TIDArr2Variant(const _array: TIDArr): variant;

function Variant2THistoryRecArr(const _variant: variant): THistoryRecArr;

function THistoryRecArr2Variant(const _array: THistoryRecArr): variant;

function Variant2THistoryRecExtArr(const _variant: variant): THistoryRecExtArr;

function THistoryRecExtArr2Variant(const _array: THistoryRecExtArr): variant;

function TIntegerDynArray2Variant(const _array: TIntegerDynArray): variant;

function Variant2TTrackingObjects(const _variant: variant): TTrackingObjects;
function TTrackingObjects2Variant(const _array: TTrackingObjects): variant;
function Variant2TTrackingObjectRec(_variant: variant): TTrackingObjectRec;
function TTrackingObjectRec2Variant(const _record: TTrackingObjectRec): variant;
function Variant2TTrackingTagRec(_variant: variant): TTrackingTagRec;
function TTrackingTagRec2Variant(const _record: TTrackingTagRec): variant;

function TIDDynArray2Variant(const _array: TIDDynArray): variant;





implementation

{$HINTS OFF} // for H2164 hints of unused variables


{ Some helpers for enumerates types }

function Variant2TDataRec(_variant: variant): TDataRec;
var
  _a: integer;
  _arr: PJSONVariantData;
begin
  result.t := _variant.t;
  result.v := _variant.v;
end;

function TDataRec2Variant(const _record: TDataRec): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  res.SetPath('t', _record.t);
  res.SetPath('v', _record.v);
  result := variant(res);
end;

function Variant2TDataRecExt(_variant: variant): TDataRecExt;
var
  _a: integer;
  _arr: PJSONVariantData;
begin
  result.SID := _variant.SID;
  result.t := _variant.t;
  result.v := _variant.v;
  result.e := _variant.e;
end;

function TDataRecExt2Variant(const _record: TDataRecExt): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  res.SetPath('SID', _record.SID);
  res.SetPath('t', _record.t);
  res.SetPath('v', _record.v);
  res.SetPath('e', _record.e);
  result := variant(res);
end;

function Variant2THistoryRec(_variant: variant): THistoryRec;
var
  _a: integer;
  _arr: PJSONVariantData;
begin
  result.t := _variant.t;
  result.v := _variant.v;
end;

function THistoryRec2Variant(const _record: THistoryRec): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  res.SetPath('t', _record.t);
  res.SetPath('v', _record.v);
  result := variant(res);
end;

function Variant2THistoryRecExt(_variant: variant): THistoryRecExt;
var
  _a: integer;
  _arr: PJSONVariantData;
begin
  result.t := _variant.t;
  result.v := _variant.v;
  result.e := _variant.e;
end;

function THistoryRecExt2Variant(const _record: THistoryRecExt): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  res.SetPath('t', _record.t);
  res.SetPath('v', _record.v);
  res.SetPath('e', _record.e);
  result := variant(res);
end;

function Variant2TSIDArr(const _variant: variant): TSIDArr;
var
  i: integer;
  arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant, jvArray);
  SetLength(result, arr^.Count);
  for i := 0 to arr^.Count - 1 do
    result[i] := (arr^.Values[i]);
end;

function TSIDArr2Variant(const _array: TSIDArr): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue((_array[i]));
  result := variant(res);
end;

function Variant2TValArr(const _variant: variant): TValArr;
var
  i: integer;
  arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant, jvArray);
  SetLength(result, arr^.Count);
  for i := 0 to arr^.Count - 1 do
    result[i] := (arr^.Values[i]);
end;

function TValArr2Variant(const _array: TValArr): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue((_array[i]));
  result := variant(res);
end;

function Variant2TDataRecArr(const _variant: variant): TDataRecArr;
var
  i: integer;
  arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant, jvArray);
  SetLength(result, arr^.Count);
  for i := 0 to arr^.Count - 1 do
    result[i] := Variant2TDataRec(arr^.Values[i]);
end;

function TDataRecArr2Variant(const _array: TDataRecArr): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue(TDataRec2Variant(_array[i]));
  result := variant(res);
end;

function Variant2TDataRecExtArr(const _variant: variant): TDataRecExtArr;
var
  i: integer;
  arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant, jvArray);
  SetLength(result, arr^.Count);
  for i := 0 to arr^.Count - 1 do
    result[i] := Variant2TDataRecExt(arr^.Values[i]);
end;

function TDataRecExtArr2Variant(const _array: TDataRecExtArr): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue(TDataRecExt2Variant(_array[i]));
  result := variant(res);
end;

function Variant2TIDArr(const _variant: variant): TIDArr;
var
  i: integer;
  arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant, jvArray);
  SetLength(result, arr^.Count);
  for i := 0 to arr^.Count - 1 do
    result[i] := (arr^.Values[i]);
end;

function TIDArr2Variant(const _array: TIDArr): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue((_array[i]));
  result := variant(res);
end;

function Variant2THistoryRecArr(const _variant: variant): THistoryRecArr;
var
  i: integer;
  arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant, jvArray);
  SetLength(result, arr^.Count);
  for i := 0 to arr^.Count - 1 do
    result[i] := Variant2THistoryRec(arr^.Values[i]);
end;

function THistoryRecArr2Variant(const _array: THistoryRecArr): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue(THistoryRec2Variant(_array[i]));
  result := variant(res);
end;

function Variant2THistoryRecExtArr(const _variant: variant): THistoryRecExtArr;
var
  i: integer;
  arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant, jvArray);
  SetLength(result, arr^.Count);
  for i := 0 to arr^.Count - 1 do
    result[i] := Variant2THistoryRecExt(arr^.Values[i]);
end;

function THistoryRecExtArr2Variant(const _array: THistoryRecExtArr): variant;
var
  i: integer;
  res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue(THistoryRecExt2Variant(_array[i]));
  result := variant(res);
end;

function TIntegerDynArray2Variant(const _array: TIntegerDynArray): variant;
var i: integer;
    res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue((_array[i]));
  result := variant(res);
end;


function Variant2TTrackingObjects(const _variant: variant): TTrackingObjects;
var i: integer;
    arr: PJSONVariantData;
begin
  arr := JSONVariantDataSafe(_variant,jvArray);
  SetLength(result,arr^.Count);
  for i := 0 to arr^.Count-1 do
    result[i] := Variant2TTrackingObjectRec(arr^.Values[i]);
end;

function TTrackingObjects2Variant(const _array: TTrackingObjects): variant;
var i: integer;
    res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue(TTrackingObjectRec2Variant(_array[i]));
  result := variant(res);
end;

function Variant2TTrackingObjectRec(_variant: variant): TTrackingObjectRec;
var _a: integer;
    _arr: PJSONVariantData;
begin
  result.id := _variant.id;
  result.name := _variant.name;
  result.groupId := _variant.groupId;
  _arr := JSONVariantDataSafe(_variant.tags,jvArray);
  SetLength(result.tags,_arr^.Count);
  for _a := 0 to high(result.tags) do
    result.tags[_a] := Variant2TTrackingTagRec(_arr^.Values[_a]);
end;

function TTrackingObjectRec2Variant(const _record: TTrackingObjectRec): variant;
var i: integer;
    res: TJSONVariantData;
begin
  res.Init;
  res.SetPath('id',_record.id);
  res.SetPath('name',_record.name);
  res.SetPath('groupId',_record.groupId);
  with res.EnsureData('tags')^ do
    for i := 0 to high(_record.tags) do
      AddValue(TTrackingTagRec2Variant(_record.tags[i]));
  result := variant(res);
end;

function Variant2TTrackingTagRec(_variant: variant): TTrackingTagRec;
var _a: integer;
    _arr: PJSONVariantData;
begin
  result.sid := _variant.sid;
  result.name := _variant.name;
  result.addr := _variant.addr;
  result.un := _variant.un;
  result.kind := _variant.kind;
end;

function TTrackingTagRec2Variant(const _record: TTrackingTagRec): variant;
var i: integer;
    res: TJSONVariantData;
begin
  res.Init;
  res.SetPath('sid',_record.sid);
  res.SetPath('name',_record.name);
  res.SetPath('addr',_record.addr);
  res.SetPath('un',_record.un);
  res.SetPath('kind',_record.kind);
  result := variant(res);
end;

function TIDDynArray2Variant(const _array: TIDDynArray): variant;
var i: integer;
    res: TJSONVariantData;
begin
  res.Init;
  for i := 0 to high(_array) do
    res.AddValue((_array[i]));
  result := variant(res);
end;






{$HINTS ON} // for H2164 hints of unused variables

function GetModel(const aRoot: string): TSQLModel;
begin
  result := TSQLModel.Create([], aRoot);
end;

function GetClient(const aServerAddress, aUserName, aPassword: string; aServerPort: integer; const aServerRoot: string; aHttps: boolean; const aProxyName, aProxyByPass: string; aSendTimeout, aReceiveTimeout, aConnectionTimeOut: Cardinal): TSQLRestClientHTTP;
begin
  result := TSQLRestClientHTTP.Create(aServerAddress, aServerPort,    //nil,
    GetModel(aServerRoot), true, aHttps, aProxyName, aProxyByPass, aSendTimeout, aReceiveTimeout, aConnectionTimeOut);
  try
    if (not result.Connect) or (result.ServerTimeStamp = 0) then
      raise ERestException.CreateFmt('Impossible to connect to %s:%d server', [aServerAddress, aServerPort]);
    if not result.SetUser(TSQLRestServerAuthenticationDefault, aUserName, aPassword) then
      raise ERestException.CreateFmt('%s:%d server rejected "%s" credentials', [aServerAddress, aServerPort, aUserName]);
  except
    result.Free;
    raise;
  end;
end;

function GetClientNoUser(const aServerAddress: string; aServerPort: integer; const aServerRoot: string; aHttps: boolean; const aProxyName, aProxyByPass: string; aSendTimeout, aReceiveTimeout, aConnectionTimeOut: Cardinal): TSQLRestClientHTTP;
begin
  result := TSQLRestClientHTTP.Create(aServerAddress, aServerPort,    //nil,
    GetModel(aServerRoot), true, aHttps, aProxyName, aProxyByPass, aSendTimeout, aReceiveTimeout, aConnectionTimeOut);
  try
    if (not result.Connect) or (result.ServerTimeStamp = 0) then
      raise ERestException.CreateFmt('Impossible to connect to %s:%d server', [aServerAddress, aServerPort]);
//    if not result.SetUser(TSQLRestServerAuthenticationDefault, aUserName, aPassword) then
//      raise ERestException.CreateFmt('%s:%d server rejected "%s" credentials', [aServerAddress, aServerPort, aUserName]);
  except
    result.Free;
    raise;
  end;
end;



{ TServiceMonitoring }

constructor TServiceMonitoring.Create(aClient: TSQLRestClientURI);
begin
  fServiceName := 'Monitoring';
  fServiceURI := 'Monitoring';
  fInstanceImplementation := sicClientDriven;
  fContractExpected := '*'; //'Monitoring 1.0'; //'1284AACBA83DEC79';
  inherited Create(aClient);
end;

function TServiceMonitoring.DateTimeToMoment(const aDateTime: TDateTime): Int64;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'DateTimeToMoment',1, // raise EServiceException on error
    [DateTimeToIso8601(aDateTime)],res);
  Result := res[0];
end;

destructor TServiceMonitoring.Destroy;
begin
  inherited;
end;

function TServiceMonitoring.MomentToDateTime(const aMoment: Int64): TDateTime;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'MomentToDateTime',1, // raise EServiceException on error
    [aMoment],res);
  Result := Iso8601ToDateTime(res[0]);
end;

function TServiceMonitoring.SensorValue(const SID: String): String;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorValue',1, // raise EServiceException on error
    [SID],res);
  Result := res[0];
end;

function TServiceMonitoring.SensorData(const SID: String): TDataRec;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorData',1, // raise EServiceException on error
    [SID],res);
  Result := Variant2TDataRec(res[0]);
end;

function TServiceMonitoring.SensorDataAsText(const SID: string): string;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorDataAsText',1, // raise EServiceException on error
    [SID],res);
  Result := res[0];
end;

function TServiceMonitoring.SensorDataExt(const SID: String): TDataRecExt;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorDataExt',1, // raise EServiceException on error
    [SID],res);
  Result := Variant2TDataRecExt(res[0]);
end;

function TServiceMonitoring.GroupSensorValue(const SIDs: TSIDArr): TValArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GroupSensorValue',1, // raise EServiceException on error
    [TSIDArr2Variant(SIDs)],res);
  Result := Variant2TValArr(res[0]);
end;

function TServiceMonitoring.GroupSensorData(const SIDs: TSIDArr): TDataRecArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GroupSensorData',1, // raise EServiceException on error
    [TSIDArr2Variant(SIDs)],res);
  Result := Variant2TDataRecArr(res[0]);
end;

function TServiceMonitoring.GroupSensorDataExt(const SIDs: TSIDArr): TDataRecExtArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GroupSensorDataExt',1, // raise EServiceException on error
    [TSIDArr2Variant(SIDs)],res);
  Result := Variant2TDataRecExtArr(res[0]);
end;

function TServiceMonitoring.SensorValueByID(const ID: TID): String;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorValueByID',1, // raise EServiceException on error
    [ID],res);
  Result := res[0];
end;

function TServiceMonitoring.SensorDataByID(const ID: TID): TDataRec;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorDataByID',1, // raise EServiceException on error
    [ID],res);
  Result := Variant2TDataRec(res[0]);
end;

function TServiceMonitoring.SensorDataExtByID(const ID: TID): TDataRecExt;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorDataExtByID',1, // raise EServiceException on error
    [ID],res);
  Result := Variant2TDataRecExt(res[0]);
end;

function TServiceMonitoring.GroupSensorValueByID(const IDs: TIDArr): TValArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GroupSensorValueByID',1, // raise EServiceException on error
    [TIDArr2Variant(IDs)],res);
  Result := Variant2TValArr(res[0]);
end;

function TServiceMonitoring.GroupSensorDataByID(const IDs: TIDArr): TDataRecArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GroupSensorDataByID',1, // raise EServiceException on error
    [TIDArr2Variant(IDs)],res);
  Result := Variant2TDataRecArr(res[0]);
end;

function TServiceMonitoring.GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GroupSensorDataExtByID',1, // raise EServiceException on error
    [TIDArr2Variant(IDs)],res);
  Result := Variant2TDataRecExtArr(res[0]);
end;

function TServiceMonitoring.SensorHistory(const SID: String; const FromDate: Int64; const ToDate: Int64): THistoryRecArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorHistory',1, // raise EServiceException on error
    [SID,FromDate,ToDate],res);
  Result := Variant2THistoryRecArr(res[0]);
end;

function TServiceMonitoring.SensorHistoryExt(const SID: String; const FromDate: Int64; const ToDate: Int64): THistoryRecExtArr;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorHistoryExt',1, // raise EServiceException on error
    [SID,FromDate,ToDate],res);
  Result := Variant2THistoryRecExtArr(res[0]);
end;

function TServiceMonitoring.SensorsDataAsText(const SIDs: TSIDArr; const aUseCache: Boolean): String;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SensorsDataAsText',1, // raise EServiceException on error
    [TSIDArr2Variant(SIDs),aUseCache],res);
  Result := res[0];
end;

function TServiceMonitoring.CloneGroup(const aGroupID: TID; const aCloneChildren: Boolean; const aCloneSensors: Boolean; const aCloneDevices: Boolean; const aCount: Integer): Integer;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'CloneGroup',1, // raise EServiceException on error
    [aGroupID,aCloneChildren,aCloneSensors,aCloneDevices,aCount],res);
  Result := res[0];
end;

procedure TServiceMonitoring.InitSensorsByID(const IDs: TIntegerDynArray);
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'InitSensorsByID',0, // raise EServiceException on error
    [TIntegerDynArray2Variant(IDs)],res);
end;

procedure TServiceMonitoring.InitGroupsByID(const IDs: TIntegerDynArray);
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'InitGroupsByID',0, // raise EServiceException on error
    [TIntegerDynArray2Variant(IDs)],res);
end;

procedure TServiceMonitoring.InitDevicesByID(const IDs: TIntegerDynArray);
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'InitDevicesByID',0, // raise EServiceException on error
    [TIntegerDynArray2Variant(IDs)],res);
end;


{ TServiceTracking }

constructor TServiceTracking.Create(aClient: TSQLRestClientURI);
begin
  fServiceName := 'Tracking';
  fServiceURI := 'Tracking';
  fInstanceImplementation := sicClientDriven;
//  fContractExpected := 'E1BDF348B7324919'; //'2FF6A08E0B28DB55';
  fContractExpected := 'TrackingService 1.0'; //'2FF6A08E0B28DB55';
  inherited Create(aClient);
end;

procedure TServiceTracking.CreateDevice(const aName, aLogin, aPhone, aProto: string);
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'CreateDevice',0, [aName, aLogin, aPhone, aProto], res);
end;

destructor TServiceTracking.Destroy;
begin
  inherited;
end;

function TServiceTracking.GetClients(): Variant;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GetClients',1, // raise EServiceException on error
    [],res);
  Result := res[0];
end;

function TServiceTracking.GetDevices(const Clients: TIDDynArray): Variant;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GetDevices',1, // raise EServiceException on error
    [TIDDynArray2Variant(Clients)],res);
  Result := res[0];
end;

function TServiceTracking.GetDevicesData(const Devices: TIDDynArray): Variant;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'GetDevicesData',1, // raise EServiceException on error
    [TIDDynArray2Variant(Devices)],res);
  Result := res[0];
end;

function TServiceTracking.GetReport(const DeviceID: TID; const Date1, Date2: Int64): Variant;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GetReport', 1, [DeviceID, Date1, Date2], res);
  result := res[0];
end;

function TServiceTracking.GetTrack(const DeviceID: TID; const Date1, Date2: Int64): Variant;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GetTrack', 1, [DeviceID, Date1, Date2], res);
  result := res[0];
end;

procedure TServiceTracking.SetDevice(const Device: Variant);
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SetDevice', 0, [Device], res);
end;

procedure TServiceTracking.SetTagValue(const DeviceID: TID; const TagSID: string; const Value: Variant);
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'SetTagValue', 0, [DeviceID, TagSID, Value], res);
end;

procedure TServiceTracking.ShareDevice(const aDeviceID: TID; const aLogin, aRight: string);
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'ShareDevice', 0, [aDeviceID, aLogin, aRight], res);
end;

{ TServiceDCSignUp }

constructor TServiceDCSignUp.Create(aClient: TSQLRestClientURI);
begin
  fServiceName := 'DCSignUp';
  fServiceURI := 'DCSignUp';
  fInstanceImplementation := sicClientDriven;
  fContractExpected := 'SignUp 1.0'; //'*';
  inherited Create(aClient);
end;

function TServiceDCSignUp.Help(): String;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'Help',1, // raise EServiceException on error
    [],res);
  Result := res[0];
end;

procedure TServiceDCSignUp.RequestSignUp(const Login: String; const EMail: String; const Password: String);
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'RequestSignUp',0, // raise EServiceException on error
    [Login,EMail,Password],res);
end;

function TServiceDCSignUp.ConfirmSignUp(const Key: String): THttpBody;
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'ConfirmSignUp',1, // raise EServiceException on error
    [Key],res,true);
  Result := VariantToHttpBody(res[0]);
end;

procedure TServiceDCSignUp.RequestResetPassword(const Login: String; const EMail: String);
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'RequestResetPassword',0, // raise EServiceException on error
    [Login,EMail],res);
end;

procedure TServiceDCSignUp.ConfirmResetPassword(const Key: String);
var res: TVariantDynArray;
begin
  fClient.CallRemoteService(self,'ConfirmResetPassword',0, // raise EServiceException on error
    [Key],res);
end;


{ TServiceDCSession }

constructor TServiceDCSession.Create(aClient: TSQLRestClientURI);
begin
  fServiceName := 'DCSession';
  fServiceURI := 'DCSession';
  fInstanceImplementation := sicClientDriven;
  fContractExpected := '*'; // '49237C319C65EA85';
  inherited Create(aClient);
end;

procedure TServiceDCSession.SetSessionInfo(const Client: String);
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SetSessionInfo', 0, [Client], res); // raise EServiceException on error
end;

end.
