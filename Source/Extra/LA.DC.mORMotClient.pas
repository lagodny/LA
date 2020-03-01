﻿/// remote access to a mORMot server using SynCrossPlatform* units
// - retrieved from http://localhost:89/DC/wrapper/CrossPlatform/mORMotClient.pas
// at 2019-12-26 09:36:41 using "CrossPlatform.pas.mustache" template
unit LA.DC.mORMotClient;

{
  WARNING:
    This unit has been generated by a mORMot 1.18.5438 server.
    Any manual modification of this file may be lost after regeneration.

  Synopse mORMot framework. Copyright (C) 2019 Arnaud Bouchez
    Synopse Informatique - http://synopse.info

  This unit is released under a MPL/GPL/LGPL tri-license,
  and therefore may be freely included in any application.

  This unit would work on Delphi 6 and later, under all supported platforms
  (including MacOSX, and NextGen iPhone/iPad), and the Free Pascal Compiler.
}

interface

uses
  SynCrossPlatformJSON, SynCrossPlatformSpecific, SynCrossPlatformREST;

type // define some record types, used as properties below
  TDataRec = record
    t: Int64;
    v: string;
  end;

  TDataRecExt = record
    SID: string;
    t: Int64;
    v: string;
    e: string;
  end;

  THistoryRec = record
    t: Int64;
    v: string;
  end;

  THistoryRecExt = record
    t: Int64;
    v: string;
    e: string;
  end;

type // define some dynamic array types, used as properties below
  TSIDArr = array of RawUTF8;

  TValArr = array of RawUTF8;

  TDataRecArr = array of TDataRec;

  TDataRecExtArr = array of TDataRecExt;

  TIDArr = array of Int64;

  THistoryRecArr = array of THistoryRec;

  THistoryRecExtArr = array of THistoryRecExt;

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
    function SensorValue(const SID: string): string;
    function SensorData(const SID: string): TDataRec;
    function SensorDataExt(const SID: string): TDataRecExt;
    function GroupSensorValue(const SIDs: TSIDArr): TValArr;
    function GroupSensorData(const SIDs: TSIDArr): TDataRecArr;
    function GroupSensorDataExt(const SIDs: TSIDArr): TDataRecExtArr;
    function SensorValueByID(const ID: UInt64): string;
    function SensorDataByID(const ID: UInt64): TDataRec;
    function SensorDataExtByID(const ID: UInt64): TDataRecExt;
    function GroupSensorValueByID(const IDs: TIDArr): TValArr;
    function GroupSensorDataByID(const IDs: TIDArr): TDataRecArr;
    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
    function SensorHistory(const SID: string; const FromDate: Int64; const ToDate: Int64): THistoryRecArr;
    function SensorHistoryExt(const SID: string; const FromDate: Int64; const ToDate: Int64): THistoryRecExtArr;
    function CloneGroup(const aGroupID: UInt64; const aCloneChildren: Boolean; const aCloneSensors: Boolean; const aCloneDevices: Boolean; const aCount: Integer): Integer;
  end;


  /// implements IMonitoring from http://localhost:89/DC/Monitoring
  // - this service will run in sicClientDriven mode
  TServiceMonitoring = class(TServiceClientAbstractClientDriven, IMonitoring)
  public
    constructor Create(aClient: TSQLRestClientURI); override;
    function SensorValue(const SID: string): string;
    function SensorData(const SID: string): TDataRec;
    function SensorDataExt(const SID: string): TDataRecExt;
    function GroupSensorValue(const SIDs: TSIDArr): TValArr;
    function GroupSensorData(const SIDs: TSIDArr): TDataRecArr;
    function GroupSensorDataExt(const SIDs: TSIDArr): TDataRecExtArr;
    function SensorValueByID(const ID: UInt64): string;
    function SensorDataByID(const ID: UInt64): TDataRec;
    function SensorDataExtByID(const ID: UInt64): TDataRecExt;
    function GroupSensorValueByID(const IDs: TIDArr): TValArr;
    function GroupSensorDataByID(const IDs: TIDArr): TDataRecArr;
    function GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
    function SensorHistory(const SID: string; const FromDate: Int64; const ToDate: Int64): THistoryRecArr;
    function SensorHistoryExt(const SID: string; const FromDate: Int64; const ToDate: Int64): THistoryRecExtArr;
    function CloneGroup(const aGroupID: UInt64; const aCloneChildren: Boolean; const aCloneSensors: Boolean; const aCloneDevices: Boolean; const aCount: Integer): Integer;
  end;

const
  /// the server port, corresponding to http://localhost:89
  SERVER_PORT = 89;
  /// the server model root name, corresponding to http://localhost:89
  SERVER_ROOT = 'DC';
  cSendTimeout = 30000;
  cReceiveTimeout = 30000;
  cConnectionTimeOut = 30000;

/// return the database Model corresponding to this server
//function GetModel(const aRoot: string=SERVER_ROOT): TSQLModel;

/// create a TSQLRestClientHTTP instance and connect to the server
// - it will use by default port 89 over root 'DC', corresponding
// to http://localhost:89/DC
// - secure connection will be established via TSQLRestServerAuthenticationDefault
// with the supplied credentials - on connection or authentication error,
// this function will raise a corresponding exception
function GetClient(const aServerAddress, aUserName, aPassword: string; aServerPort: integer = SERVER_PORT; const aServerRoot: string = SERVER_ROOT; aHttps: boolean = false; const aProxyName: string = ''; const aProxyByPass: string = ''; aSendTimeout: Cardinal = cSendTimeout; aReceiveTimeout: Cardinal = cReceiveTimeout; aConnectionTimeOut: Cardinal = cConnectionTimeOut): TSQLRestClientHTTP;

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


{ TServiceViewData }


{ TServiceMonitoring }

constructor TServiceMonitoring.Create(aClient: TSQLRestClientURI);
begin
  fServiceName := 'Monitoring';
  fServiceURI := 'Monitoring';
  fInstanceImplementation := sicClientDriven;
  fContractExpected := 'B815D8DCCF28CD72'; //'B80B355D5FB4D4A2';
  inherited Create(aClient);
end;

function TServiceMonitoring.SensorValue(const SID: string): string;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorValue', 1, // raise EServiceException on error
    [SID], res);
  Result := res[0];
end;

function TServiceMonitoring.SensorData(const SID: string): TDataRec;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorData', 1, // raise EServiceException on error
    [SID], res);
  Result := Variant2TDataRec(res[0]);
end;

function TServiceMonitoring.SensorDataExt(const SID: string): TDataRecExt;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorDataExt', 1, // raise EServiceException on error
    [SID], res);
  Result := Variant2TDataRecExt(res[0]);
end;

function TServiceMonitoring.GroupSensorValue(const SIDs: TSIDArr): TValArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GroupSensorValue', 1, // raise EServiceException on error
    [TSIDArr2Variant(SIDs)], res);
  Result := Variant2TValArr(res[0]);
end;

function TServiceMonitoring.GroupSensorData(const SIDs: TSIDArr): TDataRecArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GroupSensorData', 1, // raise EServiceException on error
    [TSIDArr2Variant(SIDs)], res);
  Result := Variant2TDataRecArr(res[0]);
end;

function TServiceMonitoring.GroupSensorDataExt(const SIDs: TSIDArr): TDataRecExtArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GroupSensorDataExt', 1, // raise EServiceException on error
    [TSIDArr2Variant(SIDs)], res);
  Result := Variant2TDataRecExtArr(res[0]);
end;

function TServiceMonitoring.SensorValueByID(const ID: UInt64): string;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorValueByID', 1, // raise EServiceException on error
    [ID], res);
  Result := res[0];
end;

function TServiceMonitoring.SensorDataByID(const ID: UInt64): TDataRec;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorDataByID', 1, // raise EServiceException on error
    [ID], res);
  Result := Variant2TDataRec(res[0]);
end;

function TServiceMonitoring.SensorDataExtByID(const ID: UInt64): TDataRecExt;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorDataExtByID', 1, // raise EServiceException on error
    [ID], res);
  Result := Variant2TDataRecExt(res[0]);
end;

function TServiceMonitoring.GroupSensorValueByID(const IDs: TIDArr): TValArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GroupSensorValueByID', 1, // raise EServiceException on error
    [TIDArr2Variant(IDs)], res);
  Result := Variant2TValArr(res[0]);
end;

function TServiceMonitoring.GroupSensorDataByID(const IDs: TIDArr): TDataRecArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GroupSensorDataByID', 1, // raise EServiceException on error
    [TIDArr2Variant(IDs)], res);
  Result := Variant2TDataRecArr(res[0]);
end;

function TServiceMonitoring.GroupSensorDataExtByID(const IDs: TIDArr): TDataRecExtArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'GroupSensorDataExtByID', 1, // raise EServiceException on error
    [TIDArr2Variant(IDs)], res);
  Result := Variant2TDataRecExtArr(res[0]);
end;

function TServiceMonitoring.SensorHistory(const SID: string; const FromDate: Int64; const ToDate: Int64): THistoryRecArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorHistory', 1, // raise EServiceException on error
    [SID, FromDate, ToDate], res);
  Result := Variant2THistoryRecArr(res[0]);
end;

function TServiceMonitoring.SensorHistoryExt(const SID: string; const FromDate: Int64; const ToDate: Int64): THistoryRecExtArr;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'SensorHistoryExt', 1, // raise EServiceException on error
    [SID, FromDate, ToDate], res);
  Result := Variant2THistoryRecExtArr(res[0]);
end;

function TServiceMonitoring.CloneGroup(const aGroupID: UInt64; const aCloneChildren: Boolean; const aCloneSensors: Boolean; const aCloneDevices: Boolean; const aCount: Integer): Integer;
var
  res: TVariantDynArray;
begin
  fClient.CallRemoteService(self, 'CloneGroup', 1, // raise EServiceException on error
    [aGroupID, aCloneChildren, aCloneSensors, aCloneDevices, aCount], res);
  Result := res[0];
end;

end.

