unit App.Events;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  NX.Horizon,
  LA.Interval;

type
  // map
  TMapCommandEnum = (
    mcFocusOnObject,
    mcAddObjectsToMap,
    mcRemovObjectsFromMap,
    mcMakeTrack,
    mcMakeReport,
    mcUpdateTrack,
    mcUpdateDevice
  );

  // Map view
  TMapViewCommand = class
  private
    FCommand: TMapCommandEnum;
    FIDArray: TIntegerDynArray;
    FInterval: TLAInterval;
    procedure SetInterval(const Value: TLAInterval);
  public
    constructor Create; overload;
    constructor Create(aCommand: TMapCommandEnum; aIDArray: TIntegerDynArray; aInterval: TLAInterval); overload;
    destructor Destroy; override;

    property Command: TMapCommandEnum read FCommand write FCommand;
    property IDArray: TIntegerDynArray read FIDArray write FIDArray;
    property Interval: TLAInterval read FInterval write SetInterval;
  end;
  IMapViewCommandEvent = INxEvent<TMapViewCommand>;
  TMapViewCommandEvent = TNxEvent<TMapViewCommand>;

  // Objects
  TObjectsCommandEnum = (
    ocRefresh
  );

  // Objcects view
  TObjectsViewCommand = class
  private
    FCommand: TObjectsCommandEnum;
  public
    constructor Create; overload;
    constructor Create(aCommand: TObjectsCommandEnum); overload;

    property Command: TObjectsCommandEnum read FCommand write FCommand;
  end;
  IObjectsViewCommandEvent = INxEvent<TObjectsViewCommand>;
  TObjectsViewCommandEvent = TNxEvent<TObjectsViewCommand>;

  // Report
  TReportCommandEnum = (
    rcMakeReport
  );

  // Report view
  TReportViewCommand = class
  private
    FInterval: TLAInterval;
    FCommand: TReportCommandEnum;
    FIDArray: TIntegerDynArray;
    procedure SetInterval(const Value: TLAInterval);
  public
    constructor Create; overload;
    constructor Create(aCommand: TReportCommandEnum; aIDArray: TIntegerDynArray; aInterval: TLAInterval); overload;
    destructor Destroy; override;

    property Command: TReportCommandEnum read FCommand write FCommand;
    property IDArray: TIntegerDynArray read FIDArray write FIDArray;
    property Interval: TLAInterval read FInterval write SetInterval;
  end;

  IReportViewCommandEvent = INxEvent<TReportViewCommand>;
  TReportViewCommandEvent = TNxEvent<TReportViewCommand>;


  // Device editor
  TDeviceEditorCommandEnum = (
    decEditDevice
  );

  // Device editor view
  TDeviceEditorViewCommand = class
  private
    FCommand: TDeviceEditorCommandEnum;
    FDeviceID: Integer;
  public
    constructor Create; overload;
    constructor Create(aCommand: TDeviceEditorCommandEnum; aDeviceID: Integer); overload;

    property Command: TDeviceEditorCommandEnum read FCommand write FCommand;
    property DeviceID: Integer read FDeviceID write FDeviceID;
  end;

  IDeviceEditorViewCommandEvent = INxEvent<TDeviceEditorViewCommand>;
  TDeviceEditorViewCommandEvent = TNxEvent<TDeviceEditorViewCommand>;

  // Device sharing
  TDeviceShareCommandEnum = (
    dscShareDevice
  );

  // Device share view
  TDeviceShareViewCommand = class
  private
    FCommand: TDeviceShareCommandEnum;
    FDeviceID: Integer;
  public
    constructor Create; overload;
    constructor Create(aCommand: TDeviceShareCommandEnum; aDeviceID: Integer); overload;

    property Command: TDeviceShareCommandEnum read FCommand write FCommand;
    property DeviceID: Integer read FDeviceID write FDeviceID;
  end;

  IDeviceShareViewCommandEvent = INxEvent<TDeviceShareViewCommand>;
  TDeviceShareViewCommandEvent = TNxEvent<TDeviceShareViewCommand>;





  // Main view
  TMainViewCommandEnum = (
    mcNone,
    mcShowLogin,
    mcHome,
    mcShowInterval,
    mcShowSettings
//    mcShowMap,
//    mcShowObjects,
//    mcShowObjectEditor,
//    mcShowAddObject,
//    mcShowShareObject,
//    mcShowMainMenu,
//    mcSignUp,
//    mcShowTest,
//    mcShowReport,
//    mcShowNotifications,
//    mcShowSenderSettings

//    mcAppStateChanged
  );

  TMainViewEventRec = record
    // команда, которая должна быть распознана
    Command: TMainViewCommandEnum;
    // процедура, которая может быть выполнена, например при нажатии кнопки в вызванном диалоге, либо при переключении состояния
    Proc: TProc<TObject>;
    // параметр, может быть использован в процедуре, например результат работы диалога в виде объекта, например TLAInterval
    // запись отвечает
    //Param: TPersistent;
    // дополнительные данные (JSON)
    Data: string;
    constructor Create(aCommand: TMainViewCommandEnum; aProc: TProc<TObject>; aData: string); overload;
  end;
  IMainViewCommand = INxEvent<TMainViewEventRec>;
  TMainViewCommand = TNxEvent<TMainViewEventRec>;

//  TMainViewCommand = class
//  private
//    FCommand: TMainViewCommandEnum;
//    FProc: TProc;
//    FParam: TPersistent;
//    FData: string;
//  public
//    constructor Create; overload;
//    constructor Create(aCommand: TMainViewCommandEnum; aProc: TProc; aParam: TPersistent; const aData: string); overload;
//
//    property Command: TMainViewCommandEnum read FCommand write FCommand;
//    property Proc: TProc read FProc write FProc;
//    property Param: TPersistent read FParam write FParam;
//    property Data: string read FData write FData;
//  end;
//  IMainViewCommandEvent = INxEvent<TMainViewCommand>;
//  TMainViewCommandEvent = TNxEvent<TMainViewCommand>;




implementation

//{ TMapIDArrayParam }
//
//constructor TMapIDArrayParam.Create(aCommand: TMapCommandEnum; aIDArray: TIntegerDynArray);
//begin
//  Command := aCommand;
//  IDArray := aIDArray;
//end;

{ TMainViewEventRec }

constructor TMainViewEventRec.Create(aCommand: TMainViewCommandEnum; aProc: TProc<TObject>; aData: string);
begin
  Command := aCommand;
  Proc := aProc;
  Data := aData;
end;

{ TMapViewCommnd }

constructor TMapViewCommand.Create(aCommand: TMapCommandEnum; aIDArray: TIntegerDynArray; aInterval: TLAInterval);
begin
  Create;
  Command := aCommand;
  IDArray := aIDArray;
  Interval := aInterval;
end;

destructor TMapViewCommand.Destroy;
begin
  FInterval.Free;
  inherited;
end;

constructor TMapViewCommand.Create;
begin
  FInterval := TLAInterval.Create;
end;

procedure TMapViewCommand.SetInterval(const Value: TLAInterval);
begin
  if Assigned(Value) then
    FInterval.Assign(Value);
end;

{ TReportViewCommand }

constructor TReportViewCommand.Create;
begin
  FInterval := TLAInterval.Create;
end;

constructor TReportViewCommand.Create(aCommand: TReportCommandEnum; aIDArray: TIntegerDynArray; aInterval: TLAInterval);
begin
  Create;
  Command := aCommand;
  IDArray := aIDArray;
  Interval := aInterval;
end;

destructor TReportViewCommand.Destroy;
begin
  FInterval.Free;
  inherited;
end;

procedure TReportViewCommand.SetInterval(const Value: TLAInterval);
begin
  if Assigned(Value) then
    FInterval.Assign(Value);
end;

{ TMainViewCommand }

//constructor TMainViewCommand.Create(aCommand: TMainViewCommandEnum; aProc: TProc; aParam: TPersistent; const aData: string);
//begin
//  aParam := T
//end;
//
//constructor TMainViewCommand.Create;
//begin
//
//end;

{ TDeviceEditorViewCommand }

constructor TDeviceEditorViewCommand.Create(aCommand: TDeviceEditorCommandEnum; aDeviceID: Integer);
begin
  Create;
  Command := aCommand;
  DeviceID := aDeviceID;
end;

constructor TDeviceEditorViewCommand.Create;
begin
  inherited;
end;

{ TObjectsViewCommand }

constructor TObjectsViewCommand.Create(aCommand: TObjectsCommandEnum);
begin
  Create;
  FCommand := aCommand;
end;

constructor TObjectsViewCommand.Create;
begin
  inherited;
end;

{ TDeviceShareViewCommand }

constructor TDeviceShareViewCommand.Create(aCommand: TDeviceShareCommandEnum; aDeviceID: Integer);
begin
  Create;
  FCommand := aCommand;
  FDeviceID := aDeviceID;
end;

constructor TDeviceShareViewCommand.Create;
begin
  inherited;
end;

end.
