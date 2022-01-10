unit LA.Data.Sensor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  LA.Data.Source;

type
  TLASensorList = class;

  /// <summary>
  ///   Невизуальный компонент Датчик Мониторинга
  ///  содержит информацию о датчике Мониторинга:
  ///  - адрес в Мониторинге
  ///  - значение, момент времени и состояние
  /// </summary>
  [ObservableMember('Value')]
  [ObservableMember('Timestamp')]
  [ObservableMember('Status')]
  TLASensor = class(TComponent) //, IDCObserver)
  private
    [weak] FSensorList: TLASensorList;

    FID: string;
    FData: string;
    FValue: string;
    FTimestamp: TDateTime;
    FStatus: string;
    FEnabled: Boolean;
    FUpdateCounter: Integer;
    FIsDataChanged: Boolean;
    procedure SetValue(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTimestamp(const Value: TDateTime);
    procedure SetEnabled(const Value: Boolean);
  private
    procedure DataChanged;
    procedure SetID(const Value: string);
    function GetDataSource: TLADataSource;
    procedure SetSensorList(const Value: TLASensorList);
  protected
    function CanObserve(const ID: Integer): Boolean; override;
    procedure ObserverAdded(const ID: Integer; const Observer: IObserver); override;
    procedure ObserverToggle(const AObserver: IObserver; const Value: Boolean);

    procedure ReadState(Reader: TReader); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(AParent: TComponent); override;

    function GetID: string;
    procedure SetData(const aData: string);

    procedure EncodeData(const aData: string);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateData(const aValue: string; aTimestamp: TDateTime; const aStatus: string);

    property DataSource: TLADataSource read GetDataSource;

  published
    property SensorList: TLASensorList read FSensorList write SetSensorList;

    property ID: string read GetID write SetID;

    property Value: string read FValue write SetValue;
    property Timestamp: TDateTime read FTimestamp write SetTimestamp;
    property Status: string read FStatus write SetStatus;
    property Data: string read FData write SetData;

    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  /// <summary>
  ///   список датчиков
  ///  является родителем и владельцем датчиков
  ///  содержит ссылку на источник данных для датчиков
  /// </summary>
  TLASensorList = class(TComponent)
  private
    FSensors: TList<TLASensor>;
    FDataSource: TLADataSource;
    procedure SetDataSource(const Value: TLADataSource);
  protected

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddSensor(aSensor: TLASensor);
    procedure RemoveSensor(aSensor: TLASensor);

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Senosrs: TList<TLASensor> read FSensors;
  published
    property DataSource: TLADataSource read FDataSource write SetDataSource;
  end;



implementation

uses
  System.SysUtils, System.DateUtils,
  //vcl.Dialogs,
  LA.Data.Link.Sensor;

procedure TLASensor.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

function TLASensor.CanObserve(const ID: Integer): Boolean;
begin
  case ID of
    TObserverMapping.EditLinkID,
    TObserverMapping.ControlValueID:
      Result := True;
  else
    Result := False;
  end;
end;

constructor TLASensor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  if AOwner is TLASensorList then
//    SensorList := TLASensorList(AOwner);
end;

procedure TLASensor.DataChanged;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  if FUpdateCounter = 0 then
  begin
    TLinkObservers.ControlChanged(Self);
    FIsDataChanged := False;
  end
  else
    FIsDataChanged := True;
end;

destructor TLASensor.Destroy;
begin
//  SensorList := nil;
  inherited;
end;

procedure TLASensor.EncodeData(const aData: string);
begin
  var a := aData.Split([';']);
  var L := Length(a);
  if L >= 2 then
  begin
    if a[0] = ID then
    begin
      FValue := a[1];
      if L >= 3 then
      begin
        FStatus := a[2];
        if L >= 4 then
          FTimestamp := ISO8601ToDate(a[2])
        else
          FTimestamp := Now;
      end
      else
    end;
  end;
end;

procedure TLASensor.EndUpdate;
begin
  Dec(FUpdateCounter);
  if (FUpdateCounter = 0) and (FIsDataChanged) then
    DataChanged;
end;

function TLASensor.GetDataSource: TLADataSource;
begin
  if Assigned(FSensorList) then
    Result := FSensorList.DataSource
  else
    Result := nil;
end;

function TLASensor.GetID: string;
begin
  Result := FID;
end;

function TLASensor.GetParentComponent: TComponent;
begin
  if Assigned(SensorList) then
    Result := SensorList
  else
    Result := inherited GetParentComponent;
end;

function TLASensor.HasParent: Boolean;
begin
  if FSensorList <> nil then
    Result := True
  else
    Result := inherited HasParent;
end;

procedure TLASensor.ObserverAdded(const ID: Integer; const Observer: IObserver);
begin
  if ID = TObserverMapping.EditLinkID then
    Observer.OnObserverToggle := ObserverToggle;
end;

procedure TLASensor.ObserverToggle(const AObserver: IObserver; const Value: Boolean);
var
  LEditLinkObserver: IEditLinkObserver;
begin
  if Value then
  begin
    if Supports(AObserver, IEditLinkObserver, LEditLinkObserver) then
      { disable the Item if the associated field does not support editing }
      Enabled := not LEditLinkObserver.IsReadOnly;
  end else
    Enabled := True;
end;

procedure TLASensor.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);

  if Reader.Parent is TLASensorList then
     SensorList := TLASensorList(Reader.Parent);
end;

procedure TLASensor.SetData(const aData: string);
begin
  if aData <> FData then
  begin
    FData := aData;
    EncodeData(FData);
    DataChanged;
  end;
end;

procedure TLASensor.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TLASensor.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLASensor.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) and (AParent is TLASensorList) then
    SensorList := TLASensorList(AParent);
end;

procedure TLASensor.SetSensorList(const Value: TLASensorList);
begin
  if FSensorList = Value then
    Exit;

  if Assigned(FSensorList) then
    FSensorList.RemoveSensor(Self);

  if Assigned(Value) then
    Value.AddSensor(Self);

  FSensorList := Value;
end;

procedure TLASensor.SetStatus(const Value: string);
begin
  if FStatus <> Value then
  begin
    FStatus := Value;
    DataChanged;
  end;
end;

procedure TLASensor.SetTimestamp(const Value: TDateTime);
begin
  if FTimestamp <> Value then
  begin
    FTimestamp := Value;
    DataChanged;
  end;
end;

procedure TLASensor.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    DataChanged;
  end;
end;

procedure TLASensor.UpdateData(const aValue: string; aTimestamp: TDateTime; const aStatus: string);
begin
  if (FValue <> aValue) or (FTimestamp <> aTimestamp) or (FStatus <> aStatus) then
  begin
    FValue := aValue;
    FTimestamp := aTimestamp;
    FStatus := aStatus;
    DataChanged;
  end;
end;


{ TLASensorList }

procedure TLASensorList.AddSensor(aSensor: TLASensor);
begin
  Assert(Assigned(aSensor));

  // добавляем в список
  FSensors.Add(aSensor);
  aSensor.FSensorList := Self;
  aSensor.FreeNotification(Self);

  // подключаем к DataSource
  if Assigned(FDataSource) then
    FDataSource.Attach(TLASensorLink.Create(aSensor));
end;

constructor TLASensorList.Create(AOwner: TComponent);
begin
  inherited;
  FSensors := TObjectList<TLASensor>.Create(True);
end;

destructor TLASensorList.Destroy;
begin
  DataSource := nil;
  FSensors.Free;
  FSensors := nil;
  inherited;
end;

procedure TLASensorList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  aSensor: TLASensor;
begin
  for aSensor in FSensors do
  begin
    if aSensor.Owner = Root then Proc(aSensor);
  end;
end;

procedure TLASensorList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent is TLASensor) and Assigned(FSensors) then
      RemoveSensor(TLASensor(AComponent))
    else if (AComponent is TLADataSource) then
      DataSource := nil;
  end;
end;

procedure TLASensorList.RemoveSensor(aSensor: TLASensor);
begin
  Assert(Assigned(aSensor));

  // отключаем датчик от DataSource
  if Assigned(FDataSource) then
    FDataSource.DetachObject(aSensor);

  // удаляем из списка
  //FSensors.Remove(aSensor);

  if FSensors.Extract(aSensor) <> nil then
  begin
    aSensor.RemoveFreeNotification(Self);
    aSensor.FSensorList := nil;
  end;

end;

procedure TLASensorList.SetDataSource(const Value: TLADataSource);
var
  i: Integer;
begin
  if FDataSource = Value then
    Exit;

  // отключаем ВСЕ датчики от старого DataSource
  if Assigned(FDataSource) then
  begin
    for i := 0 to FSensors.Count - 1 do
      FDataSource.DetachObject(FSensors[i]);
  end;

  // подлключаем ВСЕ датчики к новому DataSorce
  if Assigned(Value) then
  begin
    for i := 0 to FSensors.Count - 1 do
      Value.Attach(TLASensorLink.Create(FSensors[i]));
  end;
  FDataSource := Value;
end;


end.
