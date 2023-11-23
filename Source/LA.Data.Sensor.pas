unit LA.Data.Sensor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  LA.Data.Link.Sensor,
  LA.Data.Source;

type
  TLASensorList = class;

  /// <summary>
  ///   Невизуальный компонент Датчик Мониторинга
  ///  содержит линк на датчик Мониторинга TLASensorLink
  ///  Поддерживает возможность связывания Binding
  /// </summary>
  [ObservableMember('Value')]
  [ObservableMember('Text')]
//  [ObservableMember('Timestamp')]
//  [ObservableMember('Status')]
  TLASensor = class(TComponent) //, IDCObserver)
  private
    [weak] FSensorList: TLASensorList;

    FLink: TLASensorLink;

//  FEnabled: Boolean;
    FUpdateCounter: Integer;
    FIsDataChanged: Boolean;

    function GetValue: Double;
    procedure SetValue(const Value: Double);
    function GetText: string;
    procedure SetText(const Value: string);
  private
    procedure DataChanged;
    procedure SetSensorList(const Value: TLASensorList);
//  procedure SetEnabled(const Value: Boolean);
    procedure SetLink(const Value: TLASensorLink);
    procedure DoDataLinkChanged(Sender: TObject);
  protected
    function CanObserve(const ID: Integer): Boolean; override;
//    procedure ObserverAdded(const ID: Integer; const Observer: IObserver); override;
//    procedure ObserverToggle(const AObserver: IObserver; const Value: Boolean);

    procedure ReadState(Reader: TReader); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(AParent: TComponent); override;

//    procedure BeginUpdate;
//    procedure EndUpdate;
//    procedure UpdateData(const aValue: string; aTimestamp: TDateTime; const aStatus: string);
  published
    property Link: TLASensorLink read FLink write SetLink;
    property SensorList: TLASensorList read FSensorList write SetSensorList;

    property Value: Double read GetValue write SetValue;
    property Text: string read GetText write SetText;

//  property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  /// <summary>
  ///   Список датчиков
  ///  является родителем и владельцем датчиков
  ///  Удобно создавать несколько списков датчиков для их группировки
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
    property Sensors: TList<TLASensor> read FSensors;
  published
    property DataSource: TLADataSource read FDataSource write SetDataSource;
  end;



implementation

uses
  System.SysUtils, System.DateUtils;

//procedure TLASensor.BeginUpdate;
//begin
//  Inc(FUpdateCounter);
//end;

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
  FLink := TLASensorLink.Create(Self);
  FLink.OnDataChange := DoDataLinkChanged;

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
  FreeAndNil(FLink);
//  SensorList := nil;
  inherited;
end;

procedure TLASensor.DoDataLinkChanged(Sender: TObject);
begin
  TLinkObservers.ControlChanged(Self);
end;

//procedure TLASensor.EncodeData(const aData: string);
//begin
//  var a := aData.Split([';']);
//  var L := Length(a);
//  if L >= 2 then
//  begin
//    if a[0] = ID then
//    begin
//      FValue := a[1];
//      if L >= 3 then
//      begin
//        FStatus := a[2];
//        if L >= 4 then
//          FTimestamp := ISO8601ToDate(a[2])
//        else
//          FTimestamp := Now;
//      end
//      else
//    end;
//  end;
//end;

//procedure TLASensor.EndUpdate;
//begin
//  Dec(FUpdateCounter);
//  if (FUpdateCounter = 0) and (FIsDataChanged) then
//    DataChanged;
//end;

//function TLASensor.GetDataSource: TLADataSource;
//begin
//  if Assigned(FSensorList) then
//    Result := FSensorList.DataSource
//  else
//    Result := nil;
//end;

//function TLASensor.GetID: string;
//begin
//  Result := FID;
//end;

function TLASensor.GetParentComponent: TComponent;
begin
  if Assigned(SensorList) then
    Result := SensorList
  else
    Result := inherited GetParentComponent;
end;

function TLASensor.GetText: string;
begin
  Result := FLink.Text;
end;

function TLASensor.GetValue: Double;
begin
  Result := FLink.Value;
end;

function TLASensor.HasParent: Boolean;
begin
  if FSensorList <> nil then
    Result := True
  else
    Result := inherited HasParent;
end;

procedure TLASensor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

//procedure TLASensor.ObserverAdded(const ID: Integer; const Observer: IObserver);
//begin
//  if ID = TObserverMapping.EditLinkID then
//    Observer.OnObserverToggle := ObserverToggle;
//end;

//procedure TLASensor.ObserverToggle(const AObserver: IObserver; const Value: Boolean);
//var
//  LEditLinkObserver: IEditLinkObserver;
//begin
//  if Value then
//  begin
//    if Supports(AObserver, IEditLinkObserver, LEditLinkObserver) then
//      { disable the Item if the associated field does not support editing }
//      Enabled := not LEditLinkObserver.IsReadOnly;
//  end else
//    Enabled := True;
//end;

procedure TLASensor.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);

  if Reader.Parent is TLASensorList then
     SensorList := TLASensorList(Reader.Parent);
end;

//procedure TLASensor.SetData(const aData: string);
//begin
//  if aData <> FData then
//  begin
//    FData := aData;
//    EncodeData(FData);
//    DataChanged;
//  end;
//end;

//procedure TLASensor.SetEnabled(const Value: Boolean);
//begin
//  FEnabled := Value;
//end;

procedure TLASensor.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

//procedure TLASensor.SetID(const Value: string);
//begin
//  FID := Value;
//end;

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
  begin
    FSensorList.RemoveSensor(Self);
    Link.DataSource := nil;
  end;

  if Assigned(Value) then
  begin
    Value.AddSensor(Self);
    Link.DataSource := Value.DataSource
  end;

  FSensorList := Value;
end;

procedure TLASensor.SetText(const Value: string);
begin
  FLink.Text := Value;
end;

//procedure TLASensor.SetStatus(const Value: string);
//begin
//  if FStatus <> Value then
//  begin
//    FStatus := Value;
//    DataChanged;
//  end;
//end;

//procedure TLASensor.SetTimestamp(const Value: TDateTime);
//begin
//  if FTimestamp <> Value then
//  begin
//    FTimestamp := Value;
//    DataChanged;
//  end;
//end;

procedure TLASensor.SetValue(const Value: Double);
begin
//  if FValue <> Value then
//  begin
//    FValue := Value;
//    DataChanged;
//  end;
  FLink.Value := Value;
end;

//procedure TLASensor.UpdateData(const aValue: string; aTimestamp: TDateTime; const aStatus: string);
//begin
////  if (FValue <> aValue) or (FTimestamp <> aTimestamp) or (FStatus <> aStatus) then
////  begin
////    FValue := aValue;
////    FTimestamp := aTimestamp;
////    FStatus := aStatus;
////    DataChanged;
////  end;
//end;


{ TLASensorList }

procedure TLASensorList.AddSensor(aSensor: TLASensor);
begin
  Assert(Assigned(aSensor));

  // добавляем в список
  FSensors.Add(aSensor);
  aSensor.FSensorList := Self;
  aSensor.FreeNotification(Self);
  // проставляем DataSource
  aSensor.Link.DataSource := DataSource;
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
    else if (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TLASensorList.RemoveSensor(aSensor: TLASensor);
begin
  Assert(Assigned(aSensor));

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
  if FDataSource <> Value then
  begin
    // проставляем всем датчикам новый DataSource
    for i := 0 to FSensors.Count - 1 do
      FSensors[i].Link.DataSource := Value;

    FDataSource := Value;
  end;
end;


end.
