unit LA.FMX.Chart.LineSeries;

interface

uses
  System.Classes, System.UITypes, System.SysUtils,
  FMXTee.Chart, FMXTee.Series,
  LA.Data.Link.Sensor, LA.Data.Link.Sensor.Intf,
  LA.Data.Types,
  LA.Interval,
  LA.FMX.Chart.Series.Intf;

const
  cSeriesErrorColor = TAlphaColorRec.Gray;

type
  TLALineSeries = class(TLineSeries, ILASensorLink, ILASeries)
  private
    FLink: TLASensorLink;
    /// оригинальные показания, полученные с сервера
    ///  над ними можно производить операции сдвига и маштабирования
    FValues: TLASensorValueSnapshots;
    FShift: Double;
    FScale: Double;
    procedure SetLink(const Value: TLASensorLink);
    function GetInterval: TLAInterval;
    procedure DoDataLinkChanged(Sender: TObject);
    procedure SetScale(const Value: Double);
    procedure SetShift(const Value: Double);
    function GetStateColor(aStateCode: Double): TAlphaColor;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FillData(aSkipIfNotPossible: Boolean = True);

    procedure AddValueRecToView(const aRec: TLASensorValueSnapshot);
    procedure ValuesToView;

    // по времени расчитывает значение
    // возвращает True, если удалось расчитать значение
    function CalcSeriesValue(aTime: Double; var aValue: Double): Boolean;
    function CalcSeriesValueAndDuration(aTime: Double; var aValue: Double; var aDuration: Double): Boolean;


    property Interval: TLAInterval read GetInterval;
  published
    property Link: TLASensorLink read FLink  write SetLink implements ILASensorLink;

    property Scale: Double read FScale write SetScale;
    property Shift: Double read FShift write SetShift;
  end;

var
  TeeMsg_GalleryLALine: string;
  TeeMsg_GalleryLA: string;

implementation

uses
  System.DateUtils, System.Math,
  LA.Net.Connector;

{ TLALineSeries }

procedure TLALineSeries.AddValueRecToView(const aRec: TLASensorValueSnapshot);
begin
  AddXY(aRec.FDateTime, aRec.FValue * Scale + Shift, FLink.ValueToText(aRec.FValue), GetStateColor(aRec.FState));
end;

function TLALineSeries.CalcSeriesValue(aTime: Double; var aValue: Double): Boolean;
var
  i: Integer;
  i1,i2: Integer;
begin
  i2 := -1;
  // ищем время больше заданного (возможен бинарный поиск, т.к. время только возрастает)
  for i := 0 to XValues.Count - 1 do
  begin
    // нашли точное соответствие
    if XValues[i] = aTime then
    begin
      aValue := YValues[i];
      Exit(True);
    end;
    // нашли индекс точки с большим временем
    if XValues[i] > aTime then
    begin
      i2 := i;
      Break;
    end;
  end;

  // все точки имеют МЕНЬШЕЕ время
  if i2 = -1 then
    Exit(False);

  // все точки имеют БОЛЬШЕЕ время
  if i2 = 0 then
    Exit(False);

  // мы что-то нашли
  Result := True;
  // результат где-то между i1 и i2
  i1 := i2 - 1;
  if (Link.Kind = TLASensorKind.skDiscret) or (YValues[i1] = YValues[i2]) then
    aValue := YValues[i1]
  else if (YValues[i1] <= YValues[i2]) and (Link.Kind = TLASensorKind.skCounterDown) then
    aValue := YValues[i1]
  else if (YValues[i1] >= YValues[i2]) and (Link.Kind = TLASensorKind.skCounterUp) then
    aValue := YValues[i1]
  else
  begin
    // y = y2 - (y2-y1)*(x2-x)/(x2-x1)
    aValue := YValues[i2] - (YValues[i2]-YValues[i1])*(XValues[i2]-aTime)/(XValues[i2]-XValues[i1]);
  end;
end;

function TLALineSeries.CalcSeriesValueAndDuration(aTime: Double; var aValue, aDuration: Double): Boolean;
var
  i: Integer;
  i1,i2: Integer;
begin
  aDuration := 0;

  i2 := -1;
  // ищем время больше заданного (возможен бинарный поиск, т.к. время только возрастает)
  for i := 0 to XValues.Count - 1 do
  begin
    // нашли точное соответствие
    if XValues[i] = aTime then
    begin
      aValue := YValues[i];
//      if (Link.Kind in  TLASensorKind.skCounterDown) and (i < XValues.Count - 1) then

      Result := True;
      Exit;
    end;
    // нашли индекс точки с большим временем
    if XValues[i] > aTime then
    begin
      i2 := i;
      Break;
    end;
  end;

  // все точки имеют МЕНЬШЕЕ время
  if i2 = -1 then
  begin
    Result := False;
    Exit;
  end;

  // все точки имеют БОЛЬШЕЕ время
  if i2 = 0 then
  begin
    Result := False;
    Exit;
  end;

  // мы что-то нашли
  Result := True;
  // результат где-то между i1 и i2
  i1 := i2 - 1;
  if (Link.Kind = TLASensorKind.skDiscret) or (YValues[i1] = YValues[i2]) then
  begin
    aValue := YValues[i1];
    aDuration := XValues[i2]-XValues[i1];
  end
  else if (YValues[i1] <= YValues[i2]) and (Link.Kind = TLASensorKind.skCounterDown) then
  begin
    aValue := YValues[i1];
    aDuration := XValues[i2]-XValues[i1];
  end
  else if (YValues[i1] >= YValues[i2]) and (Link.Kind = TLASensorKind.skCounterUp) then
  begin
    aValue := YValues[i1];
    aDuration := XValues[i2]-XValues[i1];
  end
  else
  begin
    // y = y2 - (y2-y1)*(x2-x)/(x2-x1)
    aValue := YValues[i2] - (YValues[i2]-YValues[i1])*(XValues[i2]-aTime)/(XValues[i2]-XValues[i1]);
  end;
end;

constructor TLALineSeries.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.NotifyConstantly := True;
  FLink.OnOwnerNotify := DoDataLinkChanged;

  FScale := 1;
  FShift := 0;
  XValues.DateTime := True;
  ColorEachLine := True;
  ColorEachPoint := True;
end;

destructor TLALineSeries.Destroy;
begin
  FLink.Free;
  FLink := nil;
  inherited;
end;

procedure TLALineSeries.DoDataLinkChanged(Sender: TObject);
var
  aRec: TLASensorValueSnapshot;
  aInterval: TLAInterval;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  aInterval := Interval;
  if not Assigned(aInterval) then
    Exit;

  if (FLink.Timestamp >= aInterval.Date1) and (FLink.Timestamp <= aInterval.Date2) then
  begin
    aRec.FDateTime := Now; //FLink.Timestamp;
    aRec.FValue := FLink.Value;
    aRec.FState := FLink.StatusCode;
    SetLength(FValues, Length(FValues) + 1);
    FValues[High(FValues)] := aRec;
    AddValueRecToView(aRec);

    // если ось времени в нормальном состоянии, то обновим ее шкалу
    if not ParentChart.Zoomed then
    begin
//      if (Interval.Kind = ) then
//        D2 := Max(Interval.Date2, Rec.x)
//      else
//        D2 := Interval.Date2;

      ParentChart.BottomAxis.SetMinMax(aInterval.Date1, aInterval.Date2); //Interval.Date2);
    end;

  end;
end;

procedure TLALineSeries.FillData(aSkipIfNotPossible: Boolean = True);
var
  i: Integer;
  aStream: TMemoryStream;
  aInterval: TLAInterval;
  aConnector: TLACustomConnector;
  aRec: TLASensorValueSnapshot;
  aDate1, aDate2: TDateTime;
begin
  if FLink.ID = '' then
    if aSkipIfNotPossible then
      Exit
    else
      raise Exception.Create('No sensor ID');

  aInterval := Interval;
  if not Assigned(aInterval) then
    if aSkipIfNotPossible then
      Exit
    else
      raise Exception.Create('No Interval');

  aConnector := FLink.Connector;
  if not Assigned(aConnector) then
    if aSkipIfNotPossible then
      Exit
    else
      raise Exception.Create('No Connector');

  if not aConnector.Connected and aSkipIfNotPossible then
    Exit;

  aStream := TMemoryStream.Create;
  try
    aDate1 := aInterval.Date1;
    aDate2 := aInterval.Date2;
    aConnector.SensorHistoryStream(aStream,
      FLink.ID, DateTimeToUnix(aDate1, False), DateTimeToUnix(aDate2, False),
      True, True, False, True, True);

    SetLength(FValues, aStream.Size div SizeOf(aRec));

    i := 0;
    aStream.Position := 0;
    while aStream.Position <= (aStream.Size - SizeOf(aRec)) do
    begin
      aStream.Read(aRec, SizeOf(aRec));

//      // исключаем заведомо неверные записи
//      if (aRec.FDateTime < aDate1) or (aRec.FDateTime > aDate2) then
//        Continue;
//      // контролируем, что время возрастает
//      if (i > 0) and (FValues[i - 1].FDateTime >= aRec.FDateTime) then
//        Continue;

      FValues[i] := aRec;
      Inc(i);
    end;

    if i < Length(FValues) then
      SetLength(FValues, i);
  finally
    aStream.Free;
  end;

  ValuesToView;
end;

function TLALineSeries.GetInterval: TLAInterval;
var
  i: IHasLAInterval;
begin
  // возвращаем Интервал построителя графиков, на котором наш график находится
  if Supports(ParentChart, IHasLAInterval, i) then
    Result := i.GetInterval
  else
    Result := nil;
end;

function TLALineSeries.GetStateColor(aStateCode: Double): TAlphaColor;
begin
  if SameValue(aStateCode, 0) then
    Result := Color
  else
    Result := cSeriesErrorColor;
end;

procedure TLALineSeries.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLALineSeries.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

procedure TLALineSeries.SetScale(const Value: Double);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    ValuesToView;
  end;
end;

procedure TLALineSeries.SetShift(const Value: Double);
begin
  if FShift <> Value then
  begin
    FShift := Value;
    ValuesToView;
  end;
end;

procedure TLALineSeries.ValuesToView;
begin
  BeginUpdate;
  try
    Clear;
    for var i := Low(FValues) to High(FValues) do
    begin
      if (i > 0) and
        (not SameValue(FValues[i-1].FValue, FValues[i].FValue)) and
        (
        (FLink.Kind = skDiscret) or
        not SameValue(FValues[i-1].FState, FValues[i].FState) or
        ((FLink.Kind = skCounterUp) and (FValues[i-1].FValue > FValues[i].FValue)) or
        ((FLink.Kind = skCounterDown) and (FValues[i-1].FValue < FValues[i].FValue))
        ) then
        AddXY(FValues[i].FDateTime, FValues[i-1].FValue * Scale + Shift, FLink.ValueToText(FValues[i-1].FValue), GetStateColor(FValues[i-1].FState));

      AddValueRecToView(FValues[i]);
    end;
  finally
    EndUpdate;
  end;
end;

initialization
  TeeMsg_GalleryLALine := 'LA Line';
  TeeMsg_GalleryLA := 'LA';
  RegisterTeeSeries(TLALineSeries, @TeeMsg_GalleryLALine, @TeeMsg_GalleryLA, 1);

finalization
  UnRegisterTeeSeries([TLALineSeries]);



end.
