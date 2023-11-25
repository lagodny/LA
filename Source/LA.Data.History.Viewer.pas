unit LA.Data.History.Viewer;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults, System.Generics.Collections,
  LA.Data.Link.Sensor,
  LA.Data.Source,
  LA.Net.Connector;

type
  // снимок данных датчика: значение и состояние на момент времени
  TLASensorValueSnapshot = packed record
    FDateTime: TDateTime;
    FValue: Double;
    FState: Double;
  end;
  TLASensorValueSnapshots = array of TLASensorValueSnapshot;

  /// группа датчиков с историей и возможностью работы с ней
  ///  - получение данных на момент времени
  ///  - поиск следующего/предыдущего значения в истории
  TLASensorLinkGroupHistory = class
  private
    FID: string;
    FKind: TLASensorKind;
    FValues: TLASensorValueSnapshots;
    /// позиция в массиве Values, полученная при вычислении снимка на момент времени функцией GetSnapshotOnDate
    ///  будет использована при получении следующего или предыдущего доступного момента времени
    FPosition: Integer;
    // сохнаненный источник данных нужен для восстановления исходного при отключении линков
    FStoredDataSource: TLADataSource;
    FItems: TLASensorLinkList;
    function GetIsEmpty: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetNextMoment(var aMoment: TDateTime): Boolean;
    function GetPredMoment(var aMoment: TDateTime): Boolean;

    function GetIndexOnDate(aDateTime: TDateTime): Integer;
    function GetSnapshotOnDate(aDateTime: TDateTime; var aSnapshot: TLASensorValueSnapshot): Boolean;

    procedure ClearValues;
    procedure FillValues(const aDate1, aDate2: TDateTime; aDefConnector: TLACustomConnector = nil);

    property ID: string read FID write FID;
    property Kind: TLASensorKind read FKind write FKind;

    property Items: TLASensorLinkList read FItems;
    property Values: TLASensorValueSnapshots read FValues write FValues;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  ///  Предназначен для просмотра изменений данных по подключеным датчикам за выбранный период.
  /// </summary>
  /// <remarks> Пример использования:
  /// <code>
  ///  FHistoryViewer.FillHistory(EncodeDateTime(2023,11,21,0,0,0,0), EncodeDateTime(2023,11,22,0,0,0,0));
  ///  FHistoryViewer.First;
  ///  while not FHistoryViewer.Eof do
  ///  begin
  ///    //Some code to process Links
  ///    // ...
  ///    FHistoryViewer.Next;
  ///  end;
  /// </code>
  /// </remarks>
  TLAHistoryViewer = class(TLADataSource)
  private
    FDate2: TDateTime;
    FDate1: TDateTime;
    FConnector: TLACustomConnector;
    FGroups: TObjectList<TLASensorLinkGroupHistory>;
    FCurrentMoment: TDateTime;
    FBof: Boolean;
    FEof: Boolean;
    FActive: Boolean;

    procedure SetDate1(const Value: TDateTime);
    procedure SetDate2(const Value: TDateTime);
    procedure SetConnector(const Value: TLACustomConnector);
    procedure SetCurrentMoment(const Value: TDateTime);
    procedure SetActive(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Attach(const aLink: TLADataLink); override;
    procedure Detach(const aLink: TLADataLink); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisconnectLinks;

    function GetNextMoment: TDateTime;
    function GetPredMoment: TDateTime;
    procedure CalcValuesOnMoment(aMoment: TDateTime);

    procedure ClearHistory;
    procedure FillHistory(const aDate1, aDate2: TDateTime);

    procedure First;
    procedure Last;
    function Pred: Boolean;
    function Next: Boolean;

    property Groups: TObjectList<TLASensorLinkGroupHistory> read FGroups;

    // знание Конца и Начала данных нам нужно для определения доступности кнопок при управлении промоткой
    property Eof: Boolean read FEof;
    property Bof: Boolean read FBof;

  published
    property Date1: TDateTime read FDate1 write SetDate1;
    property Date2: TDateTime read FDate2 write SetDate2;
    property Active: Boolean read FActive write SetActive stored False;
    // 1. назначаем HistoryViewer источником данных для визуальных компонентов (LALabel.DataSource := LAHistoryViewer)
    // 2. задаем период (Date1, Date2)
    // 3. подключаем Connector
    // 4. активируем Active := True
    // 5. меняем CurrentMoment, чтобы видеть показания датчиков на этот момент времени в режиме разработки
    property CurrentMoment: TDateTime read FCurrentMoment write SetCurrentMoment stored False;
    // подключение к серверу Мониторинга
    property Connector: TLACustomConnector read FConnector write SetConnector;
  end;

implementation

uses
  System.DateUtils,
  LA.Data.Sensor.Updater;

{ TLAHistoryViewer }

procedure TLAHistoryViewer.Attach(const aLink: TLADataLink);
var
  aGroup: TLASensorLinkGroupHistory;
  aInsertIndex: Integer;
begin
  if not (aLink is TLASensorLink) then
    Exit;

  FLock.BeginWrite;
  try
    aGroup := TLASensorLinkGroupHistory.Create;
    aGroup.ID := aLink.ID;
    if aLink.DataSource is TLASensorUpdater then
      aGroup.FStoredDataSource := aLink.DataSource
    else
      aGroup.FStoredDataSource := nil;

    if not FGroups.BinarySearch(aGroup, aInsertIndex) then
    begin
      FGroups.Insert(aInsertIndex, aGroup);
      aGroup.Kind := TLASensorLink(aLink).Kind;
    end
    else
    begin
      aGroup.Free;
      aGroup := FGroups[aInsertIndex];
    end;
    aGroup.Items.Add(TLASensorLink(aLink));

    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TLAHistoryViewer.CalcValuesOnMoment(aMoment: TDateTime);
var
  aLink: TLASensorLink;
  aGroup: TLASensorLinkGroupHistory;
  aRec: TLASensorValueSnapshot;
begin
  for aGroup in FGroups do
  begin
    if aGroup.GetSnapshotOnDate(aMoment, aRec) then
    begin
      for aLink in aGroup.Items do
      begin
        aLink.Value := aRec.FValue;
        aLink.StatusCode := Trunc(aRec.FState);
        aLink.Timestamp := aRec.FDateTime;
        aLink.Text := aLink.DisplayValue;
        aLink.Status := aLink.DisplayStatus;
        aLink.Notify;
      end;
    end;
  end;
end;

procedure TLAHistoryViewer.ClearHistory;
begin
  for var aGroup in Groups do
    aGroup.ClearValues;
  FCurrentMoment := 0;
  FActive := False;
end;

constructor TLAHistoryViewer.Create(AOwner: TComponent);
begin
  inherited;
  FDate1 := Trunc(Now - 1);
  FDate2 := Trunc(Now);

  FGroups := TObjectList<TLASensorLinkGroupHistory>.Create(TDelegatedComparer<TLASensorLinkGroupHistory>.Create(
    function (const aLeft, aRight: TLASensorLinkGroupHistory): Integer
    begin
      // группы должны быть отсортированы в порядке возрастания ID, чтобы ускорить поиск группы при добавлении/удалении линков
      Result := CompareText(aLeft.ID, aRight.ID);
      // группы могут иметь одинаковые ID, тогда сравниваем их FStoredDataSource (источники данных)
      if Result = 0 then
        Result := Integer(aLeft.FStoredDataSource) - Integer(aRight.FStoredDataSource);
    end)
    // мы отвечаем за уничтожение групп
  , True);
end;

destructor TLAHistoryViewer.Destroy;
begin
  FGroups.Free;
  inherited;
end;

procedure TLAHistoryViewer.Detach(const aLink: TLADataLink);
var
  aGroup: TLASensorLinkGroupHistory;
  aGroupIndex: Integer;
begin
  FLock.BeginWrite;
  try
    aGroup := TLASensorLinkGroupHistory.Create;
    try
      aGroup.ID := aLink.ID;
      if FGroups.BinarySearch(aGroup, aGroupIndex, TDelegatedComparer<TLASensorLinkGroupHistory>.Create(
        function (const aLeft, aRight: TLASensorLinkGroupHistory): Integer
        begin
          // при удалении поиск выполняем только по ID
          Result := CompareText(aLeft.ID, aRight.ID);
        end)) then
      begin
        // может быть несколько групп с одинаковым ID (мы нашли одну из них, возможно не первую)

        // находим первую
        while (aGroupIndex > 0) and SameText(FGroups[aGroupIndex - 1].ID, aLink.ID) do
          Dec(aGroupIndex);

        // перебираем все группы с заданным ID в поисках нужного линка
        while (aGroupIndex < FGroups.Count) and SameText(FGroups[aGroupIndex].ID, aLink.ID) do
        begin
          // пытаемся найти и удалить нужный линк
          FLinksChanged := FGroups[aGroupIndex].Items.Remove(TLASensorLink(aLink)) >= 0;
          // выходим, если удалили
          if FLinksChanged then
          begin
            // удаляем группу, если в ней не осталось линков
            if FGroups[aGroupIndex].Items.Count = 0 then
              FGroups.Delete(aGroupIndex);
            Break;
          end;
          Inc(aGroupIndex);
        end;

      end;
    finally
      aGroup.Free;
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TLAHistoryViewer.DisconnectLinks;
begin
  for var aGroup in Groups do
  begin
    if Assigned(aGroup.FStoredDataSource) then
      for var aLink in aGroup.Items do
        aLink.DataSource := aGroup.FStoredDataSource;
  end;
end;

procedure TLAHistoryViewer.FillHistory(const aDate1, aDate2: TDateTime);
begin
//  if not Assigned(Connector) then
//    raise Exception.Create('No connector');

  FDate1 := aDate1;
  FDate2 := aDate2;
  //ClearHistory;

  for var aGroup in FGroups do
    aGroup.FillValues(aDate1, aDate2, Connector);

  FActive := True;
  First;
end;


//procedure TLAHistoryViewer.FillHistory(const aDate1, aDate2: TDateTime);
//var
//  i: Integer;
//  aStream: TMemoryStream;
//  aRec: TLASensorValueSnapshot;
//begin
//  if not Assigned(Connector) then
//    raise Exception.Create('No connector');
//
//  FDate1 := aDate1;
//  FDate2 := aDate2;
//  ClearHistory;
//
//  aStream := TMemoryStream.Create;
//  try
//    for var aGroup in FGroups do
//    begin
//      aStream.Clear;
//      Connector.SensorHistoryStream(aStream, aGroup.ID, DateTimeToUnix(aDate1, False), DateTimeToUnix(aDate2, False),
//         True, True, False, True, True);
//
//      SetLength(aGroup.FValues, aStream.Size div SizeOf(aRec));
//
//      i := 0;
//      aStream.Position := 0;
//      while aStream.Position <= (aStream.Size - SizeOf(aRec)) do
//      begin
//        aStream.Read(aRec, SizeOf(aRec));
//
//        // исключаем заведомо неверные записи
//        if (aRec.FDateTime < aDate1) or (aRec.FDateTime > aDate2) then
//          Continue;
//        // контролируем, что время возрастает
//        if (i > 0) and (aGroup.FValues[i - 1].FDateTime >= aRec.FDateTime) then
//          Continue;
//
//        aGroup.FValues[i] := aRec;
//        Inc(i);
//      end;
//
//      if i < Length(aGroup.FValues) then
//        SetLength(aGroup.FValues, i);
//    end;
//  finally
//    aStream.Free;
//  end;
//  FActive := True;
//  First;
//end;

procedure TLAHistoryViewer.First;
begin
  CurrentMoment := Date1;
end;

function TLAHistoryViewer.GetNextMoment: TDateTime;
var
  m: TDateTime;
begin
  Result := 0;
  for var aGroup in FGroups do
  begin
    if aGroup.GetNextMoment(m) then
      if (Result = 0) or (Result > m) then
        Result := m;
  end;
end;

function TLAHistoryViewer.GetPredMoment: TDateTime;
var
  m: TDateTime;
begin
  Result := 0;
  for var aGroup in FGroups do
  begin
    if aGroup.GetPredMoment(m) then
      if (Result = 0) or (Result < m) then
        Result := m;
  end;
end;

procedure TLAHistoryViewer.Last;
begin
  CurrentMoment := Date2;
end;

procedure TLAHistoryViewer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      if Date1 = 0 then
        raise Exception.Create('Date1 is empty');
      if Date2 = 0 then
        raise Exception.Create('Date2 is empty');
      if Groups.Count = 0 then
        raise Exception.Create('No links connected');

      FillHistory(Date1, Date2);
    end
    else
      ClearHistory;
  end;
end;

procedure TLAHistoryViewer.SetConnector(const Value: TLACustomConnector);
begin
  FConnector := Value;
end;

procedure TLAHistoryViewer.SetCurrentMoment(const Value: TDateTime);
begin
  if FCurrentMoment <> Value then
  begin
    if Value > Date2 then
      FCurrentMoment := Date2
    else if Value < Date1 then
      FCurrentMoment := Date1
    else
      FCurrentMoment := Value;

    CalcValuesOnMoment(Value);
  end;
end;

procedure TLAHistoryViewer.SetDate1(const Value: TDateTime);
begin
  FDate1 := Value;
end;

procedure TLAHistoryViewer.SetDate2(const Value: TDateTime);
begin
  FDate2 := Value;
end;

function TLAHistoryViewer.Next: Boolean;
var
  aNewMoment: TDateTime;
begin
  aNewMoment := GetNextMoment;
  // следующий момент времени должен быть всегда больше текущего,
  // если это не так, значит данных больше нет - конец данных Eof
  if aNewMoment > CurrentMoment then
  begin
    CurrentMoment := aNewMoment;
    FEof := False;
  end
  else
    FEof := True;

  Result := not Eof;
end;

procedure TLAHistoryViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = Connector) then
      Connector := nil;
  end;
end;

function TLAHistoryViewer.Pred: Boolean;
var
  aNewMoment: TDateTime;
begin
  aNewMoment := GetPredMoment;
  // предыдущий момент времени должен быть всегда меньше текущего,
  // если это не так, значит мы достигли начала данных Bof
  if aNewMoment < CurrentMoment then
  begin
    CurrentMoment := aNewMoment;
    FBof := False;
  end
  else
    FBof := True;

  Result := not Bof;
end;

{ TLASensorLinkGroupHistory }

procedure TLASensorLinkGroupHistory.ClearValues;
begin
  SetLength(FValues, 0);
end;

constructor TLASensorLinkGroupHistory.Create;
begin
  FItems := TLASensorLinkList.Create;
end;

destructor TLASensorLinkGroupHistory.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLASensorLinkGroupHistory.FillValues(const aDate1, aDate2: TDateTime; aDefConnector: TLACustomConnector = nil);
var
  i: Integer;
  aStream: TMemoryStream;
  aConnector: TLACustomConnector;
  aRec: TLASensorValueSnapshot;
begin
  // если для
  aConnector := aDefConnector;
  if Assigned(FStoredDataSource) and (FStoredDataSource is TLASensorUpdater) then
    aConnector := TLASensorUpdater(FStoredDataSource).Connector;

  if not Assigned(aConnector) then
    raise Exception.Create('No connector');

  ClearValues;

  aStream := TMemoryStream.Create;
  try
    aConnector.SensorHistoryStream(aStream, ID, DateTimeToUnix(aDate1, False), DateTimeToUnix(aDate2, False),
       True, True, False, True, True);

    SetLength(FValues, aStream.Size div SizeOf(aRec));

    i := 0;
    aStream.Position := 0;
    while aStream.Position <= (aStream.Size - SizeOf(aRec)) do
    begin
      aStream.Read(aRec, SizeOf(aRec));

      // исключаем заведомо неверные записи
      if (aRec.FDateTime < aDate1) or (aRec.FDateTime > aDate2) then
        Continue;
      // контролируем, что время возрастает
      if (i > 0) and (FValues[i - 1].FDateTime >= aRec.FDateTime) then
        Continue;

      FValues[i] := aRec;
      Inc(i);
    end;

    if i < Length(FValues) then
      SetLength(FValues, i);
  finally
    aStream.Free;
  end;
end;

function TLASensorLinkGroupHistory.GetIndexOnDate(aDateTime: TDateTime): Integer;
var
  L, H, I: integer;
  Item1: TDateTime;
begin
  H := High(FValues);
  if H = -1 then
  begin
    Result := -1;
    Exit;
  end;
  I := -1;
  L := Low(FValues);
  while L <= H do
  begin
    I := (L + H) shr 1;
    Item1 := FValues[I].FDateTime;

    if Item1 < aDateTime then
      L := I + 1
    else
    begin
      H := I - 1;
      if Item1 = aDateTime then
        L := I;
    end;
  end;

  if Item1 <= aDateTime then
    Result := I
  else if H < L then
    Result := H
  else
    Result := L;

  if Result < 0 then
    Exit;

  while (Result < High(FValues)) and (FValues[Result].FDateTime = FValues[Result + 1].FDateTime) do
    Inc(Result);
end;

function TLASensorLinkGroupHistory.GetIsEmpty: Boolean;
begin
  Result := High(FValues) = -1;
end;

function TLASensorLinkGroupHistory.GetNextMoment(var aMoment: TDateTime): Boolean;
begin
  if FPosition >= High(FValues) then
    Result := False
  else
  begin
    aMoment := FValues[FPosition + 1].FDateTime;
    Result := True;
  end;

end;

function TLASensorLinkGroupHistory.GetPredMoment(var aMoment: TDateTime): Boolean;
begin
  if FPosition <= Low(FValues) then
    Result := False
  else
  begin
    aMoment := FValues[FPosition - 1].FDateTime;
    Result := True;
  end;
end;

function TLASensorLinkGroupHistory.GetSnapshotOnDate(aDateTime: TDateTime; var aSnapshot: TLASensorValueSnapshot): Boolean;
begin
  // данных нет
  if Length(FValues) = 0 then
    Exit(False);
  // на заданный момент данных нет
  FPosition := GetIndexOnDate(aDateTime);
  if FPosition < 0 then
    Exit(False);

  Result := True;
  // берем левую запись (с меньшим временем)
  aSnapshot := FValues[FPosition];
  // 1. есть точное соответствие
  if aSnapshot.FDateTime = aDateTime then
    Exit;
    
  // 2. расчет промежуточного значения не нужен
  aSnapshot.FDateTime := aDateTime;
  if (aSnapshot.FState <> 0) or (FPosition = High(FValues)) or (Kind = skDiscret)  then
    Exit;

  // 3. возможно нужен расчет промежуточного значения
  if (Kind = skAnalog)
    or ((Kind = skCounterUp) and (aSnapshot.FValue < FValues[FPosition + 1].FValue))
    or ((Kind = skCounterDown) and (aSnapshot.FValue > FValues[FPosition + 1].FValue)) then
  begin    
    //v = v2 - (v2 - v1) * (x2 - x) / (x2 - x1)
    aSnapshot.FValue := FValues[FPosition + 1].FValue -
      (FValues[FPosition + 1].FValue - FValues[FPosition].FValue) * (FValues[FPosition + 1].FDateTime - aDateTime) /
      (FValues[FPosition + 1].FDateTime - FValues[FPosition].FDateTime);
  end
end;

end.
