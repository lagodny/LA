unit LA.Data.Link.Sensor;

interface

uses
  System.Classes,
  System.Generics.Defaults, System.Generics.Collections,
  System.SysUtils, System.DateUtils,
  System.Math,
  LA.Data.Types,
  LA.Data.Lookup,
  LA.Data.Source,
  LA.Net.Connector,
  LA.Data.Link.Sensor.Intf;

const
  cStyleDefault = 'default';
  cStyleError = 'error';
  cStyleHH = 'hh';
  cStyleH = 'h';
  cStyleL = 'l';
  cStyleLL = 'll';
  cStyleInTarget = 'in_target';
  cStyleNotInTarget = 'not_in_target';

type
  /// <summary>
  ///   тип показаний датчика
  ///  - аналоговый (температура, давление) - убва
  ///  - возрастающий счетчик (счетчик электроэнергии)
  ///  - убывающий счетчик (секунд до окончания операции)
  ///  - дискретный (клапан)
  /// </summary>
  TLASensorKind = (skAnalog, skDiscret, skCounterUp, skCounterDown);

  /// <summary>
  ///   Линк/адаптер к Датчику
  /// </summary>
  TLASensorLink = class(TLADataLink, ILASensorLink)
  private
    {$REGION 'Fields'}
    FValue: Double;
    FStatus: string;
    FStatusCode: Integer;
    FTimestamp: TDateTime;
    FUn: string;
    FText: string;
    FName: string;
    FEquipment: string;
    FValueRange: TValueRange;
    FKind: TLASensorKind;
    FValueLookupList: TLALookupList;
    FLookupTableName: string;
    FDisplayFormat: string;
    FStatusLookupList: TLALookupList;
    // сохраненный LASensorUpdater для поиска подключения
    FSensorUpdater: TLADataSource;

    function GetValue: Double;
    function GetText: string;
    function GetStatus: string;
    function GetStatusCode: Integer;
    function GetTimestamp: TDateTime;
    function GetDisplayValue: string;
    function GetDisplayStatus: string;
    function GetStatusStyleName: string;
    procedure SetValue(const Value: Double);
    procedure SetText(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetStatusCode(const Value: Integer);
    procedure SetTimestamp(const Value: TDateTime);
    procedure SetUn(const Value: string);
    procedure SetName(const Value: string);
    procedure SetEquipment(const Value: string);
    procedure SetValueRange(const Value: TValueRange);
    procedure SetKind(const Value: TLASensorKind);
    procedure SetValueLookupList(const Value: TLALookupList);
    procedure SetStatusLookupList(const Value: TLALookupList);
    procedure SetLookupTableName(const Value: string);
    procedure SetDisplayFormat(const Value: string);
    function GetConnector: TLACustomConnector;
    {$ENDREGION}
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure SetDataSource(const Value: TLADataSource); override;
    procedure EncodeData; override;
  public
    constructor Create(const AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function ValueToText(aValue: Double): string;
    function StatusCodeToText(aStatusCode: Integer): string;

    procedure InitFromJSON(v: Variant);
    property Connector: TLACustomConnector read GetConnector;
  published
    /// *** периодически получаем с сервера ***
    // числовое значение, необходимо для проверок на допустимые значения
    property Value: Double read GetValue write SetValue;
    // текстовое представление для отображения справочной информации и специального форматирования (date, hex, bin..)
    property Text: string read GetText write SetText;
    // статус датчика, текстовое описание ошибок, если они есть, если ошибок нет, то пустая строка
    property Status: string read GetStatus write SetStatus;
    // код ошибки
    property StatusCode: Integer read GetStatusCode write SetStatusCode default 0;
    // момент получения данных с датчика на сервере
    property Timestamp: TDateTime read GetTimestamp write SetTimestamp;

    /// *** настраиваются для датчика на клиенте / можно запросить с сервера ***
    /// наименование оборудования
    property Equipment: string read FEquipment write SetEquipment;
    /// наименование датчика
    property Name: string read FName write SetName;
    /// единица измерения показаний датчика (литр, кг, км, км/ч ...)
    property Un: string read FUn write SetUn;
    /// вид датчика
    property Kind: TLASensorKind read FKind write SetKind default skAnalog;
    /// форматирование значений
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    /// наименование справочника на сервере
    property LookupTableName: string read FLookupTableName write SetLookupTableName;

    /// справочники показаний и статусов (ошибок)
    ///  можно не использовать, тогда за поиск описания значения или ошибки будет отвечать ConnectionManager
    ///  будет использовано наименование справочника из LookupTableName для значений и cStatesLookupTableName (States) для ошибок
    property ValueLookupList: TLALookupList read FValueLookupList write SetValueLookupList;
    property StatusLookupList: TLALookupList read FStatusLookupList write SetStatusLookupList;

    /// текстовое представление значений
    property DisplayValue: string read GetDisplayValue;
    /// текстовое представление ошибок
    property DisplayStatus: string read GetDisplayStatus;

    /// текстовое наименование стиля, которым должен быть представлен датчик
    ///  - default, error, hh, h, l, ll
    ///  эти наименования следует давать слоям стиля визуального компонента для их переключения
    property StatusStyleName: string read GetStatusStyleName;

    /// диапазон допустимых значений датчика
    property ValueRange: TValueRange read FValueRange write SetValueRange;
  end;
  TLASensorLinkList = class(TList<TLASensorLink>);

//  /// <summary>
//  ///   Группа объединяющая в себе линки к Датчику со схожими ID
//  /// </summary>
//  TLASensorLinkGroup = class
//  private
//    {$REGION 'Fields'}
//    FItems: TLASensorLinkList;
//    FID: string;
//    FKind: TLASensorKind;
//    FMoment: TDateTime;
//    {$ENDREGION}
//  public
//    constructor Create;
//    destructor Destroy; override;
//
//    property Items: TLASensorLinkList read FItems;
//
//    property ID: string read FID write FID;
//    property Kind: TLASensorKind read FKind write FKind;
////    property Moment: TDateTime read FMoment write FMoment;
//  end;

implementation

uses
  SynCrossPlatformJSON,
  LA.Common.Consts,
  LA.Data.Sensor.Updater,
  LA.Data.Connection.Manager,
  LA.Utils.Str;

{ TLASensorLink }

procedure TLASensorLink.AssignTo(Dest: TPersistent);
begin
  if Dest is TLASensorLink then
  begin
    inherited;
    var aDest: TLASensorLink := TLASensorLink(Dest);

    aDest.Name := Name;
    aDest.Equipment := Equipment;
    aDest.Un := Un;

    aDest.Value := Value;
    aDest.Text := Text;
    aDest.Status := Status;
    aDest.Timestamp := Timestamp;

    aDest.ValueRange := ValueRange;
  end
  else
    inherited;
end;

constructor TLASensorLink.Create(const AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FValueRange := TValueRange.Create;
end;

destructor TLASensorLink.Destroy;
begin
  FValueRange.Free;
  inherited;
end;

procedure TLASensorLink.EncodeData;
begin
  // сбросим признак необходимости декодировать данные
  inherited;

  if Data = '' then
    Exit;

  try
    // разбираем строку вида
    // 6;19708;19708.00;
    /// id;value;text;status;moment
    ///  id - обязательный: идентификатор датчика
    ///  value - обязательный: числовое значение: разделитель тысяч - '' (нет) : разделитель десятичных - '.' (точка)
    ///  text - необязательный: текстовое представление (может не передаваться - быть пустым, если совпадает с value)
    ///  status - необязательный: пустая строка, если ошибок нет, или описание ошибки
    ///  moment - необязательный: если пустая строка, то берем текущее время : иначе момент изменения датчика на сервере в формате ISO8601 UTC
    var a := Data.Split([';']);
    var L := Length(a);
    if L >= 2 then
    begin
      if a[0] = ID then
      begin
        if a[1] <> '' then
          FValue := TLAStrUtils.DotStrToFloatDef(a[1]);

        if L >= 3 then
          FText := a[2]
        else
          FText := a[1];

        if L >= 4 then
          FStatus := a[3]
        else
          FStatus := '';

        if L >= 5 then
          FTimestamp := TTimeZone.Local.ToLocalTime(ISO8601ToDate(a[4]))
        else
          FTimestamp := Now;
      end;
    end
    // при возниконовении ошибок получения данных, в поле Data записывается эта ошибка
    // используем ее, чтобы показать, что есть проблемы с полученем данных этого датчика
    else
    begin
      FStatus := Data;
      FText := Data;
    end;

    { TODO : добавить передачу кода ошибки }
    FStatusCode := 0;

  except
    on e: Exception do
    begin
      FStatus := e.Message;
      FText := FStatus;
    end;
  end;
end;

function TLASensorLink.GetConnector: TLACustomConnector;
begin
  if FSensorUpdater is TLASensorUpdater then
    Result := TLASensorUpdater(FSensorUpdater).Connector
  else
    Result := nil;
end;

function TLASensorLink.GetDisplayStatus: string;
begin
  Result := StatusCodeToText(StatusCode);
//  if Assigned(StatusLookupList) then
//    StatusLookupList.Lookup(StatusCode.ToString, Result)
//  else if (StatusCode <> 0) and Assigned(FSensorUpdater) and Assigned(TLAConnectionManager.DefInstance) then
//    Result := TLAConnectionManager.DefInstance.Lookup(StatusCode.ToString, cStatesLookupTableName, TLASensorUpdater(DataSource))
//  else
//    Result := '';
end;

function TLASensorLink.GetDisplayValue: string;
begin
  Result := ValueToText(Value);
end;

function TLASensorLink.GetStatus: string;
begin
  Result := FStatus;
end;

function TLASensorLink.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TLASensorLink.GetStatusStyleName: string;
begin
  if (StatusCode <> 0) or (Status <> '') then
    Result := cStyleError
  else
  begin
    case ValueRange.Check(Value) of
      TValueCheckResult.Correct, TValueCheckResult.NoRange:
        Result := cStyleDefault;
      TValueCheckResult.LowLow:
        Result := cStyleLL;
      TValueCheckResult.Low:
        Result := cStyleL;
      TValueCheckResult.High:
        Result := cStyleH;
      TValueCheckResult.HighHigh:
        Result := cStyleHH;
      TValueCheckResult.InTarget:
        Result := cStyleInTarget;
      TValueCheckResult.NoTarget:
        Result := cStyleNotInTarget;
    end;
  end;

end;

function TLASensorLink.GetText: string;
begin
  Result := FText;
end;

function TLASensorLink.GetTimestamp: TDateTime;
begin
  Result := FTimestamp;
end;

function TLASensorLink.GetValue: Double;
begin
  Result := FValue;
end;

procedure TLASensorLink.InitFromJSON(v: Variant);
begin
{$REGION 'Sample'}
  {
      "id": 1,
      "name": "Время на сервере",
      "kind": 0,
      "un": "",
      "format": "datedd.mm.yyyy hh:mm:ss",
      "lookup": ""
    }
{$ENDREGION}
//  ID := v.id;
  Name := v.name;
  Kind := TLASensorKind(v.kind);
  Un := v.un;
  DisplayFormat := v.format;
  LookupTableName := v.lookup;
end;

procedure TLASensorLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = ValueLookupList) then
      ValueLookupList := nil
    else if (AComponent = StatusLookupList) then
      StatusLookupList := nil
    else if AComponent = FSensorUpdater then
      FSensorUpdater := nil;
  end;
end;

procedure TLASensorLink.SetDataSource(const Value: TLADataSource);
begin
  inherited;
  if Value is TLASensorUpdater then
    FSensorUpdater := Value;
end;

procedure TLASensorLink.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetEquipment(const Value: string);
begin
  if FEquipment <> Value then
  begin
    FEquipment := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetKind(const Value: TLASensorKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetLookupTableName(const Value: string);
begin
  if FLookupTableName <> Value then
  begin
    FLookupTableName := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetStatus(const Value: string);
begin
  if FStatus <> Value then
  begin
    FStatus := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetStatusCode(const Value: Integer);
begin
  if FStatusCode <> Value then
  begin
    FStatusCode := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetStatusLookupList(const Value: TLALookupList);
begin
  if FStatusLookupList <> Value then
  begin
    FStatusLookupList := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetTimestamp(const Value: TDateTime);
begin
  if FTimestamp <> Value then
  begin
    FTimestamp := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetUn(const Value: string);
begin
  if FUn <> Value then
  begin
    FUn := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetValue(const Value: Double);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    DoNeedNotify;
  end;
end;


procedure TLASensorLink.SetValueLookupList(const Value: TLALookupList);
begin
  if FValueLookupList <> Value then
  begin
    FValueLookupList := Value;
    DoNeedNotify;
  end;
end;

procedure TLASensorLink.SetValueRange(const Value: TValueRange);
begin
  FValueRange.Assign(Value);
end;

function TLASensorLink.StatusCodeToText(aStatusCode: Integer): string;
begin
  if Assigned(StatusLookupList) then
    StatusLookupList.Lookup(aStatusCode.ToString, Result)
  else if (aStatusCode <> 0) and Assigned(FSensorUpdater) and Assigned(TLAConnectionManager.DefInstance) then
    Result := TLAConnectionManager.DefInstance.Lookup(aStatusCode.ToString, cStatesLookupTableName, TLASensorUpdater(DataSource))
  else
    Result := '';
end;

function TLASensorLink.ValueToText(aValue: Double): string;
begin
  if Assigned(ValueLookupList) then
    ValueLookupList.Lookup(aValue.ToString, Result)
  else if (LookupTableName <> '') and Assigned(FSensorUpdater) and Assigned(TLAConnectionManager.DefInstance) then
    Result := TLAConnectionManager.DefInstance.Lookup(aValue.ToString, LookupTableName, TLASensorUpdater(FSensorUpdater))
  else
    Result := TLAStrUtils.FormatValue(aValue, DisplayFormat);
end;


end.
