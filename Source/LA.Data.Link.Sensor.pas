unit LA.Data.Link.Sensor;

interface

uses
  System.Classes,
  System.SysUtils, System.DateUtils,
  System.Math,
  LA.Data.Types,
  LA.Data.Source;

type
  /// <summary>
  ///   Линк/адаптер к Датчику
  /// </summary>
  TLASensorLink = class(TLADataLink)
  private
    {$REGION 'Fields'}
    FValue: Double;
    FStatus: string;
    FTimestamp: TDateTime;
    FUn: string;
    FText: string;
    FName: string;
    FEquipment: string;
    FValueRange: TValueRange;
    procedure SetValue(const Value: Double);
    procedure SetText(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTimestamp(const Value: TDateTime);
    procedure SetUn(const Value: string);
    procedure SetName(const Value: string);
    procedure SetEquipment(const Value: string);
    procedure SetValueRange(const Value: TValueRange);
    {$ENDREGION}
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure EncodeData; override;
  public
    constructor Create(const AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    /// *** периодически получаем с сервера ***
    // числовое значение, необходимо для проверок на допустимые значения
    property Value: Double read FValue write SetValue;
    // текстовое представление для отображения справочной информации и специального форматирования (date, hex, bin..)
    property Text: string read FText write SetText;
    // статус датчика, текстовое описание ошибок, если они есть, если ошибок нет, то пустая строка
    property Status: string read FStatus write SetStatus;
    // момент получения данных с датчика на сервере
    property Timestamp: TDateTime read FTimestamp write SetTimestamp;

    /// *** настраиваются для датчика на клиенте ***
    /// наименование оборудования
    property Equipment: string read FEquipment write SetEquipment;
    /// наименование датчика
    property Name: string read FName write SetName;
    /// единица измерения показаний датчика (литр, кг, км, км/ч ...)
    property Un: string read FUn write SetUn;

    /// диапазон допустимых значений датчика
    property ValueRange: TValueRange read FValueRange write SetValueRange;
  end;


implementation

uses
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
  try
    var a := Data.Split([';']);
    var L := Length(a);
    if L >= 2 then
    begin
      if a[0] = ID then
      begin
        if a[1] <> '' then
          FValue := TLAStrUtils.DotStrToFloatDef(a[1]);

        if L >= 3 then
        begin
          FText := a[2];
          if L >= 4 then
          begin
            FStatus := a[3];
            if L >= 5 then
              FTimestamp := ISO8601ToDate(a[4])
            else
              FTimestamp := Now;
          end
        end
      end;
    end;
  except
    on e: Exception do
    begin
      FStatus := e.Message;
      FText := FStatus;
    end;
  end;
end;

procedure TLASensorLink.SetEquipment(const Value: string);
begin
  FEquipment := Value;
end;

procedure TLASensorLink.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TLASensorLink.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TLASensorLink.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TLASensorLink.SetTimestamp(const Value: TDateTime);
begin
  FTimestamp := Value;
end;

procedure TLASensorLink.SetUn(const Value: string);
begin
  FUn := Value;
end;

procedure TLASensorLink.SetValue(const Value: Double);
begin
  FValue := Value;
end;


procedure TLASensorLink.SetValueRange(const Value: TValueRange);
begin
  FValueRange.Assign(Value);
end;

end.
