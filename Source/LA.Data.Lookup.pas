﻿unit LA.Data.Lookup;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults, System.Generics.Collections,
  LA.Net.Connector;

type
  /// способ поиска значений
  TLAValueSearchMethod = (
    vsmExact, // только точное соответствие
    vsmLeft,  // левое, если точное не найдено
    vsmRight  // правое, если точное не найдено
  );

  /// объект для поиска текстовой справочной информации по числовому значению
  ///  - хранит соответствия кода и его представления, может возвращать неточное соответствие
  ///  - умеет автоматически подгружать справочную информацию, если не находит соответствие (только в режиме vsmExact)
  TLALookupList = class(TComponent)
  private
    FItems: TStrings;
    FValueSearchMethod: TLAValueSearchMethod;
    FAutoUpdate: Boolean;
    FConnector: TLACustomConnector;
    FTableName: string;
    procedure SetItems(const Value: TStrings);
    procedure SetValueSearchMethod(const Value: TLAValueSearchMethod);
    procedure SetAutoUpdate(const Value: Boolean);
    procedure SetConnector(const Value: TLACustomConnector);
    procedure SetTableName(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function InitFromServer: Boolean;
    function Lookup(const aKey: string; var aValue: string; aConnector: TLACustomConnector = nil): Integer;

    function GetValueByKey(aKey: Double; const aDefault: string = ''): string;
  published
    /// пары: ключ = текстовое представление
    ///  0=
    ///  1=работа
    ///  2=мойка
    property Items: TStrings read FItems write SetItems;
    property Connector: TLACustomConnector read FConnector write SetConnector;

    property TableName: string read FTableName write SetTableName;
    property ValueSearchMethod: TLAValueSearchMethod read FValueSearchMethod write SetValueSearchMethod default vsmExact;
    property AutoUpdate: Boolean read FAutoUpdate write SetAutoUpdate default True;
  end;

  /// <summary>Позволяет создавать LALookupList по запросу</summary>
  TLALookupListManager = class
  private
    class var
      FItems: TObjectList<TLALookupList>;
  public
    class constructor Create;
    class destructor Destroy;

    /// возвращает существующий или создает новый, если не найден
    class function GetLookupList(const aName: string; aConnector: TLACustomConnector): TLALookupList;
  end;

implementation

{ TLALookup }

constructor TLALookupList.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  FValueSearchMethod := vsmExact;
  FAutoUpdate := True;
end;

destructor TLALookupList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TLALookupList.GetValueByKey(aKey: Double; const aDefault: string): string;
begin
  if Lookup(aKey.ToString, Result) < 0 then
    Result := aDefault;
end;

function TLALookupList.InitFromServer: Boolean;
begin
  if not Assigned(Connector) or (TableName = '') then
    Exit(False);

  Items.Text := Connector.GetLookup(TableName);
  Result := True;
end;

function TLALookupList.Lookup(const aKey: string; var aValue: string; aConnector: TLACustomConnector = nil): Integer;
var
  aIndexNum, aNameNum: Double;
  i: Integer;
begin
  aValue := aKey;
  if Trim(aKey) = '' then
    Exit(-1);

  if ValueSearchMethod = vsmExact then
  begin
    Result := Items.IndexOfName(aKey);
    if Result < 0 then
    begin
      if AutoUpdate and (TableName <> '') then
      begin
        if Assigned(aConnector) then
           Items.Text := aConnector.GetLookup(TableName)
        else if Assigned(FConnector) then
           Items.Text := FConnector.GetLookup(TableName)
        else
          Exit;

        Result := Items.IndexOfName(aKey);
        if Result < 0 then
        begin
          Items.Add(aKey+'='+aKey);
          aValue := aKey;
          Exit;
        end;
      end
      else
        Exit;
    end;
    aValue := Items.ValueFromIndex[Result];
  end

  else
  begin
    Result := -1;
    aIndexNum := StrToFloat(aKey);
    for i := 0 to Items.Count - 1 do
    begin
      aNameNum := StrToFloat(Items.Names[i]);
      // точное совпадение
      if aNameNum = aIndexNum then
      begin
        Result := i;
        aValue := Items.ValueFromIndex[Result];
        Exit;
      end
      // искомое значение меньше чем значение в текущей позииции
      else if aIndexNum < aNameNum then
      begin
        // нам нужно правое? - это оно и есть
        if ValueSearchMethod = vsmRight then
        begin
          Result := i;
          aValue := Items.ValueFromIndex[Result];
          Exit;
        end

        // нам нужно левое? - берем предыдущее
        else
        begin
          if i > 0 then
          begin
            Result := i - 1;
            aValue := Items.ValueFromIndex[Result];
            Exit;
          end
          else
          begin
            Result := 0;
            Items.Insert(Result, aKey + '=' + aKey);
            aValue := aKey;
            Exit;
          end;
        end;
      end;
    end;

    // не нашли - добавим в конец и вернем
    if Result = -1 then
    begin
      Result := Items.Count;
      Items.Add(aKey + '=' + aKey);
      aValue := aKey;
      Exit;
    end;
  end;
end;

procedure TLALookupList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = Connector) then
      Connector := nil;
  end;
end;

procedure TLALookupList.SetAutoUpdate(const Value: Boolean);
begin
  FAutoUpdate := Value;
end;

procedure TLALookupList.SetConnector(const Value: TLACustomConnector);
begin
  FConnector := Value;
end;

procedure TLALookupList.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TLALookupList.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

procedure TLALookupList.SetValueSearchMethod(const Value: TLAValueSearchMethod);
begin
  FValueSearchMethod := Value;
end;

{ TLALookupListManager }

class constructor TLALookupListManager.Create;
begin
  FItems := TObjectList<TLALookupList>.Create(TDelegatedComparer<TLALookupList>.Create(
    function (const aLeft, aRight: TLALookupList): Integer
    begin
      Result := CompareStr(aLeft.TableName, aRight.TableName);
      if Result = 0 then
        Result := Integer(aLeft.Connector) - Integer(aRight.Connector);
    end)
    // мы отвечаем за уничтожение
  , True);
end;

class destructor TLALookupListManager.Destroy;
begin
  FItems.Free;
end;

class function TLALookupListManager.GetLookupList(const aName: string; aConnector: TLACustomConnector): TLALookupList;
var
  aSearchItem: TLALookupList;
  aInsertIndex: Integer;
begin
  if (aName.Trim = '') or (aConnector = nil) then
    Exit(nil);

  aSearchItem := TLALookupList.Create(nil);
  aSearchItem.TableName := aName;
  aSearchItem.Connector := aConnector;
  if FItems.BinarySearch(aSearchItem, aInsertIndex) then
  begin
    aSearchItem.Free;
    Result := FItems[aInsertIndex];
  end
  else
  begin
    FItems.Insert(aInsertIndex, aSearchItem);
    Result := aSearchItem;
  end;
end;

end.
