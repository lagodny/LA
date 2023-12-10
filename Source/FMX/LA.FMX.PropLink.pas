unit LA.FMX.PropLink;

interface

uses
  System.Classes, System.Rtti, System.SysUtils, System.UITypes,
  System.Generics.Collections,
  FMX.Types, FMX.Utils,
  FMX.Effects,
  //FMX.Ani,
  LA.Data.Link.Sensor, LA.Data.Link.Sensor.Intf;

type
  /// обновляет свойство компонента значением датчика
  /// делаем по аналогии с TAnimation
  /// наследование от TEffect позволяет
  /// - корректно привязывать компонент из ПАЛИТРЫ к нужному компоненту в ДИЗАЙНЕРЕ
  /// - а также копировать / вставлять компонент в СТРУКТУРЕ
  TLAPropLink = class(TEffect, ILASensorLink)
//  TLAPropLink = class(TFmxObject, ILASensorLink)
//  TLAPropLink = class(TAnimation, ILASensorLink)
  private
    FLink: TLASensorLink;
    procedure SetLink(const Value: TLASensorLink);
    procedure SetPropertyName(const Value: string);
  protected
    FInstance: TObject;
    FRttiProperty: TRttiProperty;
    FPath, FPropertyName: string;
    FNeedFindProperty: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ProcessUpdate; virtual;

    function FindProperty: Boolean;
    procedure ParentChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoDataLinkChanged(Sender: TObject);
  published
    property Link: TLASensorLink read FLink  write SetLink implements ILASensorLink;
    property PropertyName: string read FPropertyName write SetPropertyName;
  end;

  // ключ для поиска
  TLAKey = class(TCollectionItem)
  private
    FKey: Single;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Key: Single read FKey write FKey;
  end;
  TLAKeyClass = class of TLAKey;

  // коллекция ключей
  TLAKeys = class(TCollection)
  public
    function FindKeys(const Time: Single; var Key1, Key2: TLAKey): Boolean;
  end;

  // по ключу определяем цвет
  TLAColorKey = class(TLAKey)
  private
    FValue: TAlphaColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Value: TAlphaColor read FValue write FValue;
  end;

  // по ключу определяем другое число
  TLAFloatKey = class(TLAKey)
  private
    FValue: Single;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Value: Single read FValue write FValue;
  end;

  /// общий предок для вычисления результирующих значений по таблице ключей
  ///  наследники должны вернуть коректный класс в функции GetKeyClass
  TLACustomKeyPropLink = class(TLAPropLink)
  private
    FKeys: TLAKeys;
    procedure SetKeys(const Value: TLAKeys);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetKeyClass: TLAKeyClass; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Keys: TLAKeys read FKeys write SetKeys;
  end;

  /// для значений проставляются цвета
  TLAColorKeyPropLink = class(TLACustomKeyPropLink)
  protected
    function GetKeyClass: TLAKeyClass; override;
    procedure ProcessUpdate; override;
  end;

  /// для значений проставляются значения
  ///  если Свойство компонента имеет тип Integer, то значение округляется
  TLAFloatKeyPropLink = class(TLACustomKeyPropLink)
  protected
    function GetKeyClass: TLAKeyClass; override;
    procedure ProcessUpdate; override;
  end;



implementation

{ TLAPropLink }

procedure TLAPropLink.AssignTo(Dest: TPersistent);
var
  aDest: TLAPropLink;
begin
  if Dest Is TLAPropLink then
  begin
    aDest := TLAPropLink(Dest);
    aDest.Link := Link;
    aDest.PropertyName := PropertyName;
  end
  else
    inherited;
end;

constructor TLAPropLink.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.OnOwnerNotify := DoDataLinkChanged;
end;

destructor TLAPropLink.Destroy;
begin
  FLink.Free;
  inherited;
end;

procedure TLAPropLink.DoDataLinkChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  ProcessUpdate;
end;

function TLAPropLink.FindProperty: Boolean;
var
  Persistent: string;
  Comp: TFmxObject;
  I: Integer;
  T: TRttiType;
  P: TRttiProperty;
  Properties: TList<TRttiProperty>;
begin
  Result := False;
  FNeedFindProperty := False;

  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while FPath.Contains('.') do
      begin
        Persistent := GetToken(FPath, '.');
        T := SharedContext.GetType(FInstance.ClassInfo);
        if T <> nil then
        begin
          P := T.GetProperty(Persistent);
          if (P <> nil) and (P.PropertyType.IsInstance) then
            FInstance := P.GetValue(FInstance).AsObject
          else
          if Parent <> nil then
          begin
            for I := 0 to Parent.ChildrenCount - 1 do
              if CompareText(Parent.Children[I].Name, Persistent) = 0 then
              begin
                Comp := Parent.Children[I];
                T := SharedContext.GetType(Comp.ClassInfo);
                if T <> nil then
                begin
                  P := T.GetProperty(FPath);
                  if P <> nil then
                  begin
                    FInstance := Comp;
                    Break;
                  end;
                end;
              end;
          end;
        end;
      end;
      if FInstance <> nil then
      begin

        if not ClonePropertiesCache.TryGetValue(FInstance.ClassName, Properties) then
        begin
          Properties := TList<TRttiProperty>.Create;
          ClonePropertiesCache.Add(FInstance.ClassName, Properties);
        end;

        for P in Properties do
          if P.Name = FPath then
          begin
            FRttiProperty := P;
            Break;
          end;

        if FRttiProperty = nil then
        begin
          T := SharedContext.GetType(FInstance.ClassInfo);
          FRttiProperty := T.GetProperty(FPath);
          if FRttiProperty <> nil then
            Properties.Add(FRttiProperty);
        end;
        Result := FRttiProperty <> nil;
      end;
    end
    else
      Result := True;
  end;
end;

procedure TLAPropLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if (AComponent = FInstance) then
    begin
      FNeedFindProperty := True;
      FInstance := nil;
    end;
  end;

  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLAPropLink.ParentChanged;
begin
  inherited;
  FInstance := nil;
  FNeedFindProperty := True;
end;

procedure TLAPropLink.ProcessUpdate;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FNeedFindProperty then
    FindProperty;

  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) then
      begin
        if (P.PropertyType.TypeKind = tkFloat) then
          P.SetValue(FInstance, Link.Value)
        else if (P.PropertyType.TypeKind in [tkString, tkUString, tkLString, tkWString, tkVariant]) then
          P.SetValue(FInstance, Link.Text)
        else if (P.PropertyType.TypeKind = tkInteger) then
          P.SetValue(FInstance, Round(Link.Value))
      end;
    end;
  end;
end;

procedure TLAPropLink.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

procedure TLAPropLink.SetPropertyName(const Value: string);
begin
  if not SameText(Value, PropertyName) then
  begin
    FInstance := nil;
    FNeedFindProperty := True;
    FPropertyName := Value;
  end;
end;

{ TLAKey }

procedure TLAKey.AssignTo(Dest: TPersistent);
begin
  if Dest is TLAKey then
  begin
    var aDest := TLAKey(Dest);
    aDest.Key := Key;
  end;
end;

{ TLAColorKey }

procedure TLAColorKey.AssignTo(Dest: TPersistent);
begin
  if Dest is TLAColorKey then
  begin
    var aDest := TLAColorKey(Dest);
    aDest.Key := Key;
    aDest.Value := Value;
  end
  else
    inherited;
end;

{ TLAFloatKey }

procedure TLAFloatKey.AssignTo(Dest: TPersistent);
begin
  if Dest is TLAFloatKey then
  begin
    var aDest := TLAFloatKey(Dest);
    aDest.Key := Key;
    aDest.Value := Value;
  end
  else
    inherited;
end;

{ TLAColorKeyPropLink }

function TLAColorKeyPropLink.GetKeyClass: TLAKeyClass;
begin
  Result := TLAColorKey;
end;

procedure TLAColorKeyPropLink.ProcessUpdate;
var
  T: TRttiType;
  P: TRttiProperty;
  Key1, Key2: TLAKey;
begin
  if FNeedFindProperty then
    FindProperty;

  if FInstance <> nil then
  begin
    if FKeys.FindKeys(Link.Value, Key1, Key2) then
    begin
      T := SharedContext.GetType(FInstance.ClassInfo);
      if T <> nil then
      begin
        P := T.GetProperty(FPath);
        if (P <> nil) and P.PropertyType.IsOrdinal then
        begin
          if (Key2.Key - Key1.Key) = 0 then
            P.SetValue(FInstance, TLAColorKey(Key1).Value)
          else
            P.SetValue(FInstance,
              InterpolateColor(TLAColorKey(Key1).Value, TLAColorKey(Key2).Value,
                (Link.Value - Key1.Key) / (Key2.Key - Key1.Key)));
        end;
      end;
    end;
  end;
end;


{ TLAKeys }

function TLAKeys.FindKeys(const Time: Single; var Key1, Key2: TLAKey): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count < 2 then
    Exit;
  for I := 0 to Count - 2 do
    if (Time >= TLAKey(Items[I]).Key) and (Time <= TLAKey(Items[I + 1]).Key) then
    begin
      Result := True;
      Key1 := TLAKey(Items[I]);
      Key2 := TLAKey(Items[I + 1]);
      Exit;
    end;
end;

{ TLACustomKeyPropLink }

procedure TLACustomKeyPropLink.AssignTo(Dest: TPersistent);
begin
  if Dest is TLACustomKeyPropLink then
  begin
    var aDest := TLACustomKeyPropLink(Dest);
    aDest.Keys := Keys;
  end;
  inherited;
end;

constructor TLACustomKeyPropLink.Create(AOwner: TComponent);
begin
  inherited;
  FKeys := TLAKeys.Create(GetKeyClass);
end;

destructor TLACustomKeyPropLink.Destroy;
begin
  FKeys.Free;
  inherited;
end;

procedure TLACustomKeyPropLink.SetKeys(const Value: TLAKeys);
begin
  FKeys.Assign(Value);
end;

{ TLAFloatKeyPropLink }

function TLAFloatKeyPropLink.GetKeyClass: TLAKeyClass;
begin
  Result := TLAFloatKey;
end;

procedure TLAFloatKeyPropLink.ProcessUpdate;
var
  T: TRttiType;
  P: TRttiProperty;
  Key1, Key2: TLAKey;
  r: Double;
begin
  if FNeedFindProperty then
    FindProperty;

  if FInstance <> nil then
  begin
    if FKeys.FindKeys(Link.Value, Key1, Key2) then
    begin
      T := SharedContext.GetType(FInstance.ClassInfo);
      if T <> nil then
      begin
        P := T.GetProperty(FPath);
        if (P <> nil) then
        begin
          if (Key2.Key - Key1.Key) = 0 then
            r := TLAFloatKey(Key1).Value
          else
            r := InterpolateSingle(TLAFloatKey(Key1).Value, TLAFloatKey(Key2).Value, (Link.Value - Key1.Key) / (Key2.Key - Key1.Key));

          if (P.PropertyType.TypeKind = tkFloat) then
            P.SetValue(FInstance, r)
          else if (P.PropertyType.TypeKind = tkInteger) then
            P.SetValue(FInstance, Round(r))
        end;
      end;
    end;
  end;
end;

initialization
  RegisterFmxClasses([TLAPropLink, TLAColorKeyPropLink, TLAFloatKeyPropLink]);


end.
