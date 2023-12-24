unit LA.Keys;

interface

uses
  System.Classes, System.UITypes;

type
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

implementation

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



end.
