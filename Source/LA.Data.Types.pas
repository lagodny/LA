unit LA.Data.Types;

interface

uses
  System.Classes,
  System.Math;

type
{$SCOPEDENUMS ON}
  TValueCheckResult = (
    NoRange,  // нет проверок
    LowLow,   // ниже нижнего уровня
    Low,      // ниже уровня - предупреждение
    InTarget, // в цели
    NoTarget, // не в цели
    High,     // выше уровня - предупреждение
    HighHigh, // выше верхнего уровня
    Correct   // значение корректно
  );

const
  vcrWarnings = [TValueCheckResult.Low, TValueCheckResult.High];
  vcrAlarms = [TValueCheckResult.LowLow, TValueCheckResult.HighHigh];
  vcrCorrects = [TValueCheckResult.InTarget, TValueCheckResult.Correct, TValueCheckResult.NoRange];

type
  // снимок данных датчика: значение и состояние на момент времени
  TLASensorValueSnapshot = packed record
    FDateTime: TDateTime;
    FValue: Double;
    FState: Double;
  end;
  TLASensorValueSnapshots = array of TLASensorValueSnapshot;


  TValueRange = class(TPersistent)
  private
    FH: Double;
    FHH: Double;
    FL: Double;
    FLL: Double;
    FT: Double;
    function IsHHStored: Boolean;
    function IsHStored: Boolean;
    function IsLLStored: Boolean;
    function IsLStored: Boolean;
    function IsTStored: Boolean;
    procedure ReadH(Reader: TReader);
    procedure ReadHH(Reader: TReader);
    procedure ReadL(Reader: TReader);
    procedure ReadLL(Reader: TReader);
    procedure ReadT(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    function Check(aValue: Double): TValueCheckResult;
  published
    property LL: Double read FLL write FLL stored IsLStored nodefault;
    property L: Double read FL write FL stored IsLLStored nodefault;
    property H: Double read FH write FH stored IsHStored nodefault;
    property HH: Double read FHH write FHH stored IsHHStored nodefault;
    // целевое значение
    property T: Double read FT write FT stored IsTStored nodefault;
  end;


implementation

{ TValueRange }

procedure TValueRange.AssignTo(Dest: TPersistent);
begin
  if Dest is TValueRange then
  begin
    var aDest: TValueRange := TValueRange(Dest);
    aDest.LL := LL;
    aDest.L := L;
    aDest.H := H;
    aDest.HH := HH;
    aDest.T := T;
  end
  else
    inherited;
end;

function TValueRange.Check(aValue: Double): TValueCheckResult;
begin
  if SameValue(L, H) and SameValue(LL, HH) and SameValue(T, L) then
    Exit(TValueCheckResult.NoRange);

  if not SameValue(LL, HH) then
  begin
    if aValue < LL then
      Exit(TValueCheckResult.LowLow)
    else if aValue > HH then
      Exit(TValueCheckResult.HighHigh);
  end;

  if not SameValue(L, H) then
  begin
    if aValue < L then
      Exit(TValueCheckResult.Low)
    else if aValue > H then
      Exit(TValueCheckResult.High);
  end;

  if SameValue(LL, HH) and SameValue(L, H) then
  begin
    if SameValue(aValue, T) then
      Exit(TValueCheckResult.InTarget)
    else
      Exit(TValueCheckResult.NoTarget);
  end;

  Exit(TValueCheckResult.Correct);
end;

procedure TValueRange.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('H', ReadH, nil, False);
  Filer.DefineProperty('HH', ReadHH, nil, False);
  Filer.DefineProperty('L', ReadL, nil, False);
  Filer.DefineProperty('LL', ReadLL, nil, False);
  Filer.DefineProperty('T', ReadT, nil, False);
end;

function TValueRange.IsHHStored: Boolean;
begin
  Result := not SameValue(FHH, 0);
end;

function TValueRange.IsHStored: Boolean;
begin
  Result := not SameValue(FH, 0);
end;

function TValueRange.IsLLStored: Boolean;
begin
  Result := not SameValue(FLL, 0);
end;

function TValueRange.IsLStored: Boolean;
begin
  Result := not SameValue(FL, 0);
end;

function TValueRange.IsTStored: Boolean;
begin
  Result := not SameValue(FT, 0);
end;


procedure TValueRange.ReadH(Reader: TReader);
begin
  FH := Reader.ReadDouble;
end;

procedure TValueRange.ReadHH(Reader: TReader);
begin
  FHH := Reader.ReadDouble;
end;

procedure TValueRange.ReadL(Reader: TReader);
begin
  FL := Reader.ReadDouble;
end;

procedure TValueRange.ReadLL(Reader: TReader);
begin
  FLL := Reader.ReadDouble;
end;

procedure TValueRange.ReadT(Reader: TReader);
begin
  FT := Reader.ReadDouble;
end;

end.
