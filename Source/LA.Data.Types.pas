unit LA.Data.Types;

interface

uses
  System.Classes,
  System.Math;

type
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
  vcrWarnings = [Low, High];
  vcrAlarms = [LowLow, HighHigh];
  vcrCorrects = [InTarget, Correct, NoRange];

type
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
  if (L = H) and (LL = HH) and (T = L) then
    Exit(NoRange);

  if LL <> HH then
  begin
    if aValue < LL then
      Exit(LowLow)
    else if aValue > HH then
      Exit(HighHigh);
  end;

  if L <> H then
  begin
    if aValue < L then
      Exit(Low)
    else if aValue > H then
      Exit(High);
  end;

  if (LL = HH) and (L = H) then
  begin
    if SameValue(aValue, T) then
      Exit(InTarget)
    else
      Exit(NoTarget);
  end;

  Exit(Correct);
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
