unit LA.Interval;

interface

uses
  System.Classes, System.SysUtils,
  System.DateUtils, System.Math;

type
  ELAIntervalException = class(Exception)

  end;

  TLAIntervalKind = (
    // абсолютные значения начала и окончания
    ikAbsolute,
    // относительно текущего значения времени
    ikToday,
    ikYesterday,
    ikThisWeek,
    ikPreviousWeek,
    ikThisMonth,
    ikPreviousMonth,
    ikThisYear,
    // требует дополнительный параметр (Shift)
    ikLastNHours,
    ikLastNDays
  );

  TLAInterval = class(TPersistent)
  private
    class var FLastInterval: TLAInterval;
  private
    FLockCount: integer;
    FWasChanged: Boolean;

    FKind: TLAIntervalKind;
    FShift: Double;

    FDate1: TDatetime;
    FDate2: TDatetime;

    FOnChanged: TNotifyEvent;

    class function GetLastInterval: TLAInterval; static;
    class procedure SetLastInterval(const Value: TLAInterval); static;

    function GetDate1: TDatetime;
    function GetDate2: TDatetime;
    procedure SetDate1(const Value: TDatetime);
    procedure SetDate2(const Value: TDatetime);
    procedure SetKind(const Value: TLAIntervalKind);
    procedure SetShift(const Value: Double);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChanged;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create;

//    class function ShiftKindToStr(aShiftKind: TShiftKind): string;
//    class function ShiftUnitToStr(aShiftUnit: TOPCIntervalTimeShiftUnit): string;

    class function IntervalKindToStr(aKind: TLAIntervalKind): string;
    class property LastInterval: TLAInterval read GetLastInterval write SetLastInterval;

    /// устанавливает Абсолютный период
    ///  если даты равны, то длительность периода равна 1 день
    procedure SetInterval(aDate1, aDate2: TDateTime);

//    procedure Save(aReg: TCustomIniFile; aSectionName: string);
//    procedure Load(aReg: TCustomIniFile; aSectionName: string);

    function AsText: string;
    function AsTextAbsolute: string;

    procedure Lock;
    procedure Unlock;

  published
    property Kind: TLAIntervalKind read FKind write SetKind;
    property Shift: Double read FShift write SetShift;

    property Date1: TDatetime read GetDate1 write SetDate1;
    property Date2: TDatetime read GetDate2 write SetDate2;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

resourcestring
  rsYouHaveToSetAbsoluteIntervalKindFirst = 'You have to set Absolute interval Kind first';
  rsDate1GreaterThanDate2 = 'Date1 is greater than Date2';

  rsAbsolute = 'Absolute';
  rsToday = 'Today';
  rsYesterday = 'Yesterday';
  rsThisWeek = 'This week';
  rsPreviousWeek = 'Previous week';
  rsThisMonth = 'This month';
  rsPreviousMonth = 'Previous month';
  rsThisYear = 'This year';
  rsLastNHoursFmt = 'Last %g hours';
  rsLastNDaysFmt = 'Last %g days';


implementation

uses
  DKLang;

//const
//  cKindArr: array of string = [
//    rsAbsolute,
//    rsToday,
//    rsYesterday,
//    rsThisWeek,
//    rsPreviousWeek,
//    rsThisMonth,
//    rsPreviousMonth,
//    rsThisYear,
//    rsLastNHoursFmt,
//    rsLastNDaysFmt
//  ];
//
//
//  cKindArr: array of string = [
//    rsAbsolute,
//    rsLastNHoursFmt,
//    rsLastNDaysFmt,
//    rsToday,
//    rsYesterday,
//    rsThisWeek,
//    rsPreviousWeek,
//    rsThisMonth,
//    rsPreviousMonth,
//    rsThisYear
//  ];

{ TLAInterval }

procedure TLAInterval.AssignTo(Dest: TPersistent);
var
  aDest: TLAInterval;
begin
  if Dest = Self then
    Exit;

  if Dest is TLAInterval then
  begin
    aDest := TLAInterval(Dest);

    aDest.Lock;
    try
      aDest.FKind := FKind;
      aDest.FShift := FShift;
      aDest.FDate1 := FDate1;
      aDest.FDate2 := FDate2;
//      aDest.SetInterval(Date1, Date2);
//      if Kind in [ikAbsolute] then
//        aDest.SetInterval(Date1, Date2);
    finally
      aDest.Unlock;
    end;
  end
  else
    inherited;
end;

function TLAInterval.AsText: string;
begin
  case Kind of
    ikAbsolute:
      Result := AsTextAbsolute;
    ikLastNHours:
      Result := Format(rsLastNHoursFmt, [Shift]);
    ikLastNDays:
      Result := Format(rsLastNDaysFmt, [Shift]);
    ikToday,
    ikYesterday,
    ikThisWeek,
    ikPreviousWeek,
    ikThisMonth,
    ikPreviousMonth,
    ikThisYear:
//      Result := cKindArr[Ord(FKind)];
      Result := TLAInterval.IntervalKindToStr(FKind);
  end;
end;

function TLAInterval.AsTextAbsolute: string;
begin
  if (Frac(Date1) = 0) and (Frac(Date2) = 0) then
  begin
    if Date1 + 1 = Date2 then
      Result := FormatDateTime('dd.mm.yyyy', Date1)
    else
      Result :=
        FormatDateTime('dd.mm.yyyy', Date1) + ' - ' +
        FormatDateTime('dd.mm.yyyy', Date2 - 1)
  end
  else
  begin
    if (Frac(Date1) = 0) then
      Result := FormatDateTime('dd.mm.yyyy', Date1)
    else
      Result := FormatDateTime('dd.mm.yyyy hh:mm.ss', Date1);

    if (Frac(Date2) = 0) then
      Result := Result + ' - ' + FormatDateTime('dd.mm.yyyy', Date2 - 1)
    else
      Result := Result + ' - ' + FormatDateTime('dd.mm.yyyy hh:mm.ss', Date2);
  end;
end;

class constructor TLAInterval.Create;
begin
  FLastInterval := TLAInterval.Create;
end;

constructor TLAInterval.Create;
begin
  FKind := ikLastNHours;
  FShift := 12;
end;

class destructor TLAInterval.Destroy;
begin
  FreeAndNil(FLastInterval);
end;

procedure TLAInterval.DoChanged;
begin
  FWasChanged := True;
  if (FLockCount = 0) and Assigned(FOnChanged) then
  begin
    FWasChanged := false;
    FOnChanged(Self);
  end;
end;

function TLAInterval.GetDate1: TDatetime;
var
  y,m,d: Word;
begin
  case Kind of
    ikAbsolute:
      ; //Result := FDate1;

    ikLastNHours:
      FDate1 := Now - Shift / HoursPerDay;
    ikLastNDays:
      FDate1 := Now - Shift;
    ikToday:
      FDate1 := Trunc(Now);
    ikYesterday:
      FDate1 := Trunc(Now) - 1;
    ikThisWeek:
      FDate1 := Trunc(Now - DayOfTheWeek(Now)) + 1;
    ikPreviousWeek:
      FDate1 := (Trunc(Now) - DayOfTheWeek(Now)) - 7 + 1;
    ikThisMonth:
      FDate1 := Trunc(StartOfTheMonth(Now));
    ikPreviousMonth:
      FDate1 := Trunc(StartOfTheMonth(Trunc(StartOfTheMonth(Now) - 1)));
    ikThisYear:
      FDate1 := Trunc(StartOfTheYear(Now));
//      begin
//        DecodeDate(Now, y, m, d);
//        Result := EncodeDate(y, m, 1);
//      end;
  end;
  Result := FDate1;

end;

function TLAInterval.GetDate2: TDatetime;
begin
  case Kind of
    ikAbsolute:
      ; //Result := FDate2;
    ikYesterday:
      FDate2 := Trunc(Now);
    ikPreviousWeek:
      FDate2 := Trunc(Now) - DayOfTheWeek(Now) + 1;
    ikPreviousMonth:
      FDate2 := Trunc(StartOfTheMonth(Now));
    else
      FDate2 := Now;
  end;
  Result := FDate2;
end;

class function TLAInterval.GetLastInterval: TLAInterval;
begin
  if not Assigned(FLastInterval) then
    FLastInterval := TLAInterval.Create;
  Result := FLastInterval;
end;

//class function TLAInterval.IntervalKindToStr(aKind: TLAIntervalKind): string;
//begin
//  case aKind of
//    ikAbsolute:
//      Result := rsAbsolute;
//    ikLastNHours:
//      Result := rsLastNHoursFmt;
//    ikLastNDays:
//      Result := rsLastNDaysFmt;
//    ikToday:
//      Result := rsToday;
//    ikYesterday:
//      Result := rsYesterday;
//    ikThisWeek:
//      Result := rsThisWeek;
//    ikPreviousWeek:
//      Result := rsPreviousWeek;
//    ikThisMonth:
//      Result := rsThisMonth;
//    ikPreviousMonth:
//      Result := rsPreviousMonth;
//    ikThisYear:
//      Result := rsThisYear;
//  end;
//end;

class function TLAInterval.IntervalKindToStr(aKind: TLAIntervalKind): string;
begin
  case aKind of
    ikAbsolute:
      Result := DKLangConstW('sAbsolute');// rsAbsolute;
    ikLastNHours:
      Result := DKLangConstW('sLastNHoursFmt');// rsLastNHoursFmt;
    ikLastNDays:
      Result := DKLangConstW('sLastNDaysFmt');// rsLastNDaysFmt;
    ikToday:
      Result := DKLangConstW('sToday');// rsToday;
    ikYesterday:
      Result := DKLangConstW('sYesterday');// rsYesterday;
    ikThisWeek:
      Result := DKLangConstW('sThisWeek');// rsThisWeek;
    ikPreviousWeek:
      Result := DKLangConstW('sPreviousWeek');// rsPreviousWeek;
    ikThisMonth:
      Result := DKLangConstW('sThisMonth');// rsThisMonth;
    ikPreviousMonth:
      Result := DKLangConstW('sPreviousMonth');// rsPreviousMonth;
    ikThisYear:
      Result := DKLangConstW('sThisYear');// rsThisYear;
  end;
end;


procedure TLAInterval.Lock;
begin
  Inc(FLockCount);
end;

procedure TLAInterval.SetDate1(const Value: TDatetime);
begin
  if SameValue(FDate1, Value) then
    Exit;

  // если нет блокировок, то проверяем корректность дат
  if FLockCount <= 0 then
  begin
    // менять даты можем только в Абсолютном режиме
    if Kind <> ikAbsolute then
      raise ELAIntervalException.Create(rsYouHaveToSetAbsoluteIntervalKindFirst);

    if Value > FDate2 then
      raise ELAIntervalException.Create(rsDate1GreaterThanDate2);
  end;

  FDate1 := Max(1, Value);
  DoChanged;
end;

procedure TLAInterval.SetDate2(const Value: TDatetime);
begin
  if SameValue(FDate2, Value) then
    Exit;

  // если нет блокировок, то проверяем корректность дат
  if FLockCount <= 0 then
  begin
    // менять даты можем только в Абсолютном режиме
    if Kind <> ikAbsolute then
      raise ELAIntervalException.Create(rsYouHaveToSetAbsoluteIntervalKindFirst);

    if Value < FDate1 then
      raise ELAIntervalException.Create(rsDate1GreaterThanDate2);
  end;

  FDate2 := Max(1, Value);
  DoChanged;
end;

procedure TLAInterval.SetInterval(aDate1, aDate2: TDateTime);
begin
  if aDate1 < 1 then
    aDate1 := 1;
  if aDate2 < 1 then
    aDate2 := 1;

  if SameValue(aDate1, aDate2) then
  begin
    FDate1 := aDate1;
    FDate2 := aDate1 + 1;
  end
  else if aDate1 < aDate2 then
  begin
    FDate1 := aDate1;
    FDate2 := aDate2;
  end

  else
  begin
    FDate1 := aDate2;
    FDate2 := aDate1;
  end;

  FKind := ikAbsolute;
end;

procedure TLAInterval.SetKind(const Value: TLAIntervalKind);
begin
  FKind := Value;
end;

class procedure TLAInterval.SetLastInterval(const Value: TLAInterval);
begin
  if not Assigned(FLastInterval) then
    FLastInterval := TLAInterval.Create;
  FLastInterval.Assign(Value);
end;

procedure TLAInterval.SetShift(const Value: Double);
begin
  FShift := Value;
end;

procedure TLAInterval.Unlock;
begin
  Dec(FLockCount);

  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    if FWasChanged then
      DoChanged;
  end;
end;

end.
