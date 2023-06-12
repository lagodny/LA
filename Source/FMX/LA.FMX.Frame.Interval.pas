unit LA.FMX.Frame.Interval;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.DateUtils,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  LA.Interval, FMX.ListBox, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.Controls.Presentation, FMX.DateTimeCtrls;

type
  TLAIntervalFrame = class(TFrame)
    CenterLayout: TLayout;
    rbLast: TRadioButton;
    nbNumber: TNumberBox;
    cbHours: TComboBox;
    rbDay: TRadioButton;
    deDay: TDateEdit;
    LastLayout: TLayout;
    DayLayout: TLayout;
    MonthLayout: TLayout;
    rbMonth: TRadioButton;
    deMonth: TDateEdit;
    PeriodLayout: TLayout;
    rbPeriod: TRadioButton;
    FromLayout: TLayout;
    lFrom: TLabel;
    deFrom: TDateEdit;
    teFrom: TTimeEdit;
    ToLayout: TLayout;
    lTo: TLabel;
    deTo: TDateEdit;
    teTo: TTimeEdit;
    cbKind: TComboBox;
    procedure rbLastChange(Sender: TObject);
    procedure cbKindChange(Sender: TObject);
    procedure deFromChange(Sender: TObject);
  private
    FViewUpdating: Boolean;
    FInterval: TLAInterval;
    procedure SetInterval(const Value: TLAInterval);

    procedure InitIntervalKind;
    procedure SetAbsolute;
    procedure UpdateUI;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure IntervalToView;
    procedure ViewToInterval;

    property Interval: TLAInterval read FInterval write SetInterval;
  end;

implementation

uses
  System.Math;

{$R *.fmx}

{ TLAIntervalFrame }

procedure TLAIntervalFrame.cbKindChange(Sender: TObject);
begin
  Interval.Kind := TLAIntervalKind(cbKind.ItemIndex);
  if not FViewUpdating then
    IntervalToView;
end;

constructor TLAIntervalFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitIntervalKind;

  FInterval := TLAInterval.Create;
  Interval := TLAInterval.LastInterval;
end;

procedure TLAIntervalFrame.deFromChange(Sender: TObject);
begin
  SetAbsolute;
end;

destructor TLAIntervalFrame.Destroy;
begin
  FreeAndNil(FInterval);
  inherited;
end;

procedure TLAIntervalFrame.InitIntervalKind;
begin
//  cbKind.Items.Clear;
//  cbKind.ListBox.Items.Add('hello');
  if cbKind.Count = 0 then
  begin
    cbKind.Items.Add(rsAbsolute);
    cbKind.Items.Add(rsToday);
    cbKind.Items.Add(rsYesterday);
    cbKind.Items.Add(rsThisWeek);
    cbKind.Items.Add(rsPreviousWeek);
    cbKind.Items.Add(rsThisMonth);
    cbKind.Items.Add(rsPreviousMonth);
    cbKind.Items.Add(rsThisYear);
  end;
end;

procedure TLAIntervalFrame.rbLastChange(Sender: TObject);
begin
  UpdateUI;
end;

procedure TLAIntervalFrame.SetAbsolute;
begin
  FViewUpdating := True;
  cbKind.ItemIndex := 0;
  FViewUpdating := False;
//  FInterval.Kind := ikAbsolute;
end;

procedure TLAIntervalFrame.SetInterval(const Value: TLAInterval);
begin
  FInterval.Assign(Value);
  IntervalToView;
end;

procedure TLAIntervalFrame.IntervalToView;
begin
  FViewUpdating := True;
  try
    case Interval.Kind of
      ikAbsolute,
      ikToday,
      ikYesterday,
      ikThisWeek,
      ikPreviousWeek,
      ikThisMonth,
      ikPreviousMonth,
      ikThisYear:
      begin
        rbPeriod.IsChecked := True;
        cbKind.ItemIndex := Ord(Interval.Kind);
      end;

      ikLastNHours:
      begin
        rbLast.IsChecked := True;
        cbHours.ItemIndex := 0;
        cbKind.ItemIndex := Ord(Interval.Kind);
      end;

      ikLastNDays:
      begin
        rbLast.IsChecked := True;
        cbHours.ItemIndex := 1;
        cbKind.ItemIndex := Ord(Interval.Kind);
      end;
    end;

    rbLast.IsChecked := Interval.Kind in [ikLastNHours, ikLastNDays];
    rbDay.IsChecked := (Interval.Kind = ikAbsolute) and SameValue(Interval.Date1 + 1, Interval.Date2);
    rbMonth.IsChecked := (Interval.Kind = ikAbsolute)
      and SameValue(Interval.Date1, Trunc(StartOfTheMonth(Interval.Date1)))
      and SameValue(Interval.Date2, Trunc(StartOfTheMonth(Interval.Date2)))
      and (MonthsBetween(Interval.Date1, Interval.Date2) = 1);
    rbPeriod.IsChecked := not (rbLast.IsChecked or rbDay.IsChecked or rbMonth.IsChecked);

    nbNumber.Value := Interval.Shift;

    deDay.Date := Interval.Date1;
    deMonth.Date := Interval.Date1;

    deFrom.Date := Trunc(Interval.Date1);
    teFrom.Time := Frac(Interval.Date1);
    deTo.Date := Trunc(Interval.Date2);
    teTo.Time := Frac(Interval.Date2);

    UpdateUI;
  finally
    FViewUpdating := False;
  end;
end;

procedure TLAIntervalFrame.UpdateUI;
begin
  cbHours.Enabled := rbLast.IsChecked;
  nbNumber.Enabled := rbLast.IsChecked;

  deDay.Enabled := rbDay.IsChecked;

  deMonth.Enabled := rbMonth.IsChecked;

  cbKind.Enabled := rbPeriod.IsChecked;
  var aPeriodEnabled := rbPeriod.IsChecked;

  deFrom.Enabled := aPeriodEnabled;
  teFrom.Enabled := aPeriodEnabled;
  deTo.Enabled := aPeriodEnabled;
  teTo.Enabled := aPeriodEnabled;
end;

procedure TLAIntervalFrame.ViewToInterval;
begin
  if rbLast.IsChecked then
  begin
    if cbHours.ItemIndex = 0 then
      Interval.Kind := ikLastNHours
    else
      Interval.Kind := ikLastNDays;

    Interval.Shift := nbNumber.Value;
  end
  else if rbDay.IsChecked then
  begin
    Interval.Kind := ikAbsolute;
    Interval.SetInterval(
      Trunc(deDay.Date),
      Trunc(deDay.Date) + 1
    );
  end
  else if rbMonth.IsChecked then
  begin
    Interval.Kind := ikAbsolute;
    Interval.SetInterval(
      Trunc(StartOfTheMonth(deMonth.Date)),
      Trunc(EndOfTheMonth(deMonth.Date)) + 1
    );
  end
  else if rbPeriod.IsChecked then
  begin
    Interval.Kind := TLAIntervalKind(cbKind.ItemIndex);
    if Interval.Kind = ikAbsolute then
      Interval.SetInterval(
        Trunc(deFrom.Date) + Frac(teFrom.Time),
        Trunc(deTo.Date) + Frac(teTo.Time)
      );
  end;
end;

end.
