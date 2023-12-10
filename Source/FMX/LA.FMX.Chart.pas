unit LA.FMX.Chart;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Types,
  FMXTee.Chart, FMXTee.Engine, FMXTee.Procs, FMXTee.Canvas,
  LA.Interval;

const
  cMouseWheelZoomFactor = 1.5;

type
//  TLAChartZoomKind = (zsTime, zsValue);
//  TLAChartZoomSet = set of TLAChartZoomKind;

  /// построитель графиков: изменение показаний датчиков во времени
  ///  нижняя ось - время
  ///  левая или дополнительные вертикальные оси - показания датчиков
  ///  можно задать интервал, если интервал перекрывает текущий момент времени, то графики достраиваются
  ///  для построения графиков используются специальные наследники содержащие линк на датчик
  TLAChart = class(TChart, IHasLAInterval)
  private
    FLastTouches: TTouches;
    FInterval: TLAInterval;
    FOnIntervalChanged: TNotifyEvent;
    FMouseWheelZoomFactor: Single;
    function GetInterval: TLAInterval;
    procedure SetInterval(const Value: TLAInterval);
    procedure DoChangeInterval(Sender: TObject);
    function GetAutoScaleY: Boolean;
    procedure SetAutoScaleY(const Value: Boolean);
    procedure SetMouseWheelZoomFactor(const Value: Single);
  protected
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // возвращает корректный TRectF с учетом сдвигов осей
    function GetChartRectFixed: TRectF;
    // выполняем маштабирование относительно центральной точки с разными масштабами по осям
    procedure ZoomByPoint(aPoint: TPointF; aZoomFactorX, aZoomFactorY: Double);
    /// обработку касаний выполнить в событии OnTouch формы
    ///  - обрабатывает масштабирование и перетягивание (одним и двумя пальцами)
    ///  - для сброса логично использовать DblClick (LAChart1.UndoZoom)
    procedure DoTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);

    procedure UpdateSeriesData;
  published
    property Interval: TLAInterval read GetInterval write SetInterval;
    property AutoScaleY : Boolean read GetAutoScaleY write SetAutoScaleY default True;
    property MouseWheelZoomFactor: Single read FMouseWheelZoomFactor write SetMouseWheelZoomFactor;


    property View3D default False;

    property OnIntervalChanged: TNotifyEvent read FOnIntervalChanged write FOnIntervalChanged;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  LA.FMX.Chart.Series.Intf;

{ TLAChart }

constructor TLAChart.Create(AOwner: TComponent);
begin
  inherited;
  FInterval := TLAInterval.Create;
  FInterval.OnChanged := DoChangeInterval;

  FMouseWheelZoomFactor := cMouseWheelZoomFactor;

  Zoom.Allow := False;
  Zoom.MouseWheel := pmwNone;
  ZoomWheel := pmwNone;
  Panning.Active := False;
  Panning.MouseWheel := pmwNone;

  //Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan, TInteractiveGesture.DoubleTap];

  BackWall.Visible := False;

  Legend.Alignment := laBottom;
  Legend.LegendStyle := TLegendStyle.lsSeries;

  BottomAxis.LabelStyle := talValue;
  BottomAxis.AutomaticMaximum := false;
  BottomAxis.AutomaticMinimum := false;
  BottomAxis.SetMinMax(Interval.Date1, Interval.Date2);
  BottomAxis.Axis.Width := 1;
  BottomAxis.Grid.Visible := False;

  LeftAxis.StartPosition := 1;
  LeftAxis.EndPosition := 99;
  LeftAxis.Axis.Width := 1;

  LeftAxis.Grid.Style := TPenStyle.psDash;
  LeftAxis.MinorTicks.Visible := False;
  LeftAxis.TicksInner.Visible := False;
  LeftAxis.Title.Visible := False;

  RightAxis.Visible := False;
  TopAxis.Visible := False;

//  BufferedDisplay := True;
  View3D := False;
  Color := TAlphaColorRec.White;
  Title.Visible := False;
end;

destructor TLAChart.Destroy;
begin
  FInterval.Free;
  inherited;
end;

procedure TLAChart.DoChangeInterval(Sender: TObject);
begin
  if Assigned(FOnIntervalChanged) then
    FOnIntervalChanged(Sender);

  UpdateSeriesData;
  BottomAxis.SetMinMax(Interval.Date1, Interval.Date2);
end;

procedure TLAChart.DoTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
  function OrderTouches(const aTouches: TTouches): TTouches;
  begin
    Result := Copy(aTouches, 0, Length(aTouches));
    if Length(Result) < 2 then
      Exit;

    if aTouches[0].Location.X > aTouches[1].Location.X then
    begin
      Result[0].Location.X := aTouches[1].Location.X;
      Result[1].Location.X := aTouches[0].Location.X;
    end;

    if aTouches[0].Location.Y > aTouches[1].Location.Y then
    begin
      Result[0].Location.Y := aTouches[1].Location.Y;
      Result[1].Location.Y := aTouches[0].Location.Y;
    end;
  end;
var
  dx0, dx1, dy0, dy1: Double;
  aLocalTouches, aOrderedTouches: TTouches;
  aCenterPoint: TPointF;
  aZoomFactorX, aZoomFactorY: Double;
  aZoomRect: TRectF;
begin
  SetLength(aLocalTouches, Length(Touches));
  for var i := 0 to High(Touches) do
    aLocalTouches[i].Location := AbsoluteToLocal(Touches[i].Location);

  case Action of
    TTouchAction.None: ;

    TTouchAction.Up, TTouchAction.Cancel:
    begin
      SetLength(FLastTouches, 0);
    end;

    TTouchAction.Down:
    begin
      FLastTouches := OrderTouches(aLocalTouches);
    end;

    TTouchAction.Move:
    begin
      if (Length(aLocalTouches) = 1) and (Length(FLastTouches) = 1) then
      begin
        aZoomRect := GetChartRectFixed;

        dx0 := aLocalTouches[0].Location.X - FLastTouches[0].Location.X;
        dy0 := aLocalTouches[0].Location.Y - FLastTouches[0].Location.Y;

        aZoomRect.Left := aZoomRect.Left - dx0;
        aZoomRect.Right := aZoomRect.Right - dx0;
        aZoomRect.Top := aZoomRect.Top - dy0;
        aZoomRect.Bottom := aZoomRect.Bottom - dy0;

        ZoomRect(aZoomRect);
      end
      else if (Length(aLocalTouches) >= 2) and (Length(FLastTouches) >= 2) then
      begin
        aOrderedTouches := OrderTouches(aLocalTouches);

        dx0 := aOrderedTouches[0].Location.X - FLastTouches[0].Location.X;
        dy0 := aOrderedTouches[0].Location.Y - FLastTouches[0].Location.Y;
        dx1 := aOrderedTouches[1].Location.X - FLastTouches[1].Location.X;
        dy1 := aOrderedTouches[1].Location.Y - FLastTouches[1].Location.Y;

        if (Sign(dx0) = Sign(dx1)) or (Sign(dy0) = Sign(dy1)) then
        begin
          aZoomRect := GetChartRectFixed;
          aZoomRect.Left := aZoomRect.Left - dx0;
          aZoomRect.Right := aZoomRect.Right - dx1;
          aZoomRect.Top := aZoomRect.Top - dy0;
          aZoomRect.Bottom := aZoomRect.Bottom - dy1;
          ZoomRect(aZoomRect);
        end
        else
        begin
          var dx := aOrderedTouches[1].Location.X - aOrderedTouches[0].Location.X;
          var dy := aOrderedTouches[1].Location.Y - aOrderedTouches[0].Location.Y;

          aCenterPoint.X := (aOrderedTouches[1].Location.X + aOrderedTouches[0].Location.X)/2;
          aCenterPoint.Y := (aOrderedTouches[1].Location.Y + aOrderedTouches[0].Location.Y)/2;
          if (dx < 50) then
            aZoomFactorX := 1
          else
            aZoomFactorX := (FLastTouches[1].Location.X - FLastTouches[0].Location.X)/
              (aOrderedTouches[1].Location.X - aOrderedTouches[0].Location.X);

          if dy < 50 then
            aZoomFactorY := 1
          else
            aZoomFactorY := (FLastTouches[1].Location.Y - FLastTouches[0].Location.Y)/
              (aOrderedTouches[1].Location.Y - aOrderedTouches[0].Location.Y);

          ZoomByPoint(aCenterPoint, aZoomFactorX, aZoomFactorY);
        end;
      end;

      FLastTouches := OrderTouches(aLocalTouches);
    end;
  end;
end;

function TLAChart.GetAutoScaleY: Boolean;
begin
  Result := LeftAxis.Automatic;
end;

function TLAChart.GetChartRectFixed: TRectF;
var
  i: Integer;
  aVertAxis, aHorizAxis: TChartAxis;
  aSerie: TChartSeries;
begin
  Result := ChartRect;

  aVertAxis := nil;
  aHorizAxis := nil;
  for i := 0 to SeriesList.Count - 1 do
  begin
    aSerie := Series[i];
    if aSerie.Active then
    begin
      case aSerie.VertAxis of
        aLeftAxis, aBothVertAxis:
          aVertAxis := LeftAxis;
        aRightAxis:
          aVertAxis := RightAxis;
        aCustomVertAxis:
          aVertAxis := aSerie.CustomVertAxis;
      end;

      case aSerie.HorizAxis of
        aTopAxis, aBothHorizAxis:
          aHorizAxis := TopAxis;
        aBottomAxis:
          aHorizAxis := BottomAxis;
        aCustomHorizAxis:
          aHorizAxis := aSerie.CustomHorizAxis;
      end;

      Break;
    end;
  end;

  if Assigned(aVertAxis) and (aVertAxis.PositionUnits = muPercent) then
  begin
    Result.Top := ChartRect.Top + (0.01 * ChartHeight * aVertAxis.StartPosition);
    Result.Bottom := ChartRect.Top + (0.01 * ChartHeight * aVertAxis.EndPosition);
  end;

  if Assigned(aHorizAxis) and (aHorizAxis.PositionUnits = muPercent) then
  begin
    Result.Left := ChartRect.Left + (0.01 * ChartWidth * aHorizAxis.StartPosition);
    Result.Right := ChartRect.Left + (0.01 * ChartWidth * aHorizAxis.EndPosition);
  end;
end;

function TLAChart.GetInterval: TLAInterval;
begin
  Result := FInterval;
end;

procedure TLAChart.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  p: TPointF;
  aCoef: Single;
  aVertRect, aHorRect: TRectF;
begin
  inherited;

  if Panning.MouseWheel = pmwNone then
  begin
    p := GetCursorPos; //  ScreenToClient(Mouse.CursorPos);

    if WheelDelta < 0 then
      aCoef := MouseWheelZoomFactor
    else
      aCoef := 1/MouseWheelZoomFactor;

    aVertRect := TeeRect(0, LeftAxis.IStartPos, LeftAxis.PosAxis + 10, LeftAxis.IEndPos);
    aHorRect := TeeRect(BottomAxis.IStartPos, BottomAxis.PosAxis - 10, BottomAxis.IEndPos, Height);

    // c Ctrl масштабируем по обоим осям
    if ssCtrl in Shift then
      ZoomByPoint(p, aCoef, aCoef)
    // с Shift или курсор в области оси значений - по оси значений
    else if (ssShift in Shift) or PointInRect(aVertRect, p) then
      ZoomByPoint(p, 1, aCoef)
    // иначе по оси времени
    else
      ZoomByPoint(p, aCoef, 1);
  end;
end;

procedure TLAChart.SetAutoScaleY(const Value: Boolean);
begin
  for var i := 0 to Axes.Count - 1 do
    if not Axes[i].Horizontal then
      Axes[i].Automatic := Value;
end;

procedure TLAChart.SetInterval(const Value: TLAInterval);
begin
  FInterval.Assign(Value);
end;

procedure TLAChart.SetMouseWheelZoomFactor(const Value: Single);
begin
  FMouseWheelZoomFactor := Value;
end;

procedure TLAChart.UpdateSeriesData;
var
  s: ILASeries;
begin
  for var aSerie in SeriesList do
  begin
    if Supports(aSerie, ILASeries, s) then
      s.FillData(False)
  end;
end;

procedure TLAChart.ZoomByPoint(aPoint: TPointF; aZoomFactorX, aZoomFactorY: Double);
var
  w1, h1: Single;
  dw, dh: Single;
  aZoomRect: TRectF;
begin
  aZoomRect := GetChartRectFixed;

  w1 := (aZoomRect.Right - aZoomRect.Left) * aZoomFactorX / 2;
  dw := (aPoint.X - (aZoomRect.Right + aZoomRect.Left) / 2) * aZoomFactorX;

  aZoomRect.Left := aPoint.X - w1 - dw;
  aZoomRect.Right := aPoint.X + w1 - dw;

  h1 := (aZoomRect.Bottom - aZoomRect.Top) * aZoomFactorY / 2;
  dh := (aPoint.Y - (aZoomRect.Bottom + aZoomRect.Top) / 2) * aZoomFactorY;

  aZoomRect.Top := aPoint.Y - h1 - dh;
  aZoomRect.Bottom := aPoint.Y + h1 - dh;

  var SaveAnimatedZoom := AnimatedZoom;
  try
    AnimatedZoom := False;
    ZoomRect(aZoomRect);
  finally
    AnimatedZoom := SaveAnimatedZoom;
  end;
end;

end.
