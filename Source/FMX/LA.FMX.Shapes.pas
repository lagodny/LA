unit LA.FMX.Shapes;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Types, FMX.Graphics, FMX.Objects, FMX.Utils,
  LA.Data.Types, LA.Keys,
  La.Interval,
  LA.Data.Link.Sensor, LA.Data.Link.Sensor.Intf;

type
  TLAShape = class(TShape)

  end;

  TLAShapeSensorLinked = class(TLAShape, ILASensorLink)
  private
    FLink: TLASensorLink;
    procedure SetLink(const Value: TLASensorLink);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // необходимо переопределить у наследников для выполнения полезной работы
    procedure DoDataLinkChanged(Sender: TObject); virtual;
  published
    property Link: TLASensorLink read FLink  write SetLink implements ILASensorLink;
    // Inherited Properties & Events
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Locked;
    property Height;
    property HitTest;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Stroke;
    property Visible;
    property Width;

    property OnClick;
    property OnDblClick;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

  TLAColorKeyLine = class(TLAShapeSensorLinked)
  strict private
    const
      DefErrorColor = TAlphaColorRec.Gray;
  private
    FInterval: TLAInterval;
    FOnIntervalChanged: TNotifyEvent;
    FKeys: TLAKeys;
    FErrorColor: TAlphaColor;
    function GetInterval: TLAInterval;
    procedure SetInterval(const Value: TLAInterval);
    procedure DoChangeInterval(Sender: TObject);
    procedure SetKeys(const Value: TLAKeys);
    procedure SetErrorColor(const Value: TAlphaColor);
  protected
    procedure Paint; override;
  public
    FValues: TLASensorValueSnapshots;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoDataLinkChanged(Sender: TObject); override;
    procedure FillData(aSkipIfNotPossible: Boolean = True);

    procedure ClearNotUsedRecs;
  published
    property Keys: TLAKeys read FKeys write SetKeys;
    property Interval: TLAInterval read GetInterval write SetInterval;
    property ErrorColor: TAlphaColor read FErrorColor write SetErrorColor default DefErrorColor;

    property OnIntervalChanged: TNotifyEvent read FOnIntervalChanged write FOnIntervalChanged;
  end;


implementation

uses
  System.SysUtils, System.DateUtils, System.Math,
  LA.Net.Connector;


{ TLAShapeSenosrLinked }

constructor TLAShapeSensorLinked.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.OnOwnerNotify := DoDataLinkChanged;
end;

destructor TLAShapeSensorLinked.Destroy;
begin
  FLink.Free;
  FLink := nil;
  inherited;
end;

procedure TLAShapeSensorLinked.DoDataLinkChanged(Sender: TObject);
begin

end;

procedure TLAShapeSensorLinked.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLAShapeSensorLinked.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

{ TLAColorLine }

procedure TLAColorKeyLine.ClearNotUsedRecs;
var
  i, i1, i2: Integer;
  aDate1, aDate2: TDateTime;
begin
  aDate1 := Interval.Date1;
  aDate2 := Interval.Date2;
  // находим левый индекс записи, которая не входит в интервал
  i1 := 0;
  for i := 0 to High(FValues) do
    if FValues[i].FDateTime >= aDate1 then
    begin
      if i > 0 then
        i1 := i - 1
      else
        i1 := 0;
      Break;
    end;

  // находим правый индекс записи, которая входит в интервал
  i2 := High(FValues);
  for i := High(FValues) downto 0 do
    if FValues[i].FDateTime <= aDate2 then
    begin
      i2 := i;
      Break;
    end;

  // сдвигаем массив
  if i1 > 0 then
    for i := i1 to i2 do
      FValues[i - i1] := FValues[i];
  SetLength(FValues, i2 - i1 + 1);
end;

constructor TLAColorKeyLine.Create(AOwner: TComponent);
begin
  inherited;
  FLink.NotifyConstantly := True;
  FKeys := TLAKeys.Create(TLAColorKey);
  FInterval := TLAInterval.Create;
  FInterval.OnChanged := DoChangeInterval;
  FErrorColor := DefErrorColor;
end;

destructor TLAColorKeyLine.Destroy;
begin
  FInterval.Free;
  FKeys.Free;
  inherited;
end;

procedure TLAColorKeyLine.DoChangeInterval(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  if Assigned(FOnIntervalChanged) then
    FOnIntervalChanged(Sender);

  FillData;
end;

procedure TLAColorKeyLine.DoDataLinkChanged(Sender: TObject);
var
  aNow: TDateTime;
  aRec: TLASensorValueSnapshot;
begin
  aNow := Now;
  if (aNow >= Interval.Date1) and (aNow <= Interval.Date2) then
  begin
    if (Length(FValues) = 0)
      or not SameValue(FValues[High(FValues)].FState, Link.StatusCode)
      or not SameValue(FValues[High(FValues)].FValue, Link.Value) then
    begin
      aRec.FDateTime := aNow; //FLink.Timestamp;
      aRec.FValue := FLink.Value;
      aRec.FState := FLink.StatusCode;
      SetLength(FValues, Length(FValues) + 1);

      FValues[High(FValues)] := aRec;
    end
  end;
  ClearNotUsedRecs;
  Repaint;
end;

procedure TLAColorKeyLine.FillData(aSkipIfNotPossible: Boolean);
var
  i: Integer;
  aStream: TMemoryStream;
  aInterval: TLAInterval;
  aConnector: TLACustomConnector;
  aRec: TLASensorValueSnapshot;
  aDate1, aDate2: TDateTime;
begin
  if FLink.ID = '' then
    if aSkipIfNotPossible then
      Exit
    else
      raise Exception.Create('No sensor ID');

  aInterval := Interval;
  if not Assigned(aInterval) then
    if aSkipIfNotPossible then
      Exit
    else
      raise Exception.Create('No Interval');

  aConnector := FLink.Connector;
  if not Assigned(aConnector) then
    if aSkipIfNotPossible then
      Exit
    else
      raise Exception.Create('No Connector');

  if not aConnector.Connected and aSkipIfNotPossible then
    Exit;

  aStream := TMemoryStream.Create;
  try
    aDate1 := aInterval.Date1;
    aDate2 := aInterval.Date2;
    aConnector.SensorHistoryStream(aStream,
      FLink.ID, DateTimeToUnix(aDate1, False), DateTimeToUnix(aDate2, False),
      True, True, False, True, False);

    SetLength(FValues, aStream.Size div SizeOf(aRec));

    i := 0;
    aStream.Position := 0;
    while aStream.Position <= (aStream.Size - SizeOf(aRec)) do
    begin
      aStream.Read(aRec, SizeOf(aRec));

//      // исключаем заведомо неверные записи
//      if (aRec.FDateTime < aDate1) or (aRec.FDateTime > aDate2) then
//        Continue;
//      // контролируем, что время возрастает
//      if (i > 0) and (FValues[i - 1].FDateTime >= aRec.FDateTime) then
//        Continue;

      FValues[i] := aRec;
      Inc(i);
    end;

    if i < Length(FValues) then
      SetLength(FValues, i);
  finally
    aStream.Free;
  end;

  //ValuesToView;
  Repaint;
end;

function TLAColorKeyLine.GetInterval: TLAInterval;
begin
  Result := FInterval;
end;

procedure TLAColorKeyLine.Paint;
const
  cNoKeyColor = TAlphaColorRec.Alpha;
var
  aDate1, aDate2: TDateTime;
  aCoef: Double;
  aShapeRect: TRectF;
  x1, x2: Single;
  aColor: TAlphaColor;
  Key1, Key2: TLAKey;
  aFill: TBrush;

  procedure DrawRec(aRec: TLASensorValueSnapshot);
  begin
    if x2 > 0 then
    begin
      if x1 < 0 then
        x1 := 0;

      if aRec.FState <> 0 then
        aColor := ErrorColor
      else if FKeys.FindKeys(aRec.FValue, Key1, Key2) then
        aColor := InterpolateColor(TLAColorKey(Key1).Value, TLAColorKey(Key2).Value, (aRec.FValue - Key1.Key) / (Key2.Key - Key1.Key))
      else
        aColor := cNoKeyColor;

      aFill.Color := aColor;
      Canvas.FillRect(RectF(aShapeRect.Left + x1, aShapeRect.Top, aShapeRect.Left + x2, aShapeRect.Bottom), 0, 0, [],
        AbsoluteOpacity, aFill, TCornerType.Round);
    end;

  end;
begin
  inherited;

  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;

  if Length(FValues) = 0 then
    Exit;

  aShapeRect := GetShapeRect;
  aDate1 := Interval.Date1;
  aDate2 := Interval.Date2;
  aCoef := (aShapeRect.Right - aShapeRect.Left) / (aDate2 - aDate1);

  aFill := TBrush.Create(Fill.Kind, Fill.Color);
  try
    aFill.Assign(Fill);

    x1 := (FValues[0].FDateTime - aDate1) * aCoef;
    for var i := 1 to High(FValues) do
    begin
      x2 := (FValues[i].FDateTime - aDate1) * aCoef;

      DrawRec(FValues[i-1]);

//      if x2 > 0 then
//      begin
//        if x1 < 0 then
//          x1 := 0;
//
//        if FKeys.FindKeys(FValues[i-1].FValue, Key1, Key2) then
//          aColor := InterpolateColor(TLAColorKey(Key1).Value, TLAColorKey(Key2).Value,
//                  (FValues[i-1].FValue - Key1.Key) / (Key2.Key - Key1.Key))
//        else
//          aColor := cNoKeyColor;
//
//        aFill.Color := aColor;
//        Canvas.FillRect(RectF(aShapeRect.Left + x1, aShapeRect.Top, aShapeRect.Left + x2, aShapeRect.Bottom), 0, 0, [],
//          AbsoluteOpacity, aFill, TCornerType.Round);
//      end;
      x1 := x2;
    end;

    var aNow: TDateTime := Now;
    if (aDate1 <= aNow) and (aNow <= aDate2)  then
    begin
      x2 := (aNow - aDate1) * aCoef;
      DrawRec(FValues[High(FValues)]);
    end;


  finally
    aFill.Free;
  end;
end;

procedure TLAColorKeyLine.SetErrorColor(const Value: TAlphaColor);
begin
  if FErrorColor <> Value then
  begin
    FErrorColor := Value;
    Repaint;
  end;
end;

procedure TLAColorKeyLine.SetInterval(const Value: TLAInterval);
begin
  FInterval.Assign(Value);
end;

procedure TLAColorKeyLine.SetKeys(const Value: TLAKeys);
begin
  FKeys.Assign(Value);
end;

end.
