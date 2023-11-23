unit LA.FMX.Objects;

interface

uses
  System.Classes, System.Types, System.UITypes,
  System.SysUtils, System.Math,
  FMX.Objects, FMX.ImgList, FMX.Graphics, FMX.Types, FMX.Layouts,
  LA.Utils.Str,
  LA.Data.Link.Sensor, LA.Data.Link.Sensor.Intf,
  LA.Data.Source;

type
  TCalcImageIndexEvent = procedure(Sender: TObject; aValue: Double; var aImageIndex: Integer) of object;

  [ComponentPlatforms(pidAllPlatforms)]
  TLAText = class(TText, ILASensorLink)
  private
    FLink: TLASensorLink;
    procedure SetLink(const Value: TLASensorLink);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoDataLinkChanged(Sender: TObject);
  published
    property Link: TLASensorLink read FLink  write SetLink implements ILASensorLink;
  end;


  [ComponentPlatforms(pidAllPlatforms)]
  TLAGlyph = class(TGlyph, ILASensorLink)
  private
    FLink: TLASensorLink;
    FOnCalcImageIndex: TCalcImageIndexEvent;
    FMatchingTable: TStrings;
    FErrorImageIndex: Integer;
    procedure SetLink(const Value: TLASensorLink);
    procedure SetMatchingTable(const Value: TStrings);
    procedure SetErrorImageIndex(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoDataLinkChanged(Sender: TObject);

    function LinkValueToImageIndex(aValue: Double): Integer;
  published
    property Link: TLASensorLink read FLink  write SetLink implements ILASensorLink;

    // индекс картинки для значений с ошибкой
    property ErrorImageIndex: Integer read FErrorImageIndex write SetErrorImageIndex default -1;
    /// таблица соответствий значений датчика и индексов картинок в списке картинок
    ///  0=21
    ///  1=22
    ///  or
    ///  0=0
    ///  10=1
    ///  50=10
    ///  значения 0-9.9(9) - получат индекс 0
    ///  10-49.9(9) - индекс 1
    ///  все что больше или равно 50 - индекс 10
    ///  значения с десятичной частью записываем через точку: 0.123
    property MatchingTable: TStrings read FMatchingTable write SetMatchingTable;
    // дадим возможность пользователю переопределить механизм определения ImageIndex по значению датчика
    property OnCalcImageIndex: TCalcImageIndexEvent read FOnCalcImageIndex write FOnCalcImageIndex;

    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;

    property Locked default False;
    property HitTest;

    property Hint;
    property ParentShowHint;
    property ShowHint;

    property PopupMenu;

    property RotationAngle;
    property RotationCenter;
    property Scale;

    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
  end;

implementation

{ TLAText }

constructor TLAText.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.OnOwnerNotify := DoDataLinkChanged;
end;

destructor TLAText.Destroy;
begin
  FLink.Free;
  FLink := nil;
  inherited;
end;

procedure TLAText.DoDataLinkChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  Text := Link.Text;
end;

procedure TLAText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLAText.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

{ TLAGlyph }

constructor TLAGlyph.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.OnOwnerNotify := DoDataLinkChanged;
  FMatchingTable := TStringList.Create;

  HitTest := True;
end;

destructor TLAGlyph.Destroy;
begin
  FMatchingTable.Free;
  FLink.Free;
  FLink := nil;
  inherited;
end;

procedure TLAGlyph.DoDataLinkChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  ImageIndex := LinkValueToImageIndex(Link.Value);
end;

function TLAGlyph.LinkValueToImageIndex(aValue: Double): Integer;
var
  aImageIndex: Integer;
  aLeft, aRight: Double;
begin
  if Assigned(FOnCalcImageIndex) then
    FOnCalcImageIndex(Self, Link.Value, Result)
  else
  begin
    // если задана картинка для значений с ошибкой и есть ошбики, показываем эту картинку
    if (ErrorImageIndex > -1) and (Link.Status <> '') then
      Result := ErrorImageIndex

    // если таблица соответствий не задана, то для индекса картинки используем значение датчика
    else if MatchingTable.Count = 0 then
      Result := Round(Link.Value)

    else
    begin
      // 1. проверяем точное соответствие
      var aIndexStr := MatchingTable.Values[Link.Value.ToString(dotFS)];
      if aIndexStr <> '' then
        Result := aIndexStr.ToInteger

      // 2. выполняем поиск наиболее подходящего индекса (предполагаем, что таблица отсортирована в порядке возрастания Значений датчика)
      else
      begin
        // меньше перевого - берем первый
        aLeft := StrToFloat(MatchingTable.Names[0], dotFS);
        if aValue < aLeft then
          Exit(StrToInt(MatchingTable.ValueFromIndex[0]));

        // больше последнего - берем последний
        aRight := StrToFloat(MatchingTable.Names[MatchingTable.Count - 1], dotFS);
        if aValue > aRight then
          Exit(StrToInt(MatchingTable.ValueFromIndex[MatchingTable.Count - 1]));

        // ответ внутри таблицы
        for var i := 1 to FMatchingTable.Count - 1 do
        begin
          aLeft := StrToFloat(MatchingTable.Names[i - 1], dotFS);
          aRight := StrToFloat(MatchingTable.Names[i], dotFS);
          if (aLeft <= aValue) and (aValue < aRight) then
          begin
            Result := StrToInt(MatchingTable.ValueFromIndex[i-1]);
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TLAGlyph.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLAGlyph.Paint;
const
  MinCrossSize = 3;
  MaxCrossSize = 13;
var
  TextRect, ImgRect, BitmapRect: TRectF;
  CrossSize, ScreenScale: Single;
  Bitmap: TBitmap;
  BitmapSize: TSize;
begin
//  inherited;
  if [csLoading, csDestroying] * ComponentState = [] then
  begin
    BitmapSize := TSize.Create(0, 0);
    Bitmap := nil;
    ImgRect := LocalRect;
    if Scene <> nil then
      ScreenScale := Scene.GetSceneScale
    else
      ScreenScale := 1;

    var c: TFmxObject := Parent;
    while Assigned(c) do
    begin
      if c is TScaledLayout then
        ScreenScale := ScreenScale * TScaledLayout(c).Width/TScaledLayout(c).OriginalWidth;
      c := c.Parent;
    end;




    if (Images <> nil) and (ImgRect.Width >= 1) and (ImgRect.Height >= 1) and (ImageIndex <> -1) and
      ([csLoading, csUpdating, csDestroying] * Images.ComponentState = []) then
    begin
      BitmapSize := TSize.Create(Round(ImgRect.Width * ScreenScale), Round(ImgRect.Height * ScreenScale));
      if not Stretch then
        Images.BestSize(ImageIndex, BitmapSize);
      Bitmap := Images.Bitmap(BitmapSize, ImageIndex)
    end;
    if (csDesigning in ComponentState) and not Locked then
      DrawDesignBorder(DesignBorderColor, DesignBorderColor);
    if Bitmap <> nil then
    begin
      BitmapRect := TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height);
      ImgRect := TRectF.Create(CenteredRect(ImgRect.Round, TRectF.Create(0, 0, Bitmap.Width / ScreenScale,
        Bitmap.Height/ ScreenScale).Round));
      Canvas.DrawBitmap(Bitmap, BitmapRect, ImgRect, AbsoluteOpacity, DisableInterpolation);
    end;
    if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
    begin
      TextRect := LocalRect;
      TextRect.Inflate(0.5, 0.5);
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.Stroke.Color := TAlphaColorRec.Darkgray;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      CrossSize := Trunc(Min(MaxCrossSize, Min(TextRect.Width, TextRect.Height) / 6)) + 1;
      if CrossSize >= MinCrossSize then
      begin
        TextRect.TopLeft.Offset(2, 2);
        TextRect.BottomRight := TextRect.TopLeft;
        TextRect.BottomRight.Offset(CrossSize, CrossSize);
        if Bitmap = nil then
        begin
          if Images = nil then
            Canvas.Stroke.Color := TAlphaColorRec.Red;
          Canvas.DrawLine(TextRect.TopLeft, TextRect.BottomRight, AbsoluteOpacity);
          Canvas.DrawLine(TPointF.Create(TextRect.Right, TextRect.Top), TPointF.Create(TextRect.Left, TextRect.Bottom),
            AbsoluteOpacity);
          TextRect := TRectF.Create(TextRect.Left, TextRect.Bottom, Width, Height);
        end;
        if ImageIndex <> -1 then
        begin
          Canvas.Font.Family := 'Small Font';
          Canvas.Font.Size := 7;
          TextRect.Bottom := TextRect.Top + Canvas.TextHeight(Inttostr(ImageIndex));
          if TextRect.Bottom <= Height then
          begin
            Canvas.Fill.Color := TAlphaColorRec.Darkgray;
            Canvas.FillText(TextRect, Inttostr(ImageIndex), False, AbsoluteOpacity, [], TTextAlign.Leading,
              TTextAlign.Leading);
          end;
        end;
      end;
    end;
  end;
end;

procedure TLAGlyph.SetErrorImageIndex(const Value: Integer);
begin
  FErrorImageIndex := Value;
end;

procedure TLAGlyph.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

procedure TLAGlyph.SetMatchingTable(const Value: TStrings);
begin
  FMatchingTable.Assign(Value);
end;

end.
