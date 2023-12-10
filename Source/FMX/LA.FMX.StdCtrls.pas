unit LA.FMX.StdCtrls;

interface

uses
  System.Classes, System.Types, System.UITypes, System.SysUtils,
  FMX.Types, FMX.Objects, FMX.StdCtrls, FMX.Graphics, FMX.Styles, FMX.Layouts,
  LA.Data.Types,
  LA.Data.Link.Sensor, LA.Data.Link.Sensor.Intf,
  LA.Data.Source;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TLALabel = class(TLabel, ILASensorLink)
  private
    FLink: TLASensorLink;
    FLastLinkStatus: string;
    procedure SetLink(const Value: TLASensorLink);
  protected
    procedure ApplyStyle; override;

    function GetDefaultSize: TSizeF; override;
    function GetStyleObject: TFmxObject; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// возвращает текстовое представление данных линка для выбора соответствующего стиля отображения (суфикс добавляется к стилю)
    ///  'default' - данные в порядке
    ///  'error' - ошибка
    ///  'h' - превышение высокого уровня - предупреждение
    ///  'hh' - превышение максимально высокого уровня - авария
    ///  'l' - низкий уровень - предупреждение
    ///  'll' - низкий уровень - авария
    //function GetLinkStatus: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoDataLinkChanged(Sender: TObject);
  published
    property Link: TLASensorLink read FLink write SetLink implements ILASensorLink;
  end;


implementation

//{$R *.all.res}
{$R LA.FMX.StdCtrls.all.RES}


const
  cLALabelStyleName = 'LALabelStyle';


{ TLALabel }

procedure TLALabel.ApplyStyle;
var
  aLayout: TLayout;
begin
  inherited;

  /// нужно сделать невидимыми все лайауты кроме того, который соответствует состоянию данных
  if FindStyleResource<TLayout>('layout', aLayout) then
  begin
    for var c in aLayout.Controls do
      if (c is TRectangle) then
        c.Visible := SameText(c.StyleName, FLastLinkStatus);
  end;
end;
//var
//  r: TRectangle;
//begin
//  inherited;
//
//  if Link.Status <> '' then
//  begin
//    if FindStyleResource<TRectangle>(cSysleResBackground, r) then
//    begin
//      r.Stroke.Dash := TStrokeDash.Dash;
//      r.Stroke.Color := TAlphaColorRec.Coral;
//      r.Fill.Color := TAlphaColorRec.Lemonchiffon;
//    end;
//  end;
//end;

constructor TLALabel.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.OnOwnerNotify := DoDataLinkChanged;

  FLastLinkStatus := 'default';

  TextSettings.HorzAlign := TTextAlign.Center;
  Size.Width := 100;
  Size.Height := 25;
end;

destructor TLALabel.Destroy;
begin
  FLink.Free;
  FLink := nil;
  inherited;
end;

procedure TLALabel.DoDataLinkChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  Text := Link.Text;

  if FLastLinkStatus <> Link.StatusStyleName then
  begin
    FLastLinkStatus := Link.StatusStyleName;
    ApplyStyle;
  end;
end;

function TLALabel.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 25);
end;

//function TLALabel.GetLinkStatus: string;
//begin
//  if (Link.Status <> '') or (Link.StatusCode <> 0) then
//    Result := 'error'
//  else
//  begin
//    case Link.ValueRange.Check(Link.Value) of
//      Correct, NoRange:
//        Result := 'default';
//      LowLow:
//        Result := 'LL';
//      Low:
//        Result := 'L';
//      High:
//        Result := 'H';
//      HighHigh:
//        Result := 'HH';
//      InTarget:
//        Result := 'T';
//      NoTarget:
//        Result := 'NT';
//    end;
//  end;
//end;

function TLALabel.GetStyleObject: TFmxObject;
begin
  if StyleLookup = '' then
    Result := TFmxObject(TStyleStreaming.LoadFromResource(HInstance, cLALabelStyleName, RT_RCDATA))
  else
    Result := inherited GetStyleObject;
end;

procedure TLALabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLALabel.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

end.
