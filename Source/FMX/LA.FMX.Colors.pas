unit LA.FMX.Colors;

interface

uses
  System.Classes,
  LA.FMX.UI.Consts;

type
  // компонент позволяет хранить информацию о цветах и переключать их в зависимости от указанной схемы
  TLAColorManager = class(TComponent)
  private
    FData: TStrings;
    FScheme: string;
    FOnApply: TNotifyEvent;
    procedure SetData(const Value: TStrings);
    procedure SetScheme(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // применение новой схемы выполняем явным вызовом процедуры
    procedure Apply;
  published
    // поле для хранения JSON полученного выгрузкой из Figma (plugin Matherial Theme Builder)
    property Data: TStrings read FData write SetData;
    property Scheme: string read FScheme write SetScheme;

    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

implementation

uses
  LA.FMX.Prop.Utils;

{ TLAColorManager }

procedure TLAColorManager.Apply;
begin
  InitLAColorsFromJSON(Data.Text, Scheme);
  TPropertyReloader.Reload;

  // возможно нужно выполнить другие операции после обновления цветов
  //  TStyleManager.UpdateScenes;
  //  SVGImages.RefreshAllIcons;
  if Assigned(FOnApply) then
    FOnApply(Self);
end;

constructor TLAColorManager.Create(AOwner: TComponent);
begin
  inherited;
  FData := TStringList.Create;
end;

destructor TLAColorManager.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TLAColorManager.SetData(const Value: TStrings);
begin
  if FData.Text <> Value.Text then
  begin
    FData.Assign(Value);
//    InitLAColorsFromJSON(Data.Text, Scheme);
  end;
end;

procedure TLAColorManager.SetScheme(const Value: string);
begin
  if FScheme <> Value then
  begin
    FScheme := Value;
//    InitLAColorsFromJSON(Data.Text, Scheme);
  end;
end;

end.
