unit MD.Main;

interface

uses
  LA.FMX.UI.Consts,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TForm8 = class(TForm)
    Button1: TButton;
    chDark: TCheckBox;
    background: TRectangle;
    onBackground: TText;
    primaryContainer: TRectangle;
    onPrimaryContainer: TText;
    secondaryContainer: TRectangle;
    onSecondaryContainer: TText;
    secondary: TRectangle;
    onSecondary: TText;
    secondaryFixed: TRectangle;
    onSecondaryFixed: TText;
    secondaryFixedDim: TRectangle;
    onSecondaryFixedVariant: TText;
    tertiaryContainer: TRectangle;
    onTertiaryContainer: TText;
    tertiary: TRectangle;
    onTertiary: TText;
    tertiaryFixed: TRectangle;
    onTertiaryFixed: TText;
    tertiaryFixedDim: TRectangle;
    onTertiaryFixedVariant: TText;
    errorContainer: TRectangle;
    onErrorContainer: TText;
    error: TRectangle;
    onError: TText;
    outline: TLine;
    inverseSurface: TRectangle;
    inverseOnSurface: TText;
    surface: TRectangle;
    onSurface: TText;
    surfaceVariant: TRectangle;
    onSurfaceVariant: TText;
    outlineVariant: TLine;
    surfaceContainerHighest: TRectangle;
    surfaceContainerHigh: TRectangle;
    surfaceContainer: TRectangle;
    surfaceContainerLow: TRectangle;
    surfaceContainerLowest: TRectangle;
    surfaceDim: TRectangle;
    surfaceBright: TRectangle;
    primaryFixedDim: TRectangle;
    onPrimaryFixedVariant: TText;
    primaryFixed: TRectangle;
    onPrimaryFixed: TText;
    primary: TRectangle;
    onPrimary: TText;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    function StrToColor(const aStr: string): TAlphaColor;
    procedure ThemeToView(const aFileName: string);
  end;

var
  Form8: TForm8;

implementation

uses
  System.IOUtils,
  SynCrossPlatformJSON;

{$R *.fmx}

{ TForm8 }

procedure TForm8.Button1Click(Sender: TObject);
begin
  var f := TOpenDialog.Create(nil);
  try
    if f.Execute then
      ThemeToView(f.FileName);
  finally
    f.Free;
  end;
end;

function TForm8.StrToColor(const aStr: string): TAlphaColor;
begin
  Result := TAlphaColor(StrToInt(StringReplace(aStr, '#', '$FF', [rfReplaceAll, rfIgnoreCase])));
end;

procedure TForm8.ThemeToView(const aFileName: string);
var
  v, schemes, theme: Variant;
  aData: string;
begin
  aData := TFile.ReadAllText(aFileName, TEncoding.UTF8);
  v := JSONVariant(aData);
//    aValue := TJSONObject.ParseJSONValue(aData);
  schemes := v.schemes;
  if chDark.IsChecked then
    theme := schemes.dark
  else
    theme := schemes.light;

  primary.Fill.Color := StrToColor(theme.primary);
  onPrimary.TextSettings.FontColor := StrToColor(theme.onPrimary);
  primaryContainer.Fill.Color := StrToColor(theme.primaryContainer);
  onPrimaryContainer.TextSettings.FontColor := StrToColor(theme.onPrimaryContainer);
  primaryFixed.Fill.Color := StrToColor(theme.primaryFixed);
  onPrimaryFixed.TextSettings.FontColor := StrToColor(theme.onPrimaryFixed);
  primaryFixedDim.Fill.Color := StrToColor(theme.primaryFixedDim);
  onPrimaryFixedVariant.TextSettings.FontColor := StrToColor(theme.onPrimaryFixedVariant);

  secondary.Fill.Color := StrToColor(theme.secondary);
  onSecondary.TextSettings.FontColor := StrToColor(theme.onSecondary);
  secondaryContainer.Fill.Color := StrToColor(theme.secondaryContainer);
  onSecondaryContainer.TextSettings.FontColor := StrToColor(theme.onSecondaryContainer);
  secondaryFixed.Fill.Color := StrToColor(theme.secondaryFixed);
  onSecondaryFixed.TextSettings.FontColor := StrToColor(theme.onSecondaryFixed);
  secondaryFixedDim.Fill.Color := StrToColor(theme.secondaryFixedDim);
  onSecondaryFixedVariant.TextSettings.FontColor := StrToColor(theme.onSecondaryFixedVariant);

  tertiary.Fill.Color := StrToColor(theme.tertiary);
  onTertiary.TextSettings.FontColor := StrToColor(theme.onTertiary);
  tertiaryContainer.Fill.Color := StrToColor(theme.tertiaryContainer);
  onTertiaryContainer.TextSettings.FontColor := StrToColor(theme.onTertiaryContainer);
  tertiaryFixed.Fill.Color := StrToColor(theme.tertiaryFixed);
  onTertiaryFixed.TextSettings.FontColor := StrToColor(theme.onTertiaryFixed);
  tertiaryFixedDim.Fill.Color := StrToColor(theme.tertiaryFixedDim);
  onTertiaryFixedVariant.TextSettings.FontColor := StrToColor(theme.onTertiaryFixedVariant);

  error.Fill.Color := StrToColor(theme.error);
  onError.TextSettings.FontColor := StrToColor(theme.onError);
  errorContainer.Fill.Color := StrToColor(theme.errorContainer);
  onErrorContainer.TextSettings.FontColor := StrToColor(theme.onErrorContainer);

  background.Fill.Color := StrToColor(theme.background);
  onBackground.TextSettings.FontColor := StrToColor(theme.onBackground);

  outline.Stroke.Color := StrToColor(theme.outline);
  outlineVariant.Stroke.Color := StrToColor(theme.outlineVariant);

  surface.Fill.Color := StrToColor(theme.surface);
  onSurface.TextSettings.FontColor := StrToColor(theme.onSurface);
  inverseSurface.Fill.Color := StrToColor(theme.inverseSurface);
  inverseOnSurface.TextSettings.FontColor := StrToColor(theme.inverseOnSurface);
  surfaceVariant.Fill.Color := StrToColor(theme.surfaceVariant);
  onSurfaceVariant.TextSettings.FontColor := StrToColor(theme.onSurfaceVariant);

  surfaceContainerHighest.Fill.Color := StrToColor(theme.surfaceContainerHighest);
  surfaceContainerHigh.Fill.Color := StrToColor(theme.surfaceContainerHigh);
  surfaceContainer.Fill.Color := StrToColor(theme.surfaceContainer);
  surfaceContainerLow.Fill.Color := StrToColor(theme.surfaceContainerLow);
  surfaceContainerLowest.Fill.Color := StrToColor(theme.surfaceContainerLowest);

  surfaceDim.Fill.Color := StrToColor(theme.surfaceDim);
  surfaceBright.Fill.Color := StrToColor(theme.surfaceBright);



end;

end.
