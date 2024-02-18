unit LA.FMX.UI.Consts;

interface

uses
  System.UITypes, System.Classes;

// https://stackoverflow.com/questions/72125688/register-and-use-my-own-colortoident-identtocolor
procedure LAGetAlphaColorValues(Proc: TGetStrProc);
function LAAlphaColorToString(Value: TAlphaColor): string;
function LAStringToAlphaColor(const Value: string): TAlphaColor;
function LAAlphaColorToIdent(Color: Integer; var Ident: string): Boolean;
function LAIdentToAlphaColor(const Ident: string; var Color: Integer): Boolean;
procedure LARegisterAlphaColorIntegerConsts;

procedure InitLAColorsFromJSON(const aJSON: string; aDark: Boolean = false);
procedure InitLAColorsFromFile(const aFileName: string; aDark: Boolean = false);


implementation

uses
  System.UIConsts,
  System.SysUtils, System.IOUtils,
  SynCrossPlatformJSON;

var
  // https://m3.material.io/styles/color/roles
  // https://developer.android.com/reference/kotlin/androidx/compose/material3/ColorScheme
  // Source color = #264361 (Matherial theme builder)
  LAAlphaColors: array [0..48] of TIdentMapEntry = (
    (Value: Integer($FFF8F9FE); Name: 'clabackground'),         // The background color that appears behind scrollable content.
    (Value: Integer($FFBA1A1A); Name: 'claerror'),              // The error color is used to indicate errors in components, such as invalid text in a text field.
    (Value: Integer($FFFFDAD6); Name: 'claerrorContainer'),     // The preferred tonal color of error containers.
    (Value: Integer($FFEFF0F7); Name: 'clainverseOnSurface'),   // A color that contrasts well with inverseSurface.
    (Value: Integer($FF9FCAFD); Name: 'clainversePrimary'),     // Color to be used as a "primary" color in places where the inverse color scheme is needed, such as the button on a SnackBar.
    (Value: Integer($FF2E3135); Name: 'clainverseSurface'),     // A color that contrasts sharply with surface.
    (Value: Integer($FF191C20); Name: 'claonBackground'),       // Color used for text and icons displayed on top of the background color.
    (Value: Integer($FFFFFFFF); Name: 'claonError'),            // Color used for text and icons displayed on top of the error color.
    (Value: Integer($FF410002); Name: 'claonErrorContainer'),   // The color (and state variants) that should be used for content on top of errorContainer.
    (Value: Integer($FFFFFFFE); Name: 'claonPrimary'),          // Color used for text and icons displayed on top of the primary color.
    (Value: Integer($FF001D35); Name: 'claonPrimaryContainer'), // The color (and state variants) that should be used for content on top of primaryContainer.
    (Value: Integer($FF001D36); Name: 'claonPrimaryFixed'),
    (Value: Integer($FF174974); Name: 'claonPrimaryFixedVariant'),
    (Value: Integer($FFFFFFFD); Name: 'claonSecondary'),        // Color used for text and icons displayed on top of the secondary color.
    (Value: Integer($FF0F1C2B); Name: 'claonSecondaryContainer'),  // The color (and state variants) that should be used for content on top of secondaryContainer.
    (Value: Integer($FF0F1C29); Name: 'claonSecondaryFixed'),
    (Value: Integer($FF3B4858); Name: 'claonSecondaryFixedVariant'),
    (Value: Integer($FF191C21); Name: 'claonSurface'),          // Color used for text and icons displayed on top of the surface color.
    (Value: Integer($FF42474E); Name: 'claonSurfaceVariant'),   // The color (and state variants) that can be used for content on top of surface.
    (Value: Integer($FFFFFFFC); Name: 'claonTertiary'),         // Color used for text and icons displayed on top of the tertiary color.
    (Value: Integer($FF251432); Name: 'claonTertiaryContainer'),// The color (and state variants) that should be used for content on top of tertiaryContainer.
    (Value: Integer($FF251433); Name: 'claonTertiaryFixed'),
    (Value: Integer($FF524060); Name: 'claonTertiaryFixedVariant'),
    (Value: Integer($FF73777F); Name: 'claoutline'),            // Subtle color used for boundaries.
    (Value: Integer($FFC3C7CF); Name: 'claoutlineVariant'),     // Utility color used for boundaries for decorative elements when strong contrast is not required.
    (Value: Integer($FF35618E); Name: 'claprimary'),            // The primary color is the color displayed most frequently across your app’s screens and components.
    (Value: Integer($FFD1E4FF); Name: 'claprimaryContainer'),   // The preferred tonal color of containers.
    (Value: Integer($FFD1E4FE); Name: 'claprimaryFixed'),
    (Value: Integer($FF9FCAFC); Name: 'claprimaryFixedDim'),
    (Value: Integer($FF000100); Name: 'clascrim'),              // Color of a scrim that obscures content.
    (Value: Integer($FF535F70); Name: 'clasecondary'),          // The secondary color provides more ways to accent and distinguish your product.
    (Value: Integer($FFD6E4F7); Name: 'clasecondaryContainer'), // A tonal color to be used in containers.
    (Value: Integer($FFD6E4F8); Name: 'clasecondaryFixed'),
    (Value: Integer($FFBAC8DB); Name: 'clasecondaryFixedDim'),
    (Value: Integer($FF000001); Name: 'clashadow'),
    (Value: Integer($FFF8F9FF); Name: 'clasurface'),            // The surface color that affect surfaces of components, such as cards, sheets, and menus.
    (Value: Integer($FFF8F9FD); Name: 'clasurfaceBright'),      // A surface variant that is always brighter than surface, whether in light or dark mode.
    (Value: Integer($FFECEEF4); Name: 'clasurfaceContainer'),   // A surface variant that affects containers of components, such as cards, sheets, and menus.
    (Value: Integer($FFE6E8EE); Name: 'clasurfaceContainerHigh'),
    (Value: Integer($FFE1E2E8); Name: 'clasurfaceContainerHighest'),
    (Value: Integer($FFF2F3F9); Name: 'clasurfaceContainerLow'),
    (Value: Integer($FFFFFFFB); Name: 'clasurfaceContainerLowest'),
    (Value: Integer($FFD8DAE0); Name: 'clasurfaceDim'),         // A surface variant that is always dimmer than surface, whether in light or dark mode.
    (Value: Integer($FF35618F); Name: 'clasurfaceTint'),
    (Value: Integer($FFDFE2EB); Name: 'clasurfaceVariant'),     // Another option for a color with similar uses of surface.
    (Value: Integer($FF6A5778); Name: 'clatertiary'),
    (Value: Integer($FFF2DAFF); Name: 'clatertiaryContainer'),
    (Value: Integer($FFF2DAFE); Name: 'clatertiaryFixed'),
    (Value: Integer($FFD6BEE5); Name: 'clatertiaryFixedDim')
  );

procedure InitLAColorsFromJSON(const aJSON: string; aDark: Boolean = false);
var
  v, schemes, theme: Variant;
  aName, aValueStr: string;
  vd: TJSONVariantData;
  function JSONColorToInt(const aStr: string): Integer;
  begin
    Result := StrToInt(StringReplace(aStr, '#', '$FF', []));
  end;
begin
  v := JSONVariant(aJSON);
  schemes := v.schemes;
  if aDark then
    theme := schemes.dark
  else
    theme := schemes.light;

  vd.Init(theme);

  for var i := Low(LAAlphaColors) to High(LAAlphaColors) do
  begin
    aName := LAAlphaColors[i].Name.SubString(3);
    aValueStr := vd.Value[aName];
    LAAlphaColors[i].Value := JSONColorToInt(aValueStr);
  end;
end;

procedure InitLAColorsFromFile(const aFileName: string; aDark: Boolean = false);
var
  aData: string;
begin
  aData := TFile.ReadAllText(aFileName, TEncoding.UTF8);
  InitLAColorsFromJSON(aData, aDark);
end;


{ TAlphaColor string functions }

procedure LAGetAlphaColorValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(LAAlphaColors) to High(LAAlphaColors) do
    Proc(LAAlphaColors[i].Name.Substring(3));
end;

function LAAlphaColorToString(Value: TAlphaColor): string;
begin
  LAAlphaColorToIdent(Integer(Value), Result);
  if Result.Chars[0] = 'x' then
    Result := '#' + Result.SubString(1)
  else
    Result := Result.Remove(0, 3);
end;

function LAStringToAlphaColor(const Value: string): TAlphaColor;
var
  LValue: string;
  LColor: Integer;
begin
  LValue := Value;
  if LValue = #0 then
    LValue := '$0'
  else if (LValue <> '') and ((LValue.Chars[0] = '#') or (LValue.Chars[0] = 'x')) then
    LValue := '$' + LValue.SubString(1);

  if (not LAIdentToAlphaColor('cla' + LValue, LColor)) and (not LAIdentToAlphaColor(LValue, LColor)) then
    Result := TAlphaColor(StrToInt64(LValue))
  else
    Result := TAlphaColor(LColor);
end;

var
  Int2ID: TIntToIdent;
  ID2Int: TIdentToInt;

function LAAlphaColorToIdent(Color: Integer; var Ident: string): Boolean;
begin
  Result := IntToIdent(Color, Ident, LAAlphaColors);
  if (not Result) and Assigned(Int2ID) then
    Result := Int2ID(Color, Ident);

  if not Result then
  begin
    Ident := 'x' + IntToHex(Color, 8);
    Result := True;
  end;
end;

function LAIdentToAlphaColor(const Ident: string; var Color: Integer): Boolean;
var
  LIdent: string;
begin
  LIdent := Ident;
  if (LIdent.Length > 0) and (LIdent.Chars[0] = 'x') then
  begin
    Color := Integer(LAStringToAlphaColor(LIdent));
    Result := True;
  end
  else
  begin
    Result := IdentToInt(LIdent, Color, LAAlphaColors);
    if (not Result) and Assigned(ID2Int) then
      Result := ID2Int(LIdent, Color);
  end;

  // Allow "clXXXX" constants and convert it to TAlphaColor
  if not Result and (LIdent.Length > 3) then
  begin
    LIdent := LIdent.Insert(2, 'a');
    Result := IdentToInt(LIdent, Integer(Color), LAAlphaColors);
  end;
end;

procedure LARegisterAlphaColorIntegerConsts;
begin
  Int2ID := FindIntToIdent(TypeInfo(TAlphaColor));
  ID2Int := FindIdentToInt(TypeInfo(TAlphaColor));
  RegisterIntegerConsts(TypeInfo(TAlphaColor), LAIdentToAlphaColor, LAAlphaColorToIdent);
//  UnregisterIntegerConsts(TypeInfo(TAlphaColor), System.UIConsts.IdentToAlphaColor, System.UIConsts.AlphaColorToIdent);
//  if not Assigned(FindIntToIdent(TypeInfo(TAlphaColor))) then
//    RegisterIntegerConsts(TypeInfo(TAlphaColor), LAIdentToAlphaColor, LAAlphaColorToIdent);
end;

procedure LAUnRegisterAlphaColorIntegerConsts;
begin
  UnregisterIntegerConsts(TypeInfo(TAlphaColor), LAIdentToAlphaColor, LAAlphaColorToIdent);
end;


initialization
  LARegisterAlphaColorIntegerConsts;

finalization
  LAUnRegisterAlphaColorIntegerConsts;


end.
