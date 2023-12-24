unit LA.FMX.Editors;

interface

uses
  System.Classes, System.TypInfo, System.Types,
  System.Generics.Collections,
  System.UITypes,
  DesignEditors, DesignIntf,
  VCLEditors,
//  System.UITypes, System.UIConsts,
  FMX.Types,
  FMX.Graphics,
  VCL.Graphics,
  FmxAnimationEditors,
//  FMX.Graphics, FMX.Ani,
//  VCLEditors, System.Types, Vcl.Graphics, Vcl.ComCtrls, Vcl.Controls, FMX.Design.Bitmap;
  LA.FMX.PropLink;

type

  TLAPropLinkPropertyName = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure GetValueList(List: TList<string>); virtual;
  end;

  TLAColorKeyPropLinkPropertyName = class(TLAPropLinkPropertyName)
  public
    procedure GetValueList(List: TList<string>); override;
  end;

  TLAFloatKeyPropLinkPropertyName = class(TLAPropLinkPropertyName)
  public
    procedure GetValueList(List: TList<string>); override;
  end;

  TLAAlphaColorProperty = class(TAlphaColorProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing, ICustomPropertyDrawing80, IProperty160)
  protected
    function TextToAlphaColor(const Value: string): TAlphaColor; override;
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    { ICustomPropertyListDrawing }
    //procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    //procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    { ICustomPropertyDrawing }
    //procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    //procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

  end;




implementation

uses
  System.SysUtils,
  System.Rtti,
  VCL.Dialogs, VCL.Controls, VCL.Forms,
  BrandingAPI,
  FmxDsnConst,
  FMX.Ani,
  FMXVclUtils,
  LA.FMX.UI.Consts;

procedure AddProperties(List: TList<string>; PropertyPrefix: string; RType: TRttiType;
  targetKind: TTypeKinds; targetType: TRttiType = nil); overload;
var
  LProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
begin
    LProperties := RType.GetProperties;
    if Length(LProperties) > 0 then
    begin
      for LProperty in LProperties do
      begin
        if (LProperty.PropertyType.TypeKind in targetKind) and
          (LProperty.Visibility = mvPublished) and
          ((targetType = nil) or (LProperty.PropertyType = targetType)) then
        begin
          if not List.Contains(PropertyPrefix + LProperty.Name) then
            List.Add(PropertyPrefix + LProperty.Name);
        end
        else
          // Only look one level deep
          if Length(PropertyPrefix) = 0 then
            AddProperties(List, LProperty.Name + '.', LProperty.PropertyType, targetKind, targetType);
      end;
    end;
end;

const
  FilmStripMargin: Integer = 2;
  FilmStripWidth: Integer = 12;
  FilmStripHeight: Integer = 13;

function PaintFilmStrip(const Value: string; ACanvas: TCanvas; const ARect: TRect;
  IsAnimated: Boolean): TRect;
var
  I, Right, Left, Top: Integer;
  OldPenColor, OldBrushColor: TColor;
  BorderColor, CellColor: TColor;
begin
  Left := ARect.Left + FilmStripMargin;
  Right := Left + FilmStripWidth;
  Top := ARect.Top +  Round((ARect.Bottom - ARect.Top - FilmStripHeight)/2);
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;

    Pen.Color := ACanvas.Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right + FilmStripMargin, ARect.Bottom);
    // frame things
    if IsAnimated then
    begin
      BorderColor := TColors.Black;
      CellColor := TColors.LtGray;
    end
    else
    begin
      BorderColor := TColors.LtGray;
      CellColor := TColors.White;
    end;

    Pen.Color := BorderColor;
    Rectangle(Left, Top, Right, Top + FilmStripHeight);
    for I := 0 to 2 do
    begin
      Rectangle(Left, Top + 2 + (4 * I), Right, Top + 5 + (4 * I));
    end;
    Rectangle(Left + 2, Top, Right - 2, Top + FilmStripHeight);

    Brush.Color := CellColor;
    Pen.Color := CellColor;
    Rectangle(Left + 3, Top, Right - 3, Top + FilmStripHeight);

    Pen.Color := BorderColor;
    Rectangle(Left + 2, Top + 3, Right - 2, Top + FilmStripHeight - 3);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    Result := Rect(Right + FilmStripMargin, ARect.Top, ARect.Right, ARect.Bottom);
  end;
end;



var
  VCLBitmap: Vcl.Graphics.TBitmap = nil;
  FMXBitmap: FMX.Graphics.TBitmap = nil;

function PaintColorBox(const Value: TAlphaColor; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean): TRect;
  overload;
const
  SquareSize = 3;
var
  Right, Size: Integer;
  NewRect: TRect;
  OldPenColor, OldBrushColor: TColor;
begin
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  // save off things
  OldBrushColor := ACanvas.Brush.Color;
  OldPenColor := ACanvas.Pen.Color;
  try
    Size := ((ARect.Height - 4) div SquareSize) * SquareSize + 2;
    NewRect := TRect.Create(ARect.TopLeft, Size, Size);
    NewRect.Offset(FilmStripMargin, (ARect.Height - NewRect.Height + 1) div 2);
    if FMXBitmap = nil then
      FMXBitmap := FMX.Graphics.TBitmap.Create(Size - 2, Size - 2)
    else
      FMXBitmap.SetSize(Size - 2, Size - 2);
    FMXBitmap.Clear(Value);
    CreatePreview(FMXBitmap, VCLBitmap, TRect.Create(TPoint.Create(0, 0), FMXBitmap.Width, FMXBitmap.Height), clBlack,
      clWhite, SquareSize, True);
    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);
    if ASelected then
      ACanvas.Pen.Color := clHighlightText
    else
      ACanvas.Pen.Color := clWindowText;
    ACanvas.Rectangle(NewRect);
    ACanvas.Draw(NewRect.Left + 1, NewRect.Top + 1, VCLBitmap);
  finally
    // restore the things we twiddled with
    ACanvas.Pen.Color := OldPenColor;
    ACanvas.Brush.Color := OldBrushColor;
    Result := Rect(Right, ARect.Top, ARect.Right, ARect.Bottom);
  end;
end;

function PaintColorBox(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean): TRect; overload;
var
  LValue: TAlphaColor;
begin
  LValue := LAStringToAlphaColor(Value);
  Result := PaintColorBox(LValue, ACanvas, ARect, ASelected);
end;



{ TLAPropLinkPropertyName }

function TLAPropLinkPropertyName.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TLAPropLinkPropertyName.GetValueList(List: TList<string>);
var
  LAni, LAniParent: TFmxObject;
  LContext: TRttiContext;
  LType: TRttiInstanceType;
begin
  LAni := GetComponent(0) as TFmxObject;
  if Assigned(LAni) then
  begin
    LAniParent := LAni.Parent;
    if Assigned(LAniParent) then
    begin
      LType := LContext.GetType(LAniParent.ClassType) as TRttiInstanceType;
      AddProperties(List, '', LType, [tkFloat, tkInteger, tkString, tkUString, tkLString, tkWString, tkVariant]);
    end;
  end;
end;

procedure TLAPropLinkPropertyName.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TList<string>;
begin
  Values := TList<string>.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TLAColorKeyPropLinkPropertyName }

procedure TLAColorKeyPropLinkPropertyName.GetValueList(List: TList<string>);
var
  LAni, LAniParent: TFmxObject;
  LContext: TRttiContext;
  LType, LTargetType: TRttiType;
begin
  LAni := GetComponent(0) as TFmxObject;
  if Assigned(LAni) then
  begin
    LAniParent := LAni.Parent;
    if Assigned(LAniParent) then
    begin
      LType := LContext.GetType(LAniParent.ClassType);
      LTargetType := LContext.GetType(TypeInfo(TAlphaColor));
      AddProperties(List, '', LType, [LTargetType.TypeKind], LTargetType);
    end;
  end;
end;

{ TLAFloatKeyPropLinkPropertyName }

procedure TLAFloatKeyPropLinkPropertyName.GetValueList(List: TList<string>);
var
  LAni, LAniParent: TFmxObject;
  LContext: TRttiContext;
  LType: TRttiInstanceType;
begin
  LAni := GetComponent(0) as TFmxObject;
  if Assigned(LAni) then
  begin
    LAniParent := LAni.Parent;
    if Assigned(LAniParent) then
    begin
      LType := LContext.GetType(LAniParent.ClassType) as TRttiInstanceType;
      AddProperties(List, '', LType, [tkFloat, tkInteger]);
    end;
  end;
end;

{ TLAAlphaColorProperty }

function TLAAlphaColorProperty.GetValue: string;
begin
  try
    Result := LAAlphaColorToString(TAlphaColor(GetOrdValue));
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TLAAlphaColorProperty.GetValues(Proc: TGetStrProc);
begin
  LAGetAlphaColorValues(Proc);
  inherited;
//  Proc(SCreateNewColorAnimation);
//  Proc(SCreateNewColorKeyAnimation);
end;

procedure TLAAlphaColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ValueRect: TRect;
  LControl: TWinControl;
begin
  if Screen.ActiveControl <> nil then
    LControl := Screen.ActiveControl
  else
    LControl := Application.MainForm;
  if (not SameText(Value, SCreateNewColorAnimation)) and
    (not SameText(Value, SCreateNewColorKeyAnimation)) then
    ValueRect := PaintColorBox(Value, ACanvas, Rect(ARect.Left + PropertyDrawingOffset,
    ARect.Top, ARect.Right, ARect.Bottom), ASelected)
  else
    ValueRect := PaintFilmStrip(Value, ACanvas, Rect(ARect.Left + PropertyDrawingOffset,
    ARect.Top, ARect.Right, ARect.Bottom), true);
  ValueRect.Left := ValueRect.Left + LControl.ScaleValue(TIDEThemeMetrics.AlignFactor);
  DefaultPropertyListDrawValue(Value, ACanvas, ValueRect, ASelected);
end;

procedure TLAAlphaColorProperty.SetValue(const Value: string);
begin
  if SameText(Value, SCreateNewColorAnimation) then
  begin
    CreateNewAnimation(TColorAnimation);
    Exit;
  end;
  if SameText(Value, SCreateNewColorKeyAnimation) then
  begin
    CreateNewAnimation(TColorKeyAnimation);
    Exit;
  end;

  try
    SetOrdValue(Integer(LAStringToAlphaColor(Value)));
    Modified;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

function TLAAlphaColorProperty.TextToAlphaColor(const Value: string): TAlphaColor;
begin
  Result := LAStringToAlphaColor(Value);
end;

end.
