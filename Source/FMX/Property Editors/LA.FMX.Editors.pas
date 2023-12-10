unit LA.FMX.Editors;

interface

uses
  System.Classes,
  DesignEditors, DesignIntf,
//  System.UITypes, System.UIConsts,
  System.Generics.Collections,
  FMX.Types, System.TypInfo,
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




implementation

uses
  System.UITypes,
  System.Rtti;

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

end.
