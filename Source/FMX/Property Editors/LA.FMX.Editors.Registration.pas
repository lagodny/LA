unit LA.FMX.Editors.Registration;

interface

uses
  System.Classes, System.UITypes,
  FMX.Types,
  DesignEditors, DesignIntf;

procedure Register;

implementation

uses
  LA.FMX.PropLink,
  LA.FMX.Editors,
  LA.FMX.StdCtrls;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TLAPropLink, 'PropertyName', TLAPropLinkPropertyName);
  RegisterPropertyEditor(TypeInfo(string), TLAColorKeyPropLink, 'PropertyName', TLAColorKeyPropLinkPropertyName);
  RegisterPropertyEditor(TypeInfo(string), TLAFloatKeyPropLink, 'PropertyName', TLAFloatKeyPropLinkPropertyName);

  RegisterPropertyEditor(TypeInfo(TAlphaColor), TFmxObject, '', TLAAlphaColorProperty);
  RegisterPropertyEditor(TypeInfo(TAlphaColor), TPersistent, '', TLAAlphaColorProperty);
end;

end.
