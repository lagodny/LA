unit LA.FMX.Editors.Registration;

interface

uses
  System.Classes,
  DesignEditors, DesignIntf;

procedure Register;

implementation

uses
  LA.FMX.PropLink,
  LA.FMX.Editors;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TLAPropLink, 'PropertyName', TLAPropLinkPropertyName);
  RegisterPropertyEditor(TypeInfo(string), TLAColorKeyPropLink, 'PropertyName', TLAColorKeyPropLinkPropertyName);
  RegisterPropertyEditor(TypeInfo(string), TLAFloatKeyPropLink, 'PropertyName', TLAFloatKeyPropLinkPropertyName);
end;

end.
