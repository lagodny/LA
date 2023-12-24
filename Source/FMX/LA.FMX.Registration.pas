unit LA.FMX.Registration;

interface

procedure Register;

implementation

uses
  System.Classes,
  LA.FMX.Objects,
  LA.FMX.StdCtrls,
  LA.FMX.Chart, LA.FMX.Chart.LineSeries,
  LA.FMX.PropLink,
  LA.FMX.Shapes;

procedure Register;
begin
  RegisterComponents('LA Controls', [
    TLAText,
    TLAGlyph,
    TLALabel,
    TLAChart,
    TLAPropLink, TLAColorKeyPropLink, TLAFloatKeyPropLink,
    TLAColorKeyLine
    ]);

//  RegisterNoIcon([TLAPropLink, TLAColorKeyPropLink]);
end;

end.
