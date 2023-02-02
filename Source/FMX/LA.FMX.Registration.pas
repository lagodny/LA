unit LA.FMX.Registration;

interface

procedure Register;

implementation

uses
  System.Classes,
  LA.FMX.Objects,
  LA.FMX.StdCtrls;

procedure Register;
begin
  RegisterComponents('LA Controls', [
    TLAText,
    TLALabel
    ]);
end;

end.
