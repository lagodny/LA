unit LA.Utils.Str;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TLAStrUtils = class
  public
    class function StrToHex(const aStr: string; const aDelimiter: string = ' '): string; overload;
    class function StrToHex(const Str: RawByteString; const aDelimiter: string = ' '): string; overload;
    class function DotStrToFloat(const aStr: string): Double;
    class function DotStrToFloatDef(const aStr: string; const aDefault: Double = 0): Double;
  end;

var
  dotFS: TFormatSettings;

implementation


{ TLAStrUtils }

class function TLAStrUtils.StrToHex(const aStr, aDelimiter: string): string;
var
  i: Integer;
  b: TBytes;
begin
  Result := '';
  b := TEncoding.ANSI.GetBytes(aStr);
  if Length(b) > 0 then
  begin
    Result := IntToHex(b[0], 2);
    for i := 1 to Length(b) - 1 do
      Result := Result + aDelimiter + IntToHex(b[i], 2);
  end;
end;

class function TLAStrUtils.DotStrToFloat(const aStr: string): Double;
begin
  Result := StrToFloat(aStr, dotFS);
end;

class function TLAStrUtils.DotStrToFloatDef(const aStr: string; const aDefault: Double): Double;
begin
  Result := StrToFloatDef(aStr, aDefault, dotFS);
end;

class function TLAStrUtils.StrToHex(const Str: RawByteString; const aDelimiter: string): string;
var
  i: Integer;
  b: TBytes;
begin
  Result := '';
  b := BytesOf(Str);
  if Length(b) > 0 then
  begin
    Result := IntToHex(b[0], 2);
    for i := 1 to Length(b) - 1 do
      Result := Result + aDelimiter + IntToHex(b[i], 2);
  end;
end;

initialization
  dotFS := TFormatSettings.Create;
  dotFS.DecimalSeparator := '.';

finalization

end.
