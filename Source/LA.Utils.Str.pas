unit LA.Utils.Str;

interface

uses
  System.Classes;

type
  TDCStrUtils = class
  public
    class function StrToHex(const aStr: string; const aDelimiter: string = ' '): string; overload;
    class function StrToHex(const Str: RawByteString; const aDelimiter: string = ' '): string; overload;
  end;


implementation

uses
  System.SysUtils;


{ TLAStrUtils }

class function TDCStrUtils.StrToHex(const aStr, aDelimiter: string): string;
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

class function TDCStrUtils.StrToHex(const Str: RawByteString; const aDelimiter: string): string;
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

end.
