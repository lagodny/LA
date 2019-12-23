unit LA.DC.StrUtils;

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
  for I := 0 to Length(b) - 1 do
    Result := Result + IntToHex(b[i], 2) + aDelimiter;
end;

class function TDCStrUtils.StrToHex(const Str: RawByteString; const aDelimiter: string): string;
var
  i: Integer;
  b: TBytes;
begin
  Result := '';
  b := BytesOf(Str);
  for I := 0 to Length(b) - 1 do
    Result := Result + IntToHex(b[i], 2) + aDelimiter;
end;

end.
