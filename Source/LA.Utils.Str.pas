unit LA.Utils.Str;

interface

uses
  System.Classes,
  System.SysUtils, System.StrUtils;

type
  TLAStrUtils = class
  public
    class function StrToHex(const aStr: string; const aDelimiter: string = ' '): string; overload;
    class function StrToHex(const Str: RawByteString; const aDelimiter: string = ' '): string; overload;
    class function DotStrToFloat(const aStr: string): Double;
    class function DotStrToFloatDef(const aStr: string; const aDefault: Double = 0): Double;

    class function IntToBin(const IValue: UInt64; NumBits: Word = 0): string;
    class function FormatValue(aValue: Double; const aFormatString: string): string;

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

class function TLAStrUtils.FormatValue(aValue: Double; const aFormatString: string): string;
const
  cDividerSymbol = ';';
  cDateString = 'DATE';
  cHexString = 'HEX';
  cBinString = 'BIN';
var
  aFormat    : string;
  aDividerPos: integer;
  aDigits: Integer;
begin
  if SameText(Copy(aFormatString, 1, 4), cDateString) then
  begin
    // формат начинается с 'DATE'
    // FormatDateTime не умеет обрабатывать значения >0, =0, <0
    // сделаем это сами
    aFormat := Copy(aFormatString, Length(cDateString)+1, length(aFormatString));
    aDividerPos := Pos(cDividerSymbol, aFormat);
    if (aDividerPos > 0) then
    begin
      if aValue > 0 then
        aFormat := Copy(aFormat,1,aDividerPos-1)
      else
      begin
        aFormat := Copy(aFormat, aDividerPos+1, Length(aFormat) - aDividerPos);
        aDividerPos := Pos(cDividerSymbol, aFormat);
        if aDividerPos > 0 then
        begin
          if aValue = 0 then
            aFormat := Copy(aFormat,1,aDividerPos-1)
          else
            aFormat := Copy(aFormat, aDividerPos+1, Length(aFormat) - aDividerPos);
        end;
      end;
    end;
    Result := FormatDateTime(aFormat,aValue);
  end
  else if SameText(Copy(aFormatString, 1, Length(cHexString)), cHexString) then
  begin
    // HEX4 - отобразить целую часть числа в HEX с 4 знаками
    aFormat := Copy(aFormatString, Length(cHexString)+1, length(aFormatString));
    aDigits := StrToIntDef(aFormat, 4);
    Result := '0x'+IntToHex(Trunc(aValue), aDigits);
  end
  else if SameText(Copy(aFormatString, 1, Length(cBinString)), cBinString) then
  begin
    // BIN16 - отобразить целую часть числа в Двоичной ситсеме с 16 знаками
    aFormat := Copy(aFormatString, Length(cBinString)+1, length(aFormatString));
    aDigits := StrToIntDef(aFormat, 0);
    Result := '0b'+IntToBin(Trunc(aValue), aDigits);
  end
  else
    Result := FormatFloat(aFormatString, aValue);
end;

class function TLAStrUtils.IntToBin(const IValue: UInt64; NumBits: Word): string;
var
  i: Integer;
  aValue: UInt64;
  RetVar: string;
begin
  i := 0;
  RetVar := '';
  aValue := IValue;

  while True do
  begin
    RetVar := char(48 + (aValue and 1)) + RetVar;
    aValue := aValue shr 1;
    Inc(i);

    // выходим, если задано количество символов и мы его достигли
    if (NumBits > 0) then
    begin
      if(i >= NumBits) then
        Break
    end
    // или количество символов не задано (=0) но данных больше нет:
    //  - получаем сокращенную строку (старшие (левые) нули отсекаются)
    else if aValue = 0 then
      Break;

  end;

  if RetVar = '' then
    RetVar := '0';
  Result := RetVar;
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
  dotFS.ThousandSeparator := #0;

finalization

end.
