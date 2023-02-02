unit LA.Net.Connector.Utils;

interface

uses
  System.SysUtils,
  SynCrossPlatformREST,
  LA.Types.Monitoring;

type
  TLANetConnectorUtils = class
  public
    class function SIDArr2IDDynArr(const aSIDArr: TSIDArr): TIDDynArray;
    class function ID2Ident(const aID: TID): string; overload;
    class function ID2Ident(const aID: string): string; overload;
  end;

implementation

{ TLANetConnectorUtils }

class function TLANetConnectorUtils.ID2Ident(const aID: TID): string;
begin
  Result := '_' + IntToStr(aID);
end;

class function TLANetConnectorUtils.ID2Ident(const aID: string): string;
begin
  Result := '_' + aID;
end;

class function TLANetConnectorUtils.SIDArr2IDDynArr(const aSIDArr: TSIDArr): TIDDynArray;
begin
  SetLength(Result, Length(aSIDArr));
  for var i := Low(aSIDArr) to High(aSIDArr) do
    Result[i] := StrToInt64(aSIDArr[i]);
end;

end.
