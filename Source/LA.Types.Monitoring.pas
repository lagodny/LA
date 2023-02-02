unit LA.Types.Monitoring;

interface

uses
  System.SysUtils;

type
  TDataRec = record
    t: Int64;
    v: string;
  end;

  TDataRecExt = record
    SID: string;
    t: Int64;
    v: string;
    e: string;
  end;

  THistoryRec = record
    t: Int64;
    v: string;
  end;

  THistoryRecExt = record
    t: Int64;
    v: string;
    e: string;
  end;

type
  TIDArr = array of Int64;
  TSIDArr = array of string;

  TValArr = array of string;
  TDataRecArr = array of TDataRec;
  TDataRecExtArr = array of TDataRecExt;

  THistoryRecArr = array of THistoryRec;
  THistoryRecExtArr = array of THistoryRecExt;

function SID2IDArr(aSIDArr: TSIDArr): TIDArr;


implementation

function SID2IDArr(aSIDArr: TSIDArr): TIDArr;
begin
  SetLength(Result, Length(aSIDArr));
  for var i := Low(aSIDArr) to High(aSIDArr) do
      Result[i] := StrToInt(aSIDArr[i]);
end;

end.
