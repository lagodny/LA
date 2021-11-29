unit LA.Types.Monitoring;

interface

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


implementation

end.
