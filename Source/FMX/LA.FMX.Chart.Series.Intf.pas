unit LA.FMX.Chart.Series.Intf;

interface

type
  ILASeries = interface
  ['{141E8E24-233B-45A7-83DF-15E9EC7C6239}']
    procedure FillData(aSkipIfNotPossible: Boolean = True);
//    procedure FillData;
  end;

implementation

end.
