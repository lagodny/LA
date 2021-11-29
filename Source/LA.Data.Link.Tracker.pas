unit LA.Data.Link.Tracker;

interface

uses
  LA.Data.Link, LA.Data.Sensor;

type
  TDCTrackerLink = class(TDCLink)
  private
    FObserver: TDCSensor;
  public
    constructor Create(aObserver: TDCSensor);

    function GetID: string; override;
    procedure Notify; override;
  end;


implementation

{ TDCTrackerLink }

constructor TDCTrackerLink.Create(aObserver: TDCSensor);
begin
  FObserver := aObserver;
end;

function TDCTrackerLink.GetID: string;
begin
  Result := FObserver.GetID;
end;

procedure TDCTrackerLink.Notify;
begin
  FObserver.SetData(Data);
end;

end.
