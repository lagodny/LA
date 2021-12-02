unit LA.Data.Link.Tracker;

interface

uses
  LA.Data.Link, LA.Data.Tracker;

type
  TDCTrackerLink = class(TDCLink)
  private
    FObserver: TDCTracker;
  public
    constructor Create(aObserver: TDCTracker);

    function GetID: string; override;
    procedure Notify; override;
  end;


implementation

{ TDCTrackerLink }

constructor TDCTrackerLink.Create(aObserver: TDCTracker);
begin
  FObserver := aObserver;
end;

function TDCTrackerLink.GetID: string;
begin
  Result := FObserver.ID;
end;

procedure TDCTrackerLink.Notify;
begin
  inherited;
  FObserver.SetData(Data);
end;

end.
