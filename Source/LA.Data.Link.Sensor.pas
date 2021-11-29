unit LA.Data.Link.Sensor;

interface

uses
  LA.Data.Link, LA.Data.Sensor;

type
  /// <summary>
  ///   Линк/адаптер к Датчику
  /// </summary>
  TDCSensorLink = class(TDCLink)
  private
    FObserver: TDCSensor;
  public
    constructor Create(aObserver: TDCSensor);

    function GetID: string; override;
    procedure Notify; override;
  end;


implementation

{ TDCSensorLink }

constructor TDCSensorLink.Create(aObserver: TDCSensor);
begin
  FObserver := aObserver;
end;

function TDCSensorLink.GetID: string;
begin
  Result := FObserver.GetID;
end;

procedure TDCSensorLink.Notify;
begin
  FObserver.SetData(Data);
end;

end.
