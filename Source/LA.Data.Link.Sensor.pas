unit LA.Data.Link.Sensor;

interface

uses
  LA.Data.Link, LA.Data.Sensor;

type
  /// <summary>
  ///   Линк/адаптер к Датчику
  /// </summary>
  TLASensorLink = class(TLALink)
  private
    FObserver: TLASensor;
  public
    constructor Create(aObserver: TLASensor);

    function GetID: string; override;
    procedure Notify; override;
  end;


implementation

{ TDCSensorLink }

constructor TLASensorLink.Create(aObserver: TLASensor);
begin
  FObserver := aObserver;
end;

function TLASensorLink.GetID: string;
begin
  Result := FObserver.GetID;
end;

procedure TLASensorLink.Notify;
begin
  inherited;
  FObserver.SetData(Data);
end;

end.
