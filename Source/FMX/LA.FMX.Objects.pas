unit LA.FMX.Objects;

interface

uses
  System.Classes,
  FMX.Objects,
  LA.Data.Link.Sensor, LA.Data.Link.Sensor.Intf,
  LA.Data.Source;

type
  TLAText = class(TText, ILASensorLink)
  private
    FLink: TLASensorLink;
    procedure SetLink(const Value: TLASensorLink);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoDataLinkChanged(Sender: TObject);
  published
    property Link: TLASensorLink read FLink  write SetLink implements ILASensorLink;
  end;

implementation

{ TLAText }

constructor TLAText.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.OnOwnerNotify := DoDataLinkChanged;
end;

destructor TLAText.Destroy;
begin
  FLink.Free;
  FLink := nil;
  inherited;
end;

procedure TLAText.DoDataLinkChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  Text := Link.Text;
end;

procedure TLAText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLAText.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

end.
