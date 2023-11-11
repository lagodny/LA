unit LA.FMX.StdCtrls;

interface

uses
  System.Classes,
  FMX.StdCtrls,
  LA.Data.Link.Sensor, LA.Data.Link.Sensor.Intf,
  LA.Data.Source;

type
  TLALabel = class(TLabel, ILASensorLink)
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
    property Link: TLASensorLink read FLink write SetLink implements ILASensorLink;
  end;


implementation

{ TLALabel }

constructor TLALabel.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TLASensorLink.Create(Self);
  FLink.OnOwnerNotify := DoDataLinkChanged;
end;

destructor TLALabel.Destroy;
begin
  FLink.Free;
  FLink := nil;
  inherited;
end;

procedure TLALabel.DoDataLinkChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  Text := Link.Text;
end;

procedure TLALabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(Link) then
    Link.Notification(AComponent, Operation);
end;

procedure TLALabel.SetLink(const Value: TLASensorLink);
begin
  FLink.Assign(Value);
end;

end.
