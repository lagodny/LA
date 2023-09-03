unit LA.Data.Tracking.Model;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Collections, System.Generics.Defaults,
  SynCrossPlatformJSON,
  LA.Types.Tracker,
  LA.Data.Link.Tracker;


type
  // клиент
  TClient = class
  private
    {$REGION 'Fields'}
    FName: string;
    FID: Integer;
    {$ENDREGION}
  public
    procedure Init(v: Variant);

    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
  end;
  TClients = TObjectList<TClient>;

  // группа
  TGroup = class
  private
    {$REGION 'Fields'}
    FName: string;
    FID: Integer;
    FGroup: TGroup;
    {$ENDREGION}
  public
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Group: TGroup read FGroup write FGroup;
  end;
  TGroups = TObjectDictionary<Integer, TGroup>;

  /// объект наблюдения
  TDevice = class(TPersistent)
  private
    {$REGION 'Fields'}
    FID: Integer;
    FName: string;
    FGroup: TGroup;
    FLink: TLATrackerLink;
    function GetFL: Double;
    function GetSpeed: Double;
    {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(v: Variant; aPrototypes: TLATagPrototypes);

    property Link: TLATrackerLink read FLink;
  published
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Group: TGroup read FGroup write FGroup;
    property Speed: Double read GetSpeed;
    property FL: Double read GetFL;
  end;
  TDevices = TObjectDictionary<Integer, TDevice>;


implementation

{ TDevice }

constructor TDevice.Create;
begin
  FLink := TLATrackerLink.Create(nil);
end;

destructor TDevice.Destroy;
begin
  FLink.Free;
  inherited;
end;

function TDevice.GetFL: Double;
begin
  Result := FLink.FL;
end;

function TDevice.GetSpeed: Double;
begin
  Result := FLink.Speed;
end;

procedure TDevice.Init(v: Variant; aPrototypes: TLATagPrototypes);
begin
  FID := v.id;
  FName := Trim(v.name);
  Link.FromJSON(v, aPrototypes);
end;

{ TClient }

procedure TClient.Init(v: Variant);
begin
  FID := v.id;
  FName := v.name;
end;

end.
