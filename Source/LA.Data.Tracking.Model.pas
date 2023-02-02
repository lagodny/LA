unit LA.Data.Tracking.Model;

interface

uses
  System.SysUtils,
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
  TDevice = class
  private
    {$REGION 'Fields'}
    FID: Integer;
    FName: string;
    FGroup: TGroup;
    FLink: TLATrackerLink;
    {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(v: Variant; aPrototypes: TLATagPrototypes);

    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Group: TGroup read FGroup write FGroup;

    property Link: TLATrackerLink read FLink;
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

procedure TDevice.Init(v: Variant; aPrototypes: TLATagPrototypes);
begin
  FID := v.id;
  FName := v.name;
  Link.FromJSON(v, aPrototypes);
end;

{ TClient }

procedure TClient.Init(v: Variant);
begin
  FID := v.id;
  FName := v.name;
end;

end.
