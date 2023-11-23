unit LA.Data.Link.Tracker;

interface

uses
  System.Classes, System.Variants,
  System.Generics.Defaults, System.Generics.Collections,
  System.SysUtils, System.DateUtils,
  System.Math,
  LA.Types.Tracker,
  LA.Data.Source;

type
  /// <summary>
  ///   Линк/адаптер к Трекеру
  /// </summary>
  TLATrackerLink = class(TLADataLink)
  private
    {$REGION 'Fields'}
    FName: string;
    FTime: TDateTime;
    FStatus: string;
    FTags: TLATags;
    procedure SetName(const Value: string);
    procedure SetTime(const Value: TDateTime);
    procedure SetStatus(const Value: string);
    function GetLat: Double;
    function GetLon: Double;
    function GetSpeed: Double;
    function GetFL: Double;
    function GetCourse: Double;
    {$ENDREGION}
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure EncodeData; override;
  public
    constructor Create(const AOwner: TPersistent); override;
    destructor Destroy; override;

    function TagValueByName(const aName: string; aDefault: Variant): Variant;

    procedure FromJSON(const aJSON: string; aTagPrototypes: TLATagPrototypes);

    property Tags: TLATags read FTags;
  published
    /// наименование трекера
    property Name: string read FName write SetName;
    property Status: string read FStatus write SetStatus;
    property Time: TDateTime read FTime write SetTime;
    // быстрый доступ к значениям тегов
    property Lat: Double read GetLat;
    property Lon: Double read GetLon;
    property Speed: Double read GetSpeed;
    property Course: Double read GetCourse;
    property FL: Double read GetFL;
  end;
  TLATrackerLinks = TObjectDictionary<string, TLATrackerLink>;

implementation

uses
  SynCrossPlatformJSON;

{ TLATrackerLink }

procedure TLATrackerLink.AssignTo(Dest: TPersistent);
var
  aDestTag: TLATag;
begin
  if Dest is TLATrackerLink then
  begin
    inherited;
    var aDest: TLATrackerLink := TLATrackerLink(Dest);

    aDest.Name := Name;
    aDest.Time := Time;
    aDest.Status := Status;

    aDest.Tags.Clear;
    for var aItem in Tags do
    begin
      aDestTag := TLATag.Create;

      aDestTag.Proto := aItem.Value.Proto;

      aDestTag.SID := aItem.Value.SID;
      aDestTag.Addr := aItem.Value.Addr;

      aDestTag.Value := aItem.Value.Value;
//      aDestTag.Text := aItem.Value.Text;
      aDestTag.Status := aItem.Value.Status;

      aDest.Tags.Add(aItem.Key, aDestTag);
    end;
  end
  else
    inherited;
end;

constructor TLATrackerLink.Create(const AOwner: TPersistent);
begin
  inherited;
  FTags := TLATags.Create([doOwnsValues]);
end;

destructor TLATrackerLink.Destroy;
begin
  FTags.Free;
  inherited;
end;

procedure TLATrackerLink.EncodeData;
var
  aTag: TLATag;
  vd, vTags, vErrors: TJSONVariantData;
begin
  // сбросим признак необходимости декодировать данные
  inherited;
  try
    vd.Init(Data);
    if VarIsEmpty(vd.Value['time']) then
      Status := Data
    else
    begin
      Time := UnixToDateTime(vd.Value['time'], False);
      Status := vd.Value['status'];
      vTags.Init(vd.Value['tags']);
      vErrors.Init(vd.Value['errors']);
      for var i := 0 to vTags.Count - 1 do
      begin
        if Tags.TryGetValue(vTags.Names[i], aTag) then
        begin
          aTag.Value := vTags.Values[i];
          aTag.Status := vErrors.Value[aTag.SID];
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      Status := e.Message;
    end;
  end;
end;

(*
{
  "id": 3476,
  "name": "N  2",
  "group": 0,
  "tags": {
    "Height": {
      "sid": "Height",
      "addr": "48670",
      "proto": 600
    },
    "Lon": {
      "sid": "Lon",
      "addr": "48669",
      "proto": 599
    },
    "Ignition": {
      "sid": "Ignition",
      "addr": "48678",
      "proto": 604
    },
    "Curs": {
      "sid": "Curs",
      "addr": "48672",
      "proto": 602
    },
    "ExternalVoltage": {
      "sid": "ExternalVoltage",
      "addr": "48679",
      "proto": 621
    },
    "Distance": {
      "sid": "Distance",
      "addr": "48675",
      "proto": 603
    },
    "DistanceRace": {
      "sid": "DistanceRace",
      "addr": "48677",
      "proto": 624
    },
    "DistanceTO": {
      "sid": "DistanceTO",
      "addr": "48676",
      "proto": 623
    },
    "Speed": {
      "sid": "Speed",
      "addr": "48671",
      "proto": 601
    },
    "Lat": {
      "sid": "Lat",
      "addr": "48668",
      "proto": 598
    }
  }
}
*)
procedure TLATrackerLink.FromJSON(const aJSON: string; aTagPrototypes: TLATagPrototypes);
var
  aTag: TLATag;
  vd, vTags: TJSONVariantData;
begin
  // параметры трекера
  vd.Init(aJSON);
  ID := vd.Value['id'];
  Name := vd.Value['name'];
  // теги трекера
  Tags.Clear;
  vTags.Init(vd.Value['tags']);
  for var i := 0 to vTags.Count - 1 do
  begin
    aTag := TLATag.Create;
    aTag.FromJson(vTags.Values[i], aTagPrototypes);
    Tags.Add(aTag.SID, aTag);
  end;
end;

function TLATrackerLink.GetCourse: Double;
begin
  Result := TagValueByName(TLATagNamesRec.Course, 0);
end;

function TLATrackerLink.GetFL: Double;
begin
  Result := TagValueByName(TLATagNamesRec.FuelLevel0, 0);
end;

function TLATrackerLink.GetLat: Double;
begin
  Result := TagValueByName(TLATagNamesRec.Lat, 0);
end;

function TLATrackerLink.GetLon: Double;
begin
  Result := TagValueByName(TLATagNamesRec.Lon, 0);
end;

function TLATrackerLink.GetSpeed: Double;
begin
  Result := TagValueByName(TLATagNamesRec.Speed, 0);
end;

procedure TLATrackerLink.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TLATrackerLink.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TLATrackerLink.SetTime(const Value: TDateTime);
begin
  FTime := Value;
end;

function TLATrackerLink.TagValueByName(const aName: string; aDefault: Variant): Variant;
var
  aTag: TLATag;
begin
  if Tags.TryGetValue(aName, aTag) then
    Result := aTag.Value
  else
    Result := aDefault;
end;

end.
