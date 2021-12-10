unit LA.Data.Source;

interface

uses
  System.Classes,
  System.Generics.Collections, System.Generics.Defaults,
  System.SyncObjs,
  System.SysUtils,
  LA.Data.Source.Intf,
  LA.Data.Link;

type
  TLADataSource = class(TComponent, IDCObservable<TLALink>)
  private
    FLock: TMREWSync;
    FLinks: TObjectList<TLALink>;
    FLinksChanged: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // подключение, отключение, уведомление наблюдателей
    procedure Attach(const aLink: TLALink); virtual;
    procedure Detach(const aLink: TLALink); virtual;
    procedure Notify; virtual;

    procedure DetachObject(aObject: TObject);

  end;

implementation

{ TLADataSource }

procedure TLADataSource.Attach(const aLink: TLALink);
var
  aInsertIndex: Integer;
begin
  FLock.BeginWrite;
  try
    if FLinks.Count = 0 then
      FLinks.Add(aLink)
    else
    begin
      FLinks.BinarySearch(aLink, aInsertIndex);
      FLinks.Insert(aInsertIndex, aLink);
    end;

    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

constructor TLADataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TMREWSync.Create;

  FLinks := TObjectList<TLALink>.Create(TDelegatedComparer<TLALink>.Create(
    function (const aLeft, aRight: TLALink): Integer
    begin
      Result := CompareStr(aLeft.GetID, aRight.GetID);
    end)
  , True);
end;

destructor TLADataSource.Destroy;
begin
  FLinks.Free;
  FLock.Free;
  inherited;
end;

procedure TLADataSource.Detach(const aLink: TLALink);
begin
  FLock.BeginWrite;
  try
    FLinks.Remove(aLink);
    FLinksChanged := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TLADataSource.DetachObject(aObject: TObject);
begin
  FLock.BeginWrite;
  try
    for var i := FLinks.Count - 1 downto 0 do
    begin
      if FLinks[i].LinkedObject = aObject then
      begin
        FLinks.Delete(i);
        FLinksChanged := True;
      end;
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TLADataSource.Notify;
var
  aLink: TLALink;
begin
  FLock.BeginRead;
  try
    for aLink in FLinks do
      aLink.Notify;
  finally
    FLock.EndRead;
  end;
end;

end.
