unit LA.Tests.Data.Updater;

interface

uses
  DUnitX.TestFramework,
  LA.Data.Updater, LA.Data.Link;

type
  [TestFixture]
  TTest_TDataUpdater = class
  private
    CUT: TDataUpdater;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAddLinksToSortedList;
    [Test]
    procedure TestStartStopThread;
  end;

implementation

type
  TMockLink = class(TDCLink)
  private
    FID: Int64;
    function GetID: Int64; override;
  public
    property ID: Int64 read GetID write FID;
  end;

procedure TTest_TDataUpdater.Setup;
begin
  CUT := TDataUpdater.Create(nil);
end;

procedure TTest_TDataUpdater.TearDown;
begin
  CUT.Free;
end;

procedure TTest_TDataUpdater.TestAddLinksToSortedList;
const
  cExpectedCount = 100000;
var
  i: Integer;
  aLink: TMockLink;
begin
  // добавляем линки со случайным ID
  Randomize;
  for i := 1 to cExpectedCount do
  begin
    aLink := TMockLink.Create;
    aLink.ID := Random(cExpectedCount);
    CUT.Attach(aLink);
  end;

  // проверяем, что все добавились
  Assert.AreEqual(cExpectedCount, CUT.Links.Count);

  // проверяем что они в порядке возрастания ID
  for i := 1 to CUT.Links.Count - 1 do
    Assert.IsTrue(CUT.Links[i].GetID >= CUT.Links[i-1].GetID);

end;

procedure TTest_TDataUpdater.TestStartStopThread;
begin
  CUT.Active := True;
  CUT.Active := False;
end;

{ TMockLink }

function TMockLink.GetID: Int64;
begin
  Result := FID;
end;

initialization
  TDUnitX.RegisterTestFixture(TTest_TDataUpdater);

end.
