unit LA.Tests.Data.Updater;

interface

uses
  DUnitX.TestFramework,
  LA.Data.Source,
  LA.Data.Updater,
  LA.Data.Link.Sensor;

type
  TCrackLADataUpdater = class(TLADataUpdater);

  [TestFixture]
  TTest_TLADataUpdater = class
  private
    CUT: TCrackLADataUpdater;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAddLinksToSortedList;
//    [Test]
    procedure TestProcessServerResponse;
//    [Test]
    procedure TestStartStopThread;

    procedure TestUpdater;

  end;

implementation

uses
  System.SysUtils;

//type
//  TMockLink = class(TLADataLink)
//  private
//    FID: string;
//    function GetID: string; override;
//  public
//    property ID: string read GetID write FID;
//  end;

procedure TTest_TLADataUpdater.Setup;
begin
  CUT := TCrackLADataUpdater.Create(nil);
end;

procedure TTest_TLADataUpdater.TearDown;
begin
  CUT.Free;
end;

procedure TTest_TLADataUpdater.TestAddLinksToSortedList;
const
  cExpectedCount = 100000;
var
  i: Integer;
  aLink: TLADataLink;
begin
  // добавляем линки со случайным ID
  Randomize;
  for i := 1 to cExpectedCount do
  begin
    aLink := TLADataLink.Create(nil);
    aLink.ID := Random(cExpectedCount).ToString;
    CUT.Attach(aLink);
  end;

  // проверяем, что все добавились
  Assert.AreEqual(cExpectedCount, CUT.Links.Count);

  // проверяем что они в порядке возрастания ID
  for i := 1 to CUT.Links.Count - 1 do
    Assert.IsTrue(CUT.Links[i].ID >= CUT.Links[i-1].ID);

end;

procedure TTest_TLADataUpdater.TestProcessServerResponse;
const
  cExpectedCount = 3;
  cData0 = ';;not found;';
  cData1 = '1;1;;2022-12-02 00:00:00';
  cData2 = '2;2;;2022-12-02 00:00:00';
var
  aLink: TLASensorLink;
  aList: TLADataLinkList;
begin
  aList := TLADataLinkList.Create(True);
  try

    aLink := TLASensorLink.Create(nil);
    aLink.ID := '2';
    CUT.Attach(aLink);
    aList.Add(aLink);

    aLink := TLASensorLink.Create(nil);
    aLink.ID := '1';
    CUT.Attach(aLink);
    aList.Add(aLink);

    aLink := TLASensorLink.Create(nil);
    aLink.ID := '';
    CUT.Attach(aLink);
    aList.Add(aLink);

    CUT.ProcessServerResponse(
      cData0 + #13 +
      cData1 + #13 +
      cData2 + #13
    );

    Assert.AreEqual(cExpectedCount, CUT.Links.Count);

    Assert.IsTrue(CUT.Links[0].Data = '');      // линки без ID не обрабатываем
    Assert.IsTrue(CUT.Links[1].Data = cData1);
    Assert.IsTrue(CUT.Links[2].Data = cData2);
  finally
    aList.Free;
  end;
end;

procedure TTest_TLADataUpdater.TestStartStopThread;
begin
  CUT.Active := True;
  CUT.Active := False;
end;

procedure TTest_TLADataUpdater.TestUpdater;
begin

end;


initialization
  TDUnitX.RegisterTestFixture(TTest_TLADataUpdater);

end.
