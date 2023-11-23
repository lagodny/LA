unit LA.Tests.Data.History.Viewer;

interface

uses
  DUnitX.TestFramework,
  LA.Data.Link.Sensor,
  LA.Net.Connector.Tcp, LA.Net.Connector.Http,
  LA.Data.History.Viewer;

type
  [TestFixture]
  TTestHistoryViewer = class
  private
    FLink1, FLink2: TLASensorLink;
    FConnector: TLAHttpConnector;
    FHistoryViewer: TLAHistoryViewer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAttachLink;
    [Test]
    procedure TestFillHistory;


//    [TestCase('TestTryConnectArrangeAddr','host1:1;https://dc.tdc.org.ua,https://dc.tdc.org.ua;host1:1;')]
//    [TestCase('TestTryConnectArrangeAddr','https://dc.tdc.org.ua;host1:1,https://dc.tdc.org.ua;host1:1')]
//    procedure TestTryConnectArrangeAddr(const aAddrIn, aAddrOut: string);

  end;


implementation

uses
  System.DateUtils,
  La.Tests.Net.Consts;


{ TTestHistoryViewer }

procedure TTestHistoryViewer.Setup;
begin
  FConnector := TLAHttpConnector.Create(nil);
  FConnector.UserName := cUserName;
  FConnector.Password := cPassword;
  FConnector.Address := cHttpAddr;

  FLink1 := TLASensorLink.Create(nil);
  FLink1.ID := '4';
  FLink1.DisplayFormat := '# ##0';
  FLink1.Kind := skAnalog;

  FLink2 := TLASensorLink.Create(nil);
  FLink2.ID := '1';
  FLink2.DisplayFormat := 'datedd.mm.yyyy hh:mm:ss';
  FLink2.Kind := skCounterUp;

  FHistoryViewer := TLAHistoryViewer.Create(nil);
  FHistoryViewer.Connector := FConnector;
end;

procedure TTestHistoryViewer.TearDown;
begin
  FLink1.Free;
  FLink2.Free;
  FHistoryViewer.Free;
  FConnector.Free;
end;

procedure TTestHistoryViewer.TestAttachLink;
begin
  Assert.AreEqual(0, FHistoryViewer.Links.Count);

  FLink1.DataSource := FHistoryViewer;
  Assert.AreEqual(0, FHistoryViewer.Links.Count);
  Assert.AreEqual(1, FHistoryViewer.Groups.Count);

  FLink1.DataSource := nil;
  Assert.AreEqual(0, FHistoryViewer.Groups.Count);
end;

procedure TTestHistoryViewer.TestFillHistory;
begin
  FLink1.DataSource := FHistoryViewer;
  FLink2.DataSource := FHistoryViewer;
  FHistoryViewer.FillHistory(EncodeDateTime(2023,11,21,0,0,0,0), EncodeDateTime(2023,11,22,0,0,0,0));

  FHistoryViewer.First;
  while not FHistoryViewer.Eof do
  begin
    Assert.AreEqual(True, FLink1.Timestamp >= FHistoryViewer.Date1);
    Assert.AreEqual(True, FLink1.Timestamp <= FHistoryViewer.Date2);
    FHistoryViewer.Next;
  end;

end;

end.
