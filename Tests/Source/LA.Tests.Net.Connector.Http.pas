unit LA.Tests.Net.Connector.Http;

interface

uses
  DUnitX.TestFramework,
  LA.Net.Connector,
  LA.Net.Connector.Http, LA.Net.Connector.Intf;

type
  [TestFixture]
  TTest_TDCHttpConnector = class
  private
    FConnector: TDCHttpConnector;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestConnect;
    [Test]
    [TestCase('AddrEncode', 'dc.tdc.org.ua,dc.tdc.org.ua,80,False')]
    [TestCase('AddrEncode', 'dc.tdc.org.ua:3131,dc.tdc.org.ua,3131,False')]
    [TestCase('AddrEncode', 'http://dc.tdc.org.ua,dc.tdc.org.ua,80,False')]
    [TestCase('AddrEncode', 'https://dc.tdc.org.ua,dc.tdc.org.ua,443,True')]
    [TestCase('AddrEncode', 'https://dc.tdc.org.ua:6553:,dc.tdc.org.ua,6553,True')]
    [TestCase('AddrEncode', 'https://dc.tdc.org.ua:6553,dc.tdc.org.ua,6553,True')]
    procedure TestAddrEncode(const AddrLine, Host, Port: string; Https: Boolean);

    [TestCase('EmpttyAddrRaiseException', '')]
    procedure TestBadAddrRaiseException(const AddrLine: string);
//    [Test]
    [TestCase('TestSensorValue', '41204,d:\_services\DC\Data\UploadFile\Demo.scp')]
//    [TestCase('TestSensorValue', '5,74332')]
    procedure TestSensorValue(const aSensorAddr, aSensorValue: string);

    [Test]
    procedure GroupSensorDataExtByID;


  end;

implementation

uses
  LA.Tests.Net.Consts,
  LA.Types.Monitoring;

procedure TTest_TDCHttpConnector.GroupSensorDataExtByID;
var
  r: TDataRecExtArr;
begin
  r := FConnector.GroupSensorDataExtByID([41204]);
  Assert.AreEqual(r[0].SID, '41204');
  Assert.AreEqual(r[0].v, 'd:\_services\DC\Data\UploadFile\Demo.scp');
  Assert.AreEqual(r[0].e, '');

  r := FConnector.GroupSensorDataExtByID([41204,31174]);
  Assert.AreEqual(2, Length(r));

  Assert.AreEqual('41204', r[0].SID);
  Assert.AreEqual('31174', r[1].SID);

  Assert.AreEqual('d:\_services\DC\Data\UploadFile\Demo.scp', r[0].v);
  Assert.AreEqual('d:\_services\DC\Data\UploadFile\Vladimir.scp', r[1].v);

  Assert.AreEqual('', r[0].e);
  Assert.AreEqual('', r[1].e);

end;

procedure TTest_TDCHttpConnector.Setup;
begin
  FConnector := TDCHttpConnector.Create(nil);
  FConnector.UserName := cUserName;
  FConnector.Password := cPassword;
  FConnector.Address := cHttpAddr;
end;

procedure TTest_TDCHttpConnector.TearDown;
begin
  FConnector.Free;
end;

procedure TTest_TDCHttpConnector.TestAddrEncode(const AddrLine, Host, Port: string; Https: Boolean);
var
  aRec: TDCHttpAddr;
begin
  aRec.InitFrom(AddrLine);
  Assert.AreEqual(Host, aRec.Host);
  Assert.AreEqual(Port, aRec.Port);
  Assert.AreEqual(Https, aRec.Https);
end;

procedure TTest_TDCHttpConnector.TestBadAddrRaiseException(const AddrLine: string);
var
  aRec: TDCHttpAddr;
begin
  Assert.WillRaise(
    procedure
    begin
     aRec.InitFrom(AddrLine);
    end,
    EDCConnectorBadAddress);
end;

procedure TTest_TDCHttpConnector.TestConnect;
begin
  FConnector.Connect;
  Assert.IsTrue(FConnector.Connected);
end;

procedure TTest_TDCHttpConnector.TestSensorValue(const aSensorAddr, aSensorValue: string);
begin
  FConnector.Connect;
  Assert.AreEqual(FConnector.SensorValue(aSensorAddr), aSensorValue);
end;

initialization
  TDUnitX.RegisterTestFixture(TTest_TDCHttpConnector);

end.
