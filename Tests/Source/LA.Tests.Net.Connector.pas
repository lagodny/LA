unit LA.Tests.Net.Connector;

interface

uses
  DUnitX.TestFramework,
  LA.Net.Connector.Http;

type
  [TestFixture]
  TTestTDCCustomConnector = class
  private
    FConnector: TLAHttpConnector;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('TestTryConnectArrangeAddr','host1:1;dc.tdc.org.ua:80,dc.tdc.org.ua:80;host1:1;')]
    [TestCase('TestTryConnectArrangeAddr','dc.tdc.org.ua:80;host1:1,dc.tdc.org.ua:80;host1:1')]
    procedure TestTryConnectArrangeAddr(const aAddrIn, aAddrOut: string);

  end;

implementation

uses
  La.Tests.Net.Consts;

procedure TTestTDCCustomConnector.Setup;
begin
  FConnector := TLAHttpConnector.Create(nil);
  FConnector.UserName := cUserName;
  FConnector.Password := cPassword;
end;

procedure TTestTDCCustomConnector.TearDown;
begin
  FConnector.Free;
end;


procedure TTestTDCCustomConnector.TestTryConnectArrangeAddr(const aAddrIn, aAddrOut: string);
begin
  FConnector.Address := aAddrIn;
  FConnector.Connect;
  Assert.AreEqual(FConnector.Address, aAddrOut);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTDCCustomConnector);

end.
