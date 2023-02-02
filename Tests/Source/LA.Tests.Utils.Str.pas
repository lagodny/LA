unit LA.Tests.Utils.Str;

interface

uses
  DUnitX.TestFramework,
  LA.Utils.Str;

type
  [TestFixture]
  TTestTDCStrUtils = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Test4','12,31 32')]
    [TestCase('Test4','12345,31 32 33 34 35')]
    procedure TestStrToHex(const aStr: string; const aResult: string);
  end;

implementation

procedure TTestTDCStrUtils.Setup;
begin
end;

procedure TTestTDCStrUtils.TearDown;
begin
end;

procedure TTestTDCStrUtils.TestStrToHex(const aStr, aResult: string);
begin
  Assert.AreEqual(aResult, TLAStrUtils.StrToHex(aStr, ' '));

end;

initialization
  TDUnitX.RegisterTestFixture(TTestTDCStrUtils);

end.
