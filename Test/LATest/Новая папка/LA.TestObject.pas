unit LA.TestObject;

interface
uses
  DUnitX.TestFramework, LA.DC.StrUtils;

type

  [TestFixture]
  TestTDCStrUtils = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);

    [Test]
    [TestCase('Case1','1, ,31')]
    procedure TestStrToHex(const aStr: string; const aDelimiter: string; _Result: string);
  end;

implementation

procedure TestTDCStrUtils.Setup;
begin
end;

procedure TestTDCStrUtils.TearDown;
begin
end;

procedure TestTDCStrUtils.Test1;
begin
end;

procedure TestTDCStrUtils.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

procedure TestTDCStrUtils.TestStrToHex(const aStr, aDelimiter: string; _Result: string);
var
  R: string;
begin
  R := TDCStrUtils.StrToHex(aStr, aDelimiter);
  Assert.AreEqual(R, _Result);
end;

initialization
  TDUnitX.RegisterTestFixture(TestTDCStrUtils);
end.
