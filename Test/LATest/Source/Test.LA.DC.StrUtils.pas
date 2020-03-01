unit Test.LA.DC.StrUtils;

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
//    [Test]
//    procedure Test1;

    [Test]
    [TestCase('Num 1','1, ,31')]
    [TestCase('Num 12','12, ,31 32')]
    [TestCase('Num 123','123, ,31 32 33')]
    [TestCase('En ABC','ABC, ,41 42 43')]
    [TestCase('Ru ÀÁÂÃÄ','ÀÁÂÃÄ, ,C0 C1 C2 C3 C4')]
    procedure TestStrToHex(const aStr: string; const aDelimiter: string; _Result: string);

    [Test]
    procedure TestRawStrToHex;
  end;

implementation

uses
  System.SysUtils;

procedure TestTDCStrUtils.Setup;
begin
end;

procedure TestTDCStrUtils.TearDown;
begin
end;

//procedure TestTDCStrUtils.Test1;
//begin
//end;

procedure TestTDCStrUtils.TestRawStrToHex;
var
  s: RawByteString;
  R: string;
begin
  s := StringOf([$30,$31,$32]);

  R := TDCStrUtils.StrToHex(s, ' ');
  Assert.AreEqual(R, '30 31 32');
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
