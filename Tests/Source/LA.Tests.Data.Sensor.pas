unit LA.Tests.Data.Sensor;

interface

uses
  DUnitX.TestFramework,
  System.DateUtils, System.SysUtils,
  LA.Data.Source,
  LA.Data.Updater,
  LA.Data.Sensor,
  LA.Utils.Str;

type
  [TestFixture]
  TTest_TLASensor = class
  private
    CUT: TLASensor;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestLink;
    [Test]
    procedure TestEncodeData;
    [Test]
    [TestCase('','1,1;3;Production;no data;2022-12-19T00:01:02,3,Production,no data,2022-12-19T00:01:02')]
    [TestCase('','2,2;5;CIP;;2022-12-19T00:01:02,5,CIP,,2022-12-19T00:01:02')]
    procedure TestEncodeData_Case(const aID, aResponse, aValue, aText, aStatus, aTimestamp: string);

  end;


implementation

{ TTest_TLASensor }

procedure TTest_TLASensor.Setup;
begin
  CUT := TLASensor.Create(nil);

end;

procedure TTest_TLASensor.TearDown;
begin
  CUT.Free;
end;

procedure TTest_TLASensor.TestEncodeData;
begin
  CUt.Link.ID := '1';
  CUT.Link.Data :=
    '1;3;Production;no data;2022-12-19T00:01:02';
  CUT.Link.Notify;
  Assert.AreEqual<String>(CUT.Link.ID, '1');
  Assert.AreEqual<Double>(CUT.Link.Value, 3);
  Assert.AreEqual<string>(CUT.Link.Text, 'Production');
  Assert.AreEqual<string>(CUT.Link.Status, 'no data');
  Assert.AreEqual<TDateTime>(CUT.Link.Timestamp, ISO8601ToDate('2022-12-19T00:01:02'));
end;

procedure TTest_TLASensor.TestEncodeData_Case(const aID, aResponse, aValue, aText, aStatus, aTimestamp: string);
begin
  CUt.Link.ID := aID;
  CUT.Link.Data := aResponse;
  CUT.Link.Notify;
  Assert.AreEqual<String>(CUT.Link.ID, aID);
  Assert.AreEqual<Double>(CUT.Link.Value, TLAStrUtils.DotStrToFloat(aValue));
  Assert.AreEqual<string>(CUT.Link.Text, aText);
  Assert.AreEqual<string>(CUT.Link.Status, aStatus);
  Assert.AreEqual<TDateTime>(CUT.Link.Timestamp, ISO8601ToDate(aTimestamp));
end;

procedure TTest_TLASensor.TestLink;
begin
  Assert.IsTrue(Assigned(CUT.Link));

end;

end.
