unit LA.Tests.Threads;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  LA.Threads;

type
  [TestFixture]
  TTest_TLAIntervalThread = class
  private
    CUT: TLAIntervalThread;
    procedure DoTerminate(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
//    [Test]
    procedure TestStartStopThread;
  end;

implementation

procedure TTest_TLAIntervalThread.DoTerminate(Sender: TObject);
begin
  CUT := nil;
end;

procedure TTest_TLAIntervalThread.Setup;
begin
end;

procedure TTest_TLAIntervalThread.TearDown;
begin
end;

procedure TTest_TLAIntervalThread.TestStartStopThread;
begin
  CUT := TLAIntervalThread.CreateInterval(True, 100);
  CUT.FreeOnTerminate := True;
  CUT.OnTerminate := DoTerminate;
  CUT.Start;
  TThread.Yield;

  CUT.Terminate;
  TThread.Yield;

  TThread.Sleep(1000);
  //CUT.WaitFor;
  Assert.IsNull(CUT)
end;

initialization
  TDUnitX.RegisterTestFixture(TTest_TLAIntervalThread);

end.
