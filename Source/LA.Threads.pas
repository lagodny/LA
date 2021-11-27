unit LA.Threads;

interface

uses
  System.Classes, System.SyncObjs, System.Diagnostics;

type
  TDCIntervalThread = class(TThread)
  strict private
    FInterval: Int64;
    FTimer: TStopwatch;
  strict protected
    function CalculateTimeout: Int64;
    function GetInterval: Int64;
    procedure SetInterval(const Value: Int64);
  protected
    FEvent: TEvent;
    procedure Execute; override;
    procedure TerminatedSet; override;

    // нужно переопределить для выполнения нужной работы
    procedure Initialize; virtual;
    procedure ProcessTimer; virtual;
    procedure Cleanup; virtual;
  public
    constructor Create(CreateSuspended: Boolean; aInterval: Int64); overload;

    property Interval: Int64 read GetInterval write SetInterval;
  end;


implementation

{ TDCIntervalThread }

function TDCIntervalThread.CalculateTimeout: Int64;
begin
  if FInterval = 0 then
    Result := INFINITE
  else if FTimer.ElapsedMilliseconds >= FInterval then
    Result := 0
  else
    Result := FInterval - FTimer.ElapsedMilliseconds;
end;

procedure TDCIntervalThread.Cleanup;
begin
  // очистка после выхода из цикла
end;

constructor TDCIntervalThread.Create(CreateSuspended: Boolean; aInterval: Int64);
begin
  inherited Create(CreateSuspended);
  FInterval := aInterval;
end;

procedure TDCIntervalThread.Execute;
var
  aWaitResult: TWaitResult;
begin
  Initialize;
  try
    repeat
      aWaitResult := FEvent.WaitFor(CalculateTimeout);
      FEvent.ResetEvent;
      case aWaitResult of
        wrSignaled:
          begin
            if Terminated then
              Break;
          end;
        wrTimeout:
          begin
            FTimer.Reset;
            FTimer.Start;
            ProcessTimer;
          end
        else
          Break; //Terminate thread
      end;
    until False;
  finally
    Cleanup;
  end;
end;

function TDCIntervalThread.GetInterval: Int64;
begin
  Result := TInterlocked.Read(FInterval);
end;

procedure TDCIntervalThread.Initialize;
begin
  // инициализация перед входом в цикл
end;

procedure TDCIntervalThread.ProcessTimer;
begin
  // выполняем полезную работу каждые Interval мс
end;

procedure TDCIntervalThread.SetInterval(const Value: Int64);
begin
  TInterlocked.Exchange(FInterval, Value);
  FTimer := TStopwatch.StartNew;
end;

procedure TDCIntervalThread.TerminatedSet;
begin
  FEvent.SetEvent;
  inherited;
end;

end.
