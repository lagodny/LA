unit LA.Threads;

interface

uses
  System.Classes, System.SyncObjs, System.Diagnostics;

type
  TLAIntervalThread = class(TThread)
  strict private
    FInterval: Int64;
    FTimer: TStopwatch;
    FIdelMode: Boolean; protected
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
    procedure Process; virtual;
    procedure Cleanup; virtual;
  public
    constructor CreateInterval(CreateSuspended: Boolean; aInterval: Int64);
    destructor Destroy; override;

    property Interval: Int64 read GetInterval write SetInterval;
    // поток будет работать но без выполнения полезной работы
    property IdelMode: Boolean read FIdelMode write FIdelMode;
  end;


implementation

{ TDCIntervalThread }

function TLAIntervalThread.CalculateTimeout: Int64;
begin
  if FInterval = 0 then
    Result := INFINITE
  else if FTimer.ElapsedMilliseconds >= FInterval then
    Result := 0
  else
    Result := FInterval - FTimer.ElapsedMilliseconds;
end;

procedure TLAIntervalThread.Cleanup;
begin
  // очистка после выхода из цикла
end;

constructor TLAIntervalThread.CreateInterval(CreateSuspended: Boolean; aInterval: Int64);
begin
  inherited Create(CreateSuspended);
  FEvent := TEvent.Create;
  FInterval := aInterval;
end;

destructor TLAIntervalThread.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TLAIntervalThread.Execute;
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
              Break
            else
            begin
              // старт или принудительный запуск
              FTimer.Reset;
              FTimer.Start;
              if not IdelMode then
                Process;
            end;
          end;
        wrTimeout:
          begin
            FTimer.Reset;
            FTimer.Start;
            if not IdelMode then
              Process;
          end
        else
          Break; //Terminate thread
      end;
    until Terminated;
  finally
    Cleanup;
  end;
end;

function TLAIntervalThread.GetInterval: Int64;
begin
  Result := TInterlocked.Read(FInterval);
end;

procedure TLAIntervalThread.Initialize;
begin
  // инициализация перед входом в цикл
end;

procedure TLAIntervalThread.Process;
begin
  // выполняем полезную работу каждые Interval мс
end;

procedure TLAIntervalThread.SetInterval(const Value: Int64);
begin
  TInterlocked.Exchange(FInterval, Value);
  FTimer := TStopwatch.StartNew;
end;

procedure TLAIntervalThread.TerminatedSet;
begin
  FEvent.SetEvent;
  inherited;
end;

end.
