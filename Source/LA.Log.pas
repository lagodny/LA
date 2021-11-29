unit LA.Log;

interface

type
  // класс для ведения лога
  TDCLog = class
  private
    type
      TDCLogImpl = class
      public
        procedure WriteToLog(const aMessage: string);
        procedure WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);
      end;

    class var FLog: TDCLogImpl;
    class function GetLogImpl: TDCLogImpl;
  public
    class procedure WriteToLog(const aMessage: string);
    class procedure WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);
  end;

implementation

{ TDCLog }

class function TDCLog.GetLogImpl: TDCLogImpl;
begin
  if not Assigned(FLog) then
    FLog := TDCLogImpl.Create;
  Result := FLog;
end;

class procedure TDCLog.WriteToLog(const aMessage: string);
begin
  GetLogImpl.WriteToLog(aMessage);
end;

class procedure TDCLog.WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);
begin
  GetLogImpl.WriteToLogFmt(aMessage, Args);
end;

{ TDCLog.TDCLogImpl }

procedure TDCLog.TDCLogImpl.WriteToLog(const aMessage: string);
begin

end;

procedure TDCLog.TDCLogImpl.WriteToLogFmt(const aMessage: string; const Args: array of TVarRec);
begin

end;

initialization

finalization
  TDCLog.FLog.Free;

end.
