unit App.State.Controller;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Collections,
  App.State,
  App.Events;

type
  TAppStateController = class
  private
    class var FInstance: TAppStateController;
  private
    FStateStack: TStack<TAppState>;
    FAppState: TAppState;
    FDelay: Integer;
    function AppStateToCommand(aAppState: TAppState): TMainViewCommandEnum;
    procedure SetAppState(const Value: TAppState);
    procedure NotifyAppStateChanged( aProc: TProc<TObject>);
  public
    constructor Create;
    destructor Destroy; override;

    // возаращает истину, если есть куда выходить
    function Back: Boolean;
    procedure SetAppStateAndRun(Value: TAppState; aProc: TProc<TObject>);

    property AppState: TAppState read FAppState write SetAppState;
    property Delay: Integer read FDelay write FDelay;
  end;

  function AppStateController: TAppStateController;

implementation

uses
  NX.Horizon;

function AppStateController: TAppStateController;
begin
  if not Assigned(TAppStateController.FInstance) then
    TAppStateController.FInstance := TAppStateController.Create;

  Result := TAppStateController.FInstance;
end;


{ TAppStateController }

function TAppStateController.AppStateToCommand(aAppState: TAppState): TMainViewCommandEnum;
begin
  case aAppState of
    asUnknown:
      Result := mcNone;
    asLogin:
      Result := mcShowLogin;
    asHome:
      Result := mcHome;
    asSettings:
      Result := mcShowSettings;
    asInterval:
      Result := mcShowInterval;
    else
      raise Exception.Create('Unknown AppState: {96052BE3-F8C0-436B-A368-902A59BA1BB6}');
  end;
end;

function TAppStateController.Back: Boolean;
var
  aAppState: TAppState;
begin
  Result := False;
  while FStateStack.Count > 0 do
  begin
    aAppState := FStateStack.Pop;
    if aAppState <> FAppState then
    begin
      FAppState := aAppState;
      NotifyAppStateChanged(nil);
      Exit(True);
    end;
  end;
end;

constructor TAppStateController.Create;
begin
  FStateStack := TStack<TAppState>.Create;
  FDelay := 50;
  FAppState := TAppState.asUnknown;
end;

procedure TAppStateController.SetAppState(const Value: TAppState);
begin
  SetAppStateAndRun(Value, nil);
end;

procedure TAppStateController.SetAppStateAndRun(Value: TAppState; aProc: TProc<TObject>);
begin
  // если переходим в такие состояния
  if (Value in [asSettings, asInterval])
  //  и текущее не равно таким
    and not (FAppState in [asInterval]) then
  begin
    // то старое сохраняем
    if (FStateStack.Count = 0) or (FStateStack.Peek <> FAppState) then
      FStateStack.Push(FAppState);
  end
  // если переходим в такие, то стек очищаем
  else if Value in [asHome, asLogin] then
    FStateStack.Clear;

  FAppState := Value;

  NotifyAppStateChanged(aProc);
end;

destructor TAppStateController.Destroy;
begin
  FStateStack.Free;
  inherited;
end;

procedure TAppStateController.NotifyAppStateChanged(aProc: TProc<TObject>);
begin
  NxHorizon.Instance.Post<IMainViewCommand>(
    TMainViewCommand.Create(TMainViewEventRec.Create(AppStateToCommand(AppState), aProc, '')));
end;

initialization
  TAppStateController.FInstance := nil;

finalization
  TAppStateController.FInstance.Free;

end.
