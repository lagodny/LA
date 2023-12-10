unit LA.Designer.Regisration;

interface

uses
  System.Classes,
  DesignEditors, DesignIntf;

type
  TLASensorListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TLASensorUpdaterEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TLAConnectionManagerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


procedure Register;

implementation

uses
  LA.Data.Sensor,
  LA.Data.Sensor.Updater,
  LA.Data.Connection.Manager;

resourcestring
  sDesAddSensor = 'Add sensor';
  sInitSensors = 'Init sensors from server';
  sConnectLinksToHistoryViewer = 'Connect links to HistoryViewer';
  sDisconnectLinksFromHistoryViewer = 'Disconnect links from HistoryViewer';

procedure Register;
begin
  RegisterComponentEditor(TLASensorList, TLASensorListEditor);
  RegisterComponentEditor(TLASensorUpdater, TLASensorUpdaterEditor);
  RegisterComponentEditor(TLAConnectionManager, TLAConnectionManagerEditor);
end;


{ TLASensorListEditor }

procedure TLASensorListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
    begin
      var aSensor := TLASensor.Create(Component.Owner);
      aSensor.Name := Designer.UniqueName(aSensor.ClassName);
      aSensor.SensorList := TLASensorList(Component);
    end;
  end;
end;

function TLASensorListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sDesAddSensor;
  end;
end;

function TLASensorListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TLASensorUpdaterEditor }

procedure TLASensorUpdaterEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
    begin
      if Component is TLASensorUpdater then
        TLASensorUpdater(Component).InitItems;
    end;
  end;
end;

function TLASensorUpdaterEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sInitSensors;
  end;
end;

function TLASensorUpdaterEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TLAConnectionManagerEditor }

procedure TLAConnectionManagerEditor.ExecuteVerb(Index: Integer);
var
  i: Integer;
  aConnection: TLAConnectionItem;
begin
  case Index of
    0:
    begin
      if Component is TLAConnectionManager then
      begin
        for i := 0 to TLAConnectionManager(Component).Items.Count - 1 do
        begin
          aConnection := TLAConnectionManager(Component).Items[i];
          TLAConnectionManager(Component).HistoryViewer.ConnectLinksFromDataSource(aConnection.Updater);
        end;
      end;
    end;
    1:
    begin
      if Component is TLAConnectionManager then
        TLAConnectionManager(Component).HistoryViewer.DisconnectLinks;
    end;
  end;
end;

function TLAConnectionManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sConnectLinksToHistoryViewer;
    1: Result := sDisconnectLinksFromHistoryViewer;
  end;
end;

function TLAConnectionManagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
