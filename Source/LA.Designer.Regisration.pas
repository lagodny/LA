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

procedure Register;

implementation

uses
  Vcl.Dialogs,
  LA.Data.Sensor;

resourcestring
  sDesAddSensor = 'Add sensor';

procedure Register;
begin
  RegisterComponentEditor(TLASensorList, TLASensorListEditor);
end;


{ TLASensorListEditor }

procedure TLASensorListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
    begin
      ShowMessage(Component.Name);
      var aSensor := TLASensor.Create(Component.Owner);
      aSensor.Name := Designer.UniqueName(aSensor.ClassName);
      ShowMessage(aSensor.Name);
      aSensor.SensorList := TLASensorList(Component);
      ShowMessage(aSensor.GetParentComponent.Name);
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

end.
