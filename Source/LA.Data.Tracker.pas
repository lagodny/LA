unit LA.Data.Tracker;

interface

uses
  System.Classes;

type

  /// <summary>
  ///   Трекер - может быть использован как наблюдатель, по аналогии с Датчиком
  /// </summary>
  TDCTracker = class(TComponent)
  private
    FID: string;
    FData: string;
  public
    procedure SetData(const aData: string);
  published
    property ID: string read FID write FID;
  end;


implementation

{ TDCTracker }

procedure TDCTracker.SetData(const aData: string);
begin
  FData := aData;
end;

end.
