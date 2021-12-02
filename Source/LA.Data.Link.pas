unit LA.Data.Link;

interface

type

  /// <summary>
  ///  Абстрактный класс Линк/адаптер
  ///  наследники должны реализовать доступ к Наблюдателю определенного класса
  ///  - определить конструктор, в котрый будет передан Наблюдатель
  ///  - переопределить GetID и Notify для работы с этим Наблюдателем
  /// </summary>
  TDCLink = class abstract
  private
    FData: string;
    FIsNeedNotify: Boolean;
  public
    function GetID: string; virtual; abstract;
    procedure SetData(const aData: string);
    procedure Notify; virtual;

    property Data: string read FData write SetData;
  end;


implementation

{ TDCLink }

procedure TDCLink.Notify;
begin
  FIsNeedNotify := False;
end;

procedure TDCLink.SetData(const aData: string);
begin
  if aData <> FData then
  begin
    FData := aData;
    FIsNeedNotify := True;
  end;
end;

end.
