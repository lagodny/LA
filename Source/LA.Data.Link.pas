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
  public
    function GetID: string; virtual; abstract;
    procedure SetData(const aData: string);
    procedure Notify; virtual; abstract;

    property Data: string read FData write FData;
  end;


implementation

{ TDCLink }

procedure TDCLink.SetData(const aData: string);
begin
  FData := aData;
end;

end.
