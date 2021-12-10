unit LA.Data.Link;

interface

type

  /// <summary>
  ///  Абстрактный класс Линк/адаптер
  ///  наследники должны реализовать доступ к Наблюдателю определенного класса
  ///  - определить конструктор, в котрый будет передан Наблюдатель
  ///  - переопределить GetID и Notify для работы с этим Наблюдателем
  /// </summary>
  TLALink = class abstract
  private
    FData: string;
    FIsNeedNotify: Boolean;
    FLinkedObject: TObject;
  public
    constructor Create(aLinkedObject: TObject);

    function GetID: string; virtual; abstract;
    procedure SetData(const aData: string);
    procedure Notify; virtual;

    property Data: string read FData write SetData;
    property LinkedObject: TObject read FLinkedObject;
  end;


implementation

{ TDCLink }

constructor TLALink.Create(aLinkedObject: TObject);
begin
  FLinkedObject := aLinkedObject;
end;

procedure TLALink.Notify;
begin
  FIsNeedNotify := False;
end;

procedure TLALink.SetData(const aData: string);
begin
  if aData <> FData then
  begin
    FData := aData;
    FIsNeedNotify := True;
  end;
end;

end.
