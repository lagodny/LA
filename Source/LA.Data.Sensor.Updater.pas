unit LA.Data.Sensor.Updater;

interface

uses
  System.SysUtils,
  LA.Types.Monitoring,
  LA.Data.Source,
  LA.Data.Updater;

type
  TLASensorUpdater = class(TLADataUpdater)
  protected
    function GetDataFromServer(const IDs: TSIDArr): string; override;
    procedure ProcessServerResponse(const aResponse: string); override;
  public
    procedure Attach(const aLink: TLADataLink); override;
  end;

implementation

uses
  LA.Data.Link.Sensor;


{ TLASensorUpdater }

procedure TLASensorUpdater.Attach(const aLink: TLADataLink);
begin
  // можем работать только с датчиками
  if not (aLink is TLASensorLink) then
    raise Exception.Create(sIncorrectDataLinkType);

  inherited;
end;

function TLASensorUpdater.GetDataFromServer(const IDs: TSIDArr): string;
begin
  if Links.Count = 0 then
    Exit('');

  Result := Connector.SensorsDataAsText(IDs, True);
end;

procedure TLASensorUpdater.ProcessServerResponse(const aResponse: string);
const
  cValueDelimiter = ';';
  cDataDelimiter = #13;
var
  p1, p2: Integer;
  aLineID: string;
  i, aLinkIndex, aLinkMaxIndex: Integer;
begin
//  if FLinksChanged then
//    Exit;

  aLinkMaxIndex := FLinks.Count - 1;
  if aLinkMaxIndex < 0 then
    Exit;

  p1 := 1;
  aLinkIndex := 0;

  FLock.BeginRead;
  try
    /// из строки вида:
    ///  id1;value1;status1;moment1$13
    ///  id2;value2;status2;moment2$13
    ///  вырезаем строку
    ///  и эту строку сохраняем в Link с ID = id строки
    ///  пропускать строки у которых id = ''
    i := 1;
    while i <= Length(aResponse) do
    begin
      // нашли конец значения и aLineID еще не найден
      if (aResponse[i] = cValueDelimiter) and (aLineID = '') then
      begin
        aLineID := Copy(aResponse, p1, i - p1);
        // строки с пустым ID пропускаем
        if aLineID = '' then
        begin
          while (i <= Length(aResponse)) and (aResponse[i] <> cDataDelimiter)  do
            Inc(i);
          Inc(i);
          p1 := i;
          Continue;
        end;

        // ищем Link для этого ID
        while (aLinkIndex <= aLinkMaxIndex) and (FLinks[aLinkIndex].ID <> aLineID) do
          Inc(aLinkIndex);

        // не нашли Линк - выходим (нет необходимости разбирать оставшуюся часть)
        if aLinkIndex > aLinkMaxIndex then
          Break;
      end

      // нашли конец строки
      else if aResponse[i] = cDataDelimiter then
      begin
        p2 := i;
        FLinks[aLinkIndex].Data := Copy(aResponse, p1, p2 - p1);
        p1 := p2 + 1;
        // сбрасываем, чтобы иницировать из следующей строки
        aLineID := '';
        // переходим к следующему линку
        Inc(aLinkIndex);
      end;

      //
      Inc(i);
    end;

  finally
    FLock.EndRead;
  end;
end;

end.
