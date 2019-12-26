unit LA.DC.HTTPConnector;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  IdGlobal, IdTCPClient, IdException,
  LA.DC.CustomConnector;

type
  TDCTCPConnector = class(TDCCustomConnector)
  private
    FEncrypt: boolean;
    FCompressionLevel: Integer;
  protected
    function GetEncrypt: boolean; override;
    function GetCompressionLevel: Integer; override;
    function GetConnectTimeOut: Integer; override;
    function GetReadTimeOut: Integer; override;

    procedure SetEncrypt(const Value: boolean); override;
    procedure SetCompressionLevel(const Value: Integer); override;
    procedure SetReadTimeOut(const Value: Integer); override;
    procedure SetConnectTimeOut(const Value: Integer); override;

    function GetConnected: Boolean; override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;

  public
    procedure Connect; override;
    procedure Disconnect; override;
  end;



implementation

{ TDCTCPConnector }

procedure TDCTCPConnector.Connect;
begin

end;

procedure TDCTCPConnector.Disconnect;
begin

end;

procedure TDCTCPConnector.DoConnect;
begin

end;

procedure TDCTCPConnector.DoDisconnect;
begin

end;

function TDCTCPConnector.GetCompressionLevel: Integer;
begin
  Result := FCompressionLevel;
end;

function TDCTCPConnector.GetConnected: Boolean;
begin

end;

function TDCTCPConnector.GetConnectTimeOut: Integer;
begin

end;

function TDCTCPConnector.GetEncrypt: boolean;
begin
  Result := FEncrypt;
end;

function TDCTCPConnector.GetReadTimeOut: Integer;
begin

end;

procedure TDCTCPConnector.SetCompressionLevel(const Value: Integer);
begin
  if FCompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    DoPropChanged;
  end;
end;

procedure TDCTCPConnector.SetConnectTimeOut(const Value: Integer);
begin

end;

procedure TDCTCPConnector.SetEncrypt(const Value: boolean);
begin
  if FEncrypt <> Value then
  begin
    FEncrypt := Value;
    DoPropChanged;
  end;
end;

procedure TDCTCPConnector.SetReadTimeOut(const Value: Integer);
begin

end;

end.
