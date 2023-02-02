unit LA.Utils.System;

interface
  uses
    System.SysUtils
{$IFDEF WIN32}
    , Winapi.Windows
{$ELSE}
{$ENDIF}
    ;



type
  TLASystemUtils = class
  public
    class function GetLocalUserName: string;
    class function GetComputerName: string;
  end;

implementation

{ TDCSystemUtils }

class function TLASystemUtils.GetComputerName: string;
{$IFDEF WIN32}
var
  Size: UInt32;
begin
  Result := '';
  Size := MAX_PATH;
  SetLength(Result, Size);
  if WinApi.Windows.GetComputerName(PChar(Result), Size) then
    SetLength(Result, StrLen(PChar(Result)))
  else
    RaiseLastOSError;
{$ELSE}
begin
  Result := '';
{$ENDIF}
end;

class function TLASystemUtils.GetLocalUserName: string;
{$IFDEF WIN32}
var
  Count: UInt32;
begin
  Count := 256 + 1; // UNLEN + 1
  SetLength(Result, Count);
  if GetUserName(PChar(Result), Count) then
    SetLength(Result, StrLen(PChar(Result)))
  else
    Result := '';
{$ELSE}
begin
  Result := '';
{$ENDIF}
end;

end.
