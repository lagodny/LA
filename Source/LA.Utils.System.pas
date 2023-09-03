unit LA.Utils.System;

interface
  uses
    System.SysUtils
  {$IFDEF WIN32}
    , Winapi.Windows
  {$ENDIF}
  {$IFDEF OSX}
  , Macapi.CoreFoundation
  {$ENDIF}
  {$IFDEF ANDROID}
  , Androidapi.Helpers
  , Androidapi.JNI.GraphicsContentViewText
  , Androidapi.JNI.JavaTypes
  , Androidapi.JNI.App
  , Androidapi.NativeActivity
  {$ENDIF}
  {$IFDEF IOS}
  , Macapi.ObjectiveC
  , iOSapi.CocoaTypes
  , iOSapi.Foundation
  , iOSapi.CoreGraphics
  , Macapi.Helpers
  , FMX.Helpers.iOS
  {$ENDIF}
    ;



type
  TLASystemUtils = class
  public
    class function GetLocalUserName: string;
    class function GetComputerName: string;
    class function ProgramFullSpec: string;
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

class function TLASystemUtils.ProgramFullSpec: string;
  {$IFDEF MSWINDOWS}
  function GetVerInfo(sPath, sInfo:string): string;
  var
    BufSize, Len: DWORD;
    Buf: PChar;
    pBuf, pValue: Pointer;
    VIndexStr: string;
  begin
    Result := '';
    BufSize := GetFileVersionInfoSize(PChar(sPath), BufSize);

    if BufSize > 0 then
    begin
      GetMem(Buf, BufSize);
      try
        if GetFileVersionInfo(PChar(sPath), 0, BufSize, Buf) then
        begin
          VerQueryValue(Buf, PChar('\VarFileInfo\Translation'), pBuf, Len);
          VIndexStr := InttoHex(LoWord(Integer(pBuf^)), 4) + InttoHex(HiWord(Integer(pBuf^)), 4);
          if VerQueryValue(Buf, PChar('StringFileInfo\' + VIndexStr + '\' + sInfo), pValue, Len) then
            Result := PChar(pValue);
        end;
      finally
        FreeMem(Buf, BufSize);
      end;
    end;
  end;

  function InnerWin(): string;
  begin
    Result := ParamStr(0) + ' : ' + GetVerInfo(ParamStr(0), 'FileVersion');
  end;
  {$ENDIF}

  {$IFDEF ANDROID}
  function InnerAndroid(): string;
  var
    PackageInfo: JPackageInfo;
    PackageName: JString;
  begin
    PackageName := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}.getPackageName;
    PackageInfo := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}.
      getPackageManager.getPackageInfo(PackageName, 0);
    if Assigned(PackageInfo) then
      Result := JStringToString(PackageName) + ' : ' + JStringToString(PackageInfo.versionName)
    else
      Result := '';
  end;
  {$ENDIF}
  {$IFDEF IOS}
  function InnerIOS(): string;
  var
    aCFBundleVersion, aCFBundleDisplayName: Pointer;
    NSAppVersion: NSString;
    AppBundle: NSBundle;
  begin
    AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
    // AppNameKey := (NSSTR('CFBundleVersion') as ILocalObject).GetObjectID;
    aCFBundleVersion := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;
    NSAppVersion := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(aCFBundleVersion));
    Result :=
      UTF8ToString(AppBundle.bundleIdentifier.UTF8String) + ' : ' +
      UTF8ToString(NSAppVersion.UTF8String);
  end;
  {$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  Result := InnerWin();
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := InnerAndroid();
  {$ENDIF}
  {$IFDEF IOS}
  Result := InnerIOS();
  {$ENDIF}
end;


end.
