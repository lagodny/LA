unit LA.Version;

interface

function GetVersion: String;

implementation

uses
  System.SysUtils
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
  , Macapi.Helpers
  , iOSapi.Foundation
  {$ENDIF}
  ;

function GetVersion: String;
begin
  Result := 'unknown';

  {$IFDEF MSWINDOWS}
  var Major, Minor, Build: Cardinal;

  if GetProductVersion(ParamStr(0), Major, Minor, Build) then
    Result := Format('%d.%d.%d', [Major, Minor, Build]);
  {$ENDIF}

  {$IFDEF OSX}
  var CFStr: CFStringRef :=
    CFBundleGetValueForInfoDictionaryKey(
      CFBundleGetMainBundle,
      kCFBundleVersionKey
    );

  var Range: CFRange;
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);

  CFStringGetCharacters(CFStr, Range, PChar(Result));

  Result := Result.Trim;
  {$ENDIF}

  {$IFDEF IOS}
  var Ver :=
    TNSBundle
    .Wrap(TNSBundle.OCClass.mainBundle)
    .infoDictionary
    .objectForKey(StringToID('CFBundleVersion'));

  if Ver <> nil then
    Result := NSStrToStr(TNSString.Wrap(Ver));
  {$ENDIF}

  {$IFDEF ANDROID}
  var Activity :=
    TJNativeActivity.Wrap(PANativeActivity(System.DelphiActivity)^.clazz);
  var Info :=
    Activity.getPackageManager.getPackageInfo(Activity.getPackageName, 0);

  Result := JStringToString(Info.versionName);
  {$ENDIF}
end;

end.

