unit LA.FMX.UI.Toast;

interface


procedure Toast(const Msg: string);

implementation

uses
  System.Classes
{$IFDEF ANDROID}
  ,
  Androidapi.JNI.Widget,
  Androidapi.Helpers
{$ENDIF}
  ;

{$IFDEF ANDROID}
  procedure Toast(const Msg: string);
  begin
//    CallInUiThread (
    TThread.Synchronize(nil,
      procedure
      begin
        TJToast.JavaClass.makeText (SharedActivityContext,
            StrToJCharSequence(Msg), TJToast.JavaClass.LENGTH_SHORT).show
      end
    );
  end;
{$ELSE}
  procedure Toast(const Msg: string);
  begin

  end;
{$ENDIF}

end.
