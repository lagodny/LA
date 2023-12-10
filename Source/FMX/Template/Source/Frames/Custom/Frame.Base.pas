unit Frame.Base;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Platform, FMX.VirtualKeyboard,
  iPub.FMX.SystemBars;

type
  TBaseFrame = class(TFrame)
  private
    FProc: TProc<TObject>;
  public
    procedure SaveSettings; virtual;
    procedure LoadSettings; virtual;

    procedure DoAfterCreate; virtual;

    function HandleApplicationEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean; virtual;

    // главная форма сообщает фрейму, о нажатии кнопки Back
    // если фрейм, что-то у себя закрыл (например, клавиатуру), то нужно вернуть True,
    // иначе будет произведен либо переход в другое состояние либо выход из приложения
    function Back: Boolean; virtual;
    procedure HideKeyboard;

    function GetFormOwner: TCustomForm;
    function GetSystemBars(aSystemBars: TipFormSystemBars): TipFormSystemBars;

    procedure ApplySystemBarsInsets(aSystemBars: TipFormSystemBars); virtual;

    procedure OrientationChanged(const ASender: TObject; const AMessage: TMessage); virtual;

    property Proc: TProc<TObject> read FProc write FProc;
  end;

implementation

{$R *.fmx}

{ TBaseFrame }

procedure TBaseFrame.ApplySystemBarsInsets(aSystemBars: TipFormSystemBars);
begin
  var s := GetSystemBars(aSystemBars);
  if not Assigned(s) then
    Exit;

  Padding.Rect := s.Insets;
  s.StatusBarBackgroundColor := $00000000;
  s.NavigationBarBackgroundColor := $00000000;
end;

function TBaseFrame.Back: Boolean;
var
  k: IFMXVirtualKeyboardService;
begin
  Result := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(k)) then
    if TVirtualKeyboardState.Visible in k.VirtualKeyboardState then
    begin
      k.HideVirtualKeyboard;
      Exit(True);
    end;
end;

procedure TBaseFrame.DoAfterCreate;
begin

end;

function TBaseFrame.GetFormOwner: TCustomForm;
begin
  if Owner is TCustomForm then
    Result := TCustomForm(Owner)
  else if Owner is TBaseFrame then
    Result := TBaseFrame(Owner).GetFormOwner;
end;

function TBaseFrame.GetSystemBars(aSystemBars: TipFormSystemBars): TipFormSystemBars;
begin
  if Assigned(aSystemBars) then
    Exit(aSystemBars);

  var f := GetFormOwner;
  if Assigned(f) then
    Result := f.SystemBars
  else
    Result := nil;
end;

function TBaseFrame.HandleApplicationEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  Result := False;
end;

procedure TBaseFrame.HideKeyboard;
var
  k: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(k)) then
    if TVirtualKeyboardState.Visible in k.VirtualKeyboardState then
      k.HideVirtualKeyboard;
end;

procedure TBaseFrame.LoadSettings;
begin

end;

procedure TBaseFrame.OrientationChanged(const ASender: TObject; const AMessage: TMessage);
begin

end;

procedure TBaseFrame.SaveSettings;
begin

end;

end.
