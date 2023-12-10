unit Form.Base;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Platform, FMX.VirtualKeyboard,
  iPub.FMX.SystemBars;

type
//  TBaseForm = class(TFrame)
  TBaseForm = class(TForm)
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

uses DM.Common;

{ TBaseForm }

procedure TBaseForm.ApplySystemBarsInsets(aSystemBars: TipFormSystemBars);
begin
  var s := GetSystemBars(aSystemBars);
  if not Assigned(s) then
    Exit;

  Padding.Rect := s.Insets;
  s.StatusBarBackgroundColor := $00000000;
  s.NavigationBarBackgroundColor := $00000000;
end;

function TBaseForm.Back: Boolean;
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

procedure TBaseForm.DoAfterCreate;
begin

end;

function TBaseForm.GetFormOwner: TCustomForm;
begin
  if Owner is TCustomForm then
    Result := TCustomForm(Owner)
  else if Owner is TBaseForm then
    Result := TBaseForm(Owner).GetFormOwner
  else
    Result := nil;
end;

function TBaseForm.GetSystemBars(aSystemBars: TipFormSystemBars): TipFormSystemBars;
begin
  if Assigned(aSystemBars) then
    Exit(aSystemBars);

  var f := GetFormOwner;
  if Assigned(f) then
    Result := f.SystemBars
  else
    Result := nil;
end;

function TBaseForm.HandleApplicationEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  Result := False;
end;

procedure TBaseForm.HideKeyboard;
var
  k: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(k)) then
    if TVirtualKeyboardState.Visible in k.VirtualKeyboardState then
      k.HideVirtualKeyboard;
end;

procedure TBaseForm.LoadSettings;
begin

end;

procedure TBaseForm.OrientationChanged(const ASender: TObject; const AMessage: TMessage);
begin

end;

procedure TBaseForm.SaveSettings;
begin

end;

end.
