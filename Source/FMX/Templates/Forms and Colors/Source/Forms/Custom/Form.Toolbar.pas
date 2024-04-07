unit Form.Toolbar;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, System.Actions,
  FMX.ActnList, FMX.MultiView, FMX.ListBox, FMX.Objects, Form.Base,
  iPub.FMX.SystemBars;

type
  TToolbarForm = class(TBaseForm)
    sbDrawer: TSpeedButton;
    sbMore: TSpeedButton;
    laTitle: TLabel;
    loContent: TLayout;
    acMore: TActionList;
    loTop: TLayout;
    loClient: TLayout;
    sbBack: TSpeedButton;
    aBack: TAction;
    pNavigator: TPanel;
    pContent: TPanel;
    procedure aBackExecute(Sender: TObject);
  public
    procedure ApplySystemBarsInsets(aSystemBars: TipFormSystemBars); override;
  end;


implementation

{$R *.fmx}

uses
  App.State, App.State.Controller,
  DM.Common;

procedure TToolbarForm.aBackExecute(Sender: TObject);
begin
  AppStateController.Back;
end;

procedure TToolbarForm.ApplySystemBarsInsets(aSystemBars: TipFormSystemBars);
begin
  inherited;
  var s := GetSystemBars(aSystemBars);
  if not Assigned(s) then
    Exit;
  loClient.Margins.Rect := s.Insets;
end;

end.
