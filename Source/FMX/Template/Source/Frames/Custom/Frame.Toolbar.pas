unit Frame.Toolbar;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, System.Actions,
  FMX.ActnList, FMX.MultiView, FMX.ListBox, FMX.Objects, Frame.Base;

type
  TToolbarFrame = class(TBaseFrame)
    sbDrawer: TSpeedButton;
    sbMore: TSpeedButton;
    laTitle: TLabel;
    loContent: TLayout;
    acMore: TActionList;
    loTop: TLayout;
    loClient: TLayout;
    sbBack: TSpeedButton;
    aBack: TAction;
    procedure aBackExecute(Sender: TObject);
  end;


implementation

{$R *.fmx}

uses
  App.State, App.State.Controller,
  DM.Common;

procedure TToolbarFrame.aBackExecute(Sender: TObject);
begin
//  AppStateController.AppState := asMap;
  AppStateController.Back;
end;

end.
