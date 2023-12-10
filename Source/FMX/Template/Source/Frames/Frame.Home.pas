unit Frame.Home;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, Frame.Toolbar, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  FMX.Layouts, LA.FMX.StdCtrls, FMX.Objects, LA.FMX.Objects;

type
  THomeFrame = class(TToolbarFrame)
    LALabel1: TLALabel;
    Label2: TLabel;
    LALabel2: TLALabel;
    Label1: TLabel;
    LALabel3: TLALabel;
    Label3: TLabel;
    LALabel4: TLALabel;
    Label4: TLabel;
    LALabel5: TLALabel;
    Label5: TLabel;
    LALabel6: TLALabel;
    LALabel7: TLALabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HomeFrame: THomeFrame;

implementation

{$R *.fmx}

uses DM.DataController;

end.
