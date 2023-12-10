unit Form.Test;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Frame.Base, Frame.Toolbar, Frame.Home, FMX.Controls.Presentation, FMX.StdCtrls,
  LA.FMX.StdCtrls;

type
  TTestForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TestForm: TTestForm;

implementation

{$R *.fmx}

uses App.Forms.Main;

end.
