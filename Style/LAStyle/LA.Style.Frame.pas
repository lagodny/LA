unit LA.Style.Frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, LA.Style.Frame2;

type
  TTestFrame = class(TFrame)
    Rectangle1: TRectangle;
    Text1: TText;
    Frame21: TFrame2;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
