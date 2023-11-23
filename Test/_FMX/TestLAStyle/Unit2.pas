unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, LA.FMX.StdCtrls, FMX.Layouts,
  System.ImageList, FMX.ImgList, FMX.Objects, LA.FMX.Objects, FMX.Edit, Radiant.Shapes, FMX.SVGIconImageList;

type
  TForm2 = class(TForm)
    CheckBox1: TCheckBox;
    ImageList2: TImageList;
    ScaledLayout1: TScaledLayout;
    LAGlyph1: TLAGlyph;
    TrackBar1: TTrackBar;
    Edit1: TEdit;
    Button1: TButton;
    SVGIconImageList1: TSVGIconImageList;
    procedure CheckBox1Change(Sender: TObject);
    procedure TrackBar1Tracking(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  ScaledLayout1.Width := ScaledLayout1.OriginalWidth * Edit1.Text.ToInteger;
  ScaledLayout1.Height := ScaledLayout1.OriginalHeight * Edit1.Text.ToInteger;
end;

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
  begin
    ScaledLayout1.Width := ScaledLayout1.OriginalWidth * 2;
    ScaledLayout1.Height := ScaledLayout1.OriginalHeight * 2;
  end
  else
  begin
    ScaledLayout1.Width := ScaledLayout1.OriginalWidth;
    ScaledLayout1.Height := ScaledLayout1.OriginalHeight;
  end;
end;

procedure TForm2.TrackBar1Tracking(Sender: TObject);
begin
  ScaledLayout1.Width := ScaledLayout1.OriginalWidth * TrackBar1.Value/100;
  ScaledLayout1.Height := ScaledLayout1.OriginalHeight * TrackBar1.Value/100;

end;

end.
