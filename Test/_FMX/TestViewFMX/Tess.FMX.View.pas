unit Tess.FMX.View;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, LA.Data.Source, LA.Data.Updater, LA.Data.Sensor.Updater, LA.Net.Connector,
  LA.Net.Connector.Http, FMX.Objects, LA.FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, LA.Net.Connector.Tcp, Radiant.Shapes, FMX.Edit,
  FMX.Layouts, LA.FMX.StdCtrls, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Data.Bind.Components, LA.Data.Sensor,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, FMX.SVGIconImage, FMX.Ani, FMX.Menus, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  LA.Data.History.Viewer;

type
  TForm1 = class(TForm)
    LASensorUpdater1: TLASensorUpdater;
    LAHttpConnector1: TLAHttpConnector;
    LATCPConnector1: TLATCPConnector;
    LASensorUpdater2: TLASensorUpdater;
    ScrollBox1: TScrollBox;
    ScaledLayout1: TScaledLayout;
    Rectangle1: TRectangle;
    LAText1: TLAText;
    RadiantCapsule1: TRadiantCapsule;
    LAText3: TLAText;
    Rectangle2: TRectangle;
    LAText4: TLAText;
    Rectangle3: TRectangle;
    LAText5: TLAText;
    Rectangle4: TRectangle;
    LAText6: TLAText;
    Rectangle5: TRectangle;
    LAText7: TLAText;
    Layout1: TLayout;
    TrackBar1: TTrackBar;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    LAText2: TLAText;
    LALabel1: TLALabel;
    LALabel2: TLALabel;
    StyleBook1: TStyleBook;
    ImageList2: TImageList;
    eIndex: TEdit;
    Button1: TButton;
    Glyph1: TGlyph;
    LALabel3: TLALabel;
    SVGIconImageList1: TSVGIconImageList;
    Path1: TPath;
    LAGlyph1: TLAGlyph;
    LAGlyph2: TLAGlyph;
    Glyph2: TGlyph;
    LAGlyph3: TLAGlyph;
    RadiantRectangle1: TRadiantRectangle;
    Layout2: TLayout;
    LAGlyph4: TLAGlyph;
    FloatAnimation1: TFloatAnimation;
    Text1: TText;
    LAGlyph5: TLAGlyph;
    Rectangle6: TRectangle;
    FloatAnimation2: TFloatAnimation;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    bStream: TButton;
    Memo1: TMemo;
    bGetLookup: TButton;
    bTestHistory: TButton;
    LAHistoryViewer1: TLAHistoryViewer;
    LAText8: TLAText;
    LAText9: TLAText;
    tbHistory: TTrackBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure TrackBar1Tracking(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Layout2Click(Sender: TObject);
    procedure LAGlyph5Click(Sender: TObject);
    procedure Rectangle6Click(Sender: TObject);
    procedure bStreamClick(Sender: TObject);
    procedure bGetLookupClick(Sender: TObject);
    procedure bTestHistoryClick(Sender: TObject);
    procedure LAText9Click(Sender: TObject);
    procedure tbHistoryTracking(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Diagnostics,
  System.DateUtils;

{$R *.fmx}

procedure TForm1.bGetLookupClick(Sender: TObject);
begin
  Memo1.Lines.Add(LAHTTPConnector1.GetLookup('states'));
end;

procedure TForm1.bStreamClick(Sender: TObject);
var
  s: TMemoryStream;
  t: string;
  d: TDateTime;
  v: Double;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.BeginUpdate;
  s := TMemoryStream.Create;
  try
    var sw := TStopwatch.Create;
    sw.Start;
    //for var i := 1 to 1000 do
    begin
      LAHttpConnector1.
//      LATCPConnector1.
      SensorHistoryStream(s, '4', DateTimeToUnix(Now - 1), 0, True, False, False, True, True);
    end;
    Memo1.Lines.Add(sw.ElapsedMilliseconds.ToString);
    while s.Position < s.Size do
    begin
      s.ReadData<TDateTime>(d);
      s.ReadData<Double>(v);
      Memo1.Lines.Add(DateTimeToStr(d) + ' : ' + FloatToStr(v));
    end;
  finally
    s.Free;
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TForm1.bTestHistoryClick(Sender: TObject);
begin
  LAHistoryViewer1.FillHistory(LAHistoryViewer1.Date1, LAHistoryViewer1.Date2);
  tbHistory.Max := LAHistoryViewer1.Date2;
  tbHistory.Min := LAHistoryViewer1.Date1;
  tbHistory.Value := tbHistory.Min;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  LAGlyph1.Link.Value := eIndex.Text.ToInteger;
  LAGlyph1.DoDataLinkChanged(Self);
//  LAGlyph1.Link.Notify;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  // 192.168.126.1:89
  LAHttpConnector1.Address := Edit1.Text;
  LASensorUpdater1.Active := CheckBox1.IsChecked;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  LATCPConnector1.Address := Edit2.Text;
  LASensorUpdater2.Active := CheckBox3.IsChecked;
end;

procedure TForm1.LAGlyph5Click(Sender: TObject);
begin
  ShowMessage('glyph click')
end;

procedure TForm1.LAText9Click(Sender: TObject);
begin
  LAHistoryViewer1.Next;
end;

procedure TForm1.Layout2Click(Sender: TObject);
begin
  FloatAnimation1.Start;
end;

procedure TForm1.Rectangle6Click(Sender: TObject);
begin
  ShowMessage('rectangle click');
end;

procedure TForm1.tbHistoryTracking(Sender: TObject);
begin
  LAHistoryViewer1.CurrentMoment := tbHistory.Value;
end;

procedure TForm1.TrackBar1Tracking(Sender: TObject);
begin
//  ScaledLayout1.Scale.X := TrackBar1.Value/100;
//  ScaledLayout1.Scale.Y := TrackBar1.Value/100;
  ScaledLayout1.Width := ScaledLayout1.OriginalWidth * TrackBar1.Value/100;
  ScaledLayout1.Height := ScaledLayout1.OriginalHeight * TrackBar1.Value/100;
//  SVGIconImageList1.RefreshAllIcons;
//  ScrollBox1.RealignContent;
//  Glyph2.Width := 16 * TrackBar1.Value/100;
//  Glyph2.Height := Glyph2.Width;
end;

end.
