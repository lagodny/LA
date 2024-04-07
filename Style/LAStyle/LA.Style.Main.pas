unit LA.Style.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Ani, FMX.Objects,
  FMX.Styles,
  //LA.System.Classes,
  LA.FMX.Prop.Utils,
  LA.FMX.UI.Consts, LA.Style.Frame, System.ImageList, FMX.ImgList, FMX.SVGIconImageList, LA.FMX.Colors, FMX.ListBox;

type
  TForm9 = class(TForm)
    MainStyleBook: TStyleBook;
    LAStyleBook: TStyleBook;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Rectangle1: TRectangle;
    PanelDark: TPanel;
    PanelLight: TPanel;
    Panel6: TPanel;
    Text1: TText;
    TestFrame1: TTestFrame;
    Text2: TText;
    Text3: TText;
    SVGImages: TSVGIconImageList;
    Button1: TButton;
    StyleBook1: TStyleBook;
    Button2: TButton;
    ImageList1: TImageList;
    Glyph1: TGlyph;
    Glyph2: TGlyph;
    Glyph3: TGlyph;
    ComboBox1: TComboBox;
    LAColorManager1: TLAColorManager;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanelDarkClick(Sender: TObject);
    procedure PanelLightClick(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  protected
    FReloadResMode: Boolean;
    function GetAppFilesPath: string;
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

uses
  System.Diagnostics,
  System.IOUtils,
  LA.Style.Second;

procedure TForm9.Button1Click(Sender: TObject);
begin
//  StyleBook := MainStyleBook;
  InitLAColorsFromFile('LA_Test.json', 'light');
//  LAStyleBook.LoadFromFile('MainStyle.style');
//  StyleBook.ResourceChanged(nil);
//  StyleBook.LoadFromFile('LAStyle.style');

//  Form10 := TForm10.Create(Self);
//  Form10.ShowModal;
  TStyleManager.SetStyleFromFile('LAStyle.style');
end;


procedure TForm9.ComboBox1Change(Sender: TObject);
begin
  LAColorManager1.Scheme := ComboBox1.Text;
  LAColorManager1.Apply;
  TStyleManager.SetStyleFromFile(TPath.Combine(GetAppFilesPath, 'LAStyle.style'));
//    TStyleManager.UpdateScenes;
  SVGImages.RefreshAllIcons;


//  InitLAColorsFromFile(TPath.Combine(GetAppFilesPath, 'LA_Test.json'), ComboBox1.Text);
////  TStyleManager.SetStyleFromFile(TPath.Combine(GetAppFilesPath, 'LAStyle.style'));
////  MainStyleBook.
//
//  TPropertyReloader.Reload;
//
//  TStyleManager.UpdateScenes;
//  SVGImages.RefreshAllIcons;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  StyleBook := nil;
//  TStyleManager.SetStyleFromFile('MainStyle.style');
end;

function TForm9.GetAppFilesPath: string;
begin
  {$IFDEF WINDOWS}
  Result := '';
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := TPath.GetHomePath;
  {$ENDIF}
  {$IFDEF IOS}
  Result := TPath.GetDocumentsPath;
  {$ENDIF}
end;

procedure TForm9.PanelDarkClick(Sender: TObject);
begin
//  InitLAColorsFromFile('LA_Test.json', True);
//  TStyleManager.SetStyleFromFile('LAStyle.style');
//  {$IFDEF WINDOWS}
//  InitLAColorsFromFile('LA_Test.json', True);
//  TStyleManager.SetStyleFromFile('LAStyle.style');
//  {$ENDIF}
//  {$IFDEF ANDROID}
//  InitLAColorsFromFile(TPath.Combine(TPath.GetHomePath, 'LA_Test.json'), True);
//  TStyleManager.SetStyleFromFile(TPath.Combine(TPath.GetHomePath, 'LAStyle.style'));
//  {$ENDIF}
  InitLAColorsFromFile(TPath.Combine(GetAppFilesPath, 'LA_Test.json'), 'dark');
  TPropertyReloader.Reload;
  TStyleManager.SetStyleFromFile(TPath.Combine(GetAppFilesPath, 'LAStyle.style'));
//  TStyleManager.UpdateScenes;

 // var s := TStopwatch.StartNew;
  SVGImages.RefreshAllIcons;
//  ShowMessage(s.ElapsedMilliseconds.ToString);
end;

procedure TForm9.PanelLightClick(Sender: TObject);
begin
//  {$IFDEF WINDOWS}
//  InitLAColorsFromFile('LA_Test.json', False);
//  TStyleManager.SetStyleFromFile('LAStyle.style');
//  {$ENDIF}
//  {$IFDEF ANDROID}
//  InitLAColorsFromFile(TPath.Combine(TPath.GetHomePath, 'LA_Test.json'), False);
//  TStyleManager.SetStyleFromFile(TPath.Combine(TPath.GetHomePath, 'LAStyle.style'));
//  {$ENDIF}
  InitLAColorsFromFile(TPath.Combine(GetAppFilesPath, 'LA_Test.json'), 'light');
  TPropertyReloader.Reload;
  TStyleManager.SetStyleFromFile(TPath.Combine(GetAppFilesPath, 'LAStyle.style'));
//  TStyleManager.UpdateScenes;

  SVGImages.RefreshAllIcons;
end;

procedure TForm9.Panel6Click(Sender: TObject);
begin
  TForm10.Create(Self).Show;
end;

end.
