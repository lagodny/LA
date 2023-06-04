unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.SearchBox,
  SynCrossPlatformJSON,
  LA.Net.Connector.Http,
  LA.Data.Link.Tracker,
  LA.Data.Tracker.Updater,
  La.Data.Tracking.Manager, FMX.TabControl, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox, FMX.Edit,
  FMX.ComboEdit, FMX.DateTimeCtrls, FMX.Objects, FMX.Layouts;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    bTestVariantData: TButton;
    bCreateTrackerLink: TButton;
    CheckBox1: TCheckBox;
    bTestManger: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ListView1: TListView;
    Button1: TButton;
    TabItem3: TTabItem;
    ComboBox1: TComboBox;
    cb: TComboEdit;
    bClearLog: TButton;
    Button2: TButton;
    eDeviceID: TEdit;
    dDate1: TDateEdit;
    dDate2: TDateEdit;
    ComboEdit1: TComboEdit;
    Image1: TImage;
    ListBox1: TListBox;
    procedure bTestVariantDataClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure bCreateTrackerLinkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bTestMangerClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbChange(Sender: TObject);
    procedure cbTyping(Sender: TObject);
    procedure bClearLogClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FManager: TLATrackingManager;

    FConnection: TLAHttpTrackingConnection;
    FUpdater: TLATrackerUpdater;
    FLink: TLATrackerLink;

    procedure DoDataChange(Sender: TObject);
    procedure DoException(const aStr: string);

    procedure Init;
    procedure Deinit;
    procedure InitManager;
    procedure DeinitManager;
    procedure Log(const aMsg: string);

    procedure InitListView;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses uDM;

procedure TForm4.bClearLogClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm4.bCreateTrackerLinkClick(Sender: TObject);
begin
  Deinit;
  Init;
end;

procedure TForm4.bTestMangerClick(Sender: TObject);
begin
  DeinitManager;
  InitManager;
end;

procedure TForm4.bTestVariantDataClick(Sender: TObject);
var
  r: string;
  v: Variant;
  vd: TJSONVariantData;
begin
  var aConnection: TLAHttpTrackingConnection := TLAHttpTrackingConnection.Create(Self);
  aConnection.Address := 'https://dc.tdc.org.ua';
  aConnection.UserName := 'demo';
  aConnection.Password := 'demo';
  aConnection.Connect;
  Log(aConnection.GetClients);
  Log(aConnection.GetDevices([]));
  r := aConnection.GetDevicesData([]);
  Log(r);
//  v := JSONVariant(r);
//  Log(v);
//  if v._1 <> null then
//    Log(v._1);
//  Log(v._3475);
//  Log(v.Value['_3475']);

  vd.Init(r);
  if VarIsEmpty(vd.Value['_']) then
//  if vd.Value['_'] = null then
    Log('_  = null')
  else
    Log(vd.Value['_']);

  Log(vd.Value['_3475']);

  aConnection.Free;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  var aItem := ListView1.Items.AddItem;
  aItem.Text := ListView1.Items.Count.ToString;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  r: string;
  v: Variant;
  vd: TJSONVariantData;
begin
  var aConnection: TLAHttpTrackingConnection := TLAHttpTrackingConnection.Create(Self);
//  aConnection.Address := '192.168.126.1:89';
  aConnection.Address :='https://dc.tdc.org.ua';
  aConnection.UserName := 'Старовойт';
  aConnection.Password := '1004';
  aConnection.Connect;
  Log(aConnection.GetClients);
  Log(aConnection.GetDevices([]));
  r := aConnection.GetDevicesData([]);
  Log(r);

  r := aConnection.GetTrack(eDeviceID.Text.ToInteger, DateTimeToUnix(dDate1.Date, False), DateTimeToUnix(dDate2.Date, False));
  r := StringReplace(r, '}', '}'#13, [rfReplaceAll]);
  Memo1.Text := r;
//  Log(r);

//  vd.Init(r);
//  if VarIsEmpty(vd.Value['_']) then
////  if vd.Value['_'] = null then
//    Log('_  = null')
//  else
//    Log(vd.Value['_']);
//
//  Log(vd.Value['_3475']);

  aConnection.Free;
end;

procedure TForm4.CheckBox1Change(Sender: TObject);
begin
  FUpdater.Active := CheckBox1.IsChecked;
end;

procedure TForm4.cbChange(Sender: TObject);
var
  s:Integer;
  tmpstr:string;
  suggestions: TStrings;
begin
  suggestions := TStringList.Create;
  suggestions.Add('1234');
  suggestions.Add('2993');
  suggestions.Add('1234');
  suggestions.Add('3421');
  suggestions.Add('6545');
  suggestions.Add('1234');
  suggestions.Add('7657');
  suggestions.Add('1234');
  suggestions.Add('1234');
  suggestions.Add('1234');
  suggestions.Add('1234');
  suggestions.Add('1234');
  suggestions.Add('1234');
  suggestions.Add('9879');
  suggestions.Add('1234');
  //suggestions: tstringlist
//  cb.AutoComplete:=false;
  tmpstr:=cb.Text;
  cb.Items.Clear;
  for s:=0 to suggestions.Count - 1 do
    if Pos(tmpstr, suggestions[s]) > 0 then

//    if StringMatches(suggestions[s],cb.Text+'*') then
      cb.Items.Add(suggestions[s]);
  if (cb.Items.Count<>0) and (Length(cb.Text)<>0) then
  begin
    cb.DropDownCount := cb.Items.Count;
    if not cb.DroppedDown then
      cb.DropDown
  end
  else if cb.DroppedDown then
    cb.DropDown;

  cb.Text:=tmpstr;
  cb.SelStart:=Length(cb.Text)
end;
procedure TForm4.cbTyping(Sender: TObject);
begin
  cbChange(Sender);
end;

procedure TForm4.Deinit;
begin
  FLink.Free;
  FUpdater.Free;
  FConnection.Free;
end;

procedure TForm4.DeinitManager;
begin
  if not Assigned(FManager) then
    Exit;
  FManager.Updater.Free;
  FManager.Connector.Free;
  FManager.Free;
  FManager := nil;
end;

procedure TForm4.DoDataChange(Sender: TObject);
begin
  if not (TLATrackerLink(Sender).Name = 'N3') then
    Exit;

  Log(TLATrackerLink(Sender).Name + ' data changed: ' + TLATrackerLink(Sender).Data);
end;

procedure TForm4.DoException(const aStr: string);
begin
  Memo1.Lines.Add('Ecxeption: ' + aStr);
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  Deinit;
  DeinitManager;
end;

procedure TForm4.Init;
var
  r: string;
begin
  FConnection := TLAHttpTrackingConnection.Create(Self);
  FConnection.Address := 'https://dc.tdc.org.ua';
  FConnection.UserName := 'Старовойт';
  FConnection.Password := '1004';
  FConnection.Connect;
  r := FConnection.GetDevices([]);
  Log(r);

  FUpdater := TLATrackerUpdater.Create(Self);
  FUpdater.Connector := FConnection;

  FLink := TLATrackerLink.Create(Self);
  FLink.ID := '3475';
  FLink.DataSource := FUpdater;
end;

procedure TForm4.InitListView;
begin
  var aItem := ListView1.Items.AddItem;
  aItem.Text := ListView1.Items.Count.ToString;
end;

procedure TForm4.InitManager;
begin
  FManager := TLATrackingManager.Create(nil);
  FManager.Connector := TLAHttpTrackingConnection.Create(nil);
//  FManager.Connector.Address := 'https://dc.tdc.org.ua';
  FManager.Connector.Address := '192.168.126.1:89';
//  FManager.Connector.UserName := 'demo';
//  FManager.Connector.Password := 'demo';
//  FManager.Connector.UserName := 'Лагодный';
//  FManager.Connector.Password := '314';
  FManager.Connector.UserName := 'Старовойт';
  FManager.Connector.Password := '1004';
  FManager.Connector.Connect;

  FManager.Updater := TLATrackerUpdater.Create(nil);
  FManager.Updater.Connector := FManager.Connector;
  FManager.Updater.OnException := DoException;

  FManager.InitClients;
  FManager.InitDevices;
  for var t in FManager.Devices.Values do
  begin
    t.Link.OnDataChange := DoDataChange
  end;

  FManager.Updater.Active := True;

end;

procedure TForm4.Log(const aMsg: string);
begin
  Memo1.Lines.Add(aMsg);
end;

end.
