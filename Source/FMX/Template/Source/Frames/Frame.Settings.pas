unit Frame.Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, Frame.Toolbar, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  FMX.Layouts, FMX.Edit, FMX.ComboEdit, FMX.ListBox, FMX.DialogService, FMX.DialogService.Async,
  DKLang,
  Data.Settings;

type
  TSettingsFrame = class(TToolbarFrame)
    lbSettings: TListBox;
    lng: TDKLanguageController;
    lbiLanguage: TListBoxItem;
    cbLanguage: TComboBox;
    lbiConnection: TListBoxItem;
    eConnection: TEdit;
    procedure cbLanguageChange(Sender: TObject);
//    procedure SendDataSwitchClick(Sender: TObject);
//    procedure lbiSendLocationClick(Sender: TObject);
  private
    FViewUpdating: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ModelToView;
    procedure ViewToModel;
  end;


implementation

uses
  App.State, App.State.Controller;

{$R *.fmx}

{ TSettingsFrame }

procedure TSettingsFrame.cbLanguageChange(Sender: TObject);
var
  aLangIndex: Integer;
begin
  if FViewUpdating then
    Exit;

  aLangIndex := cbLanguage.ItemIndex;
  if aLangIndex < 0 then
    aLangIndex := 0;

  LangManager.LanguageID := LangManager.LanguageIDs[aLangIndex];
  ViewToModel;
end;

constructor TSettingsFrame.Create(AOwner: TComponent);
begin
  inherited;

  cbLanguage.Clear;
  for var i := 0 to LangManager.LanguageCount - 1 do
    cbLanguage.Items.Add(LangManager.LanguageNativeNames[i]);

  ModelToView;
end;

destructor TSettingsFrame.Destroy;
begin
  ViewToModel;
  inherited;
end;

////  lbiConnection.ItemData.Detail :=
//  TDialogService.InputQuery('Connection', [''], [Settings.Addr],
////  InputBox('', '', Settings.Addr,
//    procedure(const AResult: TModalResult; const AValues: array of string)
//    begin
//      if AResult = mrOK then
//        lbiConnection.ItemData.Detail := aValues[0];
//    end
//  );
////  TInputCloseQueryProc
//end;

//procedure TSettingsFrame.lbiSendLocationClick(Sender: TObject);
//begin
//  AppStateController.AppState := TAppState.asSenderSettings;
//end;

procedure TSettingsFrame.ModelToView;
begin
  FViewUpdating := True;
  cbLanguage.ItemIndex := LangManager.IndexOfLanguageID(Settings.Language);
  eConnection.Text := Settings.Addr;
  FViewUpdating := False;
end;

//procedure TSettingsFrame.SendDataSwitchClick(Sender: TObject);
//begin
//  Settings.SendData := SendDataSwitch.IsChecked;
//  Settings.SetSendData(Settings.SendData);
//end;

procedure TSettingsFrame.ViewToModel;
begin
  var aLangIndex := cbLanguage.ItemIndex;
  if aLangIndex < 0 then
    aLangIndex := 0;
  Settings.Language := LangManager.LanguageIDs[aLangIndex];
//  Settings.SendData := SendDataSwitch.IsChecked;
  Settings.Addr := eConnection.Text;

  Settings.SaveSettings;
end;

end.
