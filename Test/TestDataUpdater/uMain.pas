unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  LA.Data.Updater;

type
  TForm1 = class(TForm)
    bStart: TButton;
    bStop: TButton;
    bCreate: TButton;
    aLog: TMemo;
    procedure bStartClick(Sender: TObject);
    procedure bCreateClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
  private
    FUpdater: TDataUpdater;
  public
    procedure DoUpdate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bCreateClick(Sender: TObject);
begin
  FUpdater := TDataUpdater.Create(Self);
  FUpdater.OnUpdate := DoUpdate;
  FUpdater.Interval := 100;
end;

procedure TForm1.bStartClick(Sender: TObject);
begin
  FUpdater.Active := True;
end;

procedure TForm1.bStopClick(Sender: TObject);
begin
  FUpdater.Active := False;
end;

procedure TForm1.DoUpdate(Sender: TObject);
begin
  aLog.Lines.Add(FormatDateTime('hh:mm:ss:zzz', Now));
end;

end.
