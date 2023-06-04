unit LA.FMX.Dialog.Interval;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  LA.Interval,
  LA.FMX.Frame.Interval;

type
  TIntervalDlg = class(TForm)
    frInterval: TLAIntervalFrame;
    pButton: TPanel;
    bOK: TButton;
    bCancel: TButton;
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
  private
    FApplyProc: TProc;
  public
    class procedure Execute(aInterval: TLAInterval; aApplyProc: TProc);
  public
    property ApplyProc: TProc read FApplyProc write FApplyProc;
  end;


implementation

{$R *.fmx}

{ TIntervalDlg }

procedure TIntervalDlg.bCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TIntervalDlg.bOKClick(Sender: TObject);
begin
  frInterval.ViewToInterval;
  TLAInterval.LastInterval := frInterval.Interval;
  if Assigned(ApplyProc) then
    ApplyProc;
  Close;
end;

class procedure TIntervalDlg.Execute(aInterval: TLAInterval; aApplyProc: TProc);
var
  f: TIntervalDlg;
begin
  f := TIntervalDlg.Create(nil);
  f.ApplyProc := aApplyProc;
  f.frInterval.Interval := aInterval;
  f.frInterval.IntervalToView;
  f.ShowModal;
end;

procedure TIntervalDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

end.
