unit Frame.LongRunButton;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects;

type
  TThreadFailProcedure = reference to procedure (const aMsg: string);

  TLongRunButtonFrame = class(TFrame)
    RunIndicator: TAniIndicator;
    RunButton: TButton;
    procedure DefFailProc(e: Exception);
  public
    procedure ShowIndicator;
    procedure HideIndicator;
    procedure RunAsync(const aProc, aSucces: TThreadProcedure; aFail: TThreadFailProcedure);
  end;


implementation

{$R *.fmx}

{ TLongRunButtonFrame }

procedure TLongRunButtonFrame.DefFailProc(e: Exception);
begin
//  Enabled := True;
//  RunIndicator.Visible := False;
//  RunIndicator.Enabled := False;
  HideIndicator;
  ShowMessage(e.Message);
end;

procedure TLongRunButtonFrame.HideIndicator;
begin
  RunIndicator.Visible := False;
  RunIndicator.Enabled := False;
end;

procedure TLongRunButtonFrame.RunAsync(const aProc, aSucces: TThreadProcedure; aFail: TThreadFailProcedure);
begin
  //Enabled := False;
  ShowIndicator;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        //Sleep(3000);
        aProc;
        if Assigned(aSucces) then
          TThread.Synchronize(nil, aSucces);
      except
        on e: Exception do
          if Assigned(aFail) then
            TThread.Synchronize(nil, procedure
            begin
              aFail(e.Message);
            end)
          else
            TThread.Synchronize(nil, procedure
            begin
              DefFailProc(e);
            end);
      end;
//      TThread.Synchronize(nil, procedure
//        begin
//          RunIndicator.Enabled := False;
//          RunIndicator.Visible := False;
//          Enabled := True;
//        end
//      );
    end
  ).Start;

end;

procedure TLongRunButtonFrame.ShowIndicator;
begin
  RunIndicator.Visible := True;
  RunIndicator.Enabled := True;
end;

end.
