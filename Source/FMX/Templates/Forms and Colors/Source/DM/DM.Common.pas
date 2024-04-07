unit DM.Common;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList, FMX.Types, FMX.Controls, FMX.Gestures, FMX.SVGIconImageList,
  System.Messaging;

type
  TDMCommon = class(TDataModule)
    Img1: TImageList;
    img16x16: TImageList;
    Img: TImageList;
    img16: TImageList;
    SVGIconImageList1: TSVGIconImageList;
    imgMap: TSVGIconImageList;
    SVGImages: TSVGIconImageList;
    MapImg: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure StyleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  end;

var
  DMCommon: TDMCommon;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDMCommon.DataModuleCreate(Sender: TObject);
begin
//  FStyleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

procedure TDMCommon.StyleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
//  if (Msg is TStyleChangedMessage) and (not (csDestroying in ComponentState)) then
//  begin
////    DoStyleChanged;
//    if csLoading in ComponentState then
//      Exit;
//    if csDestroying in ComponentState then
//      Exit;
//    if not (csLoading in ComponentState) then
//      ApplyStyleLookup;
//
//  end;
end;

end.
