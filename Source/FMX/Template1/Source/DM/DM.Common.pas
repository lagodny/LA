unit DM.Common;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList, FMX.Types, FMX.Controls, FMX.Gestures, FMX.SVGIconImageList;

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
    MainStyleBook: TStyleBook;
  end;

var
  DMCommon: TDMCommon;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
