object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  DesignSize = (
    624
    441)
  TextHeight = 15
  object bStart: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = bStartClick
  end
  object bStop: TButton
    Left = 177
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = bStopClick
  end
  object bCreate: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 2
    OnClick = bCreateClick
  end
  object aLog: TMemo
    Left = 8
    Top = 39
    Width = 608
    Height = 394
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
end
