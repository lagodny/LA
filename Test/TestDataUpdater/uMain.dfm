object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 1053
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  DesignSize = (
    1053
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
    Width = 1037
    Height = 394
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnDblClick = aLogDblClick
  end
  object bTestSessionCache: TButton
    Left = 288
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Test session cache'
    TabOrder = 4
    OnClick = bTestSessionCacheClick
  end
  object bTestNoSID: TButton
    Left = 439
    Top = 8
    Width = 130
    Height = 25
    Caption = 'No SIDs request'
    TabOrder = 5
    OnClick = bTestNoSIDClick
  end
  object bTestMany: TButton
    Left = 575
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Many (cache)'
    TabOrder = 6
    OnClick = bTestManyClick
  end
  object eItemCount: TEdit
    Left = 656
    Top = 8
    Width = 81
    Height = 23
    TabOrder = 7
    Text = '100'
  end
  object bManyNoCache: TButton
    Left = 743
    Top = 8
    Width = 114
    Height = 25
    Caption = 'Many (no cache)'
    TabOrder = 8
    OnClick = bManyNoCacheClick
  end
end
