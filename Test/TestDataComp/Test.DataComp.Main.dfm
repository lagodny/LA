object DataCompMainForm: TDataCompMainForm
  Left = 0
  Top = 0
  Caption = 'Test data components'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 304
    Top = 232
    Width = 30
    Height = 15
    Caption = '32113'
  end
  object Edit1: TEdit
    Left = 109
    Top = 24
    Width = 121
    Height = 23
    TabOrder = 0
    OnChange = Edit1Change
  end
  object bEd2Label: TButton
    Left = 236
    Top = 23
    Width = 157
    Height = 25
    Caption = 'Edit text to label caption'
    TabOrder = 1
    OnClick = bEd2LabelClick
  end
  object bAddLink: TButton
    Left = 119
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Add link'
    TabOrder = 2
    OnClick = bAddLinkClick
  end
  object chActive: TCheckBox
    Left = 200
    Top = 99
    Width = 97
    Height = 17
    Caption = 'Active'
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 440
    Top = 24
    Width = 121
    Height = 23
    TabOrder = 4
    Text = 'Edit2'
  end
  object DCHttpConnector1: TDCHttpConnector
    Address = 'localhost:89'
    ConnectTimeOut = 0
    ReadTimeOut = 0
    CompressionLevel = 0
    UserName = 'demo'
    Password = 'demo'
    Connected = False
    Https = False
    SendTimeOut = 0
    Left = 168
    Top = 152
  end
  object DataUpdater1: TDataUpdater
    Connector = DCHttpConnector1
    Interval = 1000
    Left = 168
    Top = 224
  end
  object DCSensor1: TDCSensor
    ID = '4'
    Data = '21'
    Enabled = True
    Left = 296
    Top = 152
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 20
    Top = 5
    object LinkControlToPropertyActive: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chActive
      Track = True
      Component = DataUpdater1
      ComponentProperty = 'Active'
    end
  end
end
