object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 160
    Top = 64
    Width = 34
    Height = 15
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 160
    Top = 85
    Width = 34
    Height = 15
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 160
    Top = 106
    Width = 34
    Height = 15
    Caption = 'Label2'
  end
  object CheckBox1: TCheckBox
    Left = 160
    Top = 32
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 0
  end
  object LASensorList1: TLASensorList
    DataSource = DataUpdater1
    Left = 40
    Top = 104
    object LASensor1: TLASensor
      Link.ID = '1'
      Link.DataSource = DataUpdater1
      SensorList = LASensorList1
    end
  end
  object DCHttpConnector1: TLAHttpConnector
    Address = 'https://dc.tdc.org.ua'
    ConnectTimeOut = 0
    ReadTimeOut = 0
    CompressionLevel = 0
    UserName = 'demo'
    Password = 'demo'
    Https = False
    SendTimeOut = 0
    Left = 40
    Top = 168
  end
  object DataUpdater1: TLADataUpdater
    Connector = DCHttpConnector1
    Left = 48
    Top = 236
  end
end
