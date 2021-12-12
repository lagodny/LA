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
    Left = 40
    Top = 104
    object LASensor1: TLASensor
      SensorList = LASensorList1
      ID = '4'
      Value = '124554'
      Enabled = True
    end
    object LASensor2: TLASensor
      SensorList = LASensorList1
      ID = '3'
      Value = 'Label2'
      Enabled = True
    end
    object LASensor3: TLASensor
      SensorList = LASensorList1
      ID = '1'
      Value = 'Label2'
      Enabled = False
    end
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 20
    Top = 5
    object LinkControlToPropertyCaption: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = LASensor1
      Component = Label1
      ComponentProperty = 'Caption'
      InitializeControlValue = False
    end
    object LinkControlToPropertyActive: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = CheckBox1
      Track = True
      Component = DataUpdater1
      ComponentProperty = 'Active'
    end
    object LinkControlToPropertyCaption2: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = LASensor2
      Component = Label2
      ComponentProperty = 'Caption'
    end
    object LinkControlToPropertyCaption3: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = LASensor3
      Component = Label3
      ComponentProperty = 'Caption'
    end
  end
  object DCHttpConnector1: TDCHttpConnector
    Address = 'https://dc.tdc.org.ua'
    ConnectTimeOut = 0
    ReadTimeOut = 0
    CompressionLevel = 0
    UserName = 'demo'
    Password = 'demo'
    Connected = False
    Https = False
    SendTimeOut = 0
    Left = 40
    Top = 168
  end
  object DataUpdater1: TDataUpdater
    Connector = DCHttpConnector1
    Interval = 1000
    Left = 400
    Top = 136
  end
end
