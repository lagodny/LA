object DMData: TDMData
  Height = 480
  Width = 608
  object Updater: TLASensorUpdater
    Connector = Connector
    Left = 288
    Top = 224
  end
  object Connector: TLATCPConnector
    SendTimeOut = 0
    CompressionLevel = 0
    Language = 'ru'
    Left = 288
    Top = 288
  end
  object HisroryViewer: TLAHistoryViewer
    Date1 = 45253.000000000000000000
    Date2 = 45254.000000000000000000
    Left = 288
    Top = 160
  end
end
