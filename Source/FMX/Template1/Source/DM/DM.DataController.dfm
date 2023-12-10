object DataController: TDataController
  Height = 480
  Width = 608
  object Updater: TLASensorUpdater
    Connector = Connector
    Left = 288
    Top = 224
  end
  object Connector: TLATCPConnector
    Address = '193.109.249.118:5152'
    SendTimeOut = 0
    CompressionLevel = 0
    UserName = #1051#1072#1075#1086#1076#1085#1099#1081
    Password = '314'
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
  object Manager: TLAConnectionManager
    Items = <
      item
        Name = 'default'
        Connector = Connector
        Updater = Updater
      end>
    HistoryViewer = HisroryViewer
    Left = 432
    Top = 288
  end
end
