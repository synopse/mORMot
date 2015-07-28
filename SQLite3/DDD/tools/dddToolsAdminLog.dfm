object LogFrame: TLogFrame
  Left = 0
  Top = 0
  Width = 516
  Height = 367
  TabOrder = 0
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 278
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      145
      278)
    object edtSearch: TEdit
      Left = 8
      Top = 16
      Width = 101
      Height = 21
      Hint = 'Search (Ctrl+F, F3 for next) '
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Visible = False
      OnChange = btnSearchNextClick
    end
    object btnSearchNext: TButton
      Left = 113
      Top = 14
      Width = 20
      Height = 23
      Hint = 'Search Next (F3)'
      Anchors = [akTop, akRight]
      Caption = '?'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Visible = False
      OnClick = btnSearchNextClick
    end
    object chklstEvents: TCheckListBox
      Left = 8
      Top = 48
      Width = 129
      Height = 105
      ItemHeight = 13
      PopupMenu = pmFilter
      Style = lbOwnerDrawFixed
      TabOrder = 2
      OnClick = chklstEventsClick
      OnDrawItem = chklstEventsDrawItem
    end
    object btnStartLog: TButton
      Left = 16
      Top = 16
      Width = 113
      Height = 25
      Caption = 'Start Logging'
      TabOrder = 3
      OnClick = btnStartLogClick
    end
  end
  object pnlRight: TPanel
    Left = 145
    Top = 0
    Width = 371
    Height = 278
    Align = alClient
    TabOrder = 1
    object spl1: TSplitter
      Left = 1
      Top = 1
      Height = 276
    end
    object drwgrdEvents: TDrawGrid
      Left = 4
      Top = 1
      Width = 366
      Height = 276
      Align = alClient
      ColCount = 3
      DefaultColWidth = 100
      DefaultRowHeight = 14
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect, goThumbTracking]
      TabOrder = 0
      Visible = False
      OnClick = drwgrdEventsClick
      OnDrawCell = drwgrdEventsDrawCell
    end
  end
  object mmoBottom: TMemo
    Left = 0
    Top = 278
    Width = 516
    Height = 89
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object pmFilter: TPopupMenu
    Left = 96
    Top = 112
  end
  object tmrRefresh: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrRefreshTimer
    Left = 153
    Top = 32
  end
end
