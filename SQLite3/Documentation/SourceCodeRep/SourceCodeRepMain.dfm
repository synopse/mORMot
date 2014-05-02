object MainForm: TMainForm
  Left = 265
  Top = 235
  Width = 617
  Height = 422
  Caption = ' mORMot Source Code Repository Synch'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    601
    384)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 24
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Pending Files:'
  end
  object lbl2: TLabel
    Left = 24
    Top = 176
    Width = 95
    Height = 13
    Caption = 'Commit Description:'
  end
  object mmoStatus: TMemo
    Left = 16
    Top = 24
    Width = 569
    Height = 145
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object mmoDescription: TMemo
    Left = 16
    Top = 192
    Width = 569
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnFossilSynch: TButton
    Left = 40
    Top = 312
    Width = 113
    Height = 41
    Caption = 'Fossil Synch'
    TabOrder = 3
    OnClick = btnFossilSynchClick
  end
  object btnFullSynch: TButton
    Left = 472
    Top = 312
    Width = 113
    Height = 57
    Caption = 'Fossil and Git Synch'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    WordWrap = True
    OnClick = btnFullSynchClick
  end
  object btnGitSynch: TButton
    Left = 256
    Top = 312
    Width = 113
    Height = 41
    Caption = 'Git Synch'
    TabOrder = 4
    OnClick = btnGitSynchClick
  end
  object btnRefreshStatus: TButton
    Left = 520
    Top = 5
    Width = 67
    Height = 17
    Caption = 'Refresh'
    TabOrder = 5
    OnClick = btnRefreshStatusClick
  end
  object btnGitShell: TButton
    Left = 376
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Git Shell'
    TabOrder = 6
    OnClick = btnGitShellClick
  end
  object btnFossilShell: TButton
    Left = 160
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Fossil Shell'
    TabOrder = 7
    OnClick = btnFossilShellClick
  end
end
