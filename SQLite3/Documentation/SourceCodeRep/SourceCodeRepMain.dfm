object MainForm: TMainForm
  Left = 265
  Top = 235
  Width = 617
  Height = 543
  Caption = ' mORMot Source Code Repository Synch'
  Color = clBtnFace
  Constraints.MinHeight = 422
  Constraints.MinWidth = 617
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    601
    505)
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
    Top = 224
    Width = 95
    Height = 13
    Caption = 'Commit Description:'
  end
  object mmoStatus: TMemo
    Left = 16
    Top = 24
    Width = 569
    Height = 193
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
    Top = 240
    Width = 569
    Height = 178
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    Left = 168
    Top = 425
    Width = 113
    Height = 41
    Anchors = [akLeft, akBottom]
    Caption = 'Fossil Synch'
    TabOrder = 3
    OnClick = btnFossilSynchClick
  end
  object btnFullSynch: TButton
    Left = 472
    Top = 425
    Width = 113
    Height = 57
    Anchors = [akLeft, akBottom]
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
    Left = 304
    Top = 425
    Width = 113
    Height = 41
    Anchors = [akLeft, akBottom]
    Caption = 'Git Synch'
    TabOrder = 4
    OnClick = btnGitSynchClick
  end
  object btnRefreshStatus: TButton
    Left = 520
    Top = 5
    Width = 75
    Height = 18
    Anchors = [akTop, akRight]
    Caption = 'Refresh'
    TabOrder = 5
    OnClick = btnRefreshStatusClick
  end
  object btnGitShell: TButton
    Left = 304
    Top = 473
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Git Shell'
    TabOrder = 6
    OnClick = btnGitShellClick
  end
  object btnFossilShell: TButton
    Left = 208
    Top = 473
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Fossil Shell'
    TabOrder = 7
    OnClick = btnFossilShellClick
  end
  object btnTests: TButton
    Left = 16
    Top = 425
    Width = 113
    Height = 60
    Anchors = [akLeft, akBottom]
    Caption = 'Regression Tests'
    TabOrder = 8
    WordWrap = True
    OnClick = btnTestsClick
  end
  object btnCopyLink: TButton
    Left = 512
    Top = 222
    Width = 75
    Height = 18
    Caption = 'Copy Link'
    TabOrder = 9
    OnClick = btnCopyLinkClick
  end
  object btnGitAll: TButton
    Left = 384
    Top = 472
    Width = 73
    Height = 25
    Hint = 
      'Git Commit mORMot + SynPDF + SynMustache + LVCL + SynProject rep' +
      'ositories'
    Caption = 'Git ALL'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = btnGitSynchClick
  end
end
