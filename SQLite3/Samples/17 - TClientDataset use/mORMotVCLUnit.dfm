object Form1: TForm1
  Left = 192
  Top = 124
  Width = 754
  Height = 419
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = chkFromSQLClick
  PixelsPerInch = 96
  TextHeight = 13
  object dbgrdData: TDBGrid
    Left = 0
    Top = 41
    Width = 738
    Height = 340
    Align = alClient
    DataSource = ds1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 738
    Height = 41
    Align = alTop
    TabOrder = 1
    object lblTiming: TLabel
      Left = 456
      Top = 16
      Width = 241
      Height = 13
      AutoSize = False
    end
    object chkFromSQL: TCheckBox
      Left = 32
      Top = 16
      Width = 137
      Height = 17
      Caption = 'From SQL'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkFromSQLClick
    end
    object chkViaTClientDataSet: TCheckBox
      Left = 216
      Top = 16
      Width = 193
      Height = 17
      Caption = 'Via TClientDataSet'
      TabOrder = 1
      OnClick = chkFromSQLClick
    end
  end
  object ds1: TDataSource
    Left = 96
    Top = 72
  end
end
