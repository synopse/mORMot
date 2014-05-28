object Form1: TForm1
  Left = 317
  Top = 279
  Width = 490
  Height = 214
  Caption = ' mORMot Client using VCL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 16
    Top = 16
    Width = 87
    Height = 13
    Caption = 'Enter some value:'
  end
  object lbl2: TLabel
    Left = 16
    Top = 72
    Width = 82
    Height = 13
    Caption = 'Computed JSON:'
  end
  object edtValue: TEdit
    Left = 16
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = edtValueChange
  end
  object mmoJSON: TMemo
    Left = 16
    Top = 88
    Width = 425
    Height = 65
    TabOrder = 1
  end
  object btnRewind: TButton
    Left = 208
    Top = 32
    Width = 75
    Height = 25
    Caption = 'First'
    TabOrder = 2
    OnClick = btnNextClick
  end
  object btnNext: TButton
    Left = 288
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 3
    OnClick = btnNextClick
  end
end
