object Form1: TForm1
  Left = 341
  Top = 83
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'FISH FACTS'
  ClientHeight = 615
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DBLabel1: TDBText
    Left = 8
    Top = 230
    Width = 121
    Height = 24
    Alignment = taCenter
    DataField = 'Common_Name'
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -19
    Font.Name = 'MS Serif'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 211
    Height = 216
    Hint = 'Scroll grid below to see other fish'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object img: TImage
      Left = 1
      Top = 1
      Width = 209
      Height = 214
      Align = alClient
      Stretch = True
    end
  end
  object Panel3: TPanel
    Left = 311
    Top = 8
    Width = 223
    Height = 211
    BevelOuter = bvLowered
    TabOrder = 1
    object DBMemo1: TDBMemo
      Left = 1
      Top = 1
      Width = 221
      Height = 209
      Align = alClient
      BorderStyle = bsNone
      Color = clSilver
      Ctl3D = False
      DataField = 'Notes'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 256
    Width = 542
    Height = 359
    Align = alBottom
    BevelInner = bvRaised
    BorderStyle = bsSingle
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object DBGrid1: TDBGrid
      Left = 2
      Top = 2
      Width = 534
      Height = 326
      Hint = 'Scroll up/down to see other fish!'
      Align = alClient
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clBlack
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object dbnvgr1: TDBNavigator
      Left = 2
      Top = 328
      Width = 534
      Height = 25
      DataSource = DataSource1
      Align = alBottom
      TabOrder = 1
      OnClick = dbnvgr1Click
    end
  end
  object btnUpload: TButton
    Left = 135
    Top = 230
    Width = 75
    Height = 19
    Caption = 'Upload'
    TabOrder = 3
    OnClick = btnUploadClick
  end
  object pnl1: TPanel
    Left = 309
    Top = 224
    Width = 225
    Height = 22
    TabOrder = 4
    object lbl1: TLabel
      Left = 7
      Top = 4
      Width = 56
      Height = 13
      Caption = 'About the'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object dbtxtCommon_Name: TDBText
      Left = 91
      Top = 4
      Width = 115
      Height = 13
      AutoSize = True
      DataField = 'Common_Name'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object DataSource1: TDataSource
    Left = 19
    Top = 193
  end
  object dlgOpenPic1: TOpenPictureDialog
    Filter = 
      'All (*.png;*.jpg;*.jpeg;*.gif;*.cur;*.pcx;*.ani;*.jpg;*.jpeg;*.b' +
      'mp;*.ico;*.emf;*.wmf)|*.png;*.jpg;*.jpeg;*.gif;*.cur;*.pcx;*.ani' +
      ';*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf|JPEG Image File (*.jpg)|*.' +
      'jpg|JPEG Image File (*.jpeg)|*.jpeg|CompuServe GIF Image (*.gif)' +
      '|*.gif|Cursor files (*.cur)|*.cur|PCX Image (*.pcx)|*.pcx|ANI Im' +
      'age (*.ani)|*.ani|JPEG Image File (*.jpg)|*.jpg|JPEG Image File ' +
      '(*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanc' +
      'ed Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|PNG Image Fil' +
      'e (*.png)|*.png'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Fish Image'
    Left = 238
    Top = 40
  end
end
