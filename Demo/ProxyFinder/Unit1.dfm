object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Proxy Finder: TmsProxyFind Example'
  ClientHeight = 327
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 425
    Height = 273
    Align = alLeft
    Caption = 'Sites with proxy'
    TabOrder = 0
    ExplicitHeight = 296
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 415
      Height = 250
      Align = alClient
      Lines.Strings = (
        'http://fineproxy.org/'
        
          'http://www.prime-speed.ru/proxy/free-proxy-list/all-working-prox' +
          'ies.php'
        'http://proxy-list.org/russian/index.php?p=1'
        'http://proxy-list.org/russian/index.php?p=2'
        'http://proxy-list.org/russian/index.php?p=3'
        'http://proxy-list.org/russian/index.php?p=4'
        'http://proxy-list.org/russian/index.php?p=5'
        'http://proxy-list.org/russian/index.php?p=6'
        'http://proxy-list.org/russian/index.php?p=7'
        'http://proxy-list.org/russian/index.php?p=8'
        'http://proxy-list.org/russian/index.php?p=9'
        'http://proxy-list.org/russian/index.php?p=10')
      TabOrder = 0
      ExplicitLeft = 227
      ExplicitTop = 3
      ExplicitWidth = 250
      ExplicitHeight = 321
    end
  end
  object GroupBox2: TGroupBox
    Left = 425
    Top = 0
    Width = 210
    Height = 273
    Align = alClient
    Caption = 'Point proxy'
    TabOrder = 1
    ExplicitLeft = 65
    ExplicitTop = 80
    ExplicitWidth = 185
    ExplicitHeight = 105
    object Memo2: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 200
      Height = 250
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 96
      ExplicitTop = 72
      ExplicitWidth = 185
      ExplicitHeight = 89
    end
  end
  object Button1: TButton
    AlignWithMargins = True
    Left = 3
    Top = 299
    Width = 629
    Height = 25
    Align = alBottom
    Caption = 'Get a proxy'
    TabOrder = 2
    OnClick = Button1Click
    ExplicitLeft = 48
    ExplicitTop = 96
    ExplicitWidth = 75
  end
  object ProgressBar1: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 276
    Width = 629
    Height = 17
    Align = alBottom
    TabOrder = 3
    ExplicitLeft = 80
    ExplicitTop = 296
    ExplicitWidth = 150
  end
end
