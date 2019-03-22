object FormAnalyzer: TFormAnalyzer
  Left = 191
  Top = 111
  Width = 544
  Height = 375
  Caption = 'Analizador'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnDeactivate = FormDeactivate
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 536
    Height = 316
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 316
    Width = 536
    Height = 25
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    object ProgressBar1: TProgressBar
      Left = 0
      Top = 0
      Width = 447
      Height = 25
      Align = alClient
      Min = 0
      Max = 100
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 447
      Top = 0
      Width = 89
      Height = 25
      Align = alRight
      BevelOuter = bvNone
      Caption = 'Detener'
      TabOrder = 1
      object StopBtn: TButton
        Left = 2
        Top = 2
        Width = 86
        Height = 22
        Caption = 'Detener'
        Enabled = False
        TabOrder = 0
        OnClick = StopBtnClick
      end
    end
  end
end
