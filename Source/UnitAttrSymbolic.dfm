object FormSimbolico: TFormSimbolico
  Left = 230
  Top = 142
  BorderStyle = bsDialog
  Caption = 'Atributo Simb'#243'lico'
  ClientHeight = 207
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 97
    Height = 13
    Caption = 'N'#250'mero de s'#237'mbolos'
  end
  object AddBtn: TSpeedButton
    Left = 152
    Top = 64
    Width = 23
    Height = 22
    Caption = '-->'
    OnClick = AddBtnClick
  end
  object RemoveBtn: TSpeedButton
    Left = 152
    Top = 96
    Width = 23
    Height = 22
    Caption = '<--'
    OnClick = RemoveBtnClick
  end
  object Label2: TLabel
    Left = 8
    Top = 140
    Width = 66
    Height = 13
    Caption = 'Peso (0 - 100)'
  end
  object SetBtn: TSpeedButton
    Left = 152
    Top = 136
    Width = 23
    Height = 22
    Caption = '!'
    OnClick = SetBtnClick
  end
  object Label3: TLabel
    Left = 224
    Top = 140
    Width = 70
    Height = 13
    Caption = 'Ruido (0 - 100)'
  end
  object EditSimbolico: TSpinEdit
    Left = 114
    Top = 9
    Width = 63
    Height = 22
    MaxValue = 2
    MinValue = 2
    TabOrder = 0
    Value = 2
    OnChange = EditSimbolicoChange
  end
  object BitBtnOk: TBitBtn
    Left = 200
    Top = 176
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 288
    Top = 176
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 40
    Width = 137
    Height = 89
    Caption = ' Variables '
    TabOrder = 3
    object VarList: TListBox
      Left = 2
      Top = 15
      Width = 133
      Height = 72
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnMouseDown = VarListMouseDown
    end
  end
  object GroupBox1: TGroupBox
    Left = 192
    Top = 40
    Width = 169
    Height = 89
    Caption = ' Dependencias '
    TabOrder = 4
    object DepenList: TListBox
      Left = 2
      Top = 15
      Width = 165
      Height = 72
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnMouseDown = DepenListMouseDown
    end
  end
  object WeightEdit: TSpinEdit
    Left = 88
    Top = 137
    Width = 57
    Height = 22
    Increment = 10
    MaxValue = 100
    MinValue = 0
    TabOrder = 5
    Value = 100
  end
  object RuidoEdit: TSpinEdit
    Left = 304
    Top = 137
    Width = 57
    Height = 22
    Increment = 10
    MaxValue = 100
    MinValue = 0
    TabOrder = 6
    Value = 100
  end
end
