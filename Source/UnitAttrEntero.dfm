object FormEntero: TFormEntero
  Left = 244
  Top = 153
  ActiveControl = EditEnteroValMax
  BorderStyle = bsDialog
  Caption = 'Atributo Entero'
  ClientHeight = 279
  ClientWidth = 371
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
  object Label3: TLabel
    Left = 10
    Top = 221
    Width = 55
    Height = 13
    Caption = 'Distribuci'#243'n'
  end
  object AddBtn: TSpeedButton
    Left = 152
    Top = 112
    Width = 23
    Height = 22
    Caption = '-->'
    OnClick = AddBtnClick
  end
  object RemoveBtn: TSpeedButton
    Left = 152
    Top = 144
    Width = 23
    Height = 22
    Caption = '<--'
    OnClick = RemoveBtnClick
  end
  object Label1: TLabel
    Left = 8
    Top = 188
    Width = 66
    Height = 13
    Caption = 'Peso (0 - 100)'
  end
  object SetBtn: TSpeedButton
    Left = 152
    Top = 184
    Width = 23
    Height = 22
    Caption = '!'
    OnClick = SetBtnClick
  end
  object Label2: TLabel
    Left = 224
    Top = 188
    Width = 70
    Height = 13
    Caption = 'Ruido (0 - 100)'
  end
  object PanelDesde: TGroupBox
    Left = 8
    Top = 7
    Width = 169
    Height = 74
    Caption = ' Desde '
    TabOrder = 0
    object RadioMenosInf: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = '- oo'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioMinVal: TRadioButton
      Left = 16
      Top = 40
      Width = 113
      Height = 17
      Caption = 'A'
      TabOrder = 1
    end
    object EditEnteroValMin: TSpinEdit
      Left = 34
      Top = 36
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnClick = EditEnteroValMinClick
    end
  end
  object PanelHasta: TGroupBox
    Left = 192
    Top = 7
    Width = 169
    Height = 74
    Caption = ' Hasta '
    TabOrder = 1
    object RadioMasInf: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = '+ oo'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioMaxVal: TRadioButton
      Left = 16
      Top = 40
      Width = 113
      Height = 17
      Caption = 'B'
      TabOrder = 1
    end
    object EditEnteroValMax: TSpinEdit
      Left = 34
      Top = 36
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnClick = EditEnteroValMaxClick
    end
  end
  object BitBtnOk: TBitBtn
    Left = 200
    Top = 248
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = BitBtnOkClick
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 288
    Top = 248
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object ComboDistribution: TComboBox
    Left = 72
    Top = 218
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'Uniforme'
    Items.Strings = (
      'Uniforme'
      'Normal')
  end
  object GroupBox1: TGroupBox
    Left = 192
    Top = 88
    Width = 169
    Height = 89
    Caption = ' Dependencias '
    TabOrder = 5
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
  object GroupBox2: TGroupBox
    Left = 8
    Top = 88
    Width = 137
    Height = 89
    Caption = ' Variables '
    TabOrder = 6
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
  object WeightEdit: TSpinEdit
    Left = 88
    Top = 185
    Width = 57
    Height = 22
    Increment = 10
    MaxValue = 100
    MinValue = 0
    TabOrder = 7
    Value = 100
  end
  object RuidoEdit: TSpinEdit
    Left = 304
    Top = 185
    Width = 57
    Height = 22
    Increment = 10
    MaxValue = 100
    MinValue = 0
    TabOrder = 8
    Value = 100
  end
end
