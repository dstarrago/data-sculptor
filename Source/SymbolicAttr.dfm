object SymbolicForm: TSymbolicForm
  Left = 355
  Top = 176
  BorderStyle = bsDialog
  Caption = 'Symbolic Attribute'
  ClientHeight = 385
  ClientWidth = 472
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
  object AddBtn: TSpeedButton
    Left = 216
    Top = 220
    Width = 41
    Height = 22
    Caption = '-->'
    OnClick = AddBtnClick
  end
  object RemoveBtn: TSpeedButton
    Left = 216
    Top = 246
    Width = 41
    Height = 22
    Caption = '<--'
    OnClick = RemoveBtnClick
  end
  object Label1: TLabel
    Left = 201
    Top = 137
    Width = 65
    Height = 26
    Alignment = taCenter
    Caption = 'Dependence Weight'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 24
    Top = 318
    Width = 97
    Height = 13
    Caption = 'Add noise (0 - 100%)'
  end
  object Label5: TLabel
    Left = 30
    Top = 22
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label8: TLabel
    Left = 208
    Top = 165
    Width = 50
    Height = 13
    Caption = ' (0 - 100%)'
  end
  object Label3: TLabel
    Left = 32
    Top = 68
    Width = 91
    Height = 13
    Caption = 'Number of Symbols'
  end
  object BitBtnOk: TBitBtn
    Left = 280
    Top = 342
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 368
    Top = 342
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object GroupBox2: TGroupBox
    Left = 17
    Top = 112
    Width = 170
    Height = 185
    Caption = 'Available variables'
    TabOrder = 2
    object VarList: TListBox
      Left = 2
      Top = 15
      Width = 166
      Height = 168
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnMouseDown = VarListMouseDown
    end
  end
  object GroupBox1: TGroupBox
    Left = 286
    Top = 112
    Width = 170
    Height = 185
    Caption = ' Dependent variables'
    TabOrder = 3
    object DepenList: TListBox
      Left = 2
      Top = 15
      Width = 166
      Height = 168
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnMouseDown = DepenListMouseDown
    end
  end
  object WeightEdit: TSpinEdit
    Left = 200
    Top = 185
    Width = 73
    Height = 22
    Increment = 10
    MaxValue = 100
    MinValue = 0
    TabOrder = 4
    Value = 10
  end
  object RuidoEdit: TSpinEdit
    Left = 129
    Top = 315
    Width = 57
    Height = 22
    Increment = 10
    MaxValue = 100
    MinValue = 0
    TabOrder = 5
    Value = 0
  end
  object attrName: TEdit
    Left = 70
    Top = 18
    Width = 345
    Height = 21
    TabOrder = 6
  end
  object EditSimbolico: TSpinEdit
    Left = 138
    Top = 65
    Width = 63
    Height = 22
    MaxValue = 2
    MinValue = 2
    TabOrder = 7
    Value = 2
  end
end
