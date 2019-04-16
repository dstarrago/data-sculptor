object NumericForm: TNumericForm
  Left = 377
  Top = 106
  BorderStyle = bsDialog
  Caption = 'Numeric Attribute'
  ClientHeight = 563
  ClientWidth = 469
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
    Top = 372
    Width = 41
    Height = 22
    Caption = '-->'
    OnClick = AddBtnClick
  end
  object RemoveBtn: TSpeedButton
    Left = 216
    Top = 398
    Width = 41
    Height = 22
    Caption = '<--'
    OnClick = RemoveBtnClick
  end
  object Label1: TLabel
    Left = 201
    Top = 289
    Width = 65
    Height = 26
    Alignment = taCenter
    Caption = 'Dependence Weight'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 24
    Top = 470
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
    Top = 317
    Width = 50
    Height = 13
    Caption = ' (0 - 100%)'
  end
  object BitBtnOk: TBitBtn
    Left = 280
    Top = 518
    Width = 75
    Height = 25
    TabOrder = 0
    OnClick = BitBtnOkClick
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 368
    Top = 518
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object GroupBox2: TGroupBox
    Left = 17
    Top = 264
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
    Top = 264
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
    Top = 337
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
    Top = 467
    Width = 57
    Height = 22
    Increment = 10
    MaxValue = 100
    MinValue = 0
    TabOrder = 5
    Value = 0
  end
  object NormalDistButton: TRadioButton
    Left = 32
    Top = 54
    Width = 113
    Height = 17
    Caption = 'Normal distribution'
    Checked = True
    TabOrder = 6
    TabStop = True
  end
  object Panel1: TPanel
    Left = 64
    Top = 73
    Width = 350
    Height = 72
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 7
    object Label3: TLabel
      Left = 20
      Top = 13
      Width = 27
      Height = 13
      Caption = 'Mean'
    end
    object Label4: TLabel
      Left = 20
      Top = 46
      Width = 66
      Height = 13
      Caption = 'Standard Dev'
    end
    object Mean: TEdit
      Left = 100
      Top = 10
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object StdDev: TEdit
      Left = 100
      Top = 42
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '1'
    end
  end
  object UniformDistButton: TRadioButton
    Left = 30
    Top = 155
    Width = 113
    Height = 17
    Caption = 'Uniform distribution'
    TabOrder = 8
  end
  object Panel2: TPanel
    Left = 64
    Top = 175
    Width = 350
    Height = 73
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 9
    object Label6: TLabel
      Left = 20
      Top = 13
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object Label7: TLabel
      Left = 20
      Top = 44
      Width = 13
      Height = 13
      Caption = 'To'
    end
    object MinVal: TEdit
      Left = 100
      Top = 10
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object MaxVal: TEdit
      Left = 100
      Top = 41
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '10'
    end
  end
  object attrName: TEdit
    Left = 70
    Top = 18
    Width = 345
    Height = 21
    TabOrder = 10
  end
end
