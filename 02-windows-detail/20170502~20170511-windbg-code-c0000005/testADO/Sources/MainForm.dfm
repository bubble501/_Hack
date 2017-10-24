object Form1: TForm1
  Left = 192
  Top = 130
  BorderStyle = bsDialog
  Caption = 'testADO'
  ClientHeight = 234
  ClientWidth = 576
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblname: TLabel
    Left = 22
    Top = 35
    Width = 56
    Height = 13
    AutoSize = False
    Caption = 'UserName '
  end
  object lblPasswd: TLabel
    Left = 24
    Top = 76
    Width = 49
    Height = 13
    Caption = 'PassWord'
  end
  object lblDatabase: TLabel
    Left = 26
    Top = 113
    Width = 46
    Height = 13
    Caption = 'Database'
  end
  object edtName: TEdit
    Left = 85
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'trade'
  end
  object edtPasswd: TEdit
    Left = 85
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'trade'
  end
  object edtDatabase: TEdit
    Left = 85
    Top = 109
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'BAOPAN-PUB'
  end
  object chkconntype: TCheckBox
    Left = 32
    Top = 152
    Width = 135
    Height = 17
    Caption = 'Query'#20351#29992'Connection'
    TabOrder = 3
  end
  object btnStartTest: TButton
    Left = 360
    Top = 16
    Width = 137
    Height = 25
    Caption = #27979#35797#21333#32447#31243#36830#25509
    TabOrder = 4
    OnClick = btnStartTestClick
  end
  object btnThreads: TButton
    Left = 360
    Top = 56
    Width = 137
    Height = 25
    Caption = #27979#35797#22810#32447#31243#36830#25509
    TabOrder = 5
    OnClick = btnThreadsClick
  end
end
