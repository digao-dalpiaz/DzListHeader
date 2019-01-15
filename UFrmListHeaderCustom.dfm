object FrmListHeaderCustom: TFrmListHeaderCustom
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Columns Customization'
  ClientHeight = 432
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BtnAll: TSpeedButton
    Left = 8
    Top = 8
    Width = 57
    Height = 25
    Caption = 'All'
    OnClick = BtnAllClick
  end
  object BtnNone: TSpeedButton
    Left = 72
    Top = 8
    Width = 57
    Height = 25
    Caption = 'None'
    OnClick = BtnNoneClick
  end
  object BtnSwap: TSpeedButton
    Left = 136
    Top = 8
    Width = 57
    Height = 25
    Caption = 'Swap'
    OnClick = BtnSwapClick
  end
  object BtnDefOrder: TSpeedButton
    Left = 232
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Default Order'
    OnClick = BtnDefOrderClick
  end
  object L: TCheckListBox
    Left = 8
    Top = 40
    Width = 313
    Height = 353
    DragMode = dmAutomatic
    ItemHeight = 18
    Style = lbOwnerDrawFixed
    TabOrder = 0
    OnDragDrop = LDragDrop
    OnDragOver = LDragOver
    OnDrawItem = LDrawItem
  end
  object BoxBtns: TPanel
    Left = 88
    Top = 400
    Width = 153
    Height = 33
    BevelOuter = bvNone
    TabOrder = 1
    object BtnOK: TButton
      Left = 8
      Top = 0
      Width = 65
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = BtnOKClick
    end
    object BtnCancel: TButton
      Left = 80
      Top = 0
      Width = 65
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
