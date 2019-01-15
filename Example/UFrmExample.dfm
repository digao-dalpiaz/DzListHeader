object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 332
  ClientWidth = 539
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
  object LH: TListHeader
    Left = 24
    Top = 24
    Width = 489
    Height = 249
    ListBox = L
    Columns = <
      item
        Name = 'ID'
        Caption = 'ID'
        Width = 100
      end
      item
        Name = 'Name'
        Caption = 'Name'
        Width = 100
      end
      item
        Name = 'Gender'
        Caption = 'Gender'
        Width = 100
      end>
    object L: TListBox
      Left = 0
      Top = 20
      Width = 489
      Height = 212
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 20
      TabOrder = 2
      OnDrawItem = LDrawItem
    end
  end
end
