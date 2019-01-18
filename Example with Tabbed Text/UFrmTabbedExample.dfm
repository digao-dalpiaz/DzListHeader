object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Tabbed Text ListHeader Example (no code)'
  ClientHeight = 201
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListHeader1: TListHeader
    Left = 8
    Top = 8
    Width = 433
    Height = 185
    ListBox = ListBox1
    Columns = <
      item
        Name = 'ID'
        Aligmnent = taRightJustify
        Caption = 'ID'
        Width = 50
      end
      item
        Name = 'Name'
        Caption = 'Name'
        Width = 150
      end
      item
        Name = 'Phone'
        Aligmnent = taCenter
        Caption = 'Phone'
        Width = 120
      end>
    AutoDrawTabbedText = True
    object ListBox1: TListBox
      Left = 0
      Top = 20
      Width = 433
      Height = 148
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 20
      Items.Strings = (
        '1'#9'JHON'#9'1111-2222'
        '2'#9'SARAH'#9'3333-4444'
        '3'#9'ALFRED'#9'5555-6666')
      TabOrder = 2
    end
  end
end
