object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Bitcoin blockchain explorer'
  ClientHeight = 647
  ClientWidth = 935
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 196
    Top = 31
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 400
    Top = 25
    Width = 535
    Height = 603
    Align = alRight
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 628
    Width = 935
    Height = 19
    Panels = <
      item
        Text = 'Initializing...'
        Width = 50
      end>
  end
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 935
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager1
    Caption = 'ActionMainMenuBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 24
    Top = 64
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = Action1
            Caption = 'E&xit'
          end
          item
            Action = Action1
            Caption = '&Exit'
          end>
      end
      item
        Items = <
          item
            Items = <
              item
                Action = Action1
                Caption = '&Exit'
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = Action2
                Caption = '&Block details'
              end>
            Caption = '&View'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    Left = 64
    Top = 208
    StyleName = 'Platform Default'
    object Action1: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Category = 'View'
      Caption = 'Block details'
      OnExecute = Action2Execute
    end
  end
end
