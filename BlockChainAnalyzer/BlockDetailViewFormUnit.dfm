object BlockDetailViewForm: TBlockDetailViewForm
  Left = 0
  Top = 0
  Caption = 'BlockDetailViewForm'
  ClientHeight = 579
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblNext: TLabel
    Left = 608
    Top = 387
    Width = 33
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdRightToLeftNoAlign
    Caption = 'lblNext'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentBiDiMode = False
    ParentFont = False
    OnClick = lblNextClick
  end
  object lblPrev: TLabel
    Left = 8
    Top = 357
    Width = 529
    Height = 13
    BiDiMode = bdRightToLeftNoAlign
    Caption = 'lblNext'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentBiDiMode = False
    ParentFont = False
    OnClick = lblNextClick
  end
  object Label1: TLabel
    Left = 320
    Top = 332
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Memo1: TMemo
    Left = 0
    Top = 406
    Width = 673
    Height = 173
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 256
    ExplicitWidth = 398
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 673
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitWidth = 558
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 673
    Height = 289
    ActivePage = TabSheet2
    Align = alTop
    TabOrder = 2
    ExplicitLeft = 152
    ExplicitTop = 62
    ExplicitWidth = 385
    object TabSheet1: TTabSheet
      Caption = 'Block info'
      ExplicitLeft = -20
      ExplicitTop = 0
      ExplicitWidth = 377
    end
    object TabSheet2: TTabSheet
      Caption = 'Transacctions'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Memo2: TMemo
        Left = 35
        Top = 11
        Width = 558
        Height = 198
        TabOrder = 0
      end
    end
  end
end
