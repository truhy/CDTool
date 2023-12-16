object Form7: TForm7
  Left = 183
  Top = 114
  Width = 565
  Height = 424
  Caption = 'Read disc info'
  Color = clBtnFace
  Constraints.MinHeight = 424
  Constraints.MinWidth = 565
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    557
    390)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 541
    Height = 137
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Input parameters'
    TabOrder = 0
    DesignSize = (
      541
      137)
    object Device_Lbl: TLabel
      Left = 8
      Top = 36
      Width = 82
      Height = 13
      Caption = 'Device selection:'
    end
    object RescanBtn: TButton
      Left = 443
      Top = 16
      Width = 89
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Rescan'
      TabOrder = 0
      OnClick = RescanBtnClick
    end
    object CD_dev_CB: TComboBox
      Left = 8
      Top = 52
      Width = 525
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = CD_dev_CBChange
    end
    object BTN_read_disc_info: TButton
      Left = 443
      Top = 88
      Width = 89
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Read info'
      TabOrder = 2
      OnClick = BTN_read_disc_infoClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 152
    Width = 541
    Height = 113
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Disc information'
    TabOrder = 1
    DesignSize = (
      541
      113)
    object SGRID_disc_info: TStringGrid
      Left = 8
      Top = 24
      Width = 524
      Height = 74
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 3
      DefaultColWidth = 300
      DefaultRowHeight = 19
      FixedCols = 0
      RowCount = 16
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goTabs, goThumbTracking]
      TabOrder = 0
      ColWidths = (
        165
        71
        321)
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 272
    Width = 541
    Height = 111
    Align = alCustom
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Disc information - OPC table entries'
    TabOrder = 2
    DesignSize = (
      541
      111)
    object SGRID_OPC: TStringGrid
      Left = 8
      Top = 24
      Width = 524
      Height = 72
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 2
      DefaultColWidth = 300
      DefaultRowHeight = 19
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goTabs, goThumbTracking]
      TabOrder = 0
      ColWidths = (
        165
        393)
    end
  end
end
