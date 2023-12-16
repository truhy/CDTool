object ATIP_form: TATIP_form
  Left = 184
  Top = 93
  Width = 565
  Height = 424
  BorderWidth = 5
  Caption = 'ATIP information'
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
    547
    380)
  PixelsPerInch = 96
  TextHeight = 13
  object Device_Lbl: TLabel
    Left = 0
    Top = 22
    Width = 82
    Height = 13
    Caption = 'Device selection:'
  end
  object BTN_read_ATIP: TButton
    Left = 458
    Top = 80
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Read ATIP'
    TabOrder = 0
    OnClick = BTN_read_ATIPClick
  end
  object RescanBtn: TButton
    Left = 458
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Rescan'
    TabOrder = 1
    OnClick = RescanBtnClick
  end
  object CD_dev_CB: TComboBox
    Left = 0
    Top = 40
    Width = 547
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnChange = CD_dev_CBChange
  end
  object SGRID_ATIP: TStringGrid
    Left = 0
    Top = 112
    Width = 547
    Height = 267
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultColWidth = 300
    DefaultRowHeight = 19
    FixedCols = 0
    RowCount = 22
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goTabs, goThumbTracking]
    TabOrder = 3
    ColWidths = (
      281
      225)
  end
end
