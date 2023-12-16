object Form6: TForm6
  Left = 204
  Top = 122
  Width = 565
  Height = 424
  Caption = 'Read track info'
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
    TabOrder = 1
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
    object Label1: TLabel
      Left = 8
      Top = 84
      Width = 56
      Height = 13
      Caption = 'Input mode:'
    end
    object LBL_enter_trackno_LBA: TLabel
      Left = 184
      Top = 84
      Width = 93
      Height = 13
      Caption = 'Enter track number:'
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
    object CB_track_mode: TComboBox
      Left = 8
      Top = 100
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'Track number'
      OnChange = CB_track_modeChange
      Items.Strings = (
        'Track number'
        'LBA (sector) of track')
    end
    object ED_trackno_LBA: TEdit
      Left = 184
      Top = 100
      Width = 244
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = '1'
      OnExit = ED_trackno_LBAExit
      OnKeyPress = ED_trackno_LBAKeyPress
    end
    object BTN_read_track_info: TButton
      Left = 443
      Top = 100
      Width = 89
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Read info'
      TabOrder = 4
      OnClick = BTN_read_track_infoClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 152
    Width = 541
    Height = 230
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Track information'
    TabOrder = 0
    DesignSize = (
      541
      230)
    object SGRID_track_info: TStringGrid
      Left = 8
      Top = 24
      Width = 524
      Height = 197
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 3
      DefaultColWidth = 300
      DefaultRowHeight = 19
      FixedCols = 0
      RowCount = 25
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goTabs, goThumbTracking]
      TabOrder = 0
      ColWidths = (
        165
        54
        321)
    end
  end
end
