object Form5: TForm5
  Left = 170
  Top = 120
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Write image to CDR/W'
  ClientHeight = 390
  ClientWidth = 557
  Color = clBtnFace
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
  object Device_Lbl: TLabel
    Left = 8
    Top = 22
    Width = 82
    Height = 13
    Caption = 'Device selection:'
  end
  object DataWriteSpeed_Lbl: TLabel
    Left = 8
    Top = 340
    Width = 60
    Height = 13
    Caption = 'Write speed:'
  end
  object ImageFileName_Lbl: TLabel
    Left = 8
    Top = 150
    Width = 82
    Height = 13
    Caption = 'Image File Name:'
  end
  object Label3: TLabel
    Left = 8
    Top = 89
    Width = 70
    Height = 13
    Caption = 'Lead-in format:'
  end
  object Label1: TLabel
    Left = 8
    Top = 286
    Width = 111
    Height = 13
    Caption = 'Skip sector position file:'
  end
  object Label2: TLabel
    Left = 128
    Top = 340
    Width = 51
    Height = 13
    Caption = 'Write type:'
  end
  object CD_dev_CB: TComboBox
    Left = 8
    Top = 40
    Width = 457
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = CD_dev_CBChange
  end
  object RescanBtn: TButton
    Left = 476
    Top = 40
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Rescan'
    TabOrder = 1
    OnClick = RescanBtnClick
  end
  object DataWriteSpeed_CB: TComboBox
    Left = 8
    Top = 356
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = DataWriteSpeed_CBChange
  end
  object ImageNameEdit: TEdit
    Left = 8
    Top = 168
    Width = 457
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object ImageSaveAsBtn: TButton
    Left = 476
    Top = 168
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 4
    OnClick = ImageSaveAsBtnClick
  end
  object BTN_write: TButton
    Left = 476
    Top = 352
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Write'
    TabOrder = 5
    OnClick = BTN_writeClick
  end
  object ED_SSP_file_name: TEdit
    Left = 8
    Top = 304
    Width = 457
    Height = 21
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 6
  end
  object BTN_browse_SSP: TButton
    Left = 476
    Top = 304
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 7
    OnClick = BTN_browse_SSPClick
  end
  object CB_lead_in_type: TComboBox
    Left = 8
    Top = 106
    Width = 209
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 8
    Text = 'Assume audio CDDA (no scrambling)'
    Items.Strings = (
      'Assume audio CDDA (no scrambling)'
      'Assume data CDROM (scrambling)'
      'Same as track 1 entry in TOC')
  end
  object ED_start_LBA: TEdit
    Left = 224
    Top = 106
    Width = 137
    Height = 21
    Enabled = False
    TabOrder = 9
    Text = '0'
    OnExit = ED_start_LBAExit
    OnKeyPress = ED_start_LBAKeyPress
  end
  object BTN_read_ATIP: TButton
    Left = 296
    Top = 136
    Width = 65
    Height = 25
    Caption = 'Read ATIP'
    Enabled = False
    TabOrder = 10
    OnClick = BTN_read_ATIPClick
  end
  object RG_ssp_method: TRadioGroup
    Left = 8
    Top = 200
    Width = 457
    Height = 57
    Anchors = [akTop, akRight]
    Caption = 'Sector skip method:'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'No skipping'
      '1. Use write position parameter'
      '2. Use sync cache')
    TabOrder = 11
  end
  object CHK_show_all_CDROMs: TCheckBox
    Left = 320
    Top = 8
    Width = 145
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Show all CDROM devices'
    Color = clInactiveBorder
    ParentColor = False
    TabOrder = 12
    OnClick = CHK_show_all_CDROMsClick
  end
  object CHK_specify_start_write: TCheckBox
    Left = 224
    Top = 80
    Width = 137
    Height = 17
    Caption = 'Specify start pos (LBA):'
    Color = clInactiveBorder
    ParentColor = False
    TabOrder = 13
    OnClick = CHK_specify_start_writeClick
  end
  object BTN_set_PG1_at_0: TButton
    Left = 224
    Top = 136
    Width = 65
    Height = 25
    Caption = 'PG1 at 0'
    TabOrder = 14
    OnClick = BTN_set_PG1_at_0Click
  end
  object CHK_test_write: TCheckBox
    Left = 312
    Top = 356
    Width = 73
    Height = 17
    Caption = 'Test write'
    Color = clInactiveBorder
    ParentColor = False
    TabOrder = 15
  end
  object CB_write_type: TComboBox
    Left = 128
    Top = 356
    Width = 177
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 16
    Text = 'Raw DAO, raw sub mode'
    Items.Strings = (
      'Raw DAO, raw sub mode'
      'Raw DAO, packed sub mode')
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'All known types|*.DAO;*.LI;*.TU;*.TUI;*.TR|2448 byte binary file' +
      ' (*.DAO)|*.DAO|2448 byte lead-in TOC file (*.LI)|*.LI|96 byte le' +
      'ad-in TOC unique deint file (*.TU)|*.TU|96 byte lead-in TOC uniq' +
      'ue int file (*.TU)|*.TUI|96 byte lead-in TOC repeated deint file' +
      ' (*.TR)|*.TR|All files (*.*)|*.*'
    Title = 'Open Image File'
    Left = 424
    Top = 104
  end
end
