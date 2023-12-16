object _MSR_form: T_MSR_form
  Left = 143
  Top = 145
  Width = 551
  Height = 252
  BorderWidth = 5
  Caption = 'Measure Sector Reads/Seeks'
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
    533
    208)
  PixelsPerInch = 96
  TextHeight = 13
  object Device_Lbl: TLabel
    Left = 0
    Top = 22
    Width = 82
    Height = 13
    Caption = 'Device selection:'
  end
  object FirstSector_Lbl: TLabel
    Left = 0
    Top = 78
    Width = 54
    Height = 13
    Caption = 'First sector:'
  end
  object LastSector_Lbl: TLabel
    Left = 72
    Top = 78
    Width = 55
    Height = 13
    Caption = 'Last sector:'
  end
  object DataReadSpeed_Lbl: TLabel
    Left = 160
    Top = 78
    Width = 61
    Height = 13
    Caption = 'Read speed:'
  end
  object Subch_Lbl: TLabel
    Left = 0
    Top = 162
    Width = 127
    Height = 13
    Caption = 'Subchannel reading mode:'
  end
  object Label1: TLabel
    Left = 288
    Top = 78
    Width = 105
    Height = 13
    Caption = 'Command to measure:'
  end
  object RescanBtn: TButton
    Left = 443
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Rescan'
    TabOrder = 0
    OnClick = RescanBtnClick
  end
  object CD_dev_CB: TComboBox
    Left = 0
    Top = 40
    Width = 532
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnChange = CD_dev_CBChange
  end
  object BTN_measure: TButton
    Left = 443
    Top = 174
    Width = 89
    Height = 25
    Caption = 'Measure'
    TabOrder = 2
    OnClick = BTN_measureClick
  end
  object FirstSector_Edit: TEdit
    Left = 0
    Top = 96
    Width = 65
    Height = 21
    MaxLength = 11
    TabOrder = 3
    Text = '0'
  end
  object LastSector_Edit: TEdit
    Left = 72
    Top = 96
    Width = 65
    Height = 21
    MaxLength = 6
    TabOrder = 4
  end
  object LastSectorFromTOC_Btn: TButton
    Left = 72
    Top = 124
    Width = 65
    Height = 25
    Caption = 'From TOC'
    TabOrder = 5
    OnClick = LastSectorFromTOC_BtnClick
  end
  object DataReadSpeed_CB: TComboBox
    Left = 160
    Top = 96
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    OnChange = DataReadSpeed_CBChange
  end
  object ImageSaveAsBtn: TButton
    Left = 443
    Top = 112
    Width = 89
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = ImageSaveAsBtnClick
  end
  object Subch_CB: TComboBox
    Left = 0
    Top = 178
    Width = 433
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 8
    Text = 'No subs'
    OnChange = Subch_CBChange
    Items.Strings = (
      'No subs'
      'P-W (full subs) not error corrected (96 bytes/sector)'
      
        'P-W (full subs) error corrected, de-interleaved (96 bytes/sector' +
        ')'
      'P & Q only (16 bytes/sector)')
  end
  object CB_MMC_command_to_measure: TComboBox
    Left = 288
    Top = 96
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 9
    Text = 'ReadCD CDB12'
    OnChange = CB_MMC_command_to_measureChange
    Items.Strings = (
      'ReadCD CDB12'
      'Seek CDB10')
  end
  object CHK_save_rep_speed: TCheckBox
    Left = 160
    Top = 128
    Width = 121
    Height = 17
    Caption = 'Save reported speed'
    Color = clInactiveBorder
    ParentColor = False
    TabOrder = 10
  end
  object BTN_gen_SSP: TButton
    Left = 443
    Top = 143
    Width = 89
    Height = 25
    Caption = 'Generate .SSP'
    TabOrder = 11
    Visible = False
    OnClick = BTN_gen_SSPClick
  end
  object ImageSaveDialog: TSaveDialog
    DefaultExt = '*.MSR'
    Filter = '.MSR (Measured Sector Reads)|*.MSR|All files (*.*)|*.*'
    Title = 'Save CD Image As'
    Left = 504
    Top = 72
  end
end
