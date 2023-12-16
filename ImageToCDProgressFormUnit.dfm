object ImageToCDProgressForm: TImageToCDProgressForm
  Left = 192
  Top = 114
  Width = 450
  Height = 201
  Caption = 'ImageToCDProgressForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object LBL_write_sect: TLabel
    Left = 112
    Top = 40
    Width = 321
    Height = 17
    AutoSize = False
    Color = clInactiveBorder
    ParentColor = False
  end
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 97
    Height = 13
    Caption = 'Writing sector (LBA):'
  end
  object Label2: TLabel
    Left = 8
    Top = 88
    Width = 97
    Height = 13
    Caption = 'Writing speed set at:'
  end
  object LBL_curr_write_speed: TLabel
    Left = 112
    Top = 88
    Width = 273
    Height = 17
    AutoSize = False
    Color = clInactiveBorder
    ParentColor = False
  end
  object LBL_drive_buffer_status: TLabel
    Left = 112
    Top = 64
    Width = 321
    Height = 17
    AutoSize = False
    Color = clInactiveBorder
    ParentColor = False
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 89
    Height = 13
    Caption = 'Drive buffer status:'
  end
  object Label4: TLabel
    Left = 392
    Top = 88
    Width = 41
    Height = 13
    Caption = 'kbytes/s'
  end
  object LBL_read_sect: TLabel
    Left = 112
    Top = 16
    Width = 321
    Height = 17
    AutoSize = False
    Color = clInactiveBorder
    ParentColor = False
  end
  object Label6: TLabel
    Left = 8
    Top = 16
    Width = 104
    Height = 13
    Caption = 'Reading sector (LBA):'
  end
  object BTN_stop_write: TButton
    Left = 352
    Top = 120
    Width = 81
    Height = 33
    Caption = 'Stop'
    TabOrder = 0
    OnClick = BTN_stop_writeClick
  end
end
