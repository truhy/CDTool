object CDToImageProgressForm: TCDToImageProgressForm
  Left = 200
  Top = 142
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CDToImageProgressForm'
  ClientHeight = 113
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Sect_reading_disp_Lbl: TLabel
    Left = 112
    Top = 31
    Width = 321
    Height = 18
    AutoSize = False
    Color = clInactiveBorder
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Reading_sector_Lbl: TLabel
    Left = 8
    Top = 29
    Width = 78
    Height = 16
    Caption = 'Sector (LBA):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LBL_curr_read_speed: TLabel
    Left = 112
    Top = 55
    Width = 265
    Height = 18
    AutoSize = False
    Color = clInactiveBorder
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 55
    Width = 97
    Height = 16
    Caption = 'Reading speed:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 384
    Top = 55
    Width = 51
    Height = 16
    Caption = 'kbytes/s'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Stop_Image_Btn: TButton
    Left = 384
    Top = 81
    Width = 50
    Height = 25
    Caption = 'Stop'
    TabOrder = 0
    OnClick = Stop_Image_BtnClick
  end
end
