unit ATIP_form_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GetCDROMListThreadUnit, MMC1Unit, Tools_Unit, Grids;

type
  TATIP_form = class(TForm)
    BTN_read_ATIP: TButton;
    RescanBtn: TButton;
    Device_Lbl: TLabel;
    CD_dev_CB: TComboBox;
    SGRID_ATIP: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RescanBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BTN_read_ATIPClick(Sender: TObject);
    procedure CD_dev_CBChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc;
    GetCDROMListThread : T_GetCDROMListThread;
    Curr_CDROM_indexNo : Integer;
    is_busy : Boolean;
    disabled_ctrl_list : TList;
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure ShowErrMsg(s : String);
    Procedure FillCDComboSelect;
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
  public
    { Public declarations }
  end;

var
  ATIP_form: TATIP_form;

implementation

uses MainFormUnit;

{$R *.dfm}

Procedure TATIP_form.Disable_controls;
Var i : Integer;
    a_ctrl_enabled_state : ^Boolean;
Begin
     disabled_ctrl_list:=TList.Create;

     For i:=0 To ControlCount-1 Do
     Begin
          New(a_ctrl_enabled_state);
          a_ctrl_enabled_state^:=Controls[i].Enabled;
          disabled_ctrl_list.Add(a_ctrl_enabled_state);

          Controls[i].Enabled:=False;
     End;
End;

Procedure TATIP_form.Enable_controls;
Var i : Integer;
    a_ctrl_enabled_state : ^Boolean;
Begin
     For i:=0 To ControlCount-1 Do
     Begin
          a_ctrl_enabled_state:=disabled_ctrl_list.Items[i];
          If a_ctrl_enabled_state^=True Then
          Begin
               Controls[i].Enabled:=True;
          End;
          Dispose(a_ctrl_enabled_state);
     End;

     disabled_ctrl_list.Clear;
     disabled_ctrl_list.Free;
End;

Procedure TATIP_form.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

Procedure TATIP_form.FillCDComboSelect;
Begin
     Form1.CDFormUtils.FillCB_with_CDROM(CD_dev_CB);
     If CD_dev_CB.Items.Count>0 Then
     Begin
          If Form1.CommonCDSettings.SCSI_select_init_CDROM(0)=True Then //Attempt to select and init first CDROM.
          Begin
               CD_dev_CB.ItemIndex:=0; //Selects first item in combobox list.
               Curr_CDROM_indexNo:=0;
          End;
     End
     Else
         MessageDlg('No CD/DVD devices.', mtError, [mbOk], 0);
End;

Procedure TATIP_form.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;

     GetCDROMListThread:=nil;
     is_busy:=False;
End;

procedure TATIP_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     If GetCDROMListThread<>nil Then
     Begin
          try
             GetCDROMListThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     Form1.SetATIPFormNil; //Prevent main form from showing this form.
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

procedure TATIP_form.RescanBtnClick(Sender: TObject);
begin
     If Not is_busy Then
     Begin
          is_busy:=True;
          Disable_controls;

          GetCDROMListThread:=T_GetCDROMListThread.Create(SCSI_init_Get_CDROM_list_done,
                                                          ShowErrMsg,
                                                          False);
     End;
end;

procedure TATIP_form.FormCreate(Sender: TObject);
begin
     Curr_CDROM_indexNo:=0;
     GetCDROMListThread:=nil;

     SGRID_ATIP.Cells[0, 0]:='Description';
     SGRID_ATIP.Cells[1, 0]:='Value';

     RescanBtn.Click;
end;

procedure TATIP_form.BTN_read_ATIPClick(Sender: TObject);
var
   MMCLBA_sector : LongInt;
   s : String;
   Disc_type : Byte;
   Disc_sub_type : Byte;
   i : Integer;
   There_is_ATIP_data : Boolean;
   A1_BIT : Byte;
   Lowest_CLV_rec_speed : Byte;
   Highest_CLV_rec_speed : Byte;
begin
     For i:=1 To SGRID_ATIP.RowCount-1 Do
         SGRID_ATIP.Rows[i].Clear;
         
     There_is_ATIP_data:=Form1.SCSI.MMC1_any_link.Do_read_T_P_A_ATIP_out_CDB10(Out_ATIP_desc);
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          If There_is_ATIP_data Then
          Begin
               SGRID_ATIP.Cells[0, 1]:='Indicative initial laser writing power:';
               SGRID_ATIP.Cells[1, 1]:=IntToStr((Out_ATIP_desc.WritePower_RecSpeed AND $70) SHR 4);

               SGRID_ATIP.Cells[0, 2]:='Recommended writing speed for CDRW media:';
               SGRID_ATIP.Cells[1, 2]:=IntToStr(Out_ATIP_desc.WritePower_RecSpeed AND $07);

               SGRID_ATIP.Cells[0, 3]:='Unrestricted Use Disc:';
               SGRID_ATIP.Cells[1, 3]:=IntToStr((Out_ATIP_desc.URU SHR 6) AND 1);

               Disc_type:=(Out_ATIP_desc.DType_DSubType_A1A2A3 SHR 6) AND 1;
               SGRID_ATIP.Cells[0, 4]:='Disc type (0=CDR, 1=CDRW media):';
               SGRID_ATIP.Cells[1, 4]:=IntToStr(Disc_type);
               If Disc_type=0 Then
                   SGRID_ATIP.Cells[1, 4]:=SGRID_ATIP.Cells[1, 4] + ' (CDR)'
               Else
                   SGRID_ATIP.Cells[1, 4]:=SGRID_ATIP.Cells[1, 4] + ' (CDRW)';

               Disc_sub_type:=(Out_ATIP_desc.DType_DSubType_A1A2A3 SHR 3) AND 7;
               SGRID_ATIP.Cells[0, 5]:='Disc sub type:';
               SGRID_ATIP.Cells[1, 5]:=IntToStr(Disc_sub_type);
               If Disc_type=1 Then
               Begin
                    If Disc_sub_type=0 Then
                        SGRID_ATIP.Cells[1, 5]:=SGRID_ATIP.Cells[1, 5] + ' (Standard speed CDRW)'
                    Else
                        SGRID_ATIP.Cells[1, 5]:=SGRID_ATIP.Cells[1, 5] + ' (High speed CDRW)';
               End;

               MMCLBA_sector:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_MMCLBA(Out_ATIP_desc.StartMin,
                                                                              Out_ATIP_desc.StartSec,
                                                                              Out_ATIP_desc.StartFrame);

               SGRID_ATIP.Cells[0, 6]:='ATIP start of lead-in time (MM:SS:FF):';
               SGRID_ATIP.Cells[1, 6]:=IntToStrFixZeroes(Out_ATIP_desc.StartMin, 2)
                                       + ':'
                                       + IntToStrFixZeroes(Out_ATIP_desc.StartSec, 2)
                                       + ':'
                                       + IntToStrFixZeroes(Out_ATIP_desc.StartFrame, 2)
                                       + ' (MMC LBA: '
                                       + IntToStr(MMCLBA_sector)
                                       + ')';

               MMCLBA_sector:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_MMCLBA(Out_ATIP_desc.LastMin,
                                                                              Out_ATIP_desc.LastSec,
                                                                              Out_ATIP_desc.LastFrame);

               SGRID_ATIP.Cells[0, 7]:='ATIP start of lead-out time (MM:SS:FF):';
               SGRID_ATIP.Cells[1, 7]:=IntToStrFixZeroes(Out_ATIP_desc.LastMin, 2)
                                       + ':'
                                       + IntToStrFixZeroes(Out_ATIP_desc.LastSec, 2)
                                       + ':'
                                       + IntToStrFixZeroes(Out_ATIP_desc.LastFrame, 2)
                                       + ' (MMC LBA: '
                                       + IntToStr(MMCLBA_sector)
                                       + ')';

               A1_BIT:=(Out_ATIP_desc.DType_DSubType_A1A2A3 SHR 2) AND 1;
               SGRID_ATIP.Cells[0, 8]:='A1 BIT (1 indicates that below are valid):';
               SGRID_ATIP.Cells[1, 8]:=IntToStr(A1_BIT);

               Lowest_CLV_rec_speed:=(Out_ATIP_desc.CLV_rec_speeds SHR 4) AND $07;
               SGRID_ATIP.Cells[0, 9]:='   Lowest CLV recording speed code:';
               SGRID_ATIP.Cells[1, 9]:=IntToStr(Lowest_CLV_rec_speed);

               Highest_CLV_rec_speed:=Out_ATIP_desc.CLV_rec_speeds AND $0F;
               SGRID_ATIP.Cells[0, 10]:='   Highest CLV recording speed code:';
               SGRID_ATIP.Cells[1, 10]:=IntToStr(Highest_CLV_rec_speed);

               If A1_BIT=1 Then
               Begin
                    Case Lowest_CLV_rec_speed Of
                    1:
                    Begin
                         SGRID_ATIP.Cells[1, 9]:=SGRID_ATIP.Cells[1, 9] + ' (2X)'
                    End;
                    End;

                    Case Highest_CLV_rec_speed Of
                    1:
                    Begin
                         SGRID_ATIP.Cells[1, 10]:=SGRID_ATIP.Cells[1, 10] + ' (2X)'
                    End;
                    2:
                    Begin
                         SGRID_ATIP.Cells[1, 10]:=SGRID_ATIP.Cells[1, 10] + ' (4X)'
                    End;
                    3:
                    Begin
                         SGRID_ATIP.Cells[1, 10]:=SGRID_ATIP.Cells[1, 10] + ' (6X)'
                    End;
                    4:
                    Begin
                         SGRID_ATIP.Cells[1, 10]:=SGRID_ATIP.Cells[1, 10] + ' (8X)'
                    End;
                    End;
               End;

               SGRID_ATIP.Cells[0, 11]:='   Power multiplication factor p:';
               SGRID_ATIP.Cells[1, 11]:=IntToStr((Out_ATIP_desc.Power_fx_values SHR 4) AND $07);

               SGRID_ATIP.Cells[0, 12]:='   Target y value of the Modulation/Power function:';
               SGRID_ATIP.Cells[1, 12]:=IntToStr(Out_ATIP_desc.Power_fx_values AND $07);

               SGRID_ATIP.Cells[0, 13]:='   Recommended Erase/Write Power Ratio:';
               SGRID_ATIP.Cells[1, 13]:=IntToStr((Out_ATIP_desc.Erase_write_power_ratio SHR 4) AND $07);

               SGRID_ATIP.Cells[0, 14]:='A2 BIT (1 indicates that below are valid):';
               SGRID_ATIP.Cells[1, 14]:=IntToStr((Out_ATIP_desc.DType_DSubType_A1A2A3 SHR 1) AND 1);

               SGRID_ATIP.Cells[0, 15]:='   A2 value byte 1:';
               SGRID_ATIP.Cells[1, 15]:=IntToStr(Out_ATIP_desc.A2_byte1);

               SGRID_ATIP.Cells[0, 16]:='   A2 value byte 2:';
               SGRID_ATIP.Cells[1, 16]:=IntToStr(Out_ATIP_desc.A2_byte2);

               SGRID_ATIP.Cells[0, 17]:='   A2 value byte 3:';
               SGRID_ATIP.Cells[1, 17]:=IntToStr(Out_ATIP_desc.A2_byte3);

               SGRID_ATIP.Cells[0, 18]:='A3 BIT (1 indicates that below are valid):';
               SGRID_ATIP.Cells[1, 18]:=IntToStr(Out_ATIP_desc.DType_DSubType_A1A2A3 AND 1);

               SGRID_ATIP.Cells[0, 19]:='   A3 value byte 1:';
               SGRID_ATIP.Cells[1, 19]:=IntToStr(Out_ATIP_desc.A3_byte1);

               SGRID_ATIP.Cells[0, 20]:='   A3 value byte 2:';
               SGRID_ATIP.Cells[1, 20]:=IntToStr(Out_ATIP_desc.A3_byte2);

               SGRID_ATIP.Cells[0, 21]:='   A3 value byte 3:';
               SGRID_ATIP.Cells[1, 21]:=IntToStr(Out_ATIP_desc.A3_byte3);
          End
          Else
          Begin
               s:='Read TOC/ATIP command was sent, but there is no ATIP info returned.';
               MessageDlg(s, mtError, [mbOk], 0);
          End;
     End
     Else
     Begin
          s:='Error while processing read TOC/ATIP command.' + Chr(10) + Chr(13);
          s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
          MessageDlg(s, mtError, [mbOk], 0);
     End;
end;

procedure TATIP_form.CD_dev_CBChange(Sender: TObject);
begin
     If Form1.CommonCDSettings.SCSI_select_init_CDROM(CD_dev_CB.ItemIndex)=False Then //Attempt to select and init first CDROM.
     Begin
          CD_dev_CB.ItemIndex:=Curr_CDROM_indexNo; //Selects prev selected item in combobox list.

          MessageDlg('Cannot initialise device.', mtError, [mbOk], 0);
     End
     Else
     Begin
          Curr_CDROM_indexNo:=CD_dev_CB.ItemIndex;
     End;
end;

procedure TATIP_form.FormDestroy(Sender: TObject);
begin
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

end.
