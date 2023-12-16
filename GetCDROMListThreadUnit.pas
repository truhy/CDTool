unit GetCDROMListThreadUnit;

interface

uses
    SCSIUnit,
    Classes,
    SysUtils,
    Tools_Unit,
    SPC_Unit,
    MMC1Unit;

Type
    T_p_byte=^byte;
    T_P_CDROM_rec=^T_CDROM_rec;
    T_CDROM_rec=Record
                      Name        : String;
                      HA_ID       : Byte;
                      Dev_ID      : Byte;
                      LUN         : Byte;
                      Res_supp    : Boolean;
                      Max_buf_len : LongWord;
                End;
    T_P_CDROM_SPT_rec=^T_CDROM_SPT_rec;
    T_CDROM_SPT_rec=Record
                          Drive_letter  : Char;
                          Vendor        : String;
                          Product       : String;
                          Revision      : String;
                    End;
    T_Method=procedure(s : String) of object;

type
  T_GetCDROMListThread = class(TThread)
  private
    { Private declarations }
    sender_method : T_Method;
    A_CDROM       : T_P_CDROM_rec;
    A_CDROM_SPT   : T_P_CDROM_SPT_rec;
    err_msg       : String;
    is_writers_only : Boolean;
    Procedure ShowErrMsg;
  protected
    procedure Execute; override;
  public
    Function Find_CDROM(is_find_first_device : Boolean) : Boolean;
    Function Find_CDRW(is_find_first_device : Boolean) : Boolean;
    Procedure Add_found_CDROM_to_list;
    Procedure SCSI_init_Get_CDROM_list;
    Constructor Create(thread_done_proc   : TNotifyEvent;
                       in_method          : T_Method;
                       in_is_writers_only : Boolean);
  end;

implementation

Uses
    MainFormUnit;

Constructor T_GetCDROMListThread.Create(thread_done_proc   : TNotifyEvent;
                                        in_method          : T_Method;
                                        in_is_writers_only : Boolean);
Begin
     is_writers_only:=in_is_writers_only;
     sender_method:=in_method;
     OnTerminate:=thread_done_proc;
     FreeOnTerminate := True;
     Inherited Create(False);
End;

Procedure T_GetCDROMListThread.ShowErrMsg;
Begin
     sender_method(err_msg);
End;

Function T_GetCDROMListThread.Find_CDROM(is_find_first_device : Boolean) : Boolean;
Var
    Found_CDROM : Boolean;
Begin
     If is_find_first_device Then
     Begin
          Found_CDROM:=Form1.SCSI.SPC_any_link.Find_first_CDROM;
     End
     Else
     Begin
          Found_CDROM:=Form1.SCSI.SPC_any_link.Find_next_CDROM;
     End;

     Case Form1.SCSI.interface_method Of
     U_METHOD_WNASPI:
     Begin
          If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               If Found_CDROM Then
               Begin
                    Form1.SCSI.SPC_any_link.Set_found_CDROM_active;
               End;
          End
          Else
          Begin
               err_msg:='Scanning for CDROM error.' + Chr(10) + Chr(13) +
                        Form1.SCSI.SPC_any_link.Get_sendcmd_err_msg;
               Synchronize(ShowErrMsg);
          End;
     End;
     U_METHOD_SCSI_PASS_THRU,
     U_METHOD_SCSI_PASS_THRU_D:
     Begin
          If Found_CDROM Then
          Begin
               Form1.SCSI.SPC_any_link.Set_found_CDROM_active;
          End;
     End;
     End;

     Find_CDROM:=Found_CDROM;
End;

Function T_GetCDROMListThread.Find_CDRW(is_find_first_device : Boolean) : Boolean;
Var
    Found_CDROM : Boolean;
    Found_CDRW  : Boolean;
    Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;
Begin
     Found_CDRW:=False;

     Found_CDROM:=Find_CDROM(is_find_first_device);

     If Found_CDROM Then
     Begin
          Repeat
                Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
                //Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_sub6_out(Out_CD_cap_mech_st);
                If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
                Begin
                     If (Out_CD_cap_mech_st.TestWR_WRCDRW_WRCDR And 1)=1 Then
                     Begin
                          Found_CDRW:=True;
                     End
                     Else
                         Found_CDROM:=Find_CDROM(False);
                End
                Else
                    Found_CDROM:=Find_CDROM(False);
          Until(Found_CDRW Or (Not Found_CDROM));
     End;

     Find_CDRW:=Found_CDRW;
End;

Procedure T_GetCDROMListThread.Add_found_CDROM_to_list;
Var Str_msg : String;
    p : T_p_byte;
Begin
     Form1.SCSI.SPC_any_link.Get_dev_inquiry_CDB6;
     If Not Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          err_msg:='Get device inquiry error.' + Chr(10) + Chr(13) +
                   Form1.SCSI.SPC_any_link.Get_sendcmd_err_msg;
          Synchronize(ShowErrMsg);
     End
     Else
     Begin
          Case Form1.SCSI.interface_method Of
          U_METHOD_WNASPI:
          Begin
               With T_dev_inq_std_data(Form1.SCSI.SPC_any_link.Get_data_buf^) Do
               Begin
                    PStrMLCopy(Str_msg, @VendorID, 8);
                    Str_msg:=TrimRight(Str_msg) + ' / ';
                    PStrMLCopyAppend(Str_msg, @ProdID, 16);
                    Str_msg:=TrimRight(Str_msg) + ' / ';
                    PStrMLCopyAppend(Str_msg, @ProdRev, 4);
                    Str_msg:=TrimRight(Str_msg);
               End;

               New(A_CDROM);
               A_CDROM.Name:=Str_msg;
               A_CDROM.HA_ID:=Form1.SCSI.SPC_any_link.WNASPI32.Get_Found_HA_ID;
               A_CDROM.Dev_ID:=Form1.SCSI.SPC_any_link.WNASPI32.Get_Found_Dev_ID;
               A_CDROM.LUN:=Form1.SCSI.SPC_any_link.WNASPI32.Get_Found_LUN;
               A_CDROM.Res_supp:=Form1.SCSI.SPC_any_link.WNASPI32.Get_Found_Res_supp;
               A_CDROM.Max_buf_len:=Form1.SCSI.SPC_any_link.WNASPI32.Get_Found_Max_buf_len;
               Form1.CommonCDSettings.CDROM_list.Add(A_CDROM);
          End;
          U_METHOD_SCSI_PASS_THRU,
          U_METHOD_SCSI_PASS_THRU_D:
          Begin
               New(A_CDROM_SPT);

               p:=Form1.SCSI.SPC_any_link.SPT.Get_data_buf;

               inc(p, 8);
               PStrMLCopy(Str_msg, p, 8); //Vendor string
               A_CDROM_SPT.Vendor:=Str_msg;

               inc(p, 8);
               PStrMLCopy(Str_msg, p, 16); //Product string
               A_CDROM_SPT.Product:=Str_msg;

               inc(p, 16);
               PStrMLCopy(Str_msg, p, 4); //Rev string
               A_CDROM_SPT.Revision:=Str_msg;

               A_CDROM_SPT.Drive_letter:=Form1.SCSI.SPC_any_link.SPT.drive_letter_found;
               Form1.CommonCDSettings.CDROM_list.Add(A_CDROM_SPT);
          End;
          End;
     End;
End;

Procedure T_GetCDROMListThread.SCSI_init_Get_CDROM_list;
Begin
     { Checks if we allocated WNASPI32 buffer first. }
     If Form1.SCSI.SPC_any_link.Get_data_buf_size>0 Then
     Begin
          Form1.CommonCDSettings.Del_CDROM_list_items;

          If is_writers_only Then
          Begin
               If Find_CDRW(True) Then
               Begin
                    Add_found_CDROM_to_list;
                    While (Find_CDRW(False)=True) Do
                    Begin
                         Add_found_CDROM_to_list;
                    End;
               End;
          End
          Else
          Begin
               If Find_CDROM(True) Then
               Begin
                    Add_found_CDROM_to_list;
                    While (Find_CDROM(False)=True) Do
                    Begin
                         Add_found_CDROM_to_list;
                    End;
               End;
          End;
     End;
End;

procedure T_GetCDROMListThread.Execute;
begin
     SCSI_init_Get_CDROM_list;
end;

end.
