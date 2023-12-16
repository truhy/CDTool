unit CommonCDSettingsUnit;

interface

Uses
    Classes,
    SysUtils,
    Dialogs,
    Windows,
    MMC1Unit,
    SCSIUnit,
    WNASPI32Unit;

Type
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
    T_P_read_speed_rec=^T_read_speed_rec;
    T_read_speed_rec=Record
                           KbSpeed : Word;
                     End;

type
    Ptr_byte=^Byte;

    T_P_subch_deint_PW_96=^T_subch_deint_PW_96;
    T_subch_group_12=Array[1..12] Of Byte;
    T_subch_deint_PW_96=Record
                              PW : Array[1..8] Of T_subch_group_12;
                        End;

    T_P_sub_int_PW_96=^T_sub_int_PW_96;
    T_sub_int_PW_96=Record
                          PW : Array[1..96] Of Byte;
                    End;

    T_write_param=Record
                        starting_MMCLBA_sector_to_write : LongInt;
                        no_of_sectors_to_write          : Word;
                        is_do_sync_cache                : Boolean;
                        is_block_for_reading            : Boolean;
                  End;
    T_P_write_param=^T_write_param;

Type
    T_CommonCDSettings=Class
                       Private
                       Public
                             A_read_speed       : T_P_read_speed_rec;
                             CDROM_list         : TList;
                             Read_speed_list    : TList;
                             SectFormattedData  : String;
                             SubChFormattedData : String;
                             MC_valid           : Boolean;
                             MCN_str            : String;
                             TC_valid           : Boolean;
                             ISRC_str           : String;
                             TOC_data           : T_out_read_T_P_A_TOC_desc_MSF;
                             Sess_TOC_data      : T_out_read_T_P_A_sess_desc_MSF;
                             Full_TOC_data      : T_out_read_T_P_A_full_TOC_desc;
                             First_trk_sess_no  : Byte;
                             Last_trk_sess_no   : Byte;

                             P_write_buffer : Pointer;
                             P_write_param_buffer : T_P_write_param;
                             sector_buffer_block_len : LongWord; //Max'm no of sectors that can be buffered.
                             current_start_loc_of_reading : LongInt; //Location of buffer for reading (sector unit).
                             current_start_loc_of_writing : LongInt; //Location of buffer for writing (sector unit).
                             max_no_of_blocks_to_read_synch : Word; //Must be >= 1 and should be <= no_of_blocks_to_wait_b4_reading.
                             max_no_of_blocks_to_write_synch : Word; //Must be >= 1
                             no_of_blocks_to_wait_b4_reading : LongWord; //Must be >= 1 and should be < sector_buffer_block_len.
                             no_of_blocks_read : LongWord;
                             reading_done : Boolean;

                             Constructor Create;
                             Destructor Destroy; Override;
                             Procedure Set_default_timeout_buf;
                             Function SCSI_select_init_CDROM(List_indexNo : Integer) : Boolean;
                             Procedure SCSI_select_deinit_CDROM;
                             Procedure Del_CDROM_list_items;
                             Procedure Del_read_speed_list_items;
                             Procedure Del_mem_in_list(list : TList);
                             Procedure Add_read_speed(In_KbSpeed : Word);
                             Function Set_read_err_rec_verify_param(In_read_err_rec_param : Byte;
                                                                    In_read_retry_count   : Byte) : Boolean;
                             Function Set_read_err_rec_verify_count(In_read_err_rec_param : Byte;
                                                                    In_read_retry_count   : Byte) : Boolean;
                             procedure Deint_subs(P_sub_int_PW_96     : Pointer;
                                                  P_subch_deint_PW_96 : T_P_subch_deint_PW_96);
                             procedure Int_subs(P_subch_deint_PW_96 : Pointer;
                                                P_sub_int_PW_96     : T_P_sub_int_PW_96);
                             Function Set_write_params(test_write_YN : Boolean; in_data_block_type : Byte) : Boolean;
                             Procedure Convert_full_TOC_Hex_to_Dec;
                             Function Create_new_SCSI_obj(Var SCSI : T_SCSI) : Boolean;
                       End;

implementation

uses MainFormUnit;

Constructor T_CommonCDSettings.Create;
Begin
     Inherited Create;

     CDROM_list:=TList.Create;
     Read_speed_list:=TList.Create;
End;

Destructor T_CommonCDSettings.Destroy;
Begin
     Del_CDROM_list_items;
     CDROM_list.Free;

     Del_Read_speed_list_items;
     Read_speed_list.Free;

     Inherited Destroy;
End;

Procedure T_CommonCDSettings.Set_default_timeout_buf;
Begin
     Case Form1.SCSI.interface_method Of
     U_METHOD_WNASPI:
     Begin
          If Form1.SCSI.SPC_any_link.WNASPI32.Get_data_buf_size=0 Then
          Begin
               Form1.SCSI.SPC_any_link.WNASPI32.AllocNBufferVirtualAlloc(65536);
               If Form1.SCSI.SPC_any_link.WNASPI32.Get_data_buf_size=0 Then
               Begin
                    MessageDlg('Buffer allocation error.' + Chr(10) + Chr(13) + 'Insufficient memory resources.', mtError, [mbOk], 0);
               End
          End;
          //Setting the timeout doesn't seem to work.
          //Form1.SCSI.SCSI_interface.SetGlobalTimeOutSecs(10);
     End;
     U_METHOD_SCSI_PASS_THRU:
     Begin
          //To do for SPT method.
     End;
     U_METHOD_SCSI_PASS_THRU_D:
     Begin
          //To do for SPTD method.
     End;
     End;
End;

Function T_CommonCDSettings.SCSI_select_init_CDROM(List_indexNo : Integer) : Boolean;
Begin
     Case Form1.SCSI.interface_method Of
     U_METHOD_WNASPI:
     Begin
          Form1.SCSI.SPC_any_link.WNASPI32.Set_Cur_HA_ID(T_CDROM_rec(CDROM_list[List_indexNo]^).HA_ID);
          Form1.SCSI.SPC_any_link.WNASPI32.Set_Cur_Dev_ID(T_CDROM_rec(CDROM_list[List_indexNo]^).Dev_ID);
          Form1.SCSI.SPC_any_link.WNASPI32.Set_Cur_LUN(T_CDROM_rec(CDROM_list[List_indexNo]^).LUN);
          Form1.SCSI.SPC_any_link.WNASPI32.Set_Cur_Res_supp(T_CDROM_rec(CDROM_list[List_indexNo]^).Res_supp);
     End;
     U_METHOD_SCSI_PASS_THRU,
     U_METHOD_SCSI_PASS_THRU_D:
     Begin
          Form1.SCSI.SPC_any_link.SPT.Open_set_active_drive_letter(T_CDROM_SPT_rec(CDROM_list[List_indexNo]^).Drive_letter);
     End
     End;

     Result:=True;
End;

Procedure T_CommonCDSettings.SCSI_select_deinit_CDROM;
Begin
     Form1.SCSI.SPC_any_link.SPT.CloseVolume;
End;

Procedure T_CommonCDSettings.Del_CDROM_list_items;
Var i : Integer;
    A_CDROM : Pointer;
Begin
     For i:=0 To (CDROM_list.Count - 1) Do
     Begin
          A_CDROM := CDROM_list.Items[i];
          Dispose(A_CDROM);
     End;
     CDROM_list.Clear;
End;

Procedure T_CommonCDSettings.Del_read_speed_list_items;
Var i : Integer;
    A_read_speed : Pointer;
Begin
     For i:=0 To (Read_speed_list.Count - 1) Do
     Begin
          A_read_speed := Read_speed_list.Items[i];
          Dispose(A_read_speed);
     End;
     Read_speed_list.Clear;
End;

Procedure T_CommonCDSettings.Del_mem_in_list(list : TList);
Var i : Integer;
Begin
     For i:=0 To (list.Count - 1) Do
     Begin
          Dispose(list.Items[i]);
     End;
     list.Clear;
End;

Procedure T_CommonCDSettings.Add_read_speed(In_KbSpeed : Word);
Begin
     New(A_read_speed);
     A_read_speed^.KbSpeed:=In_KbSpeed;
     Read_speed_list.Add(A_read_speed);
End;

Function T_CommonCDSettings.Set_read_err_rec_verify_param(In_read_err_rec_param : Byte;
                                                          In_read_retry_count   : Byte) : Boolean;
Var Curr_read_err_rec_param : Byte;
    is_set_read_err_rec_ok : Boolean;
Begin
     //Form1.SCSI.MMC1.Do_sense10_read_err_rec;
     //If Form1.SCSI.WNASPI32.SRB_status_OK Then
     //Begin
          Form1.SCSI.MMC1_any_link.Do_select10_read_err_rec(In_read_err_rec_param, In_read_retry_count);
          If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               Form1.SCSI.MMC1_any_link.Do_sense10_read_err_rec;
               If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
               Begin
                    Curr_read_err_rec_param:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Err_rec_param;

                    If In_read_err_rec_param<>Curr_read_err_rec_param Then
                    Begin
                         is_set_read_err_rec_ok:=False;
                    End
                    Else
                    Begin
                         is_set_read_err_rec_ok:=True;
                    End;
               End
               Else
               Begin
                    is_set_read_err_rec_ok:=False;
               End;
          End
          Else
          Begin
               is_set_read_err_rec_ok:=False;
          End;
     //End
     //Else
     //Begin
     //     is_set_read_err_rec_ok:=False;
     //End;

     Set_read_err_rec_verify_param:=is_set_read_err_rec_ok;
End;

Function T_CommonCDSettings.Set_read_err_rec_verify_count(In_read_err_rec_param : Byte;
                                                          In_read_retry_count   : Byte) : Boolean;
Var Curr_read_retry_count : Byte;
    is_set_read_err_rec_ok : Boolean;
Begin
     //Form1.SCSI.MMC1.Do_sense10_read_err_rec;
     //If Form1.SCSI.WNASPI32.SRB_status_OK Then
     //Begin
          Form1.SCSI.MMC1_any_link.Do_select10_read_err_rec(In_read_err_rec_param, In_read_retry_count);
          If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               Form1.SCSI.MMC1_any_link.Do_sense10_read_err_rec;
               If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
               Begin
                    Curr_read_retry_count:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Read_retry_count;

                    If In_read_retry_count<>Curr_read_retry_count Then
                    Begin
                         is_set_read_err_rec_ok:=False;
                    End
                    Else
                    Begin
                         is_set_read_err_rec_ok:=True;
                    End;
               End
               Else
               Begin
                    is_set_read_err_rec_ok:=False;
               End;
          End
          Else
          Begin
               is_set_read_err_rec_ok:=False;
          End;
     //End
     //Else
     //Begin
     //     is_set_read_err_rec_ok:=False;
     //End;

     Set_read_err_rec_verify_count:=is_set_read_err_rec_ok;
End;

procedure T_CommonCDSettings.Deint_subs(P_sub_int_PW_96     : Pointer;
                                        P_subch_deint_PW_96 : T_P_subch_deint_PW_96);
{ This will take an 96 bytes of interlaced raw sub-channel data
  (sub-channels of 1 sector), pointed to by P_sub_int_PW_96, and deinterlaces them,
  storing the result into memory pointed to by P_subch_deint_PW_96.

  De-interlace sub-channel data means to:

  group the P sub-ch data together to form a byte,
  group the Q sub-ch data together to form a byte,
  group the R sub-ch data together to form a byte,
  group the S sub-ch data together to form a byte,
  group the T sub-ch data together to form a byte,
  group the U sub-ch data together to form a byte,
  group the V sub-ch data together to form a byte,
  group the W sub-ch data together to form a byte. }
Var
   P_sub_byte : Ptr_byte;
   i, j, k    : Byte;
   mask_val   : Byte;
   shift_val  : Shortint;
Begin
     P_sub_byte:=P_sub_int_PW_96;

     ZeroMemory(P_subch_deint_PW_96, 96);

     For i:=1 To 12 Do
     Begin
          For k:=1 To 8 Do
          Begin
              mask_val:=$80;
              shift_val:=1-k;
              For j:=1 To 8 Do
              Begin
                   If shift_val>0 Then
                   Begin
                        P_subch_deint_PW_96^.PW[j][i]:=(P_subch_deint_PW_96^.PW[j][i]) OR
                                                            ((P_sub_byte^ AND mask_val) SHL shift_val);
                   End
                   Else
                   Begin
                        P_subch_deint_PW_96^.PW[j][i]:=(P_subch_deint_PW_96^.PW[j][i]) OR
                                                            ((P_sub_byte^ AND mask_val) SHR ABS(shift_val));
                   End;
                   mask_val:=mask_val SHR 1;
                   shift_val:=shift_val+1;
              End;
              Inc(P_sub_byte);
          End;
     End;
End;

procedure T_CommonCDSettings.Int_subs(P_subch_deint_PW_96 : Pointer;
                                      P_sub_int_PW_96     : T_P_sub_int_PW_96);
Var i, j, k : Byte;
    P_sub_byte : Ptr_byte;
    sub_byte   : Byte;
    mask_val   : Byte;
    shift_val  : Byte;
Begin
     ZeroMemory(P_sub_int_PW_96, 96);

     mask_val:=$80;
     shift_val:=0;
     P_sub_byte:=P_subch_deint_PW_96;
     For i:=0 To 7 Do
     Begin
          For j:=0 To 11 Do
          Begin
               sub_byte:=P_sub_byte^;
               For k:=0 To 7 Do
               Begin
                    P_sub_int_PW_96^.PW[j*8+1+k]:=P_sub_int_PW_96^.PW[j*8+1+k] OR
                                                       ((sub_byte AND mask_val) SHR shift_val);
                    sub_byte:=sub_byte SHL 1;
               End;
               Inc(P_sub_byte);
          End;
          shift_val:=shift_val + 1;
     End;
End;

Function T_CommonCDSettings.Set_write_params(test_write_YN : Boolean; in_data_block_type : Byte) : Boolean;
Var
   Write_params : T_pub_write_params;
   i : Byte;
Begin
     If test_write_YN Then
     Begin
          Write_params.TestWR_WRType:=MMC_MODE_PARAM_TESTWR_ON Or
                                      MMC_MODE_PARAM_WRTYPE_RAW;
     End
     Else
     Begin
          Write_params.TestWR_WRType:=MMC_MODE_PARAM_WRTYPE_RAW;
     End;
     Write_params.MultiS_FP_Copy_TrkMode:=4;
     Write_params.DataBlockType:=in_data_block_type;
     Write_params.HostAppCode:=0;
     Write_params.SessFormat:=0;
     Write_params.PacketSize:=0;
     Write_params.AudioPauseLen:=150;
     //Write_params.AudioPauseLen:=0;
     For i:=0 To 16 Do
     Begin
          Write_params.MCN[i]:=Chr(0);
          Write_params.ISRC[i]:=Chr(0);
     End;
     Form1.SCSI.MMC1_any_link.Do_select10_write(Write_params);
     If Not Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          MessageDlg('MMC RAW DAO+RAW PW write parameters could not be set.', mtInformation, [mbOk], 0);
     End;
End;

Procedure T_CommonCDSettings.Convert_full_TOC_Hex_to_Dec;
Begin
     //Treat any values between 10 to 99 as hex representation, converting hex to decimal (base 10)
     If (Full_TOC_data.TNO>=10) And
        (Full_TOC_data.TNO<=99) Then
     Full_TOC_data.TNO:=StrToInt('$' + IntToStr(Full_TOC_data.TNO));

     If (Full_TOC_data.POINT>=10) And
        (Full_TOC_data.POINT<=99) Then
     Full_TOC_data.POINT:=StrToInt('$' + IntToStr(Full_TOC_data.POINT));

     If (Full_TOC_data.Min>=10) And
        (Full_TOC_data.Min<=99) Then
     Full_TOC_data.Min:=StrToInt('$' + IntToStr(Full_TOC_data.Min));

     If (Full_TOC_data.Sec>=10) And
        (Full_TOC_data.Sec<=99) Then
     Full_TOC_data.Sec:=StrToInt('$' + IntToStr(Full_TOC_data.Sec));

     If (Full_TOC_data.Frame>=10) And
        (Full_TOC_data.Frame<=99) Then
     Full_TOC_data.Frame:=StrToInt('$' + IntToStr(Full_TOC_data.Frame));

     If (Full_TOC_data.Zero>=10) And
        (Full_TOC_data.Zero<=99) Then
     Full_TOC_data.Zero:=StrToInt('$' + IntToStr(Full_TOC_data.Zero));

     If (Full_TOC_data.PMin>=10) And
        (Full_TOC_data.PMin<=99) Then
     Full_TOC_data.PMin:=StrToInt('$' + IntToStr(Full_TOC_data.PMin));

     If (Full_TOC_data.PSec>=10) And
        (Full_TOC_data.PSec<=99) Then
     Full_TOC_data.PSec:=StrToInt('$' + IntToStr(Full_TOC_data.PSec));

     If (Full_TOC_data.PFrame>=10) And
        (Full_TOC_data.PFrame<=99) Then
     Full_TOC_data.PFrame:=StrToInt('$' + IntToStr(Full_TOC_data.PFrame));
End;

Function T_CommonCDSettings.Create_new_SCSI_obj(Var SCSI : T_SCSI) : Boolean;
Begin
     Case Form1.SCSI.interface_method Of
     U_METHOD_WNASPI:
     Begin
          //Create SCSI class object
          SCSI:=T_SCSI.Create(U_METHOD_WNASPI);
          //Try to open and link to WNASPI32.DLL
          If SCSI.SPC_any_link.WNASPI32.Get_OpenASPIStatus<>USER_WNASPI32_OPENASPI_DLL_LOAD_ERR Then
          Begin
               If SCSI.SPC_any_link.WNASPI32.Get_OpenASPIStatus<>USER_WNASPI32_OPENASPI_FX_IMPORT_ERR Then
               Begin
                    If SCSI.SPC_any_link.WNASPI32.Get_SRB_status_OK=True Then
                    Begin
                         SCSI.SPC_any_link.WNASPI32.AllocNBufferVirtualAlloc(65536);
                         If SCSI.SPC_any_link.WNASPI32.Get_data_buf_size>0 Then
                         Begin
                              //Set same current drive to be used for the new SCSI object.
                              SCSI.SPC_any_link.WNASPI32.Set_Cur_HA_ID(Form1.SCSI.SPC_any_link.WNASPI32.Get_Cur_HA_ID);
                              SCSI.SPC_any_link.WNASPI32.Set_Cur_Dev_ID(Form1.SCSI.SPC_any_link.WNASPI32.Get_Cur_Dev_ID);
                              SCSI.SPC_any_link.WNASPI32.Set_Cur_LUN(Form1.SCSI.SPC_any_link.WNASPI32.Get_Cur_LUN);
                              SCSI.SPC_any_link.WNASPI32.Set_Cur_Res_supp(Form1.SCSI.SPC_any_link.WNASPI32.Get_Cur_Res_supp);

                              Result:=True;
                         End
                         Else
                         Begin
                              Result:=False;
                         End;
                    End
                    Else
                    Begin
                         Result:=False;
                    End;
               End
               Else
               Begin
                    Result:=False;
               End;
          End
          Else
          Begin
               Result:=False;
          End;
     End;
     U_METHOD_SCSI_PASS_THRU:
     Begin
          SCSI:=T_SCSI.Create(U_METHOD_SCSI_PASS_THRU);

          SCSI.SPC_any_link.SPT.Open_set_active_drive_letter(Form1.SCSI.SPC_any_link.SPT.get_active_drive_letter);

          Result:=True;
     End;
     U_METHOD_SCSI_PASS_THRU_D:
     Begin
          SCSI:=T_SCSI.Create(U_METHOD_SCSI_PASS_THRU_D);

          Result:=True;
     End;
     End;
End;

end.
