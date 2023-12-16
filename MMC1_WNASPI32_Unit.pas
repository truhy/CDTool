unit MMC1_WNASPI32_Unit;

interface

Uses
    MMC1_any_link_Unit,
    SPC_WNASPI32_Unit,
    WNASPI32Unit,
    SPC_Unit,
    MMC1Unit,
    Windows,
    //Dialogs, //For debugging only. Allows message boxes. Remove this in final release.
    SysUtils;

{$I MMC1_WNASPI32_inc.pas} //A separate include file listing constants and types.

Type
    P_byte=^Byte;

Type T_MMC1_WNASPI32=
Class(T_MMC1_any_link)
Private
      {** Select/sense for MMC mode parameters.. **}
      Function Calc_sense6_return_cap_from_param_len(PageCode : Byte) : Byte;
      Function Calc_sense10_return_cap_from_param_len(PageCode : Byte) : Word;
      Function Calc_sense6_return_cap_from_mode_data_len(PageCode : Byte) : Byte;
      Function Calc_sense10_return_cap_from_mode_data_len(PageCode : Byte) : Word;
      Function Calc_sense6_return_cap_from_mode_data_and_param_len(PageCode : Byte) : Byte;
      Function Calc_sense10_return_cap_from_mode_data_and_param_len(PageCode : Byte) : Word;
      Function Get_modesense6_DBD_last_used : Byte;
      Function Get_modesense10_DBD_last_used : Byte;
      Function Is_sense6_block_desc_returned(PageCode : Byte) : Boolean;
      Function Is_sense10_block_desc_returned(PageCode : Byte) : Boolean;
      Function Is_sense6_block_desc_returned_from_data : Boolean;
      Function Is_sense10_block_desc_returned_from_data : Boolean;
Public
      Constructor Create(Var In_SPC_WNASPI32 : T_SPC_WNASPI32);
      Destructor Destroy; Override;
      Function Get_MMC1 : T_MMC1; Override;
      Function Get_err_msg : PChar; Override;
      { SCSI-3 MMC command methods using Win ASPI 32 interface.. }
      Procedure Do_read_rec_cap_CDB10; Override;
      Procedure Do_read_subch_CDB10(In_MSF     : Byte;
                                    In_SubQ    : Byte;
                                    In_Params  : Byte;
                                    In_TrackNo : Byte); Override;
      Procedure Do_read_T_P_A_CDB10(In_MSF       : Byte;
                                    In_format    : Byte;
                                    In_trkSessNo : Byte); Override;
      Procedure Do_read_header_CDB10(In_MSF        : Byte;
                                     In_MMCLBA_MSF : Pointer); Override;
      Procedure Do_read_header_MMCLBA_CDB10(In_MMCLBA : LongInt); Override;
      Procedure Do_read_header_MSF_CDB10(In_MSF : T_MSF); Override;
      Procedure Do_readCD_MSF_byFormat_CDB12(Var In_StartMSF        : T_MSF;
                                             Var In_EndMSF          : T_MSF;
                                                 In_SectTypeFilter  : Byte;
                                                 In_ReadFormatFlags : Byte;
                                                 In_SubChSel        : Byte); Override;
      Procedure Do_set_CD_speed_CDB12(In_ReadSpeed  : Word;
                                      In_WriteSpeed : Word); Override;
      Procedure Do_readCD_byFormat_CDB12(In_Start_MMCLBA    : LongInt;
                                         In_N_sectors       : LongWord;
                                         In_SectTypeFilter  : Byte;
                                         In_ReadFormatFlags : Byte;
                                         In_SubChSel        : Byte); Override;
      Procedure Do_read_buf_cap_CDB10; Override;
      Procedure Do_read_disc_info_CDB10; Override;
      Procedure Do_read_disc_info_with_OPC_CDB10; Override;
      Procedure Do_read_track_info_CDB10(In_TrkBIT       : Byte;
                                         In_MMCLBA_TrkNo : LongInt); Override;
      Procedure Do_reserve_track_CDB10(In_Reverv_size : LongWord); Override;
      Procedure Do_synchronize_cache_CDB10(In_MMCLBA   : LongInt;
                                           In_N_blocks : Word); Override;
      Procedure Do_write_CDB10(In_DPO_FUA_RELADR : Byte;
                               In_MMCLBA         : LongInt;
                               In_N_blocks       : Word;
                               In_block_size     : Word); Override;
      {** Select/sense for MMC mode parameters.. **}
      Function Calc_modepage6_ptr : Pointer; Override;
      Function Calc_modepage10_ptr : Pointer; Override;
      Procedure Do_select6_read_err_rec(In_err_rec_param    : Byte;
                                        In_read_retry_count : Byte); Override;
      Procedure Do_select10_read_err_rec(In_err_rec_param    : Byte;
                                         In_read_retry_count : Byte); Override;
      Procedure Do_sense6_read_err_rec; Override;
      Procedure Do_sense10_read_err_rec; Override;
      Procedure Do_select6_write(Var In_write_params : T_pub_write_params); Override;
      Procedure Do_select10_write(Var In_write_params : T_pub_write_params); Override;
      Procedure Do_sense6_write; Override;
      Procedure Do_sense10_write; Override;
      Procedure Do_select6_veri_err_rec(In_err_rec_param    : Byte;
                                        In_veri_retry_count : Byte); Override;
      Procedure Do_select10_veri_err_rec(In_err_rec_param    : Byte;
                                         In_veri_retry_count : Byte); Override;
      Procedure Do_sense6_veri_err_rec; Override;
      Procedure Do_sense10_veri_err_rec; Override;
      Procedure Do_select6_CD(In_inactivity_mul : Byte;
                              In_sec_per_MSF    : Word;
                              In_frames_per_MSF : Word); Override;
      Procedure Do_select10_CD(In_inactivity_mul : Byte;
                               In_sec_per_MSF    : Word;
                               In_frames_per_MSF : Word); Override;
      Procedure Do_sense6_CD; Override;
      Procedure Do_sense10_CD; Override;
      Procedure Do_select6_CD_audio_ctrl(Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl); Override;
      Procedure Do_select10_CD_audio_ctrl(Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl); Override;
      Procedure Do_sense6_CD_audio_ctrl; Override;
      Procedure Do_sense10_CD_audio_ctrl; Override;
      Procedure Do_sense6_CD_cap_mech_st_sub6; Override;
      Procedure Do_sense10_CD_cap_mech_st_sub6; Override;
      Procedure Do_sense6_CD_cap_mech_st; Override;
      Procedure Do_sense10_CD_cap_mech_st; Override;
      { Friendly versions of SCSI MMC command methods.. }
      Procedure Do_read_rec_cap_out_CDB10(Var LastMMCLBA     : LongInt;
                                          Var MMCLBABlockLen : LongWord); Override;
      Procedure Do_read_disc_info_out_CDB10(Var Out_read_disc_info_CDB10_block : T_out_read_disc_info_CDB10_block); Override;
      Procedure Do_read_disc_info_with_OPC_out_CDB10(Var Out_read_disc_info_CDB10_block : T_out_read_disc_info_CDB10_block); Override;
      Procedure Do_read_track_info_out_CDB10(    In_TrkBIT       : Byte;
                                                 In_MMCLBA_TrkNo : LongInt;
                                             Var Out_read_track_info_CDB10 : T_out_read_track_info_CDB10_block); Override;
      { Variations of read subchannel command methods.. }
      Procedure Do_read_subch_curr_MMCLBA_CDB10(Var Out_abs_MMCLBA : LongInt;
                                                Var Out_rel_MMCLBA : LongInt); Override;
      Procedure Do_read_subch_curr_MSF_CDB10(Var Out_abs_MSF : T_MSF;
                                             Var Out_rel_MSF : T_MSF); Override;
      Procedure Do_read_subch_aud_st_out_CDB10(Var Out_audioStatus : Byte); Override;
      Procedure Do_read_subch_MCN_out_CDB10(Var Out_MC_valid : Boolean;
                                            Var Out_MCN_str  : String); Override;
      Procedure Do_read_subch_ISRC_out_CDB10(    In_TrackNo   : Byte;
                                             Var Out_TC_valid : Boolean;
                                             Var Out_ISRC_str : String); Override;
      { Variations of read TOC/PMA/ATIP command methods.. }
      Function Do_read_T_P_A_TOC_MMCLBA_first_out_CDB10(Var Out_first_trkNo : Byte;
                                                        Var Out_last_trkNo  : Byte;
                                                        Var Out_TOC_desc    : T_out_read_T_P_A_TOC_desc_MMCLBA;
                                                        Track_no            : Byte) : Boolean; Override;
      Function Do_read_T_P_A_TOC_MMCLBA_next_out_CDB10(Var Out_TOC_desc : T_out_read_T_P_A_TOC_desc_MMCLBA) : Boolean; Override;
      Function Do_read_T_P_A_TOC_MSF_first_out_CDB10(Var Out_first_trkNo : Byte;
                                                     Var Out_last_trkNo  : Byte;
                                                     Var Out_TOC_desc    : T_out_read_T_P_A_TOC_desc_MSF;
                                                     Track_no            : Byte) : Boolean; Override;
      Function Do_read_T_P_A_TOC_MSF_next_out_CDB10(Var Out_TOC_desc : T_out_read_T_P_A_TOC_desc_MSF) : Boolean; Override;
      Function Do_read_T_P_A_sess_MMCLBA_out_CDB10(Var Out_first_sessNo : Byte;
                                                   Var Out_last_sessNo  : Byte;
                                                   Var Out_sess_desc : T_out_read_T_P_A_sess_desc_MMCLBA) : Boolean; Override;
      Function Do_read_T_P_A_sess_MSF_out_CDB10(Var Out_first_sessNo : Byte;
                                                Var Out_last_sessNo  : Byte;
                                                Var Out_sess_desc : T_out_read_T_P_A_sess_desc_MSF) : Boolean; Override;
      Function Do_read_T_P_A_full_TOC_first_out_CDB10(Var Out_first_sessNo  : Byte;
                                                      Var Out_last_sessNo   : Byte;
                                                      Var Out_full_TOC_desc : T_out_read_T_P_A_full_TOC_desc;
                                                      Session_no            : Byte) : Boolean; Override;
      Function Do_read_T_P_A_full_TOC_next_out_CDB10(Var Out_full_TOC_desc : T_out_read_T_P_A_full_TOC_desc) : Boolean; Override;
      Function Do_read_T_P_A_PMA_first_out_CDB10(Var Out_PMA_desc : T_out_read_T_P_A_PMA_desc) : Boolean; Override;
      Function Do_read_T_P_A_PMA_next_out_CDB10(Var Out_PMA_desc : T_out_read_T_P_A_PMA_desc) : Boolean; Override;
      Function Do_read_T_P_A_ATIP_out_CDB10(Var Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc) : Boolean; Override;
      Procedure Do_sense6_CD_cap_mech_st_sub6_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); Override;
      Procedure Do_sense10_CD_cap_mech_st_sub6_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); Override;
      Procedure Do_sense6_CD_cap_mech_st_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); Override;
      Procedure Do_sense10_CD_cap_mech_st_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); Override;
End;

implementation

Constructor T_MMC1_WNASPI32.Create(Var In_SPC_WNASPI32 : T_SPC_WNASPI32);
Begin
     SPC_WNASPI32:=In_SPC_WNASPI32;
     MMC1:=T_MMC1.Create;
End;

Destructor T_MMC1_WNASPI32.Destroy;
Begin
     MMC1.Destroy;
End;

Function T_MMC1_WNASPI32.Get_MMC1 : T_MMC1;
Begin
     Result:=MMC1;
End;

Function T_MMC1_WNASPI32.Get_err_msg : PChar;
{ *****************************************
  A wrapper to Get_err_msg in SPC_WNASPI32.
  ***************************************** }
Begin
     Result:=SPC_WNASPI32.Get_err_msg;
End;

Procedure T_MMC1_WNASPI32.Do_read_rec_cap_CDB10;
{ ********************************************************
  Sends a read recorded capacity (CDB10) command to target
  device.

  Note: on some CDROM/CD writer drives they will return
  FFFFh and SRB status OK.  It should have returned a
  SRB status error!!
  ******************************************************** }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, 8, 10);
     MMC1.Fill_CDB10_do_read_rec_cap(T_SRB_for_readCD_rec_cap(SPC_WNASPI32.WNASPI32.P_SRB^).readCD_rec_cap);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_read_subch_CDB10(In_MSF     : Byte;
                                              In_SubQ    : Byte;
                                              In_Params  : Byte;
                                              In_TrackNo : Byte);
{ **********************************************************
  Sends a read sub-channel (CDB10) command to target device.
  ********************************************************** }
Var
   Buf_len : LongWord;
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     If In_SubQ=MMC_READ_SUBCH_SUBQ Then
     Begin
          Case In_Params Of
               MMC_READ_SUBCH_CURR_POS:
               Begin
                    Buf_len:=SizeOf(T_read_subch_CD_curr_pos_data);
               End;
               MMC_READ_SUBCH_MCN:
               Begin
                    Buf_len:=SizeOf(T_read_subch_MCN_data);
               End;
               MMC_READ_SUBCH_ISRC:
               Begin
                    Buf_len:=SizeOf(T_read_subch_ISRC_data);
               End;
          End;
     End;
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, Buf_len, 10);
     MMC1.Fill_CDB10_do_read_subch(T_SRB_for_read_subch(SPC_WNASPI32.WNASPI32.P_SRB^).Read_subch,
                                   In_MSF,
                                   In_SubQ,
                                   In_Params,
                                   In_TrackNo);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_read_T_P_A_CDB10(In_MSF       : Byte;
                                              In_format    : Byte;
                                              In_trkSessNo : Byte);
{ ***********************************************************
  Sends a read TOC/PMA/ATIP (CDB10) command to target device.
  This is a 3 in 1 command.
  *********************************************************** }
Var
   Buf_len : LongWord;
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     Case In_format Of
          MMC_READ_T_P_A_FORMAT_TOC:
          Begin
               //Specs don't allow to determine buffer size, so using arbitrary size
               Buf_len:=USER_MEM_UNIT_SMALLMIN;
          End;
          MMC_READ_T_P_A_FORMAT_SESS_INFO:
          Begin
               //Specific size is known
               Buf_len:=SizeOf(T_read_T_P_A_sess_info_data);
          End;
          MMC_READ_T_P_A_FORMAT_FULL_TOC:
          Begin
               //Specs don't allow to determine buffer size, so using arbitrary size
               Buf_len:=USER_MEM_UNIT_SMALLMIN;
          End;
          MMC_READ_T_P_A_FORMAT_PMA:
          Begin
               //Specs don't allow to determine buffer size, so using arbitrary size
               Buf_len:=USER_MEM_UNIT_SMALLMIN;
          End;
          MMC_READ_T_P_A_FORMAT_ATIP:
          Begin
               Buf_len:=SizeOf(T_read_T_P_A_ATIP_data);
          End
     Else
         //Unknown format, so using arbitrary size
         Buf_len:=USER_MEM_UNIT_SMALLMIN;
     End;
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, Buf_len, 10);
     MMC1.Fill_CDB10_do_read_T_P_A(T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A,
                                   In_MSF,
                                   In_format,
                                   In_trkSessNo);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_read_header_CDB10(In_MSF        : Byte;
                                               In_MMCLBA_MSF : Pointer);
{ ************************************************************************
  Sends a read header (CDB10) command using MSF or MMCLBA option to target
  device.
  ************************************************************************ }
Var
   Buf_len : LongWord;
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     Case In_MSF Of
          MMC_READ_HEADER_MMCLBA:
          Begin
               Buf_len:=SizeOf(T_read_header_MMCLBA_data);
          End;
          MMC_READ_HEADER_MSF:
          Begin
               Buf_len:=SizeOf(T_read_header_MSF_data);
          End;
     End;
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, Buf_len, 10);
     MMC1.Fill_CDB10_do_read_header(T_SRB_for_read_header(SPC_WNASPI32.WNASPI32.P_SRB^).Read_header,
                                    In_MSF,
                                    In_MMCLBA_MSF);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_read_header_MMCLBA_CDB10(In_MMCLBA : LongInt);
{ *************************************************************************
  Sends a read header (CDB10) command using MMCLBA option to target device.
  ************************************************************************* }
Begin
     Do_read_header_CDB10(MMC_READ_HEADER_MMCLBA,
                          @In_MMCLBA);
End;

Procedure T_MMC1_WNASPI32.Do_read_header_MSF_CDB10(In_MSF : T_MSF);
{ **********************************************************************
  Sends a read header (CDB10) command using MSF option to target device.
  ********************************************************************** }
Begin
     Do_read_header_CDB10(MMC_READ_HEADER_MSF,
                          @In_MSF);
End;

Procedure T_MMC1_WNASPI32.Do_readCD_MSF_byFormat_CDB12(Var In_StartMSF        : T_MSF;
                                                       Var In_EndMSF          : T_MSF;
                                                           In_SectTypeFilter  : Byte;
                                                           In_ReadFormatFlags : Byte;
                                                           In_SubChSel        : Byte);
{ *****************************************************
  Sends a read CD MSF (CDB12) command to target device.
  ***************************************************** }
Var
   Buf_len : LongWord;
   Start_sector : LongInt;
   N_sectors : LongWord;
   //Save_blockSize : LongWord;
   //N_sect_not_read : LongWord;
   //LBA_not_read : LongInt;
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     Buf_len:=MMC1.GetFullBlockSizeFromFilterReadFormat(In_SectTypeFilter,
                                                        In_ReadFormatFlags)+
              MMC1.GetBlockSizeFromSubCh(In_SubChSel);
     Start_sector:=MMC1.MSF_to_LBA(In_StartMSF.M,
                                   In_StartMSF.S,
                                   In_StartMSF.F);
     N_sectors:=MMC1.MSF_to_LBA(In_EndMSF.M,
                                In_EndMSF.S,
                                In_EndMSF.F)-Start_sector+1;
     Buf_len:=Buf_len*N_sectors;
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, Buf_len, 12);
     MMC1.Fill_CDB12_do_readCD_MSF_byFormat(T_SRB_for_readCD_MSF(SPC_WNASPI32.WNASPI32.P_SRB^).ReadCD_MSF,
                                            In_StartMSF,
                                            In_EndMSF,
                                            In_SectTypeFilter,
                                            In_ReadFormatFlags,
                                            In_SubChSel);

     SPC_WNASPI32.V_Send_SRBCDB;

     { ** Code below is commented out - the code is unreliable for all hardware.
     // Below attempts to calculate the number of sectors that were read..
     If WNASPI32.FA_SRB_status_OK Then
     Begin
          N_sect_read:=N_sectors;
     End
     Else
         Begin
              If WNASPI32.Cur_Res_supp Then
              Begin
                   // Below uses residual byte count to calculate number of sectors
                   // read..
                   N_sect_not_read:=T_exec_SCSI_hdr(WNASPI32.P_SRB^).Buf_len Div Save_blockSize;
                   N_sect_read:=N_sectors-N_sect_not_read;
              End
              Else
              Begin
                   If (SPC_WNASPI32.Std_sense_exists) AND
                      ((T_sense_data(WNASPI32.Get_sense_buf^).ResponseCode AND $7F)
                      =SEN_RESP_CURR_ERR) Then
                   Begin
                        With T_sense_data(WNASPI32.Get_sense_buf^) Do
                        Begin
                             // Below uses sense data to calculate number of sectors
                             //  read, but may not work on all devices..
                             LBA_not_read:=MSF_to_LBA(Info1,
                                                      Info2,
                                                      Info3);
                             If LBA_not_read>=MMCLBA_to_LBA(Start_sector) Then
                                 N_sect_read:=LBA_not_read-MMCLBA_to_LBA(Start_sector)
                             Else
                                 N_sect_read:=0;
                        End;
                   End
                   Else
                       N_sect_read:=0;
              End;
         End;
     ** }
End;

Procedure T_MMC1_WNASPI32.Do_set_CD_speed_CDB12(In_ReadSpeed  : Word;
                                                In_WriteSpeed : Word);
{ *************************************
  Sends a set CD speed (CDB12) command.
  ************************************* }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_OUT, 0, 12);
     MMC1.Fill_CDB12_do_set_CD_speed(T_SRB_for_set_CD_speed(SPC_WNASPI32.WNASPI32.P_SRB^).Set_CD_speed,
                                     In_ReadSpeed,
                                     In_WriteSpeed);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_readCD_byFormat_CDB12(In_Start_MMCLBA    : LongInt;
                                                   In_N_sectors       : LongWord;
                                                   In_SectTypeFilter  : Byte;
                                                   In_ReadFormatFlags : Byte;
                                                   In_SubChSel        : Byte);
{ ******************************************************
  Sends a read CD MMCLBA CDB12 command to target device.
  ****************************************************** }
Var
   Buf_len : LongWord;
   N_sectors : LongWord;
   //Save_blockSize : LongWord;
   //N_sect_not_read : LongWord;
   //LBA_not_read : LongInt;
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     Buf_len:=MMC1.GetFullBlockSizeFromFilterReadFormat(In_SectTypeFilter,
                                                        In_ReadFormatFlags)+
              MMC1.GetBlockSizeFromSubCh(In_SubChSel);
     If In_N_sectors=0 Then
         N_sectors:=SPC_WNASPI32.WNASPI32.Get_data_buf_size Div Buf_len
     Else
         N_sectors:=In_N_sectors;
     Buf_len:=Buf_len*N_sectors;
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, Buf_len, 12);
     MMC1.Fill_CDB12_do_readCD_byFormat(T_SRB_for_readCD_CDB12(SPC_WNASPI32.WNASPI32.P_SRB^).ReadCD_CDB12,
                                        In_Start_MMCLBA,
                                        N_sectors,
                                        In_SectTypeFilter,
                                        In_ReadFormatFlags,
                                        In_SubChSel);

     SPC_WNASPI32.V_Send_SRBCDB;

     { ** Code below is commented out - the code is unreliable for all hardware.
     // Below calculates the number of sectors that were read..
     If WNASPI32.FA_SRB_status_OK Then
     Begin
          N_sect_read:=Temp_N_sectors;
     End
     Else
         Begin
              If WNASPI32.Cur_Res_supp Then
              Begin
                   // Below uses residual byte count to calculate number of sectors
                   // read..
                   N_sect_not_read:=T_exec_SCSI_hdr(WNASPI32.P_SRB^).Buf_len Div Save_blockSize;
                   N_sect_read:=Temp_N_sectors-N_sect_not_read;
              End
              Else
              Begin
                   If (SPC_WNASPI32.Std_sense_exists) AND
                      ((T_sense_data(WNASPI32.Get_sense_buf^).ResponseCode AND $7F)
                      =SEN_RESP_CURR_ERR) Then
                   Begin
                        With T_sense_data(WNASPI32.Get_sense_buf^) Do
                        Begin
                             // Below uses sense data to calculate number of sectors
                             //  read, but may not work on all devices..
                             LBA_not_read:=MMCLBA_to_LBA(SPC_WNASPI32.ReverseBytesToLongInt(Info0,
                                                                                          Info1,
                                                                                          Info2,
                                                                                          Info3));
                             If LBA_not_read>=MMCLBA_to_LBA(In_Start_MMCLBA) Then
                                 N_sect_read:=LBA_not_read-MMCLBA_to_LBA(In_Start_MMCLBA)
                             Else
                                 N_sect_read:=0;
                        End;
                   End
                   Else
                       N_sect_read:=0;
              End;
         End;
     ** }
End;

Procedure T_MMC1_WNASPI32.Do_read_buf_cap_CDB10;
{ ************************************************************
  Sends a read buffer capacity CDB10 command to target device.
  ************************************************************ }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, SizeOf(T_read_buf_cap_CDB10_block), 10);
     MMC1.Fill_CDB10_do_read_buf_cap(T_SRB_for_read_buf_cap_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Read_buf_cap_CDB10);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_read_disc_info_CDB10;
{ ******************************************************
  Sends a read disc info CDB10 command to target device.
  Excludes returning of OPC table entries.
  ****************************************************** }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, SizeOf(T_read_disc_info_CDB10_block), 10);
     MMC1.Fill_CDB10_do_read_disc_info(T_SRB_for_read_disc_info_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Read_disc_info_CDB10, SizeOf(T_read_disc_info_CDB10_block));

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_read_disc_info_with_OPC_CDB10;
{ ******************************************************
  Sends a read disc info CDB10 command to target device.
  Includes returning of OPC table entries.
  ****************************************************** }
Var
   OPC_table_entries_total_bytes : Byte;
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, SizeOf(T_read_disc_info_CDB10_block), 10);
     MMC1.Fill_CDB10_do_read_disc_info(T_SRB_for_read_disc_info_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Read_disc_info_CDB10, SizeOf(T_read_disc_info_CDB10_block));

     SPC_WNASPI32.V_Send_SRBCDB;

     //Check if there are any OPC table entries.
     If T_out_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_OPC_tab_entries>0 Then
     Begin
          OPC_table_entries_total_bytes:=T_out_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_OPC_tab_entries*8;
          SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, SizeOf(T_read_disc_info_CDB10_block)+OPC_table_entries_total_bytes, 10);
          MMC1.Fill_CDB10_do_read_disc_info(T_SRB_for_read_disc_info_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Read_disc_info_CDB10, SizeOf(T_read_disc_info_CDB10_block)+OPC_table_entries_total_bytes);

          SPC_WNASPI32.V_Send_SRBCDB;
     End;
End;

Procedure T_MMC1_WNASPI32.Do_read_track_info_CDB10(In_TrkBIT       : Byte;
                                                   In_MMCLBA_TrkNo : LongInt);
{ *******************************************************
  Sends a read track info CDB10 command to target device.
  ******************************************************* }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, SizeOf(T_read_track_info_CDB10_block), 10);
     MMC1.Fill_CDB10_do_read_track_info(T_SRB_for_read_track_info_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Read_track_info_CDB10,
                                        In_TrkBIT,
                                        In_MMCLBA_TrkNo);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_reserve_track_CDB10(In_Reverv_size : LongWord);
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_NO_DATA, 0, 10);
     MMC1.Fill_CDB10_do_reserve_track(T_SRB_for_reserve_track_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Reserve_track_CDB10,
                                      In_Reverv_size);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_synchronize_cache_CDB10(In_MMCLBA   : LongInt;
                                                     In_N_blocks : Word);
{ *********************************************************
  Sends a synchronize cache CDB10 command to target device.
  This is for writing any remaining data in cache to CDR/W.
  ********************************************************* }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_NO_DATA, 0, 10);
     MMC1.Fill_CDB10_do_synchronize_cache(T_SRB_for_synchronize_cache_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Synchronize_cache_CDB10,
                                          In_MMCLBA,
                                          In_N_blocks);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_MMC1_WNASPI32.Do_write_CDB10(In_DPO_FUA_RELADR : Byte;
                                         In_MMCLBA         : LongInt;
                                         In_N_blocks       : Word;
                                         In_block_size     : Word);
{ *********************************************
  Sends a write CDB10 command to target device.
  ********************************************* }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_OUT, In_N_blocks*In_block_size, 10);
     MMC1.Fill_CDB10_do_write(T_SRB_for_write_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Write_CDB10,
                              In_DPO_FUA_RELADR,
                              In_MMCLBA,
                              In_N_blocks,
                              In_block_size);

     SPC_WNASPI32.V_Send_SRBCDB;
End;

Function T_MMC1_WNASPI32.Get_modesense6_DBD_last_used : Byte;
{ *********************************************
  Reads the CDB in the SRB to get the DBD value
  that was used.  This assumes the previous
  command sent was mode sense CDB6.
  ********************************************* }
Begin
     With T_SRB_for_mode_sense_CDB6(SPC_WNASPI32.WNASPI32.P_SRB^) Do
     Begin
          With Mode_sense_CDB6 Do
          Begin
               Get_modesense6_DBD_last_used:=DBD;
          End;
     End;
End;

Function T_MMC1_WNASPI32.Get_modesense10_DBD_last_used : Byte;
{ *********************************************
  Reads the CDB in the SRB to get the DBD value
  that was used.  This assumes the previous
  command sent was mode sense CDB10.
  ********************************************* }
Begin
     With T_SRB_for_mode_sense_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^) Do
     Begin
          With Mode_sense_CDB10 Do
          Begin
               Get_modesense10_DBD_last_used:=DBD;
          End;
     End;
End;

Function T_MMC1_WNASPI32.Is_sense6_block_desc_returned(PageCode : Byte) : Boolean;
{ ***************************************************
  Checks whether any block descriptor/s are returned.
  Sense data is first read from the device.
  If making the sense call failed then it will
  assume that no block descriptor is returned.
  Also assumes that the command will complete in
  a short amount of time - otherwise the app will
  appear to hang.
  *************************************************** }
Begin
     { Sends a mode sense CDB6 with request for returning of block descriptor/s
       and retrieving just the 4 byte parameter header to determine if any block
       descriptor/s are used by the device. }
     SPC_WNASPI32.Mode_sense_CDB6(MODE_SENSE_DBD_BLK_DESC,
                                  PageCode,
                                  SizeOf(T_mode_param_hdr_6));
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          { Checks whether any block descriptor/s are returned. }
          Is_sense6_block_desc_returned:=Is_sense6_block_desc_returned_from_data;
     End
     Else
     Begin
          { Previous mode sense command failed, tries again, but with the
            constant: MODE_SENSE_DBD_NO_BLK_DESC. }

          { Actually, we don't need to check since constant means no returning
            of block descriptors, so we should assume no block descriptors.
            We still need to call the mode sense to give a SRB status for the
            calling procedure/function. }

          { Sends a mode sense CDB6 with request for returning no block
            descriptors and retrieving just the 4 byte parameter header. }
          SPC_WNASPI32.Mode_sense_CDB6(MODE_SENSE_DBD_NO_BLK_DESC,
                                       PageCode,
                                       SizeOf(T_mode_param_hdr_6));

          Is_sense6_block_desc_returned:=False;
     End;
End;

Function T_MMC1_WNASPI32.Is_sense10_block_desc_returned(PageCode : Byte) : Boolean;
{ *************************************************
  Checks whether a block descriptor/s are returned.
  Sense data is first read from the device.
  If making the sense call failed then it will
  assume that no block descriptor is returned.
  Also assumes that the command will complete in
  a short amount of time - otherwise the app will
  appear to hang.
  ************************************************* }
Begin
     { Sends a mode sense CDB10 with request for returning of block descriptor/s
       and retrieving just the 8 byte parameter header to determine if any block
       descriptor/s are used by the device. }
     SPC_WNASPI32.Mode_sense_CDB10(MODE_SENSE_DBD_BLK_DESC,
                                   PageCode,
                                   SizeOf(T_mode_param_hdr_10));
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          { Checks whether any block descriptor/s are returned. }
          Is_sense10_block_desc_returned:=Is_sense10_block_desc_returned_from_data;
     End
     Else
     Begin
          { Previous mode sense command failed, tries again, but with the
            constant: MODE_SENSE_DBD_NO_BLK_DESC. }

          { Actually, we don't need to check since constant means no returning
            of block descriptors, so we should assume no block descriptors.
            We still need to call the mode sense to give a SRB status for the
            calling procedure/function. }

          { Sends a mode sense CDB10 with request for returning no block
            descriptors and retrieving just the 8 byte parameter header. }
          SPC_WNASPI32.Mode_sense_CDB10(MODE_SENSE_DBD_NO_BLK_DESC,
                                        PageCode,
                                        SizeOf(T_mode_param_hdr_10));

          Is_sense10_block_desc_returned:=False;
     End;
End;

Function T_MMC1_WNASPI32.Is_sense6_block_desc_returned_from_data : Boolean;
{ ************************************************
  Check/s whether any block descriptor/s were
  returned from the recent mode sense 6 call.
  Becareful when using this function, it will
  use the data in the transfer buffer for checking
  - so the buffer should have valid sense data,
  i.e. a sense 6 call should be made prior to
  calling this method.
  This method is used by the method:
  Is_sense6_block_desc_returned
  ************************************************ }
Begin
     With T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
     Begin
          { Checks whether any block descriptor/s were returned. }
          If Block_desc_len=0 Then
          Begin
               Is_sense6_block_desc_returned_from_data:=False;
          End
          Else
          Begin
               Is_sense6_block_desc_returned_from_data:=True;
          End;
     End;
End;

Function T_MMC1_WNASPI32.Is_sense10_block_desc_returned_from_data : Boolean;
{ ************************************************
  Checks whether any block descriptor/s were
  returned from the recent mode sense 10 call.
  Becareful when using this function, it will
  use the data in the transfer buffer for checking
  - so the buffer should have valid sense data,
  i.e. a sense 10 call should be made prior to
  calling this method.
  This method is used by the method:
  Is_sense10_block_desc_returned
  ************************************************ }
Begin
     With T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
     Begin
          { Checks whether any block descriptor/s were returned.
            The ReverseBytesToWord need not be used, instead MSB and LSB
            can be checked to both equate 0. }
          If SPC_WNASPI32.SPC.ReverseBytesToWord(Block_desc_len_MSB,
                                                 Block_desc_len_LSB)=0 Then
          Begin
               Is_sense10_block_desc_returned_from_data:=False;
          End
          Else
          Begin
               Is_sense10_block_desc_returned_from_data:=True;
          End;
     End;
End;

Function T_MMC1_WNASPI32.Calc_modepage6_ptr : Pointer;
{ **************************************************
  Calculates the address where the mode page begins.
  A pointer with the address is returned.
  This uses the recent mode sense 6 call.
  Becareful when using this function, it uses the
  data in the transfer buffer for finding the
  address - so the buffer should have valid sense
  data.
  ************************************************** }
Var
   Block_desc_len : Byte;
Begin
     Block_desc_len:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len div 8 * 8;

     Calc_modepage6_ptr:=Ptr(Integer(SPC_WNASPI32.WNASPI32.Get_data_buf)+
                             SizeOf(T_mode_param_hdr_6)+
                             Block_desc_len);
End;

Function T_MMC1_WNASPI32.Calc_modepage10_ptr : Pointer;
{ **************************************************
  Calculates the address where the mode page begins.
  A pointer with the address is returned.
  This uses the recent mode sense 10 call.
  Becareful when using this function, it uses the
  data in the transfer buffer for finding the
  address - so the buffer should have a valid sense
  data.
  ************************************************** }
Var
   Block_desc_len : Byte;
Begin
     if (T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1 and 1)=0 then
     begin
          Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                              T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                          div 8 * 8;
     end
     else
     begin
          Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                              T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                          div 16 * 16;
     end;

     Calc_modepage10_ptr:=Ptr(Integer(SPC_WNASPI32.WNASPI32.Get_data_buf)+
                              SizeOf(T_mode_param_hdr_10)+
                              Block_desc_len);
End;

Function T_MMC1_WNASPI32.Calc_sense6_return_cap_from_param_len(PageCode : Byte) : Byte;
{ ****************************************************************************************
  This function is for compatibility of being able to issue sense commands to various
  CDROM drives without Win ASPI32 or the drive waiting for ever.

  Calculates return capacity using sense CDB 6 for a PageCode.

  This method will first get the header (i.e. without retrieving the block descriptor and
  parameters) to find out how much data the drive can return.  Then use this to retrieve
  the length of the full parameter list.  This version uses the page param_len in the mode
  page for the calculation.  If call to mode sense failed then Returns 0.

  Extra info about the problem:
        Some old CDROM drives do not return the last 2 or 6 bytes and Win ASPI32 will wait
  forever.  The reason for this is because they are allowed to return less than the full
  mode parameters.  Also, drives may or may not return a block descriptor.
        If you ask the drive to return more data than it can return Win Aspi32 will wait
  for ever, well until the device or SCSI bus timesout, which may be a very long time!!
  **************************************************************************************** }
Var Return_cap : Byte;
    P : Pointer;
    is_blocks : Boolean;
    dbd : Byte;
Begin
     { Checks whether any block descriptor/s were returned.  Then calculate
       offset to Param_len and retrieve it. }
     is_blocks:=Is_sense6_block_desc_returned(PageCode);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          If is_blocks=False Then
          Begin
               dbd:=Get_modesense6_DBD_last_used;

               //Assume no block descriptor/s will be returned.
               //Retrieve parameter length.
               Return_cap:=SizeOf(T_mode_param_hdr_6)+2;
               SPC_WNASPI32.Mode_sense_CDB6(dbd,
                                            PageCode,
                                            Return_cap);
               If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
               Begin
                    With T_mode_param_6_hdr_noblk_2_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
                    Begin
                         Return_cap:=Return_cap+Param_len;
                    End;
               End
               Else
               Begin
                    Return_cap:=0;
               End;
          End
          Else
          Begin
               //Assume block descriptor/s will be returned.
               //Retrieve parameter length.
               Return_cap:=SizeOf(T_mode_param_hdr_6)+
                           T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len+
                           2;
               SPC_WNASPI32.Mode_sense_CDB6(MODE_SENSE_DBD_BLK_DESC,
                                            PageCode,
                                            Return_cap);
               If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
               Begin
                    P:=Ptr(Integer(SPC_WNASPI32.WNASPI32.Get_data_buf)+Return_cap-1);
                    Return_cap:=Return_cap+Byte(P^);
               End
               Else
               Begin
                    Return_cap:=0;
               End;
          End;
     End
     Else
         Return_cap:=0;

     Calc_sense6_return_cap_from_param_len:=Return_cap;
End;

Function T_MMC1_WNASPI32.Calc_sense10_return_cap_from_param_len(PageCode : Byte) : Word;
{ ****************************************************************************************
  This function is for compatibility of being able to issue sense commands to various
  CDROM drives without Win ASPI32 or the drive waiting for ever.

  Calculates return capacity using sense CDB 10 for a PageCode.

  This method will first get the header (i.e. without retrieving the block descriptor and
  parameters) to find out how much data the drive can return.  Then use this to retrieve
  the length of the full parameter list.  This version uses the page param_len in the mode
  page for the calculation.  If call to mode sense failed then Returns 0.

  Extra info about the problem:
        Some old CDROM drives do not return the last 2 or 6 bytes and Win ASPI32 will wait
  forever.  The reason for this is because they are allowed to return less than the full
  mode parameters.  Also, drives may or may not return a block descriptor.
        If you ask the drive to return more data than it can return Win Aspi32 will wait
  for ever, well until the device or SCSI bus timesout, which may be a very long time!!
  **************************************************************************************** }
Var Return_cap : Word;
    P : Pointer;
    is_blocks : Boolean;
    dbd : Byte;
Begin
     { Checks whether any block descriptor/s were returned.  Then calculate
       offset to Param_len and retrieve it. }
     is_blocks:=Is_sense10_block_desc_returned(PageCode);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          If is_blocks=False Then
          Begin
               dbd:=Get_modesense10_DBD_last_used;

               //Assume no block descriptor/s will be returned.
               //Retrieve parameter length.
               Return_cap:=SizeOf(T_mode_param_hdr_10)+2;
               SPC_WNASPI32.Mode_sense_CDB10(dbd,
                                             PageCode,
                                             Return_cap);
               If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
               Begin
                    With T_mode_param_10_hdr_noblk_2_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
                    Begin
                         Return_cap:=Return_cap+Param_len;
                    End;
               End
               Else
               Begin
                    Return_cap:=0;
               End;
          End
          Else
          Begin
               //Assume block descriptor/s will be returned.
               //Retrieve parameter length.
               Return_cap:=SizeOf(T_mode_param_hdr_10)+
                           SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                               T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)+
                           2;
               SPC_WNASPI32.Mode_sense_CDB10(MODE_SENSE_DBD_BLK_DESC,
                                             PageCode,
                                             Return_cap);
               If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
               Begin
                    P:=Ptr(Integer(SPC_WNASPI32.WNASPI32.Get_data_buf)+Return_cap-1);
                    Return_cap:=Return_cap+Word(P^);
               End
               Else
               Begin
                    Return_cap:=0;
               End;
          End;
     End
     Else
         Return_cap:=0;

     Calc_sense10_return_cap_from_param_len:=Return_cap;
End;

Function T_MMC1_WNASPI32.Calc_sense6_return_cap_from_mode_data_len(PageCode : Byte) : Byte;
{ ****************************************************************************************
  This function is for compatibility of being able to issue sense commands to various
  CDROM drives without Win ASPI32 or the drive waiting for ever.

  Calculates return capacity using sense CDB 6 for a PageCode.

  This method will first get the header (i.e. without retrieving the block descriptor and
  parameters) to find out how much data the drive can return.  Then use this to retrieve
  the length of the full parameter list.  This version uses the mode_data_len in the header
  for the calculation.  If call to mode sense failed then Returns 0.

  Extra info about the problem:
        Some old CDROM drives do not return the last 2 or 6 bytes and Win ASPI32 will wait
  forever.  The reason for this is because they are allowed to return less than the full
  mode parameters.  Also, drives may or may not return a block descriptor.
        If you ask the drive to return more data than it can return Win Aspi32 will wait
  for ever, well until the device or SCSI bus timesout, which may be a very long time!!
  **************************************************************************************** }
Var Return_cap : Byte;
Begin
     { Sends a mode sense CDB6 with request for returning of block descriptor/s
       and retrieve just the 4 byte parameter header to calculate the return
       capacity of the mode page. }
     SPC_WNASPI32.Mode_sense_CDB6(MODE_SENSE_DBD_BLK_DESC,
                                  PageCode,
                                  SizeOf(T_mode_param_hdr_6));
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               { Checks whether the mode data len is at least greater than the header. }
               If Mode_data_len+1>SizeOf(T_mode_param_hdr_6) Then
               Begin
                    //Assumes mode data len is okay.  Mode data len is less 1,
                    //so we need to add 1 to it.
                    Return_cap:=Mode_data_len+1;
               End
               Else
               Begin
                    Return_cap:=0;
               End;
          End;
     End
     Else
     Begin
          { Previous mode sense command failed, tries again, but with the
            constant: MODE_SENSE_DBD_NO_BLK_DESC. }

          { Sends a mode sense CDB10 with request for returning no block
            descriptors and retrieve just the 4 byte parameter header to
            calculate the return capacity of the mode page. }
          SPC_WNASPI32.Mode_sense_CDB6(MODE_SENSE_DBD_NO_BLK_DESC,
                                       PageCode,
                                       SizeOf(T_mode_param_hdr_6));
          If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
          Begin
               With T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    { Checks whether the mode data len is at least greater than the header. }
                    If Mode_data_len+1>SizeOf(T_mode_param_hdr_6) Then
                    Begin
                         //Assumes mode data len is okay.  Mode data len is less 1,
                         //so we need to add 1 to it.
                         Return_cap:=Mode_data_len+1;
                    End
                    Else
                    Begin
                         Return_cap:=0;
                    End;
               End;
          End
          Else
              Return_cap:=0;
     End;

     Calc_sense6_return_cap_from_mode_data_len:=Return_cap;
End;

Function T_MMC1_WNASPI32.Calc_sense10_return_cap_from_mode_data_len(PageCode : Byte) : Word;
{ ****************************************************************************************
  This function is for compatibility of being able to issue sense commands to various
  CDROM drives without Win ASPI32 or the drive waiting for ever.

  Calculates return capacity using sense CDB 10 for a PageCode.

  This method will first get the header (i.e. without retrieving the block descriptor and
  parameters) to find out how much data the drive can return.  Then use this to retrieve
  the length of the full parameter list.  This version uses the mode_data_len in the header
  for the calculation.  If call to mode sense failed then Returns 0.

  Extra info about the problem:
        Some old CDROM drives do not return the last 2 or 6 bytes and Win ASPI32 will wait
  forever.  The reason for this is because they are allowed to return less than the full
  mode parameters.  Also, drives may or may not return a block descriptor.
        If you ask the drive to return more data than it can return Win Aspi32 will wait
  for ever, well until the device or SCSI bus timesout, which may be a very long time!!
  **************************************************************************************** }
Var Return_cap : Word;
Begin
     { Sends a mode sense CDB6 with request for returning of block descriptor/s
       and retrieve just the 8 byte parameter header to calculate the return
       capacity of the mode page. }
     SPC_WNASPI32.Mode_sense_CDB10(MODE_SENSE_DBD_BLK_DESC,
                                   PageCode,
                                   SizeOf(T_mode_param_hdr_10));
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               { Checks whether the mode data len is at least greater than the header. }
               If SPC_WNASPI32.SPC.ReverseBytesToWord(Mode_data_len_MSB,
                                                      Mode_data_len_LSB)+2>SizeOf(T_mode_param_hdr_10) Then
               Begin
                    //Assumes mode data len is okay.  Mode data len is less 2,
                    //so we need to add 2 to it.
                    Return_cap:=SPC_WNASPI32.SPC.ReverseBytesToWord(Mode_data_len_MSB,
                                                                    Mode_data_len_LSB)+2;
               End
               Else
               Begin
                    Return_cap:=0;
               End;
          End;
     End
     Else
     Begin
          { Previous mode sense command failed, tries again, but with the
            constant: MODE_SENSE_DBD_NO_BLK_DESC }

          { Sends a mode sense CDB10 with request for returning no block
            descriptors and retrieve just the 8 byte parameter header to
            calculate the return capacity of the mode page. }
          SPC_WNASPI32.Mode_sense_CDB10(MODE_SENSE_DBD_NO_BLK_DESC,
                                        PageCode,
                                        SizeOf(T_mode_param_hdr_10));
          If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
          Begin
               With T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    { Check whether the mode data len is at least greater than the header. }
                    If SPC_WNASPI32.SPC.ReverseBytesToWord(Mode_data_len_MSB,
                                                           Mode_data_len_LSB)+2>SizeOf(T_mode_param_hdr_10) Then
                    Begin
                         //Assumes mode data len is okay.  Mode data len is less 2,
                         //so we need to add 2 to it.
                         Return_cap:=SPC_WNASPI32.SPC.ReverseBytesToWord(Mode_data_len_MSB,
                                                                         Mode_data_len_LSB)+2;
                    End
                    Else
                    Begin
                         Return_cap:=0;
                    End;
               End;
          End
          Else
              Return_cap:=0;
     End;

     Calc_sense10_return_cap_from_mode_data_len:=Return_cap;
End;

Function T_MMC1_WNASPI32.Calc_sense6_return_cap_from_mode_data_and_param_len(PageCode : Byte) : Byte;
{ **************************************************************************
  Calculates the return capacity for mode sense CDB6 using the returned mode
  data len in the mode sense CDB6 header.  If return capacity is 0 then use
  param len from beginning of the returned mode page data.
  ************************************************************************** }
Var Return_cap : Byte;
Begin
     Return_cap:=Calc_sense6_return_cap_from_mode_data_len(PageCode);
     If Return_cap=0 Then
     Begin
          Return_cap:=Calc_sense6_return_cap_from_param_len(PageCode);
     End;

     Calc_sense6_return_cap_from_mode_data_and_param_len:=Return_cap;
End;

Function T_MMC1_WNASPI32.Calc_sense10_return_cap_from_mode_data_and_param_len(PageCode : Byte) : Word;
{ ***************************************************************************
  Calculates the return capacity for mode sense CDB10 using the returned mode
  data len in the mode sense CDB10 header.  If return capacity is 0 then use
  param len from beginning of the returned mode page data.
  *************************************************************************** }
Var Return_cap : Word;
Begin
     Return_cap:=Calc_sense10_return_cap_from_mode_data_len(PageCode);
     If Return_cap=0 Then
     Begin
          Return_cap:=Calc_sense10_return_cap_from_param_len(PageCode);
     End;

     Calc_sense10_return_cap_from_mode_data_and_param_len:=Return_cap;
End;

Procedure T_MMC1_WNASPI32.Do_select6_read_err_rec(In_err_rec_param    : Byte;
                                                  In_read_retry_count : Byte);
{ *******************************************************************
  Sends read err recovery parameters using mode select CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ******************************************************************* }
Var Alloc_size : Byte;
    Block_desc_len : Byte;
    P_read_err_rec_param_page : Pointer;
Begin
     Do_sense6_read_err_rec;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Block_desc_len:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len
                          div 8 * 8;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_6_read_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_read_err_rec(@Read_err_rec,
                                                   Read_err_rec.Param_len,
                                                   In_err_rec_param,
                                                   In_read_retry_count,
                                                   True);
                    Alloc_size:=Hdr.Mode_data_len+1;
                    //Alloc_size:=SizeOf(T_mode_param_6_read_err_rec_noblk_data);
               End;
          End
          Else
          Begin
               P_read_err_rec_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_read_err_rec_param_page), SizeOf(T_mode_param_hdr_6) + Block_desc_len);
               MMC1.Fill_mode_pg_read_err_rec(P_read_err_rec_param_page,
                                              T_mode_pg_read_err_rec(P_read_err_rec_param_page^).Param_len,
                                              In_err_rec_param,
                                              In_read_retry_count,
                                              True);
               Alloc_size:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len+1;
               //Alloc_size:=SizeOf(T_mode_param_6_read_err_rec_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_6_read_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_read_err_rec(@Read_err_rec,
                                              6,
                                              In_err_rec_param,
                                              In_read_retry_count,
                                              True);
          End;

          Alloc_size:=SizeOf(T_mode_param_6_read_err_rec_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB6(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_select10_read_err_rec(In_err_rec_param    : Byte;
                                                   In_read_retry_count : Byte);
{ ********************************************************************
  Sends read err recovery parameters using mode select CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ******************************************************************** }
Var Alloc_size : Byte;
    Block_desc_len : Word;
    P_read_err_rec_param_page : Pointer;
Begin
     Do_sense10_read_err_rec;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          if (T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1 and 1)=0 then
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 8 * 8;
          end
          else
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 16 * 16;
          end;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_10_read_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_read_err_rec(@Read_err_rec,
                                                   Read_err_rec.Param_len,
                                                   In_err_rec_param,
                                                   In_read_retry_count,
                                                   True);
                    Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Mode_data_len_MSB,
                                                                    Hdr.Mode_data_len_LSB)+2;
                    //Alloc_size:=SizeOf(T_mode_param_10_read_err_rec_noblk_data);
               End;
          End
          Else
          Begin
               P_read_err_rec_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_read_err_rec_param_page), SizeOf(T_mode_param_hdr_10) + Block_desc_len);
               MMC1.Fill_mode_pg_read_err_rec(P_read_err_rec_param_page,
                                              T_mode_pg_read_err_rec(P_read_err_rec_param_page^).Param_len,
                                              In_err_rec_param,
                                              In_read_retry_count,
                                              True);
               Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_MSB,
                                                               T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_LSB)+2;
               //Alloc_size:=SizeOf(T_mode_param_10_read_err_rec_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_10_read_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_read_err_rec(@Read_err_rec,
                                              6,
                                              In_err_rec_param,
                                              In_read_retry_count,
                                              True);
          End;

          Alloc_size:=SizeOf(T_mode_param_10_read_err_rec_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB10(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_sense6_read_err_rec;
{ ***********************************************************************
  Retrieve read err recovery parameters using mode sense CDB6 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  *********************************************************************** }
Var
   Return_cap : Byte;
Begin
     Return_cap:=Calc_sense6_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_READ_ERR_REC);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB6(Get_modesense6_DBD_last_used,
                                       MMC_MODE_PAGE_READ_ERR_REC,
                                       Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense10_read_err_rec;
{ ************************************************************************
  Retrieve read err recovery parameters using mode sense CDB10 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ************************************************************************ }
Var
   Return_cap : Word;
Begin
     Return_cap:=Calc_sense10_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_READ_ERR_REC);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB10(Get_modesense10_DBD_last_used,
                                        MMC_MODE_PAGE_READ_ERR_REC,
                                        Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_select6_write(Var In_write_params : T_pub_write_params);
{ *******************************************************
  Sends write parameters using mode select CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ******************************************************* }
Var Alloc_size : Byte;
    Block_desc_len : Byte;
    P_write_param_page : Pointer;
Begin
     Do_sense6_write;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Block_desc_len:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len
                          div 8 * 8;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_6_write_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_write(@Write,
                                            Write.Param_len,
                                            In_write_params,
                                            True);
                    Alloc_size:=Hdr.Mode_data_len+1;
                    //Alloc_size:=SizeOf(T_mode_param_6_write_noblk_data);
               End;
          End
          Else
          Begin
               P_write_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_write_param_page), SizeOf(T_mode_param_hdr_6) + Block_desc_len);
               MMC1.Fill_mode_pg_write(P_write_param_page,
                                       T_mode_pg_write(P_write_param_page^).Param_len,
                                       In_write_params,
                                       True);
               Alloc_size:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len+1;
               //Alloc_size:=SizeOf(T_mode_param_6_write_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_6_write_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_write(@Write,
                                       32,
                                       In_write_params,
                                       True);
          End;

          Alloc_size:=SizeOf(T_mode_param_6_write_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB6(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_select10_write(Var In_write_params : T_pub_write_params);
{ *******************************************************
  Sends write parameters using mode select CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ******************************************************* }
Var Alloc_size : Byte;
    Block_desc_len : Word;
    P_write_param_page : Pointer;
Begin
     Do_sense10_write;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          if (T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1 and 1)=0 then
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 8 * 8;
          end
          else
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 16 * 16;
          end;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_10_write_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_write(@Write,
                                            Write.Param_len,
                                            In_write_params,
                                            True);
                    Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Mode_data_len_MSB,
                                                                    Hdr.Mode_data_len_LSB)+2;
                    //Alloc_size:=SizeOf(T_mode_param_10_write_noblk_data);
               End;
          End
          Else
          Begin
               P_write_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_write_param_page), SizeOf(T_mode_param_hdr_10) + Block_desc_len);
               MMC1.Fill_mode_pg_write(P_write_param_page,
                                       T_mode_pg_write(P_write_param_page^).Param_len,
                                       In_write_params,
                                       True);
               Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_MSB,
                                                               T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_LSB)+2;
               //Alloc_size:=SizeOf(T_mode_param_10_write_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_10_write_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_write(@Write,
                                       32,
                                       In_write_params,
                                       True);
          End;

          Alloc_size:=SizeOf(T_mode_param_10_write_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB10(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_sense6_write;
{ ***********************************************************
  Retrieve write parameters using mode sense CDB6 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  *********************************************************** }
Var
   Return_cap : Byte;
Begin
     Return_cap:=Calc_sense6_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_WRITE_PARAM);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB6(Get_modesense6_DBD_last_used,
                                       MMC_MODE_PAGE_WRITE_PARAM,
                                       Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense10_write;
{ ************************************************************
  Retrieve write parameters using mode sense CDB10 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ************************************************************ }
Var
   Return_cap : Word;
Begin
     Return_cap:=Calc_sense10_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_WRITE_PARAM);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB10(Get_modesense10_DBD_last_used,
                                        MMC_MODE_PAGE_WRITE_PARAM,
                                        Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_select6_veri_err_rec(In_err_rec_param    : Byte;
                                                 In_veri_retry_count : Byte);
{ *********************************************************************
  Sends verify err recovery parameters using mode select CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ********************************************************************* }
Var Alloc_size : Byte;
    Block_desc_len : Byte;
    P_veri_err_rec_param_page : Pointer;
Begin
     Do_sense6_veri_err_rec;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Block_desc_len:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len
                          div 8 * 8;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_6_veri_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_veri_err_rec(@Veri_err_rec,
                                                   Veri_err_rec.Param_len,
                                                   In_err_rec_param,
                                                   In_veri_retry_count,
                                                   True);
                    Alloc_size:=Hdr.Mode_data_len+1;
                    //Alloc_size:=SizeOf(T_mode_param_6_veri_err_rec_noblk_data);
               End;
          End
          Else
          Begin
               P_veri_err_rec_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_veri_err_rec_param_page), SizeOf(T_mode_param_hdr_6) + Block_desc_len);
               MMC1.Fill_mode_pg_veri_err_rec(P_veri_err_rec_param_page,
                                              T_mode_pg_veri_err_rec(P_veri_err_rec_param_page^).Param_len,
                                              In_err_rec_param,
                                              In_veri_retry_count,
                                              True);
               Alloc_size:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len+1;
               //Alloc_size:=SizeOf(T_mode_param_6_veri_err_rec_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_6_veri_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_veri_err_rec(@Veri_err_rec,
                                              6,
                                              In_err_rec_param,
                                              In_veri_retry_count,
                                              True);
          End;

          Alloc_size:=SizeOf(T_mode_param_6_veri_err_rec_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB6(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_select10_veri_err_rec(In_err_rec_param    : Byte;
                                                   In_veri_retry_count : Byte);
{ **********************************************************************
  Sends verify err recovery parameters using mode select CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ********************************************************************** }
Var Alloc_size : Byte;
    Block_desc_len : Word;
    P_veri_err_rec_param_page : Pointer;
Begin
     Do_sense10_veri_err_rec;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          if (T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1 and 1)=0 then
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 8 * 8;
          end
          else
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 16 * 16;
          end;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_10_veri_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_veri_err_rec(@Veri_err_rec,
                                                   Veri_err_rec.Param_len,
                                                   In_err_rec_param,
                                                   In_veri_retry_count,
                                                   True);
                    Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Mode_data_len_MSB,
                                                                    Hdr.Mode_data_len_LSB)+2;
                    //Alloc_size:=SizeOf(T_mode_param_10_veri_err_rec_noblk_data);
               End;
          End
          Else
          Begin
               P_veri_err_rec_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_veri_err_rec_param_page), SizeOf(T_mode_param_hdr_10) + Block_desc_len);
               MMC1.Fill_mode_pg_veri_err_rec(P_veri_err_rec_param_page,
                                              T_mode_pg_veri_err_rec(P_veri_err_rec_param_page^).Param_len,
                                              In_err_rec_param,
                                              In_veri_retry_count,
                                              True);
               Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_MSB,
                                                               T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_LSB)+2;
               //Alloc_size:=SizeOf(T_mode_param_10_veri_err_rec_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_10_veri_err_rec_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_veri_err_rec(@Veri_err_rec,
                                              6,
                                              In_err_rec_param,
                                              In_veri_retry_count,
                                              True);
          End;

          Alloc_size:=SizeOf(T_mode_param_10_veri_err_rec_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB10(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_sense6_veri_err_rec;
{ *************************************************************************
  Retrieve verify err recovery parameters using mode sense CDB6 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ************************************************************************* }
Var
   Return_cap : Byte;
Begin
     Return_cap:=Calc_sense6_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_VERI_ERR_REC);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB6(Get_modesense6_DBD_last_used,
                                       MMC_MODE_PAGE_VERI_ERR_REC,
                                       Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense10_veri_err_rec;
{ **************************************************************************
  Retrieve verify err recovery parameters using mode sense CDB10 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ************************************************************************** }
Var
   Return_cap : Word;
Begin
     Return_cap:=Calc_sense10_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_VERI_ERR_REC);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB10(Get_modesense10_DBD_last_used,
                                        MMC_MODE_PAGE_VERI_ERR_REC,
                                        Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_select6_CD(In_inactivity_mul : Byte;
                                        In_sec_per_MSF    : Word;
                                        In_frames_per_MSF : Word);
{ ******************************************************
  Sends CD parameters using mode select CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ****************************************************** }
Var Alloc_size : Byte;
    Block_desc_len : Byte;
    P_CD_param_page : Pointer;
Begin
     Do_sense6_CD;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Block_desc_len:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len
                          div 8 * 8;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_6_CD_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_CD(@CD,
                                         CD.Param_len,
                                         In_inactivity_mul,
                                         In_sec_per_MSF,
                                         In_frames_per_MSF,
                                         True);
                    Alloc_size:=Hdr.Mode_data_len+1;
                    //Alloc_size:=SizeOf(T_mode_param_6_CD_noblk_data);
               End;
          End
          Else
          Begin
               P_CD_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_CD_param_page), SizeOf(T_mode_param_hdr_6) + Block_desc_len);
               MMC1.Fill_mode_pg_CD(P_CD_param_page,
                                    T_mode_pg_CD(P_CD_param_page^).Param_len,
                                    In_inactivity_mul,
                                    In_sec_per_MSF,
                                    In_frames_per_MSF,
                                    True);
               Alloc_size:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len+1;
               //Alloc_size:=SizeOf(T_mode_param_6_CD_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_6_CD_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_CD(@CD,
                                    6,
                                    In_inactivity_mul,
                                    In_sec_per_MSF,
                                    In_frames_per_MSF,
                                    True);
          End;

          Alloc_size:=SizeOf(T_mode_param_6_CD_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB6(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_select10_CD(In_inactivity_mul : Byte;
                                         In_sec_per_MSF    : Word;
                                         In_frames_per_MSF : Word);
{ ******************************************************
  Sends CD parameters using mode select CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ****************************************************** }
Var Alloc_size : Byte;
    Block_desc_len : Word;
    P_CD_param_page : Pointer;
Begin
     Do_sense10_CD;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          if (T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1 and 1)=0 then
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 8 * 8;
          end
          else
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 16 * 16;
          end;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_10_CD_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_CD(@CD,
                                         CD.Param_len,
                                         In_inactivity_mul,
                                         In_sec_per_MSF,
                                         In_frames_per_MSF,
                                         True);
                    Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Mode_data_len_MSB,
                                                                    Hdr.Mode_data_len_LSB)+2;
                    //Alloc_size:=SizeOf(T_mode_param_10_CD_noblk_data);
               End;
          End
          Else
          Begin
               P_CD_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_CD_param_page), SizeOf(T_mode_param_hdr_10) + Block_desc_len);
               MMC1.Fill_mode_pg_CD(P_CD_param_page,
                                    T_mode_pg_CD(P_CD_param_page^).Param_len,
                                    In_inactivity_mul,
                                    In_sec_per_MSF,
                                    In_frames_per_MSF,
                                    True);
               Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_MSB,
                                                               T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_LSB)+2;
               //Alloc_size:=SizeOf(T_mode_param_10_CD_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_10_CD_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_CD(@CD,
                                    6,
                                    In_inactivity_mul,
                                    In_sec_per_MSF,
                                    In_frames_per_MSF,
                                    True);
          End;

          Alloc_size:=SizeOf(T_mode_param_10_CD_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB10(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_sense6_CD;
{ ********************************************************
  Retrieve CD parameters using mode sense CDB6 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ******************************************************** }
Var
   Return_cap : Byte;
Begin
     Return_cap:=Calc_sense6_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_CD);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB6(Get_modesense6_DBD_last_used,
                                       MMC_MODE_PAGE_CD,
                                       Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense10_CD;
{ *********************************************************
  Retrieve CD parameters using mode sense CDB10 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ********************************************************* }
Var
   Return_cap : Word;
Begin
     Return_cap:=Calc_sense10_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_CD);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB10(Get_modesense10_DBD_last_used,
                                        MMC_MODE_PAGE_CD,
                                        Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_select6_CD_audio_ctrl(Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl);
{ ******************************************************************
  Sends CD audio control parameters using mode select CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ****************************************************************** }
Var Alloc_size : Byte;
    Block_desc_len : Byte;
    P_CD_audio_ctrl_param_page : Pointer;
Begin
     Do_sense6_CD_audio_ctrl;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Block_desc_len:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len
                          div 8 * 8;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_6_CD_audio_ctrl_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_CD_audio_ctrl(@CD_audio_ctrl,
                                                    CD_audio_ctrl.Param_len,
                                                    In_CD_audio_ctrl,
                                                    True);
                    Alloc_size:=Hdr.Mode_data_len+1;
                    //Alloc_size:=SizeOf(T_mode_param_6_CD_audio_ctrl_noblk_data);
               End;
          End
          Else
          Begin
               P_CD_audio_ctrl_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_CD_audio_ctrl_param_page), SizeOf(T_mode_param_hdr_6) + Block_desc_len);
               MMC1.Fill_mode_pg_CD_audio_ctrl(P_CD_audio_ctrl_param_page,
                                               T_mode_pg_CD_audio_ctrl(P_CD_audio_ctrl_param_page^).Param_len,
                                               In_CD_audio_ctrl,
                                               True);
               Alloc_size:=T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len+1;
               //Alloc_size:=SizeOf(T_mode_param_6_CD_audio_ctrl_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_6_CD_audio_ctrl_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_CD_audio_ctrl(@CD_audio_ctrl,
                                               14,
                                               In_CD_audio_ctrl,
                                               True);
          End;

          Alloc_size:=SizeOf(T_mode_param_6_CD_audio_ctrl_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB6(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_select10_CD_audio_ctrl(Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl);
{ ****************************************************************
  Sends CD audio ctrl parameters using mode select CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.
  **************************************************************** }
Var Alloc_size : Byte;
    Block_desc_len : Word;
    P_CD_audio_ctrl_param_page : Pointer;
Begin
     Do_sense10_CD_audio_ctrl;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          if (T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1 and 1)=0 then
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 8 * 8;
          end
          else
          begin
               Block_desc_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                   T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)
                               div 16 * 16;
          end;
          If Block_desc_len=0 Then
          Begin
               With T_mode_param_10_CD_audio_ctrl_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
               Begin
                    MMC1.Fill_mode_pg_CD_audio_ctrl(@CD_audio_ctrl,
                                                    CD_audio_ctrl.Param_len,
                                                    In_CD_audio_ctrl,
                                                    True);
                    Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Mode_data_len_MSB,
                                                                    Hdr.Mode_data_len_LSB)+2;
                    //Alloc_size:=SizeOf(T_mode_param_10_CD_audio_ctrl_noblk_data);
               End;
          End
          Else
          Begin
               P_CD_audio_ctrl_param_page:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(P_byte(P_CD_audio_ctrl_param_page), SizeOf(T_mode_param_hdr_10) + Block_desc_len);
               MMC1.Fill_mode_pg_CD_audio_ctrl(P_CD_audio_ctrl_param_page,
                                               T_mode_pg_CD_audio_ctrl(P_CD_audio_ctrl_param_page^).Param_len,
                                               In_CD_audio_ctrl,
                                               True);
               Alloc_size:=SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_MSB,
                                                               T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Mode_data_len_LSB)+2;
               //Alloc_size:=SizeOf(T_mode_param_10_CD_audio_ctrl_noblk_data)+Block_desc_len;
          End;
     End
     Else
     Begin
          MMC1.Fill_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf);
          With T_mode_param_10_CD_audio_ctrl_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //Fill_block_desc(@Block_desc);
               MMC1.Fill_mode_pg_CD_audio_ctrl(@CD_audio_ctrl,
                                               14,
                                               In_CD_audio_ctrl,
                                               True);
          End;

          Alloc_size:=SizeOf(T_mode_param_10_CD_audio_ctrl_noblk_data);
     End;

     SPC_WNASPI32.Mode_select_CDB10(Alloc_size, False);
End;

Procedure T_MMC1_WNASPI32.Do_sense6_CD_audio_ctrl;
{ *******************************************************************
  Retrieve CD audio ctrl parameters using mode sense CDB6 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ******************************************************************* }
Var
   Return_cap : Byte;
Begin
     Return_cap:=Calc_sense6_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_CD_AUDIO_CTRL);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB6(Get_modesense6_DBD_last_used,
                                       MMC_MODE_PAGE_CD_AUDIO_CTRL,
                                       Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense10_CD_audio_ctrl;
{ ********************************************************************
  Retrieve CD audio ctrl parameters using mode sense CDB10 from target
  device.  The T_GenSCSI will make the call to WNASPI32.
  ******************************************************************** }
Var
   Return_cap : Word;
Begin
     Return_cap:=Calc_sense10_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_CD_AUDIO_CTRL);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          SPC_WNASPI32.Mode_sense_CDB10(Get_modesense10_DBD_last_used,
                                        MMC_MODE_PAGE_CD_AUDIO_CTRL,
                                        Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense6_CD_cap_mech_st_sub6;
{ ******************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB6 from target
  device.  The T_GenSCSI will make the call to WNASPI32.

  This version is for very old CDROM drives.  Most CDROM drives do not support returning the
  last 6 bytes.  These bytes are useful only for rewriters and are returned by all
  rewriters.

  This method makes the call with 6 subtracted from parameter list len.
  ****************************************************************************************** }
Begin
     { Retrieve the full CD capabilities and mechanism status parameter list. }
     SPC_WNASPI32.Mode_sense_CDB6(MODE_SENSE_DBD_BLK_DESC,
                                  MMC_MODE_PAGE_CD_CAP_MECH_ST,
                                  20); //Normally should be 26
     If Not SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          { Retrieve the full CD capabilities and mechanism status parameter list. }
          SPC_WNASPI32.Mode_sense_CDB6(MODE_SENSE_DBD_NO_BLK_DESC,
                                       MMC_MODE_PAGE_CD_CAP_MECH_ST,
                                       20); //Normally should be 26
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense10_CD_cap_mech_st_sub6;
{ ******************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.

  This version is for very old CDROM drives.  Most CDROM drives does not support returning
  the last 6 bytes.  These bytes are useful only for rewriters and are returned by all
  rewriters.

  This method makes the call with 6 subtracted from parameter list len.
  ****************************************************************************************** }
Begin
     { Retrieve the full CD capabilities and mechanism status parameter list. }
     SPC_WNASPI32.Mode_sense_CDB10(MODE_SENSE_DBD_BLK_DESC,
                                   MMC_MODE_PAGE_CD_CAP_MECH_ST,
                                   24); //Normally should be 30
     If Not SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          { Retrieve the full CD capabilities and mechanism status parameter list. }
          SPC_WNASPI32.Mode_sense_CDB10(MODE_SENSE_DBD_NO_BLK_DESC,
                                        MMC_MODE_PAGE_CD_CAP_MECH_ST,
                                        24); //Normally should be 30
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense6_CD_cap_mech_st;
{ ****************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.

  This version should work fine for all CDROM, CD rewriter drives.  Although some drives
  does not support sense6, they may support sense10, or vice versa.
  This method calls another method to calculate the return capacity of how much data the
  drive can return.  Then use this to retrieve the full parameter list.
  **************************************************************************************** }
Var
   Return_cap : Byte;
Begin
     Return_cap:=Calc_sense6_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_CD_CAP_MECH_ST);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          { Retrieve the full CD capabilities and mechanism status parameter list. }
          SPC_WNASPI32.Mode_sense_CDB6(Get_modesense6_DBD_last_used,
                                       MMC_MODE_PAGE_CD_CAP_MECH_ST,
                                       Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_sense10_CD_cap_mech_st;
{ *****************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.

  This version should work fine for all CDROM, CD writer drives.  Although some drives does
  not support sense10, they may support sense6, or vice versa.
  This method calls another method to calculate the return capacity of how much data the
  drive can return.  Then use this to retrieve the full parameter list.
  ***************************************************************************************** }
Var
   Return_cap : Word;
Begin
     Return_cap:=Calc_sense10_return_cap_from_mode_data_and_param_len(MMC_MODE_PAGE_CD_CAP_MECH_ST);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          { Retrieve the full CD capabilities and mechanism status parameter list. }
          SPC_WNASPI32.Mode_sense_CDB10(Get_modesense10_DBD_last_used,
                                        MMC_MODE_PAGE_CD_CAP_MECH_ST,
                                        Return_cap);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_read_rec_cap_out_CDB10(Var LastMMCLBA     : LongInt;
                                                    Var MMCLBABlockLen : LongWord);
{ ********************************************************
  Sends a read recorded capacity command to target device
  and return variables (friendly version).
  ******************************************************** }
Begin
     Do_read_rec_cap_CDB10;

     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_readCD_rec_cap_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               LastMMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(MMCLBA_HiByte,
                                                                  MMCLBA_HiMiByte,
                                                                  MMCLBA_LoMiByte,
                                                                  MMCLBA_LoByte);
               MMCLBABlockLen:=SPC_WNASPI32.SPC.ReverseBytesToLongWord(Len_HiByte,
                                                                       Len_HiMiByte,
                                                                       Len_LoMiByte,
                                                                       Len_LoByte);
          End;
     End
     Else
         Begin
              LastMMCLBA:=0;
              MMCLBABlockLen:=0;
         End;
End;

Procedure T_MMC1_WNASPI32.Do_read_disc_info_out_CDB10(Var Out_read_disc_info_CDB10_block : T_out_read_disc_info_CDB10_block);
Begin
     Do_read_disc_info_CDB10;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Out_read_disc_info_CDB10_block.DataLen:=SPC_WNASPI32.SPC.ReverseBytesToWord(
                                                  T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DataLen_HiByte,
                                                  T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DataLen_LoByte);
          Out_read_disc_info_CDB10_block.Erasable_StateOfLastSess_DiscStatus:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Erasable_StateOfLastSess_DiscStatus;
          Out_read_disc_info_CDB10_block.No_of_first_trk:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_first_trk;
          Out_read_disc_info_CDB10_block.No_of_sess:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_sess;
          Out_read_disc_info_CDB10_block.First_trk_no_in_last_sess:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).First_trk_no_in_last_sess;
          Out_read_disc_info_CDB10_block.Last_trk_no_in_last_sess:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_trk_no_in_last_sess;
          Out_read_disc_info_CDB10_block.DID_V_DBC_V_URU:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DID_V_DBC_V_URU;
          Out_read_disc_info_CDB10_block.DiscType:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscType;
          Out_read_disc_info_CDB10_block.Reserved1:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1;
          Out_read_disc_info_CDB10_block.Reserved2:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved2;
          Out_read_disc_info_CDB10_block.Reserved3:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved3;
          Out_read_disc_info_CDB10_block.DiscID:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_HiByte,
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_HiMiByte,
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_LoMiByte,
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_LoByte);
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.Reserved:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_HiByte;
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.M:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_HiMiByte;
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.S:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_LoMiByte;
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.F:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_LoByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.Reserved:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_HiByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.M:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_HiMiByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.S:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_LoMiByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.F:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_LoByte;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte0:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte7;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte1:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte6;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte2:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte5;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte3:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte4;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte4:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte3;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte5:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte2;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte6:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte1;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte7:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte0;
          Out_read_disc_info_CDB10_block.Reserved4:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved4;
          Out_read_disc_info_CDB10_block.No_of_OPC_tab_entries:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_OPC_tab_entries;
     End;
End;

Procedure T_MMC1_WNASPI32.Do_read_disc_info_with_OPC_out_CDB10(Var Out_read_disc_info_CDB10_block : T_out_read_disc_info_CDB10_block);
Begin
     Do_read_disc_info_with_OPC_CDB10;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Out_read_disc_info_CDB10_block.DataLen:=SPC_WNASPI32.SPC.ReverseBytesToWord(
                                                  T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DataLen_HiByte,
                                                  T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DataLen_LoByte);
          Out_read_disc_info_CDB10_block.Erasable_StateOfLastSess_DiscStatus:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Erasable_StateOfLastSess_DiscStatus;
          Out_read_disc_info_CDB10_block.No_of_first_trk:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_first_trk;
          Out_read_disc_info_CDB10_block.No_of_sess:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_sess;
          Out_read_disc_info_CDB10_block.First_trk_no_in_last_sess:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).First_trk_no_in_last_sess;
          Out_read_disc_info_CDB10_block.Last_trk_no_in_last_sess:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_trk_no_in_last_sess;
          Out_read_disc_info_CDB10_block.DID_V_DBC_V_URU:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DID_V_DBC_V_URU;
          Out_read_disc_info_CDB10_block.DiscType:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscType;
          Out_read_disc_info_CDB10_block.Reserved1:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved1;
          Out_read_disc_info_CDB10_block.Reserved2:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved2;
          Out_read_disc_info_CDB10_block.Reserved3:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved3;
          Out_read_disc_info_CDB10_block.DiscID:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_HiByte,
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_HiMiByte,
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_LoMiByte,
                                                 T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscID_LoByte);
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.Reserved:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_HiByte;
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.M:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_HiMiByte;
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.S:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_LoMiByte;
          Out_read_disc_info_CDB10_block.Last_sess_lead_in_start_MSF.F:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_sess_lead_in_start_MSF_LoByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.Reserved:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_HiByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.M:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_HiMiByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.S:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_LoMiByte;
          Out_read_disc_info_CDB10_block.Last_start_for_lead_out_MSF.F:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Last_start_for_lead_out_MSF_LoByte;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte0:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte7;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte1:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte6;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte2:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte5;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte3:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte4;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte4:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte3;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte5:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte2;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte6:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte1;
          Out_read_disc_info_CDB10_block.DiscBarCode_Byte7:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DiscBarCode_Byte0;
          Out_read_disc_info_CDB10_block.Reserved4:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved4;
          Out_read_disc_info_CDB10_block.No_of_OPC_tab_entries:=T_read_disc_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).No_of_OPC_tab_entries;
     End;
End;

Procedure T_MMC1_WNASPI32.Do_read_track_info_out_CDB10(    In_TrkBIT       : Byte;
                                                           In_MMCLBA_TrkNo : LongInt;
                                                       Var Out_read_track_info_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     Do_read_track_info_CDB10(In_TrkBIT, In_MMCLBA_TrkNo);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          Out_read_track_info_CDB10.DataLen:=SPC_WNASPI32.SPC.ReverseBytesToWord(
                                             T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DataLen_HiByte,
                                             T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).DataLen_LoByte);
          Out_read_track_info_CDB10.TrkNo:=T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkNo;
          Out_read_track_info_CDB10.SessNo:=T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).SessNo;
          Out_read_track_info_CDB10.Reserved:=T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Reserved;
          Out_read_track_info_CDB10.Damage_Copy_TrkMode:=T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).Damage_Copy_TrkMode;
          Out_read_track_info_CDB10.RT_Blank_Packet_FP_DataMode:=T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).RT_Blank_Packet_FP_DataMode;
          Out_read_track_info_CDB10.NWA_V:=T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).NWA_V;
          Out_read_track_info_CDB10.TrkStart_MMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(
                                                     T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkStart_MMCLBA_HiByte,
                                                     T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkStart_MMCLBA_HiMiByte,
                                                     T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkStart_MMCLBA_LoMiByte,
                                                     T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkStart_MMCLBA_LoByte);
          Out_read_track_info_CDB10.NextWr_MMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(
                                                   T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).NextWr_MMCLBA_HiByte,
                                                   T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).NextWr_MMCLBA_HiMiByte,
                                                   T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).NextWr_MMCLBA_LoMiByte,
                                                   T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).NextWr_MMCLBA_LoByte);
          Out_read_track_info_CDB10.FreeBlocks:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(
                                                T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FreeBlocks_HiByte,
                                                T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FreeBlocks_HiMiByte,
                                                T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FreeBlocks_LoMiByte,
                                                T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FreeBlocks_LoByte);
          Out_read_track_info_CDB10.FixPackSize:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(
                                                 T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FixPackSize_HiByte,
                                                 T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FixPackSize_HiMiByte,
                                                 T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FixPackSize_LoMiByte,
                                                 T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).FixPackSize_LoByte);
          Out_read_track_info_CDB10.TrkSize:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(
                                             T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkSize_HiByte,
                                             T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkSize_HiMiByte,
                                             T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkSize_LoMiByte,
                                             T_read_track_info_CDB10_block(SPC_WNASPI32.WNASPI32.Get_data_buf^).TrkSize_LoByte);
     End;
End;

Procedure T_MMC1_WNASPI32.Do_read_subch_aud_st_out_CDB10(Var Out_audioStatus : Byte);
{ *************************************************
  Sends a read sub-channel command to target device
  and return audio status only (friendly version).
  ************************************************* }
Begin
     Do_read_subch_CDB10(MMC_READ_SUBCH_MMCLBA, 0, 0, 0);

     Out_audioStatus:=T_read_subch_data_hdr(SPC_WNASPI32.WNASPI32.P_SRB^).Audio_status;
End;

Procedure T_MMC1_WNASPI32.Do_read_subch_curr_MMCLBA_CDB10(Var Out_abs_MMCLBA : LongInt;
                                                          Var Out_rel_MMCLBA : LongInt);
{ **************************************************************
  Sends a read sub-channel command to target device
  and return current position data in MMCLBA (friendly version).
  ************************************************************** }
Begin
     Do_read_subch_CDB10(MMC_READ_SUBCH_MMCLBA,
                         MMC_READ_SUBCH_SUBQ,
                         MMC_READ_SUBCH_CURR_POS,
                         0);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_read_subch_CD_curr_pos_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               Out_abs_MMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(AbsAdr_HiByte,
                                                                      AbsAdr_HiMiByte,
                                                                      AbsAdr_LoMiByte,
                                                                      AbsAdr_LoByte);
               Out_rel_MMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(TrkRelAdr_HiByte,
                                                                      TrkRelAdr_HiMiByte,
                                                                      TrkRelAdr_LoMiByte,
                                                                      TrkRelAdr_LoByte);
          End;
     End;
End;

Procedure T_MMC1_WNASPI32.Do_read_subch_curr_MSF_CDB10(Var Out_abs_MSF : T_MSF;
                                                       Var Out_rel_MSF : T_MSF);
{ ***********************************************************
  Sends a read sub-channel command to target device
  and return current position data in MSF (friendly version).
  *********************************************************** }
Begin
     Do_read_subch_CDB10(MMC_READ_SUBCH_MSF,
                         MMC_READ_SUBCH_SUBQ,
                         MMC_READ_SUBCH_CURR_POS,
                         0);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_read_subch_CD_curr_pos_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               Out_abs_MSF.M:=AbsAdr_HiMiByte;
               Out_abs_MSF.S:=AbsAdr_LoMiByte;
               Out_abs_MSF.F:=AbsAdr_LoByte;
               Out_rel_MSF.M:=TrkRelAdr_HiMiByte;
               Out_abs_MSF.S:=TrkRelAdr_LoMiByte;
               Out_abs_MSF.F:=TrkRelAdr_LoByte;
          End;
     End;
End;

Procedure T_MMC1_WNASPI32.Do_read_subch_MCN_out_CDB10(Var Out_MC_valid : Boolean;
                                                      Var Out_MCN_str  : String);
{ *************************************************
  Sends a read sub-channel command to target device
  and return MCN data, which are placed into a
  string (friendly version).
  ************************************************* }
Var i         : Byte;
    ASCII_num : Char;
Begin
     Do_read_subch_CDB10(MMC_READ_SUBCH_MMCLBA,
                         MMC_READ_SUBCH_SUBQ,
                         MMC_READ_SUBCH_MCN,
                         0);

     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          If (T_read_subch_MCN_data(SPC_WNASPI32.WNASPI32.Get_data_buf^).MCN.Valid AND 128)<>0 Then
              Out_MC_valid:=True
          Else
              Out_MC_valid:=False;

          { MCN values are single BCD, but MMC will return them as ASCII numbers.
            These are first verified.. }
          Out_MCN_str:='';
          For i:=1 To 13 Do
          Begin
               Case T_read_subch_MCN_data(SPC_WNASPI32.WNASPI32.Get_data_buf^).MCN.N[i] Of
                    '0'..'9': ASCII_num:=T_read_subch_MCN_data(SPC_WNASPI32.WNASPI32.Get_data_buf^).MCN.N[i];
               Else
                    ASCII_num:='?'; //If non-ASCII numbers, error, then display '?'.
               End;
               Out_MCN_str:=Out_MCN_str+ASCII_num;
          End;
     End
     Else
         Out_MC_valid:=False;
End;

Procedure T_MMC1_WNASPI32.Do_read_subch_ISRC_out_CDB10(    In_TrackNo   : Byte;
                                                       Var Out_TC_valid : Boolean;
                                                       Var Out_ISRC_str : String);
{ ***************************************************
  Sends a read sub-channel command to target device
  and return ISRC data, which are placed into a
  string (friendly version).
  *************************************************** }
Var
   i : Byte;
Begin
     Do_read_subch_CDB10(MMC_READ_SUBCH_MMCLBA,
                         MMC_READ_SUBCH_SUBQ,
                         MMC_READ_SUBCH_ISRC,
                         In_TrackNo);
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          If (T_read_subch_ISRC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^).ISRC.Valid  AND 128)<>0 Then
              Out_TC_valid:=False
          Else
              Out_TC_valid:=True;

          { ISRC ASCII chars are placed into a string for display.. }
          Out_ISRC_str:='';
          With T_read_subch_ISRC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               For i:=1 To 12 Do
               Begin
                    Out_ISRC_str:=Out_ISRC_str+T_char_array12((@(ISRC.Country))^)[i];
               End;
          End;
     End
     Else
         Out_TC_valid:=False;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_TOC_MMCLBA_first_out_CDB10(Var Out_first_trkNo : Byte;
                                                                  Var Out_last_trkNo  : Byte;
                                                                  Var Out_TOC_desc    : T_out_read_T_P_A_TOC_desc_MMCLBA;
                                                                      Track_no        : Byte) : Boolean;
Var TOC_data_len      : Word;
    There_is_TOC_data : Boolean;
Begin
     Do_read_T_P_A_CDB10(MMC_READ_T_P_A_MMCLBA,
                         MMC_READ_T_P_A_FORMAT_TOC,
                         Track_no);

     There_is_TOC_data:=False;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK then
     Begin
          With T_read_T_P_A_TOC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               TOC_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                 Hdr.Data_len_LoByte);

               If (TOC_data_len-2)>=SizeOf(T_read_T_P_A_TOC_trk_desc_data) Then
               Begin
                    Out_first_trkNo:=Hdr.First_trk_sess;
                    Out_last_trkNo:=Hdr.Last_trk_sess;
                    Out_TOC_desc.ADR_Ctrl:=TOC_trk_desc.ADR_Control;
                    Out_TOC_desc.TrackNo:=TOC_trk_desc.TrackNo;
                    Out_TOC_desc.StartMMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(TOC_trk_desc.Adr_HiByte,
                                                                                     TOC_trk_desc.Adr_HiMiByte,
                                                                                     TOC_trk_desc.Adr_LoMiByte,
                                                                                     TOC_trk_desc.Adr_LoByte);
                    There_is_TOC_data:=True;
                    SPC_WNASPI32.WNASPI32.Desc_data_offset:=SizeOf(T_read_T_P_A_TOC_data);
               End;
          End;
     End;

     Result:=There_is_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_TOC_MMCLBA_next_out_CDB10(Var Out_TOC_desc : T_out_read_T_P_A_TOC_desc_MMCLBA) : Boolean;
Var TOC_data_len       : Word;
    There_is_TOC_data  : Boolean;
    P_desc_data_offset : Pointer;
Begin
     There_is_TOC_data:=False;
     If (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Cmd=MMC_CMD_READ_TOC_PMA_ATIP) AND
        (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Format=MMC_READ_T_P_A_FORMAT_TOC) AND
        (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.MSF=MMC_READ_T_P_A_MMCLBA) AND
        SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_read_T_P_A_TOC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               TOC_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                 Hdr.Data_len_LoByte);
          End;
          If (TOC_data_len+2)>=(SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_TOC_trk_desc_data)) Then
          Begin
               P_desc_data_offset:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(LongWord(P_desc_data_offset), SPC_WNASPI32.WNASPI32.Desc_data_offset);

               With T_read_T_P_A_TOC_trk_desc_data(P_desc_data_offset^) Do
               Begin
                    Out_TOC_desc.ADR_Ctrl:=ADR_Control;
                    Out_TOC_desc.TrackNo:=TrackNo;
                    Out_TOC_desc.StartMMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(Adr_HiByte,
                                                                                     Adr_HiMiByte,
                                                                                     Adr_LoMiByte,
                                                                                     Adr_LoByte);
               End;
               There_is_TOC_data:=True;
               SPC_WNASPI32.WNASPI32.Desc_data_offset:=SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_TOC_trk_desc_data);
          End;
     End;

     Result:=There_is_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_TOC_MSF_first_out_CDB10(Var Out_first_trkNo : Byte;
                                                               Var Out_last_trkNo  : Byte;
                                                               Var Out_TOC_desc    : T_out_read_T_P_A_TOC_desc_MSF;
                                                                   Track_no        : Byte) : Boolean;
Var TOC_data_len      : Word;
    There_is_TOC_data : Boolean;
Begin
     Do_read_T_P_A_CDB10(MMC_READ_T_P_A_MSF,
                         MMC_READ_T_P_A_FORMAT_TOC,
                         Track_no);

     There_is_TOC_data:=False;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK then
     Begin
          With T_read_T_P_A_TOC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               TOC_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                 Hdr.Data_len_LoByte);

               If (TOC_data_len-2)>=SizeOf(T_read_T_P_A_TOC_trk_desc_data) Then
               Begin
                    Out_first_trkNo:=Hdr.First_trk_sess;
                    Out_last_trkNo:=Hdr.Last_trk_sess;
                    Out_TOC_desc.ADR_Ctrl:=TOC_trk_desc.ADR_Control;
                    Out_TOC_desc.TrackNo:=TOC_trk_desc.TrackNo;
                    Out_TOC_desc.StartM:=TOC_trk_desc.Adr_HiMiByte;
                    Out_TOC_desc.StartS:=TOC_trk_desc.Adr_LoMiByte;
                    Out_TOC_desc.StartF:=TOC_trk_desc.Adr_LoByte;

                    There_is_TOC_data:=True;
                    SPC_WNASPI32.WNASPI32.Desc_data_offset:=SizeOf(T_read_T_P_A_TOC_data);
               End;
          End;
     End;

     Result:=There_is_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_TOC_MSF_next_out_CDB10(Var Out_TOC_desc : T_out_read_T_P_A_TOC_desc_MSF) : Boolean;
Var TOC_data_len       : Word;
    There_is_TOC_data  : Boolean;
    P_desc_data_offset : Pointer;
Begin
     There_is_TOC_data:=False;
     If (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Cmd=MMC_CMD_READ_TOC_PMA_ATIP) AND
        (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Format=MMC_READ_T_P_A_FORMAT_TOC) AND
        (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.MSF=MMC_READ_T_P_A_MSF) AND
        SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_read_T_P_A_TOC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               TOC_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                 Hdr.Data_len_LoByte);
          End;
          If (TOC_data_len+2)>=(SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_TOC_trk_desc_data)) Then
          Begin
               P_desc_data_offset:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(LongWord(P_desc_data_offset), SPC_WNASPI32.WNASPI32.Desc_data_offset);

               With T_read_T_P_A_TOC_trk_desc_data(P_desc_data_offset^) Do
               Begin
                    Out_TOC_desc.ADR_Ctrl:=ADR_Control;
                    Out_TOC_desc.TrackNo:=TrackNo;
                    Out_TOC_desc.StartM:=Adr_HiMiByte;
                    Out_TOC_desc.StartS:=Adr_LoMiByte;
                    Out_TOC_desc.StartF:=Adr_LoByte;
               End;
               There_is_TOC_data:=True;
               SPC_WNASPI32.WNASPI32.Desc_data_offset:=SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_TOC_trk_desc_data);
          End;
     End;

     Result:=There_is_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_sess_MMCLBA_out_CDB10(Var Out_first_sessNo : Byte;
                                                             Var Out_last_sessNo  : Byte;
                                                             Var Out_sess_desc : T_out_read_T_P_A_sess_desc_MMCLBA) : Boolean;
Var There_is_TOC_data : Boolean;
Begin
     Do_read_T_P_A_CDB10(MMC_READ_T_P_A_MMCLBA,
                         MMC_READ_T_P_A_FORMAT_SESS_INFO,
                         0);

     There_is_TOC_data:=False;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK then
     Begin
          With T_read_T_P_A_sess_info_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               If (SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                       Hdr.Data_len_LoByte)-2)>=SizeOf(T_read_T_P_A_sess_info_desc_data) Then
               Begin
                    With Sess_info_desc Do
                    Begin
                         Out_first_sessNo:=Hdr.First_trk_sess;
                         Out_last_sessNo:=Hdr.Last_trk_sess;

                         Out_sess_desc.ADR_Ctrl:=ADR_Control;
                         Out_sess_desc.FirstTrkNo_InLastSess:=FirstTrackNo;
                         Out_sess_desc.StartMMCLBA:=SPC_WNASPI32.SPC.ReverseBytesToLongInt(Adr_HiByte,
                                                                                           Adr_HiMiByte,
                                                                                           Adr_LoMiByte,
                                                                                           Adr_LoByte);
                    End;
                    There_is_TOC_data:=True;
               End;
          End;
    End;

    Result:=There_is_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_sess_MSF_out_CDB10(Var Out_first_sessNo : Byte;
                                                          Var Out_last_sessNo  : Byte;
                                                          Var Out_sess_desc    : T_out_read_T_P_A_sess_desc_MSF) : Boolean;
Var There_is_TOC_data : Boolean;
Begin
     Do_read_T_P_A_CDB10(MMC_READ_T_P_A_MSF,
                         MMC_READ_T_P_A_FORMAT_SESS_INFO,
                         0);

     There_is_TOC_data:=False;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK then
     Begin
          With T_read_T_P_A_sess_info_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               If (SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                       Hdr.Data_len_LoByte)-2)>=SizeOf(T_read_T_P_A_sess_info_desc_data) Then
               Begin
                    With Sess_info_desc Do
                    Begin
                         Out_first_sessNo:=Hdr.First_trk_sess;
                         Out_last_sessNo:=Hdr.Last_trk_sess;

                         Out_sess_desc.ADR_Ctrl:=ADR_Control;
                         Out_sess_desc.FirstTrkNo_InLastSess:=FirstTrackNo;
                         Out_sess_desc.StartM:=Adr_HiMiByte;
                         Out_sess_desc.StartS:=Adr_LoMiByte;
                         Out_sess_desc.StartF:=Adr_LoByte;
                    End;
                    There_is_TOC_data:=True;
               End;
          End;
    End;

    Result:=There_is_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_full_TOC_first_out_CDB10(Var Out_first_sessNo  : Byte;
                                                                Var Out_last_sessNo   : Byte;
                                                                Var Out_full_TOC_desc : T_out_read_T_P_A_full_TOC_desc;
                                                                    Session_no        : Byte) : Boolean;
Var Full_TOC_data_len      : Word;
    There_is_full_TOC_data : Boolean;
Begin
     Do_read_T_P_A_CDB10(MMC_READ_T_P_A_MSF,
                         MMC_READ_T_P_A_FORMAT_FULL_TOC,
                         Session_no);

     There_is_full_TOC_data:=False;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK then
     Begin
          With T_read_T_P_A_full_TOC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               Full_TOC_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                      Hdr.Data_len_LoByte);

               If (Full_TOC_data_len-2)>=(SizeOf(T_read_T_P_A_full_TOC_trk_desc_data)) Then
               Begin
                    Out_first_sessNo:=Hdr.First_trk_sess;
                    Out_last_sessNo:=Hdr.Last_Trk_sess;

                    Out_full_TOC_desc.SessionNo:=Full_TOC_trk_desc.SessionNo;
                    Out_full_TOC_desc.ADR_Ctrl:=Full_TOC_trk_desc.ADR_Control;
                    Out_full_TOC_desc.TNO:=Full_TOC_trk_desc.TNO;
                    Out_full_TOC_desc.POINT:=Full_TOC_trk_desc.POINT;
                    Out_full_TOC_desc.Min:=Full_TOC_trk_desc.Min;
                    Out_full_TOC_desc.Sec:=Full_TOC_trk_desc.Sec;
                    Out_full_TOC_desc.Frame:=Full_TOC_trk_desc.Frame;
                    Out_full_TOC_desc.Zero:=Full_TOC_trk_desc.Zero;
                    Out_full_TOC_desc.PMin:=Full_TOC_trk_desc.PMin;
                    Out_full_TOC_desc.PSec:=Full_TOC_trk_desc.PSec;
                    Out_full_TOC_desc.PFrame:=Full_TOC_trk_desc.PFrame;

                    There_is_full_TOC_data:=True;
                    SPC_WNASPI32.WNASPI32.Desc_data_offset:=SizeOf(T_read_T_P_A_full_TOC_data);
               End;
          End;
     End;

     Result:=There_is_full_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_full_TOC_next_out_CDB10(Var Out_full_TOC_desc : T_out_read_T_P_A_full_TOC_desc) : Boolean;
Var Full_TOC_data_len      : Word;
    There_is_full_TOC_data : Boolean;
    P_desc_data_offset     : Pointer;
Begin
     There_is_full_TOC_data:=False;
     If (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Cmd=MMC_CMD_READ_TOC_PMA_ATIP) AND
        (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Format=MMC_READ_T_P_A_FORMAT_FULL_TOC) AND
        SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_read_T_P_A_full_TOC_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               Full_TOC_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                      Hdr.Data_len_LoByte);
          End;
          If (Full_TOC_data_len+2)>=(SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_full_TOC_trk_desc_data)) Then
          Begin
               P_desc_data_offset:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(LongWord(P_desc_data_offset), SPC_WNASPI32.WNASPI32.Desc_data_offset);

               With T_read_T_P_A_full_TOC_trk_desc_data(P_desc_data_offset^) Do
               Begin
                    Out_full_TOC_desc.SessionNo:=SessionNo;
                    Out_full_TOC_desc.ADR_Ctrl:=ADR_Control;
                    Out_full_TOC_desc.TNO:=TNO;
                    Out_full_TOC_desc.POINT:=POINT;
                    Out_full_TOC_desc.Min:=Min;
                    Out_full_TOC_desc.Sec:=Sec;
                    Out_full_TOC_desc.Frame:=Frame;
                    Out_full_TOC_desc.Zero:=Zero;
                    Out_full_TOC_desc.PMin:=PMin;
                    Out_full_TOC_desc.PSec:=PSec;
                    Out_full_TOC_desc.PFrame:=PFrame;
               End;
               There_is_full_TOC_data:=True;
               SPC_WNASPI32.WNASPI32.Desc_data_offset:=SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_full_TOC_trk_desc_data);
          End;
     End;

     Result:=There_is_full_TOC_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_PMA_first_out_CDB10(Var Out_PMA_desc : T_out_read_T_P_A_PMA_desc) : Boolean;
Var PMA_data_len      : Word;
    There_is_PMA_data : Boolean;
Begin
     Do_read_T_P_A_CDB10(MMC_READ_T_P_A_MMCLBA,
                         MMC_READ_T_P_A_FORMAT_PMA,
                         0);

     There_is_PMA_data:=False;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK then
     Begin
          With T_read_T_P_A_PMA_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               PMA_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                 Hdr.Data_len_LoByte);

               If (PMA_data_len-2)>=(SizeOf(T_read_T_P_A_PMA_desc_data)) Then
               Begin
                    Out_PMA_desc.ADR_Ctrl:=PMA_desc.ADR_Control;
                    Out_PMA_desc.TNO:=PMA_desc.TNO;
                    Out_PMA_desc.POINT:=PMA_desc.POINT;
                    Out_PMA_desc.Min:=PMA_desc.Min;
                    Out_PMA_desc.Sec:=PMA_desc.Sec;
                    Out_PMA_desc.Frame:=PMA_desc.Frame;
                    Out_PMA_desc.Zero:=PMA_desc.Zero;
                    Out_PMA_desc.PMin:=PMA_desc.PMin;
                    Out_PMA_desc.PSec:=PMA_desc.PSec;
                    Out_PMA_desc.PFrame:=PMA_desc.PFrame;

                    There_is_PMA_data:=True;
                    SPC_WNASPI32.WNASPI32.Desc_data_offset:=SizeOf(T_read_T_P_A_PMA_data);
               End;
          End;
     End;

     Result:=There_is_PMA_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_PMA_next_out_CDB10(Var Out_PMA_desc : T_out_read_T_P_A_PMA_desc) : Boolean;
Var PMA_data_len       : Word;
    There_is_PMA_data  : Boolean;
    P_desc_data_offset : Pointer;
Begin
     There_is_PMA_data:=False;
     If (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Cmd=MMC_CMD_READ_TOC_PMA_ATIP) AND
        (T_SRB_for_read_T_P_A(SPC_WNASPI32.WNASPI32.P_SRB^).Read_T_P_A.Format=MMC_READ_T_P_A_FORMAT_PMA) AND
        SPC_WNASPI32.WNASPI32.Get_SRB_status_OK Then
     Begin
          With T_read_T_P_A_PMA_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               PMA_data_len:=SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                                 Hdr.Data_len_LoByte);
          End;
          If (PMA_data_len+2)>=(SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_PMA_desc_data)) Then
          Begin
               P_desc_data_offset:=SPC_WNASPI32.WNASPI32.Get_data_buf;
               Inc(LongWord(P_desc_data_offset), SPC_WNASPI32.WNASPI32.Desc_data_offset);

               With T_read_T_P_A_PMA_desc_data(P_desc_data_offset^) Do
               Begin
                    Out_PMA_desc.ADR_Ctrl:=ADR_Control;
                    Out_PMA_desc.TNO:=TNO;
                    Out_PMA_desc.POINT:=POINT;
                    Out_PMA_desc.Min:=Min;
                    Out_PMA_desc.Sec:=Sec;
                    Out_PMA_desc.Frame:=Frame;
                    Out_PMA_desc.Zero:=Zero;
                    Out_PMA_desc.PMin:=PMin;
                    Out_PMA_desc.PSec:=PSec;
                    Out_PMA_desc.PFrame:=PFrame;
               End;
               There_is_PMA_data:=True;
               SPC_WNASPI32.WNASPI32.Desc_data_offset:=SPC_WNASPI32.WNASPI32.Desc_data_offset+SizeOf(T_read_T_P_A_PMA_desc_data);
          End;
     End;

     Result:=There_is_PMA_data;
End;

Function T_MMC1_WNASPI32.Do_read_T_P_A_ATIP_out_CDB10(Var Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc) : Boolean;
Var There_is_ATIP_data : Boolean;
Begin
     Do_read_T_P_A_CDB10(MMC_READ_T_P_A_MMCLBA,
                         MMC_READ_T_P_A_FORMAT_ATIP,
                         0);

     There_is_ATIP_data:=False;
     If SPC_WNASPI32.WNASPI32.Get_SRB_status_OK then
     Begin
          With T_read_T_P_A_ATIP_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               //If (SPC_WNASPI32.ReverseBytesToWord(Hdr.Data_len_HiByte,
               //                                  Hdr.Data_len_LoByte)-2)>=SizeOf(T_read_T_P_A_ATIP_desc_data) Then
               //Begin
                    With ATIP_desc Do
                    Begin
                         Out_ATIP_desc.WritePower_RecSpeed:=WritePower_RecSpeed;
                         Out_ATIP_desc.URU:=URU;
                         Out_ATIP_desc.DType_DSubType_A1A2A3:=DType_DSubType_A1A2A3;
                         Out_ATIP_desc.StartMin:=StartMin;
                         Out_ATIP_desc.StartSec:=StartSec;
                         Out_ATIP_desc.StartFrame:=StartFrame;
                         Out_ATIP_desc.LastMin:=LastMin;
                         Out_ATIP_desc.LastSec:=LastSec;
                         Out_ATIP_desc.LastFrame:=LastFrame;
                         Out_ATIP_desc.CLV_rec_speeds:=CLV_rec_speeds;
                         Out_ATIP_desc.A2_byte1:=A2_values[1];
                         Out_ATIP_desc.A2_byte2:=A2_values[2];
                         Out_ATIP_desc.A2_byte3:=A2_values[3];
                         Out_ATIP_desc.A3_byte1:=A3_values[1];
                         Out_ATIP_desc.A3_byte2:=A3_values[2];
                         Out_ATIP_desc.A3_byte3:=A3_values[3];
                    End;

                    If SPC_WNASPI32.SPC.ReverseBytesToWord(Hdr.Data_len_HiByte,
                                                           Hdr.Data_len_LoByte)>0 Then
                    Begin
                         There_is_ATIP_data:=True;
                    End;
               //End;
          End;
    End;

    Result:=There_is_ATIP_data;
End;

Procedure T_MMC1_WNASPI32.Do_sense6_CD_cap_mech_st_sub6_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st);
{ ****************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.  Friendly version.
  *************************************************************************************** }
Var P_CD_cap_mech_st : Pointer;
Begin
     Do_sense6_CD_cap_mech_st_sub6;

     If T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len=0 Then
     Begin
          With T_mode_param_6_CD_cap_mech_st_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               P_CD_cap_mech_st:=@CD_cap_mech_st;
          End;
     End
     Else
     Begin
          P_CD_cap_mech_st:=Ptr(LongWord(SPC_WNASPI32.WNASPI32.Get_data_buf)+
                                SizeOf(T_mode_param_hdr_6)+
                                T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len);
     End;

     MMC1.Fill_out_CD_cap_mech_st(P_CD_cap_mech_st, Out_CD_cap_mech_st);
End;

Procedure T_MMC1_WNASPI32.Do_sense10_CD_cap_mech_st_sub6_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st);
{ *****************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.  Friendly version.
  ***************************************************************************************** }
Var P_CD_cap_mech_st : Pointer;
Begin
     Do_sense10_CD_cap_mech_st_sub6;

     If SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                            T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)=0 Then
     Begin
          With T_mode_param_10_CD_cap_mech_st_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               P_CD_cap_mech_st:=@CD_cap_mech_st;
          End;
     End
     Else
     Begin
          P_CD_cap_mech_st:=Ptr(LongWord(SPC_WNASPI32.WNASPI32.Get_data_buf)+
                                SizeOf(T_mode_param_hdr_10)+
                                SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                    T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB));
     End;

     MMC1.Fill_out_CD_cap_mech_st(P_CD_cap_mech_st, Out_CD_cap_mech_st);
End;

Procedure T_MMC1_WNASPI32.Do_sense6_CD_cap_mech_st_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st);
{ ****************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB6 to target
  device.  The T_GenSCSI will make the call to WNASPI32.  Friendly version.
  *************************************************************************************** }
Var P_CD_cap_mech_st : Pointer;
Begin
     Do_sense6_CD_cap_mech_st;

     If T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len=0 Then
     Begin
          With T_mode_param_6_CD_cap_mech_st_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               P_CD_cap_mech_st:=@CD_cap_mech_st;
          End;
     End
     Else
     Begin
          P_CD_cap_mech_st:=Ptr(LongWord(SPC_WNASPI32.WNASPI32.Get_data_buf)+
                                SizeOf(T_mode_param_hdr_6)+
                                T_mode_param_hdr_6(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len);
     End;

     MMC1.Fill_out_CD_cap_mech_st(P_CD_cap_mech_st, Out_CD_cap_mech_st);
End;

Procedure T_MMC1_WNASPI32.Do_sense10_CD_cap_mech_st_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st);
{ *****************************************************************************************
  Retrieve CD capabilities and mechanism status parameters using mode sense CDB10 to target
  device.  The T_GenSCSI will make the call to WNASPI32.  Friendly version.
  ***************************************************************************************** }
Var P_CD_cap_mech_st : Pointer;
Begin
     Do_sense10_CD_cap_mech_st;

     If SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                            T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB)=0 Then
     Begin
          With T_mode_param_10_CD_cap_mech_st_noblk_data(SPC_WNASPI32.WNASPI32.Get_data_buf^) Do
          Begin
               P_CD_cap_mech_st:=@CD_cap_mech_st;
          End;
     End
     Else
     Begin
          //MessageDlg('blocks', mtError, [mbOk], 0);
          P_CD_cap_mech_st:=Ptr(LongWord(SPC_WNASPI32.WNASPI32.Get_data_buf)+
                                SizeOf(T_mode_param_hdr_10)+
                                SPC_WNASPI32.SPC.ReverseBytesToWord(T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_MSB,
                                                                    T_mode_param_hdr_10(SPC_WNASPI32.WNASPI32.Get_data_buf^).Block_desc_len_LSB));
     End;

     MMC1.Fill_out_CD_cap_mech_st(P_CD_cap_mech_st, Out_CD_cap_mech_st);
End;

end.
