unit MMC1_any_link_Unit;

interface

Uses
    SPC_WNASPI32_Unit,
    SPC_SPT_Unit,
    MMC1Unit;

Type T_MMC1_any_link=
Class
Protected
      SPC_WNASPI32 : T_SPC_WNASPI32; //SCSI-3 SPC & Win ASPI 32 combined instance.
      SPC_SPT : T_SPC_SPT; //SPC & DeviceIOCTL SPT combined instance.
Public
      MMC1 : T_MMC1; //SCSI-3 MMC1 instance.

      Function Get_MMC1 : T_MMC1; virtual; abstract;
      Function Get_err_msg : PChar; virtual; abstract;
      { SCSI-3 MMC command methods using Win ASPI 32 interface.. }
      Procedure Do_read_rec_cap_CDB10; virtual; abstract;
      Procedure Do_read_subch_CDB10(In_MSF     : Byte;
                                    In_SubQ    : Byte;
                                    In_Params  : Byte;
                                    In_TrackNo : Byte); virtual; abstract;
      Procedure Do_read_T_P_A_CDB10(In_MSF       : Byte;
                                    In_format    : Byte;
                                    In_trkSessNo : Byte); virtual; abstract;
      Procedure Do_read_header_CDB10(In_MSF        : Byte;
                                     In_MMCLBA_MSF : Pointer); virtual; abstract;
      Procedure Do_read_header_MMCLBA_CDB10(In_MMCLBA : LongInt); virtual; abstract;
      Procedure Do_read_header_MSF_CDB10(In_MSF : T_MSF); virtual; abstract;
      Procedure Do_readCD_MSF_byFormat_CDB12(Var In_StartMSF        : T_MSF;
                                             Var In_EndMSF          : T_MSF;
                                                 In_SectTypeFilter  : Byte;
                                                 In_ReadFormatFlags : Byte;
                                                 In_SubChSel        : Byte); virtual; abstract;
      Procedure Do_set_CD_speed_CDB12(In_ReadSpeed  : Word;
                                      In_WriteSpeed : Word); virtual; abstract;
      Procedure Do_readCD_byFormat_CDB12(In_Start_MMCLBA    : LongInt;
                                         In_N_sectors       : LongWord;
                                         In_SectTypeFilter  : Byte;
                                         In_ReadFormatFlags : Byte;
                                         In_SubChSel        : Byte); virtual; abstract;
      Procedure Do_read_buf_cap_CDB10; virtual; abstract;
      Procedure Do_read_disc_info_CDB10; virtual; abstract;
      Procedure Do_read_disc_info_with_OPC_CDB10; virtual; abstract;
      Procedure Do_read_track_info_CDB10(In_TrkBIT       : Byte;
                                         In_MMCLBA_TrkNo : LongInt); virtual; abstract;
      Procedure Do_reserve_track_CDB10(In_Reverv_size : LongWord); virtual; abstract;
      Procedure Do_synchronize_cache_CDB10(In_MMCLBA   : LongInt;
                                           In_N_blocks : Word); virtual; abstract;
      Procedure Do_write_CDB10(In_DPO_FUA_RELADR : Byte;
                               In_MMCLBA         : LongInt;
                               In_N_blocks       : Word;
                               In_block_size     : Word); virtual; abstract;
      {** Select/sense for MMC mode parameters.. **}
      Function Calc_modepage6_ptr : Pointer; virtual; abstract;
      Function Calc_modepage10_ptr : Pointer; virtual; abstract;
      Procedure Do_select6_read_err_rec(In_err_rec_param    : Byte;
                                        In_read_retry_count : Byte); virtual; abstract;
      Procedure Do_select10_read_err_rec(In_err_rec_param    : Byte;
                                         In_read_retry_count : Byte); virtual; abstract;
      Procedure Do_sense6_read_err_rec; virtual; abstract;
      Procedure Do_sense10_read_err_rec; virtual; abstract;
      Procedure Do_select6_write(Var In_write_params : T_pub_write_params); virtual; abstract;
      Procedure Do_select10_write(Var In_write_params : T_pub_write_params); virtual; abstract;
      Procedure Do_sense6_write; virtual; abstract;
      Procedure Do_sense10_write; virtual; abstract;
      Procedure Do_select6_veri_err_rec(In_err_rec_param    : Byte;
                                        In_veri_retry_count : Byte); virtual; abstract;
      Procedure Do_select10_veri_err_rec(In_err_rec_param    : Byte;
                                         In_veri_retry_count : Byte); virtual; abstract;
      Procedure Do_sense6_veri_err_rec; virtual; abstract;
      Procedure Do_sense10_veri_err_rec; virtual; abstract;
      Procedure Do_select6_CD(In_inactivity_mul : Byte;
                              In_sec_per_MSF    : Word;
                              In_frames_per_MSF : Word); virtual; abstract;
      Procedure Do_select10_CD(In_inactivity_mul : Byte;
                               In_sec_per_MSF    : Word;
                               In_frames_per_MSF : Word); virtual; abstract;
      Procedure Do_sense6_CD; virtual; abstract;
      Procedure Do_sense10_CD; virtual; abstract;
      Procedure Do_select6_CD_audio_ctrl(Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl); virtual; abstract;
      Procedure Do_select10_CD_audio_ctrl(Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl); virtual; abstract;
      Procedure Do_sense6_CD_audio_ctrl; virtual; abstract;
      Procedure Do_sense10_CD_audio_ctrl; virtual; abstract;
      Procedure Do_sense6_CD_cap_mech_st_sub6; virtual; abstract;
      Procedure Do_sense10_CD_cap_mech_st_sub6; virtual; abstract;
      Procedure Do_sense6_CD_cap_mech_st; virtual; abstract;
      Procedure Do_sense10_CD_cap_mech_st; virtual; abstract;
      { Friendly versions of SCSI MMC command methods.. }
      Procedure Do_read_rec_cap_out_CDB10(Var LastMMCLBA     : LongInt;
                                          Var MMCLBABlockLen : LongWord); virtual; abstract;
      Procedure Do_read_disc_info_out_CDB10(Var Out_read_disc_info_CDB10_block : T_out_read_disc_info_CDB10_block); virtual; abstract;
      Procedure Do_read_disc_info_with_OPC_out_CDB10(Var Out_read_disc_info_CDB10_block : T_out_read_disc_info_CDB10_block); virtual; abstract;
      Procedure Do_read_track_info_out_CDB10(    In_TrkBIT       : Byte;
                                                 In_MMCLBA_TrkNo : LongInt;
                                             Var Out_read_track_info_CDB10 : T_out_read_track_info_CDB10_block); virtual; abstract;
      { Variations of read subchannel command methods.. }
      Procedure Do_read_subch_curr_MMCLBA_CDB10(Var Out_abs_MMCLBA : LongInt;
                                                Var Out_rel_MMCLBA : LongInt); virtual; abstract;
      Procedure Do_read_subch_curr_MSF_CDB10(Var Out_abs_MSF : T_MSF;
                                             Var Out_rel_MSF : T_MSF); virtual; abstract;
      Procedure Do_read_subch_aud_st_out_CDB10(Var Out_audioStatus : Byte); virtual; abstract;
      Procedure Do_read_subch_MCN_out_CDB10(Var Out_MC_valid : Boolean;
                                            Var Out_MCN_str  : String); virtual; abstract;
      Procedure Do_read_subch_ISRC_out_CDB10(    In_TrackNo   : Byte;
                                             Var Out_TC_valid : Boolean;
                                             Var Out_ISRC_str : String); virtual; abstract;
      { Variations of read TOC/PMA/ATIP command methods.. }
      Function Do_read_T_P_A_TOC_MMCLBA_first_out_CDB10(Var Out_first_trkNo : Byte;
                                                        Var Out_last_trkNo  : Byte;
                                                        Var Out_TOC_desc    : T_out_read_T_P_A_TOC_desc_MMCLBA;
                                                        Track_no            : Byte) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_TOC_MMCLBA_next_out_CDB10(Var Out_TOC_desc : T_out_read_T_P_A_TOC_desc_MMCLBA) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_TOC_MSF_first_out_CDB10(Var Out_first_trkNo : Byte;
                                                     Var Out_last_trkNo  : Byte;
                                                     Var Out_TOC_desc    : T_out_read_T_P_A_TOC_desc_MSF;
                                                     Track_no            : Byte) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_TOC_MSF_next_out_CDB10(Var Out_TOC_desc : T_out_read_T_P_A_TOC_desc_MSF) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_sess_MMCLBA_out_CDB10(Var Out_first_sessNo : Byte;
                                                   Var Out_last_sessNo  : Byte;
                                                   Var Out_sess_desc : T_out_read_T_P_A_sess_desc_MMCLBA) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_sess_MSF_out_CDB10(Var Out_first_sessNo : Byte;
                                                Var Out_last_sessNo  : Byte;
                                                Var Out_sess_desc : T_out_read_T_P_A_sess_desc_MSF) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_full_TOC_first_out_CDB10(Var Out_first_sessNo  : Byte;
                                                      Var Out_last_sessNo   : Byte;
                                                      Var Out_full_TOC_desc : T_out_read_T_P_A_full_TOC_desc;
                                                      Session_no            : Byte) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_full_TOC_next_out_CDB10(Var Out_full_TOC_desc : T_out_read_T_P_A_full_TOC_desc) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_PMA_first_out_CDB10(Var Out_PMA_desc : T_out_read_T_P_A_PMA_desc) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_PMA_next_out_CDB10(Var Out_PMA_desc : T_out_read_T_P_A_PMA_desc) : Boolean; virtual; abstract;
      Function Do_read_T_P_A_ATIP_out_CDB10(Var Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc) : Boolean; virtual; abstract;
      Procedure Do_sense6_CD_cap_mech_st_sub6_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); virtual; abstract;
      Procedure Do_sense10_CD_cap_mech_st_sub6_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); virtual; abstract;
      Procedure Do_sense6_CD_cap_mech_st_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); virtual; abstract;
      Procedure Do_sense10_CD_cap_mech_st_out(Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st); virtual; abstract;
End;

implementation

end.
 