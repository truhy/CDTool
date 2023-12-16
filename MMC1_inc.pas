{ Name:         MMC1.PAS
  File type:    Borland Delphi 4 include file.
  Description:  MMC1 definitions written for Borland Delphi 4.
                This source file based on information given from
                the working draft version of SCSI-3 MMC-1 specification
                document.

                MMC1 definitions are for SCSI multimedia devices such
                as CD readers or CD writers.
  Notes:        Written from scratch, no other samples used, except SDK.
  Date started: 14th Dec 1999.
  Developer:    Truong Hy.
}

{** Misc **}
Const C_1X_KBYTES_CDSPEED=176.4; //Calculating data rate of digital stereo (2 channels)
                                 //audio at 44.1 KHz with 16 BITs per channel.
                                 //----------------------------------------------------
                                 //Number of bytes taken up by 1 sample of audio:
                                 //16 BIT = 1 word = 2 bytes,
                                 //but there are 2 channels, so 1 sample takes up
                                 //2*2=4 bytes.
                                 //
                                 //Data rate in kilobytes/sec will be:
                                 //     s=Number of samples per second
                                 //     b=Number of bytes per sample
                                 //
                                 //     s*b
                                 //     ---- = data rate kb/s
                                 //     1000
                                 //So data rate of the digital audio is:
                                 //     44100*4
                                 //     ------- = 176.4 kb/s
                                 //     1000
                                 //This data rate is used by CDROM/CD writers as 1X speed.
                                 //This will of course have to be rounded to a
                                 //whole number: 176 kb/s, making prog. easier.
{** SCSI-3 MMC command codes **}
{** CD read commands **}
Const MMC_CMD_READCD_REC_CAP   =$25;
Const MMC_CMD_READ_SUBCH       =$42;
Const MMC_CMD_READ_TOC_PMA_ATIP=$43;
Const MMC_CMD_READ_HEADER      =$44;
Const MMC_CMD_PLAYAUDIO_CDB10  =$45;
Const MMC_CMD_PLAYAUDIO_MSF    =$47;
Const MMC_CMD_PAUSE_RESUME     =$4B;
Const MMC_CMD_PLAYAUDIO_CDB12  =$A5;
Const MMC_CMD_LOADUNLOAD       =$A6;
Const MMC_CMD_READCD_MSF       =$B9;
Const MMC_CMD_SCAN             =$BA;
Const MMC_CMD_SET_CD_SPEED     =$BB;
Const MMC_CMD_PLAYCD           =$BC;
Const MMC_CMD_READCD_MMCLBA    =$BE;
Const MMC_CMD_READ_BUF_CAP     =$5C;
Const MMC_CMD_READ_DISC_INFO   =$51;
Const MMC_CMD_READ_TRACK_INFO  =$52;
Const MMC_CMD_RESERVE_TRACK    =$53;
Const MMC_CMD_SYNC_CACHE       =$35;
Const MMC_CMD_WRITE_CDB10      =$2A;

{** Flags for read sub channel command **}
Const MMC_READ_SUBCH_MMCLBA=0;
Const MMC_READ_SUBCH_MSF=2;
{** Subchannel parameter list codes **}
Const MMC_READ_SUBCH_SUBQ=$40;
Const MMC_READ_SUBCH_CURR_POS=1;
Const MMC_READ_SUBCH_MCN=2;
Const MMC_READ_SUBCH_ISRC=3;
{** Audio status codes **}
Const MMC_READ_SUBCH_AUD_ST_INVALID    =$00;
Const MMC_READ_SUBCH_AUD_ST_PLAYING    =$11;
Const MMC_READ_SUBCH_AUD_ST_PAUSED     =$12;
Const MMC_READ_SUBCH_AUD_ST_PLAY_ENDED =$13;
Const MMC_READ_SUBCH_AUD_ST_ERR_STOPPED=$14;
Const MMC_READ_SUBCH_AUD_ST_NONE       =$15;
{** ADR Q codes **}
Const MMC_READ_SUBCH_ADR_NONE    =$00;
Const MMC_READ_SUBCH_ADR_CURR_POS=$01;
Const MMC_READ_SUBCH_ADR_MCN     =$02;
Const MMC_READ_SUBCH_ADR_ISRC    =$03;
{** Mask for "ADR" and "CTRL (Control)" **}
Const MMC_READ_SUBCH_ADRCTRL_MASK_ADR =$F0; //=11110000b
Const MMC_READ_SUBCH_ADRCTRL_MASK_CTRL=$0F; //=00001111b
{** "Control" AND masks **}
{ 1=copy (we are interested with this bit),
  0=filter out (we are not interested with this bit). }
Const MMC_READ_SUBCH_CTRL_MASK_2CH     =13; //=1101b
Const MMC_READ_SUBCH_CTRL_MASK_2CH_PRE =13; //=1101b
Const MMC_READ_SUBCH_CTRL_MASK_CH      =13; //=1101b
Const MMC_READ_SUBCH_CTRL_MASK_CH_PRE  =13; //=1101b
Const MMC_READ_SUBCH_CTRL_MASK_DAO     =13; //=1101b
Const MMC_READ_SUBCH_CTRL_MASK_INC     =13; //=1101b
Const MMC_READ_SUBCH_CTRL_MASK_COPY_BIT=2;  //=0010b
Const MMC_READ_SUBCH_CTRL_MASK_DATA    =12; //=1100b
Const MMC_READ_SUBCH_CTRL_MASK_AUDIO   =4;  //=0100b
Const MMC_READ_SUBCH_CTRL_MASK_RESERVED=12; //=1100b
{** "Control" AND mask equate results **}
{ x=orginally defined as don't care, in this case it will always be 0. }
Const MMC_READ_SUBCH_CTRL_2CH         =0; //00x0b
Const MMC_READ_SUBCH_CTRL_2CH_PRE     =1; //00x1b
Const MMC_READ_SUBCH_CTRL_CH          =8; //10x0b
Const MMC_READ_SUBCH_CTRL_CH_PRE      =9; //10x1b
Const MMC_READ_SUBCH_CTRL_DAO         =4; //01x0b
Const MMC_READ_SUBCH_CTRL_INC         =5; //01x1b
Const MMC_READ_SUBCH_CTRL_COPY_PROT   =0; //xx0xb
Const MMC_READ_SUBCH_CTRL_COPY_NOTPROT=2; //xx1xb
Const MMC_READ_SUBCH_CTRL_DATA        =4; //01x0b
Const MMC_READ_SUBCH_CTRL_AUDIO       =0; //00x0b
Const MMC_READ_SUBCH_CTRL_RESERVED    =12; //11xxb

{** Flags for read TOC/PMA/ATIP command **}
Const MMC_READ_T_P_A_FORMAT_TOC      =0;
Const MMC_READ_T_P_A_FORMAT_SESS_INFO=1;
Const MMC_READ_T_P_A_FORMAT_FULL_TOC =2;
Const MMC_READ_T_P_A_FORMAT_PMA      =3;
Const MMC_READ_T_P_A_FORMAT_ATIP     =4;
Const MMC_READ_T_P_A_MMCLBA=0;
Const MMC_READ_T_P_A_MSF=2;

{** Flags for read header command **}
Const MMC_READ_HEADER_MMCLBA=0;
Const MMC_READ_HEADER_MSF=2;
{** CD data mode flags **}
Const MMC_READ_HEADER_MODE0_CDDA=$00;
Const MMC_READ_HEADER_MODE1     =$01;
Const MMC_READ_HEADER_MODE2     =$02;

{** Flags for play audio CDB10, CDB12, playcd and readCD CDB12 command **}
Const MMC_PLAYAUDIO_CURR_MMCLBA=$FFFFFFFF;

{** Flags for play audio MSF command **}
Const MMC_PLAYAUDIO_CURR_MSF=$FF;

{** Flags for pause/resume command **}
Const MMC_PAUSE=0;
Const MMC_RESUME=1;

{** Flags for load/unload cd changer command **}
Const MMC_LOUNLO_IMMED_NOWAIT=$1;
Const MMC_LOUNLO_IMMED_WAIT=$0;
Const MMC_LOUNLO_ABORT=$0;
Const MMC_LOUNLO_UNLOAD=$2;
Const MMC_LOUNLO_LOAD=$3;

{** Expected sector type flags **}
Const MMC_SECTORTYPE_ANY=0;
Const MMC_SECTORTYPE_CDDA=1;
Const MMC_SECTORTYPE_MODE1=2;
Const MMC_SECTORTYPE_MODE2FORMLESS=3;
Const MMC_SECTORTYPE_MODE2FORM1=4;
Const MMC_SECTORTYPE_MODE2FORM2=5;

{** Flags for scan command **}
Const MMC_SCAN_MMCLBA=0;
Const MMC_SCAN_MSF=$40;
Const MMC_SCAN_TRACKNO=$80;

{** Flags for set CD command **}
Const MMC_SET_CD_SPEED_MAX=$FFFF;

{** Flags for play CD command **}
Const MMC_PLAYCD_MMCLBA=0;
Const MMC_PLAYCD_MSF=2;
Const MMC_PLAYCD_CURR_MSF=$0FFFFFF;
{** Bit flags for field byte definition **}
Const MMC_PLAYCD_ANALOG_AUDIO=$01; //Bit 0
Const MMC_PLAYCD_COMP_VID=$02;     //Bit 1
Const MMC_PLAYCD_DG_PORT1=$04;     //Bit 2
Const MMC_PLAYCD_DG_PORT2=$08;     //Bit 3
Const MMC_PLAYCD_1XSPEED=$00;      //Bit 7
Const MMC_PLAYCD_BESTSPEED=$80;    //Bit 7

{** Flags for read CD commands **}
{** Sub channel selection flags **}
Const MMC_READCD_NO_SUBCH    =$00;
Const MMC_READCD_SUBCH_RAWPW =$01;
Const MMC_READCD_SUBCH_FORMPQ=$02;
Const MMC_READCD_SUBCH_PACKPW=$04;
{** Error selection flags **}
Const MMC_READCD_C2_NONE   =$00;
Const MMC_READCD_C2_PTR    =$02;
Const MMC_READCD_C2_PTR_BLK=$04;
{** Select returning of EDC/ECC flag **}
Const MMC_READCD_EDCECC    =$08;
{** Select returning of user data only **}
Const MMC_READCD_USERDATA  =$10;
{** Select returning of header only **}
Const MMC_READCD_HDR       =$20;
{** Select returning of sub-header only **}
Const MMC_READCD_SUBHDR    =$40;
{** Select returning of all header data **}
Const MMC_READCD_ALLHDR    =$60;
{** Select returning of sync header only **}
Const MMC_READCD_SYNC      =$80;
{** Some combined flags of the above **}
Const MMC_READCD_USERDATA_EDCECC            =$18;
Const MMC_READCD_SYNC_ALLHDR_USERDATA       =$F0;
Const MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC=$F8;

{** Misc types **}
Type
    T_array3=Array[1..3] Of Byte;
    T_array4=Array[1..4] Of Byte;
    T_array6=Array[1..6] Of Byte;
    T_char_array12=Array[1..12] Of Char;

Type T_MSF=Record
                 M : Byte;
                 S : Byte;
                 F : Byte;
           End;

Type T_MMC_MSF=Record
                     Reserved : Byte;
                     M : Byte;
                     S : Byte;
                     F : Byte;
               End;

{** SCSI-3 MMC CDBs **}
{*********************}
{** CDB for read CD recorded capacity command (25h) **}
Type T_readCD_rec_cap=Record
                            Cmd             : Byte;
                            RelAdr          : Byte;
                            MMCLBA_HiByte   : Byte;
                            MMCLBA_HiMiByte : Byte;
                            MMCLBA_LoMiByte : Byte;
                            MMCLBA_LoByte   : Byte;
                            Reserved1       : Byte;
                            Reserved2       : Byte;
                            PMI             : Byte;
                            Control         : Byte;
                      End;

{** CDB for read sub-channel command (42h) **}
Type T_read_subch=Record
                        Cmd        : Byte;
                        MSF        : Byte;
                        SubQ       : Byte;
                        Params     : Byte;
                        Reserved1  : Byte;
                        Reserved2  : Byte;
                        TrackNo    : Byte;
                        Len_HiByte : Byte;
                        Len_LoByte : Byte;
                        Control    : Byte;
                  End;

{** CDB for read TOC/PMA/ATIP command (43h) **}
Type T_read_T_P_A=Record
                        Cmd        : Byte;
                        MSF        : Byte;
                        Format     : Byte;
                        Reserved1  : Byte;
                        Reserved2  : Byte;
                        Reserved3  : Byte;
                        TrkSessNo  : Byte;
                        Len_HiByte : Byte;
                        Len_LoByte : Byte;
                        Control    : Byte;
                  End;

{** CDB for read header command (44h)**}
Type T_read_header=Record
                         Cmd                  : Byte;
                         MSF                  : Byte;
                         StartMMCLBA_HiByte   : Byte;
                         StartMMCLBA_HiMiByte : Byte;
                         StartMMCLBA_LoMiByte : Byte;
                         StartMMCLBA_LoByte   : Byte;
                         Reserved1            : Byte;
                         Len_HiByte           : Byte;
                         Len_LoByte           : Byte;
                         Control              : Byte;
                   End;

{** CDB for play audio CDB10 command (45h) **}
Type T_play_audio_CDB10=Record
                              Cmd                  : Byte;
                              RelAdr               : Byte;
                              StartMMCLBA_HiByte   : Byte;
                              StartMMCLBA_HiMiByte : Byte;
                              StartMMCLBA_LoMiByte : Byte;
                              StartMMCLBA_LoByte   : Byte;
                              Reserved1            : Byte;
                              Len_HiByte           : Byte;
                              Len_LoByte           : Byte;
                              Control              : Byte;
                        End;

{** CDB for play audio MSF command (47h) **}
Type T_play_audio_MSF=Record
                            Cmd       : Byte;
                            Reserved1 : Byte;
                            Reserved2 : Byte;
                            StartM    : Byte;
                            StartS    : Byte;
                            StartF    : Byte;
                            EndM      : Byte;
                            EndS      : Byte;
                            EndF      : Byte;
                            Control   : Byte;
                      End;

{** CDB for play audio CDB12 command (A5h) **}
Type T_play_audio_CDB12=Record
                              Cmd                  : Byte;
                              RelAdr               : Byte;
                              StartMMCLBA_HiByte   : Byte;
                              StartMMCLBA_HiMiByte : Byte;
                              StartMMCLBA_LoMiByte : Byte;
                              StartMMCLBA_LoByte   : Byte;
                              Len_HiByte           : Byte;
                              Len_HiMiByte         : Byte;
                              Len_LoMiByte         : Byte;
                              Len_LoByte           : Byte;
                              Reserved1            : Byte;
                              Control              : Byte;
                        End;

{** CDB for pause/resume command (4Bh) **}
Type T_pause_resume=Record
                          Cmd       : Byte;
                          Reserved1 : Byte;
                          Reserved2 : Byte;
                          Reserved3 : Byte;
                          Reserved4 : Byte;
                          Reserved5 : Byte;
                          Reserved6 : Byte;
                          Reserved7 : Byte;
                          ResumeBit : Byte;
                          Control   : Byte;
                    End;

{** CDB for load/unload cd command (A6h) **}
Type T_loadunload_CDB12=Record
                              Cmd          : Byte;
                              Immed        : Byte;
                              Reserved1    : Byte;
                              Reserved2    : Byte;
                              LoUnlo_start : Byte;
                              Reserved3    : Byte;
                              Reserved4    : Byte;
                              Reserved5    : Byte;
                              Slot_no      : Byte;
                              Reserved6    : Byte;
                              Reserved7    : Byte;
                              Control      : Byte;
                        End;

{** CDB for read CD MSF command (B9h) **}
Type T_readCD_MSF=Record
                        Cmd          : Byte;
                        SectType     : Byte;
                        Reserved1    : Byte;
                        StartM       : Byte;
                        StartS       : Byte;
                        StartF       : Byte;
                        EndM         : Byte;
                        EndS         : Byte;
                        EndF         : Byte;
                        Format_flags : Byte;
                        SubCh_sel    : Byte;
                        Control      : Byte;
                  End;

{** CDB for scan command (BAh) **}
Type T_scan=Record
                  Cmd                   : Byte;
                  Direct_RelAdr         : Byte;
                  ScanStartAdr_HiByte   : Byte;
                  ScanStartAdr_HiMiByte : Byte;
                  ScanStartAdr_LoMiByte : Byte;
                  ScanStartAdr_LoByte   : Byte;
                  Reserved1             : Byte;
                  Reserved2             : Byte;
                  Reserved3             : Byte;
                  AdrType               : Byte;
                  Reserved4             : Byte;
                  Control               : Byte;
            End;

{** CDB for set CD speed command (BBh) **}
Type T_set_CD_speed=Record
                          Cmd               : Byte;
                          Reserved1         : Byte;
                          ReadSpeed_HiByte  : Byte;
                          ReadSpeed_LoByte  : Byte;
                          WriteSpeed_HiByte : Byte;
                          WriteSpeed_LoByte : Byte;
                          Reserved2         : Byte;
                          Reserved3         : Byte;
                          Reserved4         : Byte;
                          Reserved5         : Byte;
                          Reserved6         : Byte;
                          Control           : Byte;
                    End;

{ ** CDB for playcd command (BCh) **}
Type T_playCD=Record
                    Cmd            : Byte;
                    SectType_CMSF  : Byte;
                    Start_HiByte   : Byte;
                    Start_HiMiByte : Byte;
                    Start_LoMiByte : Byte;
                    Start_LoByte   : Byte;
                    Len_HiByte     : Byte;
                    Len_HiMiByte   : Byte;
                    Len_LoMiByte   : Byte;
                    Len_LoByte     : Byte;
                    PlayModeFields : Byte;
                    Control        : Byte;
              End;

{** CDB for read CD CDB12 command (BEh) **}
Type T_readCD_CDB12=Record
                          Cmd                  : Byte;
                          SectType_RelAdr      : Byte;
                          StartMMCLBA_HiByte   : Byte;
                          StartMMCLBA_HiMiByte : Byte;
                          StartMMCLBA_LoMiByte : Byte;
                          StartMMCLBA_LoByte   : Byte;
                          Len_HiByte           : Byte;
                          Len_MiByte           : Byte;
                          Len_LoByte           : Byte;
                          Format_flags         : Byte;
                          SubCh_sel            : Byte;
                          Control              : Byte;
                    End;

{****************************}
{** End of SCSI-3 MMC CDBs **}

{** Read CD recorded capacity return data format **}
Type T_readCD_rec_cap_data=Record
                                 MMCLBA_HiByte   : Byte;
                                 MMCLBA_HiMiByte : Byte;
                                 MMCLBA_LoMiByte : Byte;
                                 MMCLBA_LoByte   : Byte;
                                 Len_HiByte      : Byte;
                                 Len_HiMiByte    : Byte;
                                 Len_LoMiByte    : Byte;
                                 Len_LoByte      : Byte;
                           End;

{** Read sub-channel return data header format **}
Type T_read_subch_data_hdr=Record
                                 Reserved1       : Byte;
                                 Audio_status    : Byte;
                                 Data_len_HiByte : Byte;
                                 Data_len_LoByte : Byte;
                           End;

{** Read sub-channel return cd current position data format **}
Type T_read_subch_CD_curr_pos_data=Record
                                         Hdr                : T_read_subch_data_hdr;
                                         Data_format_code   : Byte;
                                         ADR_Control        : Byte;
                                         TrackNo            : Byte;
                                         IndexNo            : Byte;
                                         AbsAdr_HiByte      : Byte;
                                         AbsAdr_HiMiByte    : Byte;
                                         AbsAdr_LoMiByte    : Byte;
                                         AbsAdr_LoByte      : Byte;
                                         TrkRelAdr_HiByte   : Byte;
                                         TrkRelAdr_HiMiByte : Byte;
                                         TrkRelAdr_LoMiByte : Byte;
                                         TrkRelAdr_LoByte   : Byte;
                                   End;

{** Read sub-channel MCN format **}
Type T_read_subch_MCN_format=Record
                                   Valid  : Byte;
                                   N      : Array[1..13] Of Char;
                                   Zero   : Byte;
                                   AFrame : Byte;
                             End;

{** Read sub-channel return MCN data format **}
Type T_read_subch_MCN_data=Record
                                 Hdr              : T_read_subch_data_hdr;
                                 Data_format_code : Byte;
                                 Reserved1        : Byte;
                                 Reserved2        : Byte;
                                 Reserved3        : Byte;
                                 MCN              : T_read_subch_MCN_format;
                           End;

{** Read sub-channel ISRC format **}
Type T_read_subch_ISRC_format=Record
                                    Valid     : Byte;
                                    Country   : Array[1..2] Of Char;
                                    Owner     : Array[1..3] Of Char;
                                    Yr_of_rec : Array[1..2] Of Char;
                                    SerialNo  : Array[1..5] Of Char;
                                    Zero      : Byte;
                                    AFrame    : Byte;
                                    Reserved1 : Byte;
                              End;

{** Read sub-channel alternate ISRC format **}
Type T_read_subch_ISRC_format2=Record
                                     Valid     : Byte;
                                     ISRC      : Array[1..12] Of Char;
                                     Zero      : Byte;
                                     AFrame    : Byte;
                                     Reserved1 : Byte;
                               End;

{** Read sub-channel return ISRC data format **}
Type T_read_subch_ISRC_data=Record
                                  Hdr              : T_read_subch_data_hdr;
                                  Data_format_code : Byte;
                                  TrackNo          : Byte;
                                  Reserved1        : Byte;
                                  Reserved2        : Byte;
                                  ISRC             : T_read_subch_ISRC_format;
                            End;

{** Read TOC/PMA/ATIP return data hdr format **}
Type T_read_T_P_A_data_hdr=Record
                                 Data_len_HiByte : Byte;
                                 Data_len_LoByte : Byte;
                                 First_trk_sess  : Byte;
                                 Last_Trk_sess   : Byte;
                           End;

{** Read TOC/PMA/ATIP return TOC track descriptor data format **}
Type T_read_T_P_A_TOC_trk_desc_data=Record
                                          Reserved1    : Byte;
                                          ADR_Control  : Byte;
                                          TrackNo      : Byte;
                                          Reserved2    : Byte;
                                          Adr_HiByte   : Byte;
                                          Adr_HiMiByte : Byte;
                                          Adr_LoMiByte : Byte;
                                          Adr_LoByte   : Byte;
                                    End;

{** Read TOC/PMA/ATIP return TOC data format **}
Type T_read_T_P_A_TOC_data=Record
                                 Hdr          : T_read_T_P_A_data_hdr;
                                 TOC_trk_desc : T_read_T_P_A_TOC_trk_desc_data;
                           End;

{** Read TOC/PMA/ATIP return session info descriptor data format **}
Type T_read_T_P_A_sess_info_desc_data=Record
                                            Reserved1    : Byte;
                                            ADR_Control  : Byte;
                                            FirstTrackNo : Byte;
                                            Reserved2    : Byte;
                                            Adr_HiByte   : Byte;
                                            Adr_HiMiByte : Byte;
                                            Adr_LoMiByte : Byte;
                                            Adr_LoByte   : Byte;
                                      End;

{** Read TOC/PMA/ATIP return session info data format **}
Type T_read_T_P_A_sess_info_data=Record
                                       Hdr            : T_read_T_P_A_data_hdr;
                                       Sess_info_desc : T_read_T_P_A_sess_info_desc_data;
                                 End;

{** Read TOC/PMA/ATIP return full TOC track descriptor data format **}
Type T_read_T_P_A_full_TOC_trk_desc_data=Record
                                               SessionNo   : Byte;
                                               ADR_Control : Byte;
                                               TNO         : Byte;
                                               POINT       : Byte;
                                               Min         : Byte;
                                               Sec         : Byte;
                                               Frame       : Byte;
                                               Zero        : Byte;
                                               PMin        : Byte;
                                               PSec        : Byte;
                                               PFrame      : Byte;
                                         End;

{** Read TOC/PMA/ATIP return full TOC data format **}
Type T_read_T_P_A_full_TOC_data=Record
                                      Hdr               : T_read_T_P_A_data_hdr;
                                      Full_TOC_trk_desc : T_read_T_P_A_full_TOC_trk_desc_data
                                End;

{** Read TOC/PMA/ATIP return PMA descriptor data format **}
Type T_read_T_P_A_PMA_desc_data=Record
                                      Reserved1   : Byte;
                                      ADR_Control : Byte;
                                      TNO         : Byte;
                                      POINT       : Byte;
                                      Min         : Byte;
                                      Sec         : Byte;
                                      Frame       : Byte;
                                      Zero        : Byte;
                                      PMin        : Byte;
                                      PSec        : Byte;
                                      PFrame      : Byte;
                                End;

{** Read TOC/PMA/ATIP return PMA data format **}
Type T_read_T_P_A_PMA_data=Record
                                 Hdr      : T_read_T_P_A_data_hdr;
                                 PMA_desc : T_read_T_P_A_PMA_desc_data;
                           End;

{** Read TOC/PMA/ATIP return ATIP descriptor data format **}
Type T_read_T_P_A_ATIP_desc_data=Record
                                       WritePower_RecSpeed   : Byte;
                                       URU                   : Byte;
                                       DType_DSubType_A1A2A3 : Byte;
                                       Reserved1             : Byte;
                                       StartMin              : Byte;
                                       StartSec              : Byte;
                                       StartFrame            : Byte;
                                       Reserved2             : Byte;
                                       LastMin               : Byte;
                                       LastSec               : Byte;
                                       LastFrame             : Byte;
                                       Reserved3             : Byte;
                                       CLV_rec_speeds        : Byte;
                                       PwrFactorFunc         : Byte;
                                       EraseWritePwrRatio    : Byte;
                                       Reserved4             : Byte;
                                       A2_values             : T_array3;
                                       Reserved5             : Byte;
                                       A3_values             : T_array3;
                                       Reserved6             : Byte;
                                 End;

{** Read TOC/PMA/ATIP return ATIP data format **}
Type T_read_T_P_A_ATIP_data=Record
                                  Hdr       : T_read_T_P_A_data_hdr;
                                  ATIP_desc : T_read_T_P_A_ATIP_desc_data;
                            End;

{** Read header return MMCLBA data format **}
Type T_read_header_MMCLBA_data=Record
                                  CDDataMode      : Byte;
                                  Reserved1       : Byte;
                                  Reserved2       : Byte;
                                  Reserved3       : Byte;
                                  MMCLBA_HiByte   : Byte;
                                  MMCLBA_HiMiByte : Byte;
                                  MMCLBA_LoMiByte : Byte;
                                  MMCLBA_LoByte   : Byte;
                            End;

{** Read header return MSF data format **}
Type T_read_header_MSF_data=Record
                                  CDDataMode : Byte;
                                  Reserved1  : Byte;
                                  Reserved2  : Byte;
                                  Reserved3  : Byte;
                                  Reserved4  : Byte;
                                  Min        : Byte;
                                  Sec        : Byte;
                                  Frame      : Byte;
                            End;

{** Sub Q return format used by read CD commands **}
Type T_Q_format=Record
                      ControlAdr   : Byte;
                      TrackNo      : Byte;
                      IndexNo      : Byte;
                      Min          : Byte;
                      Sec          : Byte;
                      Frame        : Byte;
                      Zero         : Byte;
                      AMin         : Byte;
                      ASec         : Byte;
                      AFrame       : Byte;
                      CRC_or_pad1  : Byte;
                      CRC_or_pad2  : Byte;
                      Pad3         : Byte;
                      Pad4         : Byte;
                      Pad5         : Byte;
                      PSub_or_pad6 : Byte;
                End;

{** Mode parameter CD medium type codes **}
Const MMC_MODE_PARAM_CDM_DEFAULT  =$00;
Const MMC_MODE_PARAM_CDM_120_DATA =$01;
Const MMC_MODE_PARAM_CDM_120_AUDIO=$02;
Const MMC_MODE_PARAM_CDM_120_MIXED=$03;
Const MMC_MODE_PARAM_CDM_80_DATA  =$05;
Const MMC_MODE_PARAM_CDM_80_AUDIO =$06;
Const MMC_MODE_PARAM_CDM_80_MIXED =$07;

{** Mode page codes **}
Const MMC_MODE_PAGE_READ_ERR_REC  =$01;
Const MMC_MODE_PAGE_WRITE_PARAM   =$05;
Const MMC_MODE_PAGE_VERI_ERR_REC  =$07;
Const MMC_MODE_PAGE_CD            =$0D;
Const MMC_MODE_PAGE_CD_AUDIO_CTRL =$0E;
Const MMC_MODE_PAGE_CD_CAP_MECH_ST=$2A;

{** Read/verify error recovery parameter constants **}
{** Abbreviations:
      CIRC    =use CIRC for correcting errors (low level, applicable to all
                                               CD format modes)
      ECC     =use ECC for correcting errors (higher level, only applicable to
                                              certain modes.
      STOPRECV=stop on recoverable error (stops burst mode reading)
      REP     =report recoverable error
      TEC     =Transfer Error blocks and Continue reading (does not stop burst mode reading)
      TES     =Transfer Error block and Stop reading (stops burst mode reading)
    Please note:
      If TEC or TES option is selected then bad sectors are
      still transferred, otherwise the drive doesn't return any data for
      bad sectors encountered, unless they are correctable by the options
      selected: CIRC, ECC.  The CIRC does not have the option to be turned off.

      Burst mode reading means when we request the hardware to read more than 1
      sector at a time.
      **}

{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - do not report recovered errors
    - transfers recoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC                 =$00;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - do not report recovered errors
    - transfers recoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC                     =$01;
{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - report recovered errors
    - transfers recoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_REP             =$04;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - report recovered errors
    - transfers recoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_REP                 =$05;
{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - report recovered errors
    - do not transfer recoverable block
    - stops further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_REP_STOPRECV    =$06;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - report recovered errors
    - do not transfer recoverable block
    - stops further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_REP_STOPRECV        =$07;
{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - do not report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - continues further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TEC             =$10;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - do not report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - continues further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_TEC                 =$11;
{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - continues further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_REP_TEC         =$14;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - do not report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - continues further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_REP_TEC             =$15;
{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - do not report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TES             =$20;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - do not report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_TES                 =$21;
{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_REP_TES         =$24;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - report recovered errors
    - transfers recoverable block
    - transfers unrecoverable block
    - stops further reading if unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_REP_TES             =$25;
{** - use CIRC for correcting errors
    - use ECC for correcting errors
    - report recovered errors
    - transfer recoverable block
    - transfers unrecoverable block
    - stops further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_REP_STOPRECV_TES=$26;
{** - use CIRC for correcting errors
    - do not use ECC for correcting errors
    - report recovered errors
    - transfer recoverable block
    - transfers unrecoverable block
    - stops further reading if recoverable or unrecoverable error encountered **}
Const MMC_MODE_PARAM_READ_ERR_REC_CIRC_STOPRECV_REP_STOPRECV_TES    =$27;

{** Test write flag **}
Const MMC_MODE_PARAM_TESTWR_ON=$10; //Select test write.
{** Write parameter flags **}
{** Write type flags **}
Const MMC_MODE_PARAM_WRTYPE_PAO=$00; //Packet-at-once.
Const MMC_MODE_PARAM_WRTYPE_TAO=$01; //Track-at-once.
Const MMC_MODE_PARAM_WRTYPE_SAO=$02; //Session-at-once.
Const MMC_MODE_PARAM_WRTYPE_RAW=$03; //Also known as disk-at-once (DAO).
{** Multisession close flags **}
Const MMC_MODE_PARAM_MULTIS_NOB0    =$00; //Close curr session.
Const MMC_MODE_PARAM_MULTIS_B0FFFFFF=$40; //Close CD.
Const MMC_MODE_PARAM_MULTIS_B0NEXT  =$C0; //Close curr and open for next session.
{** Block type codes **}
Const MMC_MODE_PARAM_BTYPE_RAW_2352         =0;  //Raw sectors 2352.
Const MMC_MODE_PARAM_BTYPE_RAW_FORMPQ_2368  =1;  //Raw sectors 2352+formatted PQ sub-ch data.
Const MMC_MODE_PARAM_BTYPE_RAW_PACKPW_2448  =2;  //Raw sectors 2352+packed PW sub-ch data.
Const MMC_MODE_PARAM_BTYPE_RAW_RAWPW_2448   =3;  //Raw sectors 2352+raw PW sub-ch data.
Const MMC_MODE_PARAM_BTYPE_CDROM_F1_2048    =8;  //CDROM form1.
Const MMC_MODE_PARAM_BTYPE_CDROM_F2_2336    =9;  //CDROM form2.
Const MMC_MODE_PARAM_BTYPE_XA_F1_2048       =10; //CDROMXA form1.
Const MMC_MODE_PARAM_BTYPE_XA_F1_SUBHDR_2056=11; //CDROMXA form1+8 byte sub header.
Const MMC_MODE_PARAM_BTYPE_XA_F2_2324       =12; //CDROMXA form2.
Const MMC_MODE_PARAM_BTYPE_XA_F2_SUBHDR_2332=13; //CDROMXA form2+8 byte sub header.

{** Inactivity timer multiplier constants **}
Const MMC_MODE_PARAM_CD_TIMER_VENDOR=$00;
Const MMC_MODE_PARAM_CD_TIMER_125MS =$01;
Const MMC_MODE_PARAM_CD_TIMER_250MS =$02;
Const MMC_MODE_PARAM_CD_TIMER_500MS =$03;
Const MMC_MODE_PARAM_CD_TIMER_1SEC  =$04;
Const MMC_MODE_PARAM_CD_TIMER_2SEC  =$05;
Const MMC_MODE_PARAM_CD_TIMER_4SEC  =$06;
Const MMC_MODE_PARAM_CD_TIMER_8SEC  =$07;
Const MMC_MODE_PARAM_CD_TIMER_16SEC =$08;
Const MMC_MODE_PARAM_CD_TIMER_32SEC =$09;
Const MMC_MODE_PARAM_CD_TIMER_1MIN  =$0A;
Const MMC_MODE_PARAM_CD_TIMER_2MIN  =$0B;
Const MMC_MODE_PARAM_CD_TIMER_4MIN  =$0C;
Const MMC_MODE_PARAM_CD_TIMER_8MIN  =$0D;
Const MMC_MODE_PARAM_CD_TIMER_16MIN =$0E;
Const MMC_MODE_PARAM_CD_TIMER_32MIN =$0F;

{** Loading mechanism types for CD capabilities and mechanism status parameters **}
Const MMC_MODE_PARAM_LOADMECHT_CADDY       =$00;
Const MMC_MODE_PARAM_LOADMECHT_TRAY        =$20;
Const MMC_MODE_PARAM_LOADMECHT_POPUP       =$40;
Const MMC_MODE_PARAM_LOADMECHT_CHANGER_INDI=$80;
Const MMC_MODE_PARAM_LOADMECHT_CHANGER_CART=$A0;
{** Data rates for CD capabilities and mechanism status parameters **}
Const MMC_MODE_PARAM_DATARATE_1X  =176;
Const MMC_MODE_PARAM_DATARATE_2X  =353;
Const MMC_MODE_PARAM_DATARATE_2_2X=387;
Const MMC_MODE_PARAM_DATARATE_3X  =528;
Const MMC_MODE_PARAM_DATARATE_4X  =706;
Const MMC_MODE_PARAM_DATARATE_6X  =1059;
Const MMC_MODE_PARAM_DATARATE_8X  =1412;
Const MMC_MODE_PARAM_DATARATE_10X =1765;
Const MMC_MODE_PARAM_DATARATE_16X =2824;

{** Mode parameter page for read error recovery parameters **}
Type T_mode_pg_read_err_rec=Record
                                  PS_PageCode      : Byte;
                                  Param_len        : Byte;
                                  Err_rec_param    : Byte;
                                  Read_retry_count : Byte;
                                  Reserved1        : Byte;
                                  Reserved2        : Byte;
                                  Reserved3        : Byte;
                                  Reserved4        : Byte;
                            End;

{** Mode parameter return/send data for read error recovery parameters.
    This version is for mode select/sense CDB 6 and includes a block descriptor. **}
Type T_mode_param_6_read_err_rec_blk_data=Record
                                                Hdr          : T_mode_param_hdr_6;
                                                Block_desc   : T_mode_param_block_desc;
                                                Read_err_rec : T_mode_pg_read_err_rec;
                                          End;

{** Mode parameter return/send data for read error recovery parameters.
    This version is for mode select/sense CDB 6 and does not include block descriptor. **}
Type T_mode_param_6_read_err_rec_noblk_data=Record
                                                  Hdr          : T_mode_param_hdr_6;
                                                  Read_err_rec : T_mode_pg_read_err_rec;
                                            End;

{** Mode parameter return/send data for read error recovery parameters.
    This version is for mode select/sense CDB 10 and includes a block descriptor. **}
Type T_mode_param_10_read_err_rec_blk_data=Record
                                                 Hdr          : T_mode_param_hdr_10;
                                                 Block_desc   : T_mode_param_block_desc;
                                                 Read_err_rec : T_mode_pg_read_err_rec;
                                           End;

{** Mode parameter return/send data for read error recovery parameters.
    This version is for mode select/sense CDB 10 and does not include block descriptor. **}
Type T_mode_param_10_read_err_rec_noblk_data=Record
                                                   Hdr          : T_mode_param_hdr_10;
                                                   Read_err_rec : T_mode_pg_read_err_rec;
                                             End;

{** Mode parameter page for write parameters **}
T_mode_pg_write=Record
                      PS_PageCode            : Byte;
                      Param_len              : Byte;
                      TestWR_WRType          : Byte;
                      MultiS_FP_Copy_TrkMode : Byte;
                      DataBlockType          : Byte;
                      Reserved1              : Byte;
                      Reserved2              : Byte;
                      HostAppCode            : Byte;
                      SessFormat             : Byte;
                      Reserved3              : Byte;
                      PacketSize_MSB         : Byte;
                      PacketSize_HiMiByte    : Byte;
                      PacketSize_LoMiByte    : Byte;
                      PacketSize_LSB         : Byte;
                      AudioPauseLen_MSB      : Byte;
                      AudioPauseLen_LSB      : Byte;
                      MCN                    : Array[1..16] Of Char;
                      ISRC                   : Array[1..16] Of Char;
                      SubHdr0                : Byte;
                      SubHdr1                : Byte;
                      SubHdr2                : Byte;
                      SubHdr3                : Byte;
                      VendorSpec1            : Byte;
                      VendorSpec2            : Byte;
                      VendorSpec3            : Byte;
                      VendorSpec4            : Byte;
                End;

{** Mode parameter return/send data for write parameters.
    This version is for mode select/sense CDB 6 and includes a block descriptor. **}
Type T_mode_param_6_write_blk_data=Record
                                         Hdr        : T_mode_param_hdr_6;
                                         Block_desc : T_mode_param_block_desc;
                                         Write      : T_mode_pg_write;
                                   End;

{** Mode parameter return/send data for write parameters.
    This version is for mode select/sense CDB 6 and does not include block descriptor. **}
Type T_mode_param_6_write_noblk_data=Record
                                           Hdr   : T_mode_param_hdr_6;
                                           Write : T_mode_pg_write;
                                     End;

{** Mode parameter return/send data for write parameters.
    This version is for mode select/sense CDB 10 and includes a block descriptor. **}
Type T_mode_param_10_write_blk_data=Record
                                          Hdr        : T_mode_param_hdr_10;
                                          Block_desc : T_mode_param_block_desc;
                                          Write      : T_mode_pg_write;
                                    End;

{** Mode parameter return/send data for write parameters.
    This version is for mode select/sense CDB 10 and does not include block descriptor. **}
Type T_mode_param_10_write_noblk_data=Record
                                            Hdr   : T_mode_param_hdr_10;
                                            Write : T_mode_pg_write;
                                      End;

{** Mode parameter page for verify error recovery parameters **}
Type T_mode_pg_veri_err_rec=Record
                                  PS_PageCode      : Byte;
                                  Param_len        : Byte;
                                  Err_rec_param    : Byte;
                                  Veri_retry_count : Byte;
                                  Reserved1        : Byte;
                                  Reserved2        : Byte;
                                  Reserved3        : Byte;
                                  Reserved4        : Byte;
                            End;

{** Mode parameter return/send data for verify error recovery parameters.
    This version is for mode select/sense CDB 6 and includes a block descriptor. **}
Type T_mode_param_6_veri_err_rec_blk_data=Record
                                                Hdr          : T_mode_param_hdr_6;
                                                Block_desc   : T_mode_param_block_desc;
                                                Veri_err_rec : T_mode_pg_veri_err_rec;
                                          End;

{** Mode parameter return/send data for verify error recovery parameters.
    This version is for mode select/sense CDB 6 and does not include block descriptor. **}
Type T_mode_param_6_veri_err_rec_noblk_data=Record
                                                  Hdr          : T_mode_param_hdr_6;
                                                  Veri_err_rec : T_mode_pg_veri_err_rec;
                                            End;

{** Mode parameter return/send data for verify error recovery parameters.
    This version is for mode select/sense CDB 10 and includes a block descriptor. **}
Type T_mode_param_10_veri_err_rec_blk_data=Record
                                                 Hdr          : T_mode_param_hdr_10;
                                                 Block_desc   : T_mode_param_block_desc;
                                                 Veri_err_rec : T_mode_pg_veri_err_rec;
                                           End;

{** Mode parameter return/send data for verify error recovery parameters.
    This version is for mode select/sense CDB 10 and does not include block descriptor. **}
Type T_mode_param_10_veri_err_rec_noblk_data=Record
                                                   Hdr          : T_mode_param_hdr_10;
                                                   Veri_err_rec : T_mode_pg_veri_err_rec;
                                             End;

{** Mode parameter page for CD parameters **}
Type T_mode_pg_CD=Record
                        PS_PageCode        : Byte;
                        Param_len          : Byte;
                        Reserved1          : Byte;
                        Inactivity_mul     : Byte;
                        Secs_per_MSF_MSB   : Byte;
                        Secs_per_MSF_LSB   : Byte;
                        Frames_per_MSF_MSB : Byte;
                        Frames_per_MSF_LSB : Byte;
                  End;

{** Mode parameter return/send data for CD parameters.
    This version is for mode select/sense CDB 6 and includes a block descriptor. **}
Type T_mode_param_6_CD_blk_data=Record
                                      Hdr        : T_mode_param_hdr_6;
                                      Block_desc : T_mode_param_block_desc;
                                      CD         : T_mode_pg_CD;
                                End;

{** Mode parameter return/send data for CD parameters.
    This version is for mode select/sense CDB 6 and does not include block descriptor. **}
Type T_mode_param_6_CD_noblk_data=Record
                                        Hdr : T_mode_param_hdr_6;
                                        CD  : T_mode_pg_CD;
                                  End;

{** Mode parameter return/send data for CD parameters.
    This version is for mode select/sense CDB 10 and includes a block descriptor. **}
Type T_mode_param_10_CD_blk_data=Record
                                       Hdr        : T_mode_param_hdr_10;
                                       Block_desc : T_mode_param_block_desc;
                                       CD         : T_mode_pg_CD;
                                 End;

{** Mode parameter return/send data for CD parameters.
    This version is for mode select/sense CDB 10 and does not include block descriptor. **}
Type T_mode_param_10_CD_noblk_data=Record
                                         Hdr : T_mode_param_hdr_10;
                                         CD  : T_mode_pg_CD;
                                   End;

{** Mode parameter page for CD audio control parameters **}
Type T_mode_pg_CD_audio_ctrl=Record
                                   PS_PageCode : Byte;
                                   Param_len   : Byte;
                                   Immed_SOTC  : Byte;
                                   Reserved1   : Byte;
                                   Reserved2   : Byte;
                                   Undefined1  : Byte;
                                   Undefined2  : Byte;
                                   Undefined3  : Byte;
                                   Port0_sel   : Byte;
                                   Port0_vol   : Byte;
                                   Port1_sel   : Byte;
                                   Port1_vol   : Byte;
                                   Port2_sel   : Byte;
                                   Port2_vol   : Byte;
                                   Port3_sel   : Byte;
                                   Port3_vol   : Byte;
                             End;

{** Mode parameter return/send data for CD audio control parameters.
    This version is for mode select/sense CDB 6 and includes a block descriptor. **}
Type T_mode_param_6_CD_audio_ctrl_blk_data=Record
                                                 Hdr           : T_mode_param_hdr_6;
                                                 Block_desc    : T_mode_param_block_desc;
                                                 CD_audio_ctrl : T_mode_pg_CD_audio_ctrl;
                                           End;

{** Mode parameter return/send data for CD audio control parameters.
    This version is for mode select/sense CDB 6 and does not include block descriptor. **}
Type T_mode_param_6_CD_audio_ctrl_noblk_data=Record
                                                   Hdr           : T_mode_param_hdr_6;
                                                   CD_audio_ctrl : T_mode_pg_CD_audio_ctrl;
                                             End;

{** Mode parameter return/send data for CD audio control parameters.
    This version is for mode select/sense CDB 10 and includes a block descriptor. **}
Type T_mode_param_10_CD_audio_ctrl_blk_data=Record
                                                  Hdr           : T_mode_param_hdr_10;
                                                  Block_desc    : T_mode_param_block_desc;
                                                  CD_audio_ctrl : T_mode_pg_CD_audio_ctrl;
                                            End;

{** Mode parameter return/send data for CD audio control parameters.
    This version is for mode select/sense CDB 10 and does not nclude block descriptor. **}
Type T_mode_param_10_CD_audio_ctrl_noblk_data=Record
                                                    Hdr           : T_mode_param_hdr_10;
                                                    CD_audio_ctrl : T_mode_pg_CD_audio_ctrl;
                                              End;

{** Mode parameter page for CD capabilities and mechanism status parameters **}
Type T_mode_pg_CD_cap_mech_st=Record
                                    PS_PageCode                                    : Byte;
                                    Param_len                                      : Byte;
                                    Meth2_RDCDRW_RDCDRR                            : Byte;
                                    TestWR_WRCDRW_WRCDR                            : Byte;
                                    MultiS_M2F2_M2F1_P2_P1_Comp_AudPly             : Byte;
                                    BarC_UPC_ISRC_C2_RWPack_RWRaw_GoodCDDA_CDDASup : Byte;
                                    LoadMechT_Eject_PreJmp_LockSt_Lock             : Byte;
                                    SSS_DiskIn_SepChMute_SepVol                    : Byte;
                                    MaxReadSpeed_MSB                               : Byte;
                                    MaxReadSpeed_LSB                               : Byte;
                                    NVolLevels_MSB                                 : Byte;
                                    NVolLevels_LSB                                 : Byte;
                                    BufSize_MSB                                    : Byte;
                                    BufSize_LSB                                    : Byte;
                                    CurrReadSpeed_MSB                              : Byte;
                                    CurrReadSpeed_LSB                              : Byte;
                                    Reserved1                                      : Byte;
                                    Len_LSBF_RCK_BCK                               : Byte;
                                    MaxWriteSpeed_MSB                              : Byte;
                                    MaxWriteSpeed_LSB                              : Byte;
                                    CurrWriteSpeed_MSB                             : Byte;
                                    CurrWriteSpeed_LSB                             : Byte;
                              End;

{** Mode parameter return/send data for CD capabilites and mechanism status parameters.
    This version is for mode select/sense CDB 6 and includes a block descriptor. **}
Type T_mode_param_6_CD_cap_mech_st_blk_data=Record
                                                  Hdr            : T_mode_param_hdr_6;
                                                  Block_desc     : T_mode_param_block_desc;
                                                  CD_cap_mech_st : T_mode_pg_CD_cap_mech_st;
                                            End;

{** Mode parameter return/send data for CD capabilites and mechanism status parameters.
    This version is for mode select/sense CDB 6 and does not include a block descriptor. **}
Type T_mode_param_6_CD_cap_mech_st_noblk_data=Record
                                                    Hdr            : T_mode_param_hdr_6;
                                                    CD_cap_mech_st : T_mode_pg_CD_cap_mech_st;
                                              End;

{** Mode parameter return/send data for CD capabilities and mechanism status parameters.
    This version is for mode select/sense CDB 10 and includes a block descriptor. **}
Type T_mode_param_10_CD_cap_mech_st_blk_data=Record
                                                   Hdr            : T_mode_param_hdr_10;
                                                   Block_desc     : T_mode_param_block_desc;
                                                   CD_cap_mech_st : T_mode_pg_CD_cap_mech_st;
                                             End;

{** Mode parameter return/send data for CD capabilities and mechanism status parameters.
    This version is for mode select/sense CDB 10 and does not include a block descriptor. **}
Type T_mode_param_10_CD_cap_mech_st_noblk_data=Record
                                                     Hdr            : T_mode_param_hdr_10;
                                                     CD_cap_mech_st : T_mode_pg_CD_cap_mech_st;
                                               End;

{** Mode parameter list with only header + 2 bytes of mode parameters, i.e.
PS_PageCode and Param_len.  This version is for mode select/sense CDB 6 and
includes a block descriptor. **}
Type T_mode_param_6_hdr_blk_2_data=Record
                                         Hdr         : T_mode_param_hdr_6;
                                         Block_desc  : T_mode_param_block_desc;
                                         PS_PageCode : Byte;
                                         Param_len   : Byte;
                                   End;

{** Mode parameter list with only header + 2 bytes of mode parameters, i.e.
PS_PageCode and Param_len.  This version is for mode select/sense CDB 6 and does not
include a block descriptor. **}
Type T_mode_param_6_hdr_noblk_2_data=Record
                                           Hdr            : T_mode_param_hdr_6;
                                           PS_PageCode    : Byte;
                                           Param_len      : Byte;
                                     End;

{** Mode parameter list with only header + 2 bytes of mode parameters, i.e.
PS_PageCode and Param_len.  This version is for mode select/sense CDB 10 and
includes a block descriptor. **}
Type T_mode_param_10_hdr_blk_2_data=Record
                                          Hdr         : T_mode_param_hdr_10;
                                          Block_desc  : T_mode_param_block_desc;
                                          PS_PageCode : Byte;
                                          Param_len   : Byte;
                                    End;

{** Mode parameter list with only header + 2 bytes of mode parameters, i.e.
PS_PageCode and Param_len.  This version is for mode select/sense CDB 10 and does not
include a block descriptor. **}
Type T_mode_param_10_hdr_noblk_2_data=Record
                                            Hdr         : T_mode_param_hdr_10;
                                            PS_PageCode : Byte;
                                            Param_len   : Byte;
                                      End;
Type T_out_read_T_P_A_TOC_desc_MMCLBA=Record
                                         ADR_Ctrl : Byte;
                                         TrackNo  : Byte;
                                         StartMMCLBA : LongInt;
                                   End;
Type T_out_read_T_P_A_TOC_desc_MSF=Record
                                         ADR_Ctrl : Byte;
                                         TrackNo  : Byte;
                                         StartM   : Byte;
                                         StartS   : Byte;
                                         StartF   : Byte;
                                   End;
Type T_out_read_T_P_A_sess_desc_MMCLBA=Record
                                          ADR_Ctrl              : Byte;
                                          FirstTrkNo_InLastSess : Byte;
                                          StartMMCLBA           : LongInt;
                                    End;
Type T_out_read_T_P_A_sess_desc_MSF=Record
                                          ADR_Ctrl              : Byte;
                                          FirstTrkNo_InLastSess : Byte;
                                          StartM                : Byte;
                                          StartS                : Byte;
                                          StartF                : Byte;
                                    End;
Type T_out_read_T_P_A_PMA_desc=Record
                                     ADR_Ctrl : Byte;
                                     TNO      : Byte;
                                     POINT    : Byte;
                                     Min      : Byte;
                                     Sec      : Byte;
                                     Frame    : Byte;
                                     Zero     : Byte;
                                     PMin     : Byte;
                                     PSec     : Byte;
                                     PFrame   : Byte;
                               End;
Type T_out_read_T_P_A_full_TOC_desc=Record
                                          SessionNo : Byte;
                                          ADR_Ctrl  : Byte;
                                          TNO       : Byte;
                                          POINT     : Byte;
                                          Min       : Byte;
                                          Sec       : Byte;
                                          Frame     : Byte;
                                          Zero      : Byte;
                                          PMin      : Byte;
                                          PSec      : Byte;
                                          PFrame    : Byte;
                                    End;
Type T_out_read_T_P_A_ATIP_desc=Record
                                      WritePower_RecSpeed     : Byte;
                                      URU                     : Byte;
                                      DType_DSubType_A1A2A3   : Byte;
                                      StartMin                : Byte;
                                      StartSec                : Byte;
                                      StartFrame              : Byte;
                                      LastMin                 : Byte;
                                      LastSec                 : Byte;
                                      LastFrame               : Byte;
                                      CLV_rec_speeds          : Byte;
                                      Power_fx_values         : Byte;
                                      Erase_write_power_ratio : Byte;
                                      A2_byte1                : Byte;
                                      A2_byte2                : Byte;
                                      A2_byte3                : Byte;
                                      A3_byte1                : Byte;
                                      A3_byte2                : Byte;
                                      A3_byte3                : Byte;
                                End;
{** The T_pub_xx types are for the friendly versions of the MMC commands.
These are custom made for passing reference variables back from procedures. **}
Type T_pub_write_params=Record
                              TestWR_WRType          : Byte;
                              MultiS_FP_Copy_TrkMode : Byte;
                              DataBlockType          : Byte;
                              HostAppCode            : Byte;
                              SessFormat             : Byte;
                              PacketSize             : LongWord;
                              AudioPauseLen          : LongWord;
                              MCN                    : Array[1..16] Of Char;
                              ISRC                   : Array[1..16] Of Char;
                        End;
Type T_pub_CD_audio_ctrl=Record
                               Immed_SOTC : Byte;
                               Port0_sel  : Byte;
                               Port0_vol  : Byte;
                               Port1_sel  : Byte;
                               Port1_vol  : Byte;
                               Port2_sel  : Byte;
                               Port2_vol  : Byte;
                               Port3_sel  : Byte;
                               Port3_vol  : Byte;
                         End;
Type T_pub_CD_cap_mech_st=Record
                                Param_len                                      : Byte;
                                Meth2_RDCDRW_RDCDRR                            : Byte;
                                TestWR_WRCDRW_WRCDR                            : Byte;
                                MultiS_M2F2_M2F1_P2_P1_Comp_AudPly             : Byte;
                                BarC_UPC_ISRC_C2_RWPack_RWRaw_GoodCDDA_CDDASup : Byte;
                                LoadMechT_Eject_PreJmp_LockSt_Lock             : Byte;
                                SSS_DiskIn_SepChMute_SepVol                    : Byte;
                                MaxReadSpeed                                   : Word;
                                NVolLevels                                     : Word;
                                BufSize                                        : Word;
                                CurrReadSpeed                                  : Word;
                                Len_LSBF_RCK_BCK                               : Byte;
                                MaxWriteSpeed                                  : Word;
                                CurrWriteSpeed                                 : Word;
                          End;
Type T_read_buf_cap_CDB10=Record
                                Cmd             : Byte;
                                Reserved1       : Byte;
                                Reserved2       : Byte;
                                Reserved3       : Byte;
                                Reserved4       : Byte;
                                Reserved5       : Byte;
                                Reserved6       : Byte;
                                AllocLen_HiByte : Byte;
                                AllocLen_LoByte : Byte;
                                Control         : Byte;
                          End;

Type T_read_buf_cap_CDB10_block=Record
                                      DataLen_HiByte        : Byte;
                                      DataLen_LoByte        : Byte;
                                      Reserved1             : Byte;
                                      Reserved2             : Byte;
                                      BufLen_HiByte         : Byte;
                                      BufLen_HiMiByte       : Byte;
                                      BufLen_LoMiByte       : Byte;
                                      BufLen_LoByte         : Byte;
                                      Blank_BufLen_HiByte   : Byte;
                                      Blank_BufLen_HiMiByte : Byte;
                                      Blank_BufLen_LoMiByte : Byte;
                                      Blank_BufLen_LoByte   : Byte;
                                End;
Type T_read_disc_info_CDB10=Record
                                  Cmd             : Byte;
                                  Reserved1       : Byte;
                                  Reserved2       : Byte;
                                  Reserved3       : Byte;
                                  Reserved4       : Byte;
                                  Reserved5       : Byte;
                                  Reserved6       : Byte;
                                  AllocLen_HiByte : Byte;
                                  AllocLen_LoByte : Byte;
                                  Control         : Byte;
                            End;
Type T_read_disc_info_CDB10_block=Record
                                        DataLen_HiByte                       : Byte;
                                        DataLen_LoByte                       : Byte;
                                        Erasable_StateOfLastSess_DiscStatus  : Byte;
                                        No_of_first_trk                      : Byte;
                                        No_of_sess                           : Byte;
                                        First_trk_no_in_last_sess            : Byte;
                                        Last_trk_no_in_last_sess             : Byte;
                                        DID_V_DBC_V_URU                      : Byte;
                                        DiscType                             : Byte;
                                        Reserved1                            : Byte;
                                        Reserved2                            : Byte;
                                        Reserved3                            : Byte;
                                        DiscID_HiByte                        : Byte;
                                        DiscID_HiMiByte                      : Byte;
                                        DiscID_LoMiByte                      : Byte;
                                        DiscID_LoByte                        : Byte;
                                        Last_sess_lead_in_start_MSF_HiByte   : Byte;
                                        Last_sess_lead_in_start_MSF_HiMiByte : Byte;
                                        Last_sess_lead_in_start_MSF_LoMiByte : Byte;
                                        Last_sess_lead_in_start_MSF_LoByte   : Byte;
                                        Last_start_for_lead_out_MSF_HiByte   : Byte;
                                        Last_start_for_lead_out_MSF_HiMiByte : Byte;
                                        Last_start_for_lead_out_MSF_LoMiByte : Byte;
                                        Last_start_for_lead_out_MSF_LoByte   : Byte;
                                        DiscBarCode_Byte7                    : Byte;
                                        DiscBarCode_Byte6                    : Byte;
                                        DiscBarCode_Byte5                    : Byte;
                                        DiscBarCode_Byte4                    : Byte;
                                        DiscBarCode_Byte3                    : Byte;
                                        DiscBarCode_Byte2                    : Byte;
                                        DiscBarCode_Byte1                    : Byte;
                                        DiscBarCode_Byte0                    : Byte;
                                        Reserved4                            : Byte;
                                        No_of_OPC_tab_entries                : Byte;
                                  End;
Type T_read_disc_info_CDB10_OPC_table=Record
                                            Speed_kBytes_MSB : Byte;
                                            Speed_kBytes_LSB : Byte;
                                            OPC_value_Byte5  : Byte;
                                            OPC_value_Byte4  : Byte;
                                            OPC_value_Byte3  : Byte;
                                            OPC_value_Byte2  : Byte;
                                            OPC_value_Byte1  : Byte;
                                            OPC_value_Byte0  : Byte;
                                      End;
Type T_P_read_disc_info_CDB10_OPC_table=^T_read_disc_info_CDB10_OPC_table;
Type T_read_track_info_CDB10=Record
                                   Cmd                   : Byte;
                                   TrkBIT                : Byte;
                                   MMCLBA_TrkNo_HiByte   : Byte;
                                   MMCLBA_TrkNo_HiMiByte : Byte;
                                   MMCLBA_TrkNo_LoMiByte : Byte;
                                   MMCLBA_TrkNo_LoByte   : Byte;
                                   Reserved              : Byte;
                                   AllocLen_HiByte       : Byte;
                                   AllocLen_LoByte       : Byte;
                                   Control               : Byte;
                             End;
Type T_reserve_track_CDB10=Record
                                 Cmd                   : Byte;
                                 Reserve1              : Byte;
                                 Reserve2              : Byte;
                                 Reserve3              : Byte;
                                 Reserve4              : Byte;
                                 Reserve_size_HiByte   : Byte;
                                 Reserve_size_HiMiByte : Byte;
                                 Reserve_size_LoMiByte : Byte;
                                 Reserve_size_LoByte   : Byte;
                                 Control               : Byte;
                           End;
Type T_read_track_info_CDB10_block=Record
                                         DataLen_HiByte              : Byte;
                                         DataLen_LoByte              : Byte;
                                         TrkNo                       : Byte;
                                         SessNo                      : Byte;
                                         Reserved                    : Byte;
                                         Damage_Copy_TrkMode         : Byte;
                                         RT_Blank_Packet_FP_DataMode : Byte;
                                         NWA_V                       : Byte;
                                         TrkStart_MMCLBA_HiByte      : Byte;
                                         TrkStart_MMCLBA_HiMiByte    : Byte;
                                         TrkStart_MMCLBA_LoMiByte    : Byte;
                                         TrkStart_MMCLBA_LoByte      : Byte;
                                         NextWr_MMCLBA_HiByte        : Byte;
                                         NextWr_MMCLBA_HiMiByte      : Byte;
                                         NextWr_MMCLBA_LoMiByte      : Byte;
                                         NextWr_MMCLBA_LoByte        : Byte;
                                         FreeBlocks_HiByte           : Byte;
                                         FreeBlocks_HiMiByte         : Byte;
                                         FreeBlocks_LoMiByte         : Byte;
                                         FreeBlocks_LoByte           : Byte;
                                         FixPackSize_HiByte          : Byte;
                                         FixPackSize_HiMiByte        : Byte;
                                         FixPackSize_LoMiByte        : Byte;
                                         FixPackSize_LoByte          : Byte;
                                         TrkSize_HiByte              : Byte;
                                         TrkSize_HiMiByte            : Byte;
                                         TrkSize_LoMiByte            : Byte;
                                         TrkSize_LoByte              : Byte;
                                   End;
Type T_synchronize_cache_CDB10=Record
                                     Cmd             : Byte;
                                     Immed_RELADR    : Byte;
                                     MMCLBA_HiByte   : Byte;
                                     MMCLBA_HiMiByte : Byte;
                                     MMCLBA_LoMiByte : Byte;
                                     MMCLBA_LoByte   : Byte;
                                     Reserved        : Byte;
                                     Len_HiByte      : Byte;
                                     Len_LoByte      : Byte;
                                     Control         : Byte;
                               End;

Const MMC_WRITE_CDB10_DPO_FUA=$18;
Type T_write_CDB10=Record
                         Cmd : Byte;
                         DPO_FUA_RELADR : Byte;
                         MMCLBA_HiByte   : Byte;
                         MMCLBA_HiMiByte : Byte;
                         MMCLBA_LoMiByte : Byte;
                         MMCLBA_LoByte   : Byte;
                         Reserved        : Byte;
                         Len_HiByte      : Byte;
                         Len_LoByte      : Byte;
                         Control         : Byte;
                   End;
Type T_out_read_disc_info_CDB10_block=Record
                                            DataLen                             : Word;
                                            Erasable_StateOfLastSess_DiscStatus : Byte;
                                            No_of_first_trk                     : Byte;
                                            No_of_sess                          : Byte;
                                            First_trk_no_in_last_sess           : Byte;
                                            Last_trk_no_in_last_sess            : Byte;
                                            DID_V_DBC_V_URU                     : Byte;
                                            DiscType                            : Byte;
                                            Reserved1                           : Byte;
                                            Reserved2                           : Byte;
                                            Reserved3                           : Byte;
                                            DiscID                              : LongInt;
                                            Last_sess_lead_in_start_MSF         : T_MMC_MSF;
                                            Last_start_for_lead_out_MSF         : T_MMC_MSF;
                                            DiscBarCode_Byte0                   : Byte;
                                            DiscBarCode_Byte1                   : Byte;
                                            DiscBarCode_Byte2                   : Byte;
                                            DiscBarCode_Byte3                   : Byte;
                                            DiscBarCode_Byte4                   : Byte;
                                            DiscBarCode_Byte5                   : Byte;
                                            DiscBarCode_Byte6                   : Byte;
                                            DiscBarCode_Byte7                   : Byte;
                                            Reserved4                           : Byte;
                                            No_of_OPC_tab_entries               : Byte;
                                  End;
Type T_out_read_disc_info_CDB10_OPC_table=Record
                                                Speed_kBytes     : Word;
                                                OPC_value_Byte0  : Byte;
                                                OPC_value_Byte1  : Byte;
                                                OPC_value_Byte2  : Byte;
                                                OPC_value_Byte3  : Byte;
                                                OPC_value_Byte4  : Byte;
                                                OPC_value_Byte5  : Byte;
                                      End;
Type T_out_read_track_info_CDB10_block=Record
                                         DataLen                     : Word;
                                         TrkNo                       : Byte;
                                         SessNo                      : Byte;
                                         Reserved                    : Byte;
                                         Damage_Copy_TrkMode         : Byte;
                                         RT_Blank_Packet_FP_DataMode : Byte;
                                         NWA_V                       : Byte;
                                         TrkStart_MMCLBA             : LongInt;
                                         NextWr_MMCLBA               : LongInt;
                                         FreeBlocks                  : LongInt;
                                         FixPackSize                 : LongInt;
                                         TrkSize                     : LongInt;
                                   End;
