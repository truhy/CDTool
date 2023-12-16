{** SRB for read CD recorded capacity command (25h) **}
Type T_SRB_for_readCD_rec_cap=Record
                                    Exec_SCSI_hdr  : T_exec_SCSI_hdr;
                                    readCD_rec_cap : T_readCD_rec_cap;
                                    Align          : T_array6;
                                    Sense_area     : T_sense_len;
                              End;

{** SRB for read sub-channel command (42h) **}
Type T_SRB_for_read_subch=Record
                                Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                Read_subch    : T_read_subch;
                                Align         : T_array6;
                                Sense_area    : T_sense_len;
                          End;

{** SRB for read TOC/PMA/ATIP command (43h) **}
Type T_SRB_for_read_T_P_A=Record
                                Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                Read_T_P_A    : T_read_T_P_A;
                                Align         : T_array6;
                                Sense_area    : T_sense_len;
                          End;

{** SRB for read header command (44h)**}
Type T_SRB_for_read_header=Record
                                 Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                 Read_header   : T_read_header;
                                 Align         : T_array6;
                                 Sense_area    : T_sense_len;
                           End;

{** SRB for read CD MSF command (B9h) **}
Type T_SRB_for_readCD_MSF=Record
                                Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                ReadCD_MSF    : T_readCD_MSF;
                                Align         : T_array4;
                                Sense_area    : T_sense_len;
                          End;

{** SRB for set CD speed command (BBh) **}
Type T_SRB_for_set_CD_speed=Record
                                  Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                  Set_CD_speed  : T_set_CD_speed;
                                  Align         : T_array4;
                                  Sense_area    : T_sense_len;
                            End;

{** SRB for read CD CDB12 command (BEh) **}
Type T_SRB_for_readCD_CDB12=Record
                                  Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                  ReadCD_CDB12  : T_readCD_CDB12;
                                  Align         : T_array4;
                                  Sense_area    : T_sense_len;
                            End;

Type T_SRB_for_read_buf_cap_CDB10=Record
                                         Exec_SCSI_hdr      : T_exec_SCSI_hdr;
                                         Read_buf_cap_CDB10 : T_read_buf_cap_CDB10;
                                         Align              : T_array6;
                                         Sense_area         : T_sense_len;
                                  End;

Type T_SRB_for_read_disc_info_CDB10=Record
                                          Exec_SCSI_hdr        : T_exec_SCSI_hdr;
                                          Read_disc_info_CDB10 : T_read_disc_info_CDB10;
                                          Align                : T_array6;
                                          Sense_area           : T_sense_len;
                                    End;

Type T_SRB_for_read_track_info_CDB10=Record
                                           Exec_SCSI_hdr         : T_exec_SCSI_hdr;
                                           Read_track_info_CDB10 : T_read_track_info_CDB10;
                                           Align                 : T_array6;
                                           Sense_area            : T_sense_len;
                                     End;

Type T_SRB_for_reserve_track_CDB10=Record
                                         Exec_SCSI_hdr         : T_exec_SCSI_hdr;
                                         Reserve_track_CDB10   : T_reserve_track_CDB10;
                                         Align                 : T_array6;
                                         Sense_area            : T_sense_len;
                                   End;

Type T_SRB_for_synchronize_cache_CDB10=Record
                                             Exec_SCSI_hdr            : T_exec_SCSI_hdr;
                                             Synchronize_cache_CDB10  : T_synchronize_cache_CDB10;
                                             Align                    : T_array6;
                                             Sense_area               : T_sense_len;
                                       End;

Type T_SRB_for_write_CDB10=Record
                                 Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                 Write_CDB10   : T_write_CDB10;
                                 Align         : T_array6;
                                 Sense_area    : T_sense_len;
                           End;

{** SRB for play audio CDB10 command (45h) **}
Type T_SRB_for_play_audio_CDB10=Record
                                      Exec_SCSI_hdr    : T_exec_SCSI_hdr;
                                      Play_audio_CDB10 : T_play_audio_CDB10;
                                      Align            : T_array6;
                                      Sense_area       : T_sense_len;
                                End;

{** SRB for play audio MSF command (47h) **}
Type T_SRB_for_play_audio_MSF=Record
                                    Exec_SCSI_hdr  : T_exec_SCSI_hdr;
                                    Play_audio_MSF : T_play_audio_MSF;
                                    Align          : T_array6;
                                    Sense_area     : T_sense_len;
                              End;

{** SRB for play audio CDB12 command (A5h) **}
Type T_SRB_for_play_audio_CDB12=Record
                                      Exec_SCSI_hdr    : T_exec_SCSI_hdr;
                                      Play_audio_CDB12 : T_play_audio_CDB12;
                                      Align            : T_array4;
                                      Sense_area       : T_sense_len;
                                End;

{** SRB for pause/resume command (4Bh) **}
Type T_SRB_for_pause_resume=Record
                                  Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                  Pause_resume  : T_pause_resume;
                                  Align         : T_array6;
                                  Sense_area    : T_sense_len;
                            End;

{** SRB for load/unload cd command (A6h) **}
Type T_SRB_for_loadunload_CDB12=Record
                                      Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                      ReadCD_MSF    : T_loadunload_CDB12;
                                      Align         : T_array4;
                                      Sense_area    : T_sense_len;
                                End;

{** SRB for scan command (BAh) **}
Type T_SRB_for_scan=Record
                          Exec_SCSI_hdr : T_exec_SCSI_hdr;
                          Scan          : T_scan;
                          Align         : T_array4;
                          Sense_area    : T_sense_len;
                    End;

{** SRB for playcd command (BCh) **}
Type T_SRB_for_playCD=Record
                            Exec_SCSI_hdr : T_exec_SCSI_hdr;
                            PlayCD        : T_playCD;
                            Align         : T_array4;
                            Sense_area    : T_sense_len;
                      End;
