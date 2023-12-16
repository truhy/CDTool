{ SCSI commands wrapped in WinASPI32 SRB structure.. }
{** SRB start stop unit CDB6 command **}
Type T_SRB_for_start_stop_unit_CDB6=Record
                                          Exec_SCSI_hdr         : T_exec_SCSI_hdr;
                                          Start_stop_unit_CDB6  : T_start_stop_unit_CDB6;
                                          Align                 : Array[1..(16-SizeOf(T_start_stop_unit_CDB6))] Of Byte;
                                          Sense_area            : T_sense_len;
                                    End;

Type T_SRB_for_seek_CDB10=Record
                                Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                Seek_CDB10    : T_seek_CDB10;
                                Align         : Array[1..(16-SizeOf(T_seek_CDB10))] Of Byte;
                                Sense_area    : T_sense_len;
                          End;