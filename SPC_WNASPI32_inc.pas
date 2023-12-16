{ SCSI commands wrapped in WinASPI32 SRB structure.. }
{** SRB for test unit ready command **}
Type T_SRB_for_test_unit_ready=Record
                                     Exec_SCSI_hdr   : T_exec_SCSI_hdr;
                                     Test_unit_ready : T_test_unit_ready;
                                     Align           : Array[1..(16-SizeOf(T_test_unit_ready))] Of Byte;
                                     Sense_area      : T_sense_len;
                               End;

{** SRB for request sense command **}
Type T_SRB_for_req_sense=Record
                               Exec_SCSI_hdr : T_exec_SCSI_hdr;
                               Req_sense     : T_req_sense;
                               Align         : Array[1..(16-SizeOf(T_req_sense))] Of Byte;
                               Sense_area    : T_sense_len;
                         End;

{** SRB for device inquiry CDB6 command **}
Type T_SRB_for_dev_inq_CDB6=Record
                                  Exec_SCSI_hdr : T_exec_SCSI_hdr;
                                  Dev_inq_CDB6  : T_dev_inq_CDB6;
                                  Align         : Array[1..(16-SizeOf(T_dev_inq_CDB6))] Of Byte;
                                  Sense_area    : T_sense_len;
                            End;

{** SRB for mode select CDB6 command **}
Type T_SRB_for_mode_select_CDB6=Record
                                      Exec_SCSI_hdr    : T_exec_SCSI_hdr;
                                      Mode_select_CDB6 : T_mode_select_CDB6;
                                      Align            : Array[1..(16-SizeOf(T_mode_select_CDB6))] Of Byte;
                                      Sense_area       : T_sense_len;
                                End;

{** SRB for mode select CDB10 command **}
Type T_SRB_for_mode_select_CDB10=Record
                                       Exec_SCSI_hdr     : T_exec_SCSI_hdr;
                                       Mode_select_CDB10 : T_mode_select_CDB10;
                                       Align             : Array[1..(16-SizeOf(T_mode_select_CDB10))] Of Byte;
                                       Sense_area        : T_sense_len;
                                 End;

{** SRB for mode sense CDB6 command **}
Type T_SRB_for_mode_sense_CDB6=Record
                                      Exec_SCSI_hdr   : T_exec_SCSI_hdr;
                                      Mode_sense_CDB6 : T_mode_sense_CDB6;
                                      Align           : Array[1..(16-SizeOf(T_mode_sense_CDB6))] Of Byte;
                                      Sense_area      : T_sense_len;
                                End;

{** SRB for mode sense CDB10 command **}
Type T_SRB_for_mode_sense_CDB10=Record
                                      Exec_SCSI_hdr    : T_exec_SCSI_hdr;
                                      Mode_sense_CDB10 : T_mode_sense_CDB10;
                                      Align            : Array[1..(16-SizeOf(T_mode_sense_CDB10))] Of Byte;
                                      Sense_area       : T_sense_len;
                                End;