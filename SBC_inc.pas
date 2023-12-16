{ Name:         SBC.PAS
  File type:    Borland Delphi 4 include file.
  Description:  Misc SCSI definitions written for Borland Delphi 4.
                This source file is based on information given from
                the Adaptec's ASPI SDK and working draft version of
                SCSI-3 specification documents:
                     - SBC (SCSI Block Commands)
  Notes:
  Date started: 2nd Jan 2003.
  Developer:    Truong Hy.
}
{** SCSI-3 block command codes **}
Const SBC_CMD_START_STOP_UNIT=$1B;
Const SBC_CMD_SEEK_CDB10     =$2B;

{** Start stop unit command constants **}
Const START_STOP_UNIT_IMMED                         =$01;
Const START_STOP_UNIT_START                         =$01;
Const START_STOP_UNIT_LOEJ                          =$02;
Const START_STOP_UNIT_POWER_NOCHG                   =$00;
Const START_STOP_UNIT_POWER_ACTIVE                  =$10;
Const START_STOP_UNIT_POWER_IDLE                    =$20;
Const START_STOP_UNIT_POWER_STANDBY                 =$30;
Const START_STOP_UNIT_POWER_SLEEP                   =$50;
Const START_STOP_UNIT_POWER_TR_CTL_TO_DEV           =$70;
Const START_STOP_UNIT_POWER_FORCE_IDLE_TIMER_ZERO   =$A0;
Const START_STOP_UNIT_POWER_FORCE_STANDBY_TIMER_ZERO=$B0;

{** CDB6 for start stop unit command **}
Type T_start_stop_unit_CDB6=Record
                                  Cmd                        : Byte;
                                  Reserved1_IMMED            : Byte;
                                  Reserved2                  : Byte;
                                  Reserved3                  : Byte;
                                  Power_Reserved4_LOEJ_Start : Byte;
                                  Control                    : Byte;
                            End;

Type T_seek_CDB10=Record
                        Cmd             : Byte;
                        Reserved1       : Byte;
                        MMCLBA_HiByte   : Byte;
                        MMCLBA_HiMiByte : Byte;
                        MMCLBA_LoMiByte : Byte;
                        MMCLBA_LoByte   : Byte;
                        Reserved2       : Byte;
                        Reserved3       : Byte;
                        Reserved4       : Byte;
                        Control         : Byte;
                  End;
