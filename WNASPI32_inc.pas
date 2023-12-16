{ Name:          WNASPI32.PAS
  File type:     Borland Delphi 4 include file.
  Description:   WIN ASPI32 definitions written for Borland Delphi 4.
                 This source file based on information given from
                 the Adaptec's ASPI SDK for Win32.
  Notes:         Written from scratch, no other samples used, except SDK.
  Date started:  14th Dec 1999.
  Date finished: 12th Jan 2000.
  File version:  v1 final (tested and working)
  Developer:     Truong Hy.
}

{** Sense data definitions **}
Const AS_SENSE_LEN=18; { Sense data max length }

{** ASPI command codes **}
Const AS_CMD_HA_INQUIRY     =$00; { SCSI host adapter inquiry }
Const AS_CMD_GET_DEV_TYPE   =$01; { Get SCSI target type }
Const AS_CMD_EXEC_SCSI_CMD  =$02; { Execute SCSI I/O }
Const AS_CMD_ABORT_SRB      =$03; { Abort SRB (SCSI Request Block }
Const AS_CMD_RESET_DEV      =$04; { Reset SCSI target }
//Const AS_CMD_SET_HA_PARMS   =$05; { Set SCSI host adapter parameters } //Obsolete for Win ASPI32
Const AS_CMD_GET_DRIVE_INFO =$06; { Get disk drive info }
Const AS_CMD_RESCAN_SCSI_BUS=$07; { Rebuild SCSI device map }
Const AS_CMD_GETSET_TIMEOUTS=$08; { Get/Set target timeouts }

{** Win ASPI32 SRB flag definitions **}
//Const SRB_DIR_SCSI    =$00; { Data direction determined by SCSI (obsolete for Win32) }
Const SRB_POSTING     =$01; { Enable posting }
Const SRB_LINKED      =$02; { Enable linking of SRBs }
Const SRB_RESIDUAL    =$04; { Enable residual byte count }
Const SRB_DIR_IN      =$08; { Data direction from SCSI device (target) to host }
Const SRB_DIR_OUT     =$10; { Data direction from host to SCSI device (target) }
Const SRB_NO_DATA     =$18; { No data transfer }
Const SRB_EVENT_NOTIFY=$40; { Enable ASPI event notification }

{** Win ASPI32 SRB status bytes **}
Const SRB_ST_PENDING               =$00; // SRB being processed
Const SRB_ST_COMP                  =$01; // SRB completed without error
Const SRB_ST_ABORTED               =$02; // SRB aborted
Const SRB_ST_ABORT_FAIL            =$03; // Unable to abort SRB
Const SRB_ST_ERR                   =$04; // SRB completed with error
Const SRB_ST_INVALID_CMD           =$80; // Invalid ASPI command
Const SRB_ST_INVALID_HA_ID         =$81; // Invalid host adapter number
Const SRB_ST_NO_DEV                =$82; // SCSI target device not installed
Const SRB_ST_INVALID_SRB           =$E0; // Invalid parameter set in SRB
Const SRB_ST_BUFFER_ALIGN          =$E1; // Buffer not aligned (replaces OLD_MANAGER)
Const SRB_ST_ILLEGAL_MODE          =$E2; // Unsupported Windows mode
Const SRB_ST_NO_ASPI               =$E3; // No ASPI managers resident
Const SRB_ST_FAILED_INIT           =$E4; // ASPI for windows failed init
Const SRB_ST_ASPI_IS_BUSY          =$E5; // No resources available to execute cmd
Const SRB_ST_BUFFER_TOO_BIG        =$E6; // Buffer size too big to handle!
Const SRB_ST_MISMATCHED_COMPONENTS =$E7; // The DLLs/EXEs of ASPI don't version check
Const SRB_ST_NO_ADAPTERS           =$E8; // No host adapters to manage
Const SRB_ST_INSUFFICIENT_RESOURCES=$E9; // Couldn't allocate resources needed to init
Const SRB_ST_ASPI_IS_SHUTDOWN      =$EA; // Call came to ASPI after PROCESS_DETACH
Const SRB_ST_BAD_INSTALL           =$EB; // The DLL or other components are installed wrong

{** Win ASPI32 host adapter status bytes **}
Const HA_ST_OK                  =$00; // Host adapter did not detect an error
Const HA_ST_TIMEOUT             =$09; // Bus timed out
Const HA_ST_COMMAND_TIMEOUT     =$0B; // SRB timed out
Const HA_ST_MESSAGE_REJECT      =$0D; // Message REJECT received while processing SRB
Const HA_ST_BUS_RESET           =$0E; // A bus reset was detected
Const HA_ST_PARITY_ERROR        =$0F; // A parity error was detected
Const HA_ST_REQUEST_SENSE_FAILED=$10; // The adapter failed in issuing a request sense.
Const HA_ST_SEL_TO              =$11; // Selection of target device timed out
Const HA_ST_DO_DU               =$12; // Data overrun or data underrun
Const HA_ST_BUS_FREE            =$13; // Unexpected bus free
Const HA_ST_PHASE_ERR           =$14; // Target bus phase sequence failure

{** WNASPI SCSI max values **}
Const ASPI_DEFAULT_MAX_DEV_ID=7;

{** SCSI peripheral device types **}
Const DTYPE_DIR_ACC =$00; //Direct access device, e.g. magnetic disk
Const DTYPE_SEQ_ACC =$01; //Sequential access device, e.g. magnetic tape
Const DTYPE_PRINTER =$02; //Printer device
Const DTYPE_PROC    =$03; //Processor device
Const DTYPE_WORM    =$04; //Write-once device, e.g. some optical disks
Const DTYPE_CDROM   =$05; //CDROM device
Const DTYPE_SCANNER =$06; //Scanner device
Const DTYPE_OPTI_MEM=$07; //Optical memory device, e.g. some optical disks
Const DTYPE_JUKEBOX =$08; //Medium changer device, e.g. juke box
Const DTYPE_COMM    =$09; //Communication device
Const DTYPE_RESL    =$0A; //Reserved low
Const DTYPE_RESH    =$1E; //Reserved high
Const DTYPE_UNKNOWN =$1F; //Unknown or no device type

{** Misc types **}
Type T_array36  =Array[1..36] Of Byte;
     T_array20  =Array[1..20] Of Byte;
     T_array16  =Array[1..16] Of Byte;
     T_array12  =Array[1..12] Of byte;
     T_array10  =Array[1..10] Of Byte;
     T_sense_len=Array[1..AS_SENSE_LEN+2] Of Byte;

{** SCSI request block header **}
Type T_SRB_hdr=Record
                     Cmd      : Byte;     { ASPI command code }
                     Status   : Byte;     { Return status }
                     HA_ID    : Byte;     { Host Adapter number }
                     Flags    : Byte;     { Request Flags }
                     Reserved : LongWord; { Reserved }
               End;

{** SRB for host adapter inquiry command (00h) **}
Type T_SRB_for_HA_inq=Record
                            SRB_hdr        : T_SRB_hdr;
                            N_HA           : Byte;      { No. of host adapters }
                            HA_SCSI_ID     : Byte;      { Host adapter SCSI ID }
                            Manager_ID_str : T_array16; { SCSI Manager ID string }
                            HA_str         : T_array16; { Host adapter ID string }
                            HA_uniq_parms  : T_array16; { Host adapter parameters }
                            Supported_ext  : Word;      { Supported extensions }
                      End;

{** SRB for get device type command (01h) **}
Type T_SRB_for_get_dev_type=Record
                                  SRB_hdr  : T_SRB_hdr;
                                  Dev_ID   : Byte;
                                  LUN      : Byte;
                                  Dev_type : Byte;
                                  Reserved : Byte;
                            End;

{** SRB for execute SCSI I/O command (02h) header **}
Type T_exec_SCSI_hdr=Record
                           SRB_hdr          : T_SRB_hdr;
                           Dev_ID           : Byte;
                           LUN              : Byte;
                           Reserved1        : Word;
                           Buf_len          : LongWord;
                           P_Buf            : Pointer;
                           Sense_len        : Byte;
                           CDB_len          : Byte;
                           HA_status        : Byte;
                           Dev_status       : Byte;
                           Post_proc        : Procedure;
                           Reserved2        : T_array20;
                     End;

{** SRB for execute SCSI I/O command (02h) **}
Type T_exec_SCSI=Record
                       SRB_hdr          : T_SRB_hdr;
                       Dev_ID           : Byte;
                       LUN              : Byte;
                       Reserved1        : Word;
                       Buf_len          : LongWord;
                       P_Buf            : Pointer;
                       Sense_len        : Byte;
                       CDB_len          : Byte;
                       HA_status        : Byte;
                       Dev_status       : Byte;
                       Post_proc        : Procedure;
                       Reserved2        : T_array20;
                       CDB              : T_array16;
                       Sense_area       : T_sense_len;
                 End;
Type T_LargestSRB=Array[1..SizeOf(T_exec_SCSI)] Of Byte;

{** SRB for abort a SRB command (03h) **}
Type T_SRB_for_abort=Record
                           SRB_hdr : T_SRB_hdr;
                           SRB_ptr : Pointer;
                     End;

{** SRB for reset SCSI device command (04h) **}
Type T_SRB_for_reset_dev=Record
                               SRB_hdr          : T_SRB_hdr;
                               Dev_ID           : Byte;
                               LUN              : Byte;
                               Reserved1        : T_array12;
                               HA_status        : Byte;
                               Dev_status       : Byte;
                               Post_proc        : Procedure;
                               Reserved2        : T_array36;
                         End;

{** SRB for get disk drive information command (05h) **}
Type T_SRB_for_get_drv_parms=Record
                                   SRB_hdr     : T_SRB_hdr;
                                   Dev_ID      : Byte;
                                   LUN         : Byte;
                                   Drive_flags : Byte;
                                   Int13_drive : Byte;
                                   Head_XLAT   : Byte;
                                   Sector_XLAT : Byte;
                                   Reserved1   : T_array10;
                             End;

{** SRB for rescan scsi bus(es) on scsi port command (07h) **}
Type T_SRB_for_rescan_scsi=Record
                                 SRB_hdr     : T_SRB_hdr;
                           End;

{** SRB for get/set target device timeout command (08h) **}
Type T_SRB_for_getset_timeout=Record
                                    SRB_hdr     : T_SRB_hdr;
                                    Dev_ID      : Byte;
                                    LUN         : Byte;
                                    Timeout     : LongWord; // Timeout in half seconds
                              End;
{** Win get/free ASPI32 buffer structure. **}
Type T_ASPI32Buf=Record
                       P_Buf     : Pointer;
                       Buf_len   : LongWord;
                       Zero_fill : LongWord;
                       Reserved  : LongWord;
                 End;

Type T_HA_inquiry_out=
Record
      HA_SCSI_ID  : Byte;
      manager     : String;
      HA_name     : String;
      HA_params   : T_array16
End;

{ My own constants (starts with USER to indicate they're mine) }

{ Constants for OpenASPIStatus.. }
Const USER_WNASPI32_OPENASPI_OK                  =1; //WNASPI32.DLL loaded and required functions verified.
Const USER_WNASPI32_GETASPISUPINFO_ERR           =2; //Call to initial function failed.
Const USER_WNASPI32_OPENASPI_DLL_LOAD_ERR        =3; //Cannot load WNASPI32.DLL into mem.
Const USER_WNASPI32_OPENASPI_FX_IMPORT_ERR       =4; //Required functions cannot be imported.

{ Actual Win ASPI32 transfer memory buffer bounds.}
Const USER_MEM_UNIT_LARGEMIN=USER_MEM_UNIT_128K; //Smallest size for GetASPI32Buffer.
Const USER_MEM_UNIT_LARGEMAX=USER_MEM_UNIT_512K; //Largest size for GetASPI32Buffer.

