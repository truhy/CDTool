{ Name:         SPC_DEFS.PAS
  File type:    Borland Delphi 4 include file.
  Description:  Misc SCSI definitions written for Borland Delphi 4.
                This source file is based on information given from
                the Adaptec's ASPI SDK and working draft version of
                SCSI-3 specification documents:
                     - SPC (SCSI Primary Commands)
                     - SAM (SCSI Architecture Mechanism)
  Notes:
  Date started: 19th Dec 1999.
  Developer:    Truong Hy.
}
{** SCSI peripheral device types (defined in SAM) **}
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

{** Target device status values (defined in SAM) **}
Const DEV_ST_GOOD          =$00; //Target device did not detect an error
Const DEV_ST_CHKCOND       =$02; //Check condition status, error detected by device
                                 //check the returned sense data
Const DEV_ST_CONDMET       =$04; //Condition met (satisfied) for unlinked commands
Const DEV_ST_BUSY          =$08; //Target device is currently busy, cannot accept
                                 //any commands yet
Const DEV_ST_INTERM        =$10; //Linked command completed successfully
Const DEV_ST_INTERM_CONDMET=$14; //Condition met (satisfied) for linked commands
Const DEV_ST_RESCONF       =$18; //Device was reserved
Const DEV_ST_CMDTERM       =$22; //Command terminated
Const DEV_ST_QFULL         =$28; //Queue is full

{** SCSI MISC values **}
Const MAX_LUN_ID        =7;
Const DEFAULT_MAX_DEV_ID=7;
Const MAX_SCSI_LUN      =64;
Const MAX_SCSI_HA       =8;

{** SCSI-3 primary command codes **}
Const SCSI_CMD_TEST_UNIT_READY  =$00;
Const SCSI_CMD_REQ_SENSE        =$03;
Const SCSI_CMD_DEV_INQUIRY_CDB6 =$12;
Const SCSI_CMD_MODE_SELECT_CDB6 =$15;
Const SCSI_CMD_MODE_SENSE_CDB6  =$1A;
Const SCSI_CMD_MODE_SELECT_CDB10=$55;
Const SCSI_CMD_MODE_SENSE_CDB10 =$5A;

{** Request sense flags **}
{** Request sense response code **}
Const SEN_RESP_CURR_ERR   =$70; //Current errors
Const SEN_RESP_DEFER_ERR  =$71; //Deferred errors
Const SEN_RESP_VENDOR_SPEC=$7F; //Vendor specific
{** Request sense bit definitions **}
Const SEN_VALID  =$80;
Const SEN_FILEMRK=$80;
Const SEN_EOM    =$40;
Const SEN_ILI    =$20;
{** Request sense sense key definitions **}
Const SEN_KEY_NO_SEN     =$00; //No sense key info.
Const SEN_KEY_RECV_ERR   =$01; //Recovered or recoverable error/s.
Const SEN_KEY_NOT_READY  =$02; //Device not ready error.
Const SEN_KEY_MEDIUM_ERR =$03; //Medium error.
Const SEN_KEY_HARD_ERR   =$04; //Hardware error.
Const SEN_KEY_ILLEGAL_REQ=$05; //Illegal request, error/s in parameters or cmd.
Const SEN_KEY_UNIT_ATT   =$06; //Unit attention error.
Const SEN_KEY_DATA_PROT  =$07; //Data is protected for read or write access.
Const SEN_KEY_BLANK_CHK  =$08; //Blank or formatted medium encountered.
Const SEN_KEY_VEND_SPEC  =$09; //Vendor specific sense info.
Const SEN_KEY_COPY_ABORT =$0A; //Copy, compare or copy & verify aborted due to errors.
Const SEN_KEY_ABORTED_CMD=$0B; //Command aborted by device server.
Const SEN_KEY_EQUAL      =$0C; //Obsolete in SCSI-3 spec
Const SEN_KEY_VOL_OVERFLW=$0D; //Medium write capacity reached error.
Const SEN_KEY_MISCOMP    =$0E; //Source does not match data read error.
Const SEN_KEY_RESERVED   =$0F; //Reserved.
{** Request sense ASC/ASCQ flags **}

{** Mode select flags (applies to CDB6 and 10 versions) **}
Const MODE_SELECT_SP_NOSAVE  =$00;
Const MODE_SELECT_SP_SAVE    =$01;
Const MODE_SELECT_PF_VENDOR  =$00;
Const MODE_SELECT_PF_STANDARD=$10;

(** Mode sense flags **)
Const MODE_SENSE_DBD_BLK_DESC   =$00;
Const MODE_SENSE_DBD_NO_BLK_DESC=$08;
{** Page control flags. These should
be ORed with mode page code flags **}
Const MODE_SENSE_PC_CURRENT_VALS=$00;
Const MODE_SENSE_PC_CHGABLE_VALS=$40;
Const MODE_SENSE_PC_DEFAULT_VALS=$80;
Const MODE_SENSE_PC_SAVED_VALS  =$C0;
{** Mode page code flags. Rest of mode
page code flags are defined in device
specific class **}
Const MODE_SENSE_PGC_ALL   =$3F;

{** Misc types **}
Type
    T_array16=Array[1..16] Of Byte;
    T_array8=Array[1..8] Of Byte;
    T_array4=Array[1..4] Of Byte;


{** CDB for test unit ready command **}
Type T_test_unit_ready=Record
                             Cmd       : Byte;
                             Reserved1 : Byte;
                             Reserved2 : Byte;
                             Reserved3 : Byte;
                             Reserved4 : Byte;
                             Control   : Byte;
                       End;

{** CDB for request sense command **}
Type T_req_sense=Record
                       Cmd       : Byte;
                       Reserved1 : Byte;
                       Reserved2 : Byte;
                       Reserved3 : Byte;
                       Alloc_len : Byte;
                       Control   : Byte;
                 End;

{** Request sense return data format **}
Type T_sense_data=Record
                        ResponseCode   : Byte;
                        SegmentNo      : Byte;
                        Flags_SenseKey : Byte;
                        Info0          : Byte;
                        Info1          : Byte;
                        Info2          : Byte;
                        Info3          : Byte;
                        AddLen         : Byte;
                        ComSpecInfo0   : Byte;
                        ComSpecInfo1   : Byte;
                        ComSpecInfo2   : Byte;
                        ComSpecInfo3   : Byte;
                        ASC            : Byte;
                        ASCQ           : Byte;
                        FieldRepUCode  : Byte;
                        SenKeySpec15   : Byte;
                        SenKeySpec16   : Byte;
                        SenKeySpec17   : Byte;                        
                  End;

{** CDB for device inquiry CDB6 command **}
Type T_dev_inq_CDB6=Record
                          Cmd           : Byte;
                          CmdDt_EVPD    : Byte;
                          Pg_or_Op_code : Byte;
                          Reserved1     : Byte;
                          Len           : Byte;
                          Control       : Byte;
                    End;

{** Device inquiry standard return data format **}
Type T_dev_inq_std_data=Record
                              Dev_Qual_Type : Byte;
                              RMB           : Byte;
                              ConformVer    : Byte;
                              SupportFlags  : Byte;
                              AddLen        : Byte;
                              Reserved1     : Byte;
                              Flags1        : Byte;
                              Flags2        : Byte;
                              VendorID      : T_array8;
                              ProdID        : T_array16;
                              ProdRev       : T_array4;
                        End;

{** CDB for mode select CDB6 command **}
Type T_mode_select_CDB6=Record
                              Cmd            : Byte;
                              PF_SP          : Byte;
                              Reserved1      : Byte;
                              Reserved2      : Byte;
                              Param_list_len : Byte;
                              Control        : Byte;
                        End;

{** CDB for mode select CDB10 command **}
Type T_mode_select_CDB10=Record
                               Cmd                : Byte;
                               PF_SP              : Byte;
                               Reserved1          : Byte;
                               Reserved2          : Byte;
                               Reserved3          : Byte;
                               Reserved4          : Byte;
                               Reserved5          : Byte;
                               Param_list_len_MSB : Byte;
                               Param_list_len_LSB : Byte;
                               Control            : Byte;
                         End;

{** CDB for mode sense CDB6 command **}
Type T_mode_sense_CDB6=Record
                             Cmd            : Byte;
                             DBD            : Byte;
                             PC_PageCode    : Byte;
                             Reserved1      : Byte;
                             Param_list_len : Byte;
                             Control        : Byte;
                       End;

{** CDB for mode sense CDB10 command **}
Type T_mode_sense_CDB10=Record
                              Cmd                : Byte;
                              DBD                : Byte;
                              PC_PageCode        : Byte;
                              Reserved1          : Byte;
                              Reserved2          : Byte;
                              Reserved3          : Byte;
                              Reserved4          : Byte;
                              Param_list_len_MSB : Byte;
                              Param_list_len_LSB : Byte;
                              Control            : Byte;
                        End;

{** Generic mode parameter header for mode select/sense CDB6 **}
Type T_mode_param_hdr_6=Record
                              Mode_data_len      : Byte;
                              Medium_type        : Byte;
                              Dev_specific_param : Byte;
                              Block_desc_len     : Byte;
                        End;

{** Generic mode parameter header for mode select/sense CDB10 **}
Type T_mode_param_hdr_10=Record
                               Mode_data_len_MSB  : Byte;
                               Mode_data_len_LSB  : Byte;
                               Medium_type        : Byte;
                               Dev_specific_param : Byte;
                               Reserved1          : Byte;
                               Reserved2          : Byte;
                               Block_desc_len_MSB : Byte;
                               Block_desc_len_LSB : Byte;
                         End;

{** Generic (except direct access devices) mode parameter block descriptor **}
Type T_mode_param_block_desc=Record
                                   Density_code      : Byte;
                                   N_blocks_MSB      : Byte;
                                   N_blocks_MidByte  : Byte;
                                   N_blocks_LSB      : Byte;
                                   Reserved1         : Byte;
                                   Block_len_MSB     : Byte;
                                   Block_len_MidByte : Byte;
                                   Block_len_LSB     : Byte;
                             End;

{** Generic mode parameter header+block descriptor for mode select/sense CDB6 **}
Type T_mode_param_hdr_block_desc_6=Record
                                         Hdr        : T_mode_param_hdr_6;
                                         Block_desc : T_mode_param_block_desc;
                                   End;

{** Generic mode parameter header+block descriptor for mode select/sense CDB10 **}
Type T_mode_param_hdr_block_desc_10=Record
                                         Hdr        : T_mode_param_hdr_10;
                                         Block_desc : T_mode_param_block_desc;
                                   End;
