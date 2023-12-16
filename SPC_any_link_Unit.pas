unit SPC_any_link_Unit;

interface

Uses
    SPC_Unit,
    WNASPI32Unit,
    SPT_Unit;

const U_METHOD_WNASPI=1;
const U_METHOD_SCSI_PASS_THRU=2;
const U_METHOD_SCSI_PASS_THRU_D=3;

Type T_SPC_any_link=
Class
Protected
Public
      interface_method : byte;
      SPC : T_SPC; //SCSI-3 SPC instance.
      WNASPI32 : T_WNASPI32; //Win ASPI 32 interface instance.
      SPT : T_SPT; //Win32 DeviceIOCTL SPT instance.

      Function Get_err_msg : PChar; virtual; abstract;
      { SCSI commands.. }
      Procedure Test_unit_ready_CDB6; virtual; abstract;
      Procedure Req_sense_CDB6; virtual; abstract;
      Procedure Get_dev_inquiry_CDB6; virtual; abstract;
      Procedure Mode_select_CDB6(In_param_list_len : Byte;
                                 In_save_params    : Boolean); virtual; abstract;
      Procedure Mode_sense_CDB6(In_DBD            : Byte;
                                In_PC_PageCode    : Byte;
                                In_param_list_len : Byte); virtual; abstract;
      Procedure Mode_select_CDB10(In_param_list_len : Word;
                                  In_save_params    : Boolean); virtual; abstract;
      Procedure Mode_sense_CDB10(In_DBD            : Byte;
                                 In_PC_PageCode    : Byte;
                                 In_param_list_len : Word); virtual; abstract;
      { Friendly versions of SCSI commands.. }
      Procedure Get_dev_inquiry_CDB6_str(Var Out_VendorID : String;
                                         Var Out_ProdID   : String;
                                         Var Out_ProdRev  : String); virtual; abstract;
      { Some sense error data.. }
      Function Get_sense_key  : Byte; virtual; abstract;
      Function Get_sense_ASC  : Byte; virtual; abstract;
      Function Get_sense_ASCQ : Byte; virtual; abstract;

      { Indicates whether the SCSI command was sent OK
        through the interface Win ASPI 32 or DeviceIOCTL. }
      Function Get_is_sendcmd_OK  : Boolean; virtual; abstract;
      Procedure Set_is_sendcmd_OK(in_state : Boolean); virtual; abstract;
      Function Get_sendcmd_err_msg : String; virtual; abstract;

      Function Get_data_buf : Pointer; virtual; abstract;
      Function Get_data_buf_size : LongWord; virtual; abstract;
      Function Find_first_CDROM : Boolean; virtual; abstract;
      Function Find_next_CDROM : Boolean; virtual; abstract;
      Procedure Set_found_CDROM_active; virtual; abstract;

      Procedure Zero_data_buf; virtual; abstract;
End;

implementation

end.
 