Unit SPC_SPT_Unit;

Interface

Uses
    SPC_any_link_Unit,
    SPC_Unit,
    SPT_Unit,
    SysUtils; //For string functions.

const U_METHOD_WNASPI=1;
const U_METHOD_SCSI_PASS_THRU=2;
const U_METHOD_SCSI_PASS_THRU_D=3;

//This class..
Type T_SPC_SPT=
Class(T_SPC_any_link)
Public
      Constructor Create(Var In_SPT : T_SPT);
      Destructor Destroy; Override;
      Procedure V_Send_SCSI_cmd; //Sends SCSI command to device.
      Function Get_err_msg : PChar; Override;
      { SCSI commands.. }
      Procedure Test_unit_ready_CDB6; Override;
      Procedure Req_sense_CDB6; Override;
      Procedure Get_dev_inquiry_CDB6; Override;
      Procedure Mode_select_CDB6(In_param_list_len : Byte;
                                 In_save_params    : Boolean); Override;
      Procedure Mode_sense_CDB6(In_DBD            : Byte;
                                In_PC_PageCode    : Byte;
                                In_param_list_len : Byte); Override;
      Procedure Mode_select_CDB10(In_param_list_len : Word;
                                  In_save_params    : Boolean); Override;
      Procedure Mode_sense_CDB10(In_DBD            : Byte;
                                 In_PC_PageCode    : Byte;
                                 In_param_list_len : Word); Override;
      { Friendly versions of SCSI commands.. }
      Procedure Get_dev_inquiry_CDB6_str(Var Out_VendorID : String;
                                         Var Out_ProdID   : String;
                                         Var Out_ProdRev  : String); Override;
      { Some sense error data.. }
      Function Get_sense_key  : Byte; Override;
      Function Get_sense_ASC  : Byte; Override;
      Function Get_sense_ASCQ : Byte; Override;
      { Decode & verification of device status & sense error data.. }
      Function Verify_dev_status(In_dev_status : Byte) : T_dev_status_err_msg;
      Function Get_sense_key_code_err_msg(In_p_sense_area : Pointer) : T_sense_key_code_err_msg;
      Function Verify_sense_status(In_p_sense_area : Pointer) : T_sense_status_err_msg;

      { Indicates whether the SCSI command was sent OK
        through the interface Win ASPI 32 or DeviceIOCTL. }
      Function Get_is_sendcmd_OK  : Boolean; Override;
      Procedure Set_is_sendcmd_OK(in_state : Boolean); Override;
      Function Get_sendcmd_err_msg : String; Override;

      function Get_data_buf : Pointer; Override;
      Function Get_data_buf_size : LongWord; Override;
      Function Find_first_CDROM : Boolean; Override;
      Function Find_next_CDROM : Boolean; Override;
      Procedure Set_found_CDROM_active; Override;

      Procedure Zero_data_buf; Override;
End;

Implementation

Constructor T_SPC_SPT.Create(Var In_SPT : T_SPT);
Begin
     Inherited Create;

     interface_method:=U_METHOD_SCSI_PASS_THRU;

     SPC:=T_SPC.Create;
     SPT:=In_SPT;
End;

Destructor T_SPC_SPT.Destroy;
Begin
     SPC.Destroy;
     
     Inherited;
End;

Procedure T_SPC_SPT.V_Send_SCSI_cmd;
{ **************************************************************
  Sends a SCSI command to the active device selected in SPT
  object and performs a full verification and decoding of source
  error codes. The SPT structure is assumed to be filled
  in with appropriate values already. Also, a full
  verification on the error codes are decoded of the following:
       - DeviceIOCTL success (Win32 command),
       - Sense error data (SCSI-3 sense info)
  You will find human readable error message strings (if any)
  stored in the following attributes of SPT object:
       - SPT.err_msg
       - SPT.Sense_key_err_msg
       - SPT.Sense_code_err_msg
  And the following indicates whether each type of source was
  OK or not:
       - SPT.dev_io_success
       - SPT.Std_sense_exists
  ************************************************************** }
Begin
     { Initialise error flags to okay. }
     //SPT.Dev_status_OK:=False;
     //SPT.Dev_err_msg:='';
     SPT.Std_sense_exists:=False;
     SPT.Sense_key_err_msg:='';
     SPT.Sense_code_err_msg:='';

     SPT.Send_SCSI_cmd_wait; //Send the SCSI command.

     //Check if we have a SCSI-3 defined device status.
     Verify_dev_status(SPT.spt_bufs.spt.ScsiStatus);

     //Check if we have a SCSI-3 defined check condition status.
     If SPT.spt_bufs.spt.ScsiStatus=DEV_ST_CHKCOND Then
     Begin
          Verify_sense_status(SPT.Get_sense_buf);  //Verify sense error info.

          //We have a check condition, so we manually make dev_io_success false.
          If SPT.dev_io_success=True Then //Did DeviceIOCTL completed with error ?
          Begin
               SPT.dev_io_success:=False; //Manually set this to false to satisfy our error checking method.
                                          //We should really create a new separate flag for this to indicate an error exist.
          End;
     End;
End;

Function T_SPC_SPT.Get_err_msg : PChar;
{ **********************************************************************
  Gets the most appropriate (originator) error message.  Errors can
  orginate from the following (in order of highest layer first to lowest
  layer):
       - Win32 DeviceIOCTL,
       - the host adapter,
       - sense error data
  Errors from lower layers will propagate to higher layers,
  for example:
  A device check condition error may cause the DeviceIOCTL to report "Failure".
  Sometimes we do not want to display DeviceIOCTL error message, we want to
  display the sense error message or device error message instead.
  ****************************************************************************}
Begin
     If SPT.dev_io_success=False Then
     Begin
          If SPT.Dev_status_OK=False Then
          Begin
               If SPT.spt_bufs.spt.ScsiStatus=DEV_ST_CHKCOND Then
                   Result:=PChar(SPT.Sense_code_err_msg)
               Else
                   Result:=PChar(SPT.Dev_err_msg);
          End
          Else
              Result:=PChar(SPT.err_msg);
     End
     Else
         Get_err_msg:=Nil;
End;

Procedure T_SPC_SPT.Test_unit_ready_CDB6;
{ *********************************
  Sends a test unit command (CDB6).
  ********************************* }
Begin
     SPT.Fill_spt(SCSI_IOCTL_DATA_UNSPECIFIED, 0, 6); //Fill parts of SRB with zeroes.
     SPC.Fill_CDB6_test_unit_ready(T_test_unit_ready(SPT.Get_p_Cdb^));
     V_Send_SCSI_cmd;
End;

Procedure T_SPC_SPT.Req_sense_CDB6;
{ *************************************
  Sends a request sense command (CDB6).
  ************************************* }
Begin
     SPT.Fill_spt(SCSI_IOCTL_DATA_IN, SizeOf(T_sense_data), 6);
     SPC.Fill_CDB6_req_sense(T_req_sense(SPT.Get_p_Cdb^));
     V_Send_SCSI_cmd;
End;

Procedure T_SPC_SPT.Get_dev_inquiry_CDB6;
{ ******************************************************************
  Sends a get device inquiry CDB6 command to the current SCSI device
  in SPT instance object.
  ****************************************************************** }
Begin
     SPT.Fill_spt(SCSI_IOCTL_DATA_IN, 36, 6);
     SPC.Fill_CDB6_get_dev_inquiry(T_dev_inq_CDB6(SPT.Get_p_Cdb^));
     V_Send_SCSI_cmd;
End;

Procedure T_SPC_SPT.Mode_select_CDB6(In_param_list_len : Byte;
                                     In_save_params    : Boolean);
{ *********************************
  Sends a mode select CDB6 command.
  ********************************* }
Begin
     SPT.Fill_spt(SCSI_IOCTL_DATA_OUT, In_param_list_len, 6);
     SPC.Fill_CDB6_mode_select(T_mode_select_CDB6(SPT.Get_p_Cdb^),
                               In_param_list_len,
                               In_save_params);
     V_Send_SCSI_cmd;
End;

Procedure T_SPC_SPT.Mode_sense_CDB6(In_DBD            : Byte;
                                    In_PC_PageCode    : Byte;
                                    In_param_list_len : Byte);
{ ********************************
  Sends a mode sense CDB6 command.
  ******************************** }
Begin
     SPT.Fill_spt(SCSI_IOCTL_DATA_IN, In_param_list_len, 6);
     SPC.Fill_CDB6_mode_sense(T_mode_sense_CDB6(SPT.Get_p_Cdb^),
                              In_DBD,
                              In_PC_PageCode,
                              In_param_list_len);
     V_Send_SCSI_cmd;
End;

Procedure T_SPC_SPT.Mode_select_CDB10(In_param_list_len : Word;
                                      In_save_params    : Boolean);
{ **********************************
  Sends a mode select CDB10 command.
  ********************************** }
Begin
     SPT.Fill_spt(SCSI_IOCTL_DATA_OUT, In_param_list_len, 10);
     SPC.Fill_CDB10_mode_select(T_mode_select_CDB10(SPT.Get_p_Cdb^),
                                In_param_list_len,
                                In_save_params);
     V_Send_SCSI_cmd;
End;

Procedure T_SPC_SPT.Mode_sense_CDB10(In_DBD            : Byte;
                                     In_PC_PageCode    : Byte;
                                     In_param_list_len : Word);
{ *********************************
  Sends a mode sense CDB10 command.
  ********************************* }
Begin
     SPT.Fill_spt(SCSI_IOCTL_DATA_IN, In_param_list_len, 10);
     SPC.Fill_CDB10_mode_sense(T_mode_sense_CDB10(SPT.Get_p_Cdb^),
                               In_DBD,
                               In_PC_PageCode,
                               In_param_list_len);
     V_Send_SCSI_cmd;
End;

Procedure T_SPC_SPT.Get_dev_inquiry_CDB6_str(Var Out_VendorID : String;
                                             Var Out_ProdID   : String;
                                             Var Out_ProdRev  : String);
{ ******************************************************************
  Sends a get device inquiry CDB6 command to the current SCSI device
  in SPT instance object and return variable data (friendly
  version).
  ****************************************************************** }
Var
   P_str : PChar;
Begin
     Get_dev_inquiry_CDB6;
     With T_dev_inq_std_data(SPT.Get_data_buf^) Do
     Begin
          P_str:=StrAlloc(9);
          StrLCopy(P_str, @VendorID, 8);
          Out_VendorID:=String(P_str);
          StrDispose(P_str);

          P_str:=StrAlloc(17);
          StrLCopy(P_str, @ProdID, 16);
          Out_ProdID:=String(P_str);
          StrDispose(P_str);

          P_str:=StrAlloc(5);
          StrLCopy(P_str, @ProdRev, 4);
          Out_ProdRev:=String(P_str);
          StrDispose(P_str);
     End;
End;

Function T_SPC_SPT.Get_sense_key : Byte;
{ *************************
  Get sense key error code.
  ************************* }
Begin
     Result:=SPT.spt_bufs.SenseBuf[2] And $0F;
End;

Function T_SPC_SPT.Get_sense_ASC : Byte;
{ *************************
  Get sense ASC error code.
  ************************* }
Begin
     Result:=SPT.spt_bufs.SenseBuf[12];
End;

Function T_SPC_SPT.Get_sense_ASCQ : Byte;
{ **************************
  Get sense ASCQ error code.
  ************************** }
Begin
     Result:=SPT.spt_bufs.SenseBuf[13];
End;

Function T_SPC_SPT.Verify_dev_status(In_dev_status : Byte) : T_dev_status_err_msg;
{ **************************************************************
  *This is really a wrapper to the same method in SPC but this
   will set the SPT object with the results.
  ************************************************************** }
Var
   Dev_status_err_msg : T_dev_status_err_msg;
Begin
     Dev_status_err_msg:=SPC.Verify_dev_status(In_dev_status);
     SPT.Dev_status_OK:=Dev_status_err_msg.Dev_status_OK;
     SPT.Dev_err_msg:=Dev_status_err_msg.Dev_err_msg;
     Result:=Dev_status_err_msg;
End;

Function T_SPC_SPT.Get_sense_key_code_err_msg(In_p_sense_area : Pointer) : T_sense_key_code_err_msg;
{ ********************************************************************
  *This is really a wrapper to the same method in SPC but this
   will set the SPT object with the results.
  ******************************************************************** }
Var
   Sense_key_code_err_msg : T_sense_key_code_err_msg;
Begin
     Sense_key_code_err_msg:=SPC.Get_sense_key_code_err_msg(In_p_sense_area);
     SPT.Sense_key_err_msg:=Sense_key_code_err_msg.Key_err_msg;
     SPT.Sense_code_err_msg:=Sense_key_code_err_msg.Code_err_msg;
     Result:=Sense_key_code_err_msg;
End;

Function T_SPC_SPT.Verify_sense_status(In_p_sense_area : Pointer) : T_sense_status_err_msg;
{ **************************************************************
  *This is really a wrapper to the same method in SPC but this
   will set the SPT object with the results.
  ************************************************************** }
Var
   Sense_status_err_msg : T_sense_status_err_msg;
Begin
     Sense_status_err_msg:=SPC.Verify_sense_status(In_p_sense_area);
     SPT.Std_sense_exists:=Sense_status_err_msg.Std_sense_exists;
     SPT.Sense_key_err_msg:=Sense_status_err_msg.Key_code_err_msg.Key_err_msg;
     SPT.Sense_code_err_msg:=Sense_status_err_msg.Key_code_err_msg.Code_err_msg;
     Result:=Sense_status_err_msg;
End;

Function T_SPC_SPT.Get_is_sendcmd_OK  : Boolean;
Begin
     Result:=SPT.dev_io_success;
End;

Procedure T_SPC_SPT.Set_is_sendcmd_OK(in_state : Boolean);
Begin
     SPT.dev_io_success:=in_state;
End;

Function T_SPC_SPT.Get_sendcmd_err_msg : String;
Begin
     Result:=SPT.err_msg;
End;

Function T_SPC_SPT.Get_data_buf : Pointer;
Begin
     Result:=SPT.Get_data_buf;
End;

Function T_SPC_SPT.Get_data_buf_size : LongWord;
Begin
     Result:=SPT.Get_data_buf_size;
End;

Function T_SPC_SPT.Find_first_CDROM : Boolean;
Begin
     Result:=SPT.Find_first_CDROM;
End;

Function T_SPC_SPT.Find_next_CDROM : Boolean;
Begin
     Result:=SPT.Find_next_CDROM;
End;

Procedure T_SPC_SPT.Set_found_CDROM_active;
Begin
     SPT.Set_found_CDROM_active;
End;

Procedure T_SPC_SPT.Zero_data_buf;
Begin
     SPT.Zero_data_buf;
End;

End.
 