Unit SPC_WNASPI32_Unit;

Interface

Uses
    SPC_any_link_Unit,
    SPC_Unit,
    WNASPI32Unit,
    SysUtils; //For string functions.

{$I SPC_WNASPI32_inc.pas}

const U_METHOD_WNASPI=1;
const U_METHOD_SCSI_PASS_THRU=2;
const U_METHOD_SCSI_PASS_THRU_D=3;

//This class..
Type T_SPC_WNASPI32=
Class(T_SPC_any_link)
Public
      Constructor Create(Var In_WNASPI32 : T_WNASPI32);
      Destructor Destroy; Override;
      Procedure V_Send_SRBCDB; //Sends SCSI command to device.
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

      Function Get_data_buf : Pointer; Override;
      Function Get_data_buf_size : LongWord; Override;
      Function Find_first_CDROM : Boolean; Override;
      Function Find_next_CDROM : Boolean; Override;
      Procedure Set_found_CDROM_active; Override;

      Procedure Zero_data_buf; Override;
End;

Implementation

Constructor T_SPC_WNASPI32.Create(Var In_WNASPI32 : T_WNASPI32);
Begin
     Inherited Create;

     interface_method:=U_METHOD_WNASPI;

     SPC:=T_SPC.Create;
     WNASPI32:=In_WNASPI32;
End;

Destructor T_SPC_WNASPI32.Destroy;
Begin
     SPC.Destroy;
     
     Inherited;
End;

Procedure T_SPC_WNASPI32.V_Send_SRBCDB;
{ **************************************************************
  Sends a SCSI command to the active device selected in WNASPI32
  object and performs a full verification and decoding of source
  error codes. The Win ASPI 32 SRB is assumed to be filled
  in with appropriate values already. Also, a full
  verification on the error codes are decoded of the following:
       - SRB status (Win ASPI 32 command),
       - HA status (Host Adapter SCSI adapter),
       - Dev status (Device),
       - Sense error data (SCSI-3 sense info)
  You will find human readable error message strings (if any)
  stored in the following attributes of WNASPI32 object:
       - WNASPI32.HA_err_msg
       - WNASPI32.Dev_err_msg
       - WNASPI32.Sense_key_err_msg
       - WNASPI32.Sense_code_err_msg
  And the following indicates whether each type of source was
  OK or not:
       - WNASPI32.HA_status_OK
       - WNASPI32.Dev_status_OK
       - WNASPI32.Std_sense_exists
  ************************************************************** }
Begin
     WNASPI32.V_SendASPI32Command_Wait; //Send the SCSI command.
     If WNASPI32.Get_SRB_status=SRB_ST_ERR Then //Did SRB completed with error ?
     Begin
          { ******************************************************************
            If we get here, means that processing of SRB completed with error.
            ****************************************************************** }

          If WNASPI32.Get_HA_status_OK Then //Check host adapter for errors.
          Begin
               { ******************************************************************
                 If we get here, means that error came from target device.
                 ****************************************************************** }

               Verify_dev_status(T_exec_SCSI_hdr(WNASPI32.P_SRB^).Dev_status);
               If T_exec_SCSI_hdr(WNASPI32.P_SRB^).Dev_status=DEV_ST_CHKCOND Then
               Begin
                    Verify_sense_status(WNASPI32.Get_sense_buf);
               End
               Else
               Begin
                    { No sense error info. }
                    WNASPI32.Std_sense_exists:=False;
                    WNASPI32.Sense_key_err_msg:='';
                    WNASPI32.Sense_code_err_msg:='';
               End;
          End
          Else
          Begin
               { ***************************************************************
                 If we get here, means that error came from host (SCSI) adapter.
                 *************************************************************** }

               { Set target device and sense error to okay. }
               WNASPI32.Dev_status_OK:=True;
               WNASPI32.Dev_err_msg:='';
               WNASPI32.Std_sense_exists:=False;
               WNASPI32.Sense_key_err_msg:='';
               WNASPI32.Sense_code_err_msg:='';
          End;
     End
     Else
     Begin
          { *******************************************************
            If we get here, means that SRB completed without error.
            ******************************************************* }

          { So we set all other error flags to okay - no need to verify or
            decode the error codes. }
          //WNASPI32.HA_status_OK:=True; //Already set by T_WNASPI32.
          //WNASPI32.HA_err_msg:=''; //Already set by T_WNASPI32.
          WNASPI32.Dev_status_OK:=True;
          WNASPI32.Dev_err_msg:='';
          WNASPI32.Std_sense_exists:=False;
          WNASPI32.Sense_key_err_msg:='';
          WNASPI32.Sense_code_err_msg:='';
     End;
End;

Function T_SPC_WNASPI32.Get_err_msg : PChar;
{ **********************************************************************
  Gets the most appropriate (originator) error message.  Errors can
  orginate from the following (in order of highest layer first to lowest
  layer):
       - Win ASPI32,
       - the host adapter
       - or the target device (including sense error data)
  Errors from lower layers will propagate to higher layers,
  for example:
  A device error of "Target device is busy" will cause the Win ASPI32 to
  report "SRB completed with error".  Sometimes we do not want to display
  this error message, instead we may want to display the device error
  message only.

  Note that device specific or detail errors are reported by sense data
  and not the device status, the device status will be set to
  "Check condition" if there is a device specific error other than busy.
  **********************************************************************}
Begin
     If WNASPI32.Get_SRB_status_OK=False Then
     Begin
          If WNASPI32.Dev_status_OK=False Then
          Begin
               If T_exec_SCSI_hdr(WNASPI32.P_SRB^).Dev_status=DEV_ST_CHKCOND Then
                   Get_err_msg:=PChar(WNASPI32.Sense_code_err_msg)
               Else
                   Get_err_msg:=PChar(WNASPI32.Dev_err_msg);
          End
          Else
          Begin
               If WNASPI32.Get_HA_status_OK=False Then
               Begin
                    Get_err_msg:=PChar(WNASPI32.Get_HA_err_msg);
               End
               Else
               Begin
                    Get_err_msg:=PChar(WNASPI32.Get_SRB_err_msg);
               End;
          End;
     End
     Else
         Get_err_msg:=Nil;
End;

Procedure T_SPC_WNASPI32.Test_unit_ready_CDB6;
{ *********************************
  Sends a test unit command (CDB6).
  ********************************* }
Begin
     WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     WNASPI32.Fill_SRB_exec_SCSI(SRB_NO_DATA, 0, 6);
     SPC.Fill_CDB6_test_unit_ready(T_SRB_for_test_unit_ready(WNASPI32.P_SRB^).Test_unit_ready);
     V_Send_SRBCDB;
End;

Procedure T_SPC_WNASPI32.Req_sense_CDB6;
{ *************************************
  Sends a request sense command (CDB6).
  ************************************* }
Begin
     WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, SizeOf(T_sense_data), 6);
     SPC.Fill_CDB6_req_sense(T_SRB_for_req_sense(WNASPI32.P_SRB^).Req_sense);
     V_Send_SRBCDB;
End;

Procedure T_SPC_WNASPI32.Get_dev_inquiry_CDB6;
{ ******************************************************************
  Sends a get device inquiry CDB6 command to the current SCSI device
  in WNASPI32 instance object.
  ****************************************************************** }
Begin
     WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, 36, 6);
     SPC.Fill_CDB6_get_dev_inquiry(T_SRB_for_dev_inq_CDB6(WNASPI32.P_SRB^).Dev_inq_CDB6);
     V_Send_SRBCDB;
End;

Procedure T_SPC_WNASPI32.Mode_select_CDB6(In_param_list_len : Byte;
                                          In_save_params    : Boolean);
{ *********************************
  Sends a mode select CDB6 command.
  ********************************* }
Begin
     WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_OUT, In_param_list_len, 6);
     SPC.Fill_CDB6_mode_select(T_SRB_for_mode_select_CDB6(WNASPI32.P_SRB^).Mode_select_CDB6,
                               In_param_list_len,
                               In_save_params);
     V_Send_SRBCDB;
End;

Procedure T_SPC_WNASPI32.Mode_sense_CDB6(In_DBD            : Byte;
                                         In_PC_PageCode    : Byte;
                                         In_param_list_len : Byte);
{ ********************************
  Sends a mode sense CDB6 command.
  ******************************** }
Begin
     WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, In_param_list_len, 6);
     SPC.Fill_CDB6_mode_sense(T_SRB_for_mode_sense_CDB6(WNASPI32.P_SRB^).Mode_sense_CDB6,
                              In_DBD,
                              In_PC_PageCode,
                              In_param_list_len);
     V_Send_SRBCDB;
End;

Procedure T_SPC_WNASPI32.Mode_select_CDB10(In_param_list_len : Word;
                                           In_save_params    : Boolean);
{ **********************************
  Sends a mode select CDB10 command.
  ********************************** }
Begin
     WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_OUT, In_param_list_len, 10);
     SPC.Fill_CDB10_mode_select(T_SRB_for_mode_select_CDB10(WNASPI32.P_SRB^).Mode_select_CDB10,
                                In_param_list_len,
                                In_save_params);
     V_Send_SRBCDB;
End;

Procedure T_SPC_WNASPI32.Mode_sense_CDB10(In_DBD            : Byte;
                                          In_PC_PageCode    : Byte;
                                          In_param_list_len : Word);
{ *********************************
  Sends a mode sense CDB10 command.
  ********************************* }
Begin
     WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_IN, In_param_list_len, 10);
     SPC.Fill_CDB10_mode_sense(T_SRB_for_mode_sense_CDB10(WNASPI32.P_SRB^).Mode_sense_CDB10,
                               In_DBD,
                               In_PC_PageCode,
                               In_param_list_len);
     V_Send_SRBCDB;
End;

Procedure T_SPC_WNASPI32.Get_dev_inquiry_CDB6_str(Var Out_VendorID : String;
                                                  Var Out_ProdID   : String;
                                                  Var Out_ProdRev  : String);
{ ******************************************************************
  Sends a get device inquiry CDB6 command to the current SCSI device
  in WNASPI32 instance object and return variable data (friendly
  version).
  ****************************************************************** }
Var
   P_str : PChar;
Begin
     Get_dev_inquiry_CDB6;
     With T_dev_inq_std_data(WNASPI32.Get_data_buf^) Do
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

Function T_SPC_WNASPI32.Get_sense_key : Byte;
{ *************************
  Get sense key error code.
  ************************* }
Begin
     Get_sense_key:=(T_sense_data(WNASPI32.Get_sense_buf^).Flags_SenseKey AND $F);
End;

Function T_SPC_WNASPI32.Get_sense_ASC : Byte;
{ *************************
  Get sense ASC error code.
  ************************* }
Begin
     Get_sense_ASC:=T_sense_data(WNASPI32.Get_sense_buf^).ASC;
End;

Function T_SPC_WNASPI32.Get_sense_ASCQ : Byte;
{ **************************
  Get sense ASCQ error code.
  ************************** }
Begin
     Get_sense_ASCQ:=T_sense_data(WNASPI32.Get_sense_buf^).ASCQ;
End;

Function T_SPC_WNASPI32.Verify_dev_status(In_dev_status : Byte): T_dev_status_err_msg;
{ **************************************************************
  *This is really a wrapper to the same method in SPC but this
   will set the WNASPI32 object with the results.
  ************************************************************** }
Var
   Dev_status_err_msg : T_dev_status_err_msg;
Begin
     Dev_status_err_msg:=SPC.Verify_dev_status(In_dev_status);
     WNASPI32.Dev_status_OK:=Dev_status_err_msg.Dev_status_OK;
     WNASPI32.Dev_err_msg:=Dev_status_err_msg.Dev_err_msg;
End;

Function T_SPC_WNASPI32.Get_sense_key_code_err_msg(In_p_sense_area : Pointer) : T_sense_key_code_err_msg;
{ ********************************************************************
  *This is really a wrapper to the same method in SPC but this
   will set the WNASPI32 object with the results.
  ******************************************************************** }
Var
   Sense_key_code_err_msg : T_sense_key_code_err_msg;
Begin
     Sense_key_code_err_msg:=SPC.Get_sense_key_code_err_msg(In_p_sense_area);
     WNASPI32.Sense_key_err_msg:=Sense_key_code_err_msg.Key_err_msg;
     WNASPI32.Sense_code_err_msg:=Sense_key_code_err_msg.Code_err_msg;
     Result:=Sense_key_code_err_msg;
End;

Function T_SPC_WNASPI32.Verify_sense_status(In_p_sense_area : Pointer) : T_sense_status_err_msg;
{ **************************************************************
  *This is really a wrapper to the same method in SPC but this
   will set the WNASPI32 object with the results.
  ************************************************************** }
Var
   Sense_status_err_msg : T_sense_status_err_msg;
Begin
     Sense_status_err_msg:=SPC.Verify_sense_status(In_p_sense_area);
     WNASPI32.Std_sense_exists:=Sense_status_err_msg.Std_sense_exists;
     WNASPI32.Sense_key_err_msg:=Sense_status_err_msg.Key_code_err_msg.Key_err_msg;
     WNASPI32.Sense_code_err_msg:=Sense_status_err_msg.Key_code_err_msg.Code_err_msg;
     Result:=Sense_status_err_msg;
End;

Function T_SPC_WNASPI32.Get_is_sendcmd_OK  : Boolean;
Begin
     Result:=WNASPI32.Get_SRB_status_OK;
End;

Procedure T_SPC_WNASPI32.Set_is_sendcmd_OK(in_state : Boolean);
Begin
     WNASPI32.Set_SRB_status_OK(in_state);
End;

Function T_SPC_WNASPI32.Get_sendcmd_err_msg : String;
Begin
     Result:=WNASPI32.Get_SRB_err_msg;
End;

Function T_SPC_WNASPI32.Get_data_buf : Pointer;
Begin
     Result:=WNASPI32.Get_data_buf;
End;

Function T_SPC_WNASPI32.Get_data_buf_size : LongWord;
Begin
     Result:=WNASPI32.Get_data_buf_size;
End;

Function T_SPC_WNASPI32.Find_first_CDROM : Boolean;
Begin
     Result:=WNASPI32.Find_first_CDROM;
End;

Function T_SPC_WNASPI32.Find_next_CDROM : Boolean;
Begin
     Result:=WNASPI32.Find_next_CDROM;
End;

Procedure T_SPC_WNASPI32.Set_found_CDROM_active;
Begin
     WNASPI32.Set_found_CDROM_active;
End;

Procedure T_SPC_WNASPI32.Zero_data_buf;
Begin
     WNASPI32.Zero_data_buf;
End;

End.
