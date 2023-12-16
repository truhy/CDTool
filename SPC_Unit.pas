{ Name:         SPC_Unit.PAS
  File type:    Borland Delphi 4 unit file.
  Description:  Contains a class which encapsulates the SCSI-3 SPC (for any SCSI
                device) functions.  This source file uses information given from
                the Adaptec's ASPI SDK and working draft version of SCSI-3 SPC
                (SCSI Primary Commands) specification document.
  Notes:        Written from scratch, no other samples used, except SDK.
                All function and parameters are in Intel x86 byte ordering,
                i.e. from LSB to MSB.
                At the moment the sense verification only include errors
                relating to MMC1.
  Date started: 12th Jan 2000.
  Developer:    Truong Hy.
}

Unit SPC_Unit;

Interface

Uses
    Windows,
    SysUtils;

{$I SPC_inc.pas}

Type
    T_Long_Byte=Record
                      LSB, LoMid, HiMid, MSB : Byte;
                End;
    T_Word_Byte=Record
                      LSB, MSB : Byte;
                End;
    T_dev_status_err_msg=Record
                               Dev_status_OK : Boolean; //Whether target device status is ok.
                               Dev_err_msg   : String;  //Target device error message.
                          End;
    T_sense_key_code_err_msg=Record
                                   Key_err_msg  : String;  //Sense key error message.
                                   Code_err_msg : String;  //Sense ASC and ASCQ error message.
                             End;
    T_sense_status_err_msg=Record
                                 Std_sense_exists : Boolean;
                                 Key_code_err_msg : T_sense_key_code_err_msg;
                           End;

Type T_SPC=
Class
Public
      { Some reverse endian byte order methodes.. }
      Procedure ReverseWordToBytes(    In_word  : Word;
                                   Var MSB_byte : Byte;
                                   Var LSB_byte : Byte);
      Function ReverseBytesToWord(MSB_byte : Byte;
                                  LSB_byte : Byte) : Word;
      Procedure ReverseLongWordToBytes(    LongVal    : LongWord;
                                       Var MSB_byte   : Byte;
                                       Var HiMid_byte : Byte;
                                       Var LoMid_byte : Byte;
                                       Var LSB_byte   : Byte); Overload;
      Procedure ReverseLongWordToBytes(    LongVal    : LongWord;
                                       Var HiMid_byte : Byte;
                                       Var LoMid_byte : Byte;
                                       Var LSB_byte   : Byte); Overload;
      Procedure ReverseLongIntToBytes(    LongVal    : LongInt;
                                      Var MSB_byte   : Byte;
                                      Var HiMid_byte : Byte;
                                      Var LoMid_byte : Byte;
                                      Var LSB_byte   : Byte);
      Function ReverseBytesToLongWord(MSB_byte   : Byte;
                                      HiMid_byte : Byte;
                                      LoMid_byte : Byte;
                                      LSB_byte   : Byte) : LongWord; Overload;
      Function ReverseBytesToLongInt(MSB_byte   : Byte;
                                     HiMid_byte : Byte;
                                     LoMid_byte : Byte;
                                     LSB_byte   : Byte) : LongInt;
      Function ReverseBytesToLongWord(MSB_byte : Byte;
                                      Mid_byte : Byte;
                                      LSB_byte : Byte) : LongWord; Overload;
      { Methods for filling CDBs with SCSI commands. }
      Procedure Fill_CDB6_test_unit_ready(Var CDB : T_test_unit_ready);
      Procedure Fill_CDB6_req_sense(Var CDB : T_req_sense);
      Procedure Fill_CDB6_get_dev_inquiry(Var CDB : T_dev_inq_CDB6);
      Procedure Fill_CDB6_mode_select(Var CDB               : T_mode_select_CDB6;
                                          In_param_list_len : Byte;
                                          In_save_params    : Boolean);
      Procedure Fill_CDB6_mode_sense(Var CDB               : T_mode_sense_CDB6;
                                         In_DBD            : Byte;
                                         In_PC_PageCode    : Byte;
                                         In_param_list_len : Byte);
      Procedure Fill_CDB10_mode_select(Var CDB               : T_mode_select_CDB10;
                                           In_param_list_len : Word;
                                           In_save_params    : Boolean);
      Procedure Fill_CDB10_mode_sense(Var CDB               : T_mode_sense_CDB10;
                                          In_DBD            : Byte;
                                          In_PC_PageCode    : Byte;
                                          In_param_list_len : Word);
      { Decode & verification of device status & sense error data.. }
      Function Verify_dev_status(In_dev_status : Byte) : T_dev_status_err_msg;
      Function Get_unknown_sense_code_err_msg(In_p_sense_area : Pointer) : String;
      Function Get_sense_key_code_err_msg(In_p_sense_area : Pointer) : T_sense_key_code_err_msg;
      Function Verify_sense_status(In_p_sense_area : Pointer) : T_sense_status_err_msg;
End;

implementation

Procedure T_SPC.ReverseWordToBytes(    In_word  : Word;
                                   Var MSB_byte : Byte;
                                   Var LSB_byte : Byte);
{ *************************************************************
  For retrieving the 2 bytes of a word value.  Returns the
  2 byte values.  SCSI specs require ordering of high byte
  first to low byte, while intel CPUs ordering are of low byte
  first to high byte.  Basically this allows you to get at the
  2 bytes of a word value separately and pass it to the
  SRB (SCSI Request Block) in the correct order.
  ************************************************************* }
Begin
     MSB_byte:=T_Word_Byte(In_word).MSB;
     LSB_byte:=T_Word_Byte(In_word).LSB;
End;

Function T_SPC.ReverseBytesToWord(MSB_byte : Byte;
                                  LSB_byte : Byte) : Word;
{ ************************************************************
  For joining 2 byte values into a single word value and
  return the result as a word.  SCSI specs require ordering of
  high byte first to low byte, while Intel CPUs ordering are
  of low byte first to high byte.  Basically this allows a
  word value to be constructed from separate bytes so that the
  word value is in Intel CPU ordering.
  ************************************************************ }
Var WordVal : Word;
Begin
     T_Word_Byte(WordVal).MSB:=MSB_byte;
     T_Word_Byte(WordVal).LSB:=LSB_byte;

     ReverseBytesToWord:=WordVal;
End;

Procedure T_SPC.ReverseLongWordToBytes(    LongVal    : LongWord;
                                       Var MSB_byte   : Byte;
                                       Var HiMid_byte : Byte;
                                       Var LoMid_byte : Byte;
                                       Var LSB_byte   : Byte);
{ **************************************************************
  For retrieving the 4 bytes of a long word value.  Returns the
  4 byte values.  SCSI specs require ordering of high byte first
  to low byte, while intel CPUs ordering are of low byte first
  to high byte.  Basically this allows you to get at the 4 bytes
  of a long word value separately and pass it to the
  SRB (SCSI Request Block) in the correct order.
  ************************************************************** }
Begin
     MSB_byte:=T_Long_Byte(LongVal).MSB;
     HiMid_byte:=T_Long_Byte(LongVal).HiMid;
     LoMid_byte:=T_Long_Byte(LongVal).LoMid;
     LSB_byte:=T_Long_Byte(LongVal).LSB;
End;

Procedure T_SPC.ReverseLongWordToBytes(    LongVal    : LongWord;
                                       Var HiMid_byte : Byte;
                                       Var LoMid_byte : Byte;
                                       Var LSB_byte   : Byte);
{ *************************************************************
  For retrieving the 3 bytes of a long word value.  Returns the
  lower 3 byte values.  SCSI specs require ordering of high
  byte first to low byte, while intel CPUs ordering are of low
  byte first to high byte.  Basically this allows you to get at
  the 3 bytes of a long word value separately and pass it to
  the SRB (SCSI Request Block) in the correct order.
  Parameter overload version.
  ************************************************************* }
Begin
     HiMid_byte:=T_Long_Byte(LongVal).HiMid;
     LoMid_byte:=T_Long_Byte(LongVal).LoMid;
     LSB_byte:=T_Long_Byte(LongVal).LSB;
End;

Procedure T_SPC.ReverseLongIntToBytes(    LongVal    : LongInt;
                                      Var MSB_byte   : Byte;
                                      Var HiMid_byte : Byte;
                                      Var LoMid_byte : Byte;
                                      Var LSB_byte   : Byte);
{ **************************************************************
  For retrieving the 4 bytes of a long int value.  Returns the
  4 byte values.  SCSI specs require ordering of
  high byte first to low byte, while intel CPUs ordering are of
  low byte first to high byte.  Basically this allows you to
  get at the 4 bytes of a long int value separately and pass it
  to the SRB (SCSI Request Block) in the correct order.
  ************************************************************** }
Begin
     MSB_byte:=T_Long_Byte(LongVal).MSB;
     HiMid_byte:=T_Long_Byte(LongVal).HiMid;
     LoMid_byte:=T_Long_Byte(LongVal).LoMid;
     LSB_byte:=T_Long_Byte(LongVal).LSB;
End;

Function T_SPC.ReverseBytesToLongWord(MSB_byte   : Byte;
                                      HiMid_byte : Byte;
                                      LoMid_byte : Byte;
                                      LSB_byte   : Byte) : LongWord;
{ ************************************************************
  For joining 4 byte values into a single long word value
  and return the result as a long word.  SCSI specs require
  ordering of high byte first to low byte, while Intel CPUs
  ordering are of low byte first to high byte.  Basically this
  allows a long word value to be constructed from separate
  bytes so that the long word is in Intel CPU ordering.
  ************************************************************ }
Var LongVal : LongWord;
Begin
     T_Long_Byte(LongVal).MSB:=MSB_byte;
     T_Long_Byte(LongVal).HiMid:=HiMid_byte;
     T_Long_Byte(LongVal).LoMid:=LoMid_byte;
     T_Long_Byte(LongVal).LSB:=LSB_byte;

     ReverseBytesToLongWord:=LongVal;
End;

Function T_SPC.ReverseBytesToLongInt(MSB_byte   : Byte;
                                     HiMid_byte : Byte;
                                     LoMid_byte : Byte;
                                     LSB_byte   : Byte) : LongInt;
{ ************************************************************
  For joining 4 byte values into a single long int value
  and return the result as a long int.  SCSI specs require
  ordering of high byte first to low byte, while Intel CPUs
  ordering are of low byte first to high byte.  Basically this
  allows a long int value to be constructed from separate
  bytes so that the long int is in Intel CPU ordering.
  ************************************************************ }
Var LongVal : LongInt;
Begin
     T_Long_Byte(LongVal).MSB:=MSB_byte;
     T_Long_Byte(LongVal).HiMid:=HiMid_byte;
     T_Long_Byte(LongVal).LoMid:=LoMid_byte;
     T_Long_Byte(LongVal).LSB:=LSB_byte;

     ReverseBytesToLongInt:=LongVal;
End;

Function T_SPC.ReverseBytesToLongWord(MSB_byte : Byte;
                                      Mid_byte : Byte;
                                      LSB_byte : Byte) : LongWord;
{ ************************************************************
  For joining 3 byte values into a single long word value
  and return the result as a long word.  SCSI specs require
  ordering of high byte first to low byte, while Intel CPUs
  ordering are of low byte first to high byte.  Basically this
  allows a long word value to be constructed from separate
  bytes so that the long word is in Intel CPU ordering.
  Parameter overload version.
  ************************************************************ }
Var LongVal : LongWord;
Begin
     T_Long_Byte(LongVal).HiMid:=MSB_byte;
     T_Long_Byte(LongVal).LoMid:=Mid_byte;
     T_Long_Byte(LongVal).LSB:=LSB_byte;

     ReverseBytesToLongWord:=LongVal;
End;

Procedure T_SPC.Fill_CDB6_test_unit_ready(Var CDB : T_test_unit_ready);
Begin
     CDB.Cmd:=SCSI_CMD_TEST_UNIT_READY;
     LongWord((@(CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve4.
     CDB.Control:=0;
End;

Procedure T_SPC.Fill_CDB6_req_sense(Var CDB : T_req_sense);
Begin
     CDB.Cmd:=SCSI_CMD_REQ_SENSE;
     Word((@(CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve2.
     CDB.Reserved3:=0;
     CDB.Alloc_len:=SizeOf(T_sense_data);
     CDB.Control:=0;
End;

Procedure T_SPC.Fill_CDB6_get_dev_inquiry(Var CDB : T_dev_inq_CDB6);
Begin
     CDB.Cmd:=SCSI_CMD_DEV_INQUIRY_CDB6;
     CDB.CmdDt_EVPD:=0;
     CDB.Pg_or_Op_code:=0;
     CDB.Reserved1:=0;
     CDB.Len:=36;
     CDB.Control:=0;
End;

Procedure T_SPC.Fill_CDB6_mode_select(Var CDB               : T_mode_select_CDB6;
                                          In_param_list_len : Byte;
                                          In_save_params    : Boolean);
Begin
     CDB.Cmd:=SCSI_CMD_MODE_SELECT_CDB6;
     CDB.PF_SP:=MODE_SELECT_PF_STANDARD;
     If In_save_params Then
     Begin
          CDB.PF_SP:=CDB.PF_SP OR MODE_SELECT_SP_SAVE;
     End;
     Word((@(CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve2.
     CDB.Param_list_len:=In_param_list_len;
     CDB.Control:=0;
End;

Procedure T_SPC.Fill_CDB6_mode_sense(Var CDB               : T_mode_sense_CDB6;
                                         In_DBD            : Byte;
                                         In_PC_PageCode    : Byte;
                                         In_param_list_len : Byte);
Begin
     CDB.Cmd:=SCSI_CMD_MODE_SENSE_CDB6;
     CDB.DBD:=In_DBD;
     CDB.PC_PageCode:=In_PC_PageCode;
     CDB.Reserved1:=0;
     CDB.Param_list_len:=In_param_list_len;
     CDB.Control:=0;
End;

Procedure T_SPC.Fill_CDB10_mode_select(Var CDB               : T_mode_select_CDB10;
                                           In_param_list_len : Word;
                                           In_save_params    : Boolean);
Begin
     CDB.Cmd:=SCSI_CMD_MODE_SELECT_CDB10;
     CDB.PF_SP:=MODE_SELECT_PF_STANDARD;
     If In_save_params Then
     Begin
          CDB.PF_SP:=CDB.PF_SP OR MODE_SELECT_SP_SAVE;
     End;
     LongWord((@(CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve4.
     CDB.Reserved5:=0;
     ReverseWordToBytes(In_param_list_len,
                        CDB.Param_list_len_MSB,
                        CDB.Param_list_len_LSB);
     CDB.Control:=0;
End;

Procedure T_SPC.Fill_CDB10_mode_sense(Var CDB               : T_mode_sense_CDB10;
                                          In_DBD            : Byte;
                                          In_PC_PageCode    : Byte;
                                          In_param_list_len : Word);
Begin
     CDB.Cmd:=SCSI_CMD_MODE_SENSE_CDB10;
     CDB.DBD:=In_DBD;
     CDB.PC_PageCode:=In_PC_PageCode;
     LongWord((@(CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve4.
     ReverseWordToBytes(In_param_list_len,
                        CDB.Param_list_len_MSB,
                        CDB.Param_list_len_LSB);
     CDB.Control:=0;
End;

Function T_SPC.Verify_dev_status(In_dev_status : Byte) : T_dev_status_err_msg;
{ **************************************************************
  Verifies the SCSI device status and sets the Dev_status_OK
  to indicate whether there are any device errors:
       - Dev_status_OK=True means no HA errors detected
       - Dev_status_OK=False means errors
  If failure then also set Dev_err_msg with an error message
  string based on device status error code.
  ************************************************************** }
Var
   Dev_status_err_msg : T_dev_status_err_msg;
Begin
     Case In_dev_status Of
          DEV_ST_GOOD: //Device detected no errors.
          Begin
               Dev_status_err_msg.Dev_status_OK:=True;
          End;
          DEV_ST_CHKCOND:
          Begin
               Dev_status_err_msg.Dev_status_OK:=False;
               Dev_status_err_msg.Dev_err_msg:='Check condition status, i.e. check sense data';
          End;
          DEV_ST_CONDMET:
          Begin
               Dev_status_err_msg.Dev_status_OK:=True;
          End;
          DEV_ST_BUSY:
          Begin
               Dev_status_err_msg.Dev_status_OK:=False;
               Dev_status_err_msg.Dev_err_msg:='Target device is busy.';
          End;
          DEV_ST_INTERM:
          Begin
               Dev_status_err_msg.Dev_status_OK:=True;
          End;
          DEV_ST_INTERM_CONDMET:
          Begin
               Dev_status_err_msg.Dev_status_OK:=True;
          End;
          DEV_ST_RESCONF:
          Begin
               Dev_status_err_msg.Dev_status_OK:=False;
               Dev_status_err_msg.Dev_err_msg:='Target device was reserved.';
          End;
          DEV_ST_CMDTERM:
          Begin
               Dev_status_err_msg.Dev_status_OK:=False;
               Dev_status_err_msg.Dev_err_msg:='Command terminated.';
          End;
          DEV_ST_QFULL:
          Begin
               Dev_status_err_msg.Dev_status_OK:=True;
          End
     Else
         Begin
              Dev_status_err_msg.Dev_status_OK:=False;
              Dev_status_err_msg.Dev_err_msg:='Undefined or unknown device status code: ' +
                                              IntToStr(In_dev_status) +
                                              '.';
         End
     End;

     Verify_dev_status:=Dev_status_err_msg;
End;

Function T_SPC.Get_unknown_sense_code_err_msg(In_p_sense_area : Pointer) : String;
{ **********************************************************************
  Handle unknown sense key, ASC and ASCQ error code combination.
  It appends a generic unknown error message together with the sense key
  & codes to form a generic error message string.  The combination may
  be a specific error, but we don't know that (I am using the SCSI draft
  specs).
  ********************************************************************** }
Begin
     Get_unknown_sense_code_err_msg:='Unknown sense key code combination:'+
                                     Chr(10)+Chr(13)+
                                     'Sense key:'+IntToStr(T_sense_data(In_p_sense_area^).Flags_SenseKey AND $F)+
                                     Chr(10)+Chr(13)+
                                     'Additional sense code:'+IntToStr(T_sense_data(In_p_sense_area^).ASC)+
                                     Chr(10)+Chr(13)+
                                     'Additional sense code qualifier:'+
                                     IntToStr(T_sense_data(In_p_sense_area^).ASCQ)+
                                     '.';
End;

Function T_SPC.Get_sense_key_code_err_msg(In_p_sense_area : Pointer) : T_sense_key_code_err_msg;
{ ********************************************************************
  Handle all known sense key, ASC and ASCQ error code combinations.
  This is a very long exhaustive case list.  All cases in here must
  have an else for handling unknown combinations.  This method sets
  2 error messages.  These go into:
      - Key_err_msg  // This is for sense key error message
      - Code_err_msg  // This is for ASC and ASCQ error message
  Both of which are held in the custom type: T_sense_key_code_err_msg.
  ******************************************************************** }
Var
   Sense_key_code_err_msg : T_sense_key_code_err_msg;
Begin
     Case (T_sense_data(In_p_sense_area^).Flags_SenseKey AND $F) Of
          SEN_KEY_NO_SEN:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='No sense key info.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $00:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='No additional sense info.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_RECV_ERR:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Recovered or recoverable error.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $B:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Warning.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Warning-specified temperature exceeded.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Warning-enclosure degraded.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $17:
                    Begin
                         Sense_key_code_err_msg.Code_err_msg:='Recovered data-';
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with no error correction applied.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with retries.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with positive head offset.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with negative head offset.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with retries and/or CIRC applied.';
                              End;
                              $05:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'using prev sector ID.';
                              End;
                              $07:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'without ECC-recommend reassignment.';
                              End;
                              $08:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'without ECC-recommend rewrite.';
                              End;
                              $09:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'without ECC-data rewritten.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $18:
                    Begin
                         Sense_key_code_err_msg.Code_err_msg:='Recovered data-';
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with error correction applied.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with error corr. and retries applied.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'data auto-reallocated.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with CIRC.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'with L-EC.';
                              End;
                              $05:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'recommend reassignment.';
                              End;
                              $06:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'recommend rewrite.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $37:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Rounded parameter.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $73:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Power calibration area almost full.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_NOT_READY:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Logical unit not ready error.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $04:
                    Begin
                         Sense_key_code_err_msg.Code_err_msg:='Logical unit is not ready, ';
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'cause not reportable.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical unit is in the process of becoming ready.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'init command required.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'manual intervention required.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'format in progress.';
                              End;
                              $07:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'operation in progress.';
                              End;
                              $08:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'long write in progress.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $30:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Incompatible medium installed.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cannot read medium-unknown format.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cannot read medium-incompatible format.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cleaning cartridge installed.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cannot write medium-unknown format.';
                              End;
                              $05:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cannot write medium-incompatible format.';
                              End;
                              $06:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cannot format medium-incompatible medium.';
                              End;
                              $07:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cleaning failure.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $3A:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium not present.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium not present-tray closed.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium not present-tray open.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $3E:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical unit has not self configured yet.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                  Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_MEDIUM_ERR:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Medium error.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $02:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Seek error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $06:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='No reference position found.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $0C:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Write error.';
                              End;
                              $07:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Write error-recovery needed.';
                              End;
                              $08:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Write error-recovery failed.';
                              End;
                              $09:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Write error-loss of streaming.';
                              End;
                              $0A:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Write error-padding blocks added.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $11:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Unrecovered read error.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Read retries exhausted.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Error too long to correct.';
                              End;
                              $05:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='L-EC uncorrectable error.';
                              End;
                              $06:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='CIRC unrecovered error.';
                              End;
                              $0F:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Error reading UPC/EAN number.';
                              End;
                              $10:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Error reading ISRC number.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $15:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Random positioning error.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Mechanical positioning error.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Positioning error detected by read of medium.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $31:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium format corrupted.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Format command failed.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $51:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Erase failure.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $57:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Unable to recover TOC.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $72:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Session fixation error.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Session fixation error writing lead-in.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Session fixation error writing lead-out.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $73:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='CD control error.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Power calibration area is full.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Power calibration area error.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Program memory area update error.';
                              End;
                              $05:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Program memory area is full.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_HARD_ERR:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Hardware error.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $00:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $17:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cleaning requested.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $05:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical unit does not respond to selection.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $08:
                    Begin
                         Sense_key_code_err_msg.Code_err_msg:='Logical unit communication ';
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'failure.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'timeout.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:=Sense_key_code_err_msg.Code_err_msg+'parity error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $09:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Track following error.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Track servo failure.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Focus servo failure.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Spindle servo failure.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Head select fault.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $1B:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Synchronous transfer error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $3E:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical unit failure.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Timeout on logical unit.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $40:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $80..$FF:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Diagnostic failure on component:'+
                                                             IntToStr(T_sense_data(In_p_sense_area^).ASCQ)+
                                                             '.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $44:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Internal target failure.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $46:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Unsuccessful soft reset.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $47:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='SCSI parity error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $4A:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Command phase error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $4B:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Data phase error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $4C:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical unit failed self configuration.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $53:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Media load or eject failed.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $65:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Voltage fault.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_ILLEGAL_REQ:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Illegal request, error/s in parameters or cmd.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $00:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $11:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Audio play operation in progress.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $07:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Multiple peripheral devices selected.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $1A:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Parameter list length error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $20:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid command operation code.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $21:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical block address out of range.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid element address.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $24:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid field in CDB.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $25:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical unit not supported.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $26:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid field in parameter list.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Parameter not supported.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Parameter value invalid.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Threshold parameters not supported.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid release of active persistent reservation.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $27:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Write protected.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Hardware write protected.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Logical unit software write protected.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Associated write protect.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Persistent write protect.';
                              End;
                              $05:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Permanent write protect.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $2B:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Copy cannot execute since host cannot disconnect.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $2C:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Command sequence error.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Current program area is not empty.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Current program area is empty.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $30:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $08:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Cannot write-application code mismatch.';
                              End;
                              $09:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Current session not fixated for append.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $39:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Saving parameters not supported.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $3D:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid bits in identify message.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $43:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Message error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $53:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium removal prevented.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $63:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='End of user area encountered on this track.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Packet does not fit into available space.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $64:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Illegal mode for this track.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid packet size.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $72:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Session fixation error, incomplete track in session.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Empty or partially written reserved track.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_UNIT_ATT:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Unit attention error.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $0A:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Error log overflow.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $28:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Not ready to ready change, medium may have changed.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Import or export element accessed.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $29:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Power on, reset, or bus device reset occurred.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Power on occurred.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='SCSI bus reset occurred.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Bus device reset function occurred.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Device internal reset occurred.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $2A:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Parameters changed.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Mode parameters changed.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Log parameters changed.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Reservations preempted.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $2F:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Commands cleared by another initiator.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $3B:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $0D:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium destination element full.';
                              End;
                              $0E:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium source element empty.';
                              End;
                              $0F:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='End of medium reached.';
                              End;
                              $11:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium magazine not accessible.';
                              End;
                              $12:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium magazine removed.';
                              End;
                              $13:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium magazine inserted.';
                              End;
                              $14:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium magazine locked.';
                              End;
                              $15:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Medium magazine unlocked.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $3F:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Target operating conditions have changed.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Microcode have been changed.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Changed operating definition.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Inquiry data has changed.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $5A:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Operator request or state change input.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Operator medium removal request.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Operator selected write protect.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Operator selected write permit.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $5B:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Log exception.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Threshold condition met.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Log counter at maximum.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Log list codes exhausted.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $5D:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Failure prediction threshold exceeded.';
                              End;
                              $FF:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Failure prediction threshold exceeded (false)..';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $5E:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Low power condition on.';
                              End;
                              $01:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Idle condition activated by timer.';
                              End;
                              $02:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Standby condition activated by timer.';
                              End;
                              $03:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Idle condition activated by command.';
                              End;
                              $04:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Standby condition activated by command.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_DATA_PROT:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Data is protected for read or write access.';
               Sense_key_code_err_msg.Code_err_msg:='';
          End;
          SEN_KEY_BLANK_CHK:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Blank or formatted medium encountered.';
               Sense_key_code_err_msg.Code_err_msg:='';
          End;
          SEN_KEY_VEND_SPEC:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Vendor specific sense info.';
               Sense_key_code_err_msg.Code_err_msg:='';
          End;
          SEN_KEY_COPY_ABORT:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Copy, compare or copy & verify aborted due to errors.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $1D:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Miscompare during verify operation.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_ABORTED_CMD:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Command aborted by device server.';
               Case T_sense_data(In_p_sense_area^).ASC Of
                    $00:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $06:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='I/O process terminated.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $11:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $11:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Read error, loss of streaming.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $45:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Select or reselect failure.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $48:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Initiator detected error message received.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $49:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Invalid message error.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
                    $4D:
                    Begin
                         Sense_key_code_err_msg.Code_err_msg:='Tagged overlapped command queue tag:' +
                                                              IntToStr(T_sense_data(In_p_sense_area^).ASCQ)+
                                                              '.';
                    End;
                    $4E:
                    Begin
                         Case T_sense_data(In_p_sense_area^).ASCQ Of
                              $00:
                              Begin
                                   Sense_key_code_err_msg.Code_err_msg:='Overlapped commands attempted.';
                              End;
                         Else
                             Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
                         End;
                    End;
               Else
                   Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
               End;
          End;
          SEN_KEY_VOL_OVERFLW:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Medium write capacity reached error.';
               Sense_key_code_err_msg.Code_err_msg:='';
          End;
          SEN_KEY_MISCOMP:
          Begin
               Sense_key_code_err_msg.Key_err_msg:='Source does not match data read error.';
               Sense_key_code_err_msg.Code_err_msg:='';
          End;
     Else
     Begin
          Sense_key_code_err_msg.Key_err_msg:='Unknown or reserved sense key:'+
                                              IntToStr(T_sense_data(In_p_sense_area^).Flags_SenseKey AND $F)+
                                              '.';
          Sense_key_code_err_msg.Code_err_msg:=Get_unknown_sense_code_err_msg(In_p_sense_area);
     End;
     End;
     Get_sense_key_code_err_msg:=Sense_key_code_err_msg;
End;

Function T_SPC.Verify_sense_status(In_p_sense_area : Pointer) : T_sense_status_err_msg;
{ **************************************************************
  Verifies the SCSI sense status:
       - response code,
       - sense key,
       - ASC and ASCQ codes
  The Std_sense_exists is set to indicate whether the sense data
  is standard to SCSI-3 definition:
       - Std_sense_exists=True means standard
       - Std_sense_exists=False means vendor specific
  If failure then
       - Key_err_msg,
       - Code_err_msg
  are set with a standard vendor specific error message string,
  because we don't know what the actual error is.
  ************************************************************** }
Var
   Sense_status_err_msg : T_sense_status_err_msg;
Begin
     //Do we have any sense data after the header?
     If T_sense_data(In_p_sense_area^).AddLen>=6 Then
     Begin
          Case (T_sense_data(In_p_sense_area^).ResponseCode AND $7F) Of
               {** Deal with errors relating to current cmd. **}
               SEN_RESP_CURR_ERR:
               Begin
                    Sense_status_err_msg.Std_sense_exists:=True;
                    Sense_status_err_msg.Key_code_err_msg:=Get_sense_key_code_err_msg(In_p_sense_area);
               End;
               {** Deal with errors relating to previous cmd. **}
               SEN_RESP_DEFER_ERR:
               Begin
                    Sense_status_err_msg.Std_sense_exists:=True;
                    Sense_status_err_msg.Key_code_err_msg:=Get_sense_key_code_err_msg(In_p_sense_area);
                    Sense_status_err_msg.Key_code_err_msg.Code_err_msg:='Defer error for prev cmd. '+
                                                                        Chr(10)+Chr(13)+
                                                                        Sense_status_err_msg.Key_code_err_msg.Code_err_msg;
               End;

               SEN_RESP_VENDOR_SPEC:
               Begin
                    Sense_status_err_msg.Std_sense_exists:=False;
                    Sense_status_err_msg.Key_code_err_msg.Key_err_msg:='Not handled due to vendor specific response error.';
                    Sense_status_err_msg.Key_code_err_msg.Code_err_msg:='Vendor specific sense error.';
               End;
          Else
          Begin
               Sense_status_err_msg.Std_sense_exists:=False;
               Sense_status_err_msg.Key_code_err_msg.Key_err_msg:='Not handled due to unknown or reserved response error.';
               Sense_status_err_msg.Key_code_err_msg.Code_err_msg:='Unknown or reserved sense response error:'+
                                                                   IntToStr(T_sense_data(In_p_sense_area^).ResponseCode AND $7F)+
                                                                   '.';
          End;
          End;
     End
     Else
         Begin
              Sense_status_err_msg.Std_sense_exists:=False;
              Sense_status_err_msg.Key_code_err_msg.Key_err_msg:='No sense data returned.';
              Sense_status_err_msg.Key_code_err_msg.Code_err_msg:='No sense error data available.';
         End;
     Verify_sense_status:=Sense_status_err_msg;
End;

end.
