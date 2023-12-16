unit SBC_WNASPI32_Unit;

interface

Uses
    SBC_any_link_Unit,
    SPC_WNASPI32_Unit,
    SBC_Unit,
    WNASPI32Unit,
    SPC_Unit;

{$I SBC_WNASPI32_inc.pas}

Type T_SBC_WNASPI32=
Class(T_SBC_any_link)
Public
      Constructor Create(Var In_SPC_WNASPI32 : T_SPC_WNASPI32);
      Destructor Destroy; Override;

      Function Get_err_msg : PChar; Override;
      Procedure Start_Stop_Unit_CDB6(In_Reserved1_IMMED : Byte;
                                     In_Power_Reserved4_LOEJ_Start : Byte); Override;
      Procedure Do_seek_CDB10(In_MMCLBA : LongInt); Override;
End;

implementation

Constructor T_SBC_WNASPI32.Create(Var In_SPC_WNASPI32 : T_SPC_WNASPI32);
Begin
     Inherited Create;

     SBC:=T_SBC.Create;
     SPC_WNASPI32:=In_SPC_WNASPI32;
End;

Destructor T_SBC_WNASPI32.Destroy;
Begin
     SBC.Destroy;
     
     Inherited;
End;

Function T_SBC_WNASPI32.Get_err_msg : PChar;
{ **********************************************************************
  Gets the most appropriate (originator) error message.  Errors can
  orginate from the following (in order of highest layer first to lowest
  layer):
       - Win ASPI32,
       - the host adapter
       - or the target device (including sense error data)
  Errors from lower layers will propagate to higher layers,
  for example:
  a device error of "Target device is busy" will cause the Win ASPI32 to
  report "SRB completed with error".  Sometimes we do not want to display
  this error message, instead we may want to display the device error
  message only.

  Note that device specific or detail errors are reported by sense data
  and not the device status, the device status will be set to
  "Check condition" if there is a device specific error other than busy.
  **********************************************************************}
Begin
     Result:=SPC_WNASPI32.Get_err_msg;
End;

Procedure T_SBC_WNASPI32.Start_Stop_Unit_CDB6(In_Reserved1_IMMED : Byte;
                                              In_Power_Reserved4_LOEJ_Start : Byte);
{ ***************************************************************************
  Sends start stop unit CDB6 command to SCSI device - current selected device
  in SPC_WNASPI.WNASPI.
  *************************************************************************** }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_DIR_OUT, 0, 6);
     SBC.Fill_CDB6_start_stop_unit(T_SRB_for_start_stop_unit_CDB6(SPC_WNASPI32.WNASPI32.P_SRB^).Start_stop_unit_CDB6,
                                   In_Reserved1_IMMED,
                                   In_Power_Reserved4_LOEJ_Start);
     SPC_WNASPI32.V_Send_SRBCDB;
End;

Procedure T_SBC_WNASPI32.Do_seek_CDB10(In_MMCLBA : LongInt);
{ ********************************************************************
  Sends seek CDB10 command to SCSI device - current selected device in
  SPC_WNASPI.WNASPI.
  ******************************************************************** }
Begin
     SPC_WNASPI32.WNASPI32.MinZeroSRB; //Fill parts of SRB with zeroes.
     SPC_WNASPI32.WNASPI32.Fill_SRB_exec_SCSI(SRB_NO_DATA, 0, 10);
     SBC.Fill_CDB10_do_seek(T_SRB_for_seek_CDB10(SPC_WNASPI32.WNASPI32.P_SRB^).Seek_CDB10,
                            In_MMCLBA);
     SPC_WNASPI32.V_Send_SRBCDB;
End;

end.
