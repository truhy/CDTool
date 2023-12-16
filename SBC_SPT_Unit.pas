unit SBC_SPT_Unit;

interface

Uses
    SBC_any_link_Unit,
    SPC_SPT_Unit,
    SBC_Unit,
    SPT_Unit,
    SPC_Unit;

Type T_SBC_SPT=
Class(T_SBC_any_link)
Public
      Constructor Create(Var In_SPC_SPT : T_SPC_SPT);
      Destructor Destroy; Override;

      Function Get_err_msg : PChar; Override;
      Procedure Start_Stop_Unit_CDB6(In_Reserved1_IMMED : Byte;
                                     In_Power_Reserved4_LOEJ_Start : Byte); Override;
      Procedure Do_seek_CDB10(In_MMCLBA : LongInt); Override;
End;

implementation

Constructor T_SBC_SPT.Create(Var In_SPC_SPT : T_SPC_SPT);
Begin
     Inherited Create;

     SBC:=T_SBC.Create;
     SPC_SPT:=In_SPC_SPT;
End;

Destructor T_SBC_SPT.Destroy;
Begin
     SBC.Destroy;
     
     Inherited;
End;

Function T_SBC_SPT.Get_err_msg : PChar;
{ ************************************
  A wrapper to Get_err_msg in SPC_SPT.
  ************************************ }
Begin
     Result:=SPC_SPT.Get_err_msg;
End;

Procedure T_SBC_SPT.Start_Stop_Unit_CDB6(In_Reserved1_IMMED : Byte;
                                         In_Power_Reserved4_LOEJ_Start : Byte);
{ ***************************************************************************
  Sends start stop unit CDB6 command to SCSI device - current selected device
  in SPC_SPT.SPT.
  *************************************************************************** }
Begin
     SPC_SPT.SPT.Fill_spt(SCSI_IOCTL_DATA_OUT, 0, 6);
     SBC.Fill_CDB6_start_stop_unit(T_start_stop_unit_CDB6(SPC_SPT.SPT.Get_p_Cdb^),
                                   In_Reserved1_IMMED,
                                   In_Power_Reserved4_LOEJ_Start);
     SPC_SPT.V_Send_SCSI_cmd;
End;

Procedure T_SBC_SPT.Do_seek_CDB10(In_MMCLBA : LongInt);
{ ********************************************************************
  Sends seek CDB10 command to SCSI device - current selected device in
  SPC_SPT.SPT.
  ******************************************************************** }
Begin
     SPC_SPT.SPT.Fill_spt(SCSI_IOCTL_DATA_UNSPECIFIED, 0, 10);
     SBC.Fill_CDB10_do_seek(T_seek_CDB10(SPC_SPT.SPT.Get_p_Cdb^),
                            In_MMCLBA);
     SPC_SPT.V_Send_SCSI_cmd;
End;

end.
 