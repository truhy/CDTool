{ Name:         SBC_Unit.PAS
  File type:    Borland Delphi 4 unit file.
  Description:  Contains a class which encapsulates the SCSI-3 SBC (for block
                SCSI device) functions.  This source file uses information given
                from the Adaptec's ASPI SDK and working draft version of SCSI-3
                SBC (SCSI Block Commands) specification document.  These
                commands are for devices which works with blocks (sectors).
  Notes:        Written from scratch, no other samples used, except SDK.
                All function and parameters are in Intel x86 byte ordering,
                i.e. from LSB to MSB.
  Date started: 2nd Jan 2003.
  Developer:    Truong Hy.
}
unit SBC_Unit;

interface

Uses
    SPC_Unit;

{$I SBC_inc.pas}

Type T_SBC=
Class
Private
       SPC : T_SPC; //SCSI-3 SPC instance.
Public
      Constructor Create;
      Destructor Destroy; Override;
      Procedure Fill_CDB6_start_stop_unit(Var Out_CDB : T_start_stop_unit_CDB6;
                                              In_Reserved1_IMMED : Byte;
                                              In_Power_Reserved4_LOEJ_Start : Byte);
      Procedure Fill_CDB10_do_seek(Var Out_CDB : T_seek_CDB10;
                                       In_MMCLBA : LongInt);
End;

implementation

Constructor T_SBC.Create;
Begin
     //Inherited Create;

     SPC:=T_SPC.Create;
End;

Destructor T_SBC.Destroy;
Begin
     SPC.Destroy;

     //Inherited;
End;

Procedure T_SBC.Fill_CDB6_start_stop_unit(Var Out_CDB : T_start_stop_unit_CDB6;
                                              In_Reserved1_IMMED : Byte;
                                              In_Power_Reserved4_LOEJ_Start : Byte);
{ *****************************************************
  Fills a CDB with start stop unit CDB6 command values.
  ***************************************************** }
Begin
     Out_CDB.Cmd:=SBC_CMD_START_STOP_UNIT;
     Out_CDB.Reserved1_IMMED:=In_Reserved1_IMMED;
     Out_CDB.Reserved2:=0;
     Out_CDB.Reserved3:=0;
     Out_CDB.Power_Reserved4_LOEJ_Start:=In_Power_Reserved4_LOEJ_Start;
     Out_CDB.Control:=0;
End;

Procedure T_SBC.Fill_CDB10_do_seek(Var Out_CDB : T_seek_CDB10;
                                       In_MMCLBA : LongInt);
{ *******************************************
  Fills a CDB with Seek CDB10 command values.
  ******************************************* }
Begin
     Out_CDB.Cmd:=SBC_CMD_SEEK_CDB10;
     Out_CDB.Reserved1:=0;
     SPC.ReverseLongIntToBytes(In_MMCLBA,
                               Out_CDB.MMCLBA_HiByte,
                               Out_CDB.MMCLBA_HiMiByte,
                               Out_CDB.MMCLBA_LoMiByte,
                               Out_CDB.MMCLBA_LoByte);
     Out_CDB.Reserved2:=0;
     Out_CDB.Reserved3:=0;
     Out_CDB.Reserved4:=0;
     Out_CDB.Control:=0;
End;

end.
