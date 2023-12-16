unit SBC_any_link_Unit;

interface

Uses
    SPC_WNASPI32_Unit,
    SPC_SPT_Unit,
    SBC_Unit;

Type T_SBC_any_link=
Class
Protected
      SPC_WNASPI32 : T_SPC_WNASPI32; //SCSI-3 SPC & Win ASPI 32 combined instance
      SPC_SPT : T_SPC_SPT; //SPC & DeviceIOCTL SPT combined instance.
Public
      SBC : T_SBC; //SCSI-3 SBC instance

      Function Get_err_msg : PChar; virtual; abstract;
      Procedure Start_Stop_Unit_CDB6(In_Reserved1_IMMED : Byte;
                                     In_Power_Reserved4_LOEJ_Start : Byte); virtual; abstract;
      Procedure Do_seek_CDB10(In_MMCLBA : LongInt); virtual; abstract;
End;

implementation

end.
 