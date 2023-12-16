{ Name:         SCSIUnit.PAS
  File type:    Borland Delphi 4 unit file.
  Description:  Contains a class which encapsulates SCSI communications.
                This class combines the SCSI related classes together,
                allowing for parameters and functions to be shared with
                reduced parameter passing.
  Notes:        Written from scratch.
  Date started: 12th Jan 2000.
  Developer:    Truong Hy.
}

unit SCSIUnit;

interface

Uses
    WNASPI32Unit,
    SPC_WNASPI32_Unit,
    SBC_WNASPI32_Unit,
    MMC1_WNASPI32_Unit,
    SPT_Unit,
    SBC_SPT_Unit,
    MMC1_SPT_Unit,
    SPC_SPT_Unit,
    SPC_any_link_Unit,
    SBC_any_link_Unit,
    MMC1_any_link_Unit;

const U_METHOD_WNASPI=1;
const U_METHOD_SCSI_PASS_THRU=2;
const U_METHOD_SCSI_PASS_THRU_D=3;

Type T_SCSI=Class
            Private
                  WNASPI32      : T_WNASPI32;
                  SPC_WNASPI32  : T_SPC_WNASPI32;
                  SBC_WNASPI32  : T_SBC_WNASPI32;
                  MMC1_WNASPI32 : T_MMC1_WNASPI32;

                  SPT           : T_SPT;
                  SPC_SPT       : T_SPC_SPT;
                  SBC_SPT       : T_SBC_SPT;
                  MMC1_SPT      : T_MMC1_SPT;
            Public
                  interface_method : byte;
                  SPC_any_link  : T_SPC_any_link;
                  SBC_any_link  : T_SBC_any_link;
                  MMC1_any_link : T_MMC1_any_link;

                  Constructor Create(in_interface_method : byte);
                  Destructor Destroy; Override;
            End;

implementation

Constructor T_SCSI.Create(in_interface_method : byte);
Begin
     interface_method:=in_interface_method;

     //SPC_any_link:=T_SPC_any_link.Create;
     //SBC_any_link:=T_SBC_any_link.Create;
     //MMC1_any_link:=T_MMC1_any_link.Create;

     Case interface_method Of
     U_METHOD_WNASPI:
     Begin
          //Specific to Win ASPI 32..
          WNASPI32:=T_WNASPI32.Create;
          SPC_WNASPI32:=T_SPC_WNASPI32.Create(WNASPI32);
          SBC_WNASPI32:=T_SBC_WNASPI32.Create(SPC_WNASPI32);
          MMC1_WNASPI32:=T_MMC1_WNASPI32.Create(SPC_WNASPI32);

          //Dynamically bind Win APSPI 32 interfaces to generic SCSI interfaces
          SPC_any_link:=SPC_WNASPI32;
          SBC_any_link:=SBC_WNASPI32;
          MMC1_any_link:=MMC1_WNASPI32;
     End;
     U_METHOD_SCSI_PASS_THRU:
     Begin
          //Specific to DeviceIOCTL with SPT..
          SPT:=T_SPT.Create;
          SPC_SPT:=T_SPC_SPT.Create(SPT);
          SBC_SPT:=T_SBC_SPT.Create(SPC_SPT);
          MMC1_SPT:=T_MMC1_SPT.Create(SPC_SPT);

          //Dynamically bind DeviceIOCTL SPT interfaces to generic SCSI interfaces
          SPC_any_link:=SPC_SPT;
          SBC_any_link:=SBC_SPT;
          MMC1_any_link:=MMC1_SPT;
     End;
     End;
End;

Destructor T_SCSI.Destroy;
Begin
     //SPC_any_link.Free;
     //SBC_any_link.Free;
     //MMC1_any_link.Free;
     
     Case interface_method Of
     U_METHOD_WNASPI:
     Begin
          MMC1_WNASPI32.Free;
          SBC_WNASPI32.Free;
          SPC_WNASPI32.Free;
          WNASPI32.Free;
     End;
     U_METHOD_SCSI_PASS_THRU:
     Begin
          MMC1_SPT.Free;
          SBC_SPT.Free;
          SPC_SPT.Free;
          SPT.Free;
     End;
     End;
End;

end.
