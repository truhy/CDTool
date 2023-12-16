unit ReadCDSectorThreadUnit;

interface

uses
    Classes,
    Tools_Unit,
    MMC1Unit,
    SPC_Unit,
    CommonCDSettingsUnit;

Type
    T_Method=procedure(s : String) of object;

type
T_ReadCDSectorThread=
class(TThread)
private
      { Private declarations }
      sender_method : T_Method;
      StartSect     : LongInt;
      SubChSelMode  : Byte;
      is_TEB        : Boolean;
      is_no_ErrMsg  : Boolean;
      err_msg       : String;
      Procedure ShowErrMsg;
protected
      procedure Execute; override;
public
      Constructor Create(thread_done_proc : TNotifyEvent;
                         in_method        : T_Method;
                         in_StartSect     : LongInt;
                         in_SubChSelMode  : Byte;
                         in_is_TEB        : Boolean;
                         in_is_no_ErrMsg  : Boolean);
      Procedure ReadCDSector;
end;

implementation

Uses
    MainFormUnit;

Constructor T_ReadCDSectorThread.Create(thread_done_proc : TNotifyEvent;
                                        in_method        : T_Method;
                                        in_StartSect     : LongInt;
                                        in_SubChSelMode  : Byte;
                                        in_is_TEB        : Boolean;
                                        in_is_no_ErrMsg  : Boolean);
Begin
     sender_method:=in_method;
     StartSect:=in_StartSect;
     SubChSelMode:=in_SubChSelMode;
     is_TEB:=in_is_TEB;
     is_no_ErrMsg:=in_is_no_ErrMsg;

     OnTerminate:=thread_done_proc;
     FreeOnTerminate := True;
     Inherited Create(False);
End;

Procedure T_ReadCDSectorThread.ShowErrMsg;
Begin
     sender_method(err_msg);
End;

Procedure T_ReadCDSectorThread.ReadCDSector;
Var
    Data_1block_size  : Word;
    Sector_block_size : Word;
    SubCh_block_size  : Byte;
    HSA_sector        : LongInt;
    SubCh_buf         : ^Byte;
    sub_int_PW_96     : T_sub_int_PW_96;
Begin
     Sector_block_size:=Form1.SCSI.MMC1_any_link.Get_MMC1.GetBlockSizeFromFilterReadFormat(MMC_SECTORTYPE_ANY,
                                                                                           MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC);
     SubCh_block_size:=Form1.SCSI.MMC1_any_link.Get_MMC1.GetBlockSizeFromSubCh(SubChSelMode);
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Form1.SCSI.SPC_any_link.Zero_data_buf;

     {
     //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
     If StartSect>=404850 Then
         HSA_sector:=StartSect-450000
     else
         HSA_sector:=StartSect;
     }
     HSA_sector:=StartSect;

     Form1.SCSI.MMC1_any_link.Do_readCD_byFormat_CDB12(HSA_sector,
                                                       1,
                                                       MMC_SECTORTYPE_ANY,
                                                       MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC,
                                                       SubChSelMode);
     If Not (Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK OR
             (is_TEB AND
              ((Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_MEDIUM_ERR) OR
               (Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_RECV_ERR)))) Then
     Begin
          If (NOT is_no_ErrMsg) Then
          Begin
               err_msg:='Error while processing ReadCD command.' + Chr(10) + Chr(13) +
                        Form1.SCSI.MMC1_any_link.Get_err_msg;
               Synchronize(ShowErrMsg);
          End;
     End;
End;

procedure T_ReadCDSectorThread.Execute;
begin
     ReadCDSector;
end;

end.
