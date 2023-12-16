{ ****************************************************************
  A thread for displaying the hardware buffer status periodically.
  **************************************************************** }

unit ImageToCD_DispStatus_Thread_Unit;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Math,
  SCSIUnit,
  MMC1Unit,
  SPC_Unit,
  WNASPI32Unit,
  CommonCDSettingsUnit,
  Tools_Unit,
  CDROM_struct_Unit;

type
  T_ImageToCD_DispStatus_Thread = class(TThread)
  private
    { Private declarations }
    SCSI : T_SCSI;
  public
    Constructor Create;
    procedure Free;
    procedure Execute; override;
    procedure Display_status;
  end;

implementation

Uses
    MainFormUnit;

Constructor T_ImageToCD_DispStatus_Thread.Create;
Begin
     Inherited Create(False);  { Call the original create method. }

     SCSI:=nil;
End;

procedure T_ImageToCD_DispStatus_Thread.Free;
begin
     SCSI.Free;

     Inherited;
end;

procedure T_ImageToCD_DispStatus_Thread.Execute;
Begin
     //Create another SCSI object and set it to use the currently selected drive.
     If Form1.CommonCDSettings.Create_new_SCSI_obj(SCSI) Then
     Begin
          While(Terminated=False) Do
          Begin
               Display_status;
               Sleep(1000);
          End;
     End;
End;

procedure T_ImageToCD_DispStatus_Thread.Display_status;
Var s                  : String;
    drive_buflen       : LongWord;
    blank_drive_buflen : LongWord;
    used_drive_buflen  : LongWord;
Begin
     SCSI.MMC1_any_link.Do_read_buf_cap_CDB10;
     If SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          drive_buflen:=
          SCSI.SPC_any_link.SPC.ReverseBytesToLongWord(
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).BufLen_HiByte,
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).BufLen_HiMiByte,
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).BufLen_LoMiByte,
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).BufLen_LoByte);

          blank_drive_buflen:=
          SCSI.SPC_any_link.SPC.ReverseBytesToLongWord(
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).Blank_BufLen_HiByte,
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).Blank_BufLen_HiMiByte,
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).Blank_BufLen_LoMiByte,
          T_read_buf_cap_CDB10_block(SCSI.SPC_any_link.Get_data_buf^).Blank_BufLen_LoByte);

          used_drive_buflen:=drive_buflen - blank_drive_buflen;
          s:='Used: ' +
             IntToStr(used_drive_buflen) +
             ' bytes out of ' +
             IntToStr(drive_buflen);
          If drive_buflen>0 Then
          Begin
               s:=s+
                  ', ' +
                  FloatToStrF(used_drive_buflen / drive_buflen * 100, ffFixed, 7, 2) +
                  '%';
          End;

          If Not Terminated Then
          Begin
               Form1.Form5.ImageToCDProgressForm.LBL_drive_buffer_status.Caption:=s;
          End;
     End;
End;

end.
