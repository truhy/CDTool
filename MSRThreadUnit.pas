unit MSRThreadUnit;

interface

uses
  Classes,
  SysUtils,
  Windows,
  MMC1Unit,
  SPC_Unit,
  CommonCDSettingsUnit;

type
  TMSRThread = class(TThread)
  private
    { Private declarations }
    First_sector      : LongInt;
    Last_sector       : LongInt;
    save_speed        : Boolean;
    elasped_times_str : TStrings;
    command_to_MSR    : Byte;
    SubChSelMode      : Byte;
    s : String;
    procedure ShowErrMsg;
  protected
    procedure Execute; override;
    procedure Measure;
  public
    Constructor Create(Thread_done_proc     : TNotifyEvent;
                       in_First_sector      : LongInt;
                       in_Last_sector       : LongInt;
                       in_save_speed        : Boolean;
                       in_elasped_times_str : TStrings;
                       in_command_to_MSR : Byte;
                       in_SubChSelMode : Byte);
  end;

implementation

Uses
    MainFormUnit;

Constructor TMSRThread.Create(Thread_done_proc     : TNotifyEvent;
                              in_First_sector      : LongInt;
                              in_Last_sector       : LongInt;
                              in_save_speed        : Boolean;
                              in_elasped_times_str : TStrings;
                              in_command_to_MSR : Byte;
                              in_SubChSelMode : Byte);
Begin
     First_sector     :=in_First_sector;
     Last_sector      :=in_Last_sector;
     save_speed       :=in_save_speed;
     elasped_times_str:=in_elasped_times_str;
     command_to_MSR:=in_command_to_MSR;
     SubChSelMode:=in_SubChSelMode;

     OnTerminate:=Thread_done_proc;
     FreeOnTerminate := True;
     Inherited Create(False);
End;

procedure TMSRThread.Execute;
begin
     Measure;
end;

Procedure TMSRThread.ShowErrMsg;
Begin
     Form1.MSR_form.MSRProgressForm.ShowErrMsg(s);
End;

Procedure TMSRThread.Measure;
Var
   HSA_sector : LongInt;
   Curr_sect : LongInt;
   i : Integer;
   prev_diff : Int64;
   time_unit : Int64;
   prev_time : Int64;
   new_time : Int64;
   microsec_conv_ratio : Extended;
   microsec_elasped_time : Extended;
   Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;
Begin
     QueryPerformanceFrequency(time_unit);

     //Conversion ratio for converting to micro seconds.
     microsec_conv_ratio:=time_unit/1000000;

     Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          Form1.MSR_form.MSRProgressForm.LBL_curr_read_speed.Caption:=IntToStr(Out_CD_cap_mech_st.CurrReadSpeed);
     End;

     Curr_sect:=First_sector;
     Repeat
           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           If Curr_sect>=404850 Then
               HSA_sector:=Curr_sect-450000
           Else
               HSA_sector:=Curr_sect;

           Form1.MSR_form.MSRProgressForm.Sect_reading_disp_Lbl.Caption:=IntToStr(Curr_sect);

           Case command_to_MSR Of
           0:
           Begin
                QueryPerformanceCounter(prev_time);
                Form1.SCSI.MMC1_any_link.Do_readCD_byFormat_CDB12(HSA_sector,
                                                                  1,
                                                                  MMC_SECTORTYPE_ANY,
                                                                  MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC,
                                                                  SubChSelMode);
                QueryPerformanceCounter(new_time);
           End;
           1:
           Begin
                QueryPerformanceCounter(prev_time);
                Form1.SCSI.SBC_any_link.Do_seek_CDB10(HSA_sector);
                QueryPerformanceCounter(new_time);
           End
           End;

           {If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
           Begin
                s:='1,';
           End
           Else
           Begin
                s:='0,';
           End;}

           microsec_elasped_time:=(new_time-prev_time)/microsec_conv_ratio;

           s:=FloatToStrF(microsec_elasped_time, ffFixed, 18, 2);

           If save_speed Then
           Begin
                Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
                If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
                Begin
                     s:=s+','+IntToStr(Out_CD_cap_mech_st.CurrReadSpeed);
                End;
           End;

           elasped_times_str.Add(s);

           Curr_sect:=Curr_sect+1;
     Until (Curr_sect>Last_sector) Or Terminated;
End;

end.
