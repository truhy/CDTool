{ *******************************************************************
  A thread for writing data from a CD image to write to a CDR/W disk.
  ******************************************************************* }

unit ImageToCDWriteThreadUnit;

interface

{ Uncomment below for debug mode. The data is not sent to the drive, instead
  they are written to a file: W_OUTPUT.BIN. }
//{$DEFINE DEBUG}

uses
  Windows,
  Classes,
  SysUtils,
  Math,
  MMC1Unit,
  SPC_Unit,
  CommonCDSettingsUnit,
  ImageToCD_DispStatus_Thread_Unit;

type Ptr_text_file=^TextFile;

type T_P_byte=^Byte;

type
  TImageToCDWriteThread = class(TThread)
  private
    { Private declarations }
    procedure ShowErrMsg;
    procedure ShowConfirmToMsg;
  public
    confirm_from_msg : Boolean;
    write_speed : Word;
    {$IFDEF DEBUG}
    debugf : TFileStream;
    {$ENDIF}
    s  : String;
    Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;
    len_of_write_data : LongWord;
    ImageToCD_DispStatus_Thread : T_ImageToCD_DispStatus_Thread;

    Constructor Create(in_write_speed       : Word;
                       in_len_of_write_data : LongWord;
                       in_ImageToCD_DispStatus_Thread : T_ImageToCD_DispStatus_Thread);

    procedure Execute; override;
    procedure Write_remaining_cache;
  end;

implementation

Uses
    MainFormUnit;

Constructor TImageToCDWriteThread.Create(in_write_speed       : Word;
                                         in_len_of_write_data : LongWord;
                                         in_ImageToCD_DispStatus_Thread : T_ImageToCD_DispStatus_Thread);
Begin
     write_speed:=in_write_speed;
     len_of_write_data:=in_len_of_write_data;
     ImageToCD_DispStatus_Thread:=in_ImageToCD_DispStatus_Thread;

     {$IFDEF DEBUG}
     debugf:=TFileStream.Create('W_OUTPUT.BIN', fmCreate);
     {$ENDIF}

     Inherited Create(False);  { Call the original create method. }
End;

procedure TImageToCDWriteThread.Execute;
var
   i : Integer;
   j : LongWord;
   Total_n_sectors_to_write : LongWord;
   Remaining_n_sect_to_write_of_avail : LongWord;
   Actual_n_sectors_to_write : LongWord;
   N_bytes_to_write : LongWord;
   P_write_buffer_offset : Pointer;
   P_write_buffer_offset_i : Pointer;
   P_write_param_buffer_offset : T_P_write_param;
   P_write_param_buffer_offset_i : T_P_write_param;
   write_param_buffer_offset_to_check : LongWord;
   Sector_block_size : Word;
   SubCh_block_size : Byte;
   Data_1block_size : Word;
   wait_for_ready_to_write : Boolean;
   stop_looping : Boolean;
   retry_write : Boolean;
begin
     stop_looping:=False;
     While((Terminated=False) And (stop_looping=False)) Do
     Begin
          If (Form1.Form5.ImageToCDProgressForm.ImageToCDReadThread.total_n_sectors_remaining=0) Or
             (Form1.Form5.ImageToCDProgressForm.ImageToCDReadThread.wait_for_ready_to_read=True) Then
          Begin
               stop_looping:=True;
          End;
     End;

     If Terminated=False Then
     Begin
          stop_looping:=False;

          { ** Set the writing speed ** }
          Form1.SCSI.MMC1_any_link.Do_set_CD_speed_CDB12(MMC_SET_CD_SPEED_MAX, write_speed);
          Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
          If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               s:=IntToStr(Out_CD_cap_mech_st.CurrWriteSpeed)+
                  ' ('+
                  IntToStr(Trunc(SimpleRoundTo(Out_CD_cap_mech_st.CurrWriteSpeed / C_1X_KBYTES_CDSPEED, 0)))+
                  'X)';

               If Not Terminated Then
               Begin
                    Form1.Form5.ImageToCDProgressForm.LBL_curr_write_speed.Caption:=s;
               End;
          End;

          {Form1.SCSI.MMC1_any_link.Do_reserve_track_CDB10(len_of_write_data);
          If Not Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               s:='Error while processing reserve track command.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               stop_looping:=True;
          End;}

          Sector_block_size:=2352;  //Temporary fixed size for now. Will change later to reflect selected MMC writing mode.
          SubCh_block_size:=96;  //Temporary fixed size for now. Will change later to reflect selected MMC writing mode.
          Data_1block_size:=SubCh_block_size+Sector_block_size;

          //No. of sectors that we need to write.
          j:=len_of_write_data;
          //A loop to write all data to media, unless terminated.
          Repeat
                //Determine and calculate the total number of sectors we want to write in 1 go (synchronous).
                //The variable Total_n_sectors_to_write will have this total value.
                If (Form1.CommonCDSettings.current_start_loc_of_writing+
                    Form1.CommonCDSettings.max_no_of_blocks_to_write_synch)<
                    Form1.CommonCDSettings.sector_buffer_block_len Then
                Begin
                     //The current position (offset) into cyclic buffer allows for writing at max number of sectors.
                     Total_n_sectors_to_write:=Form1.CommonCDSettings.max_no_of_blocks_to_write_synch;

                     //If remaining sectors left to write is smaller, then this will be the number of sectors to write instead.
                     If j<Total_n_sectors_to_write Then Total_n_sectors_to_write:=j;
                End
                Else
                Begin
                     //The current position (offset) into cyclic buffer is near the end so is not enough for maximum,
                     //we can only write maximum of the remaining portion of cyclic buffer.
                     Total_n_sectors_to_write:=Form1.CommonCDSettings.sector_buffer_block_len-
                                               Form1.CommonCDSettings.current_start_loc_of_writing;

                     //If remaining sectors left to write is smaller, then this will be the number of sectors to write instead.
                     If j<Total_n_sectors_to_write Then Total_n_sectors_to_write:=j;
                End;
                //Calculate the write parameter buffer offset we need to check for available data.
                write_param_buffer_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_writing+
                                     Total_n_sectors_to_write-1;

                //Beginning of write buffer. A secondary cyclic buffer for holding large amounts of CD sector data, for quick
                //transfer of small amounts to be transferred into the drive's hardware buffer and then written to CDR/W.
                P_write_buffer_offset:=Form1.CommonCDSettings.P_write_buffer;
                //Move to starting offset where we want to read from and write to CDR/W.
                Inc(T_P_byte(P_write_buffer_offset),
                    Form1.CommonCDSettings.current_start_loc_of_writing*Data_1block_size);

                //Beginning of write parameter buffer. A cyclic buffer which indicates the areas of the write buffer that has
                //been filled in with CD sector data, done by the reading thread. This is almost a mirror of the write buffer.
                P_write_param_buffer_offset:=Form1.CommonCDSettings.P_write_param_buffer;
                //Move to offset where we want to check for available data.
                Inc(P_write_param_buffer_offset, write_param_buffer_offset_to_check);

                //A loop to keep checking whether we have data in the write buffer to write.
                wait_for_ready_to_write:=True;
                While((Not Terminated) And wait_for_ready_to_write) Do
                Begin
                     //Form1.CommonCDSettings.cyclic_buffer_vars_sync.BeginRead;
                     If Not P_write_param_buffer_offset^.is_block_for_reading Then
                     Begin
                          wait_for_ready_to_write:=False;
                     End;
                     //Form1.CommonCDSettings.cyclic_buffer_vars_sync.EndRead;
                End;

                {$IFDEF DEBUG}
                //s:='Debug: ' + IntToStr(Total_n_sectors_to_write)+' '+IntToStr(write_param_buffer_offset_to_check);
                //Synchronize(ShowErrMsg);
                {$ENDIF}

                If Not Terminated Then
                Begin
                     //Calculate total number of bytes that we can now write (available data) from buffer.
                     N_bytes_to_write:=Data_1block_size*Total_n_sectors_to_write;

                     {$IFDEF DEBUG}
                     //debugf.WriteBuffer(P_write_buffer_offset^, N_bytes_to_write);
                     {$ENDIF}

                     //Initialise variable for indicating total sectors remaining to write of available data.
                     Remaining_n_sect_to_write_of_avail:=Total_n_sectors_to_write;

                     P_write_buffer_offset_i:=P_write_buffer_offset;
                     P_write_param_buffer_offset_i:=P_write_param_buffer_offset;
                     Dec(P_write_param_buffer_offset_i, Total_n_sectors_to_write-1);
                     //A loop to write all of available data so far.
                     Repeat
                           //Determine the total maximum sectors that can be written in 1 go,
                           //based on the remaining of available data and the SCSI interface buffer size.
                           If Remaining_n_sect_to_write_of_avail>=(Form1.SCSI.SPC_any_link.Get_data_buf_size DIV
                                                                   Data_1block_size) Then
                           Begin
                                //Can only write upto a total based on SCSI interface buffer size.
                                Actual_n_sectors_to_write:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV
                                                           Data_1block_size;
                           End
                           Else
                           Begin
                                //Total remaining of available data can be written all in 1 go.
                                Actual_n_sectors_to_write:=Remaining_n_sect_to_write_of_avail;
                           End;

                           //Obsolete code to be removed..
                           //Form1.SCSI.SPC_any_link.WNASPI32.Set_data_buf(P_write_buffer_offset_i);
                           //Form1.SCSI.SPC_any_link.WNASPI32.Set_data_buf_size(Data_1block_size*Actual_n_sectors_to_write);

                           //Copy data from secondary write buffer (containing data) to SCSI interface data buffer.
                           CopyMemory(
                                      Form1.SCSI.SPC_any_link.Get_data_buf,
                                      P_write_buffer_offset_i,
                                      Data_1block_size*Actual_n_sectors_to_write);

                           //Generate string for displaying progress (starting sector and number of sectors to be written).
                           s:=IntToStr(P_write_param_buffer_offset_i^.starting_MMCLBA_sector_to_write) + ',' +
                              IntToStr(Actual_n_sectors_to_write);
                           Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

                           //Do we need to flush drive cache? (For sector skipping only, also assumes max_no_of_blocks_to_write_synch=1).
                           If P_write_param_buffer_offset_i^.is_do_sync_cache Then
                           Begin
                                {$IFNDEF DEBUG}
                                //Flush (write the remaining data in) the drive's cache.
                                Write_remaining_cache;
                                {$ENDIF}

                                {$IFDEF DEBUG}
                                s:='Debug sync cache at: ' + IntToStr(P_write_param_buffer_offset_i^.starting_MMCLBA_sector_to_write);
                                Synchronize(ShowErrMsg);
                                {$ENDIF}

                                P_write_param_buffer_offset_i^.is_do_sync_cache:=False;
                           End;

                           {$IFDEF DEBUG}
                           //s:='Debug MMCLBA: ' + IntToStr(P_write_param_buffer_offset_i^.starting_MMCLBA_sector_to_write) + ','+IntToStr(Actual_n_sectors_to_write);
                           //Synchronize(ShowErrMsg);
                           {$ENDIF}

                           {$IFDEF DEBUG}
                           debugf.WriteBuffer(Form1.SCSI.SPC_any_link.Get_data_buf^, Actual_n_sectors_to_write*Data_1block_size);
                           //debugf.WriteBuffer(P_write_buffer_offset_i^, Actual_n_sectors_to_write*Data_1block_size);
                           {$ENDIF}

                           repeat
                                 retry_write:=false;

                                 {$IFNDEF DEBUG}
                                 //Write data from the SCSI interface data buffer to the media. The drive it self actually does the
                                 //reading of data bytes from the SCSI interface data buffer and tranfers them into the drive's hardware
                                 //buffer and then writes them to the media.
                                 Form1.SCSI.MMC1_any_link.Do_write_CDB10(0,
                                                                         P_write_param_buffer_offset_i^.starting_MMCLBA_sector_to_write,
                                                                         Actual_n_sectors_to_write,
                                                                         Data_1block_size);
                                 {$ENDIF}
                                 {$IFDEF DEBUG}
                                 //Force send of SCSI command was OK.
                                 Form1.SCSI.SPC_any_link.Set_is_sendcmd_OK(True);
                                 {$ENDIF}
                                 If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
                                 Begin
                                      //Calculate remaining sectors to write of available data.
                                      Remaining_n_sect_to_write_of_avail:=Remaining_n_sect_to_write_of_avail-
                                                                          Actual_n_sectors_to_write;

                                      //Increase buffer offsets accordingly.
                                      Inc(T_P_byte(P_write_buffer_offset_i), Data_1block_size*Actual_n_sectors_to_write);
                                      Inc(P_write_param_buffer_offset_i, Actual_n_sectors_to_write);
                                 End
                                 Else
                                 If (Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_NOT_READY)and
                                    (Form1.SCSI.SPC_any_link.Get_sense_ASC=$04) and
                                    (Form1.SCSI.SPC_any_link.Get_sense_ASCQ=$08) Then
                                 Begin
                                      //Retry for sense error LOGICAL UNIT NOT READY, LONG WRITE IN PROGRESS.

                                      //Send a test unit ready before retrying - required for some drives.
                                      Form1.SCSI.SPC_any_link.Test_unit_ready_CDB6;

                                      retry_write:=true;
                                 End
                                 Else
                                 Begin
                                      s:='Error while processing Write CD command.' + Chr(10) + Chr(13);
                                      s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
                                      Synchronize(ShowErrMsg);
                                      stop_looping:=True;
                                 End;
                           Until((retry_write=false) Or stop_looping Or Terminated);
                     Until((Remaining_n_sect_to_write_of_avail=0) Or stop_looping Or Terminated);

                     If Not(stop_looping Or Terminated) Then
                     Begin
                          P_write_param_buffer_offset_i:=P_write_param_buffer_offset;
                          Dec(P_write_param_buffer_offset_i, Total_n_sectors_to_write-1);
                          //Form1.Form5.ImageToCDProgressForm.ImageToCDReadThread.Suspend;
                          For i:=1 To Total_n_sectors_to_write Do
                          Begin
                               P_write_param_buffer_offset_i^.is_block_for_reading:=True;
                               Inc(P_write_param_buffer_offset_i, 1);
                          End;
                          //Form1.Form5.ImageToCDProgressForm.ImageToCDReadThread.Resume;

                          //Calculate the new offset of the write buffer (cyclic buffer).
                          Form1.CommonCDSettings.current_start_loc_of_writing:=
                          Form1.CommonCDSettings.current_start_loc_of_writing+Total_n_sectors_to_write;
                          //Check if we need to cycle to the start of the cyclic buffer.
                          If Form1.CommonCDSettings.current_start_loc_of_writing=
                             Form1.CommonCDSettings.sector_buffer_block_len Then
                          Begin
                               Form1.CommonCDSettings.current_start_loc_of_writing:=0;
                          End;

                          //Decrease the remaining total sectors to write.
                          j:=j-Total_n_sectors_to_write;
                     End;
                End;

                //Because we have written some data, we can now wake up the reading thread (if it's asleep) to read some more data.
                Form1.Form5.ImageToCDProgressForm.ImageToCDReadThread.Resume;
          Until ((j=0) OR stop_looping OR Terminated);

          If j=0 Then
          Begin
               Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=
               Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption+' finished.';
          End;

          //Obsolete code to be removed..
          //Form1.SCSI.SPC_any_link.WNASPI32.Set_data_buf(Form1.SCSI.SPC_any_link.WNASPI32.Original_P_Buf);
          //Form1.SCSI.SPC_any_link.WNASPI32.Set_data_buf_size(Form1.SCSI.SPC_any_link.WNASPI32.Original_BufSize);

          {$IFNDEF DEBUG}
          //Flush the drives cache to media.
          If j<len_of_write_data Then
             Write_remaining_cache;
          {$ENDIF}
     End;

     Form1.Form5.ImageToCDProgressForm.ImageToCDReadThread.Resume;

     {$IFDEF DEBUG}
     debugf.Free;
     {$ENDIF}
end;

Procedure TImageToCDWriteThread.ShowErrMsg;
Begin
     Form1.Form5.ImageToCDProgressForm.ShowErrMsg(s);
End;

Procedure TImageToCDWriteThread.ShowConfirmToMsg;
Begin
     confirm_from_msg:=Form1.Form5.ImageToCDProgressForm.ConfirmToMsg(s);
End;

procedure TImageToCDWriteThread.Write_remaining_cache;
Var s : String;
Begin
     s:='Writing remaining buffered data..';
     Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

     Form1.SCSI.MMC1_any_link.Do_synchronize_cache_CDB10(0, 0);

     If ImageToCD_DispStatus_Thread<>nil Then
     Begin
          ImageToCD_DispStatus_Thread.Terminate;
          ImageToCD_DispStatus_Thread.WaitFor;
          ImageToCD_DispStatus_Thread:=nil;
     End;

     s:='Done.';
     Form1.Form5.ImageToCDProgressForm.LBL_drive_buffer_status.Caption:=s;
End;

end.
