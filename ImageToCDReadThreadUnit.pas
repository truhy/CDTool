{ ***************************************************************************
  A thread for reading data from a CD image, transferring into a cyclic write
  buffer and preparing them for MMC writing mode, raw DA0 2448 bytes/sector
  (write type 3).
  *************************************************************************** }

unit ImageToCDReadThreadUnit;

interface

{ Uncomment below for debug mode.
  The CD sector data written to a file:
  OUTPUT.BIN. }
//{$DEFINE DEBUG}  //Debug mode.

{ Uncomment for debug mode options. These will affect the CD write thread debug
  mode file W_OUTPUT.BIN. }
//{$DEFINE DEBUG_W_OUTPUT_DEINT_SUBS}  //Deinterlace sub-channel data.
//{$DEFINE DEBUG_W_OUTPUT_UNSCRAMBLED}  //If CDROM format, leave main data unscrambled.

uses
  Windows,
  Classes,
  SysUtils,
  Math,
  MMC1Unit,
  SPC_Unit,
  CommonCDSettingsUnit,
  CRC_Unit,
  CDROMTableSectorScramblerUnit,
  TOCUnit,
  EDC_ECC_Unit,
  Tracks_Unit,
  CDROM_struct_Unit,
  ImageToCD_DispStatus_Thread_Unit,
  ImageToCDWriteThreadUnit,
  Tools_Unit;

{ My own constants for selecting lead-in data format type. }
const
     U_TRACK_MODE_AUDIO=0;
     U_TRACK_MODE_DATA=1;
     U_TRACK_MODE_USE_TOC=2;

type Ptr_text_file=^TextFile;

type T_array16_byte=array[1..16] of Byte;
     T_P_byte=^Byte;

type T_SSP_info=Record
                      SSP_start : LongInt;
                      SSP_len   : LongWord;
                End;

type T_SSP_write_info=Record
                            start : LongInt;
                            len   : LongWord;
                      End;

type
  TImageToCDReadThread = class(TThread)
  private
    { Private declarations }
    procedure ShowErrMsg;
    procedure ShowConfirmToMsg;
  public
    confirm_from_msg : Boolean;
    start_MMCLBA : LongInt;
    write_speed : Word;
    lead_in_file_type : Byte;
    f_leadin_TOC : TFileStream;
    f_data_area : TFileStream;
    f_data_area_subch : TFileStream;
    f_pregap1 : TFileStream;
    f_LO : TFileStream;
    SSP_file : Ptr_text_file;
    SSP_method : Byte;
    SSP_info : T_SSP_info;
    SSP_list : TList;
    SSP_write_list : TList;
    total_n_sectors : Cardinal;
    total_n_sectors_remaining : Cardinal;
    {$IFDEF DEBUG}
    debugf : TFileStream;
    {$ENDIF}
    MMCLBA_for_writing : LongInt;
    LBA_for_writing : LongInt;
    correct_LBA_for_writing : LongInt;
    skipped_LBA_for_writing : LongInt;
    subch_deint_PW_96 : T_subch_deint_PW_96;
    s : String;
    Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc;
    TOC : T_TOC;
    tracks : T_tracks;
    lead_in_TOC_len : LongInt;
    pre_gap_len : LongInt;
    post_gap_len : LongInt;
    lead_out_len : LongInt;
    lead_in_type : Byte;
    Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;
    wait_for_ready_to_read : Boolean;
    ImageToCD_DispStatus_Thread : T_ImageToCD_DispStatus_Thread;
    ImageToCDWriteThread : TImageToCDWriteThread;

    Constructor Create(Thread_done_proc     : TNotifyEvent;
                       in_start_MMCLBA      : Integer;
                       in_write_speed       : Word;
                       in_lead_in_file_type : Byte;
                       in_lead_in_type      : Byte;
                       in_f_leadin_TOC      : TFileStream;
                       in_f_data_area       : TFileStream;
                       in_f_data_area_subch : TFileStream;
                       in_f_pregap1         : TFileStream;
                       in_f_LO              : TFileStream;
                       in_SSP_file          : Pointer;
                       in_SSP_method        : Byte);
    procedure Free;
    procedure Execute; override;
    procedure GetNextSkipSect;
    procedure DoWeSkipSect(write_param : T_P_write_param; in_curr_abs_LBA_to_write : Longint);
    procedure Read_SSP_write_into_list;
    function GetStartMMCLBAFromATIP : LongInt;
    procedure Read_file_2448(file_to_read : TFileStream;
                             no_of_sectors_to_read : LongWord;
                             data_track_mode : Byte;
                             track_no : Byte;
                             in_s : String);
    procedure Make_lead_in_from_TOC_file;
    procedure Make_pre_gap;
    procedure Read_file_2352_96;
    procedure Make_post_gap;
    procedure Make_lead_out;
  end;

function Compare_SSP_list(Item1, Item2: Pointer): Integer;

implementation

Uses
    MainFormUnit;

Constructor TImageToCDReadThread.Create(Thread_done_proc     : TNotifyEvent;
                                        in_start_MMCLBA      : Integer;
                                        in_write_speed       : Word;
                                        in_lead_in_file_type : Byte;
                                        in_lead_in_type      : Byte;
                                        in_f_leadin_TOC      : TFileStream;
                                        in_f_data_area       : TFileStream;
                                        in_f_data_area_subch : TFileStream;
                                        in_f_pregap1         : TFileStream;
                                        in_f_LO              : TFileStream;
                                        in_SSP_file          : Pointer;
                                        in_SSP_method        : Byte);
Var i : LongWord;
    buf_offset : T_P_write_param;
Begin
     start_MMCLBA:=in_start_MMCLBA;
     write_speed:=in_write_speed;
     lead_in_file_type:=in_lead_in_file_type;
     lead_in_type:=in_lead_in_type;
     f_leadin_TOC :=in_f_leadin_TOC;
     f_data_area:=in_f_data_area;
     f_data_area_subch:=in_f_data_area_subch;
     f_pregap1:=in_f_pregap1;
     f_LO:=in_f_LO;
     SSP_method:=in_SSP_method;

     wait_for_ready_to_read:=False;

     {$IFDEF DEBUG}
     debugf:=TFileStream.Create('OUTPUT.BIN', fmCreate);
     {$ENDIF}

     { 1/7th of system RAM: 7*2448=17136. Maximum no. of sectors that is allowed to be buffered.
       This is the amount of dynamic memory to use for the secondary cyclic buffer. }
     Form1.CommonCDSettings.sector_buffer_block_len:=GetTotalPhysMemory Div 17136;

     Form1.CommonCDSettings.current_start_loc_of_reading:=0; //Location of buffer for reading (sector unit).
     Form1.CommonCDSettings.current_start_loc_of_writing:=0; //Location of buffer for writing (sector unit).
     Form1.CommonCDSettings.max_no_of_blocks_to_read_synch:=428; //Must be >= 1 and should be <= no_of_blocks_to_wait_b4_reading.
     If (in_SSP_method=1) Or
        (in_SSP_method=2) Or
        (in_SSP_method=3) Then
     Begin
          SSP_file:=in_SSP_file;
          Form1.CommonCDSettings.max_no_of_blocks_to_write_synch:=1; //Must be 1 for skipping sectors.
     End
     Else
     Begin
          Form1.CommonCDSettings.max_no_of_blocks_to_write_synch:=
          Form1.SCSI.SPC_any_link.Get_data_buf_size DIV 2448; //Must be >= 1.
     End;
     Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading:=428; //Must be >= 1 and should be < sector_buffer_block_len.
     Form1.CommonCDSettings.reading_done:=False;

     //Allocate memory for the secondary cyclic buffers.
     GetMem(Form1.CommonCDSettings.P_write_buffer, Form1.CommonCDSettings.sector_buffer_block_len*2448);
     GetMem(Form1.CommonCDSettings.P_write_param_buffer, Form1.CommonCDSettings.sector_buffer_block_len*SizeOf(T_write_param));

     //Initialise to indicate that write buffer does not contain any writable data.
     buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
     For i:=1 To Form1.CommonCDSettings.sector_buffer_block_len Do
     Begin
          buf_offset^.is_do_sync_cache:=False;
          buf_offset^.is_block_for_reading:=True;
          Inc(buf_offset);
     End;

     OnTerminate:=Thread_done_proc;
     //FreeOnTerminate := True;  // Automatically destroy itself.
     Inherited Create(False);  //Call the original create method.
End;

procedure TImageToCDReadThread.Free;
begin
     If ImageToCDWriteThread<>nil Then
     Begin
          ImageToCDWriteThread.Free;
     End;

     Inherited Free;
end;

procedure TImageToCDReadThread.Execute;
var
   i : Integer;
   t : T_track_entry;
begin
     { ** Sets the writing speed ** }
     {Form1.SCSI.MMC1.Do_set_CD_speed(MMC_SET_CD_SPEED_MAX, write_speed);
     Form1.SCSI.MMC1.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
     If Form1.SCSI.SCSI_interface.FA_SRB_status_OK Then
     Begin
          s:=IntToStr(Out_CD_cap_mech_st.CurrWriteSpeed)+
             ' ('+
             IntToStr(Trunc(SimpleRoundTo(Out_CD_cap_mech_st.CurrWriteSpeed / C_1X_KBYTES_CDSPEED, 0)))+
             'X)';

          If Not Terminated Then
          Begin
               Form1.Form5.ImageToCDProgressForm.LBL_curr_write_speed.Caption:=s;
          End;
     End;}

     SSP_write_list:=TList.Create;

     If (SSP_method=1) Or
        (SSP_method=2) Then
     Begin
          GetNextSkipSect
     End;

     If (SSP_method=3) then
     Begin
          Read_SSP_write_into_list;
     End;

     { Reads the TOC in a lead-in file and represent it as a unique TOC
       that is stored in a TOC object. }
     TOC:=T_TOC.Create(f_leadin_TOC, 96, lead_in_file_type);
     //The TOC object read the file, so we should seek back to 0.
     f_leadin_TOC.Seek(0, soFromBeginning);

     { Uses the TOC object and main sector image file to determine the
       individual track types and times. }
     If lead_in_file_type=TOC_FILE_TYPE_2448_FULL_DAO_FILE Then
     Begin
          //Find the lead-in TOC length in the file.
          lead_in_TOC_len:=Find_track_no_from_2448_file(f_leadin_TOC, 1);
          //Find the 1st pre-gap length in the file.
          pre_gap_len:=Find_p_value_from_2448_file(f_leadin_TOC, 0);
          //The Find functions above seeked the file, so we should seek back to 0.
          f_leadin_TOC.Seek(0, soFromBeginning);

          //s:='Debug: CD image bin:' + IntToStr(leadin_TOC_len) + ' ' + IntToStr(pregap1_len);
          //Synchronize(ShowErrMsg);

          { Use the TOC object & CD image file to find track information for all
            tracks listed in the TOC object. }
          tracks:=T_tracks.Create(f_leadin_TOC, 2448, lead_in_TOC_len, TOC);

          //t:=tracks.Get_entry(0);
          //s:='Debug: CDROM struct unit:' + IntToStr(t.First_MSF.M) + ' ' +
          //                                 IntToStr(t.First_MSF.S) + ' ' +
          //                                 IntToStr(t.First_MSF.F);
          //s:='CDROM struct: '+IntToStr(tracks.Tracks.Count);
          ///Synchronize(ShowErrMsg);
     End
     Else
     Begin
          { Use the TOC object & CD image file to find track information for all
            tracks listed in the TOC object. }
          tracks:=T_tracks.Create(f_data_area, 2352, 0, TOC);

          //s:='CDROM struct: '+IntToStr(TOC.TOC_data.Count);
          //Synchronize(ShowErrMsg);
     End;

     If (SSP_method=3) And (SSP_write_list.Count>0) Then
     Begin
          { This section of if choice is to write in sections from outer sections
            to inner sections of CD (i.e. write sections backwards).  This will
            allow sector positions to be skipped.  This is asynchronous
            writing. }

          For i:=SSP_write_list.Count-1 To 0 Do
          Begin

          End;

          { Destroys all dynamic memory used by pointers in the list. }
          For i:=0 To SSP_write_list.Count-1 Do
          Begin
               Dispose(SSP_write_list.Items[i]);
          End;
     End
     Else
     Begin
          { This section of if choice is to write normally.  This is
            synchronous writing. }

          { Running sector count that is used as the parameter for the MMC write
            command. Takes into account of user specified starting sector and
            sector skips (if any). This is in MMCLBA sector style, so it has a
            negative range after 89:59:74. }
          MMCLBA_for_writing:=start_MMCLBA;

          { Running sector count that is the same as MMCLBA, but does not
            include the sector skips (if any). This is in normal LBA sector
            style, so it has a positive range after 89:59:74. This is needed to
            give correct addresses to be used for generation of sector data. }
          LBA_for_writing:=start_MMCLBA;

          { Running sector count that is like a mirror of MMCLBA, except it is
            in normal LBA sector style, so it has a positive range after
            89:59:74. This is needed so that sectors can be easily counted, and
            is intended for conversion to the MMCLBA_sector variable, which has
            a negative range after 89:59:74. }
          skipped_LBA_for_writing:=start_MMCLBA;

          { There are different lead-in or TOC file formats. }
          Case lead_in_file_type Of
          TOC_FILE_TYPE_2448_FULL_DAO_FILE:
          Begin
               //s:='Debug: CD image bin';
               //Synchronize(ShowErrMsg);

               //leadin_TOC_len:=Find_track_no_from_2448_file(f_leadin_TOC, 1);
               //f_leadin_TOC.Seek(0, soFromBeginning);

               confirm_from_msg:=true;
               { Check whether the user had specified a starting sector which gives
                 a lead-in length that is different from the length in the file.. }
               If MMCLBA_for_writing>=-pre_gap_len Then
               Begin
                    s:='Warning: starting sector will create a 0 lead-in TOC length,' + Chr(13) +
                       'so the lead-in TOC area on the disc will be unwritten (blank).' + Chr(13) +
                       'All sectors will be written shifted forward.' + Chr(13) +
                       'The lead-in TOC length in the file is: ' +
                       IntToStr(lead_in_TOC_len) + ' sectors.' + Chr(13) +
                       'Do you still want to continue writing?';
                    Synchronize(ShowConfirmToMsg);
               End;

               If confirm_from_msg Then
               Begin
                    { Running sector count that is the same as MMCLBA, but does
                      not include the user specified starting sector and the
                      sector skips (if any). This is in normal LBA sector style,
                      so it has a positive range after 89:59:74. It is needed to
                      give correct addresses to be used for generation of sector
                      data. }
                    correct_LBA_for_writing:=-(lead_in_TOC_len+150);

                    //Total no. of sectors to write to disc.
                    total_n_sectors:=f_leadin_TOC.Size DIV 2448;
                    //A decreasing counter for total no. of sectors remaining.
                    total_n_sectors_remaining:=total_n_sectors;

                    { Create a thread to show the reading/writing progress. }
                    ImageToCD_DispStatus_Thread:=T_ImageToCD_DispStatus_Thread.Create;

                    { Create a thread to do the writing to disc. Note that it
                      only uses the secondary cyclic buffer for reading, no CD
                      image files are being read here. }
                    ImageToCDWriteThread:=TImageToCDWriteThread.Create(write_speed,
                                                                       total_n_sectors,
                                                                       ImageToCD_DispStatus_Thread);

                    { Read the CD image file and processes it into the secondary
                      cyclic buffer, for the thread above. }
                    If Not Terminated Then
                       Read_file_2448(f_leadin_TOC, total_n_sectors, lead_in_type, 0, '');
               End;
          End;
          TOC_FILE_TYPE_2448_FULL_LI_FILE:
          Begin
               confirm_from_msg:=true;
               { Check whether the user had specified a starting sector which gives
                 a lead-in length that is different from the length in the file.. }
               If MMCLBA_for_writing>=-pre_gap_len Then
               Begin
                    s:='Warning: starting sector will create a 0 lead-in TOC length,' + Chr(13) +
                       'so the lead-in TOC area on the disc will be unwritten (blank).' + Chr(13) +
                       'All sectors will be written shifted forward.' + Chr(13) +
                       'The lead-in TOC length in the file is: ' +
                       IntToStr(f_leadin_TOC.Size DIV 2448) + ' sectors.' + Chr(13) +
                       'Do you still want to continue writing?';
                    Synchronize(ShowConfirmToMsg);
               End;

               If confirm_from_msg Then
               Begin
                    //MMCLBA_sector:=-((f.Size+f_pregap1.Size) DIV 2448);
                    //curr_abs_LBA_to_write:=MMCLBA_sector;

                    pre_gap_len:=f_pregap1.Size DIV 2448;
                    post_gap_len:=150;
                    lead_in_TOC_len:=f_leadin_TOC.Size DIV 2448;
                    lead_out_len:=f_LO.Size DIV 2448;
                    //Total no. of sectors to write to disc.
                    total_n_sectors:=lead_in_TOC_len+
                                     pre_gap_len+
                                     (f_data_area.Size DIV 2352)+
                                     lead_out_len;
                    //A decreasing counter for total no. of sectors remaining.
                    total_n_sectors_remaining:=total_n_sectors;

                    { Running sector count that is the same as MMCLBA, but does
                      not include the user specified starting sector and the
                      sector skips (if any). This is in normal LBA sector style,
                      so it has a positive range after 89:59:74. It is needed to
                      give correct addresses to be used for generation of sector
                      data. }
                    correct_LBA_for_writing:=-(lead_in_TOC_len+150);

                    { Create a thread to show the reading/writing progress. }
                    ImageToCD_DispStatus_Thread:=T_ImageToCD_DispStatus_Thread.Create;

                    { Create a thread to do the writing to disc. Note that it
                      only uses the secondary cyclic buffer for reading, no CD
                      image files are being read here. }
                    ImageToCDWriteThread:=TImageToCDWriteThread.Create(write_speed,
                                                                       total_n_sectors,
                                                                       ImageToCD_DispStatus_Thread);

                    { Read the CD image files and processes it into the secondary
                      cyclic buffer, for the thread above.. }
                    If Not Terminated Then
                       Read_file_2448(f_leadin_TOC, lead_in_TOC_len, lead_in_type, 0, 'Lead-in TOC: ');
                    If Not Terminated Then
                       Read_file_2448(f_pregap1, pre_gap_len, U_TRACK_MODE_USE_TOC, 1, 'Pre-gap: ');
                    If Not Terminated Then
                       Read_file_2352_96;
                    If Not Terminated Then
                       Read_file_2448(f_LO, lead_out_len, U_TRACK_MODE_USE_TOC, $A2, 'Lead-out: ');
               End;
          End;
          TOC_FILE_TYPE_96_REPEATED_DEINT,
          TOC_FILE_TYPE_96_UNIQUE_DEINT,
          TOC_FILE_TYPE_96_UNIQUE_INT:
          Begin
               { Running sector count that is the same as MMCLBA, but does
                 not include the user specified starting sector and the
                 sector skips (if any). This is in normal LBA sector style,
                 so it has a positive range after 89:59:74. It is needed to
                 give correct addresses to be used for generation of sector
                 data. }
               correct_LBA_for_writing:=GetStartMMCLBAFromATIP;

               pre_gap_len:=150;
               post_gap_len:=150;

               If lead_in_file_type=TOC_FILE_TYPE_96_REPEATED_DEINT Then
                   lead_in_TOC_len:=(f_leadin_TOC.Size DIV 96)
               Else
                   lead_in_TOC_len:=Abs(correct_LBA_for_writing+150);

               confirm_from_msg:=true;
               { Check whether the user has specified a starting sector which gives
                 a lead-in length that is different from the length in the file.. }
               If MMCLBA_for_writing>=-pre_gap_len Then
               Begin
                    s:='Warning: starting sector will create a 0 lead-in TOC length,' + Chr(13) +
                       'so the lead-in TOC area on the disc will be unwritten (blank).' + Chr(13) +
                       'All sectors will be written shifted forward.' + Chr(13) +
                       'The lead-in TOC length in the file is: ' +
                       IntToStr(lead_in_TOC_len) + ' sectors.' + Chr(13) +
                       'Do you still want to continue writing?';
                    Synchronize(ShowConfirmToMsg);
               End;

               If confirm_from_msg Then
               Begin
                    lead_out_len:=6750;

                    //Total no. of sectors to write to disc.
                    total_n_sectors:=lead_in_TOC_len+
                                     pre_gap_len+
                                     (f_data_area.Size DIV 2352)+
                                     lead_out_len;
                    //A decreasing counter for total no. of sectors remaining.
                    total_n_sectors_remaining:=total_n_sectors;

                    { Create a thread to show the reading/writing progress. }
                    ImageToCD_DispStatus_Thread:=T_ImageToCD_DispStatus_Thread.Create;

                    { Create a thread to do the writing to disc. Note that it
                      only uses the secondary cyclic buffer for reading, no CD
                      image files are being read here. }
                    ImageToCDWriteThread:=TImageToCDWriteThread.Create(write_speed,
                                                                       total_n_sectors,
                                                                       ImageToCD_DispStatus_Thread);

                    { Read the CD image files and processes it into the secondary
                      cyclic buffer, for the thread above.. }
                    If Not Terminated Then
                       Make_lead_in_from_TOC_file;
                    //If Not Terminated Then
                    //Make_post_gap;  //ECMA-130 describes a post gap - but this creates an unreadable CD.
                    If Not Terminated Then
                       Make_pre_gap;  //Instead it must be a pre-gap after the TOC in lead-in.
                    If Not Terminated Then
                       Read_file_2352_96;
                    //If Not Terminated Then
                    //Make_post_gap;  //Post gap is assumed to be included in main file.
                    If Not Terminated Then
                       Make_lead_out;
               End;
          End;
          End;
     End;

     //The thread may not have been created, so check first.
     If ImageToCDWriteThread<>nil Then
     Begin
          //Form already terminated the write thread.
          //If Terminated Then
          //   ImageToCDWriteThread.Terminate;

          ImageToCDWriteThread.WaitFor;  //Wait for the write thread to finish.
     End;

     //The Write_remaining_cache function in the write thread already terminated
     //this thread.
     {If ImageToCD_DispStatus_Thread<>nil Then
     Begin
          ImageToCD_DispStatus_Thread.Terminate;
          ImageToCD_DispStatus_Thread.WaitFor;
     End;}
     

     TOC.Free;
     tracks.Free;
     {$IFDEF DEBUG}
     debugf.Free;
     {$ENDIF}

     //Frees the memory for the secondary cyclic buffer.
     FreeMem(Form1.CommonCDSettings.P_write_buffer);
     FreeMem(Form1.CommonCDSettings.P_write_param_buffer);

     // Free the list object.
     SSP_write_list.Free;
end;

Procedure TImageToCDReadThread.GetNextSkipSect;
{ Reads a line from .SSP file and saves the start and len in variables:
  SSP_start and SSP_len. }
Var s : String;
    i : Integer;
Begin
     If EOF(SSP_file^) Then
     Begin
          SSP_info.SSP_len:=0;
     End
     Else
     Begin
          ReadLn(SSP_file^, s);
          i:=Pos(',', s);
          If i<>0 Then
          Begin
               SSP_info.SSP_start:=StrToInt(Copy(s, 1, i-1));
               SSP_info.SSP_len:=StrToInt(Copy(s, i+1, Length(s)-i));
          End
          Else
          Begin
               SSP_info.SSP_len:=0;
          End;
     End;
End;

Procedure TImageToCDReadThread.DoWeSkipSect(write_param : T_P_write_param; in_curr_abs_LBA_to_write : Longint);
Begin
     If (SSP_method=1) Or (SSP_method=2) Then
     Begin
     If SSP_info.SSP_len>0 Then
     Begin
          If in_curr_abs_LBA_to_write=SSP_info.SSP_start Then
          Begin
               skipped_LBA_for_writing:=skipped_LBA_for_writing+SSP_info.SSP_len;
               write_param^.starting_MMCLBA_sector_to_write:=Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MMCLBA(skipped_LBA_for_writing);

               GetNextSkipSect;

               If SSP_method=2 Then
                  write_param^.is_do_sync_cache:=True;
          End;
     End;
     End;
End;

procedure TImageToCDReadThread.Read_SSP_write_into_list;
var
   Ptr_SSP_write_info : ^T_SSP_write_info;
   s : String;
   i : Integer;
   Next_start : LongInt;
   total_SSP_sector_writes : LongInt;
begin
     Next_start:=-lead_in_TOC_len;

     total_SSP_sector_writes:=0;
     while (not EOF(SSP_file^)) do
     begin
          ReadLn(SSP_file^, s);
          i:=Pos(',', s);
          if i<>0 then
          begin
               New(Ptr_SSP_write_info);

               SSP_info.SSP_start:=StrToInt(Copy(s, 1, i-1));
               SSP_info.SSP_len:=StrToInt(Copy(s, i+1, Length(s)-i));

               T_SSP_write_info(Ptr_SSP_write_info^).start:=Next_start;
               T_SSP_write_info(Ptr_SSP_write_info^).len:=SSP_info.SSP_start-
                                                          Next_start;
               Next_start:=SSP_info.SSP_start+SSP_info.SSP_len;

               SSP_list.Add(Ptr_SSP_write_info);

               total_SSP_sector_writes:=total_SSP_sector_writes+
                                T_SSP_write_info(Ptr_SSP_write_info^).len;
          end
     end;

     if SSP_list.Count>0 then
     begin
          New(Ptr_SSP_write_info);

          T_SSP_write_info(Ptr_SSP_write_info^).start:=Next_start;
          T_SSP_write_info(Ptr_SSP_write_info^).len:=total_n_sectors-
                                                     total_SSP_sector_writes;

          SSP_list.Add(Ptr_SSP_write_info);
     end;
end;

Procedure TImageToCDReadThread.ShowErrMsg;
Begin
     Form1.Form5.ImageToCDProgressForm.ShowErrMsg(s);
End;

Procedure TImageToCDReadThread.ShowConfirmToMsg;
Begin
     confirm_from_msg:=Form1.Form5.ImageToCDProgressForm.ConfirmToMsg(s);
End;

Function TImageToCDReadThread.GetStartMMCLBAFromATIP : LongInt;
Begin
     Form1.SCSI.MMC1_any_link.Do_read_T_P_A_ATIP_out_CDB10(Out_ATIP_desc);
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          GetStartMMCLBAFromATIP:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_MMCLBA(Out_ATIP_desc.StartMin,
                                                                                  Out_ATIP_desc.StartSec,
                                                                                  Out_ATIP_desc.StartFrame);
     End
     Else
         GetStartMMCLBAFromATIP:=0;
End;

procedure TImageToCDReadThread.Read_file_2448(file_to_read : TFileStream;
                                              no_of_sectors_to_read : LongWord;
                                              data_track_mode : Byte;
                                              track_no : Byte;
                                              in_s : String);
{ Reads a raw 2448 bytes/sector file and transfers it into the cyclic secondary
  write buffer. Sub-channel data are interlaced and main data is scrambled if
  necessary. }
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   Max_N_Sect_block_size : LongWord;
   Data_1block_size      : Word;
   N_Sect_blocks_to_write : LongWord;
   i, j                  : LongWord;
   stop_looping          : Boolean;
   P_write_buffer_offset : Pointer;
   buf_offset            : T_P_write_param;
   buf_offset2           : Pointer;
   buf_offset_to_check   : LongWord;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   next_track_LBA        : LongInt;
   i_track_no            : Byte;
   is_next_track_lead_out : Boolean;
   P_write_param : ^T_write_param;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     j:=no_of_sectors_to_read;

     i_track_no:=track_no;

     is_next_track_lead_out:=False;

     //Check if starting track no. is lead-in track (i.e. track no. 0)
     If i_track_no=0 Then
     Begin
          Case data_track_mode Of
          U_TRACK_MODE_AUDIO:
          Begin
               is_data_track:=False;
          End;
          U_TRACK_MODE_DATA:
          Begin
               is_data_track:=True;
          End;
          U_TRACK_MODE_USE_TOC:
          Begin
               //Use track 1 TOC entry to decide the lead-in track format, which then
               //determines the use of scrambling sectors.
               If TOC.Find_mode_point_entry(out_TOC_entry, 1, 1) Then
               Begin
                    //Determine the CD data mode from the track TOC entry.
                    Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                         $00,$10,$80,$90: is_data_track:=False;
                         $40,$50: is_data_track:=True;
                    Else
                        //Unknown CD data mode, so assume it's a CDDA track.
                        is_data_track:=False;
                    End;
               End
               Else
               Begin
                    //Seems we have an invalid TOC, so we'll assume it's
                    //a CDDA track mode lead-in.
                    is_data_track:=False;
               End;
          End;
          End;
     End
     Else
     Begin
          //Use the current track no. TOC entry to find out the track format.
          If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no) Then
          Begin
               //Determine the CD data mode from the track TOC entry.
               Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                    $00,$10,$80,$90: is_data_track:=False;
                    $40,$50: is_data_track:=True;
               Else
                   //Unknown CD data mode, so assume it's a CDDA track.
                   is_data_track:=False;
               End;
          End
          Else
          Begin
               //Seems we have an invalid TOC, so we'll assume it's a CDDA track.
               is_data_track:=False;
          End;
     End;

     //Check if we are at lead-out
     If i_track_no=$A2 Then
     Begin
          //Use 99:59:74 + 1 -> last possible position + 1.
          next_track_LBA:=449850;
     End
     Else
     Begin
          { Use the next track n TOC entry to find out the next track start LBA. }
          If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no+1) Then
          Begin
               next_track_LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                            T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                            T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);

               //In the lead-in track, the TOC area does not include the 1st pregap.
               If i_track_no=0 Then next_track_LBA:=next_track_LBA-150;
          End
          Else
          Begin
               //No next track n TOC entry, so find the lead-out track TOC entry.
               is_next_track_lead_out:=True;
               If TOC.Find_mode_point_entry(out_TOC_entry, 1, $A2) Then
               Begin
                    next_track_LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                                 T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                                 T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
               End
               Else
               Begin
                    //No lead-out track TOC entry, so use 99:59:74 + 1 -> last possible position + 1.
                    next_track_LBA:=449850;
               End;
          End;
     End;

     stop_looping:=False;
     Repeat
           If (Form1.CommonCDSettings.current_start_loc_of_reading+
               Form1.CommonCDSettings.max_no_of_blocks_to_read_synch)<
               Form1.CommonCDSettings.sector_buffer_block_len Then
           Begin
                N_Sect_blocks_to_write:=Form1.CommonCDSettings.max_no_of_blocks_to_read_synch;

                If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

                If (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading<j) And
                   (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading>N_Sect_blocks_to_write) Then
                Begin
                     If (Form1.CommonCDSettings.current_start_loc_of_reading+
                         Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1)<
                         Form1.CommonCDSettings.sector_buffer_block_len Then
                     Begin
                          buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                               Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1;
                     End
                     Else
                     Begin
                          buf_offset_to_check:=Form1.CommonCDSettings.sector_buffer_block_len-1;
                     End;
                End
                Else
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                          N_Sect_blocks_to_write-1;
                End;
           End
           Else
           Begin
                N_Sect_blocks_to_write:=Form1.CommonCDSettings.sector_buffer_block_len-
                                        Form1.CommonCDSettings.current_start_loc_of_reading;
                If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;
                buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                     N_Sect_blocks_to_write-1;
           End;

           P_write_buffer_offset:=Form1.CommonCDSettings.P_write_buffer;
           Inc(T_P_byte(P_write_buffer_offset),
               Form1.CommonCDSettings.current_start_loc_of_reading*Data_1block_size);

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, buf_offset_to_check);

           wait_for_ready_to_read:=True;
           While((Not Terminated) And wait_for_ready_to_read) Do
           Begin
                If buf_offset^.is_block_for_reading Then
                Begin
                     wait_for_ready_to_read:=False;
                End
                Else
                Begin
                     Suspend;
                End;
           End;

           If Not Terminated Then
           Begin
                s:=in_s+
                   IntToStr(LBA_for_writing) + '..' +
                   IntToStr(LBA_for_writing+N_Sect_blocks_to_write-1) + ',' +
                   IntToStr(N_Sect_blocks_to_write);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption:=s;

                //Display_status;

                //s:='Debug: ' + IntToStr(buf_offset_to_check);
                //Synchronize(ShowErrMsg);

                file_to_read.ReadBuffer(P_write_buffer_offset^, Data_1block_size*N_Sect_blocks_to_write);
                {$IFDEF DEBUG}
                debugf.WriteBuffer(P_write_buffer_offset^, Data_1block_size*N_Sect_blocks_to_write);
                {$ENDIF}

                i:=0;
                buf_offset2:=P_write_buffer_offset;
                Repeat
                      //<><><> Start of scrambling CDROM data sectors <><><>
                      If is_data_track Then
                      Begin
                           {$IFNDEF DEBUG_W_OUTPUT_UNSCRAMBLED}
                           Scramble(buf_offset2);
                           {$ENDIF}
                      End;
                      //Go to the start of de-interlaced subch data
                      Inc(T_P_byte(buf_offset2), Sector_block_size);
                      //<><><> End of scrambling CDROM data sectors <><><>

                      //<><><> Start of interlacing subchannel data <><><>
                      //Copy de-interlaced subch data from dynamic buffer to a variable
                      subch_deint_PW_96:=T_subch_deint_PW_96(buf_offset2^);
                      {$IFNDEF DEBUG_W_OUTPUT_DEINT_SUBS}
                      //Interlace the subch data from variable, leaving result in dynamic buffer
                      Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, buf_offset2);
                      {$ELSE}
                      //For debugging only, MMC write type 3 does not need this. Don't interlace sub-channel data.
                      T_subch_deint_PW_96(buf_offset2^):=subch_deint_PW_96;
                      {$ENDIF}
                      //Go to the start of next sector
                      Inc(T_P_byte(buf_offset2), SubCh_block_size);
                      //<><><> End of interlacing subchannel data <><><>

                      i:=i+1;
                Until(i=N_Sect_blocks_to_write);

                buf_offset2:=Form1.CommonCDSettings.P_write_param_buffer;
                Inc(T_P_write_param(buf_offset2), Form1.CommonCDSettings.current_start_loc_of_reading);
                //ImageToCDWriteThread.Suspend;
                For i:=0 To N_Sect_blocks_to_write-1 Do
                Begin
                     T_write_param(buf_offset2^).starting_MMCLBA_sector_to_write:=
                     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MMCLBA(skipped_LBA_for_writing);

                     T_write_param(buf_offset2^).no_of_sectors_to_write:=
                     N_Sect_blocks_to_write-i;

                     //Do we skip sector position area?
                     DoWeSkipSect(buf_offset2, LBA_for_writing);

                     LBA_for_writing:=LBA_for_writing+1;
                     correct_LBA_for_writing:=correct_LBA_for_writing+1;
                     skipped_LBA_for_writing:=skipped_LBA_for_writing+1;

                     T_write_param(buf_offset2^).is_block_for_reading:=False;
                     Inc(T_P_write_param(buf_offset2), 1);

                     If LBA_for_writing=next_track_LBA Then
                     Begin
                          //Next track no.
                          If is_next_track_lead_out=True Then
                          Begin
                               i_track_no:=$A2;
                          End
                          Else
                          Begin
                               i_track_no:=i_track_no+1;
                          End;
                          //Use the current track no. TOC entry to find out the track format. 
                          If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no) Then
                          Begin
                               //Determine the CD data mode from the track TOC entry.
                               Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                                    $00,$10,$80,$90: is_data_track:=False;
                                    $40,$50: is_data_track:=True;
                               Else
                                   //Unknown CD data mode, so assume it's a CDDA track.
                                   is_data_track:=False;
                               End;
                          End
                          Else
                          Begin
                               //Seems we have an invalid TOC, so we'll assume it's a CDDA track.
                               is_data_track:=False;
                          End;

                          //Check if we are at the lead-out track.
                          If i_track_no=$A2 Then
                          Begin
                               //Use 99:59:74 + 1 -> last possible position + 1.
                               next_track_LBA:=449850;
                          End
                          Else
                          Begin
                               { Use the next track n TOC entry to find out the next track start LBA. }
                               If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no+1) Then
                               Begin
                                    next_track_LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                                                 T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                                                 T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
                               End
                               Else
                               Begin
                                    //No next track n TOC entry, so find the lead-out track TOC entry.
                                    is_next_track_lead_out:=True;
                                    If TOC.Find_mode_point_entry(out_TOC_entry, 1, $A2) Then
                                    Begin
                                         next_track_LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                                                      T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                                                      T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
                                    End
                                    Else
                                    Begin
                                         //No lead-out track TOC entry, so use 99:59:74 + 1 -> last possible position + 1.
                                         next_track_LBA:=449850;
                                    End;
                               End;
                          End;
                     End;
                End;
                //ImageToCDWriteThread.Resume;

                Form1.CommonCDSettings.current_start_loc_of_reading:=
                Form1.CommonCDSettings.current_start_loc_of_reading+N_Sect_blocks_to_write;
                If Form1.CommonCDSettings.current_start_loc_of_reading=
                   Form1.CommonCDSettings.sector_buffer_block_len Then
                Begin
                     Form1.CommonCDSettings.current_start_loc_of_reading:=0;
                End;

                //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
                If skipped_LBA_for_writing>=404850 Then
                    MMCLBA_for_writing:=skipped_LBA_for_writing-450000
                Else
                    MMCLBA_for_writing:=skipped_LBA_for_writing;
                j:=j-N_Sect_blocks_to_write;
                total_n_sectors_remaining:=total_n_sectors_remaining-N_Sect_blocks_to_write;
           End;
     Until (j=0) OR (stop_looping) OR Terminated;
End;

Procedure TImageToCDReadThread.Make_lead_in_from_TOC_file;
{ Generates a complete raw 2448 bytes/sector lead-in in the cyclic write buffer.
  Note: Lead-in is completely generated from only Q subchannel TOC info
  from file.

  If CDROM format, the sectors contains zeroes but formatted appropriately, i.e.
  complete with CDROM header, EDC/ECC values, P/Q values, & scrambled.  CDROM
  header address contain prefix Ah for the minute, i.e.: A0:00:00 (MSF).

  For CDDA, the sectors contains all zeroes and not scrambled (shouldn't be
  anyway).

  For the sub-channel data:
  - All of P, R, S, T, U, V, W sub-codes are set to 0 (binary) for the entire
    lead-in.
  - Q sub-code track no. is set to 0.
  - Q sub-code index is set to 0.
  - Q sub-code control/ADR is set 0.
  - Q sub-code correct increasing relative sector MSF address, starting from
    00:00:00 is set.
  - Q sub-code correct increasing absolute sector MSF address, starting from
    ATIP start of lead-in time is set.
  - Q sub-code 16 bit CRC is calculated.
  - interlaced.

  Note: Assumes the following global variable exists:
          - lead_in_TOC_len
          - TOC
          - LBA_for_writing
          - skipped_LBA_for_writing
          - correct_LBA_for_writing
          - lead_in_type
          - f_leadin_TOC
          - subch_deint_PW_96 }
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   Data_1block_size      : Word;
   N_Sect_blocks_to_write : LongWord;
   Total_bytes_written   : LongWord;
   CRC16_val             : Word;
   i                     : Word;
   j                     : LongWord;
   stop_looping          : Boolean;
   SubCh_buf             : T_P_byte;
   Lead_in_MSF_time      : T_MSF;
   MSF_time_main_sector       : T_MSF;
   Sync_header           : T_array16_byte;
   TOC_curr_index        : Word;
   TOC_mod3              : Byte;
   p                     : ^Byte;
   track_in_TOC          : Boolean;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   s2                    : String;
   out_track_entry       : T_track_entry;
   buf_offset_to_check   : LongWord;
   buf_offset            : T_P_write_param;
   P_write_buffer_offset : Pointer;
Begin
     //Sizes of CD logical sector parts..
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     //For debugging..
     {$IFDEF DEBUG}
     s:='ATIP lead-in start time, && length: ' + IntToStr(LBA_for_writing) + ', ' +
     IntToStr(lead_in_TOC_len);
     Synchronize(ShowErrMsg);
     {$ENDIF}

     //Should always be 1.
     N_Sect_blocks_to_write:=1;
     //A counter for counting the no. of sectors to generate in cyclic secondary
     //write buffer.
     j:=lead_in_TOC_len;

     //Lead-in MSF to be used in the Q sub-channel as absolute MSF time.
     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                                  Lead_in_MSF_time.M,
                                                  Lead_in_MSF_time.S,
                                                  Lead_in_MSF_time.F);

     //Sector MSF time to be used in the CDROM sector sync header.
     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                                  MSF_time_main_sector.M,
                                                  MSF_time_main_sector.S,
                                                  MSF_time_main_sector.F);
     //Starting TOC list index, for cycling through the list (delphi TList index).
     TOC_curr_index:=0;

     //Check which type of lead-in the user wants to generate.
     Case lead_in_type Of
     U_TRACK_MODE_AUDIO:
     Begin
          //Generate a CDDA track mode lead-in.
          is_data_track:=False;
     End;
     U_TRACK_MODE_DATA:
     Begin
          //Generate a CDROM track mode lead-in.
          is_data_track:=True;
     End;
     U_TRACK_MODE_USE_TOC:
     Begin
          { Use TOC entry of Q: mode=1 & point=1 (track 1 TOC entry) to
            determine the CD data mode, which decides the lead-in track mode to
            generate. }
          If TOC.Find_mode_point_entry(out_TOC_entry, 1, 1) Then
          Begin
               //Determine the CD data mode from the TOC entry.
               Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                    $00,$10,$80,$90: is_data_track:=False;  //Generate a CDDA track mode lead-in.
                    $40,$50: is_data_track:=True;  //Generate a CDROM track mode lead-in.
               Else
                   //Generate a CDDA track mode lead-in.
                   is_data_track:=False;
               End;
          End
          Else
          Begin
               //Seems we have an invalid TOC, so we'll assume to
               //generate a CDDA track mode lead-in.
               is_data_track:=False;
          End;
     End;
     End;

     If Not is_data_track Then
     Begin
          //For CDDA, zeroise the entire block in the secondary write buffer.
          ZeroMemory(Form1.CommonCDSettings.P_write_buffer,
                     Form1.CommonCDSettings.sector_buffer_block_len);
     End;

     //Starting mod 3 value.
     TOC_mod3:=3;
     stop_looping:=False;
     Repeat
           //Calculate the next buffer offset that we want to use for checking
           //the write parameters buffer for ready to read status. Note, this
           //offset is a relative normalised value.
           If (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading<j) And
              (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading>N_Sect_blocks_to_write) Then
           Begin
                If (Form1.CommonCDSettings.current_start_loc_of_reading+
                    Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1)<
                    Form1.CommonCDSettings.sector_buffer_block_len Then
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                          Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1;
                End
                Else
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.sector_buffer_block_len-1;
                End;
           End
           Else
           Begin
                buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                     N_Sect_blocks_to_write-1;
           End;

           //A buffer that points at the start of a sector in the secondary write buffer.
           P_write_buffer_offset:=Form1.CommonCDSettings.P_write_buffer;
           Inc(T_P_byte(P_write_buffer_offset),
               Form1.CommonCDSettings.current_start_loc_of_reading*Data_1block_size);

           //This code is same as the one below it, but that one is better.
           //SubCh_buf:=Ptr(Integer(P_write_buffer_offset) + Sector_block_size);

           //A buffer that points at the start of a sub-ch in the secondary write buffer.
           SubCh_buf:=P_write_buffer_offset;
           Inc(SubCh_buf, Sector_block_size);

           If is_data_track Then
           Begin
                //Zeroise 1 main sector size in the secondary write buffer.
                ZeroMemory(P_write_buffer_offset,
                           Sector_block_size);

                track_in_TOC:=True;
                //Find track 1 TOC entry to determine the data mode.
                If tracks.Find_track_no_entry_by_track_no(out_track_entry, 1) Then
                Begin
                     //Generates CDROM 16 byte sync header..

                     //Fill_sync_block(Form1.SCSI.SPC_any_link.Get_data_buf, CD_mode_to_data_mode(out_track_entry.CD_mode), True, MSF_time_main_sector);

                     Sync_header[1]:=0;
                     For i:=2 To 11 Do
                     Begin
                          Sync_header[i]:=$FF;
                     End;
                     Sync_header[12]:=0;
                     //Sync_header[13]:=StrToInt('$'+IntToStr(MSF_time_main_sector.M))+$A0;
                     Sync_header[13]:=StrToInt('$'+'A'+Copy(IntToStr(MSF_time_main_sector.M),2,1));
                     Sync_header[14]:=StrToInt('$'+IntToStr(MSF_time_main_sector.S));
                     Sync_header[15]:=StrToInt('$'+IntToStr(MSF_time_main_sector.F));
                     Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
                     T_array16_byte(P_write_buffer_offset^):=Sync_header;
                End
                Else
                Begin
                     //No track 1 TOC entry. So we treat it as a CDDA track.
                     track_in_TOC:=False;
                End;
           End;

           //A buffer that points at where we want to check the write parameters buffer.
           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, buf_offset_to_check);

           //A loop to check whether we need to do more reading to put into the buffer.
           wait_for_ready_to_read:=True;
           While((Not Terminated) And wait_for_ready_to_read) Do
           Begin
                If buf_offset^.is_block_for_reading Then
                Begin
                     //We can now do some more reading, so exit the loop.
                     wait_for_ready_to_read:=False;
                End
                Else
                Begin
                     //Reading buffer is done for now (limited by threshold
                     //value), so we suspend this thread here for the write to
                     //catch up, which will wake this thread up.
                     Suspend;
                End;
           End;

           If Not Terminated Then
           Begin
                s2:='Lead-in TOC: '+
                IntToStr(LBA_for_writing) + '..' +
                IntToStr(LBA_for_writing+N_Sect_blocks_to_write-1) + ',' +
                IntToStr(N_Sect_blocks_to_write);

                If is_data_track Then
                    s2:=s2+' (data track).'
                Else
                    s2:=s2+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption:=s2;
           End;

           If lead_in_file_type=TOC_FILE_TYPE_96_REPEATED_DEINT Then
           Begin
                f_leadin_TOC.ReadBuffer(subch_deint_PW_96, 96);
           End
           Else
           Begin
                If TOC_mod3=3 Then
                Begin
                     subch_deint_PW_96:=T_subch_deint_PW_96(TOC.TOC_data.Items[TOC_curr_index]^);
                     If TOC_curr_index+1=TOC.TOC_data.Count Then
                         TOC_curr_index:=0
                     Else
                         TOC_curr_index:=TOC_curr_index+1;
                     TOC_mod3:=0;
                End;
                TOC_mod3:=TOC_mod3+1;
           End;
           subch_deint_PW_96.PW[2][4]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.M));
           subch_deint_PW_96.PW[2][5]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.S));
           subch_deint_PW_96.PW[2][6]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.F));
           //Calc CRC (X25 standard CRC 16) of the first 10 bytes of Q sub-channel.
           CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
           subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
           subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;
           {$IFNDEF DEBUG_W_OUTPUT_DEINT_SUBS}
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, Pointer(SubCh_buf));
           {$ELSE}
           //For debugging only. Don't interlace sub-channel data.
           T_subch_deint_PW_96(Pointer(SubCh_buf)^):=subch_deint_PW_96;
           {$ENDIF}

           If is_data_track Then
           Begin
                If track_in_TOC Then
                Begin
                     Fill_EDCECC(P_write_buffer_offset, out_track_entry.CD_mode);

                     Fill_P(P_write_buffer_offset, out_track_entry.CD_mode);
                     Fill_Q(P_write_buffer_offset, out_track_entry.CD_mode);

                     If out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                        Fill_Mode2Form2_SubHdr_20h(P_write_buffer_offset);

                     {$IFNDEF DEBUG_W_OUTPUT_UNSCRAMBLED}
                     Scramble(P_write_buffer_offset);
                     {$ENDIF}
                End;
           End;

           {$IFDEF DEBUG}
           debugf.WriteBuffer(P_write_buffer_offset^, Data_1block_size);
           {$ENDIF}

           //A buffer that points at the current position in the write parameters buffer.
           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, Form1.CommonCDSettings.current_start_loc_of_reading);

           //Place new data in the write parameters buffer..
           buf_offset^.starting_MMCLBA_sector_to_write:=MMCLBA_for_writing;
           buf_offset^.no_of_sectors_to_write:=N_Sect_blocks_to_write;
           //Do we skip sector position area?
           DoWeSkipSect(buf_offset, LBA_for_writing);
           buf_offset^.is_block_for_reading:=False;

           //Calculate the next position to be used for cyclic secondary buffer.
           Form1.CommonCDSettings.current_start_loc_of_reading:=
           Form1.CommonCDSettings.current_start_loc_of_reading+N_Sect_blocks_to_write;
           //Check the position to see whether we need to cycle to start.
           If Form1.CommonCDSettings.current_start_loc_of_reading=
              Form1.CommonCDSettings.sector_buffer_block_len Then
           Begin
                Form1.CommonCDSettings.current_start_loc_of_reading:=0;
           End;

           LBA_for_writing:=LBA_for_writing+N_Sect_blocks_to_write;
           correct_LBA_for_writing:=correct_LBA_for_writing+N_Sect_blocks_to_write;
           skipped_LBA_for_writing:=skipped_LBA_for_writing+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector.
           If skipped_LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=skipped_LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=skipped_LBA_for_writing;

           //Decrement the counter for next round.
           j:=j-N_Sect_blocks_to_write;

           If is_data_track Then
           Begin
                //Increment the MSF time for the sector sync header.
                If MSF_time_main_sector.F=74 Then
                Begin
                     If MSF_time_main_sector.S=59 Then
                     Begin
                          If MSF_time_main_sector.M=99 Then
                          Begin
                               MSF_time_main_sector.M:=0;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End
                          Else
                          Begin
                               MSF_time_main_sector.M:=MSF_time_main_sector.M+1;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End;
                     End
                     Else
                     Begin
                          MSF_time_main_sector.S:=MSF_time_main_sector.S+1;
                          MSF_time_main_sector.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_main_sector.F:=MSF_time_main_sector.F+1;
                End;
           End;

           //Increment the MSF time for the lead-in Q subchannel.
           If Lead_in_MSF_time.F=74 Then
           Begin
                If Lead_in_MSF_time.S=59 Then
                Begin
                     If Lead_in_MSF_time.M=99 Then
                     Begin
                          Lead_in_MSF_time.M:=0;
                          Lead_in_MSF_time.S:=0;
                          Lead_in_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          Lead_in_MSF_time.M:=Lead_in_MSF_time.M+1;
                          Lead_in_MSF_time.S:=0;
                          Lead_in_MSF_time.F:=0;
                     End;
                End
                Else
                Begin
                     Lead_in_MSF_time.S:=Lead_in_MSF_time.S+1;
                     Lead_in_MSF_time.F:=0;
                End;
           End
           Else
           Begin
                Lead_in_MSF_time.F:=Lead_in_MSF_time.F+1;
           End;
           total_n_sectors_remaining:=total_n_sectors_remaining-N_Sect_blocks_to_write;
     Until (j=0) OR (stop_looping) OR Terminated;

     ZeroMemory(@subch_deint_PW_96, 96);

     //subch_deint_PW_96.PW[2][2]:=subch_deint_PW_96.PW[2][2]+1;
     //subch_deint_PW_96.PW[2][3]:=subch_deint_PW_96.PW[2][3]+1;
End;

Procedure TImageToCDReadThread.Make_pre_gap;
{ Generates a complete raw 2448 bytes/sector pregap in the cyclic write buffer.
  Note: Pre-gap is completely generated - not read from any file.

  If CDROM format, the sectors contains zeroes but formatted appropriately, i.e.
  complete with CDROM header, EDC/ECC values, P/Q values, & scrambled.  For
  CDDA, the sectors contains all zeroes and not scrambled (shouldn't be anyway).

  For the sub-channel data:
  - First pre-gap sector has P sub-codes set to 0 (binary) and set to 1 for the
    rest of the pre-gap sectors.
  - All of R, S, T, U, V, W sub-codes are set to 0 (binary) for the entire
    pre-gap.
  - *Q sub-code track no. is set to the same as the previous track as indicated
    from the previous sub-channel data.
  - Q sub-code index is set to 0.
  - Q sub-code control/ADR is set accordingly to the format of the CD.
  - Q sub-code correct decreasing relative sector MSF address, starting from
    00:02:00 is set. Note, this must be a decreasing time to 00:00:00, else
    you'll get a non readable CD.
  - Q sub-code correct increasing absolute sector MSF address is set.
  - Q sub-code 16 bit CRC is calculated.
  - interlaced.

  Note: *This procedure assumes that there exists previous sub-channel data in
        the sub-channel data buffer. }
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   Max_N_Sect_block_size : LongWord;
   Data_1block_size      : Word;
   N_Sect_blocks_to_write : LongWord;
   Total_bytes_written   : LongWord;
   CRC16_val             : Word;
   i                     : Word;
   j                     : LongWord;
   stop_looping          : Boolean;
   Out_ATIP_desc         : T_out_read_T_P_A_ATIP_desc;
   //subch_deint_PW_96           : T_subch_deint_PW_96;
   SubCh_buf             : Pointer;
   MSF_time_main_sector       : T_MSF;
   MSF_time_subch_q_rel    : T_MSF;
   MSF_time_subch_q_abs    : T_MSF;
   Sync_header           : T_array16_byte;
   p                     : ^Byte;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   s                     : String;
   out_track_entry       : T_track_entry;
   buf_offset_to_check   : LongWord;
   buf_offset            : T_P_write_param;
   P_write_buffer_offset : Pointer;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=Pre_gap_len;

     //Set all of P-code to 0 (binary).
     For i:=1 To 12 Do
     Begin
          subch_deint_PW_96.PW[1][i]:=$00;
     End;

     //For Philips style pregap..
     {MSF_time_subch_q_rel.M:=0;
     MSF_time_subch_q_rel.S:=2;
     MSF_time_subch_q_rel.F:=0;}

     //For Sony style pregap..
     MSF_time_subch_q_rel.M:=0;
     MSF_time_subch_q_rel.S:=1;
     MSF_time_subch_q_rel.F:=74;

     //Next track no., i.e. adds 1 to sub-channel Q track no.
     subch_deint_PW_96.PW[2][2]:=subch_deint_PW_96.PW[2][2]+1;

     { Find the next track no. entry in the TOC, and find out the CD data mode,
       which decides the track format for this track. }
     If TOC.Find_mode_point_entry(out_TOC_entry, 1, subch_deint_PW_96.PW[2][2]) Then
     Begin
          //Determine the CD data mode from the track TOC entry.
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;  //Audio CD, so generate a CDDA track.
               $40,$50: is_data_track:=True;  //Data CD, so generate a CDROM track.
          Else
              //Unknown CD data mode, so generate a CDDA track.
              is_data_track:=False;
          End;
     End
     Else
     Begin
          //Seems we have an invalid TOC, so we'll assume to
          //generate a CDDA track.
          is_data_track:=False;
     End;

     If is_data_track Then
     Begin
          ZeroMemory(Form1.CommonCDSettings.P_write_buffer,
                     Sector_block_size);

          {Form1.SCSI.MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                      MSF_time_main_sector.M,
                                      MSF_time_main_sector.S,
                                      MSF_time_main_sector.F);}
          //Rule: Sector block header for pregap must start from 0:0:0, otherwise CD
          //will still be normally be recognised as a blank, even though data has been
          //written.
          MSF_time_main_sector.M:=0;
          MSF_time_main_sector.S:=0;
          MSF_time_main_sector.F:=0;

          If tracks.Find_track_no_entry_by_LBA(out_track_entry, LBA_for_writing+j) Then
          Begin
               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               Sync_header[13]:=StrToInt('$'+IntToStr(MSF_time_main_sector.M));
               Sync_header[14]:=StrToInt('$'+IntToStr(MSF_time_main_sector.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(MSF_time_main_sector.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
               //Sync_header[16]:=1;
          End;
     End
     Else
     Begin
          ZeroMemory(Form1.CommonCDSettings.P_write_buffer,
                     Form1.CommonCDSettings.sector_buffer_block_len);
     End;

     subch_deint_PW_96.PW[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     subch_deint_PW_96.PW[2][3]:=0;
     subch_deint_PW_96.PW[2][4]:=MSF_time_subch_q_rel.M;
     subch_deint_PW_96.PW[2][5]:=MSF_time_subch_q_rel.S;
     subch_deint_PW_96.PW[2][6]:=MSF_time_subch_q_rel.F;
     subch_deint_PW_96.PW[2][7]:=0;
     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                                  MSF_time_subch_q_abs.M,
                                                  MSF_time_subch_q_abs.S,
                                                  MSF_time_subch_q_abs.F);

     subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.M));
     subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.S));
     subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
     subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
     subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

     stop_looping:=False;
     Repeat
           If (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading<j) And
              (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading>N_Sect_blocks_to_write) Then
           Begin
                If (Form1.CommonCDSettings.current_start_loc_of_reading+
                    Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1)<
                    Form1.CommonCDSettings.sector_buffer_block_len Then
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                          Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1;
                End
                Else
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.sector_buffer_block_len-1;
                End;
           End
           Else
           Begin
                buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                     N_Sect_blocks_to_write-1;
           End;

           P_write_buffer_offset:=Form1.CommonCDSettings.P_write_buffer;
           Inc(T_P_byte(P_write_buffer_offset),
               Form1.CommonCDSettings.current_start_loc_of_reading*Data_1block_size);

           If is_data_track Then
           Begin
                ZeroMemory(P_write_buffer_offset, Sector_block_size);
                T_array16_byte(P_write_buffer_offset^):=Sync_header;
           End;

           //SubCh_buf:=Ptr(Integer(P_write_buffer_offset) + Sector_block_size);
           SubCh_buf:=P_write_buffer_offset;
           Inc(T_P_byte(SubCh_buf), Sector_block_size);

           {$IFNDEF DEBUG_W_OUTPUT_DEINT_SUBS}
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, SubCh_buf);
           {$ELSE}
           //For debugging only. Don't interlace sub-channel data.
           T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;
           {$ENDIF}

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, buf_offset_to_check);

           wait_for_ready_to_read:=True;
           While((Not Terminated) And wait_for_ready_to_read) Do
           Begin
                If buf_offset^.is_block_for_reading Then
                Begin
                     wait_for_ready_to_read:=False;
                End
                Else
                Begin
                     Suspend;
                End;
           End;

           If Not Terminated Then
           Begin
                s:='Pregap: '+
                IntToStr(LBA_for_writing) + '..' +
                IntToStr(LBA_for_writing+N_Sect_blocks_to_write-1) + ',' +
                IntToStr(N_Sect_blocks_to_write);

                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption:=s;

                //Display_status;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=LBA_for_writing;}

           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(P_write_buffer_offset);

                Fill_EDCECC(P_write_buffer_offset, Out_track_entry.CD_mode);

                Fill_P(P_write_buffer_offset, Out_track_entry.CD_mode);
                Fill_Q(P_write_buffer_offset, Out_track_entry.CD_mode);

                {$IFNDEF DEBUG_W_OUTPUT_UNSCRAMBLED}
                Scramble(P_write_buffer_offset);
                {$ENDIF}
           End;

           {$IFDEF DEBUG}
           debugf.WriteBuffer(P_write_buffer_offset^, Data_1block_size);
           {$ENDIF}

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, Form1.CommonCDSettings.current_start_loc_of_reading);
           buf_offset^.starting_MMCLBA_sector_to_write:=MMCLBA_for_writing;
           buf_offset^.no_of_sectors_to_write:=N_Sect_blocks_to_write;
           //Do we skip sector position area?
           DoWeSkipSect(buf_offset, LBA_for_writing);
           buf_offset^.is_block_for_reading:=False;

           Form1.CommonCDSettings.current_start_loc_of_reading:=
           Form1.CommonCDSettings.current_start_loc_of_reading+N_Sect_blocks_to_write;
           If Form1.CommonCDSettings.current_start_loc_of_reading=
              Form1.CommonCDSettings.sector_buffer_block_len Then
           Begin
                Form1.CommonCDSettings.current_start_loc_of_reading:=0;
           End;

           LBA_for_writing:=LBA_for_writing+N_Sect_blocks_to_write;
           correct_LBA_for_writing:=correct_LBA_for_writing+N_Sect_blocks_to_write;
           skipped_LBA_for_writing:=skipped_LBA_for_writing+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=skipped_LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=skipped_LBA_for_writing;
           j:=j-N_Sect_blocks_to_write;

           //If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If is_data_track Then
           Begin
                If MSF_time_main_sector.F=74 Then
                Begin
                     If MSF_time_main_sector.S=59 Then
                     Begin
                          If MSF_time_main_sector.M=99 Then
                          Begin
                               MSF_time_main_sector.M:=0;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End
                          Else
                          Begin
                               MSF_time_main_sector.M:=MSF_time_main_sector.M+1;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End;
                     End
                     Else
                     Begin
                          MSF_time_main_sector.S:=MSF_time_main_sector.S+1;
                          MSF_time_main_sector.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_main_sector.F:=MSF_time_main_sector.F+1;
                End;
                Sync_header[13]:=StrToInt('$'+IntToStr(MSF_time_main_sector.M));
                Sync_header[14]:=StrToInt('$'+IntToStr(MSF_time_main_sector.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(MSF_time_main_sector.F));
           End;

           //Set all of P-code to 1 (binary).
           For i:=1 To 12 Do
           Begin
                subch_deint_PW_96.PW[1][i]:=$FF;
           End;

           //Calc q-channel relative MSF address
           If MSF_time_subch_q_rel.F=0 Then
           Begin
                If MSF_time_subch_q_rel.S=0 Then
                Begin
                     If MSF_time_subch_q_rel.M=0 Then
                     Begin
                          MSF_time_subch_q_rel.M:=99;
                          MSF_time_subch_q_rel.S:=59;
                          MSF_time_subch_q_rel.F:=74;
                     End
                     Else
                     Begin
                          MSF_time_subch_q_rel.M:=MSF_time_subch_q_rel.M-1;
                          MSF_time_subch_q_rel.S:=59;
                          MSF_time_subch_q_rel.F:=74;
                     End;
                End
                Else
                Begin
                     MSF_time_subch_q_rel.S:=MSF_time_subch_q_rel.S-1;
                     MSF_time_subch_q_rel.F:=74;
                End;
           End
           Else
           Begin
                MSF_time_subch_q_rel.F:=MSF_time_subch_q_rel.F-1;
           End;
           subch_deint_PW_96.PW[2][5]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.S));
           subch_deint_PW_96.PW[2][6]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.F));

           //Calc q-channel absolute MSF address
           If MSF_time_subch_q_abs.F=74 Then
           Begin
                If MSF_time_subch_q_abs.S=59 Then
                Begin
                     If MSF_time_subch_q_abs.M=99 Then
                     Begin
                          MSF_time_subch_q_abs.M:=0;
                          MSF_time_subch_q_abs.S:=0;
                          MSF_time_subch_q_abs.F:=0;
                     End
                     Else
                     Begin
                          MSF_time_subch_q_abs.M:=MSF_time_subch_q_abs.M+1;
                          MSF_time_subch_q_abs.S:=0;
                          MSF_time_subch_q_abs.F:=0;
                     End
                End
                Else
                Begin
                     MSF_time_subch_q_abs.S:=MSF_time_subch_q_abs.S+1;
                     MSF_time_subch_q_abs.F:=0;
                End;
           End
           Else
           Begin
                MSF_time_subch_q_abs.F:=MSF_time_subch_q_abs.F+1;
           End;
           subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.M));
           subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.S));
           subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
           subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
           subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

           total_n_sectors_remaining:=total_n_sectors_remaining-N_Sect_blocks_to_write;
     Until (j=0) OR (stop_looping) OR Terminated;

     //subch_deint_PW_96.PW[2][3]:=subch_deint_PW_96.PW[2][3]+1;
End;

Procedure TImageToCDReadThread.Read_file_2352_96;
{ Reads a 2352 bytes/sector file & 96 bytes/sector subchannel file and transfers
  them into the cyclic write buffer.

  If CDROM format, the sectors are scrambled.
  The sub-channel data are interlaced. }
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   Max_N_Sect_block_size : LongWord;
   Data_1block_size      : Word;
   N_Sect_blocks_to_write : LongWord;
   Total_bytes_written   : LongWord;
   i                     : LongWord;
   j                     : LongWord;
   stop_looping          : Boolean;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   next_track_LBA        : LongInt;
   i_track_no            : Byte;
   main_sector_buffer    : Pointer;
   SubCh_buf             : Pointer;
   main_sector_buffer_offset : Pointer;
   SubCh_buf_offset : Pointer;
   P_write_buffer_offset : Pointer;
   buf_offset            : T_P_write_param;
   buf_offset2           : Pointer;
   buf_offset_to_check   : LongWord;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     //MMCLBA_for_writing:=MMCLBA_for_writing+300;

     //N_Sect_blocks_to_write:=1;
     j:=f_data_area.Size DIV Sector_block_size;

     //A track no counter. Start at track 1.
     i_track_no:=1;
     { Use track TOC entry to find out the CD data mode, which decides the
       track format for this track. }
     If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no) Then
     Begin
          //Determine CD data mode from the track.
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;  //Audio CD, so it's a CDDA track.
               $40,$50: is_data_track:=True;  //Data CD, so it's a CDROM track.
          Else
              //Unknown CD data mode, so assume it's a CDDA track.
              is_data_track:=False;
          End;
     End
     Else
     Begin
          //Seems we have an invalid TOC, so we'll assume it's a CDDA track.
          is_data_track:=False;
     End;

     { Use the next track n TOC entry to find out the next track start LBA. }
     If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no+1) Then
     Begin
          next_track_LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                       T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                       T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
     End
     Else
     Begin
          //TOC entry not found, so use 99:59:74 + 1 -> Last possible position + 1
          next_track_LBA:=449850
     End;

     { Allocate a fixed size dynamic memory for reading of the files.
       Note this should be a good size, which the limit should depend on the
       speed (performance) of the harddrive and the user specified system
       performance ratio. This limit will be considered later. }
     GetMem(main_sector_buffer,
            Form1.CommonCDSettings.max_no_of_blocks_to_read_synch*Sector_block_size);
     GetMem(SubCh_buf,
            Form1.CommonCDSettings.max_no_of_blocks_to_read_synch*SubCh_block_size);

     //Form1.SCSI.SPC_any_link.Zero_data_buf;
     stop_looping:=False;
     Repeat
           If (Form1.CommonCDSettings.current_start_loc_of_reading+
               Form1.CommonCDSettings.max_no_of_blocks_to_read_synch)<
               Form1.CommonCDSettings.sector_buffer_block_len Then
           Begin
                N_Sect_blocks_to_write:=Form1.CommonCDSettings.max_no_of_blocks_to_read_synch;

                If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

                If (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading<j) And
                   (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading>N_Sect_blocks_to_write) Then
                Begin
                     If (Form1.CommonCDSettings.current_start_loc_of_reading+
                         Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1)<
                         Form1.CommonCDSettings.sector_buffer_block_len Then
                     Begin
                          buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                               Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1;
                     End
                     Else
                     Begin
                          buf_offset_to_check:=Form1.CommonCDSettings.sector_buffer_block_len-1;
                     End;
                End
                Else
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                          N_Sect_blocks_to_write-1;
                End;
           End
           Else
           Begin
                N_Sect_blocks_to_write:=Form1.CommonCDSettings.sector_buffer_block_len-
                                        Form1.CommonCDSettings.current_start_loc_of_reading;
                If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;
                buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                     N_Sect_blocks_to_write-1;
           End;

           P_write_buffer_offset:=Form1.CommonCDSettings.P_write_buffer;
           Inc(T_P_byte(P_write_buffer_offset),
               Form1.CommonCDSettings.current_start_loc_of_reading*Data_1block_size);

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, buf_offset_to_check);

           wait_for_ready_to_read:=True;
           While((Not Terminated) And wait_for_ready_to_read) Do
           Begin
                If buf_offset^.is_block_for_reading Then
                Begin
                     wait_for_ready_to_read:=False;
                End
                Else
                Begin
                     Suspend;
                End;
           End;

           If Not Terminated Then
           Begin
                s:='Data area: '+
                   IntToStr(LBA_for_writing) + '..' +
                   IntToStr(LBA_for_writing+N_Sect_blocks_to_write-1) + ',' +
                   IntToStr(N_Sect_blocks_to_write);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';
                Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption:=s;

                //Display_status;
           End;

           f_data_area.ReadBuffer(main_sector_buffer^, Sector_block_size*N_Sect_blocks_to_write);
           f_data_area_subch.ReadBuffer(SubCh_buf^, SubCh_block_size*N_Sect_blocks_to_write);
           {$IFDEF DEBUG}
           //debugf.WriteBuffer(main_sector_buffer^, Sector_block_size*N_Sect_blocks_to_write);
           //debugf.WriteBuffer(SubCh_buf^, SubCh_block_size*N_Sect_blocks_to_write);
           {$ENDIF}

           i:=0;
           main_sector_buffer_offset:=main_sector_buffer;
           SubCh_buf_offset:=SubCh_buf;
           buf_offset2:=P_write_buffer_offset;
           Repeat
                 {$IFDEF DEBUG}
                 debugf.WriteBuffer(main_sector_buffer_offset^, Sector_block_size);
                 debugf.WriteBuffer(SubCh_buf_offset^, SubCh_block_size);
                 {$ENDIF}
                 CopyMemory(buf_offset2, main_sector_buffer_offset, Sector_block_size);
                 //<><><> Start of scrambling CDROM data sectors <><><>
                 If is_data_track Then
                 Begin
                      {$IFNDEF DEBUG_W_OUTPUT_UNSCRAMBLED}
                      Scramble(buf_offset2);
                      {$ENDIF}
                 End;
                 //Go to the start of where subch data should be
                 Inc(T_P_byte(buf_offset2), Sector_block_size);
                 //Go to the next main sector data
                 Inc(T_P_byte(main_sector_buffer_offset), Sector_block_size);
                 //<><><> End of scrambling CDROM data sectors <><><>

                 //<><><> Start of interlacing subchannel data <><><>
                 If lead_in_file_type=TOC_FILE_TYPE_96_UNIQUE_INT Then
                 Begin
                      {$IFNDEF DEBUG_W_OUTPUT_DEINT_SUBS}
                      //Copy interlaced subch data into dynamic buffer
                      CopyMemory(buf_offset2, SubCh_buf_offset, SubCh_block_size);
                      {$ELSE}
                      //For debugging only. We want de-interlaced sub-channel data.
                      Form1.CommonCDSettings.Deint_subs(SubCh_buf_offset, buf_offset2);
                      {$ENDIF}
                 End
                 Else
                 Begin
                      //Copy de-interlaced subch data from dynamic buffer to a variable
                      subch_deint_PW_96:=T_subch_deint_PW_96(SubCh_buf_offset^);
                      {$IFNDEF DEBUG_W_OUTPUT_DEINT_SUBS}
                      //Interlace the subch data from variable, leaving result in dynamic buffer
                      Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, buf_offset2);
                      {$ELSE}
                      //For debugging only. Don't interlace sub-channel data.
                      T_subch_deint_PW_96(buf_offset2^):=subch_deint_PW_96;
                      {$ENDIF}
                 End;
                 //Go to the start of next sector
                 Inc(T_P_byte(buf_offset2), SubCh_block_size);
                 //Go to the next subch data
                 Inc(T_P_byte(SubCh_buf_offset), SubCh_block_size);
                 //<><><> End of interlacing subchannel data <><><>

                 i:=i+1;
           Until(i=N_Sect_blocks_to_write);

           buf_offset2:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(T_P_write_param(buf_offset2), Form1.CommonCDSettings.current_start_loc_of_reading);
           //ImageToCDWriteThread.Suspend;
           For i:=0 To N_Sect_blocks_to_write-1 Do
           Begin
                T_write_param(buf_offset2^).starting_MMCLBA_sector_to_write:=
                Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MMCLBA(skipped_LBA_for_writing);

                T_write_param(buf_offset2^).no_of_sectors_to_write:=
                N_Sect_blocks_to_write-i;

                //Do we skip sector position area?
                DoWeSkipSect(buf_offset2, LBA_for_writing);

                LBA_for_writing:=LBA_for_writing+1;
                correct_LBA_for_writing:=correct_LBA_for_writing+1;
                skipped_LBA_for_writing:=skipped_LBA_for_writing+1;

                T_write_param(buf_offset2^).is_block_for_reading:=False;
                Inc(T_P_write_param(buf_offset2), 1);

                If LBA_for_writing=next_track_LBA Then
                Begin
                     i_track_no:=i_track_no+1;
                     { Use track TOC entry to find out the CD data mode, which decides the
                       track format for this track. }
                     If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no) Then
                     Begin
                          //Determine CD data mode from the track.
                          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                               $00,$10,$80,$90: is_data_track:=False;  //Audio CD, so it's a CDDA track.
                               $40,$50: is_data_track:=True;  //Data CD, so it's a CDROM track.
                          Else
                              //Unknown CD data mode, so assume it's a CDDA track.
                              is_data_track:=False;
                          End;
                     End
                     Else
                     Begin
                          //Seems we have an invalid TOC, so we'll assume it's a CDDA track.
                          is_data_track:=False;
                     End;

                     { Use the next track n TOC entry to find out the next track start LBA. }
                     If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no+1) Then
                     Begin
                          next_track_LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                                       T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                                       T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
                     End
                     Else
                     Begin
                          //TOC entry not found, so use 99:59:74 + 1 -> Last possible position + 1
                          next_track_LBA:=449850
                     End;
                End;
           End;
           //ImageToCDWriteThread.Resume;

           Form1.CommonCDSettings.current_start_loc_of_reading:=
           Form1.CommonCDSettings.current_start_loc_of_reading+N_Sect_blocks_to_write;
           If Form1.CommonCDSettings.current_start_loc_of_reading=
              Form1.CommonCDSettings.sector_buffer_block_len Then
           Begin
                Form1.CommonCDSettings.current_start_loc_of_reading:=0;
           End;
           //LBA_for_writing:=LBA_for_writing+N_Sect_blocks_to_write;
           //skipped_LBA_for_writing:=skipped_LBA_for_writing+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=skipped_LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=skipped_LBA_for_writing;
           j:=j-N_Sect_blocks_to_write;
           total_n_sectors_remaining:=total_n_sectors_remaining-N_Sect_blocks_to_write;
     Until (j=0) OR (stop_looping) OR Terminated;

     FreeMem(main_sector_buffer);
     FreeMem(SubCh_buf);

     //subch_deint_PW_96.PW[2][3]:=subch_deint_PW_96.PW[2][3]+1;
End;

Procedure TImageToCDReadThread.Make_post_gap;
{ Generates a complete raw 2448 bytes/sector postgap in the cyclic write buffer.
  Note: Post-gap is completely generated - not read from any file.

  If CDROM format, the sectors contains zeroes but formatted appropriately, i.e.
  complete with CDROM header, EDC/ECC values, P/Q values, & scrambled.  For
  CDDA, the sectors contains all zeroes and not scrambled (shouldn't be anyway).

  For the sub-channel data:
  - First post-gap sector has P sub-codes set to 0 (binary) and set to 1 for the
    rest of the post-gap sectors.
  - All of R, S, T, U, V, W sub-codes are set to 0 (binary) for the entire
    post-gap.
  - *Q sub-code track no. is set to the same as the previous track as indicated
     from the previous sub-channel data.
  - *Q sub-code index is set to previous index + 1 from the previous sub-channel
    data.
  - Q sub-code control/ADR is set accordingly to the format of the CD.
  - Q sub-code correct increasing relative sector MSF address, starting from
    00:00:00 is set.
  - Q sub-code correct increasing absolute sector MSF address is set.
  - Q sub-code 16 bit CRC is calculated.
  - interlaced.

  Note: *This procedure assumes that there exists previous sub-channel data in
        the sub-channel data buffer. }
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   Max_N_Sect_block_size : LongWord;
   Data_1block_size      : Word;
   N_Sect_blocks_to_write : LongWord;
   Total_bytes_written   : LongWord;
   CRC16_val             : Word;
   i                     : Word;
   j                     : LongWord;
   stop_looping          : Boolean;
   Out_ATIP_desc         : T_out_read_T_P_A_ATIP_desc;
   //subch_deint_PW_96           : T_subch_deint_PW_96;
   SubCh_buf             : Pointer;
   MSF_time_main_sector       : T_MSF;
   MSF_time_subch_q_rel    : T_MSF;
   MSF_time_subch_q_abs    : T_MSF;
   Sync_header           : T_array16_byte;
   p                     : ^Byte;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   s                     : String;
   out_track_entry       : T_track_entry;
   buf_offset_to_check   : LongWord;
   buf_offset            : T_P_write_param;
   P_write_buffer_offset : Pointer;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=Post_gap_len;

     //Form1.SCSI.SPC_any_link.Zero_data_buf;

     //Set all of P-code to 0 (binary).
     For i:=1 To 12 Do
     Begin
          subch_deint_PW_96.PW[1][i]:=$00;
     End;
     MSF_time_subch_q_rel.M:=0;
     MSF_time_subch_q_rel.S:=0;
     MSF_time_subch_q_rel.F:=0;

     { Find the current track no. entry in the TOC, and find out the CD data mode,
       which decides the track format for this track. }
     If TOC.Find_mode_point_entry(out_TOC_entry, 1, subch_deint_PW_96.PW[2][2]) Then
     Begin
          //Determine the CD data mode from the track TOC entry.
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          Else
              //Unknown CD data mode, so generate a CDDA track.
              is_data_track:=False;
          End;
     End
     Else
     Begin
          //Seems we have an invalid TOC, so we'll assume to
          //generate a CDDA track.
          is_data_track:=False;
     End;

     If is_data_track Then
     Begin
          ZeroMemory(Form1.CommonCDSettings.P_write_buffer,
                     Sector_block_size);

          Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                                       MSF_time_main_sector.M,
                                                       MSF_time_main_sector.S,
                                                       MSF_time_main_sector.F);

          If tracks.Find_track_no_entry_by_LBA(out_track_entry, LBA_for_writing-1) Then
          Begin
               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               Sync_header[13]:=StrToInt('$'+IntToStr(MSF_time_main_sector.M));
               Sync_header[14]:=StrToInt('$'+IntToStr(MSF_time_main_sector.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(MSF_time_main_sector.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
          End;
     End
     Else
     Begin
          ZeroMemory(Form1.CommonCDSettings.P_write_buffer,
                     Form1.CommonCDSettings.sector_buffer_block_len);
     End;

     subch_deint_PW_96.PW[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     //subch_deint_PW_96.PW[2][2]:=0;  //Use previous sub-channel data, which is assumed to exist.
     //subch_deint_PW_96.PW[2][3]:=0;  //Use previous sub-channel data, which is assumed to exist.
     subch_deint_PW_96.PW[2][3]:=subch_deint_PW_96.PW[2][3]+1;  //Use previous sub-channel data, which is assumed to exist.
     subch_deint_PW_96.PW[2][4]:=MSF_time_subch_q_rel.M;
     subch_deint_PW_96.PW[2][5]:=MSF_time_subch_q_rel.S;
     subch_deint_PW_96.PW[2][6]:=MSF_time_subch_q_rel.F;
     subch_deint_PW_96.PW[2][7]:=0;
     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                                  MSF_time_subch_q_abs.M,
                                                  MSF_time_subch_q_abs.S,
                                                  MSF_time_subch_q_abs.F);

     subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.M));
     subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.S));
     subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
     subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
     subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

     stop_looping:=False;
     Repeat
           If (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading<j) And
              (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading>N_Sect_blocks_to_write) Then
           Begin
                If (Form1.CommonCDSettings.current_start_loc_of_reading+
                    Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1)<
                    Form1.CommonCDSettings.sector_buffer_block_len Then
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                          Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1;
                End
                Else
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.sector_buffer_block_len-1;
                End;
           End
           Else
           Begin
                buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                     N_Sect_blocks_to_write-1;
           End;

           P_write_buffer_offset:=Form1.CommonCDSettings.P_write_buffer;
           Inc(T_P_byte(P_write_buffer_offset),
               Form1.CommonCDSettings.current_start_loc_of_reading*Data_1block_size);

           If is_data_track Then
           Begin
                ZeroMemory(P_write_buffer_offset, Sector_block_size);
                T_array16_byte(P_write_buffer_offset^):=Sync_header;
           End;

           //SubCh_buf:=Ptr(Integer(P_write_buffer_offset) + Sector_block_size);
           SubCh_buf:=P_write_buffer_offset;
           Inc(T_P_byte(SubCh_buf), Sector_block_size);

           {$IFNDEF DEBUG_W_OUTPUT_DEINT_SUBS}
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, SubCh_buf);
           {$ELSE}
           //For debugging only. Don't interlace sub-channel data.
           T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;
           {$ENDIF}

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, buf_offset_to_check);

           wait_for_ready_to_read:=True;
           While((Not Terminated) And wait_for_ready_to_read) Do
           Begin
                If buf_offset^.is_block_for_reading Then
                Begin
                     wait_for_ready_to_read:=False;
                End
                Else
                Begin
                     Suspend;
                End;
           End;

           If Not Terminated Then
           Begin
                s:='Postgap: '+
                   IntToStr(LBA_for_writing) + '..' +
                   IntToStr(LBA_for_writing+N_Sect_blocks_to_write-1) + ',' +
                   IntToStr(N_Sect_blocks_to_write);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption:=s;

                //Display_status;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=LBA_for_writing;}
           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(P_write_buffer_offset);

                Fill_EDCECC(P_write_buffer_offset, Out_track_entry.CD_mode);

                Fill_P(P_write_buffer_offset, Out_track_entry.CD_mode);
                Fill_Q(P_write_buffer_offset, Out_track_entry.CD_mode);

                {$IFNDEF DEBUG_W_OUTPUT_UNSCRAMBLED}
                Scramble(P_write_buffer_offset);
                {$ENDIF}
           End;

           {$IFDEF DEBUG}
           debugf.WriteBuffer(P_write_buffer_offset^, Data_1block_size);
           {$ENDIF}

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, Form1.CommonCDSettings.current_start_loc_of_reading);
           buf_offset^.starting_MMCLBA_sector_to_write:=MMCLBA_for_writing;
           buf_offset^.no_of_sectors_to_write:=N_Sect_blocks_to_write;
           //Do we skip sector position area?
           DoWeSkipSect(buf_offset, LBA_for_writing);
           buf_offset^.is_block_for_reading:=False;

           Form1.CommonCDSettings.current_start_loc_of_reading:=
           Form1.CommonCDSettings.current_start_loc_of_reading+N_Sect_blocks_to_write;
           If Form1.CommonCDSettings.current_start_loc_of_reading=
              Form1.CommonCDSettings.sector_buffer_block_len Then
           Begin
                Form1.CommonCDSettings.current_start_loc_of_reading:=0;
           End;

           LBA_for_writing:=LBA_for_writing+N_Sect_blocks_to_write;
           correct_LBA_for_writing:=correct_LBA_for_writing+N_Sect_blocks_to_write;
           skipped_LBA_for_writing:=skipped_LBA_for_writing+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=skipped_LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=skipped_LBA_for_writing;
           j:=j-N_Sect_blocks_to_write;

           //If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           //Set all of P-code to 1 (binary).
           For i:=1 To 12 Do
           Begin
                subch_deint_PW_96.PW[1][i]:=$FF;
           End;

           If is_data_track Then
           Begin
                If MSF_time_main_sector.F=74 Then
                Begin
                     If MSF_time_main_sector.S=59 Then
                     Begin
                          If MSF_time_main_sector.M=99 Then
                          Begin
                               MSF_time_main_sector.M:=0;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End
                          Else
                          Begin
                               MSF_time_main_sector.M:=MSF_time_main_sector.M+1;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End;
                     End
                     Else
                     Begin
                          MSF_time_main_sector.S:=MSF_time_main_sector.S+1;
                          MSF_time_main_sector.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_main_sector.F:=MSF_time_main_sector.F+1;
                End;
                Sync_header[13]:=StrToInt('$'+IntToStr(MSF_time_main_sector.M));
                Sync_header[14]:=StrToInt('$'+IntToStr(MSF_time_main_sector.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(MSF_time_main_sector.F));
           End;

           //Calc q-channel relative MSF address
           If MSF_time_subch_q_rel.F=74 Then
           Begin
                If MSF_time_subch_q_rel.S=59 Then
                Begin
                     If MSF_time_subch_q_rel.M=99 Then
                     Begin
                          MSF_time_subch_q_rel.M:=0;
                          MSF_time_subch_q_rel.S:=0;
                          MSF_time_subch_q_rel.F:=0;
                     End
                     Else
                     Begin
                          MSF_time_subch_q_rel.M:=MSF_time_subch_q_rel.M+1;
                          MSF_time_subch_q_rel.S:=0;
                          MSF_time_subch_q_rel.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_subch_q_rel.S:=MSF_time_subch_q_rel.S+1;
                     MSF_time_subch_q_rel.F:=0;
                End;
           End
           Else
           Begin
                MSF_time_subch_q_rel.F:=MSF_time_subch_q_rel.F+1;
           End;
           subch_deint_PW_96.PW[2][4]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.M));
           subch_deint_PW_96.PW[2][5]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.S));
           subch_deint_PW_96.PW[2][6]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.F));

           //Calc q-channel absolute MSF address
           If MSF_time_subch_q_abs.F=74 Then
           Begin
                If MSF_time_subch_q_abs.S=59 Then
                Begin
                     If MSF_time_subch_q_abs.M=99 Then
                     Begin
                          MSF_time_subch_q_abs.M:=0;
                          MSF_time_subch_q_abs.S:=0;
                          MSF_time_subch_q_abs.F:=0;
                     End
                     Else
                     Begin
                          MSF_time_subch_q_abs.M:=MSF_time_subch_q_abs.M+1;
                          MSF_time_subch_q_abs.S:=0;
                          MSF_time_subch_q_abs.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_subch_q_abs.S:=MSF_time_subch_q_abs.S+1;
                     MSF_time_subch_q_abs.F:=0;
                End;
           End
           Else
           Begin
                MSF_time_subch_q_abs.F:=MSF_time_subch_q_abs.F+1;
           End;
           subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.M));
           subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.S));
           subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
           subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
           subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

           total_n_sectors_remaining:=total_n_sectors_remaining-N_Sect_blocks_to_write;
     Until (j=0) OR (stop_looping) OR Terminated;

     subch_deint_PW_96.PW[2][2]:=subch_deint_PW_96.PW[2][2]+1;  //Increase the track no. for the next track.
     subch_deint_PW_96.PW[2][3]:=subch_deint_PW_96.PW[2][3]+1;  //Increase the index for the next track.
End;

Procedure TImageToCDReadThread.Make_lead_out;
{ Generates a complete raw 2448 bytes/sector lead-out in the cyclic write buffer.
  Note: Lead-out is completely generated - not read from any file.

  If CDROM format, the sectors contains zeroes but formatted appropriately, i.e.
  complete with CDROM header, EDC/ECC values, P/Q values, & scrambled.  For
  CDDA, the sectors contains all zeroes and not scrambled (shouldn't be anyway).

  For the sub-channel data:
  - First P sub-code sector has P sub-codes set to 0 (binary) and also set to 0
    (binary) for the first 2 seconds, then alternates at 2Hz.
  - All of R, S, T, U, V, W sub-codes are set to 0 (binary) for the entire
    lead-out.
  - Q sub-code track no. is set to AAh.
  - Q sub-code index is set to 1.
  - Q sub-code control/ADR is set accordingly to the format of the CD.
  - Q sub-code correct increasing relative sector MSF address is set.
  - Q sub-code correct increasing absolute sector MSF address is set.
  - Q sub-code 16 bit CRC is calculated.
  - interlaced. }
Var
   Sector_block_size      : Word;
   SubCh_block_size       : Byte;
   Max_N_Sect_block_size  : LongWord;
   Data_1block_size       : Word;
   N_Sect_blocks_to_write : LongWord;
   Total_bytes_written    : LongWord;
   CRC16_val              : Word;
   i                      : Word;
   j                      : LongWord;
   stop_looping           : Boolean;
   Out_ATIP_desc          : T_out_read_T_P_A_ATIP_desc;
   subch_deint_PW_96      : T_subch_deint_PW_96;
   SubCh_buf              : Pointer;
   MSF_time_main_sector        : T_MSF;
   Sync_header            : T_array16_byte;
   MSF_time_subch_q_rel   : T_MSF;
   MSF_time_subch_q_abs   : T_MSF;
   subch_p_2Hz_counter    : Byte;
   subch_p_2Hz_bit_val    : Byte;
   p                      : ^Byte;
   out_TOC_entry          : T_TOC_entry;
   is_data_track          : Boolean;
   s                      : String;
   out_track_entry        : T_track_entry;
   buf_offset_to_check    : LongWord;
   buf_offset             : T_P_write_param;
   P_write_buffer_offset  : Pointer;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=Lead_out_len;

     //Form1.SCSI.SPC_any_link.Zero_data_buf;

     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                                  MSF_time_main_sector.M,
                                                  MSF_time_main_sector.S,
                                                  MSF_time_main_sector.F);

     subch_p_2Hz_counter:=0;
     subch_p_2Hz_bit_val:=$00;
     For i:=1 To 12 Do
     Begin
          //subch_deint_PW_96.PW[1][i]:=subch_p_2Hz_bit_val;
          subch_deint_PW_96.PW[1][i]:=$FF;
     End;

     MSF_time_subch_q_rel.M:=0;
     MSF_time_subch_q_rel.S:=0;
     MSF_time_subch_q_rel.F:=0;

     { Find the lead-out track entry in the TOC and find out the CD data mode,
       which decides the track format for this track. }
     If TOC.Find_mode_point_entry(out_TOC_entry, 1, $A2) Then
     Begin
          //Determine the CD data mode from the track TOC entry.
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          Else
              //Unknown CD data mode, so generate a CDDA track.
              is_data_track:=False;
          End;
     End
     Else
     Begin
          //Seems we have an invalid TOC, so we'll assume to
          //generate a CDDA track.
          is_data_track:=False;
     End;

     If is_data_track Then
     Begin
          ZeroMemory(Form1.CommonCDSettings.P_write_buffer,
                     Sector_block_size);

          If tracks.Find_track_no_entry_by_LBA(out_track_entry, LBA_for_writing-Post_gap_len) Then
          Begin
               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               Sync_header[13]:=StrToInt('$'+IntToStr(MSF_time_main_sector.M));
               Sync_header[14]:=StrToInt('$'+IntToStr(MSF_time_main_sector.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(MSF_time_main_sector.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
          End;
     End
     Else
     Begin
          ZeroMemory(Form1.CommonCDSettings.P_write_buffer,
                     Form1.CommonCDSettings.sector_buffer_block_len);
     End;

     subch_deint_PW_96.PW[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     subch_deint_PW_96.PW[2][2]:=$AA;
     subch_deint_PW_96.PW[2][3]:=1;
     subch_deint_PW_96.PW[2][4]:=MSF_time_subch_q_rel.M;
     subch_deint_PW_96.PW[2][5]:=MSF_time_subch_q_rel.S;
     subch_deint_PW_96.PW[2][6]:=MSF_time_subch_q_rel.F;
     subch_deint_PW_96.PW[2][7]:=0;
     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(correct_LBA_for_writing,
                                                  MSF_time_subch_q_abs.M,
                                                  MSF_time_subch_q_abs.S,
                                                  MSF_time_subch_q_abs.F);

     subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.M));
     subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.S));
     subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
     subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
     subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

     stop_looping:=False;
     Repeat
           If (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading<j) And
              (Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading>N_Sect_blocks_to_write) Then
           Begin
                If (Form1.CommonCDSettings.current_start_loc_of_reading+
                    Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1)<
                    Form1.CommonCDSettings.sector_buffer_block_len Then
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                          Form1.CommonCDSettings.no_of_blocks_to_wait_b4_reading-1;
                End
                Else
                Begin
                     buf_offset_to_check:=Form1.CommonCDSettings.sector_buffer_block_len-1;
                End;
           End
           Else
           Begin
                buf_offset_to_check:=Form1.CommonCDSettings.current_start_loc_of_reading+
                                     N_Sect_blocks_to_write-1;
           End;

           P_write_buffer_offset:=Form1.CommonCDSettings.P_write_buffer;
           Inc(T_P_byte(P_write_buffer_offset),
               Form1.CommonCDSettings.current_start_loc_of_reading*Data_1block_size);

           If is_data_track Then
           Begin
                ZeroMemory(P_write_buffer_offset, Sector_block_size);
                T_array16_byte(P_write_buffer_offset^):=Sync_header;
           End;

           //SubCh_buf:=Ptr(Integer(P_write_buffer_offset) + Sector_block_size);
           SubCh_buf:=P_write_buffer_offset;
           Inc(T_P_byte(SubCh_buf), Sector_block_size);

           {$IFNDEF DEBUG_W_OUTPUT_DEINT_SUBS}
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, SubCh_buf);
           {$ELSE}
           //For debugging only. Don't interlace sub-channel data.
           T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;
           {$ENDIF}

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, buf_offset_to_check);

           wait_for_ready_to_read:=True;
           While((Not Terminated) And wait_for_ready_to_read) Do
           Begin
                If buf_offset^.is_block_for_reading Then
                Begin
                     wait_for_ready_to_read:=False;
                End
                Else
                Begin
                     Suspend;
                End;
           End;

           If Not Terminated Then
           Begin
                s:='Lead-out: '+
                   IntToStr(LBA_for_writing) + '..' +
                   IntToStr(LBA_for_writing+N_Sect_blocks_to_write-1) + ',' +
                   IntToStr(N_Sect_blocks_to_write);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption:=s;

                //Display_status;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=LBA_for_writing;}

           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(P_write_buffer_offset);

                Fill_EDCECC(P_write_buffer_offset, Out_track_entry.CD_mode);

                Fill_P(P_write_buffer_offset, Out_track_entry.CD_mode);
                Fill_Q(P_write_buffer_offset, Out_track_entry.CD_mode);

                {$IFNDEF DEBUG_W_OUTPUT_UNSCRAMBLED}
                Scramble(P_write_buffer_offset);
                {$ENDIF}
           End;

           {$IFDEF DEBUG}
           debugf.WriteBuffer(P_write_buffer_offset^, Data_1block_size);
           {$ENDIF}

           buf_offset:=Form1.CommonCDSettings.P_write_param_buffer;
           Inc(buf_offset, Form1.CommonCDSettings.current_start_loc_of_reading);
           buf_offset^.starting_MMCLBA_sector_to_write:=MMCLBA_for_writing;
           buf_offset^.no_of_sectors_to_write:=N_Sect_blocks_to_write;
           //Do we skip sector position area?
           DoWeSkipSect(buf_offset, LBA_for_writing);
           buf_offset^.is_block_for_reading:=False;

           Form1.CommonCDSettings.current_start_loc_of_reading:=
           Form1.CommonCDSettings.current_start_loc_of_reading+N_Sect_blocks_to_write;
           If Form1.CommonCDSettings.current_start_loc_of_reading=
              Form1.CommonCDSettings.sector_buffer_block_len Then
           Begin
                Form1.CommonCDSettings.current_start_loc_of_reading:=0;
           End;

           LBA_for_writing:=LBA_for_writing+N_Sect_blocks_to_write;
           correct_LBA_for_writing:=correct_LBA_for_writing+N_Sect_blocks_to_write;
           skipped_LBA_for_writing:=skipped_LBA_for_writing+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_LBA_for_writing>=404850 Then
               MMCLBA_for_writing:=skipped_LBA_for_writing-450000
           Else
               MMCLBA_for_writing:=skipped_LBA_for_writing;
           j:=j-N_Sect_blocks_to_write;

           //If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           //2 seconds (150 sectors) where lead-out, p-subchannel=0.  Then after 2Hz.
           If j<=lead_out_len-150 Then
           Begin
                //Alternate..
                subch_p_2Hz_counter:=subch_p_2Hz_counter+1;
                If subch_p_2Hz_counter>19 Then
                Begin
                     //subch_p_2Hz_bit_val:=NOT subch_p_2Hz_bit_val;
                     If subch_p_2Hz_bit_val=0 Then
                         subch_p_2Hz_bit_val:=$FF
                     Else
                         subch_p_2Hz_bit_val:=$00;
                     subch_p_2Hz_counter:=0;
                End;
                For i:=1 To 12 Do
                Begin
                     subch_deint_PW_96.PW[1][i]:=subch_p_2Hz_bit_val;
                End;
           End
           Else
           Begin
                //2 seconds (150 sectors) where lead-out, p-subchannel=0.
                For i:=1 To 12 Do
                Begin
                     subch_deint_PW_96.PW[1][i]:=$00;
                End;
           End;

           If is_data_track Then
           Begin
                If MSF_time_main_sector.F=74 Then
                Begin
                     If MSF_time_main_sector.S=59 Then
                     Begin
                          If MSF_time_main_sector.M=99 Then
                          Begin
                               MSF_time_main_sector.M:=0;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End
                          Else
                          Begin
                               MSF_time_main_sector.M:=MSF_time_main_sector.M+1;
                               MSF_time_main_sector.S:=0;
                               MSF_time_main_sector.F:=0;
                          End;
                     End
                     Else
                     Begin
                          MSF_time_main_sector.S:=MSF_time_main_sector.S+1;
                          MSF_time_main_sector.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_main_sector.F:=MSF_time_main_sector.F+1;
                End;
                Sync_header[13]:=StrToInt('$'+IntToStr(MSF_time_main_sector.M));
                Sync_header[14]:=StrToInt('$'+IntToStr(MSF_time_main_sector.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(MSF_time_main_sector.F));
           End;

           //Calc q-channel relative MSF address
           If MSF_time_subch_q_rel.F=74 Then
           Begin
                If MSF_time_subch_q_rel.S=59 Then
                Begin
                     If MSF_time_subch_q_rel.M=99 Then
                     Begin
                          MSF_time_subch_q_rel.M:=0;
                          MSF_time_subch_q_rel.S:=0;
                          MSF_time_subch_q_rel.F:=0;
                     End
                     Else
                     Begin
                          MSF_time_subch_q_rel.M:=MSF_time_subch_q_rel.M+1;
                          MSF_time_subch_q_rel.S:=0;
                          MSF_time_subch_q_rel.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_subch_q_rel.S:=MSF_time_subch_q_rel.S+1;
                     MSF_time_subch_q_rel.F:=0;
                End;
           End
           Else
           Begin
                MSF_time_subch_q_rel.F:=MSF_time_subch_q_rel.F+1;
           End;
           subch_deint_PW_96.PW[2][4]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.M));
           subch_deint_PW_96.PW[2][5]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.S));
           subch_deint_PW_96.PW[2][6]:=StrToInt('$'+IntToStr(MSF_time_subch_q_rel.F));

           //Calc q-channel absolute MSF address
           If MSF_time_subch_q_abs.F=74 Then
           Begin
                If MSF_time_subch_q_abs.S=59 Then
                Begin
                     If MSF_time_subch_q_abs.M=99 Then
                     Begin
                          MSF_time_subch_q_abs.M:=0;
                          MSF_time_subch_q_abs.S:=0;
                          MSF_time_subch_q_abs.F:=0;
                     End
                     Else
                     Begin
                          MSF_time_subch_q_abs.M:=MSF_time_subch_q_abs.M+1;
                          MSF_time_subch_q_abs.S:=0;
                          MSF_time_subch_q_abs.F:=0;
                     End;
                End
                Else
                Begin
                     MSF_time_subch_q_abs.S:=MSF_time_subch_q_abs.S+1;
                     MSF_time_subch_q_abs.F:=0;
                End;
           End
           Else
           Begin
                MSF_time_subch_q_abs.F:=MSF_time_subch_q_abs.F+1;
           End;
           subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.M));
           subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.S));
           subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(MSF_time_subch_q_abs.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
           subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
           subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

           total_n_sectors_remaining:=total_n_sectors_remaining-N_Sect_blocks_to_write;
     Until (j=0) OR (stop_looping) OR Terminated;

     {If j=0 Then
     Begin
          Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption:=
          Form1.Form5.ImageToCDProgressForm.LBL_read_sect.Caption+' finished.';
     End;}
End;

function Compare_SSP_list(Item1, Item2: Pointer): Integer;
begin
     Result := T_SSP_info(Item1^).SSP_start - T_SSP_info(Item2^).SSP_start;
end;

end.
