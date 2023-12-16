{ ****************************************************************
  A thread for writing data from a CD image to CDR/W disk.
  **************************************************************** }

unit ImageToCDThreadUnit;

interface

//{$DEFINE DEBUG}

uses
  Windows,
  Classes,
  SysUtils,
  MMC1Unit,
  GenSCSIUnit,
  CommonCDSettingsUnit,
  CRC_Unit,
  SPathSectorScramblerUnit,
  TOCUnit,
  EDC_ECC_Unit,
  Tracks_Unit,
  CDROM_struct_Unit;

{ My own constants for selecting lead-in data format type. }
const
     U_DATA_TRACK_MODE_USE_TOC=0;
     U_DATA_TRACK_MODE_AUDIO=1;
     U_DATA_TRACK_MODE_DATA=2;

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
  TImageToCDThread = class(TThread)
  private
    { Private declarations }
    procedure ShowErrMsg;
    procedure ShowConfirmToMsg;
  public
    confirm_from_msg : Boolean;
    MMCLBA_start_sector : LongInt;
    write_speed : Word;
    lead_in_file_type : Byte;
    f_leadin_TOC  : TFileStream;
    f_data_area : TFileStream;
    f_data_area_subch : TFileStream;
    f_pregap1 : TFileStream;
    f_LO : TFileStream;
    use_SSP  : Boolean;
    SSP_file : Ptr_text_file;
    SSP_method : Byte;
    SSP_info : T_SSP_info;
    SSP_list : TList;
    SSP_write_list : TList;
    total_n_sectors : LongInt;
    {$IFDEF DEBUG}
    debugf : TFileStream;
    {$ENDIF}
    curr_abs_LBA_to_write : LongInt;
    MMCLBA_sector : LongInt;
    skipped_curr_abs_LBA_to_write : LongInt;
    sub_PW_pack : T_sub_PW_pack;
    s  : String;
    Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc;
    TOC : T_TOC;
    tracks : T_tracks;
    P_buf : Pointer;
    Lead_in_len : LongInt;
    Pre_gap_len : LongInt;
    Post_gap_len : LongInt;
    Lead_out_len : LongInt;
    lead_in_type : Byte;
    Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;

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
                       in_use_SSP           : Boolean;
                       in_SSP_file          : Pointer;
                       in_SSP_method        : Byte);

    procedure Execute; override;
    procedure Display_status;
    procedure GetNextSkipSect;
    procedure DoSkipSect;
    procedure Read_SSP_write_into_list;
    function GetStartMMCLBAFromATIP : LongInt;
    procedure WriteLeadIn;
    procedure WritePregap;
    procedure WritePM;
    procedure WritePostGap;
    procedure WriteLeadOut;
    procedure Write_remaining_cache;
    procedure WriteFromFile(file_to_read : TFileStream;
                            data_track_mode : Byte;
                            track_no : Byte;
                            in_s : String);
  end;

function Compare_SSP_list(Item1, Item2: Pointer): Integer;

implementation

Uses
    MainFormUnit;

Constructor TImageToCDThread.Create(Thread_done_proc     : TNotifyEvent;
                                    in_start_MMCLBA      : Integer;
                                    in_write_speed       : Word;
                                    in_lead_in_file_type : Byte;
                                    in_lead_in_type      : Byte;
                                    in_f_leadin_TOC      : TFileStream;
                                    in_f_data_area       : TFileStream;
                                    in_f_data_area_subch : TFileStream;
                                    in_f_pregap1         : TFileStream;
                                    in_f_LO              : TFileStream;
                                    in_use_SSP           : Boolean;
                                    in_SSP_file          : Pointer;
                                    in_SSP_method        : Byte);
Begin
     MMCLBA_start_sector:=in_start_MMCLBA;
     write_speed:=in_write_speed;
     lead_in_file_type:=in_lead_in_file_type;
     lead_in_type:=in_lead_in_type;
     f_leadin_TOC :=in_f_leadin_TOC;
     f_data_area:=in_f_data_area;
     f_data_area_subch:=in_f_data_area_subch;
     f_pregap1:=in_f_pregap1;
     f_LO:=in_f_LO;
     use_SSP:=in_use_SSP;
     If in_use_SSP Then
        SSP_file:=in_SSP_file;
     SSP_method:=in_SSP_method;
     {$IFDEF DEBUG}
     debugf:=TFileStream.Create('OUTPUT.BIN', fmCreate);
     {$ENDIF}

     GetMem(P_buf, 1024);

     OnTerminate:=Thread_done_proc;
     FreeOnTerminate := True;  { Automatically destroy itself. }
     Inherited Create(False);  { Call the original create method. }
End;

procedure TImageToCDThread.Execute;
var
   i : Integer;
begin
     Form1.SCSI.MMC1.Do_set_CD_speed(MMC_SET_CD_SPEED_MAX, write_speed);
     Form1.SCSI.MMC1.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
     If Form1.SCSI.WNASPI32.SRB_status_OK Then
     Begin
          s:=IntToStr(Out_CD_cap_mech_st.CurrWriteSpeed);

          If Not Terminated Then
          Begin
               Form1.Form5.ImageToCDProgressForm.LBL_curr_write_speed.Caption:=s;
          End;
     End;

     SSP_write_list:=TList.Create;

     If use_SSP Then
     Begin
          If (SSP_method=0) Or (SSP_method=1) Then
          Begin
               GetNextSkipSect
          End
          Else
          Begin
               Read_SSP_write_into_list;
          End;
     End;

     { Reads lead-in file (TOC area of lead-in) and generate a unique TOC
       in memory as TOC object. }
     TOC:=T_TOC.Create(f_leadin_TOC, 96, lead_in_file_type);

     { Uses the TOC object and main sector image file to determine the
       individual track types and times. }
     tracks:=T_tracks.Create(f_data_area, TOC);

     If (use_SSP) And (SSP_method=2) And (SSP_write_list.Count>0) Then
     Begin
          { This section of if choice is to write in sections from outer sections
            to inner sections of CD (i.e. write sections backwards).  This will
            allow sector positions to be skipped.  I will call this asynchronous
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
          { This section of if choice is to write normally.  I will call this
            synchronous writing. }

          MMCLBA_sector:=MMCLBA_start_sector;
          curr_abs_LBA_to_write:=MMCLBA_start_sector;
          skipped_curr_abs_LBA_to_write:=curr_abs_LBA_to_write;

          Case lead_in_file_type Of
          TOC_FILE_TYPE_2448_FULL:
          Begin
               confirm_from_msg:=true;
               If MMCLBA_sector<>-((f_leadin_TOC.Size + f_pregap1.Size) DIV 2448) Then
               Begin
                    s:='ATIP start of lead-in time entered does not match size of lead-in from files.' + chr(13) +
                       'Do you still want to continue writing?';
                    Synchronize(ShowConfirmToMsg);
               End;

               If confirm_from_msg Then
               Begin
                    //MMCLBA_sector:=-((f.Size+f_pregap1.Size) DIV 2448);
                    //curr_abs_LBA_to_write:=MMCLBA_sector;

                    Pre_gap_len:=f_pregap1.Size DIV 2448;
                    Post_gap_len:=150;
                    Lead_in_len:=(f_leadin_TOC.Size DIV 2448)+
                                 Pre_gap_len;
                    Lead_out_len:=f_LO.Size DIV 2448;
                    total_n_sectors:=Lead_in_len+
                                     (f_data_area.Size DIV 2352)+
                                     Lead_out_len;

                    WriteFromFile(f_leadin_TOC, lead_in_type, 1, 'Writing lead-in TOC: ');

                    WriteFromFile(f_pregap1, U_DATA_TRACK_MODE_USE_TOC, 1, 'Writing pre-gap: ');

                    WritePM;

                    WriteFromFile(f_LO, U_DATA_TRACK_MODE_USE_TOC, $A2, 'Writing lead-out: ');

                    {$IFNDEF DEBUG}
                    Write_remaining_cache;
                    {$ENDIF}
               End;
          End;
          TOC_FILE_TYPE_96_REPEATED,
          TOC_FILE_TYPE_96_UNIQUE:
          Begin
               Pre_gap_len:=150;
               Post_gap_len:=150;

               If lead_in_file_type=TOC_FILE_TYPE_96_REPEATED Then
                   Lead_in_len:=(f_leadin_TOC.Size DIV 96)+Pre_gap_len
               Else
                   Lead_in_len:=Abs(GetStartMMCLBAFromATIP);

               Lead_out_len:=6750;

               total_n_sectors:=lead_in_len+
                                (f_data_area.Size DIV 2352)+
                                Lead_out_len;

               WriteLeadIn;
               //WritePostGap;  //ECMA-130 describes a post gap - but this will not work.
               WritePreGap;     //Instead it must be a pre-gap after the TOC in lead-in.
               WritePM;
               //WritePostGap;  //Post gap is assumed to be included in main file.
               WriteLeadOut;

               {$IFNDEF DEBUG}
               Write_remaining_cache;
               {$ENDIF}
          End;
          End;
     End;

     TOC.Free;
     tracks.Free;
     {$IFDEF DEBUG}
     debugf.Free;
     {$ENDIF}

     FreeMem(P_buf, 1024);

     { Free the list object. }
     SSP_write_list.Free;
end;

Procedure TImageToCDThread.Display_status;
Var s                  : String;
    drive_buflen       : LongWord;
    blank_drive_buflen : LongWord;
    used_drive_buflen  : LongWord;

Begin
     Form1.SCSI.WNASPI32.Transfer_P_Buf:=P_buf;
     Form1.SCSI.WNASPI32.Transfer_buf_len:=1024;

     Form1.SCSI.MMC1.Do_read_buf_cap;
     If Form1.SCSI.WNASPI32.SRB_status_OK Then
     Begin
          drive_buflen:=
          Form1.SCSI.GenSCSI.ReverseBytesToLongWord(
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).BufLen_HiByte,
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).BufLen_HiMiByte,
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).BufLen_LoMiByte,
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).BufLen_LoByte);

          blank_drive_buflen:=
          Form1.SCSI.GenSCSI.ReverseBytesToLongWord(
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).Blank_BufLen_HiByte,
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).Blank_BufLen_HiMiByte,
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).Blank_BufLen_LoMiByte,
          T_read_buf_cap_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^).Blank_BufLen_LoByte);

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

     Form1.SCSI.WNASPI32.Transfer_P_Buf:=Form1.SCSI.WNASPI32.Internal_P_Buf;
     Form1.SCSI.WNASPI32.Transfer_buf_len:=Form1.SCSI.WNASPI32.Internal_BufSize;
End;

Procedure TImageToCDThread.GetNextSkipSect;
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

Procedure TImageToCDThread.DoSkipSect;
Begin
     If use_SSP And ((SSP_method=0) Or (SSP_method=1)) Then
     Begin
     If SSP_info.SSP_len>0 Then
     Begin
          If curr_abs_LBA_to_write=SSP_info.SSP_start Then
          Begin
               skipped_curr_abs_LBA_to_write:=skipped_curr_abs_LBA_to_write+SSP_info.SSP_len;
               MMCLBA_sector:=Form1.SCSI.MMC1.LBA_to_MMCLBA(skipped_curr_abs_LBA_to_write);
               GetNextSkipSect;

               If SSP_method=1 Then
               Begin
                    {$IFNDEF DEBUG}
                    Write_remaining_cache;
                    {$ENDIF}
               End;
          End;
     End;
     End;
End;

procedure TImageToCDThread.Read_SSP_write_into_list;
var
   Ptr_SSP_write_info : ^T_SSP_write_info;
   s : String;
   i : Integer;
   Next_start : LongInt;
   total_SSP_sector_writes : LongInt;
begin
     Next_start:=-Lead_in_len;

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

Procedure TImageToCDThread.ShowErrMsg;
Begin
     Form1.Form5.ImageToCDProgressForm.ShowErrMsg(s);
End;

Procedure TImageToCDThread.ShowConfirmToMsg;
Begin
     confirm_from_msg:=Form1.Form5.ImageToCDProgressForm.ConfirmToMsg(s);
End;

Function TImageToCDThread.GetStartMMCLBAFromATIP : LongInt;
Begin
     Form1.SCSI.MMC1.Do_read_T_P_A_ATIP_out(Out_ATIP_desc);
     If Form1.SCSI.WNASPI32.SRB_status_OK Then
     Begin
          GetStartMMCLBAFromATIP:=Form1.SCSI.MMC1.MSF_to_MMCLBA(Out_ATIP_desc.StartMin,
                                                                Out_ATIP_desc.StartSec,
                                                                Out_ATIP_desc.StartFrame);
     End
     Else
         GetStartMMCLBAFromATIP:=0;
End;

procedure TImageToCDThread.WriteFromFile(file_to_read : TFileStream;
                                         data_track_mode : Byte;
                                         track_no : Byte;
                                         in_s : String);
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   Max_N_Sect_block_size : LongWord;
   Data_1block_size      : Word;
   N_Sect_blocks_to_write : LongWord;
   j                     : LongWord;
   stop_looping          : Boolean;
   SubCh_buf             : Pointer;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   next_track_LBA        : LongInt;
   i_track_no            : Byte;

Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.WNASPI32.BufSize DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;

     j:=file_to_read.Size DIV Data_1block_size;

     //skipped_curr_abs_LBA_to_write:=curr_abs_LBA_to_write;

     Case data_track_mode Of
     U_DATA_TRACK_MODE_AUDIO:
     Begin
          is_data_track:=False;
     End;
     U_DATA_TRACK_MODE_DATA:
     Begin
          is_data_track:=True;
     End;
     U_DATA_TRACK_MODE_USE_TOC:
     Begin
          i_track_no:=track_no;
          If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no) Then
          Begin
               Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                    $00,$10,$80,$90: is_data_track:=False;
                    $40,$50: is_data_track:=True;
               End;
          End;
     End;
     End;

     //99:59:74 + 1 -> last possible position + 1.
     next_track_LBA:=449850;

     //Check if we are not at lead-out
     If i_track_no<>$A2 Then
     Begin
          If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no+1) Then
          Begin
               next_track_LBA:=Form1.SCSI.MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                          T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                          T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
          End;
     End;

     Form1.SCSI.WNASPI32.ZeroP_Buf;
     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:=in_s+
                   IntToStr(curr_abs_LBA_to_write) + '..' +
                   IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';
                Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

                Display_status;
           End;

           file_to_read.ReadBuffer(Form1.SCSI.WNASPI32.P_Buf^, Data_1block_size);
           SubCh_buf:=Form1.SCSI.WNASPI32.P_Buf;
           Inc(T_P_byte(SubCh_buf), Sector_block_size);
           sub_PW_pack:=T_sub_PW_pack(SubCh_buf^);
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);

           If is_data_track Then
           Begin
                {$IFDEF DEBUG}
                debugf.WriteBuffer(Form1.SCSI.WNASPI32.P_Buf^, Data_1block_size);
                {$ENDIF}

                Scramble(Form1.SCSI.WNASPI32.P_Buf);
           End;

           //Do we skip sector position area?
           DoSkipSect;

           {$IFNDEF DEBUG}
           Form1.SCSI.MMC1.Do_write(0, MMCLBA_sector, N_Sect_blocks_to_write, Data_1block_size);
           If Form1.SCSI.WNASPI32.SRB_status_OK Then
           Begin
           End
           Else
           Begin
                s:='Error while processing Write CD command.' + Chr(10) + Chr(13);
                s:=s + Form1.SCSI.MMC1.Get_err_msg;
                Synchronize(ShowErrMsg);
                stop_looping:=True;
           End;
           {$ENDIF}

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           skipped_curr_abs_LBA_to_write:=skipped_curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If curr_abs_LBA_to_write=next_track_LBA Then
           Begin
                i_track_no:=i_track_no+1;
                Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                     $00,$10,$80,$90: is_data_track:=False;
                     $40,$50: is_data_track:=True;
                End;
                If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no+1) Then
                Begin
                     next_track_LBA:=Form1.SCSI.MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
                End
                Else
                Begin
                     //99:59:74 + 1 -> Last possible position + 1
                     next_track_LBA:=449850
                End;
           End;
     Until (j=0) OR (stop_looping) OR Terminated;

     //sub_PW_pack.PW_byte[2][3]:=sub_PW_pack.PW_byte[2][3]+1;
End;

Procedure TImageToCDThread.WriteLeadIn;
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
   SubCh_buf             : P_Byte;
   Lead_in_MSF_time      : T_MSF;
   Sector_MSF_time       : T_MSF;
   Sync_header           : T_array16_byte;
   TOC_curr_index        : Word;
   TOC_mod3              : Byte;
   p                     : ^Byte;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   s2                    : String;
   out_track_entry       : T_track_entry;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.WNASPI32.BufSize DIV Data_1block_size;

     {Form1.SCSI.MMC1.Do_read_disc_info;
     If Form1.SCSI.WNASPI32.SRB_status_OK Then
     Begin
          With T_read_disc_info_CDB10_block(Form1.SCSI.WNASPI32.P_Buf^) Do
          Begin
               MMCLBA_sector:=Form1.SCSI.MMC1.MSF_to_MMCLBA(Last_sess_lead_in_start_MSF_HiMiByte,
                                                            Last_sess_lead_in_start_MSF_LoMiByte,
                                                            Last_sess_lead_in_start_MSF_LoByte);
          End;
     End;}

     //curr_abs_LBA_to_write:=MMCLBA_sector;

     //skipped_curr_abs_LBA_to_write:=curr_abs_LBA_to_write;

     {$IFDEF DEBUG}
     s:='ATIP lead-in start time: ' + IntToStr(curr_abs_LBA_to_write);
     Synchronize(ShowErrMsg);
     {$ENDIF}

     N_Sect_blocks_to_write:=1;
     j:=Lead_in_len;

     Lead_in_MSF_time.M:=0;
     Lead_in_MSF_time.S:=0;
     Lead_in_MSF_time.F:=0;

     {Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                Lead_in_MSF_time.M,
                                Lead_in_MSF_time.S,
                                Lead_in_MSF_time.F);}

     Form1.SCSI.WNASPI32.ZeroP_Buf;

     Sector_MSF_time.M:=0;
     Sector_MSF_time.S:=0;
     Sector_MSF_time.F:=0;

     {Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                Sector_MSF_time.M,
                                Sector_MSF_time.S,
                                Sector_MSF_time.F);}

     //TOC:=T_TOC.Create(f, 96, lead_in_file_type);
     TOC_curr_index:=0;

     //tracks:=T_tracks.Create(f2, TOC);

     If TOC.Find_mode_point_entry(out_TOC_entry, 1, 1) Then
     Begin
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          Else
              is_data_track:=False;
          End;
     End;

     If is_data_track Then
     Begin
          If tracks.Find_track_no_entry_by_track_no(out_track_entry, 1) Then
          Begin
               //Fill_sync_block(Form1.SCSI.WNASPI32.P_Buf, CD_mode_to_data_mode(out_track_entry.CD_mode), True, Sector_MSF_time);

               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M))+$A0;
               Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
               T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
          End;
     End;

     //SubCh_buf:=Ptr(LongWord(Form1.SCSI.WNASPI32.P_Buf) + Sector_block_size);
     SubCh_buf:=Form1.SCSI.WNASPI32.P_Buf;
     Inc(SubCh_buf, Sector_block_size);

     TOC_mod3:=3;
     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s2:='Writing lead-in: '+
                IntToStr(curr_abs_LBA_to_write) + '..' +
                IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1);

                If is_data_track Then
                    s2:=s2+' (data track).'
                Else
                    s2:=s2+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s2;

                Display_status;
           End;

           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           {If curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=curr_abs_LBA_to_write;}
           //f2.ReadBuffer(Form1.SCSI.WNASPI32.P_Buf^, Sector_block_size);
           //s:=IntToStr(MMCLBA_sector);
           //Synchronize(ShowErrMsg);
           If lead_in_file_type=TOC_FILE_TYPE_96_REPEATED Then
           Begin
                f_leadin_TOC.ReadBuffer(sub_PW_pack, 96);
           End
           Else
           Begin
                If TOC_mod3=3 Then
                Begin
                     sub_PW_pack:=T_sub_PW_pack(TOC.TOC_data.Items[TOC_curr_index]^);
                     If TOC_curr_index+1=TOC.TOC_data.Count Then
                         TOC_curr_index:=0
                     Else
                         TOC_curr_index:=TOC_curr_index+1;
                     TOC_mod3:=0;
                End;
                TOC_mod3:=TOC_mod3+1;
           End;
           sub_PW_pack.PW_byte[2][4]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.M));
           sub_PW_pack.PW_byte[2][5]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.S));
           sub_PW_pack.PW_byte[2][6]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.F));
           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(sub_PW_pack.PW_byte[2][1]), 10);
           sub_PW_pack.PW_byte[2][11]:=NOT (CRC16_val SHR 8);
           sub_PW_pack.PW_byte[2][12]:=NOT CRC16_val;
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@sub_PW_pack, @(SubCh_buf^));

           If is_data_track Then
           Begin
                Fill_EDCECC(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                Fill_P(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);
                Fill_Q(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(Form1.SCSI.WNASPI32.P_Buf);

                {$IFDEF DEBUG}
                debugf.WriteBuffer(Form1.SCSI.WNASPI32.P_Buf^, Data_1block_size);
                {$ENDIF}

                Scramble(Form1.SCSI.WNASPI32.P_Buf);
           End;

           //Do we skip sector position area?
           DoSkipSect;

           {$IFNDEF DEBUG}
           Form1.SCSI.MMC1.Do_write(0, MMCLBA_sector, N_Sect_blocks_to_write, Data_1block_size);
           If Form1.SCSI.WNASPI32.SRB_status_OK Then
           Begin
           End
           Else
           Begin
                s:='Error while processing Write CD command.' + Chr(10) + Chr(13);
                s:=s + Form1.SCSI.MMC1.Get_err_msg;
                Synchronize(ShowErrMsg);
                stop_looping:=True;
           End;
           {$ENDIF}

           p:=Form1.SCSI.WNASPI32.P_Buf;
           Inc(p, 12);
           ZeroMemory(p, 2340);

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           skipped_curr_abs_LBA_to_write:=skipped_curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If is_data_track Then
           Begin
                If Sector_MSF_time.F=74 Then
                Begin
                     If Sector_MSF_time.S=59 Then
                     Begin
                          Sector_MSF_time.M:=Sector_MSF_time.M+1;
                          Sector_MSF_time.S:=0;
                          Sector_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          Sector_MSF_time.S:=Sector_MSF_time.S+1;
                          Sector_MSF_time.F:=0;
                     End;
                End
                Else
                Begin
                     Sector_MSF_time.F:=Sector_MSF_time.F+1;
                End;
                Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M))+$A0;
                Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
                T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
           End;

           If Lead_in_MSF_time.F=74 Then
           Begin
                If Lead_in_MSF_time.S=59 Then
                Begin
                     Lead_in_MSF_time.M:=Lead_in_MSF_time.M+1;
                     Lead_in_MSF_time.S:=0;
                     Lead_in_MSF_time.F:=0;
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
     Until (j=0) OR (stop_looping) OR Terminated;

     ZeroMemory(@sub_PW_pack, 96);

     //sub_PW_pack.PW_byte[2][2]:=sub_PW_pack.PW_byte[2][2]+1;
     //sub_PW_pack.PW_byte[2][3]:=sub_PW_pack.PW_byte[2][3]+1;
End;

Procedure TImageToCDThread.WritePreGap;
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
   //sub_PW_pack           : T_sub_PW_pack;
   SubCh_buf             : Pointer;
   Sector_MSF_time       : T_MSF;
   sub_q_rel_MSF_time    : T_MSF;
   sub_q_abs_MSF_time    : T_MSF;
   Sync_header           : T_array16_byte;
   p                     : ^Byte;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   s                     : String;
   out_track_entry       : T_track_entry;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.WNASPI32.BufSize DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=Pre_gap_len;

     Form1.SCSI.WNASPI32.ZeroP_Buf;

     SubCh_buf:=Ptr(LongWord(Form1.SCSI.WNASPI32.P_Buf) + Sector_block_size);
     For i:=1 To 12 Do
     Begin
          sub_PW_pack.PW_byte[1][i]:=$FF;
     End;
     sub_q_rel_MSF_time.M:=0;
     sub_q_rel_MSF_time.S:=2;
     sub_q_rel_MSF_time.F:=0;

     sub_PW_pack.PW_byte[2][2]:=sub_PW_pack.PW_byte[2][2]+1;

     If TOC.Find_mode_point_entry(out_TOC_entry, 1, sub_PW_pack.PW_byte[2][2]) Then
     Begin
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          End;
     End;

     sub_PW_pack.PW_byte[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     //sub_PW_pack.PW_byte[2][2]:=0;
     sub_PW_pack.PW_byte[2][3]:=0;
     sub_PW_pack.PW_byte[2][4]:=sub_q_rel_MSF_time.M;
     sub_PW_pack.PW_byte[2][5]:=sub_q_rel_MSF_time.S;
     sub_PW_pack.PW_byte[2][6]:=sub_q_rel_MSF_time.F;
     sub_PW_pack.PW_byte[2][7]:=0;
     Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                sub_q_abs_MSF_time.M,
                                sub_q_abs_MSF_time.S,
                                sub_q_abs_MSF_time.F);

     sub_PW_pack.PW_byte[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
     sub_PW_pack.PW_byte[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
     sub_PW_pack.PW_byte[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(sub_PW_pack.PW_byte[2][1]), 10);
     sub_PW_pack.PW_byte[2][11]:=NOT (CRC16_val SHR 8);
     sub_PW_pack.PW_byte[2][12]:=NOT CRC16_val;
     //Interlace sub-channels
     Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);

     If is_data_track Then
     Begin
          {Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                     Sector_MSF_time.M,
                                     Sector_MSF_time.S,
                                     Sector_MSF_time.F);}
          //Rule: Sector block header for pregap must start from 0:0:0, otherwise CD
          //will still be normally be recognised as a blank, even though data has been
          //written.
          Sector_MSF_time.M:=0;
          Sector_MSF_time.S:=0;
          Sector_MSF_time.F:=0;

          If tracks.Find_track_no_entry_by_LBA(out_track_entry, curr_abs_LBA_to_write+j) Then
          Begin
               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M));
               Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
               //Sync_header[16]:=1;
               T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
          End;
     End;

     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:='Writing pregap: '+
                IntToStr(curr_abs_LBA_to_write) + '..' +
                IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1);

                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

                Display_status;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=curr_abs_LBA_to_write;}

           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(Form1.SCSI.WNASPI32.P_Buf);

                Fill_EDCECC(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                Fill_P(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);
                Fill_Q(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                {$IFDEF DEBUG}
                debugf.WriteBuffer(Form1.SCSI.WNASPI32.P_Buf^, Data_1block_size);
                {$ENDIF}

                Scramble(Form1.SCSI.WNASPI32.P_Buf);
           End;

           //Do we skip sector position area?
           DoSkipSect;

           {$IFNDEF DEBUG}
           Form1.SCSI.MMC1.Do_write(0, MMCLBA_sector, N_Sect_blocks_to_write, Data_1block_size);
           If Form1.SCSI.WNASPI32.SRB_status_OK Then
           Begin
           End
           Else
           Begin
                s:='Error while processing Write CD command.' + Chr(10) + Chr(13);
                s:=s + Form1.SCSI.MMC1.Get_err_msg;
                Synchronize(ShowErrMsg);
                stop_looping:=True;
           End;
           {$ENDIF}

           p:=Form1.SCSI.WNASPI32.P_Buf;
           Inc(p, 12);
           ZeroMemory(p, 2340);

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           skipped_curr_abs_LBA_to_write:=skipped_curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If is_data_track Then
           Begin
                If Sector_MSF_time.F=74 Then
                Begin
                     If Sector_MSF_time.S=59 Then
                     Begin
                          Sector_MSF_time.M:=Sector_MSF_time.M+1;
                          Sector_MSF_time.S:=0;
                          Sector_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          Sector_MSF_time.S:=Sector_MSF_time.S+1;
                          Sector_MSF_time.F:=0;
                     End;
                End
                Else
                Begin
                     Sector_MSF_time.F:=Sector_MSF_time.F+1;
                End;
                Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M));
                Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
                T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
           End;

           //Calc q-channel relative MSF address
           If sub_q_rel_MSF_time.F=0 Then
           Begin
                If sub_q_rel_MSF_time.S=0 Then
                Begin
                     sub_q_rel_MSF_time.M:=sub_q_rel_MSF_time.M-1;
                     sub_q_rel_MSF_time.S:=59;
                     sub_q_rel_MSF_time.F:=74;
                End
                Else
                Begin
                     sub_q_rel_MSF_time.S:=sub_q_rel_MSF_time.S-1;
                     sub_q_rel_MSF_time.F:=74;
                End;
           End
           Else
           Begin
                sub_q_rel_MSF_time.F:=sub_q_rel_MSF_time.F-1;
           End;
           sub_PW_pack.PW_byte[2][5]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.S));
           sub_PW_pack.PW_byte[2][6]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.F));

           //Calc q-channel absolute MSF address
           If sub_q_abs_MSF_time.F=74 Then
           Begin
                If sub_q_abs_MSF_time.S=59 Then
                Begin
                     sub_q_abs_MSF_time.M:=sub_q_abs_MSF_time.M+1;
                     sub_q_abs_MSF_time.S:=0;
                     sub_q_abs_MSF_time.F:=0;
                End
                Else
                Begin
                     sub_q_abs_MSF_time.S:=sub_q_abs_MSF_time.S+1;
                     sub_q_abs_MSF_time.F:=0;
                End;
           End
           Else
           Begin
                sub_q_abs_MSF_time.F:=sub_q_abs_MSF_time.F+1;
           End;
           sub_PW_pack.PW_byte[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
           sub_PW_pack.PW_byte[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
           sub_PW_pack.PW_byte[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(sub_PW_pack.PW_byte[2][1]), 10);
           sub_PW_pack.PW_byte[2][11]:=NOT (CRC16_val SHR 8);
           sub_PW_pack.PW_byte[2][12]:=NOT CRC16_val;
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);
     Until (j=0) OR (stop_looping) OR Terminated;

     //sub_PW_pack.PW_byte[2][3]:=sub_PW_pack.PW_byte[2][3]+1;
End;

Procedure TImageToCDThread.WritePM;
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   Max_N_Sect_block_size : LongWord;
   Data_1block_size      : Word;
   N_Sect_blocks_to_write : LongWord;
   Total_bytes_written   : LongWord;
   i                     : Byte;
   j                     : LongWord;
   stop_looping          : Boolean;
   SubCh_buf             : Pointer;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   next_track_LBA        : LongInt;
   i_track_no            : Byte;

Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.WNASPI32.BufSize DIV Data_1block_size;

     //MMCLBA_sector:=MMCLBA_sector+300;

     N_Sect_blocks_to_write:=1;
     j:=f_data_area.Size DIV Sector_block_size;

     i_track_no:=1;
     If TOC.Find_mode_point_entry(out_TOC_entry, 1, 1) Then
     Begin
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          End;
          If TOC.Find_mode_point_entry(out_TOC_entry, 1, 2) Then
          Begin
               next_track_LBA:=Form1.SCSI.MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                          T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                          T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
          End
          Else
          Begin
               //99:59:74 + 1 -> Last possible position + 1
               next_track_LBA:=449850
          End;
     End;

     Form1.SCSI.WNASPI32.ZeroP_Buf;
     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:='Writing program area: '+
                   IntToStr(curr_abs_LBA_to_write) + '..' +
                   IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';
                Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

                Display_status;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=curr_abs_LBA_to_write;}
           f_data_area.ReadBuffer(Form1.SCSI.WNASPI32.P_Buf^, Sector_block_size);
           SubCh_buf:=Ptr(LongWord(Form1.SCSI.WNASPI32.P_Buf) + Sector_block_size);
           f_data_area_subch.ReadBuffer(sub_PW_pack, SubCh_block_size);
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);

           If is_data_track Then
           Begin
                {$IFDEF DEBUG}
                debugf.WriteBuffer(Form1.SCSI.WNASPI32.P_Buf^, Data_1block_size);
                {$ENDIF}

                Scramble(Form1.SCSI.WNASPI32.P_Buf);
           End;

           //Do we skip sector position area?
           DoSkipSect;

           {$IFNDEF DEBUG}
           Form1.SCSI.MMC1.Do_write(0, MMCLBA_sector, N_Sect_blocks_to_write, Data_1block_size);
           If Form1.SCSI.WNASPI32.SRB_status_OK Then
           Begin
           End
           Else
           Begin
                s:='Error while processing Write CD command.' + Chr(10) + Chr(13);
                s:=s + Form1.SCSI.MMC1.Get_err_msg;
                Synchronize(ShowErrMsg);
                stop_looping:=True;
           End;
           {$ENDIF}

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           skipped_curr_abs_LBA_to_write:=skipped_curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If curr_abs_LBA_to_write=next_track_LBA Then
           Begin
                i_track_no:=i_track_no+1;
                Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                     $00,$10,$80,$90: is_data_track:=False;
                     $40,$50: is_data_track:=True;
                End;
                If TOC.Find_mode_point_entry(out_TOC_entry, 1, i_track_no+1) Then
                Begin
                     next_track_LBA:=Form1.SCSI.MMC1.MSF_to_LBA(T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_M,
                                                                T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_S,
                                                                T_TOC_data_PQ((@out_TOC_entry)^).Q.Abs_or_Point_F);
                End
                Else
                Begin
                     //99:59:74 + 1 -> Last possible position + 1
                     next_track_LBA:=449850
                End;
           End;
     Until (j=0) OR (stop_looping) OR Terminated;

     //sub_PW_pack.PW_byte[2][3]:=sub_PW_pack.PW_byte[2][3]+1;
End;

Procedure TImageToCDThread.WritePostGap;
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
   //sub_PW_pack           : T_sub_PW_pack;
   SubCh_buf             : Pointer;
   Sector_MSF_time       : T_MSF;
   sub_q_rel_MSF_time    : T_MSF;
   sub_q_abs_MSF_time    : T_MSF;
   Sync_header           : T_array16_byte;
   p                     : ^Byte;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   s                     : String;
   out_track_entry       : T_track_entry;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.WNASPI32.BufSize DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=Post_gap_len;

     Form1.SCSI.WNASPI32.ZeroP_Buf;

     SubCh_buf:=Ptr(LongWord(Form1.SCSI.WNASPI32.P_Buf) + Sector_block_size);
     For i:=1 To 12 Do
     Begin
          sub_PW_pack.PW_byte[1][i]:=$FF;
     End;
     sub_q_rel_MSF_time.M:=0;
     sub_q_rel_MSF_time.S:=0;
     sub_q_rel_MSF_time.F:=0;

     If TOC.Find_mode_point_entry(out_TOC_entry, 1, sub_PW_pack.PW_byte[2][2]) Then
     Begin
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          End;
     End;

     sub_PW_pack.PW_byte[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     //sub_PW_pack.PW_byte[2][2]:=0;
     //sub_PW_pack.PW_byte[2][3]:=0;
     sub_PW_pack.PW_byte[2][3]:=sub_PW_pack.PW_byte[2][3]+1;
     sub_PW_pack.PW_byte[2][4]:=sub_q_rel_MSF_time.M;
     sub_PW_pack.PW_byte[2][5]:=sub_q_rel_MSF_time.S;
     sub_PW_pack.PW_byte[2][6]:=sub_q_rel_MSF_time.F;
     sub_PW_pack.PW_byte[2][7]:=0;
     Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                sub_q_abs_MSF_time.M,
                                sub_q_abs_MSF_time.S,
                                sub_q_abs_MSF_time.F);

     sub_PW_pack.PW_byte[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
     sub_PW_pack.PW_byte[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
     sub_PW_pack.PW_byte[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(sub_PW_pack.PW_byte[2][1]), 10);
     sub_PW_pack.PW_byte[2][11]:=NOT (CRC16_val SHR 8);
     sub_PW_pack.PW_byte[2][12]:=NOT CRC16_val;
     //Interlace sub-channels
     Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);

     If is_data_track Then
     Begin
          Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                     Sector_MSF_time.M,
                                     Sector_MSF_time.S,
                                     Sector_MSF_time.F);

          If tracks.Find_track_no_entry_by_LBA(out_track_entry, curr_abs_LBA_to_write-1) Then
          Begin
               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M));
               Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
               T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
          End;
     End;

     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:='Writing postgap: '+
                   IntToStr(curr_abs_LBA_to_write) + '..' +
                   IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

                Display_status;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=curr_abs_LBA_to_write;}
           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(Form1.SCSI.WNASPI32.P_Buf);

                Fill_EDCECC(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                Fill_P(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);
                Fill_Q(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                {$IFDEF DEBUG}
                debugf.WriteBuffer(Form1.SCSI.WNASPI32.P_Buf^, Data_1block_size);
                {$ENDIF}

                Scramble(Form1.SCSI.WNASPI32.P_Buf);
           End;

           //Do we skip sector position area?
           DoSkipSect;

           {$IFNDEF DEBUG}
           Form1.SCSI.MMC1.Do_write(0, MMCLBA_sector, N_Sect_blocks_to_write, Data_1block_size);
           If Form1.SCSI.WNASPI32.SRB_status_OK Then
           Begin
           End
           Else
           Begin
                s:='Error while processing Write CD command.' + Chr(10) + Chr(13);
                s:=s + Form1.SCSI.MMC1.Get_err_msg;
                Synchronize(ShowErrMsg);
                stop_looping:=True;
           End;
           {$ENDIF}

           p:=Form1.SCSI.WNASPI32.P_Buf;
           Inc(p, 12);
           ZeroMemory(p, 2340);

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           skipped_curr_abs_LBA_to_write:=skipped_curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If is_data_track Then
           Begin
                If Sector_MSF_time.F=74 Then
                Begin
                     If Sector_MSF_time.S=59 Then
                     Begin
                          Sector_MSF_time.M:=Sector_MSF_time.M+1;
                          Sector_MSF_time.S:=0;
                          Sector_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          Sector_MSF_time.S:=Sector_MSF_time.S+1;
                          Sector_MSF_time.F:=0;
                     End;
                End
                Else
                Begin
                     Sector_MSF_time.F:=Sector_MSF_time.F+1;
                End;
                Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M));
                Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
                T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
           End;

           //Calc q-channel relative MSF address
           If sub_q_rel_MSF_time.F=74 Then
           Begin
                If sub_q_rel_MSF_time.S=59 Then
                Begin
                     sub_q_rel_MSF_time.M:=sub_q_rel_MSF_time.M+1;
                     sub_q_rel_MSF_time.S:=0;
                     sub_q_rel_MSF_time.F:=0;
                End
                Else
                Begin
                     sub_q_rel_MSF_time.S:=sub_q_rel_MSF_time.S+1;
                     sub_q_rel_MSF_time.F:=0;
                End;
           End
           Else
           Begin
                sub_q_rel_MSF_time.F:=sub_q_rel_MSF_time.F+1;
           End;
           sub_PW_pack.PW_byte[2][4]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.M));
           sub_PW_pack.PW_byte[2][5]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.S));
           sub_PW_pack.PW_byte[2][6]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.F));

           //Calc q-channel absolute MSF address
           If sub_q_abs_MSF_time.F=74 Then
           Begin
                If sub_q_abs_MSF_time.S=59 Then
                Begin
                     sub_q_abs_MSF_time.M:=sub_q_abs_MSF_time.M+1;
                     sub_q_abs_MSF_time.S:=0;
                     sub_q_abs_MSF_time.F:=0;
                End
                Else
                Begin
                     sub_q_abs_MSF_time.S:=sub_q_abs_MSF_time.S+1;
                     sub_q_abs_MSF_time.F:=0;
                End;
           End
           Else
           Begin
                sub_q_abs_MSF_time.F:=sub_q_abs_MSF_time.F+1;
           End;
           sub_PW_pack.PW_byte[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
           sub_PW_pack.PW_byte[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
           sub_PW_pack.PW_byte[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(sub_PW_pack.PW_byte[2][1]), 10);
           sub_PW_pack.PW_byte[2][11]:=NOT (CRC16_val SHR 8);
           sub_PW_pack.PW_byte[2][12]:=NOT CRC16_val;
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);
     Until (j=0) OR (stop_looping) OR Terminated;

     sub_PW_pack.PW_byte[2][3]:=sub_PW_pack.PW_byte[2][2]+1;
     sub_PW_pack.PW_byte[2][2]:=sub_PW_pack.PW_byte[2][3]+1;
End;

Procedure TImageToCDThread.WriteLeadOut;
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
   sub_PW_pack           : T_sub_PW_pack;
   SubCh_buf             : Pointer;
   Sector_MSF_time       : T_MSF;
   Sync_header           : T_array16_byte;
   sub_q_rel_MSF_time    : T_MSF;
   sub_q_abs_MSF_time    : T_MSF;
   sub_p_2Hz_counter     : Byte;
   sub_p_2Hz_BIT_val     : Byte;
   p                     : ^Byte;
   out_TOC_entry         : T_TOC_entry;
   is_data_track         : Boolean;
   s                     : String;
   out_track_entry       : T_track_entry;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.WNASPI32.BufSize DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=Lead_out_len;

     Form1.SCSI.WNASPI32.ZeroP_Buf;

     Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                Sector_MSF_time.M,
                                Sector_MSF_time.S,
                                Sector_MSF_time.F);

     SubCh_buf:=Ptr(LongWord(Form1.SCSI.WNASPI32.P_Buf) + Sector_block_size);
     sub_p_2Hz_counter:=0;
     sub_p_2Hz_BIT_val:=$FF;
     For i:=1 To 12 Do
     Begin
          sub_PW_pack.PW_byte[1][i]:=sub_p_2Hz_BIT_val;
     End;

     sub_q_rel_MSF_time.M:=0;
     sub_q_rel_MSF_time.S:=0;
     sub_q_rel_MSF_time.F:=0;

     If TOC.Find_mode_point_entry(out_TOC_entry, 1, $A2) Then
     Begin
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          End;
     End;

     sub_PW_pack.PW_byte[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     sub_PW_pack.PW_byte[2][2]:=$AA;
     sub_PW_pack.PW_byte[2][3]:=1;
     sub_PW_pack.PW_byte[2][4]:=sub_q_rel_MSF_time.M;
     sub_PW_pack.PW_byte[2][5]:=sub_q_rel_MSF_time.S;
     sub_PW_pack.PW_byte[2][6]:=sub_q_rel_MSF_time.F;
     sub_PW_pack.PW_byte[2][7]:=0;
     Form1.SCSI.MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                sub_q_abs_MSF_time.M,
                                sub_q_abs_MSF_time.S,
                                sub_q_abs_MSF_time.F);

     sub_PW_pack.PW_byte[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
     sub_PW_pack.PW_byte[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
     sub_PW_pack.PW_byte[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(sub_PW_pack.PW_byte[2][1]), 10);
     sub_PW_pack.PW_byte[2][11]:=NOT (CRC16_val SHR 8);
     sub_PW_pack.PW_byte[2][12]:=NOT CRC16_val;
     //Interlace sub-channels
     Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);

     If is_data_track Then
     Begin
          If tracks.Find_track_no_entry_by_LBA(out_track_entry, curr_abs_LBA_to_write-Post_gap_len) Then
          Begin
               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M));
               Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
               T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
          End;
     End;

     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:='Writing lead-out: '+
                   IntToStr(curr_abs_LBA_to_write) + '..' +
                   IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

                Display_status;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=curr_abs_LBA_to_write;}

           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(Form1.SCSI.WNASPI32.P_Buf);

                Fill_EDCECC(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                Fill_P(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);
                Fill_Q(Form1.SCSI.WNASPI32.P_Buf, Out_track_entry.CD_mode);

                {$IFDEF DEBUG}
                debugf.WriteBuffer(Form1.SCSI.WNASPI32.P_Buf^, Data_1block_size);
                {$ENDIF}

                Scramble(Form1.SCSI.WNASPI32.P_Buf);
           End;

           //Do we skip sector position area?
           DoSkipSect;

           {$IFNDEF DEBUG}
           Form1.SCSI.MMC1.Do_write(0, MMCLBA_sector, N_Sect_blocks_to_write, Data_1block_size);
           If Form1.SCSI.WNASPI32.SRB_status_OK Then
           Begin
           End
           Else
           Begin
                s:='Error while processing Write CD command.' + Chr(10) + Chr(13);
                s:=s + Form1.SCSI.MMC1.Get_err_msg;
                Synchronize(ShowErrMsg);
                stop_looping:=True;
           End;
           {$ENDIF}

           p:=Form1.SCSI.WNASPI32.P_Buf;
           Inc(p, 12);
           ZeroMemory(p, 2340);

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           skipped_curr_abs_LBA_to_write:=skipped_curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           //Convert normal LBA sector to MMCLBA (MMC style LBA) sector
           If skipped_curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=skipped_curr_abs_LBA_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           sub_p_2Hz_counter:=sub_p_2Hz_counter+1;
           If sub_p_2Hz_counter>19 Then
           Begin
                //sub_p_2Hz_BIT_val:=NOT sub_p_2Hz_BIT_val;
                If sub_p_2Hz_BIT_val=0 Then
                    sub_p_2Hz_BIT_val:=$FF
                Else
                    sub_p_2Hz_BIT_val:=$00;
                sub_p_2Hz_counter:=0;
           End;
           For i:=1 To 12 Do
           Begin
                sub_PW_pack.PW_byte[1][i]:=sub_p_2Hz_BIT_val;
           End;

           If is_data_track Then
           Begin
                If Sector_MSF_time.F=74 Then
                Begin
                     If Sector_MSF_time.S=59 Then
                     Begin
                          Sector_MSF_time.M:=Sector_MSF_time.M+1;
                          Sector_MSF_time.S:=0;
                          Sector_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          Sector_MSF_time.S:=Sector_MSF_time.S+1;
                          Sector_MSF_time.F:=0;
                     End;
                End
                Else
                Begin
                     Sector_MSF_time.F:=Sector_MSF_time.F+1;
                End;
                Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M));
                Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
                T_array16_byte(Form1.SCSI.WNASPI32.P_Buf^):=Sync_header;
           End;

           //Calc q-channel relative MSF address
           If sub_q_rel_MSF_time.F=74 Then
           Begin
                If sub_q_rel_MSF_time.S=59 Then
                Begin
                     sub_q_rel_MSF_time.M:=sub_q_rel_MSF_time.M+1;
                     sub_q_rel_MSF_time.S:=0;
                     sub_q_rel_MSF_time.F:=0;
                End
                Else
                Begin
                     sub_q_rel_MSF_time.S:=sub_q_rel_MSF_time.S+1;
                     sub_q_rel_MSF_time.F:=0;
                End;
           End
           Else
           Begin
                sub_q_rel_MSF_time.F:=sub_q_rel_MSF_time.F-1;
           End;
           sub_PW_pack.PW_byte[2][4]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.M));
           sub_PW_pack.PW_byte[2][5]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.S));
           sub_PW_pack.PW_byte[2][6]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.F));

           //Calc q-channel absolute MSF address
           If sub_q_abs_MSF_time.F=74 Then
           Begin
                If sub_q_abs_MSF_time.S=59 Then
                Begin
                     sub_q_abs_MSF_time.M:=sub_q_abs_MSF_time.M+1;
                     sub_q_abs_MSF_time.S:=0;
                     sub_q_abs_MSF_time.F:=0;
                End
                Else
                Begin
                     sub_q_abs_MSF_time.S:=sub_q_abs_MSF_time.S+1;
                     sub_q_abs_MSF_time.F:=0;
                End;
           End
           Else
           Begin
                sub_q_abs_MSF_time.F:=sub_q_abs_MSF_time.F+1;
           End;
           sub_PW_pack.PW_byte[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
           sub_PW_pack.PW_byte[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
           sub_PW_pack.PW_byte[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(sub_PW_pack.PW_byte[2][1]), 10);
           sub_PW_pack.PW_byte[2][11]:=NOT (CRC16_val SHR 8);
           sub_PW_pack.PW_byte[2][12]:=NOT CRC16_val;
           //Interlace sub-channels
           Form1.CommonCDSettings.Int_subs(@sub_PW_pack, SubCh_buf);
     Until (j=0) OR (stop_looping) OR Terminated;
End;

procedure TImageToCDThread.Write_remaining_cache;
Var s : String;
Begin
     s:='Flushing buffers..';
     Form1.Form5.ImageToCDProgressForm.LBL_write_sect.Caption:=s;

     Form1.SCSI.MMC1.Do_synchronize_cache(0, 0);

     s:='Flushed.';
     Form1.Form5.ImageToCDProgressForm.LBL_drive_buffer_status.Caption:=s;
End;

function Compare_SSP_list(Item1, Item2: Pointer): Integer;
begin
     Result := T_SSP_info(Item1^).SSP_start - T_SSP_info(Item2^).SSP_start;
end;

end.
