unit CDToImageThreadUnit;

interface

uses
  Classes,
  SysUtils,
  Windows,
  Math,
  MMC1Unit,
  SPC_Unit,
  CommonCDSettingsUnit,
  TOCUnit,
  Tracks_Unit,
  CRC_Unit,
  CDROM_struct_Unit;

{type
    Ptr_byte=^Byte;
    T_P_subch_deint_PW_96=^T_subch_deint_PW_96;
    T_sub_PW_12byte_pack=Array[1..12] Of Byte;
    T_subch_deint_PW_96=Record
                        PW_byte : Array[1..8] Of T_sub_PW_12byte_pack;
                  End;
    T_P_sub_PW_96byte=^T_sub_PW_96byte;
    T_sub_PW_96byte=Record
                          PW_byte : Array[1..96] Of Byte;
                    End;}

const
     U_TRACK_MODE_AUDIO=0;
     U_TRACK_MODE_CDROM=1;
     U_TRACK_MODE_USE_TOC=2;

type T_array16_byte=Array[1..16] Of Byte;

type
  TCDToImageThread = class(TThread)
  private
    { Private declarations }
    StartSect           : LongInt;
    LeadOutSect         : LongInt;
    N_Sect              : LongWord;
    read_data_speed     : Word;
    FilterMode          : Byte;
    SubChSelMode        : Byte;
    is_TEB              : Boolean;
    f_main              : TFileStream;
    f_subch             : TFileStream;
    f_toc               : TFileStream;
    f_pregap1           : TFileStream;
    f_LO                : TFileStream;
    f_C2                : TFileStream;
    C2_error_field      : Byte;
    C2_read_order       : Byte;
    lead_in_type        : Byte;
    lead_in_size        : LongInt;
    lead_out_size       : LongInt;
    is_deint_subs       : Boolean;
    show_err_msg        : String;
    subch_deint_PW_96   : T_subch_deint_PW_96;
    sub_int_PW_96       : T_sub_int_PW_96;
    TOC                 : T_TOC;
    tracks              : T_tracks;
    Pre_gap_len         : Word;
    Post_gap_len        : Word;
    MMCLBA_sector       : LongInt;
    curr_abs_LBA_to_write : LongInt;
    procedure ShowErrMsg;
  protected
    procedure Execute; override;
    procedure ReadCDToImage(in_main_file : TFileStream;
                            in_subch_file : TFileStream;
                            in_C2_file : TFileStream);
    procedure FillA0SubChEntry(First_trkNo : Byte;
                               Ctrl        : Byte);
    procedure AddA0TOCEntry(First_trkNo : Byte;
                            Ctrl        : Byte);
    procedure AddA1TOCEntry(Last_trkNo : Byte);
    function Read1TOCEntryFromCD(Read_first      : Boolean;
                                 Var First_trkNo : Byte;
                                 Var Last_trkNo  : Byte;
                                 Var Ctrl        : Byte) : Boolean;
    procedure ReadTOCToMem;
    procedure ReadTOCToImage;
    function Read1FullTOCEntryFromCD(Read_first : Boolean) : Boolean;
    procedure ReadFullTOCToMem;
    procedure ReadFullTOCToImage;
    procedure Gen_TOC_image(in_toc_file : TFileStream);
    procedure Gen_pregap(in_pregap1_file : TFileStream;
                         in_preceding_trackno : Byte);
    procedure Gen_LO_image(in_LO_file : TFileStream);
  public
    Constructor Create(Thread_done_proc       : TNotifyEvent;
                       in_StartSect           : LongInt;
                       in_N_Sect              : LongWord;
                       in_read_data_speed     : Word;
                       in_FilterMode          : Byte;
                       in_SubChSelMode        : Byte;
                       in_is_TEB              : Boolean;
                       in_f_main              : TFileStream;
                       in_f_subch             : TFileStream;
                       in_f_toc               : TFileStream;
                       in_f_pregap1           : TFileStream;
                       in_f_LO                : TFileStream;
                       in_f_C2                : TFileStream;
                       in_C2_error_field      : Byte;
                       in_C2_read_order       : Byte;
                       in_lead_in_type        : Byte;
                       in_lead_in_size        : LongInt;
                       in_lead_out_size       : LongInt;
                       in_deinterleave        : Boolean);
  end;

implementation

Uses
    MainFormUnit;

Constructor TCDToImageThread.Create(Thread_done_proc       : TNotifyEvent;
                                    in_StartSect           : LongInt;
                                    in_N_Sect              : LongWord;
                                    in_read_data_speed     : Word;
                                    in_FilterMode          : Byte;
                                    in_SubChSelMode        : Byte;
                                    in_is_TEB              : Boolean;
                                    in_f_main              : TFileStream;
                                    in_f_subch             : TFileStream;
                                    in_f_toc               : TFileStream;
                                    in_f_pregap1           : TFileStream;
                                    in_f_LO                : TFileStream;
                                    in_f_C2                : TFileStream;
                                    in_C2_error_field      : Byte;
                                    in_C2_read_order       : Byte;
                                    in_lead_in_type        : Byte;
                                    in_lead_in_size        : LongInt;
                                    in_lead_out_size       : LongInt;
                                    in_deinterleave        : Boolean);
Begin
     StartSect       :=in_StartSect;
     N_Sect          :=in_N_Sect;
     read_data_speed :=in_read_data_speed;
     FilterMode      :=in_FilterMode;
     SubChSelMode    :=in_SubChSelMode;
     is_TEB          :=in_is_TEB;
     f_main          :=in_f_main;
     f_subch         :=in_f_subch;
     f_toc           :=in_f_toc;
     f_pregap1       :=in_f_pregap1;
     f_LO            :=in_f_LO;
     f_C2            :=in_f_C2;
     C2_read_order   :=in_C2_read_order;
     C2_error_field  :=in_C2_error_field;
     lead_in_type    :=in_lead_in_type;
     lead_in_size    :=in_lead_in_size;
     lead_out_size   :=in_lead_out_size;
     is_deint_subs   :=in_deinterleave;

     //GetMem(De_SubCh_buf, 96);
     TOC:=T_TOC.Create;

     OnTerminate:=Thread_done_proc;
     //FreeOnTerminate := True;
     Inherited Create(False);
End;

procedure TCDToImageThread.Execute;
begin
     Pre_gap_len:=150;
     Post_gap_len:=150;

     Form1.SCSI.MMC1_any_link.Do_set_CD_speed_CDB12(read_data_speed, MMC_SET_CD_SPEED_MAX);

     If f_toc=nil Then
     Begin
          //Create single *.DAO file..
          ReadFullTOCToMem;
          tracks:=T_tracks.Create(TOC);
          Gen_TOC_image(f_main);
          Gen_pregap(f_main, 0);
          ReadCDToImage(f_main, f_subch, f_C2);
          Gen_LO_image(f_main);
     End
     Else
     Begin
          If f_LO<>nil Then
          Begin
               //Create *.LI, *.PG1 file..
               ReadFullTOCToMem;
               tracks:=T_tracks.Create(TOC);
               Gen_TOC_image(f_toc);
               Gen_pregap(f_pregap1, 0);
          End
          Else
          Begin
               ReadFullTOCToImage;
               tracks:=T_tracks.Create(TOC);
          End;

          //Create *.IMG, *.SUB file..
          ReadCDToImage(f_main, f_subch, f_C2);

          If f_LO<>nil Then
          Begin
               //Create *.LO file..
               Gen_LO_image(f_LO);
          End;
     End;

     //FreeMem(De_SubCh_buf);
     TOC.Free;
     tracks.Free;
end;

Procedure TCDToImageThread.ShowErrMsg;
Begin
     Form1.Form4.CDToImageProgressForm.ShowErrMsg(show_err_msg);
End;

Procedure TCDToImageThread.ReadCDToImage(in_main_file : TFileStream;
                                         in_subch_file : TFileStream;
                                         in_C2_file : TFileStream);
Var
   Sector_block_size     : Word;
   SubCh_block_size      : Byte;
   C2_block_size         : Word;
   Max_N_Sect_block_size : LongWord;
   Data_1block_size      : Word;
   N_Sect_blocks_to_read : LongWord;
   Total_bytes_read      : LongWord;
   i                     : LongInt;
   j                     : LongWord;
   k                     : LongWord;
   HSA_sector            : LongInt;
   stop_looping          : Boolean;
   SubCh_buf             : Ptr_byte;
   buf_offset            : Pointer;
   buf_offset2           : Pointer;
   s                     : String;
   Out_CD_cap_mech_st    : T_pub_CD_cap_mech_st;
Begin
     Sector_block_size:=Form1.SCSI.MMC1_any_link.Get_MMC1.GetBlockSizeFromFilterReadFormat(MMC_SECTORTYPE_ANY,
                                                                                           MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC);
     SubCh_block_size:=Form1.SCSI.MMC1_any_link.Get_MMC1.GetBlockSizeFromSubCh(SubChSelMode);
     C2_block_size:=Form1.SCSI.MMC1_any_link.Get_MMC1.GetBlockSizeFromC2ErrorField(MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC Or C2_error_field);
     Data_1block_size:=SubCh_block_size+Sector_block_size+C2_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     i:=StartSect;
     j:=N_Sect;

     If j>Max_N_Sect_block_size Then
         N_Sect_blocks_to_read:=Max_N_Sect_block_size
     Else
         N_Sect_blocks_to_read:=j;

     Total_bytes_read:=N_Sect_blocks_to_read*Data_1block_size;
     stop_looping:=False;
     Repeat
           s:=IntToStr(i) +
              '..' +
              IntToStr(i+N_Sect_blocks_to_read-1) +
              ',' +
              IntToStr(N_Sect_blocks_to_read);

           If Not Terminated Then
           Begin
                Form1.Form4.CDToImageProgressForm.Sect_reading_disp_Lbl.Caption:=s;
           End;

           Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
           If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
           Begin
                s:=IntToStr(Out_CD_cap_mech_st.CurrReadSpeed)+
                ' ('+
                IntToStr(Trunc(SimpleRoundTo(Out_CD_cap_mech_st.CurrReadSpeed / C_1X_KBYTES_CDSPEED, 0)))+
                'X)';

                If Not Terminated Then
                Begin
                     Form1.Form4.CDToImageProgressForm.LBL_curr_read_speed.Caption:=s;
                End;
           End;

           Form1.SCSI.SPC_any_link.Zero_data_buf;

           //Debug remove later please:
           FillMemory(Form1.SCSI.SPC_any_link.Get_data_buf, N_Sect_blocks_to_read*Data_1block_size, $FF);

           {
           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           If i>=404850 Then
               HSA_sector:=i-450000
           Else
               HSA_sector:=i;
           }
           HSA_sector:=i;
           Form1.SCSI.MMC1_any_link.Do_readCD_byFormat_CDB12(HSA_sector,
                                                             N_Sect_blocks_to_read,
                                                             FilterMode,
                                                             MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC Or C2_error_field,
                                                             SubChSelMode);

           If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK OR
              (is_TEB AND
              ((Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_MEDIUM_ERR) OR
               (Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_RECV_ERR))) Then
           Begin
                If in_subch_file=nil Then
                Begin
                     If f_toc=nil Then
                     Begin
                          If C2_error_field=MMC_READCD_C2_NONE Then
                          Begin
                               //- .DAO file only.
                               
                               If is_deint_subs Then
                               Begin
                                    k:=0;
                                    buf_offset:=Form1.SCSI.SPC_any_link.Get_data_buf;
                                    Repeat
                                          //Go to the start of de-interlaced subch data
                                          Inc(Ptr_byte(buf_offset), Sector_block_size);

                                          //<><><> Start of de-interlacing subchannel data <><><>
                                          //Copy interlaced subch data from dynamic buffer to a variable
                                          sub_int_PW_96:=T_sub_int_PW_96(buf_offset^);
                                          //De-interlace the subch data from variable, leaving result in dynamic buffer
                                          Form1.CommonCDSettings.Deint_subs(@sub_int_PW_96, buf_offset);

                                          //Go to the start of next sector
                                          Inc(Ptr_byte(buf_offset), SubCh_block_size);
                                          //<><><> End of de-interlacing subchannel data <><><>

                                          k:=k+1;
                                    Until(k=N_Sect_blocks_to_read);
                               End;

                               in_main_file.WriteBuffer(Form1.SCSI.SPC_any_link.Get_data_buf^, N_Sect_blocks_to_read*Data_1block_size);
                          End
                          Else
                          Begin
                               //- .DAO file.
                               //- .C2/.C2+ error reporting file.

                               If is_deint_subs Then
                               Begin
                                    k:=0;
                                    buf_offset:=Form1.SCSI.SPC_any_link.Get_data_buf;
                                    Repeat
                                          //Start of the sector data.
                                          buf_offset2:=buf_offset;

                                          //Write out the main sector of 2352 bytes.
                                          in_main_file.WriteBuffer(buf_offset^, Sector_block_size);

                                          //Go to the end of main sector data.
                                          Inc(Ptr_byte(buf_offset), Sector_block_size);

                                          //If main+c2+sub order.
                                          If C2_read_order=0 Then
                                          Begin
                                               //Write out the 294 or 296 bytes of C2 error pointer bits/block error bits.
                                               in_C2_file.WriteBuffer(buf_offset^, C2_block_size);

                                               //Go to the end of C2 error data.
                                               Inc(Ptr_byte(buf_offset), C2_block_size);
                                          End;

                                          //<><><> Start of de-interlacing subchannel data <><><>
                                          //Copy interlaced sub-ch data from dynamic buffer to a variable
                                          sub_int_PW_96:=T_sub_int_PW_96(buf_offset^);
                                          //De-interlace the subch data from variable, leaving result in dynamic buffer
                                          Form1.CommonCDSettings.Deint_subs(@sub_int_PW_96, buf_offset);

                                          //Write out the sub-ch code of 96 bytes.
                                          in_main_file.WriteBuffer(buf_offset^, SubCh_block_size);

                                          //Go to the end of sub-ch data.
                                          Inc(Ptr_byte(buf_offset), SubCh_block_size);
                                          //<><><> End of de-interlacing subchannel data <><><>

                                          //If main+sub+c2 order.
                                          If C2_read_order=1 Then
                                          Begin
                                               //Write out the 294 or 296 bytes of C2 error pointer bits/block error bits.
                                               in_C2_file.WriteBuffer(buf_offset^, C2_block_size);

                                               //Go to the end of C2 error data.
                                               Inc(Ptr_byte(buf_offset), C2_block_size);
                                          End;

                                          k:=k+1;
                                    Until(k=N_Sect_blocks_to_read);
                               End
                               Else
                               Begin
                                    k:=0;
                                    buf_offset:=Form1.SCSI.SPC_any_link.Get_data_buf;
                                    Repeat
                                          //Start of the sector data.
                                          buf_offset2:=buf_offset;

                                          //Write out the main sector of 2352 bytes.
                                          in_main_file.WriteBuffer(buf_offset^, Sector_block_size);

                                          //Go to the end of main sector data.
                                          Inc(Ptr_byte(buf_offset), Sector_block_size);

                                          //If main+c2+sub order.
                                          If C2_read_order=0 Then
                                          Begin
                                               //Write out the 294 or 296 bytes of C2 error pointer bits/block error bits.
                                               in_C2_file.WriteBuffer(buf_offset^, C2_block_size);

                                               //Go to the start of next sector
                                               Inc(Ptr_byte(buf_offset), C2_block_size);
                                          End;

                                          //Write out the subch code of 96 bytes.
                                          in_main_file.WriteBuffer(buf_offset^, SubCh_block_size);

                                          //Go to the end of sub-ch data.
                                          Inc(Ptr_byte(buf_offset), SubCh_block_size);

                                          //If main+sub+c2 order.
                                          If C2_read_order=1 Then
                                          Begin
                                               //Write out the 294 or 296 bytes of C2 error pointer bits/block error bits.
                                               in_C2_file.WriteBuffer(buf_offset^, C2_block_size);

                                               //Go to the start of next sector
                                               Inc(Ptr_byte(buf_offset), C2_block_size);
                                          End;

                                          k:=k+1;
                                    Until(k=N_Sect_blocks_to_read);
                               End;
                          End;
                     End
                     Else
                     Begin
                          If C2_error_field=MMC_READCD_C2_NONE Then
                          Begin
                               //- .IMG file only.

                               in_main_file.WriteBuffer(Form1.SCSI.SPC_any_link.Get_data_buf^, N_Sect_blocks_to_read*Sector_block_size);
                          End
                          Else
                          Begin
                               //- .IMG file only.
                               //- .C2/.C2+ error reporting file.

                               k:=0;
                               buf_offset:=Form1.SCSI.SPC_any_link.Get_data_buf;
                               Repeat
                                     //Write out the main sector of 2352 bytes.
                                     in_main_file.WriteBuffer(buf_offset^, Sector_block_size);

                                     //Go to the start of C2 data.
                                     Inc(Ptr_byte(buf_offset), Sector_block_size);

                                     //Write out the 294 or 296 bytes of C2 error pointer bits/block error bits.
                                     in_C2_file.WriteBuffer(buf_offset^, C2_block_size);

                                     //Go to the start of next sector
                                     Inc(Ptr_byte(buf_offset), C2_block_size);

                                     k:=k+1;
                               Until(k=N_Sect_blocks_to_read);
                          End;
                     End;
                End
                Else
                Begin
                     If C2_error_field=MMC_READCD_C2_NONE Then
                     Begin
                          //- .IMG file.
                          //- .SUB file.

                          k:=0;
                          buf_offset:=Form1.SCSI.SPC_any_link.Get_data_buf;
                          Repeat
                                //Write out the main sector data.
                                in_main_file.WriteBuffer(buf_offset^, Sector_block_size);

                                //Go to the start of de-interlaced subch data
                                Inc(Ptr_byte(buf_offset), Sector_block_size);

                                //<><><> Start of de-interlacing subchannel data <><><>
                                If is_deint_subs Then
                                Begin
                                     //De-interlace the subch data from dynamic buffer, leaving result in variable
                                     Form1.CommonCDSettings.Deint_subs(buf_offset, @subch_deint_PW_96);
                                     SubCh_buf:=@subch_deint_PW_96;
                                End
                                Else
                                Begin
                                     SubCh_buf:=buf_offset;
                                End;

                                //Write out the subch data.
                                in_subch_file.WriteBuffer(SubCh_buf^, SubCh_block_size);

                                //Go to the end of subch data.
                                Inc(Ptr_byte(buf_offset), SubCh_block_size);
                                //<><><> End of de-interlacing subchannel data <><><>

                                k:=k+1;
                          Until(k=N_Sect_blocks_to_read);
                     End
                     Else
                     Begin
                          //- .IMG file.
                          //- .SUB file.
                          //- .C2/.C2+ error reporting file.

                          k:=0;
                          buf_offset:=Form1.SCSI.SPC_any_link.Get_data_buf;
                          Repeat
                                //Write out main sector.
                                in_main_file.WriteBuffer(buf_offset^, Sector_block_size);

                                //Go to the end of main sector.
                                Inc(Ptr_byte(buf_offset), Sector_block_size);

                                //If main+c2+sub order.
                                If C2_read_order=0 Then
                                Begin
                                     //Write out the 294 or 296 bytes of C2 error pointer bits/block error bits.
                                     in_C2_file.WriteBuffer(buf_offset^, C2_block_size);

                                     //Go to the end of C2 error data.
                                     Inc(Ptr_byte(buf_offset), C2_block_size);
                                End;

                                //<><><> Start of de-interlacing subchannel data <><><>
                                If is_deint_subs Then
                                Begin
                                     //De-interlace the subch data from dynamic buffer, leaving result in variable
                                     Form1.CommonCDSettings.Deint_subs(buf_offset, @subch_deint_PW_96);
                                     SubCh_buf:=@subch_deint_PW_96;
                                End
                                Else
                                Begin
                                     SubCh_buf:=buf_offset;
                                End;

                                //Write out subs.
                                in_subch_file.WriteBuffer(SubCh_buf^, SubCh_block_size);

                                //Go to the end of sub-ch data.
                                Inc(Ptr_byte(buf_offset), SubCh_block_size);
                                //<><><> End of de-interlacing subchannel data <><><>


                                //If main+sub+c2 order.
                                If C2_read_order=1 Then
                                Begin
                                     //Write out the 294 or 296 bytes of C2 error pointer bits/block error bits.
                                     in_C2_file.WriteBuffer(buf_offset^, C2_block_size);

                                     //Go to the end of C2 error data.
                                     Inc(Ptr_byte(buf_offset), C2_block_size);
                                End;

                                k:=k+1;
                          Until(k=N_Sect_blocks_to_read);
                     End;
                End;
           End
           Else
           Begin
                show_err_msg:='Error while processing ReadCD command.' + Chr(10) + Chr(13);
                show_err_msg:=show_err_msg + Form1.SCSI.MMC1_any_link.Get_err_msg;
                Synchronize(ShowErrMsg);
                stop_looping:=True;
           End;
           i:=i+N_Sect_blocks_to_read;
           j:=j-N_Sect_blocks_to_read;

           If j<N_Sect_blocks_to_read Then N_Sect_blocks_to_read:=j;
           Total_bytes_read:=N_Sect_blocks_to_read*Data_1block_size;
     Until (j=0) OR (stop_looping) OR Terminated; 

     curr_abs_LBA_to_write:=i;
     LeadOutSect:=i;
End;

procedure TCDToImageThread.FillA0SubChEntry(First_trkNo : Byte;
                                            Ctrl        : Byte);
var
   First_trk_MSF : T_MSF;
   Track_format  : Byte;
begin
     subch_deint_PW_96.PW[2][3]:=$A0;
     subch_deint_PW_96.PW[2][8]:=First_trkNo; //PMin
     //Check the Ctrl to see if it's a data (CDROM) or an audio (CDDA) CD.
     If (Ctrl And
         MMC_READ_SUBCH_CTRL_MASK_DATA)=MMC_READ_SUBCH_CTRL_DATA Then
     Begin
          //Disk is a data (CDROM) CD..

          //First trk start location is always 00:02:00.
          First_trk_MSF.M:=0;
          First_trk_MSF.S:=2;
          First_trk_MSF.F:=0;

          //Check the exact format of the first track.
          Track_format:=Scan_format_of_data_track_from_CD(First_trk_MSF);
          Case Track_format Of
          CDROM_SECTORTYPE_ANY:
               subch_deint_PW_96.PW[2][9]:=0;
          CDROM_SECTORTYPE_MODE1:
               subch_deint_PW_96.PW[2][9]:=0;
          CDROM_SECTORTYPE_MODE2FORMLESS:
               subch_deint_PW_96.PW[2][9]:=$20;
          CDROM_SECTORTYPE_MODE2FORM1:
               subch_deint_PW_96.PW[2][9]:=$20;
          CDROM_SECTORTYPE_MODE2FORM2:
               subch_deint_PW_96.PW[2][9]:=$20;
          CDROM_SECTORTYPE_MODE0:
               subch_deint_PW_96.PW[2][9]:=0;
          CDROM_SECTORTYPE_UNKNOWN:
               subch_deint_PW_96.PW[2][9]:=0;
          End;
     End
     Else
     Begin
          //Disk is an audio (CDDA) CD..

          subch_deint_PW_96.PW[2][9]:=0;
     End;
end;

procedure TCDToImageThread.AddA0TOCEntry(First_trkNo : Byte;
                                         Ctrl        : Byte);
var
   CRC16_val : Word;
begin
     ZeroMemory(@subch_deint_PW_96, 96);

     AddA0TOCEntry(First_trkNo, Ctrl);

     //Calc CRC (X25 standard CRC 16). The 2 bytes of CRC for Q sub-channel data.
     CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
     subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
     subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

     TOC.Add_entry(subch_deint_PW_96);
end;

procedure TCDToImageThread.AddA1TOCEntry(Last_trkNo : Byte);
var
   CRC16_val : Word;
begin
     ZeroMemory(@subch_deint_PW_96, 96);

     subch_deint_PW_96.PW[2][3]:=$A1;
     subch_deint_PW_96.PW[2][8]:=Last_trkNo; //PMin

     //Calc CRC (X25 standard CRC 16). The 2 bytes of CRC for Q sub-channel data.
     CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
     subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
     subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

     TOC.Add_entry(subch_deint_PW_96);
end;

function TCDToImageThread.Read1TOCEntryFromCD(Read_first      : Boolean;
                                              Var First_trkNo : Byte;
                                              Var Last_trkNo  : Byte;
                                              Var Ctrl        : Byte) : Boolean;
Var
   TOC_data         : T_out_read_T_P_A_TOC_desc_MSF;
   TOC_entry_exists : Boolean;
   CRC16_val        : Word;
Begin
     If Read_first Then
     Begin
          TOC_entry_exists:=
          Form1.SCSI.MMC1_any_link.Do_read_T_P_A_TOC_MSF_first_out_CDB10(First_trkNo,
                                                                         Last_trkNo,
                                                                         TOC_data,
                                                                         0);
          Ctrl:=TOC_data.ADR_Ctrl And $0F;
          ZeroMemory(@subch_deint_PW_96, 96);
     End
     Else
     Begin
          TOC_entry_exists:=
          Form1.SCSI.MMC1_any_link.Do_read_T_P_A_TOC_MSF_next_out_CDB10(TOC_data);
     End;

     If TOC_entry_exists Then
     Begin
          //Place Ctrl at upper 4 bits
          subch_deint_PW_96.PW[2][1]:=TOC_data.ADR_Ctrl SHL 4;
          //Place ADR at lower 4 bits
          subch_deint_PW_96.PW[2][1]:=subch_deint_PW_96.PW[2][1] OR (TOC_data.ADR_Ctrl SHR 4);
          //subch_deint_PW_96.PW[2][2]:=0; //TNO
          Case TOC_data.TrackNo Of
          1..99:
          Begin
               subch_deint_PW_96.PW[2][3]:=TOC_data.TrackNo;
               subch_deint_PW_96.PW[2][8]:=TOC_data.StartM; //PMin
               subch_deint_PW_96.PW[2][9]:=TOC_data.StartS; //PSec
               subch_deint_PW_96.PW[2][10]:=TOC_data.StartF; //PFrame
          End;
          $A0:
          Begin
               FillA0SubChEntry(First_trkNo, TOC_data.ADR_Ctrl And $0F);
          End;
          $A1:
          Begin
               subch_deint_PW_96.PW[2][3]:=$A1;
               subch_deint_PW_96.PW[2][8]:=Last_trkNo; //PMin
          End;
          $AA:
          Begin
               subch_deint_PW_96.PW[2][3]:=$A2;
               subch_deint_PW_96.PW[2][8]:=TOC_data.StartM; //PMin
               subch_deint_PW_96.PW[2][9]:=TOC_data.StartS; //PSec
               subch_deint_PW_96.PW[2][10]:=TOC_data.StartF; //PFrame
          End;
          Else
          Begin
               subch_deint_PW_96.PW[2][3]:=TOC_data.TrackNo;
          End;
          End;

          //Calc CRC (X25 standard CRC 16). The 2 bytes of CRC for Q sub-channel data.
          CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
          subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
          subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

          TOC.Add_entry(subch_deint_PW_96);
     End;

     Read1TOCEntryFromCD:=TOC_entry_exists;
End;

procedure TCDToImageThread.ReadTOCToMem;
Var First_trkNo : Byte;
    Last_trkNo  : Byte;
    Ctrl        : Byte;
Begin
     Read1TOCEntryFromCD(True, First_trkNo, Last_trkNo, Ctrl);
     Repeat
     Until (Read1TOCEntryFromCD(False, First_trkNo, Last_trkNo, Ctrl)=False);

     If Not TOC.Find_mode_point_entry(1, $A0) Then
     Begin
          AddA0TOCEntry(First_trkNo, Ctrl);
     End;

     If Not TOC.Find_mode_point_entry(1, $A1) Then
     Begin
          AddA1TOCEntry(Last_trkNo);
     End;
End;

procedure TCDToImageThread.ReadTOCToImage;
var
   First_trkNo : Byte;
   Last_trkNo  : Byte;
   Ctrl        : Byte;
Begin
     Read1TOCEntryFromCD(True, First_trkNo, Last_trkNo, Ctrl);
     Repeat
           If is_deint_subs Then
           Begin
                //Write to TOC file
                f_toc.WriteBuffer(subch_deint_PW_96, 96);
           End
           Else
           Begin
                Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, @sub_int_PW_96);
                //Write to TOC file
                f_toc.WriteBuffer(sub_int_PW_96, 96);
           End;
     Until Read1TOCEntryFromCD(False, First_trkNo, Last_trkNo, Ctrl)=False;

     //Check if ADR(Q mode)=1 and point=$A0 was returned by ReadTOC command.
     If Not TOC.Find_mode_point_entry(1, $A0) Then
     Begin
          //Not returned, so generate our own.
          AddA0TOCEntry(First_trkNo, Ctrl);

          If is_deint_subs Then
          Begin
               //Write to TOC file
               f_toc.WriteBuffer(subch_deint_PW_96, 96);
          End
          Else
          Begin
               Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, @sub_int_PW_96);
               //Write to TOC file
               f_toc.WriteBuffer(sub_int_PW_96, 96);
          End;
     End;

     //Check if ADR(Q mode)=1 and point=$A1 was returned by ReadTOC command.
     If Not TOC.Find_mode_point_entry(1, $A1) Then
     Begin
          //Not returned, so generate our own.
          AddA1TOCEntry(Last_trkNo);

          If is_deint_subs Then
          Begin
               //Write to TOC file
               f_toc.WriteBuffer(subch_deint_PW_96, 96);
          End
          Else
          Begin
               Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, @sub_int_PW_96);
               //Write to TOC file
               f_toc.WriteBuffer(sub_int_PW_96, 96);
          End;
     End;
End;

function TCDToImageThread.Read1FullTOCEntryFromCD(Read_first : Boolean) : Boolean;
Var
   First_sessNo      : Byte;
   Last_sessNo       : Byte;
   TOC_data          : T_out_read_T_P_A_full_TOC_desc;
   TOC_entry_exists  : Boolean;
   CRC16_val         : Word;
Begin
     If Read_first Then
     Begin
          TOC_entry_exists:=
          Form1.SCSI.MMC1_any_link.Do_read_T_P_A_full_TOC_first_out_CDB10(First_sessNo,
                                                                          Last_sessNo,
                                                                          TOC_data,
                                                                          0);
          ZeroMemory(@subch_deint_PW_96, 96);
     End
     Else
     Begin
          TOC_entry_exists:=
          Form1.SCSI.MMC1_any_link.Do_read_T_P_A_full_TOC_next_out_CDB10(TOC_data);
     End;

     If TOC_entry_exists Then
     Begin
          subch_deint_PW_96.PW[2][1]:=TOC_data.ADR_Ctrl SHL 4;
          subch_deint_PW_96.PW[2][1]:=subch_deint_PW_96.PW[2][1] OR (TOC_data.ADR_Ctrl SHR 4);
          subch_deint_PW_96.PW[2][2]:=TOC_data.TNO;
          subch_deint_PW_96.PW[2][3]:=TOC_data.POINT;
          subch_deint_PW_96.PW[2][4]:=TOC_data.Min;
          subch_deint_PW_96.PW[2][5]:=TOC_data.Sec;
          subch_deint_PW_96.PW[2][6]:=TOC_data.Frame;
          subch_deint_PW_96.PW[2][7]:=TOC_data.Zero;
          subch_deint_PW_96.PW[2][8]:=TOC_data.PMin;
          subch_deint_PW_96.PW[2][9]:=TOC_data.PSec;
          subch_deint_PW_96.PW[2][10]:=TOC_data.PFrame;

          //According to MMC1, bytes 2 to 10 are converted into hex by device for values 0 to 99bcd
          //encountered on the media. This means that some drives will return hex values, but
          //we want to always deal with decimal values. Below is a conversion from hex into decimal.
          //Treat any values between 10 to 99 as hex representation, converting hex to decimal (base 10)
          {For i:=2 To 10 Do
          Begin
               If (subch_deint_PW_96.PW[2][i]>=10) AND (subch_deint_PW_96.PW[2][i]<=99) Then
               Begin
                    subch_deint_PW_96.PW[2][i]:=StrToInt('$'+IntToStr(subch_deint_PW_96.PW[2][i]));
               End;
          End;}

          //Calc CRC (X25 standard CRC 16). The 2 bytes of CRC for Q sub-channel data.
          CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
          subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
          subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;

          TOC.Add_entry(subch_deint_PW_96);
     End;

     Read1FullTOCEntryFromCD:=TOC_entry_exists;
End;

procedure TCDToImageThread.ReadFullTOCToMem;
Begin
     Read1FullTOCEntryFromCD(True);
     Repeat
     Until (Read1FullTOCEntryFromCD(False)=False);
End;

procedure TCDToImageThread.ReadFullTOCToImage;
Begin
     Read1FullTOCEntryFromCD(True);
     Repeat
           If is_deint_subs Then
           Begin
                //Write to TOC file
                f_toc.WriteBuffer(subch_deint_PW_96, 96);
           End
           Else
           Begin
                Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, @sub_int_PW_96);
                //Write to TOC file
                f_toc.WriteBuffer(sub_int_PW_96, 96);
           End;
     Until Read1FullTOCEntryFromCD(False)=False;
End;

Procedure TCDToImageThread.Gen_TOC_image(in_toc_file : TFileStream);
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
   //subch_deint_PW_96           : T_subch_deint_PW_96;
   SubCh_buf             : Pointer;
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
   data_mode             : Byte;
   s                     : String;
Begin
     Sector_block_size:=2352;
     SubCh_block_size:=96;
     Data_1block_size:=SubCh_block_size+Sector_block_size;

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     curr_abs_LBA_to_write:=-lead_in_size-150;

     N_Sect_blocks_to_write:=1;
     j:=Lead_in_size;

     {Lead_in_MSF_time.M:=0;
     Lead_in_MSF_time.S:=0;
     Lead_in_MSF_time.F:=0;}

     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                                  Lead_in_MSF_time.M,
                                                  Lead_in_MSF_time.S,
                                                  Lead_in_MSF_time.F);

     Form1.SCSI.SPC_any_link.Zero_data_buf;

     {Sector_MSF_time.M:=0;
     Sector_MSF_time.S:=0;
     Sector_MSF_time.F:=0;}

     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                                  Sector_MSF_time.M,
                                                  Sector_MSF_time.S,
                                                  Sector_MSF_time.F);

     Case lead_in_type Of
     U_TRACK_MODE_AUDIO:
     Begin
          is_data_track:=False;
     End;
     U_TRACK_MODE_CDROM:
     Begin
          is_data_track:=True;
     End;
     U_TRACK_MODE_USE_TOC:
     Begin
          If TOC.Find_mode_point_entry(out_TOC_entry, 1, 1) Then
          Begin
               Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
                    $00,$10,$80,$90: is_data_track:=False;
                    $40,$50: is_data_track:=True;
               Else
                   is_data_track:=False;
               End;
          End;
     End;
     End;

     TOC_curr_index:=0;

     If is_data_track Then
     Begin
          If tracks.Find_track_no_entry_by_track_no(out_track_entry, 1) Then
          Begin
               //Fill_sync_block(Form1.SCSI.SPC_any_link.Get_data_buf, CD_mode_to_data_mode(out_track_entry.CD_mode), True, Sector_MSF_time);

               Sync_header[1]:=0;
               For i:=2 To 11 Do
               Begin
                    Sync_header[i]:=$FF;
               End;
               Sync_header[12]:=0;
               //Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M))+$A0;
               Sync_header[13]:=StrToInt('$'+'A'+Copy(IntToStr(Sector_MSF_time.M),2,1));
               Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
               Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
               Sync_header[16]:=CD_mode_to_data_mode(out_track_entry.CD_mode);
               T_array16_byte(Form1.SCSI.SPC_any_link.Get_data_buf^):=Sync_header;
          End;
     End;

     SubCh_buf:=Ptr(Integer(Form1.SCSI.SPC_any_link.Get_data_buf) + Sector_block_size);
     //SubCh_buf:=Form1.SCSI.SPC_any_link.Get_data_buf;
     //Inc(SubCh_buf, Sector_block_size);

     TOC_mod3:=3;
     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:='Generating TOC: '+
                IntToStr(curr_abs_LBA_to_write) + '..' +
                IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1) + ',' +
                IntToStr(N_Sect_blocks_to_write);

                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                If Not Terminated Then
                Begin
                     Form1.Form4.CDToImageProgressForm.Sect_reading_disp_Lbl.Caption:=s;
                End;
           End;

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

           subch_deint_PW_96.PW[2][4]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.M));
           subch_deint_PW_96.PW[2][5]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.S));
           subch_deint_PW_96.PW[2][6]:=StrToInt('$'+IntToStr(Lead_in_MSF_time.F));
           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
           subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
           subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;
           //Interlace sub-channels
           //Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, @(SubCh_buf^));
           T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;

           If is_data_track Then
           Begin
                Fill_EDCECC(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);

                Fill_P(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);
                Fill_Q(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);

                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(Form1.SCSI.SPC_any_link.Get_data_buf);

                //Scramble(Form1.SCSI.SPC_any_link.Get_data_buf);
           End;

           in_toc_file.WriteBuffer(Form1.SCSI.SPC_any_link.Get_data_buf^, Data_1block_size);

           p:=Form1.SCSI.SPC_any_link.Get_data_buf;
           Inc(p, 12);
           ZeroMemory(p, 2340);

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           //MMCLBA_sector:=MMCLBA_sector+N_Sect_blocks_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If is_data_track Then
           Begin
                If Sector_MSF_time.F=74 Then
                Begin
                     If Sector_MSF_time.S=59 Then
                     Begin
                          If Sector_MSF_time.M=99 Then
                          Begin
                               Sector_MSF_time.M:=0;
                               Sector_MSF_time.S:=0;
                               Sector_MSF_time.F:=0;
                          End
                          Else
                          Begin
                               Sector_MSF_time.M:=Sector_MSF_time.M+1;
                               Sector_MSF_time.S:=0;
                               Sector_MSF_time.F:=0;
                          End;
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
                //Sync_header[13]:=StrToInt('$'+IntToStr(Sector_MSF_time.M))+$A0;
                Sync_header[13]:=StrToInt('$'+'A'+Copy(IntToStr(Sector_MSF_time.M),2,1));
                Sync_header[14]:=StrToInt('$'+IntToStr(Sector_MSF_time.S));
                Sync_header[15]:=StrToInt('$'+IntToStr(Sector_MSF_time.F));
                T_array16_byte(Form1.SCSI.SPC_any_link.Get_data_buf^):=Sync_header;
           End;

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
     Until (j=0) OR (stop_looping) OR Terminated;

     //ZeroMemory(@subch_deint_PW_96, 96);

     //subch_deint_PW_96.PW[2][2]:=subch_deint_PW_96.PW[2][2]+1;
     //subch_deint_PW_96.PW[2][3]:=subch_deint_PW_96.PW[2][3]+1;
End;

Procedure TCDToImageThread.Gen_pregap(in_pregap1_file : TFileStream;
                                      in_preceding_trackno : Byte);
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

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=Pre_gap_len;

     Form1.SCSI.SPC_any_link.Zero_data_buf;

     SubCh_buf:=Ptr(Integer(Form1.SCSI.SPC_any_link.Get_data_buf) + Sector_block_size);
     For i:=1 To 12 Do
     Begin
          subch_deint_PW_96.PW[1][i]:=$FF;
     End;
     sub_q_rel_MSF_time.M:=0;
     sub_q_rel_MSF_time.S:=2;
     sub_q_rel_MSF_time.F:=0;

     //Track no. for this pregap. For 1st pregap it should be 1 since lead-in is 0.
     subch_deint_PW_96.PW[2][2]:=in_preceding_trackno+1;

     If TOC.Find_mode_point_entry(out_TOC_entry, 1, subch_deint_PW_96.PW[2][2]) Then
     Begin
          Case (T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR AND $D0) Of
               $00,$10,$80,$90: is_data_track:=False;
               $40,$50: is_data_track:=True;
          End;
     End;

     subch_deint_PW_96.PW[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     //subch_deint_PW_96.PW[2][2]:=0;
     subch_deint_PW_96.PW[2][3]:=0;
     subch_deint_PW_96.PW[2][4]:=sub_q_rel_MSF_time.M;
     subch_deint_PW_96.PW[2][5]:=sub_q_rel_MSF_time.S;
     subch_deint_PW_96.PW[2][6]:=sub_q_rel_MSF_time.F;
     subch_deint_PW_96.PW[2][7]:=0;
     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(curr_abs_LBA_to_write,
                                                  sub_q_abs_MSF_time.M,
                                                  sub_q_abs_MSF_time.S,
                                                  sub_q_abs_MSF_time.F);

     subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
     subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
     subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
     subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
     subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;
     //Interlace sub-channels
     //Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, SubCh_buf);
     T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;

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

          If tracks.Find_track_no_entry_by_LBA(out_track_entry, curr_abs_LBA_to_write+Pre_gap_len) Then
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
               T_array16_byte(Form1.SCSI.SPC_any_link.Get_data_buf^):=Sync_header;
          End;
     End;

     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:='Generating 1st pregap: '+
                IntToStr(curr_abs_LBA_to_write) + '..' +
                IntToStr(curr_abs_LBA_to_write+N_Sect_blocks_to_write-1) + ',' +
                IntToStr(N_Sect_blocks_to_write);

                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                Form1.Form4.CDToImageProgressForm.Sect_reading_disp_Lbl.Caption:=s;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=curr_abs_LBA_to_write;}

           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(Form1.SCSI.SPC_any_link.Get_data_buf);

                Fill_EDCECC(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);

                Fill_P(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);
                Fill_Q(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);

                //Scramble(Form1.SCSI.SPC_any_link.Get_data_buf);
           End;

           in_pregap1_file.WriteBuffer(Form1.SCSI.SPC_any_link.Get_data_buf^, Data_1block_size);

           p:=Form1.SCSI.SPC_any_link.Get_data_buf;
           Inc(p, 12);
           ZeroMemory(p, 2340);

           curr_abs_LBA_to_write:=curr_abs_LBA_to_write+N_Sect_blocks_to_write;
           MMCLBA_sector:=MMCLBA_sector+N_Sect_blocks_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           If is_data_track Then
           Begin
                If Sector_MSF_time.F=74 Then
                Begin
                     If Sector_MSF_time.S=59 Then
                     Begin
                          If Sector_MSF_time.M=99 Then
                          Begin
                               Sector_MSF_time.M:=0;
                               Sector_MSF_time.S:=0;
                               Sector_MSF_time.F:=0;
                          End
                          Else
                          Begin
                               Sector_MSF_time.M:=Sector_MSF_time.M+1;
                               Sector_MSF_time.S:=0;
                               Sector_MSF_time.F:=0;
                          End;
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
                T_array16_byte(Form1.SCSI.SPC_any_link.Get_data_buf^):=Sync_header;
           End;

           //Calc q-channel relative MSF address
           If sub_q_rel_MSF_time.F=0 Then
           Begin
                If sub_q_rel_MSF_time.S=0 Then
                Begin
                     If sub_q_rel_MSF_time.M=0 Then
                     Begin
                          sub_q_rel_MSF_time.M:=99;
                          sub_q_rel_MSF_time.S:=59;
                          sub_q_rel_MSF_time.F:=74;
                     End
                     Else
                     Begin
                          sub_q_rel_MSF_time.M:=sub_q_rel_MSF_time.M-1;
                          sub_q_rel_MSF_time.S:=59;
                          sub_q_rel_MSF_time.F:=74;
                     End;
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
           subch_deint_PW_96.PW[2][5]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.S));
           subch_deint_PW_96.PW[2][6]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.F));

           //Calc q-channel absolute MSF address
           If sub_q_abs_MSF_time.F=74 Then
           Begin
                If sub_q_abs_MSF_time.S=59 Then
                Begin
                     If sub_q_abs_MSF_time.M=99 Then
                     Begin
                          sub_q_abs_MSF_time.M:=0;
                          sub_q_abs_MSF_time.S:=0;
                          sub_q_abs_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          sub_q_abs_MSF_time.M:=sub_q_abs_MSF_time.M+1;
                          sub_q_abs_MSF_time.S:=0;
                          sub_q_abs_MSF_time.F:=0;
                     End;
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
           subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
           subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
           subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
           subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
           subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;
           //Interlace sub-channels
           //Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, SubCh_buf);
           T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;
     Until (j=0) OR (stop_looping) OR Terminated;

     //subch_deint_PW_96.PW[2][3]:=subch_deint_PW_96.PW[2][3]+1;
End;

Procedure TCDToImageThread.Gen_LO_image(in_LO_file : TFileStream);
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
   subch_deint_PW_96           : T_subch_deint_PW_96;
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

     Max_N_Sect_block_size:=Form1.SCSI.SPC_any_link.Get_data_buf_size DIV Data_1block_size;

     N_Sect_blocks_to_write:=1;
     j:=lead_out_size;

     Form1.SCSI.SPC_any_link.Zero_data_buf;

     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(LeadOutSect,
                                                  Sector_MSF_time.M,
                                                  Sector_MSF_time.S,
                                                  Sector_MSF_time.F);

     SubCh_buf:=Ptr(Integer(Form1.SCSI.SPC_any_link.Get_data_buf) + Sector_block_size);
     sub_p_2Hz_counter:=0;
     sub_p_2Hz_BIT_val:=$00;
     For i:=1 To 12 Do
     Begin
          subch_deint_PW_96.PW[1][i]:=sub_p_2Hz_BIT_val;
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

     subch_deint_PW_96.PW[2][1]:=T_TOC_data_PQ((@out_TOC_entry)^).Q.Control_ADR;
     subch_deint_PW_96.PW[2][2]:=$AA;
     subch_deint_PW_96.PW[2][3]:=1;
     subch_deint_PW_96.PW[2][4]:=sub_q_rel_MSF_time.M;
     subch_deint_PW_96.PW[2][5]:=sub_q_rel_MSF_time.S;
     subch_deint_PW_96.PW[2][6]:=sub_q_rel_MSF_time.F;
     subch_deint_PW_96.PW[2][7]:=0;
     Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(LeadOutSect,
                                                  sub_q_abs_MSF_time.M,
                                                  sub_q_abs_MSF_time.S,
                                                  sub_q_abs_MSF_time.F);

     subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
     subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
     subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

     //Calc CRC (X25 standard CRC 16)
     CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
     subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
     subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;
     //Interlace sub-channels
     //Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, SubCh_buf);
     T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;

     If is_data_track Then
     Begin
          If tracks.Find_track_no_entry_by_LBA(out_track_entry, LeadOutSect-1) Then
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
               T_array16_byte(Form1.SCSI.SPC_any_link.Get_data_buf^):=Sync_header;
          End;
     End;

     stop_looping:=False;
     Repeat
           If Not Terminated Then
           Begin
                s:='Generating lead-out: '+
                   IntToStr(LeadOutSect) + '..' +
                   IntToStr(LeadOutSect+N_Sect_blocks_to_write-1) + ',' +
                   IntToStr(N_Sect_blocks_to_write);
                If is_data_track Then
                    s:=s+' (data track).'
                Else
                    s:=s+' (audio track).';

                If Not Terminated Then
                Begin
                     Form1.Form4.CDToImageProgressForm.Sect_reading_disp_Lbl.Caption:=s;
                End;
           End;

           //Convert positive LBA sector to MMCLBA (MMC style LBA) sector
           {If curr_abs_LBA_to_write>=404850 Then
               MMCLBA_sector:=curr_abs_LBA_to_write-450000
           Else
               MMCLBA_sector:=curr_abs_LBA_to_write;}

           If is_data_track Then
           Begin
                If Out_track_entry.CD_mode=CDROM_SECTORTYPE_MODE2FORM2 Then
                   Fill_Mode2Form2_SubHdr_20h(Form1.SCSI.SPC_any_link.Get_data_buf);

                Fill_EDCECC(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);

                Fill_P(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);
                Fill_Q(Form1.SCSI.SPC_any_link.Get_data_buf, Out_track_entry.CD_mode);

                //Scramble(Form1.SCSI.SPC_any_link.Get_data_buf);
           End;

           in_LO_file.WriteBuffer(Form1.SCSI.SPC_any_link.Get_data_buf^, Data_1block_size);

           p:=Form1.SCSI.SPC_any_link.Get_data_buf;
           Inc(p, 12);
           ZeroMemory(p, 2340);

           LeadOutSect:=LeadOutSect+N_Sect_blocks_to_write;
           //MMCLBA_sector:=MMCLBA_sector+N_Sect_blocks_to_write;
           j:=j-N_Sect_blocks_to_write;

           If j<N_Sect_blocks_to_write Then N_Sect_blocks_to_write:=j;

           //2 seconds (150 sectors) where lead-out, p-subchannel=0.  Then after 2Hz.
           If j<=lead_out_size-150 Then
           Begin
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
                     subch_deint_PW_96.PW[1][i]:=sub_p_2Hz_BIT_val;
                End;
           End;

           If is_data_track Then
           Begin
                If Sector_MSF_time.F=74 Then
                Begin
                     If Sector_MSF_time.S=59 Then
                     Begin
                          If Sector_MSF_time.M=99 Then
                          Begin
                               Sector_MSF_time.M:=0;
                               Sector_MSF_time.S:=0;
                               Sector_MSF_time.F:=0;
                          End
                          Else
                          Begin
                               Sector_MSF_time.M:=Sector_MSF_time.M+1;
                               Sector_MSF_time.S:=0;
                               Sector_MSF_time.F:=0;
                          End;
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
                T_array16_byte(Form1.SCSI.SPC_any_link.Get_data_buf^):=Sync_header;
           End;

           //Calc q-channel relative MSF address
           If sub_q_rel_MSF_time.F=74 Then
           Begin
                If sub_q_rel_MSF_time.S=59 Then
                Begin
                     If sub_q_rel_MSF_time.M=99 Then
                     Begin
                          sub_q_rel_MSF_time.M:=0;
                          sub_q_rel_MSF_time.S:=0;
                          sub_q_rel_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          sub_q_rel_MSF_time.M:=sub_q_rel_MSF_time.M+1;
                          sub_q_rel_MSF_time.S:=0;
                          sub_q_rel_MSF_time.F:=0;
                     End;
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
           subch_deint_PW_96.PW[2][4]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.M));
           subch_deint_PW_96.PW[2][5]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.S));
           subch_deint_PW_96.PW[2][6]:=StrToInt('$'+IntToStr(sub_q_rel_MSF_time.F));

           //Calc q-channel absolute MSF address
           If sub_q_abs_MSF_time.F=74 Then
           Begin
                If sub_q_abs_MSF_time.S=59 Then
                Begin
                     If sub_q_abs_MSF_time.M=99 Then
                     Begin
                          sub_q_abs_MSF_time.M:=0;
                          sub_q_abs_MSF_time.S:=0;
                          sub_q_abs_MSF_time.F:=0;
                     End
                     Else
                     Begin
                          sub_q_abs_MSF_time.M:=sub_q_abs_MSF_time.M+1;
                          sub_q_abs_MSF_time.S:=0;
                          sub_q_abs_MSF_time.F:=0;
                     End;
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
           subch_deint_PW_96.PW[2][8]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.M));
           subch_deint_PW_96.PW[2][9]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.S));
           subch_deint_PW_96.PW[2][10]:=StrToInt('$'+IntToStr(sub_q_abs_MSF_time.F));

           //Calc CRC (X25 standard CRC 16)
           CRC16_val:=Calc_CRC16(@(subch_deint_PW_96.PW[2][1]), 10);
           subch_deint_PW_96.PW[2][11]:=NOT (CRC16_val SHR 8);
           subch_deint_PW_96.PW[2][12]:=NOT CRC16_val;
           //Interlace sub-channels
           //Form1.CommonCDSettings.Int_subs(@subch_deint_PW_96, SubCh_buf);
           T_subch_deint_PW_96(SubCh_buf^):=subch_deint_PW_96;
     Until (j=0) OR (stop_looping) OR Terminated;
End;

end.
