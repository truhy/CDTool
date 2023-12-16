unit CDROM_struct_Unit;

interface

Uses Classes,
     SysUtils,
     SCSIUnit,
     MMC1Unit,
     WNASPI32Unit,
     EDC_ECC_Unit,
     CommonCDSettingsUnit;

{** CD sector type flags **}
Const CDROM_SECTORTYPE_ANY=0;
Const CDROM_SECTORTYPE_CDDA=1;
Const CDROM_SECTORTYPE_MODE1=2;
Const CDROM_SECTORTYPE_MODE2FORMLESS=3;
Const CDROM_SECTORTYPE_MODE2FORM1=4;
Const CDROM_SECTORTYPE_MODE2FORM2=5;
Const CDROM_SECTORTYPE_MODE0=6;
Const CDROM_SECTORTYPE_UNKNOWN=7;

{
Type T_MSF=Record
                 M : Byte;
                 S : Byte;
                 F : Byte;
           End;
}
//CDROM format types
Type T_CDROM_format_sync_field=Record
                                     Zero1 : Byte;
                                     FFh   : Array[1..10] Of Byte;
                                     Zero2 : Byte;
                               End;
Type T_CDROM_format_sync_hdr=Record
                                   Min        : Byte;
                                   Sec        : Byte;
                                   Frame      : Byte;
                                   Data_mode  : Byte;
                             End;
Type T_CDROM_format_sync_block=Record
                                     Sync_field : T_CDROM_format_sync_field;
                                     Hdr        : T_CDROM_format_sync_hdr;
                               End;
Type T_CDROM_format_mode1=Record
                                Sync_block : T_CDROM_format_sync_block;
                                User_data  : Array[1..2048] Of Byte;
                                CRC16      : Array[1..4] Of Byte;
                                Zero_fill  : Array[1..8] Of Byte;
                                P_parity   : Array[1..172] Of Byte;
                                Q_parity   : Array[1..104] Of Byte;
                          End;
Type T_CDROM_format_mode2_formless=Record
                                         Sync_block : T_CDROM_format_sync_block;
                                         User_data  : Array[1..2336] Of Byte;
                                   End;
Type T_CDROM_format_mode2_sub_hdr=Record
                                        File_no     : Byte;
                                        Channel_no  : Byte;
                                        Sub_mode    : Byte;
                                        Coding_info : Byte;
                                  End;
Type T_CDROM_format_mode2_sub_hdrs=Record
                                         Hdr1 : T_CDROM_format_mode2_sub_hdr;
                                         Hdr2 : T_CDROM_format_mode2_sub_hdr;
                                   End;
Type T_CDROM_format_sync_block_mode2_sub_hdrs=Record
                                                    Sync_block : T_CDROM_format_sync_block;
                                                    Sub_hdr    : T_CDROM_format_mode2_sub_hdrs;
                                              End;
Type T_CDROM_format_mode2_form1=Record
                                      Sync_block : T_CDROM_format_sync_block;
                                      Sub_hdr    : T_CDROM_format_mode2_sub_hdrs;
                                      User_data  : Array[1..2048] Of Byte;
                                      CRC16      : Array[1..4] Of Byte;
                                      P_parity   : Array[1..172] Of Byte;
                                      Q_parity   : Array[1..104] Of Byte;
                                End;
Type T_CDROM_format_mode2_form2=Record
                                      Sync_block : T_CDROM_format_sync_block;
                                      Sub_hdr    : T_CDROM_format_mode2_sub_hdrs;
                                      User_data  : Array[1..2324] Of Byte;
                                      CRC16      : Array[1..4] Of Byte;
                                End;
Type P_Byte=^Byte;
     P_LongWord=^LongWord;

Function CD_mode_to_data_mode(CD_mode : Byte) : Byte;

Procedure Fill_sync_block(P_buf : Pointer; data_mode : Byte; lead_in_flag : Boolean; Sector_MSF : T_MSF);

Procedure Fill_EDCECC(P_buf : Pointer; cd_mode : Byte);

Procedure Fill_P(P_buf : Pointer; cd_mode : Byte);

Procedure Fill_Q(P_buf : Pointer; cd_mode : Byte);

Procedure Fill_Mode2Form2_SubHdr_20h(P_buf : Pointer);

Procedure Fill_cd_block(P_buf : Pointer; cd_mode : Byte; lead_in_flag : Boolean; Sector_MSF : T_MSF);

Function Scan_format_of_data_track_from_CD(Search_MSF : T_MSF) : Byte;

Function Scan_format_of_data_track_from_file(in_sector_file : TFileStream;
                                             in_sector_file_block_size : Word;
                                             in_sector_file_offset : Integer;
                                             Search_MSF : T_MSF) : Byte;

Function Find_track_no_from_2448_file(in_file_to_read : TFileStream;
                                      in_track_no : Byte) : Integer;

Function Find_p_value_from_2448_file(in_file_to_read : TFileStream;
                                     in_p_value : Byte) : Integer;

implementation

uses MainFormUnit;

Function CD_mode_to_data_mode(CD_mode : Byte) : Byte;
Var data_mode : Byte;
Begin
     Case CD_mode Of
       CDROM_SECTORTYPE_MODE1:
       Begin
            data_mode:=1;
       End;
       CDROM_SECTORTYPE_MODE2FORMLESS:
       Begin
            data_mode:=2;
       End;
       CDROM_SECTORTYPE_MODE2FORM1:
       Begin
            data_mode:=2;
       End;
       CDROM_SECTORTYPE_MODE2FORM2:
       Begin
            data_mode:=2;
       End;
       CDROM_SECTORTYPE_MODE0:
       Begin
            data_mode:=0;
       End;
       CDROM_SECTORTYPE_UNKNOWN:
       Begin
            data_mode:=0;
       End;
     End;

     CD_mode_to_data_mode:=data_mode;
End;

Procedure Fill_sync_block(P_buf : Pointer; data_mode : Byte; lead_in_flag : Boolean; Sector_MSF : T_MSF);
Var i : Byte;
Begin
     T_CDROM_format_sync_field(P_buf^).Zero1:=0;
     For i:=1 To 10 Do
     Begin
          T_CDROM_format_sync_field(P_buf^).FFh[i]:=$FF;
     End;
     T_CDROM_format_sync_field(P_buf^).Zero2:=0;

     If lead_in_flag=False Then
     Begin
          T_CDROM_format_sync_block(P_buf^).Hdr.Min:=StrToInt('$'+IntToStr(Sector_MSF.M));
     End
     Else
     Begin
          T_CDROM_format_sync_block(P_buf^).Hdr.Min:=StrToInt('$A'+IntToStr(Sector_MSF.M));
     End;

     T_CDROM_format_sync_block(P_buf^).Hdr.Sec:=StrToInt('$'+IntToStr(Sector_MSF.S));
     T_CDROM_format_sync_block(P_buf^).Hdr.Frame:=StrToInt('$'+IntToStr(Sector_MSF.F));
     T_CDROM_format_sync_block(P_buf^).Hdr.Data_mode:=data_mode;
End;

Procedure Fill_EDCECC(P_buf : Pointer; cd_mode : Byte);
Var p : P_Byte;
Begin
     Case cd_mode Of
       CDROM_SECTORTYPE_MODE1:
       Begin
            p:=P_buf;
            Inc(p, 2064);

            LongWord((@(p^))^):=Calc_EDC(P_buf, 2064);
       End;
       CDROM_SECTORTYPE_MODE2FORMLESS:
       Begin
            //No EDC/ECC
       End;
       CDROM_SECTORTYPE_MODE2FORM1:
       Begin
            p:=P_buf;
            Inc(p, 2072);
            Inc(P_Byte(P_buf), 16);

            LongWord((@(p^))^):=Calc_EDC(P_buf, 2056);
       End;
       CDROM_SECTORTYPE_MODE2FORM2:
       Begin
            p:=P_buf;
            Inc(p, 2348);
            Inc(P_Byte(P_buf), 16);

            LongWord((@(p^))^):=Calc_EDC(P_buf, 2332);
       End;
       CDROM_SECTORTYPE_MODE0:
       Begin
            //No EDC/ECC
       End;
       CDROM_SECTORTYPE_UNKNOWN:
       Begin
            //No EDC/ECC
       End;
     End;
End;

Procedure Fill_P(P_buf : Pointer; cd_mode : Byte);
Begin
     Case cd_mode Of
       CDROM_SECTORTYPE_MODE1:
       Begin
            encode_L2_P(P_buf, 12);
       End;
       CDROM_SECTORTYPE_MODE2FORMLESS:
       Begin
            //No P parity ECC symbols
       End;
       CDROM_SECTORTYPE_MODE2FORM1:
       Begin
            encode_L2_P(P_buf, 16);
       End;
       CDROM_SECTORTYPE_MODE2FORM2:
       Begin
            //No P parity ECC symbols
       End;
       CDROM_SECTORTYPE_MODE0:
       Begin
            //No P parity ECC symbols
       End;
       CDROM_SECTORTYPE_UNKNOWN:
       Begin
            //No EDC/ECC
       End;
     End;
End;

Procedure Fill_Q(P_buf : Pointer; cd_mode : Byte);
Begin
     Case cd_mode Of
       CDROM_SECTORTYPE_MODE1:
       Begin
            encode_L2_Q(P_buf, 12);
       End;
       CDROM_SECTORTYPE_MODE2FORMLESS:
       Begin
            //No Q parity ECC symbols
       End;
       CDROM_SECTORTYPE_MODE2FORM1:
       Begin
            encode_L2_Q(P_buf, 16);
       End;
       CDROM_SECTORTYPE_MODE2FORM2:
       Begin
            //No Q parity ECC symbols
       End;
       CDROM_SECTORTYPE_MODE0:
       Begin
            //No Q parity ECC symbols
       End;
       CDROM_SECTORTYPE_UNKNOWN:
       Begin
            //No Q parity ECC symbols
       End;
     End;
End;

Procedure Fill_Mode2Form2_SubHdr_20h(P_buf : Pointer);
Begin
     T_CDROM_format_mode2_sub_hdrs(P_Buf^).Hdr1.Sub_mode:=$20;
     T_CDROM_format_mode2_sub_hdrs(P_Buf^).Hdr2.Sub_mode:=$20;
End;

Procedure Fill_cd_block(P_buf : Pointer; cd_mode : Byte; lead_in_flag : Boolean; Sector_MSF : T_MSF);
Var p : P_Byte;
    p2 : P_Byte;
Begin
     Case cd_mode Of
       CDROM_SECTORTYPE_MODE1:
       Begin
            Fill_sync_block(P_buf, 1, lead_in_flag, Sector_MSF);

            p:=P_Buf;
            Inc(p, 2064);

            LongWord((@(p^))^):=Calc_EDC(P_buf, 2064);

            encode_L2_P(P_buf, 12);
            encode_L2_Q(P_buf, 12);
       End;
       CDROM_SECTORTYPE_MODE2FORMLESS:
       Begin
            Fill_sync_block(P_buf, 2, lead_in_flag, Sector_MSF);
       End;
       CDROM_SECTORTYPE_MODE2FORM1:
       Begin
            Fill_sync_block(P_buf, 2, lead_in_flag, Sector_MSF);

            p:=P_buf;
            Inc(p, 2072);
            p2:=P_buf;
            Inc(p2, 16);

            LongWord((@(p^))^):=Calc_EDC(p2, 2056);

            encode_L2_P(P_buf, 16);
            encode_L2_Q(P_buf, 16);
       End;
       CDROM_SECTORTYPE_MODE2FORM2:
       Begin
            Fill_sync_block(P_buf, 2, lead_in_flag, Sector_MSF);

            p:=P_Buf;
            Inc(p, 2348);
            Inc(P_Byte(P_Buf), 16);

            LongWord((@(p^))^):=Calc_EDC(P_Buf, 2332);

            T_CDROM_format_mode2_sub_hdrs(P_Buf^).Hdr1.Sub_mode:=$20;
            T_CDROM_format_mode2_sub_hdrs(P_Buf^).Hdr2.Sub_mode:=$20;
       End;
       CDROM_SECTORTYPE_MODE0:
       Begin
            Fill_sync_block(P_buf, 0, lead_in_flag, Sector_MSF);
       End;
       CDROM_SECTORTYPE_UNKNOWN:
       Begin
            //Do nothing
       End;
     End;
End;

Function Scan_format_of_data_track_from_CD(Search_MSF : T_MSF) : Byte;
Var MMCLBA : LongInt;
    CD_mode : Byte;
    SCSI : T_SCSI;
Begin
     If Form1.CommonCDSettings.Create_new_SCSI_obj(SCSI) Then
     Begin
          MMCLBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_MMCLBA(Search_MSF.M,
                                                                  Search_MSF.S,
                                                                  Search_MSF.F);

          SCSI.MMC1_any_link.Do_readCD_byFormat_CDB12(MMCLBA,
                                                      1,
                                                      MMC_SECTORTYPE_ANY,
                                                      MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC,
                                                      MMC_READCD_NO_SUBCH);
          If SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               Case (T_CDROM_format_sync_block(SCSI.SPC_any_link.Get_data_buf^).Hdr.Data_mode And 3) Of
               0: CD_mode:=CDROM_SECTORTYPE_MODE0;
               1: CD_mode:=CDROM_SECTORTYPE_MODE1;
               2: Begin
                       If CompareMem(@(T_CDROM_format_sync_block_mode2_sub_hdrs(SCSI.SPC_any_link.Get_data_buf^).Sub_hdr.Hdr1),
                                     @(T_CDROM_format_sync_block_mode2_sub_hdrs(SCSI.SPC_any_link.Get_data_buf^).Sub_hdr.Hdr2), 4)
                                     Then
                       Begin
                            If ((T_CDROM_format_sync_block_mode2_sub_hdrs(SCSI.SPC_any_link.Get_data_buf^).Sub_hdr.Hdr1.Sub_mode) And
                            $20)=0
                            Then
                            Begin
                                 CD_mode:=CDROM_SECTORTYPE_MODE2FORM1;
                            End
                            Else
                            Begin
                                 CD_mode:=CDROM_SECTORTYPE_MODE2FORM2;
                            End
                       End
                       Else
                       Begin
                            CD_mode:=CDROM_SECTORTYPE_MODE2FORMLESS;
                       End;
                  End;
               Else
                   CD_mode:=CDROM_SECTORTYPE_ANY;
               End;
          End
          Else
              CD_mode:=CDROM_SECTORTYPE_UNKNOWN;

          SCSI.Free;
     End
     Else
     Begin
          CD_mode:=CDROM_SECTORTYPE_UNKNOWN;
     End;

     Scan_format_of_data_track_from_CD:=CD_mode;
End;

Function Scan_format_of_data_track_from_file(in_sector_file : TFileStream;
                                             in_sector_file_block_size : Word;
                                             in_sector_file_offset : Integer;
                                             Search_MSF : T_MSF) : Byte;
Var LBA : LongInt;
    P_buf : Pointer;
    CD_mode : Byte;
Begin
     LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(Search_MSF.M,
                                                       Search_MSF.S,
                                                       Search_MSF.F);

     If (LBA>=0) AND
        (((LBA+in_sector_file_offset)*in_sector_file_block_size)<in_sector_file.Size) Then
     Begin
          GetMem(P_buf, 21);

          in_sector_file.Seek((LBA+in_sector_file_offset)*in_sector_file_block_size,
                              soFromBeginning);
          in_sector_file.ReadBuffer(P_buf^, 21);

          Case (T_CDROM_format_sync_block(P_Buf^).Hdr.Data_mode And 3) Of
          0: CD_mode:=CDROM_SECTORTYPE_MODE0;
          1: CD_mode:=CDROM_SECTORTYPE_MODE1;
          2: Begin
                  If CompareMem(@(T_CDROM_format_sync_block_mode2_sub_hdrs(P_Buf^).Sub_hdr.Hdr1),
                                @(T_CDROM_format_sync_block_mode2_sub_hdrs(P_Buf^).Sub_hdr.Hdr2), 4)
                                Then
                  Begin
                       If ((T_CDROM_format_sync_block_mode2_sub_hdrs(P_Buf^).Sub_hdr.Hdr1.Sub_mode) And
                       $20)=0
                       Then
                       Begin
                            CD_mode:=CDROM_SECTORTYPE_MODE2FORM1;
                       End
                       Else
                       Begin
                            CD_mode:=CDROM_SECTORTYPE_MODE2FORM2;
                       End
                  End
                  Else
                  Begin
                       CD_mode:=CDROM_SECTORTYPE_MODE2FORMLESS;
                  End;
             End;
          Else
              CD_mode:=CDROM_SECTORTYPE_ANY;
          End;

          FreeMem(P_buf, 21);
     End
     Else
         CD_mode:=CDROM_SECTORTYPE_UNKNOWN;

     in_sector_file.Seek(0, soFromBeginning);

     Scan_format_of_data_track_from_file:=CD_mode;
End;

Function Find_track_no_from_2448_file(in_file_to_read : TFileStream;
                                      in_track_no : Byte) : Integer;
{ Searches from the current seek position of a 2448 byte/sector CD image file
  for the sector which has in_track_no recorded in the sub-channel Q data.
  If found it returns the sector no.  Note, this is not the file position -
  the file position is actually: 'sector no. returned' * 2448.  How ever, after
  found, the seek position will be left at the sector found.  If none found,
  then seek position will be left at the end of the file. }

Var P_read_buffer : Pointer;
    last_sector_in_file : Integer;
    j : Integer;
    i : Integer;
    N_sectors_to_read : Integer;
    P_read_buffer_i : Pointer;
    stop_loop : Boolean;
Begin
     GetMem(P_Byte(P_read_buffer), 2007360);

     last_sector_in_file:=(in_file_to_read.Size-in_file_to_read.Position) DIV 2448;

     N_sectors_to_read:=820; // 2007360 / 2448 = 820

     j:=0;
     stop_loop:=False;
     While((j<last_sector_in_file) And (stop_loop=False)) Do
     Begin
          If (last_sector_in_file-j)<820 Then
             N_sectors_to_read:=last_sector_in_file-j;

          in_file_to_read.ReadBuffer(P_read_buffer^, N_sectors_to_read*2448);

          i:=0;
          P_read_buffer_i:=P_read_buffer;
          Repeat
                //Go to the start of de-interlaced subch data
                Inc(P_Byte(P_read_buffer_i), 2352);

                If T_subch_deint_PW_96(P_read_buffer_i^).PW[2][2]=in_track_no Then
                Begin
                     stop_loop:=True;
                End
                Else
                Begin
                     //Go to the start of next sector
                     Inc(P_Byte(P_read_buffer_i), 96);

                     i:=i+1;
                     j:=j+1;
                End;
          Until((i=N_sectors_to_read) Or stop_loop);
     End;

     //Seek back to the position at j*2448
     in_file_to_read.Seek((i-N_sectors_to_read+1)*2448, soFromCurrent);

     FreeMem(P_read_buffer);

     Find_track_no_from_2448_file:=j;
End;

Function Find_p_value_from_2448_file(in_file_to_read : TFileStream;
                                     in_p_value : Byte) : Integer;
{ Searches from the current seek position of a 2448 byte/sector CD image file
  for the sector which has in_p_value recorded in the sub-channel P data.
  If found it returns the sector no.  Note, this is not the file position -
  the file position is actually: 'sector no. returned' * 2448.  How ever, after
  found, the seek position will be left at the sector found.  If none found,
  then seek position will be left at the end of the file. }

Var P_read_buffer : Pointer;
    last_sector_in_file : Integer;
    j : Integer;
    i : Integer;
    N_sectors_to_read : Integer;
    P_read_buffer_i : Pointer;
    stop_loop : Boolean;
Begin
     GetMem(P_Byte(P_read_buffer), 2007360);

     last_sector_in_file:=(in_file_to_read.Size-in_file_to_read.Position) DIV 2448;

     N_sectors_to_read:=820;

     j:=0;
     stop_loop:=False;
     While((j<last_sector_in_file) And (stop_loop=False)) Do
     Begin
          If (last_sector_in_file-j)<820 Then
             N_sectors_to_read:=last_sector_in_file-j;

          in_file_to_read.ReadBuffer(P_read_buffer^, N_sectors_to_read*2448);

          i:=0;
          P_read_buffer_i:=P_read_buffer;
          Repeat
                //Go to the start of de-interlaced subch data
                Inc(P_Byte(P_read_buffer_i), 2352);

                If T_subch_deint_PW_96(P_read_buffer_i^).PW[1][1]=in_p_value Then
                Begin
                     stop_loop:=True;
                End
                Else
                Begin
                     //Go to the start of next sector
                     Inc(P_Byte(P_read_buffer_i), 96);

                     i:=i+1;
                     j:=j+1;
                End;
          Until((i=N_sectors_to_read) Or stop_loop);
     End;

     //Seek back to the position just after j
     in_file_to_read.Seek((i-N_sectors_to_read+1)*2448, soFromCurrent);

     FreeMem(P_read_buffer);

     Find_p_value_from_2448_file:=j;
End;


end.
