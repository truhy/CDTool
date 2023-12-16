{ Name:         MMC1Unit.PAS
  File type:    Borland Delphi 4 unit file.
  Description:  Contains a class which encapsulates the SCSI-3 MMC-1 (for
                multimedia SCSI device) functions.  This source file uses
                information given from the Adaptec's ASPI SDK and working draft
                version of SCSI-3 MMC-1 specification document.  These commands
                are for devices which are considered as multimedia devices,
                examples are those which work with mediums such as CDROM, DVD,
                DVDRAM, etc.
  Notes:        Written from scratch, no other samples used, except SDK.
                All function and parameters are in Intel x86 byte ordering,
                i.e. from LSB to MSB.
  Date started: 12th Jan 2000.
  Developer:    Truong Hy.

  Other notes:

  Some detailed notes:

  Legend:
  MMC = MultiMedia Commands
  SRB = SCSI Request Block.
  CDB = Command Descriptor Block.
  LBA = Logical Block Addressing.

  LBA sectors
  -----------
  The LBA sectors as described in the MMC specs will be called 'MMC Logical
  Block Addressing' (MMCLBA) in this source code.  They are a little bit
  difficult to work with, so I have decided to define an alternative - which I
  will call normal 'Logical Block Addressing' (LBA).

  With MMCLBA each disk address is represented as logical block numbers called
  sectors.  The sectors are signed long integer values to account for negative
  values.  Negative values are used to represent MSF which are:
      1. below 2 secs, range from 0:0:0 to 0:1:74 (MSF) is represented as
         MMCLBA: -150 to -1.
      2. and the range from 90:0:0 to 99:59:74 (MSF) (MMCLBA: -150 to -1) is
         represented as MMCLBA: -45150 to -151.
  MMCLBA are linear integer values starting from -45150 to 403964.
  However, it is not linear with Red Book Addressing (MSF, Minute:Second:Frame).

  To make things easier the normal LBA is similar to MMCLBA with the difference
  where the range(2) from 90:0:0 to 99:59:74 will not be represented as negative
  values, but instead represented with positive values leading
  from 89:59:74.  This makes normal LBA linear starting from:
  -150 to 449849 (0:0:0 to 99:59:74), which is linear with Red Book Addressing
  (MSF, Minute:Second:Frame)

  The relationship between MMC Logical Block Addressing and the Red Book
  frames is described by the 2 equations:

  Valid only for MSF range from 0:0:0 to 89:59:74:
  1. MMCLBA sector=Minute * 60 * 75 + Second * 75 + Frame - 150
  The minus 150 is the 2 second pregap that is recorded on every CD.

  MSF from 90:0:0 to 99:59:74 are defined as negative MMCLBA values:
  -45150 to -151.  This is to wrap around to -150 (MSF 0:0:0).
  The equation for this range is different and is described as:
  2. MMCLBA sector=Minute * 60 * 75 + Second * 75 + Frame - 450150.

  The relationship with normal LBA is just 1 equation:
  1. LBA sector=Minute * 60 * 75 + Second * 75 + Frame - 150

  Friendly/unfriendly method versions
  -----------------------------------
  Unfriendly methods will try to follow as close as possible to the calling
  conventions and the types as described in the MMC specs and will leave the
  data in the SRB memory area.

  Friendly methods will in most cases format the data into more easier to use
  input/output parameters, variables and types.

  Hardware
  --------
  In my tests I found that most old CDROM drives (even some newer ones) are very
  choosy on which commands they support.

  Initial problem with sense/select commands
  ------------------------------------------

  Some of the commands (if not supported by the drive) will make the drive
  hang.  This means that the drive will not return a reponse back to WinASPI.
  WinAPSI will be waiting for ever for a response, so it will appear that you
  application has just hung.  The solution for this is to wait for a specified
  time and then if no response from hardware, send an abort request.  You can
  also eject the tray - which cancels the unsupported command and the drive will
  return a response, so that WinASPI will return.

  Regarding sense/select commands most drives are very sensitive.  I have found
  that most drives will work with the CDB10 version and some drives work
  with both.  I haven't come across a drive that works only with CDB6 but this
  is also possible.  The only 100% solution is to provide an option for user to
  select which version to use.  Or like some, make a list of drive models with
  tested valid versions.

  Further problems with sense/select commands
  -------------------------------------------

  Also, there are 2 more problems with the sense/select commands.

  1. The MMC spec isn't very strict on whether a drive returns any block
     descriptor/s so this means that drive firmware programmers can choose
     whether to use one or more block descriptors.  The problem is that before
     sending the sense command we have to allocate and request the number of
     bytes that the drive will return and also we have to set a parameter
     to ask whether to expect block descriptors.

  2. Some old CDROM drives do not return or accept the last bytes of the
     parameter list for that particular mode page.  The reason for this problem
     is because devices are allowed to return less than the full mode parameters
     page.  If you ask the drive to return more data than it can return,
     WinASPI32 will wait for ever.  Well until the device or SCSI bus
     times out, which may be a very long time!!  Note that some drives don't
     have this behaviour and will never hang like this, instead they just return
     and give an invalid parameter in CDB error - this is what all drives should
     do.

     Case scenario of the problems I found
     -------------------------------------

     a. The sense command with the 'CD capabilities & mechanism status mode
        page'.  The last 2 or 6 bytes from the parameter page are not supported
        by some CDROM drives and WinASPI32 will wait for them, thus it appears
        that your program is hanging when really it is waiting for WinASPI32 to
        return with a status.
     b. The select command with the 'Write parameters mode page'.  The last 4 or
        8 bytes are not accepted by some CD rewriters.  You need to send the
        mode page without those last bytes for those particular drives.
        Otherwise the drive will report CDB command failed.

  Fixing problem 1
  ----------------

  To solve problem (1) I made a method to find out whether the drive will return
  any block descriptor/s and then.  This relies on the drive reporting the
  correct values for the block descriptor/s length field.

  The method I used is to first retrieve the sense header only (i.e. without
  retrieving the parameter list (the data)) just to find out if the drive
  returns any block descriptor/s - by looking at the returned block descriptor/s
  length field.

  Fixing problem 2
  ----------------

  To solve problem (2) I made some methods to work out how much data can be
  returned.  This relies on the drive reporting the correct values for the
  sense mode data len or param len fields.

  The method I used is to first find out if descriptor/s are returned from
  device, by calling one of the solution method in problem (1).  Then send the
  sense command again but this time retrieving any block descriptor/s and if
  necessary reading only 2 bytes of the parameter list.  The returned data
  will then allow me to determine how much data the drive can return and the
  calculated rest of the parameter list can be retrieved successfully.

  I have implemented 2 possible versions for this method:
            1. uses mode_data_len from the sense header
            2. the other uses the param_len from the beginning of the parameter
               list for the calculation,

  Method 1 will mean that the sense commands will call sense command two times.
  Method 2 will mean that the sense commands will call sense command three
  times.  I suppose calling the sense command many times over doesn't really
  matter because they get processed very quickly anyway.  Note: for both methods
  - if the first call to mode sense fails then no further checking or sense
  commands are made - in this case only a single call was made.

  From tests of my own, it seems that both version work just fine, but to be
  sure I wanted to use both, so I've implemented a further method which will use
  method 1 first and if it is invalid uses method 2.

  Just to be complete there is one further solution but only for sense command
  with 'CD capabilities & mechanism status mode page'.  The methods are:
    1. Do_sense6_CD_cap_mech_st_sub6
    2. Do_sense10_CD_cap_mech_st_sub6
  These 2 will just retrieving the full parameters list with 6 bytes less.
  They are not neccessary to be used - unless the device does not report the
  length fields correctly.
}

unit MMC1Unit;

interface

Uses
    SPC_Unit,
    Windows,
    //Dialogs, //For debugging only. Allows message boxes. Remove this in final release.
    SysUtils;

{$I Buf_inc.pas}
{$I MMC1_inc.pas} //A separate include file listing constants and types.

Type T_MMC1=
Class
Public
      SPC : T_SPC; //SCSI-3 SPC instance.

      Constructor Create;
      Destructor Destroy; Override;
      Function MSF_to_MMCLBA(M, S, F : Byte) : LongInt;
      Procedure MMCLBA_to_MSF(    MMCLBA : LongInt;
                              Var M      : Byte;
                              Var S      : Byte;
                              Var F      : Byte);
      Function MSF_to_LBA(M, S, F : Byte) : LongInt;
      Procedure LBA_to_MSF(    LBA : LongInt;
                           Var M   : Byte;
                           Var S   : Byte;
                           Var F   : Byte);
      Function MMCLBA_to_LBA(MMCLBA : LongInt) : LongInt;
      Function LBA_to_MMCLBA(LBA : LongInt) : LongInt;
      Procedure ABSLBA_to_MSF(    ABSLBA : LongInt;
                              Var M      : Byte;
                              Var S      : Byte;
                              Var F      : Byte);
      Function GetFullBlockSizeFromFilterReadFormat(SectTypeFilter  : Byte;
                                                    ReadFormatFlags : Byte) : Word;
      Function GetBlockSizeFromFilterReadFormat(SectTypeFilter  : Byte;
                                                ReadFormatFlags : Byte) : Word;
      Function GetBlockSizeFromC2ErrorField(ReadFormatFlags : Byte) : Word;
      Function GetBlockSizeFromSubCh(SubChSel : Byte) : Word;
      { Methods for filling in CDBs with SCSI-3 MMC command values.. }
      Procedure Fill_CDB10_do_read_rec_cap(Var Out_CDB : T_readCD_rec_cap);
      Procedure Fill_CDB10_do_read_subch(Var Out_CDB    : T_read_subch;
                                             In_MSF     : Byte;
                                             In_SubQ    : Byte;
                                             In_Params  : Byte;
                                             In_TrackNo : Byte);
      Procedure Fill_CDB10_do_read_T_P_A(Var Out_CDB      : T_read_T_P_A;
                                             In_MSF       : Byte;
                                             In_format    : Byte;
                                             In_trkSessNo : Byte);
      Procedure Fill_CDB10_do_read_header(Var Out_CDB       : T_read_header;
                                              In_MSF        : Byte;
                                              In_MMCLBA_MSF : Pointer);
      Procedure Fill_CDB10_do_read_header_MMCLBA(Var Out_CDB   : T_read_header;
                                                     In_MMCLBA : LongInt);
      Procedure Fill_CDB10_do_read_header_MSF(Var Out_CDB : T_read_header;
                                                  In_MSF  : T_MSF);
      Procedure Fill_CDB12_do_readCD_MSF_byFormat(Var Out_CDB            : T_readCD_MSF;
                                                  Var In_StartMSF        : T_MSF;
                                                  Var In_EndMSF          : T_MSF;
                                                      In_SectTypeFilter  : Byte;
                                                      In_ReadFormatFlags : Byte;
                                                      In_SubChSel        : Byte);
      Procedure Fill_CDB12_do_set_CD_speed(Var Out_CDB       : T_set_CD_speed;
                                               In_ReadSpeed  : Word;
                                               In_WriteSpeed : Word);
      Procedure Fill_CDB12_do_readCD_byFormat(Var Out_CDB            :T_readCD_CDB12;
                                                  In_Start_MMCLBA    : LongInt;
                                                  In_N_sectors       : LongWord;
                                                  In_SectTypeFilter  : Byte;
                                                  In_ReadFormatFlags : Byte;
                                                  In_SubChSel        : Byte);
      Procedure Fill_CDB10_do_read_buf_cap(Var Out_CDB : T_read_buf_cap_CDB10);
      Procedure Fill_CDB10_do_read_disc_info(Var Out_CDB : T_read_disc_info_CDB10; In_AllocLen : Word);
      Procedure Fill_CDB10_do_read_track_info(Var Out_CDB         : T_read_track_info_CDB10;
                                                  In_TrkBIT       : Byte;
                                                  In_MMCLBA_TrkNo : LongInt);
      Procedure Fill_CDB10_do_reserve_track(Var Out_CDB : T_reserve_track_CDB10;
                                            In_Reverv_size : LongWord);
      Procedure Fill_CDB10_do_synchronize_cache(Var Out_CDB     : T_synchronize_cache_CDB10;
                                                    In_MMCLBA   : LongInt;
                                                    In_N_blocks : Word);
      Procedure Fill_CDB10_do_write(Var Out_CDB           : T_write_CDB10;
                                        In_DPO_FUA_RELADR : Byte;
                                        In_MMCLBA         : LongInt;
                                        In_N_blocks       : Word;
                                        In_block_size     : Word);
      { Mode page fill methods.. }
      Procedure Fill_mode_param_hdr_6(In_P_Buf : Pointer);
      Procedure Fill_mode_param_hdr_10(In_P_Buf : Pointer);
      Procedure Fill_block_desc(P_block_desc : Pointer);
      Procedure Fill_mode_pg_read_err_rec(    P_read_err_rec      : Pointer;
                                          In_write_params_page_len : byte;
                                          Var In_err_rec_param    : Byte;
                                          Var In_read_retry_count : Byte;
                                              Set_all             : Boolean);
      Procedure Fill_mode_pg_write(    P_read_err_rec  : Pointer;
                                       In_write_params_page_len : byte;
                                   Var In_write_params : T_pub_write_params;
                                       Set_all         : Boolean);
      Procedure Fill_mode_pg_veri_err_rec(    P_veri_err_rec      : Pointer;
                                          In_write_params_page_len : byte;
                                          Var In_err_rec_param    : Byte;
                                          Var In_veri_retry_count : Byte;
                                              Set_all             : Boolean);
      Procedure Fill_mode_pg_CD(    P_CD              : Pointer;
                                In_write_params_page_len : byte;
                                Var In_inactivity_mul : Byte;
                                Var In_sec_per_MSF    : Word;
                                Var In_frames_per_MSF : Word;
                                    Set_all           : Boolean);
      Procedure Fill_mode_pg_CD_audio_ctrl(    P_CD_audio_ctrl  : Pointer;
                                           In_write_params_page_len : byte;
                                           Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl;
                                               Set_all          : Boolean);
      Procedure Fill_out_CD_cap_mech_st(    P_CD_cap_mech_st   : Pointer;
                                        Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st);
      {** Select/sense for MMC mode parameters.. **}
      Function Calc_modepage6_ptr(In_P_Buf : Pointer) : Pointer;
      Function Calc_modepage10_ptr(In_P_Buf : Pointer) : Pointer;
End;

implementation

Constructor T_MMC1.Create;
Begin
     //Inherited Create;

     SPC:=T_SPC.Create;
End;

Destructor T_MMC1.Destroy;
Begin
     SPC.Destroy;

     //Inherited;
End;

Function T_MMC1.MSF_to_MMCLBA(M, S, F : Byte) : LongInt;
{ **********************************************************************
  Converts Red Book Addressing value (MSF) to
  MMC Logical Block Addressing (MMCLBA) value.

  With Red Book addressing each disk address is represented
  as minutes, seconds and 1/75 per second frames.
  Each of these field takes up 1 byte.

  With MMC Logical Block Addressing each disk address is represented
  as logical block numbers called sectors.  The sectors are
  signed long integer values to account for negative values which
  are used to represent for MSF which are:
      1. below 2 secs, range from 0:0:0 to 0:1:74 (MSF)
      2. and the range from 90:0:0 to 99:59:74 (MSF).

  The relationship between MMC Logical Block Addressing and the Red Book
  frames is described by 2 equations:

  Valid only for MSF range from 0:0:0 to 89:59:74:
  1. MMCLBA sector=Minute * 60 * 75 + Second * 75 + Frame - 150
  The minus 150 is the 2 second pregap that is recorded on every CD.

  MSF from 90:0:0 to 99:59:74 are defined as negative MMCLBA values:
  -45150 to -151.  This is to wrap around to -150 (MSF 0:0:0).
  The equation for this range is different and is described as:
  2. MMCLBA sector=Minute * 60 * 75 + Second * 75 + Frame - 450150.
  ********************************************************************** }
Begin
     If M<90 Then
         MSF_to_MMCLBA:=M*60*75+S*75+F-150
     Else
         MSF_to_MMCLBA:=M*60*75+S*75+F-450150;
End;

Procedure T_MMC1.MMCLBA_to_MSF(    MMCLBA : LongInt;
                               Var M      : Byte;
                               Var S      : Byte;
                               Var F      : Byte);
{ *******************************************************
  Converts MMC Logical Block Addressing value (MMCLBA) to
  Red Book Addressing value (MSF).
  ******************************************************* }
Begin
     If MMCLBA>=-150 Then
     Begin
          M:=Trunc((MMCLBA+150)/(60*75));
          S:=Trunc((MMCLBA+150-M*60*75)/75);
          F:=MMCLBA+150-M*60*75-S*75;
     End
     Else
     Begin
          M:=Trunc((MMCLBA+450150)/(60*75));
          S:=Trunc((MMCLBA+450150-M*60*75)/75);
          F:=MMCLBA+450150-M*60*75-S*75;
     End;
End;

Function T_MMC1.MSF_to_LBA(M, S, F : Byte) : LongInt;
{ **************************************************************
  Converts Red Book Addressing value (MSF) to
  Logical Block Addressing (LBA) value.

  With Red Book Addressing each disk address is represented
  as minutes, seconds and 1/75 per second frames.
  Each of these field takes up 1 byte.

  With Logical Block Addressing each disk address is represented
  as logical block numbers called sectors.  LBA is similar to MMCLBA
  except the range MSF from 90:0:0 to 99:59:74 are defined as
  positive integer values: 404850 to 449849.

  The relationship between LBA sectors and the Red Book
  frames is described by this equation:
  LBA sector=Minute * 60 * 75 + Second * 75 + Frame - 150
  ************************************************************** }
Begin
     MSF_to_LBA:=M*60*75+S*75+F-150;
End;

Procedure T_MMC1.LBA_to_MSF(    LBA : LongInt;
                            Var M   : Byte;
                            Var S   : Byte;
                            Var F   : Byte);
{ ************************************************
  Converts Logical Block Addressing value (LBA) to
  Red Book Addressing value (MSF).
  ************************************************ }
Begin
     If LBA>=-150 Then
     Begin
          M:=Trunc((LBA+150)/(60*75));
          S:=Trunc((LBA+150-M*60*75)/75);
          F:=LBA+150-M*60*75-S*75;
     End
     Else
     Begin
          M:=Trunc((LBA+450150)/(60*75));
          S:=Trunc((LBA+450150-M*60*75)/75);
          F:=LBA+450150-M*60*75-S*75;
     End;
End;

Function T_MMC1.MMCLBA_to_LBA(MMCLBA : LongInt) : LongInt;
{ *******************************************************
  Converts MMC Logical Block Addressing value (MMCLBA) to
  Logical Block Addressing (LBA) value.
  ******************************************************* }
Begin
     If MMCLBA<-150 Then
         MMCLBA_to_LBA:=MMCLBA+450000
     Else
         MMCLBA_to_LBA:=MMCLBA;
End;

Function T_MMC1.LBA_to_MMCLBA(LBA : LongInt) : LongInt;
{ ************************************************
  Converts Logical Block Addressing (LBA) value to
  MMC Logical Block Addressing value (MMCLBA).
  ************************************************ }
Begin
     If LBA>=404850 Then
         LBA_to_MMCLBA:=LBA-450000
     Else
         LBA_to_MMCLBA:=LBA;
End;

Procedure T_MMC1.ABSLBA_to_MSF(    ABSLBA : LongInt;
                               Var M      : Byte;
                               Var S      : Byte;
                               Var F      : Byte);
{ ************************************************************
  Converts Absolute Logical Block Addressing value (ABSLBA) to
  Red Book Addressing value (MSF).
  ************************************************************ }
Begin
     If ABSLBA>=0 Then
     Begin
          M:=Trunc(ABSLBA/(60*75));
          S:=Trunc((ABSLBA-M*60*75)/75);
          F:=ABSLBA-M*60*75-S*75;
     End
     Else
     Begin
          M:=Trunc((ABSLBA+450150)/(60*75));
          S:=Trunc((ABSLBA+450150-M*60*75)/75);
          F:=ABSLBA+450150-M*60*75-S*75;
     End;
End;

Function T_MMC1.GetFullBlockSizeFromFilterReadFormat(SectTypeFilter  : Byte;
                                                     ReadFormatFlags : Byte) : Word;
{ ****************************************************************
  Calculates full block size from Read CD read sector type filter
  and format flags.
  **************************************************************** }
Var BlockSize           : Word;
    SyncSize            : Byte;
    HdrSize             : Byte; //Block address+mode header size.
    SubHdrSize          : Byte; //Mode 2 subheader size.
    EDCECCSize          : Word; //CRC, P and Q symbol size.
    UserDataSize        : Word;
    C2ErrSize           : Word;
    BlockErrSize        : Byte;
    PadSize             : Byte; //Accumulative padding size.
Begin
     SyncSize:=0;
     HdrSize:=0;
     SubHdrSize:=0;
     EDCECCSize:=0;
     UserDataSize:=0;
     C2ErrSize:=0;
     BlockErrSize:=0;
     PadSize:=0; //Accumulative pad size.

     Case SectTypeFilter Of
          0: Begin //Any format.
                  UserDataSize:=2352;
                  If (ReadFormatFlags And 6)<>6 Then
                  Begin
                       If (ReadFormatFlags And MMC_READCD_C2_PTR)<>0 Then
                       Begin
                            C2ErrSize:=294;
                       End
                       Else
                       Begin
                            If (ReadFormatFlags And MMC_READCD_C2_PTR_BLK)<>0 Then
                            Begin
                                 C2ErrSize:=294;
                                 BlockErrSize:=1;
                                 PadSize:=PadSize+1;
                            End;
                       End
                  End;
             End;
          1: Begin //CDDA.
                  UserDataSize:=2352;
                  If (ReadFormatFlags And 6)<>6 Then
                  Begin
                       If (ReadFormatFlags And MMC_READCD_C2_PTR)<>0 Then
                       Begin
                            C2ErrSize:=294;
                       End
                       Else
                       Begin
                            If (ReadFormatFlags And MMC_READCD_C2_PTR_BLK)<>0 Then
                            Begin
                                 C2ErrSize:=294;
                                 BlockErrSize:=1;
                                 PadSize:=PadSize+1;
                            End;
                       End
                  End;
             End;
          2: Begin //Mode 1.
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  //SubHdr will be zero anyway so this is commented..
                  //If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                  //   SubHdr:=0
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                  Begin
                       EDCECCSize:=280;
                       PadSize:=PadSize+8;
                  End;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                     UserDataSize:=2048;
                  If (ReadFormatFlags And 6)<>6 Then
                  Begin
                       If (ReadFormatFlags And MMC_READCD_C2_PTR)<>0 Then
                       Begin
                            C2ErrSize:=294;
                       End
                       Else
                       Begin
                            If (ReadFormatFlags And MMC_READCD_C2_PTR_BLK)<>0 Then
                            Begin
                                 C2ErrSize:=294;
                                 BlockErrSize:=1;
                                 PadSize:=PadSize+1;
                            End;
                       End
                  End;
             End;
          3: Begin //Mode 2 formless
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  //SubHdr will be zero anyway so this is commented..
                  //If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                  //   SubHdr:=0
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                     EDCECCSize:=0;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                     UserDataSize:=2336;
                  If (ReadFormatFlags And 6)<>6 Then
                  Begin
                       If (ReadFormatFlags And MMC_READCD_C2_PTR)<>0 Then
                       Begin
                            C2ErrSize:=294;
                       End
                       Else
                       Begin
                            If (ReadFormatFlags And MMC_READCD_C2_PTR_BLK)<>0 Then
                            Begin
                                 C2ErrSize:=294;
                                 BlockErrSize:=1;
                                 PadSize:=PadSize+1;
                            End;
                       End
                  End;
             End;
          4: Begin //Mode 2 form 1.
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                     SubHdrSize:=8;
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                     EDCECCSize:=280;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                     UserDataSize:=2048;
                  If (ReadFormatFlags And 6)<>6 Then
                  Begin
                       If (ReadFormatFlags And MMC_READCD_C2_PTR)<>0 Then
                       Begin
                            C2ErrSize:=294;
                       End
                       Else
                       Begin
                            If (ReadFormatFlags And MMC_READCD_C2_PTR_BLK)<>0 Then
                            Begin
                                 C2ErrSize:=294;
                                 BlockErrSize:=1;
                                 PadSize:=PadSize+1;
                            End;
                       End
                  End;
             End;
          5: Begin //Mode 2 form 2.
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                     SubHdrSize:=8;
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                     EDCECCSize:=0;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                  Begin
                       UserDataSize:=2324;
                       PadSize:=PadSize+4; //These 4 bytes could optionally be
                                           //used for CRC (EDC), but with most
                                           //CDs these are filled with zeroes.
                  End;
                  If (ReadFormatFlags And 6)<>6 Then
                  Begin
                       If (ReadFormatFlags And MMC_READCD_C2_PTR)<>0 Then
                       Begin
                            C2ErrSize:=294;
                       End
                       Else
                       Begin
                            If (ReadFormatFlags And MMC_READCD_C2_PTR_BLK)<>0 Then
                            Begin
                                 C2ErrSize:=294;
                                 BlockErrSize:=1;
                                 PadSize:=PadSize+1;
                            End;
                       End
                  End;
             End
     Else Begin
               //Reserved filter types.
          End;
     End;

     BlockSize:=SyncSize+
                HdrSize+
                SubHdrSize+
                EDCECCSize+
                UserDataSize+
                C2ErrSize+
                BlockErrSize+
                PadSize;
     GetFullBlockSizeFromFilterReadFormat:=BlockSize;
End;

Function T_MMC1.GetBlockSizeFromFilterReadFormat(SectTypeFilter  : Byte;
                                                 ReadFormatFlags : Byte) : Word;
{ ***********************************************************************
  Calculates main sector block size from Read CD sector type filter and
  read format flags, but excludes the C2 error field flag.
  *********************************************************************** }
Var BlockSize           : Word;
    SyncSize            : Byte;
    HdrSize             : Byte; //Block address+mode header size.
    SubHdrSize          : Byte; //Mode 2 subheader size.
    EDCECCSize          : Word; //CRC, P and Q symbol size.
    UserDataSize        : Word;
    BlockErrSize        : Byte;
    PadSize             : Byte; //Accumulative padding size.
Begin
     SyncSize:=0;
     HdrSize:=0;
     SubHdrSize:=0;
     EDCECCSize:=0;
     UserDataSize:=0;
     BlockErrSize:=0;
     PadSize:=0; //Accumulative pad size.

     Case SectTypeFilter Of
          0: Begin //Any format.
                  UserDataSize:=2352;
             End;
          1: Begin //CDDA.
                  UserDataSize:=2352;
             End;
          2: Begin //Mode 1.
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  //SubHdr will be zero anyway so this is commented..
                  //If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                  //   SubHdr:=0
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                  Begin
                       EDCECCSize:=280;
                       PadSize:=PadSize+8;
                  End;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                     UserDataSize:=2048;
             End;
          3: Begin //Mode 2 formless
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  //SubHdr will be zero anyway so this is commented..
                  //If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                  //   SubHdr:=0
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                     EDCECCSize:=0;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                     UserDataSize:=2336;
             End;
          4: Begin //Mode 2 form 1.
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                     SubHdrSize:=8;
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                     EDCECCSize:=280;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                     UserDataSize:=2048;
             End;
          5: Begin //Mode 2 form 2.
                  If (ReadFormatFlags And MMC_READCD_SYNC)<>0 Then
                     SyncSize:=12;
                  If (ReadFormatFlags And MMC_READCD_HDR)<>0 Then
                     HdrSize:=4;
                  If (ReadFormatFlags And MMC_READCD_SUBHDR)<>0 Then
                     SubHdrSize:=8;
                  If (ReadFormatFlags And MMC_READCD_EDCECC)<>0 Then
                     EDCECCSize:=0;
                  If (ReadFormatFlags And MMC_READCD_USERDATA)<>0 Then
                  Begin
                       UserDataSize:=2324;
                       PadSize:=PadSize+4; //These 4 bytes could optionally be
                                           //used for CRC (EDC), but with most
                                           //CDs these are filled with zeroes.
                  End;
             End
     Else Begin
               //Reserved filter types.
          End;
     End;

     BlockSize:=SyncSize+
                HdrSize+
                SubHdrSize+
                EDCECCSize+
                UserDataSize+
                BlockErrSize+
                PadSize;
     GetBlockSizeFromFilterReadFormat:=BlockSize;
End;

Function T_MMC1.GetBlockSizeFromC2ErrorField(ReadFormatFlags : Byte) : Word;
{ **************************************************************
  Calculates block size from Read CD C2 error field definition
  flag within the read format flags byte.
  ************************************************************** }
Var BlockSize           : Word;
    C2ErrSize           : Word;
    BlockErrSize        : Byte;
    PadSize             : Byte; //Accumulative padding size.
Begin
     C2ErrSize:=0;
     BlockErrSize:=0;
     PadSize:=0; //Accumulative pad size.

     If (ReadFormatFlags And 6)<>6 Then
     Begin
          If (ReadFormatFlags And MMC_READCD_C2_PTR)<>0 Then
          Begin
               C2ErrSize:=294;
          End
          Else
          Begin
               If (ReadFormatFlags And MMC_READCD_C2_PTR_BLK)<>0 Then
               Begin
                    C2ErrSize:=294;
                    BlockErrSize:=1;
                    PadSize:=PadSize+1;
               End;
          End;
     End;

     BlockSize:=C2ErrSize+
                BlockErrSize+
                PadSize;
     GetBlockSizeFromC2ErrorField:=BlockSize;
End;

Function T_MMC1.GetBlockSizeFromSubCh(SubChSel : Byte) : Word;
{ ******************************************
  Calculates block size of sub-channel data.
  ******************************************}
Var BlockSize : Word;
Begin
     Case SubChSel Of
          MMC_READCD_SUBCH_RAWPW: //Raw interlaced sub-channel data.
          Begin
               BlockSize:=96;
          End;
          MMC_READCD_SUBCH_FORMPQ: //Only P and Q sub-channel data.
          Begin                    //12 bytes of Q and 4 bytes of P.
               BlockSize:=16
          End;
          MMC_READCD_SUBCH_PACKPW: //Raw de-interlaced sub-channel data.
          Begin
               BlockSize:=96
          End
          Else
              BlockSize:=0;
     End;

     GetBlockSizeFromSubCh:=BlockSize;
End;

Procedure T_MMC1.Fill_CDB10_do_read_rec_cap(Var Out_CDB : T_readCD_rec_cap);
{ ********************************************************
  Fills CDB with read recorded capacity (CDB10) command
  values.

  Note: on some CDROM/CD writer drives they will return
  FFFFh and SRB status OK.  It should have returned a
  SRB status error!!
  ******************************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_READCD_REC_CAP;
     Out_CDB.RelAdr:=0;
     Out_CDB.MMCLBA_HiByte:=0;
     Out_CDB.MMCLBA_HiMiByte:=0;
     Out_CDB.MMCLBA_LoMiByte:=0;
     Out_CDB.MMCLBA_LoByte:=0;
     Word((@(Out_CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve2.
     Out_CDB.PMI:=0;
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_read_subch(Var Out_CDB    : T_read_subch;
                                              In_MSF     : Byte;
                                              In_SubQ    : Byte;
                                              In_Params  : Byte;
                                              In_TrackNo : Byte);
{ *******************************************************
  Fills CDB with read sub-channel (CDB10) command values.
  ******************************************************* }
Var
   Buf_len : LongWord;
Begin
     If In_SubQ=MMC_READ_SUBCH_SUBQ Then
     Begin
          Case In_Params Of
               MMC_READ_SUBCH_CURR_POS:
               Begin
                    Buf_len:=SizeOf(T_read_subch_CD_curr_pos_data);
               End;
               MMC_READ_SUBCH_MCN:
               Begin
                    Buf_len:=SizeOf(T_read_subch_MCN_data);
               End;
               MMC_READ_SUBCH_ISRC:
               Begin
                    Buf_len:=SizeOf(T_read_subch_ISRC_data);
               End;
          End;
     End;

     Out_CDB.Cmd:=MMC_CMD_READ_SUBCH;
     Out_CDB.MSF:=In_MSF;
     Out_CDB.SubQ:=In_SubQ;
     Out_CDB.Params:=In_Params;
     Word((@(Out_CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve2.
     Out_CDB.TrackNo:=In_TrackNo;
     SPC.ReverseWordToBytes(Buf_Len,
                            Out_CDB.Len_HiByte,
                            Out_CDB.Len_LoByte);
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_read_T_P_A(Var Out_CDB      : T_read_T_P_A;
                                              In_MSF       : Byte;
                                              In_format    : Byte;
                                              In_trkSessNo : Byte);
{ ********************************************************
  Fills CDB with read TOC/PMA/ATIP (CDB10) command values.
  This is a 3 in 1 command.
  ******************************************************** }
Var
   Buf_len : LongWord;
Begin
     Case In_format Of
          MMC_READ_T_P_A_FORMAT_TOC:
          Begin
               //Specs don't allow to determine buffer size, so using arbitrary size
               Buf_len:=USER_MEM_UNIT_SMALLMIN;
          End;
          MMC_READ_T_P_A_FORMAT_SESS_INFO:
          Begin
               //Specific size is known
               Buf_len:=SizeOf(T_read_T_P_A_sess_info_data);
          End;
          MMC_READ_T_P_A_FORMAT_FULL_TOC:
          Begin
               //Specs don't allow to determine buffer size, so using arbitrary size
               Buf_len:=USER_MEM_UNIT_SMALLMIN;
          End;
          MMC_READ_T_P_A_FORMAT_PMA:
          Begin
               //Specs don't allow to determine buffer size, so using arbitrary size
               Buf_len:=USER_MEM_UNIT_SMALLMIN;
          End;
          MMC_READ_T_P_A_FORMAT_ATIP:
          Begin
               Buf_len:=SizeOf(T_read_T_P_A_ATIP_data);
          End
     Else
         //Unknown format, so using arbitrary size
         Buf_len:=USER_MEM_UNIT_SMALLMIN;
     End;

     Out_CDB.Cmd:=MMC_CMD_READ_TOC_PMA_ATIP;
     Out_CDB.MSF:=In_MSF;
     Out_CDB.Format:=In_format;
     Word((@(Out_CDB.Reserved1))^):=0; //Quick zero fill Reserve1 To Reserve2.
     Out_CDB.Reserved3:=0;
     Out_CDB.TrkSessNo:=In_trkSessNo;
     SPC.ReverseWordToBytes(Buf_len,
                            Out_CDB.Len_HiByte,
                            Out_CDB.Len_LoByte);
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_read_header(Var Out_CDB       : T_read_header;
                                               In_MSF        : Byte;
                                               In_MMCLBA_MSF : Pointer);
{ ************************************************************************
  Fills CDB with read header (CDB10) command values using MSF or MMCLBA
  option.
  ************************************************************************ }
Var
   Buf_len : LongWord;
Begin
     Out_CDB.Cmd:=MMC_CMD_READ_HEADER;
     Out_CDB.MSF:=In_MSF;
     Case In_MSF Of
     MMC_READ_HEADER_MMCLBA:
     Begin
          SPC.ReverseLongIntToBytes(LongInt(In_MMCLBA_MSF^),
                                    Out_CDB.StartMMCLBA_HiByte,
                                    Out_CDB.StartMMCLBA_HiMiByte,
                                    Out_CDB.StartMMCLBA_LoMiByte,
                                    Out_CDB.StartMMCLBA_LoByte);
     End;
     MMC_READ_HEADER_MSF:
     Begin
          Out_CDB.StartMMCLBA_HiByte:=0;
          Out_CDB.StartMMCLBA_HiMiByte:=T_MSF(In_MMCLBA_MSF^).M;
          Out_CDB.StartMMCLBA_LoMiByte:=T_MSF(In_MMCLBA_MSF^).S;
          Out_CDB.StartMMCLBA_LoByte:=T_MSF(In_MMCLBA_MSF^).F;
     End;
     End;
     Out_CDB.Reserved1:=0;
     Case In_MSF Of
          MMC_READ_HEADER_MMCLBA:
          Begin
               Buf_len:=SizeOf(T_read_header_MMCLBA_data);
          End;
          MMC_READ_HEADER_MSF:
          Begin
               Buf_len:=SizeOf(T_read_header_MSF_data);
          End;
     End;
     SPC.ReverseWordToBytes(Buf_len,
                            Out_CDB.Len_HiByte,
                            Out_CDB.Len_LoByte);
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_read_header_MMCLBA(Var Out_CDB   : T_read_header;
                                                      In_MMCLBA : LongInt);
{ **********************************************************************
  Fills CDB with read header (CDB10) command values using MMCLBA option.
  ********************************************************************** }
Begin
     Fill_CDB10_do_read_header(Out_CDB,
                               MMC_READ_HEADER_MMCLBA,
                               @In_MMCLBA);
End;

Procedure T_MMC1.Fill_CDB10_do_read_header_MSF(Var Out_CDB : T_read_header;
                                                   In_MSF  : T_MSF);
{ *******************************************************************
  Fills CDB with read header (CDB10) command values using MSF option.
  ******************************************************************* }
Begin
     Fill_CDB10_do_read_header(Out_CDB,
                               MMC_READ_HEADER_MSF,
                               @In_MSF);
End;

Procedure T_MMC1.Fill_CDB12_do_readCD_MSF_byFormat(Var Out_CDB            : T_readCD_MSF;
                                                   Var In_StartMSF        : T_MSF;
                                                   Var In_EndMSF          : T_MSF;
                                                       In_SectTypeFilter  : Byte;
                                                       In_ReadFormatFlags : Byte;
                                                       In_SubChSel        : Byte);
{ **************************************************
  Fills CDB with read CD MSF (CDB12) command values.
  ************************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_READCD_MSF;
     Out_CDB.SectType:=In_SectTypeFilter;
     Out_CDB.Reserved1:=0;
     Out_CDB.StartM:=In_StartMSF.M;
     Out_CDB.StartS:=In_StartMSF.S;
     Out_CDB.StartF:=In_StartMSF.F;
     Out_CDB.EndM:=In_EndMSF.M;
     Out_CDB.EndS:=In_EndMSF.S;
     Out_CDB.EndF:=In_EndMSF.F;
     Out_CDB.Format_flags:=In_ReadFormatFlags;
     Out_CDB.SubCh_sel:=In_SubChSel;
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB12_do_set_CD_speed(Var Out_CDB       : T_set_CD_speed;
                                                In_ReadSpeed  : Word;
                                                In_WriteSpeed : Word);
{ **************************************************
  Fill CDB with set CD speed (CDB12) command values.
  ************************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_SET_CD_SPEED;
     Out_CDB.Reserved1:=0;
     SPC.ReverseWordToBytes(In_ReadSpeed,
                            Out_CDB.ReadSpeed_HiByte,
                            Out_CDB.ReadSpeed_LoByte);
     SPC.ReverseWordToBytes(In_WriteSpeed,
                            Out_CDB.WriteSpeed_HiByte,
                            Out_CDB.WriteSpeed_LoByte);
     LongWord((@(Out_CDB.Reserved2))^):=0; //Quick zero fill Reserve2 To Reserve5.
     Out_CDB.Reserved6:=0;
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB12_do_readCD_byFormat(Var Out_CDB            :T_readCD_CDB12;
                                                   In_Start_MMCLBA    : LongInt;
                                                   In_N_sectors       : LongWord;
                                                   In_SectTypeFilter  : Byte;
                                                   In_ReadFormatFlags : Byte;
                                                   In_SubChSel        : Byte);
{ ***************************************************
  Fills CDB with read CD MMCLBA CDB12 command values.
  *************************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_READCD_MMCLBA;
     Out_CDB.SectType_RelAdr:=In_SectTypeFilter;
     SPC.ReverseLongIntToBytes(In_Start_MMCLBA,
                               Out_CDB.StartMMCLBA_HiByte,
                               Out_CDB.StartMMCLBA_HiMiByte,
                               Out_CDB.StartMMCLBA_LoMiByte,
                               Out_CDB.StartMMCLBA_LoByte);
     SPC.ReverseLongWordToBytes(In_N_sectors,
                                Out_CDB.Len_HiByte,
                                Out_CDB.Len_MiByte,
                                Out_CDB.Len_LoByte);
     Out_CDB.Format_flags:=In_ReadFormatFlags;
     Out_CDB.SubCh_sel:=In_SubChSel;
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_read_buf_cap(Var Out_CDB : T_read_buf_cap_CDB10);
{ *********************************************************
  Fills CDB with read buffer capacity CDB10 command values.
  ********************************************************* }
Begin
     Out_CDB.Cmd:=MMC_CMD_READ_BUF_CAP;
     LongWord((@(Out_CDB.Reserved1))^):=0;
     Word((@(Out_CDB.Reserved5))^):=0;
     SPC.ReverseWordToBytes(SizeOf(T_read_buf_cap_CDB10_block),
                            Out_CDB.AllocLen_HiByte,
                            Out_CDB.AllocLen_LoByte);
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_read_disc_info(Var Out_CDB : T_read_disc_info_CDB10; In_AllocLen : Word);
{ ***************************************************
  Fills CDB with read disc info CDB10 command values.
  *************************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_READ_DISC_INFO;
     LongWord((@(Out_CDB.Reserved1))^):=0;
     Word((@(Out_CDB.Reserved5))^):=0;
     SPC.ReverseWordToBytes(In_AllocLen,
                            Out_CDB.AllocLen_HiByte,
                            Out_CDB.AllocLen_LoByte);
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_read_track_info(Var Out_CDB         : T_read_track_info_CDB10;
                                                   In_TrkBIT       : Byte;
                                                   In_MMCLBA_TrkNo : LongInt);
{ ****************************************************
  Fills CDB with read track info CDB10 command values.
  **************************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_READ_TRACK_INFO;
     Out_CDB.TrkBIT:=In_TrkBIT;
     SPC.ReverseLongIntToBytes(In_MMCLBA_TrkNo,
                               Out_CDB.MMCLBA_TrkNo_HiByte,
                               Out_CDB.MMCLBA_TrkNo_HiMiByte,
                               Out_CDB.MMCLBA_TrkNo_LoMiByte,
                               Out_CDB.MMCLBA_TrkNo_LoByte);
     Out_CDB.Reserved:=0;
     SPC.ReverseWordToBytes(SizeOf(T_read_track_info_CDB10_block),
                            Out_CDB.AllocLen_HiByte,
                            Out_CDB.AllocLen_LoByte);
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_reserve_track(Var Out_CDB : T_reserve_track_CDB10;
                                             In_Reverv_size : LongWord);
{ ****************************************************
  Fills CDB with reserve track CDB10 command values.
  **************************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_RESERVE_TRACK;
     Out_CDB.Reserve1:=0;
     Out_CDB.Reserve2:=0;
     Out_CDB.Reserve3:=0;
     Out_CDB.Reserve4:=0;
     SPC.ReverseLongWordToBytes(In_Reverv_size,
                                Out_CDB.Reserve_size_HiByte,
                                Out_CDB.Reserve_size_HiMiByte,
                                Out_CDB.Reserve_size_LoMiByte,
                                Out_CDB.Reserve_size_LoByte);
     Out_CDB.Control:=0;
End;


Procedure T_MMC1.Fill_CDB10_do_synchronize_cache(Var Out_CDB     : T_synchronize_cache_CDB10;
                                                     In_MMCLBA   : LongInt;
                                                     In_N_blocks : Word);
{ *********************************************************
  Fills CDB with synchronize cache CDB10 command values.
  This is for writing any remaining data in cache to CDR/W.
  ********************************************************* }
Begin
     Out_CDB.Cmd:=MMC_CMD_SYNC_CACHE;
     Out_CDB.Immed_RELADR:=0;
     SPC.ReverseLongIntToBytes(In_MMCLBA,
                               Out_CDB.MMCLBA_HiByte,
                               Out_CDB.MMCLBA_HiMiByte,
                               Out_CDB.MMCLBA_LoMiByte,
                               Out_CDB.MMCLBA_LoByte);
     Out_CDB.Reserved:=0;
     SPC.ReverseWordToBytes(In_N_blocks,
                            Out_CDB.Len_HiByte,
                            Out_CDB.Len_LoByte);
     Out_CDB.Control:=0;
End;

Procedure T_MMC1.Fill_CDB10_do_write(Var Out_CDB           : T_write_CDB10;
                                         In_DPO_FUA_RELADR : Byte;
                                         In_MMCLBA         : LongInt;
                                         In_N_blocks       : Word;
                                         In_block_size     : Word);
{ ******************************************
  Fills CDB with write CDB10 command values.
  ****************************************** }
Begin
     Out_CDB.Cmd:=MMC_CMD_WRITE_CDB10;
     Out_CDB.DPO_FUA_RELADR:=In_DPO_FUA_RELADR;
     SPC.ReverseLongIntToBytes(In_MMCLBA,
                               Out_CDB.MMCLBA_HiByte,
                               Out_CDB.MMCLBA_HiMiByte,
                               Out_CDB.MMCLBA_LoMiByte,
                               Out_CDB.MMCLBA_LoByte);
     Out_CDB.Reserved:=0;
     SPC.ReverseWordToBytes(In_N_blocks,
                            Out_CDB.Len_HiByte,
                            Out_CDB.Len_LoByte);
     Out_CDB.Control:=0;
End;

Function T_MMC1.Calc_modepage6_ptr(In_P_Buf : Pointer) : Pointer;
{ **************************************************
  Calculates the address where the mode page begins.
  A pointer with the address is returned.
  This uses the recent mode sense 6 call.
  Becareful when using this function, it uses the
  data in the transfer buffer for finding the
  address - so the buffer should have valid sense
  data.
  ************************************************** }
Begin
     Calc_modepage6_ptr:=Ptr(Integer(In_P_Buf)+
                             SizeOf(T_mode_param_hdr_6)+
                             T_mode_param_hdr_6(In_P_Buf^).Block_desc_len);
End;

Function T_MMC1.Calc_modepage10_ptr(In_P_Buf : Pointer) : Pointer;
{ **************************************************
  Calculates the address where the mode page begins.
  A pointer with the address is returned.
  This uses the recent mode sense 10 call.
  Becareful when using this function, it uses the
  data in the transfer buffer for finding the
  address - so the buffer should have a valid sense
  data.
  ************************************************** }
Begin
     Calc_modepage10_ptr:=Ptr(Integer(In_P_Buf)+
                              SizeOf(T_mode_param_hdr_10)+
                              SPC.ReverseBytesToWord(T_mode_param_hdr_10(In_P_Buf^).Block_desc_len_MSB,
                                                     T_mode_param_hdr_10(In_P_Buf^).Block_desc_len_LSB));
End;

Procedure T_MMC1.Fill_mode_param_hdr_6(In_P_Buf : Pointer);
{ *******************************************************
  Fills the mode parameters header for select/sense CDB6.
  ******************************************************* }
Begin
     With T_mode_param_hdr_6(In_P_Buf^) Do
     Begin
          Mode_data_len:=0;
          Medium_type:=MMC_MODE_PARAM_CDM_DEFAULT;
          Dev_specific_param:=0;
          Block_desc_len:=SizeOf(T_mode_param_block_desc);
     End;
End;

Procedure T_MMC1.Fill_mode_param_hdr_10(In_P_Buf : Pointer);
{ ********************************************************
  Fills the mode parameters header for select/sense CDB10.
  ******************************************************** }
Begin
     With T_mode_param_hdr_10(In_P_Buf^) Do
     Begin
          Word((@Mode_data_len_MSB)^):=0; //Quick zero fill Mode_data_len_MSB
                                          //and Mode_data_len_LSB.
          Medium_type:=MMC_MODE_PARAM_CDM_DEFAULT;
          Dev_specific_param:=0;
          Word((@Reserved1)^):=0; //Quick zero fill Reserved1 to Reserved2.
          SPC.ReverseWordToBytes(SizeOf(T_mode_param_block_desc),
                                 Block_desc_len_MSB,
                                 Block_desc_len_LSB);
     End;
End;

Procedure T_MMC1.Fill_block_desc(P_block_desc : Pointer);
{ ***************************
  Fills the block descriptor.
  *************************** }
Begin
     With T_mode_param_block_desc(P_block_desc^) Do
     Begin
          Density_code:=0;
          N_blocks_MSB:=0;
          N_blocks_MidByte:=0;
          N_blocks_LSB:=0;
          Reserved1:=0;
          Block_len_MSB:=0;
          //Block_len_MidByte:=8; { = 2048 sector size }
          Block_len_MidByte:=0;
          Block_len_LSB:=0;
     End;
End;

Procedure T_MMC1.Fill_mode_pg_read_err_rec(    P_read_err_rec      : Pointer;
                                           In_write_params_page_len : byte;
                                           Var In_err_rec_param    : Byte;
                                           Var In_read_retry_count : Byte;
                                               Set_all             : Boolean);
{ ***************************************************
  Fills the read error recovery parameters mode page.
  *************************************************** }
Begin
     With T_mode_pg_read_err_rec(P_read_err_rec^) Do
     Begin
          If Set_all Then
          Begin
               PS_PageCode:=MMC_MODE_PAGE_READ_ERR_REC;
               //Param_len:=SizeOf(T_mode_pg_read_err_rec)-2;
               Param_len:=In_write_params_page_len;
               LongWord((@Reserved1)^):=0; //Quick zero fill Reserved1 to Reserved4.
          End;
          Err_rec_param:=In_err_rec_param;
          Read_retry_count:=In_read_retry_count;
     End;
End;

Procedure T_MMC1.Fill_mode_pg_write(    P_read_err_rec  : Pointer;
                                        In_write_params_page_len : byte;
                                    Var In_write_params : T_pub_write_params;
                                        Set_all         : Boolean);
{ *************************************
  Fills the write parameters mode page.
  ************************************* }
Var i : Byte;
Begin
     With T_mode_pg_write(P_read_err_rec^) Do
     Begin
          If Set_all Then
          Begin
               PS_PageCode:=MMC_MODE_PAGE_WRITE_PARAM;
               //Param_len:=SizeOf(T_mode_pg_write)-2;
               Param_len:=In_write_params_page_len;

               Word((@Reserved1)^):=0; //Quick zero fill Reserved1 adn Reserved2.

               Reserved3:=0;

               LongWord((@SubHdr0)^):=0; //Quick zero fill SubHdr0 to SubHdr3.
               LongWord((@VendorSpec1)^):=0; //Quick zero fill VendorSpec1 to VendorSpec4.
          End;
          TestWR_WRType:=In_write_params.TestWR_WRType;
          MultiS_FP_Copy_TrkMode:=In_write_params.MultiS_FP_Copy_TrkMode;
          DataBlockType:=In_write_params.DataBlockType;
          Word((@Reserved1)^):=0; //Quick zero fill Reserved1 to Reserved2.
          HostAppCode:=In_write_params.HostAppCode;
          SessFormat:=In_write_params.SessFormat;
          SPC.ReverseLongWordToBytes(In_write_params.PacketSize,
                                     PacketSize_MSB,
                                     PacketSize_HiMiByte,
                                     PacketSize_LoMiByte,
                                     PacketSize_LSB);
          SPC.ReverseWordToBytes(In_write_params.AudioPauseLen,
                                 AudioPauseLen_MSB,
                                 AudioPauseLen_LSB);
          For i:=1 To 16 Do
          Begin
               MCN[i]:=In_write_params.MCN[i];
               ISRC[i]:=In_write_params.ISRC[i];
          End;
     End;
End;

Procedure T_MMC1.Fill_mode_pg_veri_err_rec(    P_veri_err_rec      : Pointer;
                                           In_write_params_page_len : byte;
                                           Var In_err_rec_param    : Byte;
                                           Var In_veri_retry_count : Byte;
                                               Set_all             : Boolean);
{ *****************************************************
  Fills the verify error recovery parameters mode page.
  ***************************************************** }
Begin
     With T_mode_pg_veri_err_rec(P_veri_err_rec^) Do
     Begin
          If Set_all Then
          Begin
               PS_PageCode:=MMC_MODE_PAGE_VERI_ERR_REC;
               //Param_len:=SizeOf(T_mode_pg_veri_err_rec)-2;
               Param_len:=In_write_params_page_len;
               LongWord((@Reserved1)^):=0; //Quick zero fill Reserved1 to Reserved4.
          End;
          Err_rec_param:=In_err_rec_param;
          Veri_retry_count:=In_veri_retry_count;
     End;
End;

Procedure T_MMC1.Fill_mode_pg_CD(    P_CD              : Pointer;
                                 In_write_params_page_len : byte;
                                 Var In_inactivity_mul : Byte;
                                 Var In_sec_per_MSF    : Word;
                                 Var In_frames_per_MSF : Word;
                                     Set_all           : Boolean);
{ **********************************
  Fills the CD parameters mode page.
  ********************************** }
Begin
     With T_mode_pg_CD(P_CD^) Do
     Begin
          If Set_all Then
          Begin
               PS_PageCode:=MMC_MODE_PAGE_CD;
               //Param_len:=SizeOf(T_mode_pg_CD)-2;
               Param_len:=In_write_params_page_len;
               Reserved1:=0;
          End;
          Inactivity_mul:=In_inactivity_mul;
          SPC.ReverseWordToBytes(In_sec_per_MSF,
                                 Secs_per_MSF_MSB,
                                 Secs_per_MSF_LSB);
          SPC.ReverseWordToBytes(In_frames_per_MSF,
                                 Frames_per_MSF_MSB,
                                 Frames_per_MSF_LSB);
     End;
End;

Procedure T_MMC1.Fill_mode_pg_CD_audio_ctrl(    P_CD_audio_ctrl  : Pointer;
                                            In_write_params_page_len : byte;
                                            Var In_CD_audio_ctrl : T_pub_CD_audio_ctrl;
                                                Set_all          : Boolean);
{ ************************************************
  Fills the CD audio control parameters mode page.
  ************************************************ }
Begin
     With T_mode_pg_CD_audio_ctrl(P_CD_audio_ctrl^) Do
     Begin
          If Set_all Then
          Begin
               PS_PageCode:=MMC_MODE_PAGE_CD_AUDIO_CTRL;
               //Param_len:=SizeOf(T_mode_pg_CD_audio_ctrl)-2;
               Param_len:=In_write_params_page_len;

               Word((@Reserved1)^):=0; //Quick zero fill Reserved1 To Reserved2.
               Word((@Undefined1)^):=0; //Quick zero fill Undefined1 To Undefined2.
               Undefined3:=0;
          End;
          Immed_SOTC:=In_CD_audio_ctrl.Immed_SOTC;
          Port0_sel:=In_CD_audio_ctrl.Port0_sel;
          Port0_vol:=In_CD_audio_ctrl.Port0_vol;
          Port1_sel:=In_CD_audio_ctrl.Port1_sel;
          Port1_vol:=In_CD_audio_ctrl.Port1_vol;
          Port2_sel:=In_CD_audio_ctrl.Port2_sel;
          Port2_vol:=In_CD_audio_ctrl.Port2_vol;
          Port3_sel:=In_CD_audio_ctrl.Port3_sel;
          Port3_vol:=In_CD_audio_ctrl.Port3_vol;
     End;
End;

Procedure T_MMC1.Fill_out_CD_cap_mech_st(    P_CD_cap_mech_st   : Pointer;
                                         Var Out_CD_cap_mech_st : T_pub_CD_cap_mech_st);
Begin
     With T_mode_pg_CD_cap_mech_st(P_CD_cap_mech_st^) Do
     Begin
          Out_CD_cap_mech_st.Param_len:=Param_len;
          Out_CD_cap_mech_st.Meth2_RDCDRW_RDCDRR:=Meth2_RDCDRW_RDCDRR;
          Out_CD_cap_mech_st.TestWR_WRCDRW_WRCDR:=TestWR_WRCDRW_WRCDR;
          Out_CD_cap_mech_st.MultiS_M2F2_M2F1_P2_P1_Comp_AudPly:=MultiS_M2F2_M2F1_P2_P1_Comp_AudPly;
          Out_CD_cap_mech_st.BarC_UPC_ISRC_C2_RWPack_RWRaw_GoodCDDA_CDDASup:=BarC_UPC_ISRC_C2_RWPack_RWRaw_GoodCDDA_CDDASup;
          Out_CD_cap_mech_st.LoadMechT_Eject_PreJmp_LockSt_Lock:=LoadMechT_Eject_PreJmp_LockSt_Lock;
          Out_CD_cap_mech_st.SSS_DiskIn_SepChMute_SepVol:=SSS_DiskIn_SepChMute_SepVol;
          Out_CD_cap_mech_st.MaxReadSpeed:=SPC.ReverseBytesToWord(MaxReadSpeed_MSB,
                                                                  MaxReadSpeed_LSB);
          Out_CD_cap_mech_st.NVolLevels:=SPC.ReverseBytesToWord(NVolLevels_MSB,
                                                                NVolLevels_LSB);
          Out_CD_cap_mech_st.BufSize:=SPC.ReverseBytesToWord(BufSize_MSB,
                                                             BufSize_LSB);
          Out_CD_cap_mech_st.CurrReadSpeed:=SPC.ReverseBytesToWord(CurrReadSpeed_MSB,
                                                                   CurrReadSpeed_LSB);
          Out_CD_cap_mech_st.Len_LSBF_RCK_BCK:=Len_LSBF_RCK_BCK;

          If Param_len<20 Then
          Begin
               Out_CD_cap_mech_st.MaxWriteSpeed:=0;
               Out_CD_cap_mech_st.CurrWriteSpeed:=0;
          End
          Else
          Begin
               Out_CD_cap_mech_st.MaxWriteSpeed:=SPC.ReverseBytesToWord(MaxWriteSpeed_MSB,
                                                                        MaxWriteSpeed_LSB);
               Out_CD_cap_mech_st.CurrWriteSpeed:=SPC.ReverseBytesToWord(CurrWriteSpeed_MSB,
                                                                         CurrWriteSpeed_LSB);
          End;
     End;
End;

end.
