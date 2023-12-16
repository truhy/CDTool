{ Name:          WNASPI32Unit.PAS
  File type:     Borland Delphi 4 unit file.
  Description:   Contains a class which encapsulates the main Win ASPI32
                 functions.  This source file uses information
                 given from the Adaptec's ASPI SDK for Win32 (July 1, 1998).
  Assumptions:   Win ASPI32 and SCSI commands will use
                 Cur_HA_ID, Cur_Dev_ID, Cur_LUN, Buf.P_Buf.
                 Methods in here will not modify Cur_HA_ID, Cur_Dev_ID
                 and Cur_LUN, but the public may modify them.
                 Buf.Buf_len will always be the transfer buffer size.
                 Size of SRB attribute is fixed and is the largest SRB size.
  Notes:         Written from scratch.
  Date started:  12th Jan 2000.
  Date finished: 
  File version:  v1.5 final
  Developer:     Truong Hy.
  Notes:         Using this class
                 ----------------
                 When you instantiate an object from this class it attempts to
                 load the WNASPI32.DLL driver and if successful it will call
                 the GetASPI32SupportInfo function from it.  This is required
                 to be called at least once to initialise the Win ASPI32 driver.
                 The property OpenASPIStatus is used for indicating success or
                 failure of the above mentioned tasks.

                 Friendly/unfriendly method versions
                 -----------------------------------
                 Unfriendly methods are methods following as close as possible the format
                 calling convention and the format types as described in the MMC specs and
                 will leave the data in the SRB memory area.

                 Friendly methods are methods which I have custom made myself and will in most
                 cases format the data into parameters and variables of more suitable and
                 friendly to use types.

                 Buffers
                 -------
                 SRB buffer is an array and it is not allocated dynamically
                 - I suppose it doesn't need to be.

                 Buf.P_Buf is allocated dynamically in memory.  It is for large data transfers
                 to and from the hardware device.  WinASPI32 gives you GetASPI32Buffer
                 for allocating large (well larger than 64K) dynamic memory.
                 If the OS supports allocating of suitable sized dynamic memory then the
                 GetASPI32Buffer function need not be used.

                 More
                 ----
                 For more notes (if you're not bored yet) read the comments in
                 the source :).
}

unit WNASPI32Unit;

interface

Uses Windows, //For Win 32 API constants & functions.
     SysUtils; //For Delphi string functions, etc.
     //Dialogs; //For debugging only. Allows message boxes. Remove this in final release.

{$I Buf_inc.pas}
{$I WNASPI32_inc.pas} //Include file for declaration of constants and types.

{ Start of WNASPI32 class attributes and methods. }
Type T_WNASPI32=
Class
Private
      { Critical attributes for class use only.. }
      WNASPI32_Buf    : T_ASPI32Buf; //Win ASPI32 data transfer buffer structure(original transfer buffer).
      BufByASPICallOK : Boolean;     //State of using GetASPI32Buffer call.
      A_ASPI32Handle  : Integer;     //Handle to WNASPI32.DLL
      heventSRB       : Integer;     //Handle to event for SRB waiting.

      { Attributes which can be placed in public section.. }
      { Attributes which public may modify (only do so when a method in
        this class is not running!!).. }
      Active_data_buf : T_Buf;        //Data transfer buffer structure for data to/from device.
      SRB             : T_LargestSRB; //Generic SRB.  Also is largest SRB.
      Cur_HA_ID       : Byte;         //Current Host Adapter ID.
      Cur_Dev_ID      : Byte;         //Current Device Target ID.
      Cur_LUN         : Byte;         //Current Logical Unit Num.
      Cur_Res_supp    : Boolean;      //Indicates that residual byte is supported.
      { Attributes which public should really only read - although, they do not
        critically affect this class if they are accidently modified.. }
      OpenASPIStatus : Byte;    //My own status flag.
      Num_HA         : Byte;    //Number of Host Adapters.
      SRB_status     : Byte;    //SRB status returned value.
      SRB_status_OK  : Boolean; //Indicates whether SRB status is OK.
      SRB_err_msg    : String;  //SRB error message for application to optionally display.
      HA_status_OK   : Boolean; //Indicates whether Host Adapter status is OK.
      HA_err_msg     : String;  //Host Adapter error message.

      Found_HA_ID       : Byte;
      Found_Dev_ID      : Byte;
      Found_LUN         : Byte;
      Found_HA_SCSI_ID  : Byte;
      Found_Max_dev     : Byte;
      Found_Max_buf_len : LongWord;
      Found_Res_supp    : Boolean;

      { For importing of WNASPI32.DLL functions.. }
      A_GetASPI32SupportInfo   : Function : LongWord; cdecl;
      A_SendASPI32Command      : Function(P_SRB : Pointer) : LongWord; cdecl;
      A_GetASPI32Buffer        : Function(P_ASPI32Buf : Pointer) : Boolean; cdecl;
      A_FreeASPI32Buffer       : Function(P_ASPI32Buf : Pointer) : Boolean; cdecl;
      A_TranslateASPI32Address : Function(P1 : Pointer; P2 : Pointer) : Boolean; cdecl;

      { Methods for class use only.. }
      Procedure LoadWinASPI32Proc;
      Procedure UnloadWinASPI32Proc;
Public
      //Our own device & sense error messages (part of SPC SCSI spec)..
      Dev_status_OK      : Boolean; //Whether target device status is ok.
      Dev_err_msg        : String;  //Target device error message.
      Std_sense_exists   : Boolean; //Whether non-vendor sense data exists.
      Sense_key_err_msg  : String;  //Sense key error message.
      Sense_code_err_msg : String;  //Sense ASC and ASCQ error message.
      Desc_data_offset   : Word;    //Used for read TOC/PMA/ATIP SCSI command.

      { Constructors, destructors.. }
      Constructor Create;
      Destructor Destroy;

      { Get methods for some private attributes.. }
      Function Original_BufSize : LongWord;
      Function Original_P_Buf : Pointer;
      Function Get_data_buf : Pointer;
      Function Get_data_buf_size : LongWord;
      Function Get_SRB : T_LargestSRB;
      Function Get_Cur_HA_ID : Byte;  //Current Host Adapter ID.
      Function Get_Cur_Dev_ID : Byte;  //Current Device Target ID.
      Function Get_Cur_LUN : Byte;  //Current Logical Unit Num.
      Function Get_Cur_Res_supp : Boolean;  //Indicates that residual byte is supported.
      Function Get_OpenASPIStatus : Byte;
      Function Get_Num_HA         : Byte;  //Number of Host Adapters.
      Function Get_SRB_status     : Byte;  //SRB status returned value.
      Function Get_SRB_status_OK  : Boolean;  //Indicates whether SRB status is OK.
      Function Get_SRB_err_msg    : String;  //SRB error message for application to optionally display.
      Function Get_HA_status_OK   : Boolean;  //Indicates whether Host Adapter status is OK.
      Function Get_HA_err_msg     : String;  //Host Adapter error message.
      { Get methods of results of searching for a device.. }
      Function Get_Found_HA_ID       : Byte;
      Function Get_Found_Dev_ID      : Byte;
      Function Get_Found_LUN         : Byte;
      Function Get_Found_HA_SCSI_ID  : Byte;
      Function Get_Found_Max_dev     : Byte;
      Function Get_Found_Max_buf_len : LongWord;
      Function Get_Found_Res_supp    : Boolean;
      { Get methods to return generic pointers for typecasting.. }
      Function P_SRB : Pointer;
      Function Get_sense_buf : Pointer;

      { Set methods for some private attributes.. }
      Procedure Set_data_buf(in_P_Buf : Pointer);
      Procedure Set_data_buf_size(in_buf_size : LongWord);
      Procedure Set_SRB(in_SRB : T_LargestSRB);
      Procedure Set_Cur_HA_ID(in_HA_ID : Byte);
      Procedure Set_Cur_Dev_ID(in_Dev_ID : Byte);
      Procedure Set_Cur_LUN(in_LUN : Byte);
      Procedure Set_Cur_Res_supp(in_Res_supp : Boolean);
      Procedure Set_SRB_status_OK(in_SRB_status : Boolean);  //Indicates whether SRB status is OK.
      Procedure Set_SRB_err_msg(in_SRB_err_msg: String);
           
      { Methods for calling Win ASPI32 functions safely.. }
      Procedure V_GetASPI32SupportInfo;
      Procedure V_SendASPI32Command;
      Procedure V_SendASPI32Command_Wait;
      { Methods for calling Win ASPI32 get/set timeouts.. }
      Procedure SetGlobalTimeOutSecs(t_secs : LongWord);
      Function GetDeviceTimeOut : LongWord;
      Procedure SetDeviceTimeOutSecs(t_secs : LongWord);
      { Methods relating to memory de/allocations, etc.. }
      Procedure AllocLargestBuffer;
      Procedure AllocNBuffer(In_buf_len : LongWord);
      Procedure AllocNLargestBuffer(In_buf_len : LongWord);
      Procedure AllocNBufferVirtualAlloc(In_buf_len : LongWord);
      Procedure AllocNLargestBufferVirtualAlloc(In_buf_len : LongWord);
      Procedure FreeBuffer;
      Procedure Zero_data_buf;
      { Clear SRB with zeroes }
      Procedure ZeroSRB;
      Procedure MinZeroSRB;
      { Unfriendly versions (data is in SRB)..}
      Procedure Get_HA_inquiry;
      Procedure Get_dev_type;
      Procedure Abort_SRB;
      Procedure Reset_dev;
      Procedure Get_disk_drive_info;
      Procedure Rescan_SCSI_bus;
      { Methods for verification.. }
      Function Verify_SRB_status : Boolean;
      Function Verify_HA_status : Boolean;
      { Methods for calling other Win ASPI32 functions.. }
      { Friendly return versions.. }
      Function Get_HA_inquiry_out : T_HA_inquiry_out;
      Function Get_dev_type_out : Byte;
      { Helper methods for searching CDROM type drives.
        Note: Not Win ASPI 32 functions, they are my own. }
      Function Find_CDROM(Is_first_find : Boolean) : Boolean;
      Function Find_first_CDROM : Boolean;
      Function Find_next_CDROM : Boolean;
      Procedure Set_found_CDROM_active;

      Procedure Fill_SRB_exec_SCSI(In_flags : Byte;
                                   In_buf_len : LongWord;
                                   In_CDB_len : Byte);
End;

implementation

Constructor T_WNASPI32.Create;
Begin
     //Inherited; //Calls the original Delphi constructor.

     { Attribute initialisations (not sure if these are required -
     Delphi might already do these).. }
     OpenASPIStatus:=0;
     A_ASPI32Handle:=0;
     BufByASPICallOK:=False;
     HA_status_OK:=False;
     WNASPI32_Buf.P_Buf:=nil;
     WNASPI32_Buf.Buf_len:=0;
     //WNASPI32_Buf.Zero_fill:=0;
     //WNASPI32_Buf.Reserved:=0;
     Cur_HA_ID:=0;
     Cur_Dev_ID:=0;
     Cur_LUN:=0;
     Cur_Res_supp:=False;
     SRB_err_msg:='';
     HA_err_msg:='';

     Dev_status_OK:=False;
     Dev_err_msg:='';
     Std_sense_exists:=False;
     Sense_key_err_msg:='';
     Sense_code_err_msg:='';

     Active_data_buf.p_buf:=nil;
     Active_data_buf.size:=0;

     Found_HA_ID:=0;
     Found_Dev_ID:=0;
     Found_LUN:=0;
     Found_HA_SCSI_ID:=0;
     Found_Max_dev:=ASPI_DEFAULT_MAX_DEV_ID+1;
     Found_Max_buf_len:=0;
     Found_Res_supp:=False;

     LoadWinASPI32Proc;
End;

Destructor T_WNASPI32.Destroy;
Begin
     FreeBuffer;
     If A_ASPI32Handle<>0 Then
        UnloadWinASPI32Proc;

     //Inherited; //Calls the original Delphi destructor.
End;

Function T_WNASPI32.Original_BufSize;
{ ***************************************************
  Get original transfer buffer allocated memory size.
  *************************************************** }
Begin
     Result:=WNASPI32_Buf.Buf_len;
End;

Function T_WNASPI32.Original_P_Buf : Pointer;
{ ****************************************
  Get pointer to original transfer buffer.
  **************************************** }
Begin
     Result:=WNASPI32_Buf.P_Buf;
End;

Function T_WNASPI32.Get_data_buf : Pointer;
Begin
     Result := Active_data_buf.p_buf;
End;

Function T_WNASPI32.Get_data_buf_size : LongWord;
Begin
     Result := Active_data_buf.size;
End;

Function T_WNASPI32.Get_SRB : T_LargestSRB;
Begin
     Result := SRB;
End;

Function T_WNASPI32.Get_Cur_HA_ID : Byte;
Begin
     Result := Cur_HA_ID;
End;

Function T_WNASPI32.Get_Cur_Dev_ID : Byte;
Begin
     Result := Cur_Dev_ID;
End;

Function T_WNASPI32.Get_Cur_LUN : Byte;
Begin
     Result := Cur_LUN;
End;

Function T_WNASPI32.Get_Cur_Res_supp : Boolean;
Begin
     Result := Cur_Res_supp;
End;

Function T_WNASPI32.Get_OpenASPIStatus : Byte;
Begin
     Result :=OpenASPIStatus;
End;

Function T_WNASPI32.Get_Num_HA : Byte;
Begin
     Result := Num_HA;
End;

Function T_WNASPI32.Get_SRB_status : Byte;
Begin
     Result := SRB_status;
End;

Function T_WNASPI32.Get_SRB_status_OK : Boolean;
Begin
     Result := SRB_status_OK;
End;

Function T_WNASPI32.Get_SRB_err_msg : String;
Begin
     Result := SRB_err_msg;
End;

Function T_WNASPI32.Get_HA_status_OK : Boolean;
Begin
     Result := HA_status_OK;
End;

Function T_WNASPI32.Get_HA_err_msg : String;
Begin
     Result := HA_err_msg;
End;

Function T_WNASPI32.Get_Found_HA_ID : Byte;
Begin
     Result := Found_HA_ID;
End;

Function T_WNASPI32.Get_Found_Dev_ID : Byte;
Begin
     Result := Found_Dev_ID;
End;

Function T_WNASPI32.Get_Found_LUN : Byte;
Begin
     Result := Found_LUN;
End;

Function T_WNASPI32.Get_Found_HA_SCSI_ID : Byte;
Begin
     Result := Found_HA_SCSI_ID;
End;

Function T_WNASPI32.Get_Found_Max_dev : Byte;
Begin
     Result := Found_Max_dev;
End;

Function T_WNASPI32.Get_Found_Max_buf_len : LongWord;
Begin
     Result := Found_Max_buf_len;
End;

Function T_WNASPI32.Get_Found_Res_supp : Boolean;
Begin
     Result := Found_Res_supp;
End;

Function T_WNASPI32.P_SRB : Pointer;
{ **********************************************
  Pointer to SRB to allow for type casting.
  You don't have to use this method.
  It's here for completeness.
  ********************************************** }
Begin
     P_SRB:=@SRB;
End;

Function T_WNASPI32.Get_sense_buf : Pointer;
{ *****************************************************
  Pointer to sense area data to allow for type casting.
  You don't have to use this method.  For
  performance you can e.g. do
  @(T_exec_SCSI((@(WNASPI32Obj.SRB))^).sense_area)
  in your program instead.
  It's here for completeness and for making your
  source code more easier to read.
  ***************************************************** }
Begin
     Result:=@(T_exec_SCSI(SRB).sense_area);
End;

Procedure T_WNASPI32.Set_data_buf(in_P_Buf : Pointer);
Begin
     Active_data_buf.p_buf := in_P_Buf;
End;

Procedure T_WNASPI32.Set_data_buf_size(in_buf_size : LongWord);
Begin
     Active_data_buf.size:=in_buf_size;
End;

Procedure T_WNASPI32.Set_SRB(in_SRB : T_LargestSRB);
Begin
     SRB := in_SRB;
End;

Procedure T_WNASPI32.Set_Cur_HA_ID(in_HA_ID : Byte);
Begin
     Cur_HA_ID := in_HA_ID;
End;

Procedure T_WNASPI32.Set_Cur_Dev_ID(in_Dev_ID : Byte);
Begin
     Cur_Dev_ID := in_Dev_ID;
End;

Procedure T_WNASPI32.Set_Cur_LUN(in_LUN : Byte);
Begin
     Cur_LUN := in_LUN;
End;

Procedure T_WNASPI32.Set_Cur_Res_supp(in_Res_supp : Boolean);
Begin
     Cur_Res_supp := in_Res_supp;
End;

Procedure T_WNASPI32.Set_SRB_status_OK(in_SRB_status : Boolean);
Begin
     SRB_status_OK:=in_SRB_status;
End;

Procedure T_WNASPI32.Set_SRB_err_msg(in_SRB_err_msg : String);
Begin
     SRB_err_msg:=in_SRB_err_msg;
End;

Procedure T_WNASPI32.LoadWinASPI32Proc;
{ ****************************************************************
  Main functions:
  1. Dynamically loads WINASPI32.DLL into memory using Win32
     LoadLibrary function,
  2. imports the functions into procedural type variables,
  3. verifies that the 2 required functions are imported:
         - GetASPI32SupportInfo
         - SendASPI32Command
  4. sets the OpenASPIStatus to indicate success, failure:
          1=successful, no errors
          2=call to GetASPISupportInfo failed
          3=could not load the WNASPI32.DLL
          4=could not import the 2 required functions
  **************************************************************** }
Begin
     A_ASPI32Handle:=LoadLibrary('WNASPI32.DLL');
     If A_ASPI32Handle<>0 Then
     Begin
          //MessageDlg('handle okay:' + inttostr(A_ASPI32Handle), mtInformation, [mbOk], 0);
          @A_GetASPI32SupportInfo:=GetProcAddress(A_ASPI32Handle, 'GetASPI32SupportInfo');
          @A_SendASPI32Command:=GetProcAddress(A_ASPI32Handle, 'SendASPI32Command');
          @A_GetASPI32Buffer:=GetProcAddress(A_ASPI32Handle, 'GetASPI32Buffer');
          @A_FreeASPI32Buffer:=GetProcAddress(A_ASPI32Handle, 'FreeASPI32Buffer');
          @A_TranslateASPI32Address:=GetProcAddress(A_ASPI32Handle, 'TranslateASPI32Address');

          If (@A_GetASPI32SupportInfo<>Nil) And
             (@A_SendASPI32Command<>Nil) Then
          Begin
               V_GetASPI32SupportInfo; //This has to be called at least once for
                                       //Win ASPI32 to initialise (SDK).
               If SRB_status_OK Then
               Begin
                    OpenASPIStatus:=USER_WNASPI32_OPENASPI_OK; //WNASPI32.DLL loaded and required
                                                               //functions verified.
               End
               Else
               Begin
                    OpenASPIStatus:=USER_WNASPI32_GETASPISUPINFO_ERR;
               End;
          End Else
              Begin
                   OpenASPIStatus:=USER_WNASPI32_OPENASPI_FX_IMPORT_ERR; //Required functions cannot be imported.
                   FreeLibrary(A_ASPI32Handle);
              End;
     End Else
         Begin
              OpenASPIStatus:=USER_WNASPI32_OPENASPI_DLL_LOAD_ERR; //Cannot load WNASPI32.DLL into mem.
         End;
End;

Procedure T_WNASPI32.UnloadWinASPI32Proc;
{ *************************************
  Removes the WNASPI32.DLL from memory.
  *************************************}
Begin
     FreeLibrary(A_ASPI32Handle);
End;

Procedure T_WNASPI32.V_GetASPI32SupportInfo;
{ *******************************************
  1. Calls GetASPI32SupportInfo,
  2. Set attributes from the returned value,
  3. Calls function to verify the SRB_status.
  ******************************************* }
Var SupportInfo : LongWord;
Begin
     SupportInfo:=A_GetASPI32SupportInfo;
     SRB_status:=(SupportInfo SHR 8) AND $FF;

     Verify_SRB_status;

     If SRB_status_OK Then
         Num_HA:=SupportInfo AND $FF
     Else
         Num_HA:=0;
End;

Procedure T_WNASPI32.V_SendASPI32Command;
{ *********************************************************
  This procedure is for calling ONLY synchronous Win ASPI32
  commands through SendASPI32Command.  The following are
  the commands:
       - Get HA inquiry   (code=0)
       - Get device type  (code=1)
       - Abort SRB        (code=3)
       - Get disk info    (code=6)
       - Rescan SCSI bus  (code=7)
       - Get/set timeouts (code=8)
  1. Calls WNASPI32 function SendASPI32Command,
  2. Calls function to verify the SRB_status.

  Synchronous means that the command will finish in a
  resonable amount of time and that ASPI will wait until
  it's finished before returning, i.e. program execution
  halts until it returns.

  Assumptions
       The SRB parameters are already set.
       The buffer size requirements are already allocated.
  ********************************************************* }
Begin
     Try
          //Program execution at this point halts here until ASPI finishes.
          SRB_status:=A_SendASPI32Command(@SRB);

          Verify_SRB_status;
          If SRB_status=SRB_ST_ERR Then
              Verify_HA_status
          Else
          Begin
               HA_status_OK:=True;
               HA_err_msg:='';
          End;
     Finally

     End;
End;

Procedure T_WNASPI32.V_SendASPI32Command_Wait;
{ *********************************************************
  This procedure is for calling asynchronous SCSI
  commands through SendASPI32Command.
  Main functions:
  1. Creates an event handle for processing of SRB using
     Windows event driven mechanism,
  2. If event handle is created then
        Calls WNASPI32 function SendASPI32Command
     else
        Calls WNASPI32 function SendASPI32Command using
        polling
  3. Calls function to verify the SRB_status.

  Asynchronous means that the command may finish in a lengthy
  amount of time but ASPI will return immediately and let
  your program continue to execute.  The procedure for waiting is:
  1. ASPI does the checking and signals a Windows event handler
     when the command finishes.

  To make things easier the event mechanism isn't used to call
  back any procedure (as intended), instead we just sit here and
  wait for the event to be signalled (it's actually now synchronous).

  Assumptions
       The SRB parameters are already set.
       The buffer size requirements are already allocated.
  ********************************************************* }
Begin
     heventSRB:=CreateEvent(Nil, True, False, Nil);
     If Cur_Res_supp Then
        T_exec_SCSI_hdr((@SRB)^).SRB_hdr.Flags:=(T_exec_SCSI_hdr((@SRB)^).SRB_hdr.Flags)
                                                Or SRB_RESIDUAL;
     If (heventSRB<>0) Then
     Begin
          T_exec_SCSI_hdr((@SRB)^).SRB_hdr.Flags:=(T_exec_SCSI_hdr((@SRB)^).SRB_hdr.Flags)
                                                  Or SRB_EVENT_NOTIFY;
          T_exec_SCSI_hdr((@SRB)^).Post_proc:=Pointer(heventSRB);
          ResetEvent(heventSRB);

          If T_exec_SCSI_hdr((@SRB)^).SRB_hdr.Flags=SRB_DIR_IN Then
          Begin
               Zero_data_buf;
          End;

          { Do some lengthy operation ... }
          SRB_status:=A_SendASPI32Command(@SRB);
          { Uses Windows event to wait for SRB to finish.. }
          If SRB_status=SRB_ST_PENDING Then
          Begin
               WaitForSingleObject(heventSRB, INFINITE); //Wait for event to be notified.
          End;

          SRB_status:=T_SRB_hdr((@SRB)^).Status; //Since command is asynchronous our own
                                                 //copy of SRB_status has to be set again.
          CloseHandle(heventSRB);

          Verify_SRB_status;
          If SRB_status=SRB_ST_ERR Then
              Verify_HA_status
          Else
          Begin
               HA_status_OK:=True;
               HA_err_msg:='';
          End;
     End
     //Else
         //Need to put some code here for handling error if event handler can't be made
End;

Procedure T_WNASPI32.SetGlobalTimeOutSecs(t_secs : LongWord);
{ ********************************************
  Set timeout in seconds for all SCSI devices.
  ******************************************** }
Begin
     ZeroSRB;
     With T_SRB_for_getset_timeout((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_GETSET_TIMEOUTS;
          SRB_hdr.HA_ID:=$FF; //All SCSI adapters
          SRB_hdr.Flags:=SRB_DIR_OUT;
          //SRB_hdr.Reserved:=0;
          Dev_ID:=$FF; //All SCSI device targets
          LUN:=$FF; //All logical units
          Timeout:=t_secs*2;
     End;
     V_SendASPI32Command;
End;

Function T_WNASPI32.GetDeviceTimeOut : LongWord;
{ ****************************************************
  Get timeout in 1/2 seconds from current SCSI device.
  **************************************************** }
Var t : LongWord;
Begin
     ZeroSRB;
     With T_SRB_for_getset_timeout((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_GETSET_TIMEOUTS;
          SRB_hdr.HA_ID:=Cur_HA_ID; //SCSI Adapter
          SRB_hdr.Flags:=SRB_DIR_IN;
          //SRB_hdr.Reserved:=0;
          Dev_ID:=Cur_Dev_ID; //Device on the above SCSI Adapter
          LUN:=Cur_LUN; //Logical unit of the above device ID
          //TimeOut:=0;
     End;
     V_SendASPI32Command;

     If SRB_status_OK Then
         t:=T_SRB_for_getset_timeout((@SRB)^).Timeout
     Else
         t:=0;

     GetDeviceTimeOut:=t;
End;

Procedure T_WNASPI32.SetDeviceTimeOutSecs(t_secs : LongWord);
{ ***********************************************
  Set timeout in seconds for current SCSI device.
  *********************************************** }
Begin
     ZeroSRB;
     With T_SRB_for_getset_timeout((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_GETSET_TIMEOUTS;
          SRB_hdr.HA_ID:=Cur_HA_ID; //SCSI adapter
          SRB_hdr.Flags:=SRB_DIR_OUT;
          //SRB_hdr.Reserved:=0;
          Dev_ID:=Cur_Dev_ID; //Device on the above SCSI adapter
          LUN:=Cur_LUN; //Logical unit of the above device ID
          Timeout:=t_secs*2;
     End;
     V_SendASPI32Command;
End;

Procedure T_WNASPI32.AllocLargestBuffer;
{ ***********************************************************************
  Attempt allocation of the largest (upto 512K) transfer
  buffer for use with SCSI commands.  The buffer sizes are attempted
  in this order (divide by 2 each time):
       USER_MEM_UNIT_LARGEMAX,
       USER_MEM_UNIT_LARGEMAX/2,
       USER_MEM_UNIT_LARGEMAX/2/2,
                  .
                  .
       USER_MEM_UNIT_LARGEMIN
  Allocation of 64K uses Win 32 VirtualAlloc and the other uses
  Win ASPI32 GetASPI32Buffer call.

  If GetASPI32Buffer was not imported then
       USER_MEM_UNIT_SMALLMAX,
       USER_MEM_UNIT_SMALLMAX/2,
       USER_MEM_UNIT_SMALLMAX/2/2,
                  .
                  .
       USER_MEM_UNIT_SMALLMIN
  is attempted.

  If
      - or if all the allocation attempt fails
      - or n<USER_MEM_UNIT_SMALLMIN
      - or Buf.Buf_len>=0 at the start of method (indicates prior buffer allocated)
  then no buffer is allocated.  Buf.Buf_len will remain 0.

  Buf.Buf_len will be set to the allocated size.  Buf.P_Buf will be set to
  point to the buffer.

  Table of some memory or storage unit quantities.
  Power form   Bytes          Kilobytes
  2^10       = 1024         = 1K (by computer definition)
  2^19       = 524288 /1024 = 512K
  2^18       = 262144 /1024 = 256K
  2^17       = 131072 /1024 = 128K
  2^16       = 65536  /1024 = 64K
  *********************************************************************** }
Var
   StopLoop : Boolean;
Begin
     If WNASPI32_Buf.Buf_len=0 Then
     Begin
          {****************************************************************
           A loop to attempt allocation of the largest (upto 512K) transfer
           buffer.  The loop will try the buffer sizes in this order:
           USER_MEM_UNIT_LARGEMAX to USER_MEM_UNIT_LARGEMIN (dividing by 2
           each time).
           Allocation uses GetASPI32Buffer call.
           ****************************************************************}
          If @A_GetASPI32Buffer<>Nil Then
          Begin
               StopLoop:=False;
               WNASPI32_Buf.Buf_len:=USER_MEM_UNIT_LARGEMAX;
               WNASPI32_Buf.Reserved:=0;
               WNASPI32_Buf.Zero_fill:=0;
               Repeat
                     BufByASPICallOK:=A_GetASPI32Buffer(@WNASPI32_Buf);
                     If BufByASPICallOK Then
                     Begin
                          StopLoop:=True;
                     End
                     Else If WNASPI32_Buf.Buf_len<=USER_MEM_UNIT_LARGEMIN Then
                          Begin
                               StopLoop:=True;
                               WNASPI32_Buf.Buf_len:=0;
                          End
                          Else
                              Begin
                                   WNASPI32_Buf.Buf_len:=WNASPI32_Buf.Buf_len DIV 2;
                              End;
               Until StopLoop;
          End;

          {*********************************************************
           If big buffer allocation failed, i.e. if all calls to
           GetASPI32Buffer failed, then attempt allocation of 64K of
           memory using Win32 VirtualAlloc function.
           *********************************************************}
          If BufByASPICallOK=False Then
          Begin
               {***************************************************
                A loop to allocate memory using Win32 VirtualAlloc
                function.  The sizes are attempted in the following
                order:
                   USER_MEM_UNIT_SMALLMAX to USER_MEM_UNIT_SMALLMIN
                   (dividing by 2 each time).
                It is very rare that 64K cannot be allocated.
                ***************************************************}
               StopLoop:=False;
               WNASPI32_Buf.Buf_len:=USER_MEM_UNIT_SMALLMAX;
               Repeat
                     WNASPI32_Buf.P_Buf:=VirtualAlloc(Nil, WNASPI32_Buf.Buf_len, MEM_COMMIT, PAGE_READWRITE);
                     If WNASPI32_Buf.P_Buf<>Nil Then
                     Begin
                          StopLoop:=True;
                     End
                     Else If WNASPI32_Buf.Buf_len<=USER_MEM_UNIT_SMALLMIN Then
                          Begin
                               StopLoop:=True;
                               WNASPI32_Buf.Buf_len:=0;
                          End
                          Else
                              WNASPI32_Buf.Buf_len:=WNASPI32_Buf.Buf_len DIV 2;
               Until StopLoop;
          End;
     End;
     Active_data_buf.p_buf:=WNASPI32_Buf.P_Buf;
     Active_data_buf.size:=WNASPI32_Buf.Buf_len;
End;

Procedure T_WNASPI32.AllocNBuffer(In_buf_len : LongWord);
{ ***********************************************************
  Attempt allocation of n (upto 512K) transfer buffer.

  If n is larger than USER_MEM_UNIT_SMALLMAX and less than or
  equal to USER_MEM_UNIT_LARGEMAX then
      Win ASPI32 function, GetASPI32Buffer is used,
  else
      Win32 function, VirtualAlloc is used.

  If
      - n<USER_MEM_UNIT_SMALLMIN
      - or if all the allocation attempt fails
      - or WNASPI32_Buf.Buf_len>=0 at the start of method (indicates prior buffer allocated)
  then no buffer is allocated.  WNASPI32_Buf.Buf_len will remain 0.

  WNASPI32_Buf.Buf_len will be set to the allocated size.  WNASPI32_Buf.P_Buf will be set to
  point to the buffer.

  Table of some memory or storage unit quantities.
  Power form   Bytes          Kilobytes
  2^10       = 1024         = 1K (by computer definition)
  2^19       = 524288 /1024 = 512K
  2^18       = 262144 /1024 = 256K
  2^17       = 131072 /1024 = 128K
  2^16       = 65536  /1024 = 64K
  *********************************************************** }
Begin
     If WNASPI32_Buf.Buf_len=0 Then
     Begin
          If In_buf_len<=USER_MEM_UNIT_SMALLMAX Then
          Begin
               If In_buf_len>=USER_MEM_UNIT_SMALLMIN Then
               Begin
                    {**************************************************
                     Allocate memory using Win32 VirtualAlloc function.
                     **************************************************}
                    WNASPI32_Buf.P_Buf:=VirtualAlloc(Nil, In_buf_len, MEM_COMMIT, PAGE_READWRITE);
                    If WNASPI32_Buf.P_Buf<>Nil Then
                    Begin
                         WNASPI32_Buf.Buf_len:=In_buf_len;
                    End;
               End;
          End
          Else
          If In_buf_len<=USER_MEM_UNIT_LARGEMAX Then
          Begin
               If @A_GetASPI32Buffer<>Nil Then
               Begin
                   {*******************************************************************
                    Attempt allocation of n transfer buffer using GetASPI32Buffer call.
                    *******************************************************************}
                    WNASPI32_Buf.Buf_len:=In_buf_len;
                    WNASPI32_Buf.Reserved:=0;
                    WNASPI32_Buf.Zero_fill:=0;
                    BufByASPICallOK:=A_GetASPI32Buffer(@WNASPI32_Buf);
                    If BufByASPICallOK Then
                         WNASPI32_Buf.Buf_len:=In_buf_len
                    Else
                         WNASPI32_Buf.Buf_len:=0;
               End;
          End;
     End;
     Active_data_buf.p_buf:=WNASPI32_Buf.P_Buf;
     Active_data_buf.size:=WNASPI32_Buf.Buf_len;
End;

Procedure T_WNASPI32.AllocNLargestBuffer(In_buf_len : LongWord);
{ ********************************************************************
  Attempt allocation of n (upto 512K) transfer buffer.

  If n is larger than USER_MEM_UNIT_SMALLMAX and less than or
  equal to USER_MEM_UNIT_LARGEMAX then
      Win ASPI32 function, GetASPI32Buffer is used,
  else
      Win32 function, VirtualAlloc is used.

  If n buffers cannot be allocated then a loop is entered to
  attempt allocation of smaller size buffers.
  Prior to the loop the starting largest buffer value, which
  is less than n and is normalised to 2^n is first calculated.

  If n>USER_MEM_UNIT_LARGEMAX then n will be set to
  USER_MEM_UNIT_LARGEMAX.

  For more info read the comments at the loops.

  If
      - n<USER_MEM_UNIT_SMALLMIN
      - or if all the allocation attempt fails
      - or WNASPI32_Buf.Buf_len>=0 at the start of method (indicates prior
        buffer allocated)
  then no buffer is allocated.  WNASPI32_Buf.Buf_len will remain 0.

  Table of some memory or storage unit quantities, which are of 2^n.
  These are the values which may be used to attempt memory allocation:
  Power form   Bytes          Kilobytes
  2^19       = 524288 /1024 = 512K
  2^18       = 262144 /1024 = 256K
  2^17       = 131072 /1024 = 128K
  2^16       = 65536  /1024 = 64K
  2^15       = 32768  /1024 = 32K
  2^14       = 16384  /1024 = 16K
  2^13       = 8192   /1024 = 8K
  2^12       = 4096   /1024 = 4K
  ******************************************************************** }
Var StopLoop : Boolean;
    i        : LongWord;
Begin
     If WNASPI32_Buf.Buf_len=0 Then
     Begin
          If In_buf_len<=USER_MEM_UNIT_SMALLMAX Then
          Begin
               If In_buf_len>=USER_MEM_UNIT_SMALLMIN Then
               Begin
                    {**************************************************
                     Allocate memory using Win32 VirtualAlloc function.
                     It is very rare that this cannot be allocated.
                     **************************************************}
                    WNASPI32_Buf.P_Buf:=VirtualAlloc(Nil, In_buf_len, MEM_COMMIT, PAGE_READWRITE);
                    If WNASPI32_Buf.P_Buf<>Nil Then
                    Begin
                         WNASPI32_Buf.Buf_len:=In_buf_len;
                    End
                    Else
                        Begin
                             {**********************************************************
                              Determine largest starting buffer value which is less than
                              In_buf_len and is normalised to 2^n.  This will be used
                              for the loop below.
                              **********************************************************}
                             //Buf_len:=0;
                             For i:=(USER_MEM_UNIT_SMALLMAX Div 2) DownTo USER_MEM_UNIT_SMALLMIN Do
                             Begin
                                  If In_buf_len>i Then
                                  Begin
                                       WNASPI32_Buf.Buf_len:=i;
                                       Break;
                                  End;
                             End;
                             If WNASPI32_Buf.Buf_len>0 Then
                             Begin
                                  {**************************************************
                                   A loop to allocate memory using Win32 VirtualAlloc
                                   function.  The sizes are of following:
                                   USER_MEM_UNIT_SMALLMAX to USER_MEM_UNIT_SMALLMIN
                                   (dividing by 2 each time).
                                   **************************************************}
                                  StopLoop:=False;
                                  Repeat
                                        WNASPI32_Buf.P_Buf:=VirtualAlloc(Nil, WNASPI32_Buf.Buf_len, MEM_COMMIT, PAGE_READWRITE);
                                        If WNASPI32_Buf.P_Buf<>Nil Then
                                        Begin
                                             StopLoop:=True;
                                        End
                                        Else If WNASPI32_Buf.Buf_len<=USER_MEM_UNIT_SMALLMIN Then
                                             Begin
                                                  StopLoop:=True;
                                                  WNASPI32_Buf.Buf_len:=0;
                                             End
                                             Else
                                                 WNASPI32_Buf.Buf_len:=WNASPI32_Buf.Buf_len DIV 2;
                                  Until StopLoop;
                             End;
                        End;
               End;
          End
          Else
          If @A_GetASPI32Buffer<>Nil Then
          Begin
               {*****************************************************
                Attempt allocation of In_buf_len (upto 512K) transfer
                buffer using GetASPI32Buffer call.
                *****************************************************}
               If In_buf_len>USER_MEM_UNIT_LARGEMAX Then
                   WNASPI32_Buf.Buf_len:=USER_MEM_UNIT_LARGEMAX
               Else
                   WNASPI32_Buf.Buf_len:=In_buf_len;
               WNASPI32_Buf.Reserved:=0;
               WNASPI32_Buf.Zero_fill:=0;
               BufByASPICallOK:=A_GetASPI32Buffer(@WNASPI32_Buf);
               If BufByASPICallOK Then
               Begin
                    WNASPI32_Buf.Buf_len:=WNASPI32_Buf.Buf_len;
               End;

               If BufByASPICallOK=False Then
               Begin
                    {************************************************************
                     Determine largest starting buffer value which is less than
                     In_buf_len and is normalised to 2^n.  This will be used for
                     the loop below.
                     ************************************************************}
                    For i:=(USER_MEM_UNIT_LARGEMAX Div 2) DownTo USER_MEM_UNIT_LARGEMIN Do
                    Begin
                         If In_buf_len>i Then
                         Begin
                              WNASPI32_Buf.Buf_len:=i;
                              Break;
                         End;
                    End;
                    If WNASPI32_Buf.Buf_len>0 Then
                    Begin
                         {************************************************************
                          A loop to attempt allocation of a large (upto 512K) transfer
                          buffer.  The loop will try the buffer sizes in this order:
                               USER_MEM_UNIT_LARGEMAX to USER_MEM_UNIT_LARGEMIN
                               (dividing by 2 each time).
                          Allocation uses GetASPI32Buffer call.
                          ************************************************************}
                         StopLoop:=False;
                         Repeat
                               BufByASPICallOK:=A_GetASPI32Buffer(@WNASPI32_Buf);
                               If BufByASPICallOK Then
                               Begin
                                    StopLoop:=True;
                               End
                               Else If WNASPI32_Buf.Buf_len<=USER_MEM_UNIT_LARGEMIN Then
                                    Begin
                                         StopLoop:=True;
                                         WNASPI32_Buf.Buf_len:=0;
                                    End
                                    Else
                                        Begin
                                             WNASPI32_Buf.Buf_len:=WNASPI32_Buf.Buf_len DIV 2;
                                        End;
                         Until StopLoop;
                    End;
               End;
          End;
     End;
     Active_data_buf.p_buf:=WNASPI32_Buf.P_Buf;
     Active_data_buf.size:=WNASPI32_Buf.Buf_len;
End;

Procedure T_WNASPI32.AllocNBufferVirtualAlloc(In_buf_len : LongWord);
{ ***********************************************************
  Attempt allocation of n memory for transfer buffer.

  Win32 function VirtualAlloc is used.

  If
      - n=0
      - or if all the allocation attempt fails
      - or WNASPI32_Buf.Buf_len>=0 at the start of method (indicates prior buffer allocated)
  then no buffer is allocated.  WNASPI32_Buf.Buf_len will remain 0.

  WNASPI32_Buf.Buf_len will be set to the allocated size.  WNASPI32_Buf.P_Buf will be set to
  point to the buffer.

  Table of some memory or storage unit quantities.
  Power form   Bytes          Kilobytes
  2^10       = 1024         = 1K (by computer definition)
  2^19       = 524288 /1024 = 512K
  2^18       = 262144 /1024 = 256K
  2^17       = 131072 /1024 = 128K
  2^16       = 65536  /1024 = 64K
  *********************************************************** }
Begin
     If In_buf_len>0 Then
     Begin
          If WNASPI32_Buf.Buf_len=0 Then
          Begin
               {**************************************************
                Allocate memory using Win32 VirtualAlloc function.
                **************************************************}
               WNASPI32_Buf.P_Buf:=VirtualAlloc(Nil, In_buf_len, MEM_COMMIT, PAGE_READWRITE);
               If WNASPI32_Buf.P_Buf<>Nil Then
               Begin
                    WNASPI32_Buf.Buf_len:=In_buf_len;

                    Active_data_buf.p_buf:=WNASPI32_Buf.P_Buf;
                    Active_data_buf.size:=WNASPI32_Buf.Buf_len;
               End;
          End;
     End;
End;

Procedure T_WNASPI32.AllocNLargestBufferVirtualAlloc(In_buf_len : LongWord);
{ ********************************************************************
  Attempt allocation of n memory for transfer buffer.

  Win32 function VirtualAlloc is used.

  If n buffers cannot be allocated then a loop is entered to
  attempt allocation of smaller size buffers.
  Prior to the loop the starting largest buffer value, which
  is less than n and is normalised to 2^n is first calculated.

  For more info read the comments at the loops.

  If
      - n=0
      - or if all the allocation attempt fails
      - or WNASPI32_Buf.Buf_len>=0 at the start of method (indicates prior
        buffer allocated)
  then no buffer is allocated.  WNASPI32_Buf.Buf_len will remain 0.

  Table of some memory or storage unit quantities, which are of 2^n.
  These are the values which may be used to attempt memory allocation:
  Power form   Bytes          Kilobytes
  2^19       = 524288 /1024 = 512K
  2^18       = 262144 /1024 = 256K
  2^17       = 131072 /1024 = 128K
  2^16       = 65536  /1024 = 64K
  2^15       = 32768  /1024 = 32K
  2^14       = 16384  /1024 = 16K
  2^13       = 8192   /1024 = 8K
  2^12       = 4096   /1024 = 4K
  ******************************************************************** }
Var StopLoop : Boolean;
Begin
     If In_buf_len>0 Then
     Begin
          If WNASPI32_Buf.Buf_len=0 Then
          Begin
               {**************************************************
                Allocate memory using Win32 VirtualAlloc function.
                It is very rare that this cannot be allocated.
                **************************************************}
               WNASPI32_Buf.P_Buf:=VirtualAlloc(Nil, In_buf_len, MEM_COMMIT, PAGE_READWRITE);
               If WNASPI32_Buf.P_Buf<>Nil Then
               Begin
                    WNASPI32_Buf.Buf_len:=In_buf_len;
               End
               Else
                   Begin
                        {**********************************************************
                         Determine largest starting buffer value which is less than
                         In_buf_len and is normalised to 2^n.  This will be used
                         for the loop below.
                         **********************************************************}
                        //Buf_len:=0;
                        WNASPI32_Buf.Buf_len:=(In_buf_len DIV 2) * 2;
                        If WNASPI32_Buf.Buf_len>In_buf_len Then
                        Begin
                             WNASPI32_Buf.Buf_len:=WNASPI32_Buf.Buf_len DIV 2;
                        End;
                        If WNASPI32_Buf.Buf_len>0 Then
                        Begin
                             {**************************************************
                              A loop to allocate memory using Win32 VirtualAlloc
                              function.  The sizes are of following:
                              calculated value that is normalised to 2^n to
                              USER_MEM_UNIT_SMALLMIN (dividing by 2 each time).
                              **************************************************}
                             StopLoop:=False;
                             Repeat
                                   WNASPI32_Buf.P_Buf:=VirtualAlloc(Nil, WNASPI32_Buf.Buf_len, MEM_COMMIT, PAGE_READWRITE);
                                   If WNASPI32_Buf.P_Buf<>Nil Then
                                   Begin
                                        StopLoop:=True;
                                   End
                                   Else If WNASPI32_Buf.Buf_len<=USER_MEM_UNIT_SMALLMIN Then
                                        Begin
                                             StopLoop:=True;
                                             WNASPI32_Buf.Buf_len:=0;
                                        End
                                        Else
                                            WNASPI32_Buf.Buf_len:=WNASPI32_Buf.Buf_len DIV 2;
                             Until StopLoop;
                        End;
                   End;
          End;
     End;

     Active_data_buf.p_buf:=WNASPI32_Buf.P_Buf;
     Active_data_buf.size:=WNASPI32_Buf.Buf_len;
End;

Procedure T_WNASPI32.FreeBuffer;
{ *****************************
  Free buffer allocated memory.
  ***************************** }
Begin
     If WNASPI32_Buf.Buf_len>0 Then
     Begin
          If BufByASPICallOK Then
          Begin
               WNASPI32_Buf.Zero_fill:=0;
               WNASPI32_Buf.Reserved:=0;
               //Attempt to free Win ASPI32 allocated transfer buffer,
               //previously allocated using GetASPI32Buffer.
               If A_FreeASPI32Buffer(@WNASPI32_Buf) Then
               Begin
                    WNASPI32_Buf.Buf_len:=0;
                    BufByASPICallOK:=False;
               End;
          End
          Else
              Begin
                   //Attempt to decommit and release transfer buffer,
                   //previously allocated using VirtualAlloc.
                   //Assumes entire mem was committed.
                   If VirtualFree(WNASPI32_Buf.P_Buf, 0, MEM_RELEASE) Then
                   Begin
                        WNASPI32_Buf.Buf_len:=0;
                   End;
              End;
     End;
End;

Procedure T_WNASPI32.Zero_data_buf;
{ ************************************
  Zeroise active data transfer buffer.
  ************************************ }
Begin
     ZeroMemory(Active_data_buf.p_buf, Active_data_buf.size); //Uses Win32 function.
End;

Procedure T_WNASPI32.ZeroSRB;
{ *******************
  Zeroise entire SRB.
  ******************* }
Begin
     ZeroMemory(@SRB, SizeOf(SRB));
End;

Procedure T_WNASPI32.MinZeroSRB;
{ **********************************************************
  This method can be used instead of ZeroSRB.
  Minimised zeroing of SRB for performance.  I hope this is
  faster than the method ZeroSRB.
  Only fill critical parts of the SRB with zeroes.

  If these parts (except CDB) are not zero filled, prior to
  sending a command using Win ASPI32 SCSI exec I/O
  (command 02h), then Win ASPI32 may not send the command.
  ********************************************************** }
Begin
     With T_exec_SCSI(SRB) Do
     Begin
          SRB_hdr.Reserved:=0;
          Reserved1:=0;
          Post_proc:=Nil;
          ZeroMemory(@Reserved2, SizeOf(Reserved2));
          //ZeroMemory(@CDB, SizeOf(CDB));               //This is not that critical.
                                                         //You can uncomment to be on safe side.
          ZeroMemory(@Sense_area, SizeOf(Sense_area));
     End;
End;

Procedure T_WNASPI32.Get_HA_inquiry;
{ *******************************************
  Call get host adapter inquiry ASPI command.
  ******************************************* }
Begin
     With T_SRB_for_HA_inq((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_HA_INQUIRY;
          SRB_hdr.HA_ID:=Cur_HA_ID; //Selects SCSI adapter
          SRB_hdr.Flags:=0;
          SRB_hdr.Reserved:=0;
     End;
     V_SendASPI32Command;
End;

Procedure T_WNASPI32.Get_dev_type;
{ **********************************
  Call get device type ASPI command.
  ********************************** }
Begin
     With T_SRB_for_get_dev_type((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_GET_DEV_TYPE;
          SRB_hdr.HA_ID:=Cur_HA_ID; //Selects SCSI adapter
          SRB_hdr.Flags:=0;
          SRB_hdr.Reserved:=0;

          Dev_ID:=Cur_Dev_ID; //Selects device on the above SCSI adapter
          LUN:=Cur_LUN; //Selects logical unit of the above device ID
          Reserved:=0;
     End;
     V_SendASPI32Command;
End;

Procedure T_WNASPI32.Abort_SRB;
{ ****************************
  Call abort SRB ASPI command.
  **************************** }
Begin
     With T_SRB_for_abort((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_ABORT_SRB;
          SRB_hdr.HA_ID:=Cur_HA_ID; //Selects SCSI adapter
          SRB_hdr.Flags:=0;
          SRB_hdr.Reserved:=0;

          SRB_ptr:=@SRB;
     End;
     V_SendASPI32Command;
End;

Procedure T_WNASPI32.Reset_dev;
{ **********************************
  Call reset device ASPI command.
  ********************************** }
Begin
     With T_SRB_for_reset_dev((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_RESET_DEV;
          SRB_hdr.HA_ID:=Cur_HA_ID; //Selects SCSI adapter
          SRB_hdr.Flags:=0;
          SRB_hdr.Reserved:=0;

          Dev_ID:=Cur_Dev_ID; //Selects device on the above SCSI adapter
          LUN:=Cur_LUN; //Selects logical unit of the above device ID
          ZeroMemory(@Reserved1, SizeOf(Reserved1));
          ZeroMemory(@Reserved2, SizeOf(Reserved2));
     End;
     V_SendASPI32Command_Wait;
End;

Procedure T_WNASPI32.Get_disk_drive_info;
{ **************************************
  Call get disk drive info ASPI command.
  ************************************** }
Begin
     With T_SRB_for_get_drv_parms((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_GET_DRIVE_INFO;
          SRB_hdr.HA_ID:=Cur_HA_ID; //Selects SCSI adapter
          SRB_hdr.Flags:=0;
          SRB_hdr.Reserved:=0;

          Dev_ID:=Cur_Dev_ID; //Selects device on the above SCSI adapter
          LUN:=Cur_LUN; //Selects logical unit of the above device ID
          ZeroMemory(@Reserved1, SizeOf(Reserved1));
     End;
     V_SendASPI32Command;
End;

Procedure T_WNASPI32.Rescan_SCSI_bus;
{ **********************************
  Call rescan SCSI bus ASPI command.
  ********************************** }
Begin
     With T_SRB_for_abort((@SRB)^) Do
     Begin
          SRB_hdr.Cmd:=AS_CMD_RESCAN_SCSI_BUS;
          SRB_hdr.HA_ID:=Cur_HA_ID; //Selects SCSI adapter
          SRB_hdr.Flags:=0;
          SRB_hdr.Reserved:=0;
     End;
     V_SendASPI32Command;
End;

Function T_WNASPI32.Verify_SRB_status : Boolean;
{ **************************************************************
  Verifies the Win ASPI32 SRB_status and sets the SRB_status_OK
  to indicate whether the SRB was processed successfully or not:
       - SRB_status_OK=True means successful
       - SRB_status_OK=False means failure
  If failure then also set SRB_err_msg with an error message
  string based on SRB_status error code.
  ************************************************************** }
Begin
     Case SRB_status Of
          SRB_ST_COMP: //SRB completed without errors.
          Begin
               SRB_status_OK:=True;
          End;
          SRB_ST_ABORTED:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='SRB aborted.';
          End;
          SRB_ST_ABORT_FAIL:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='SRB abort failed.';
          End;
          SRB_ST_INVALID_HA_ID:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Invalid Host Adapter ID:' + IntToStr(Cur_HA_ID);
          End;
          SRB_ST_NO_DEV:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='No device found at,' + Chr(10) + Chr(13) +
                            'HA ID :' + IntToStr(Cur_HA_ID) + Chr(10) + Chr(13) +
                            'Dev ID:' + IntToStr(Cur_Dev_ID) + Chr(10) + Chr(13) +
                            'LUN   :' + IntToStr(Cur_LUN);
          End;
          SRB_ST_ERR:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Host Adapter, SCSI bus, or SCSI target error,' +
                            Chr(10) + Chr(13) + 'check HA_status and Dev_status.';
          End;
          SRB_ST_INVALID_CMD:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Invalid SRB command.'
          End;
          SRB_ST_INVALID_SRB:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Invalid SRB parameters.'
          End;
          SRB_ST_BUFFER_ALIGN:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Buffer not aligned.'
          End;
          SRB_ST_ILLEGAL_MODE:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Unsupported Windows mode.'
          End;
          SRB_ST_NO_ASPI:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='No ASPI manager resident.'
          End;
          SRB_ST_FAILED_INIT:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Win ASPI32 could not init, system may be unstable.'
          End;
          SRB_ST_ASPI_IS_BUSY:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Win ASPI32 is busy.'
          End;
          SRB_ST_BUFFER_TOO_BIG:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Buffer is too large to handle for host adapter.'
          End;
          SRB_ST_MISMATCHED_COMPONENTS:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='Versions of ASPI components mismatch.'
          End;
          SRB_ST_NO_ADAPTERS:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='No host adapters found.'
          End;
          SRB_ST_INSUFFICIENT_RESOURCES:
          Begin
               SRB_status_OK:=False;
               SRB_err_msg:='ASPI could not init, insufficient resources.'
          End
     Else
         Begin
              SRB_status_OK:=False;
              SRB_err_msg:='Undefined or unknown error code: ' +
                           IntToStr(SRB_status) +
                           '.';
         End
     End;

     Verify_SRB_status:=SRB_status_OK;
End;

Function T_WNASPI32.Verify_HA_status : Boolean;
{ **************************************************************
  Verifies the Win ASPI32 HA_status and sets the HA_status_OK
  to indicate whether there are any HA errors:
       - HA_status_OK=True means no HA errors detected
       - HA_status_OK=False means errors
  If failure then also set HA_err_msg with an error message
  string based on HA_status error code.
  ************************************************************** }
Begin
     Case T_exec_SCSI_hdr((@SRB)^).HA_status Of
          HA_ST_OK: //HA detected no errors.
          Begin
               HA_status_OK:=True;
          End;
          HA_ST_TIMEOUT:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Host adapter bus timed out.';
          End;
          HA_ST_COMMAND_TIMEOUT:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='SRB command timed out.';
          End;
          HA_ST_MESSAGE_REJECT:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Message REJECT received.';
          End;
          HA_ST_BUS_RESET:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Host adapter bus reset detected.';
          End;
          HA_ST_PARITY_ERROR:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Host adapter parity error detected.';
          End;
          HA_ST_REQUEST_SENSE_FAILED:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Request sense command failed.';
          End;
          HA_ST_SEL_TO:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Target device select timed out.';
          End;
          HA_ST_DO_DU:
          Begin
               HA_status_OK:=False;
               If (T_SRB_hdr((@SRB)^).Flags AND SRB_DIR_IN)=SRB_DIR_IN Then
               Begin
                    HA_err_msg:='Data underrun (data transfer to device not fast enough).';
               End
               Else
                   HA_err_msg:='Data overrun (data transfer from device is too fast).';
          End;
          HA_ST_BUS_FREE:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Unexpected host adapter bus free.';
          End;
          HA_ST_PHASE_ERR:
          Begin
               HA_status_OK:=False;
               HA_err_msg:='Phase sequence error.';
          End
     Else
         Begin
              HA_status_OK:=False;
              HA_err_msg:='Undefined or unknown error code: ' +
                           IntToStr(T_exec_SCSI_hdr((@SRB)^).HA_status) +
                           '.';
         End
     End;

     Verify_HA_status:=HA_status_OK;
End;


Function T_WNASPI32.Get_HA_inquiry_out : T_HA_inquiry_out;
{ *******************************************
  Call get host adapter inquiry ASPI command.
  ******************************************* }
Var
   P_str : PChar;
Begin
     Get_HA_inquiry;

     If SRB_status_OK Then
     Begin
          With T_SRB_for_HA_inq((@SRB)^) Do
          Begin
               Get_HA_inquiry_out.HA_SCSI_ID:=HA_SCSI_ID;

               P_str:=StrAlloc(17);

               StrLCopy(P_str, @Manager_ID_str, 16);
               Get_HA_inquiry_out.manager:=String(P_str);

               StrLCopy(P_str, @HA_str, 16);
               Get_HA_inquiry_out.HA_name:=String(P_str);

               StrDispose(P_str);

               Get_HA_inquiry_out.HA_params:=T_array16(HA_uniq_parms);
          End;
     End;
End;

Function T_WNASPI32.Get_dev_type_out : Byte;
{ **********************************
  Call get device type ASPI command.
  ********************************** }
Begin
     Get_dev_type;

     If SRB_status_OK Then
          Get_dev_type_out:=T_SRB_for_get_dev_type((@SRB)^).Dev_Type
     Else
          Get_dev_type_out:=DTYPE_UNKNOWN;
End;

{$WARNINGS OFF}
Function T_WNASPI32.Find_CDROM(Is_first_find : Boolean) : Boolean;
{ ********************************************************************
  Searches for a CDROM.  It iterates through the device IDs for each
  host adapter (HA) looking for a CDROM.  At the moment LUNs are not
  iterated - because I don't have any hardware to test this (LUN will
  always be 0).
  If found, it will store the following:
       - HA ID,
       - device target ID,
       - LUN,
       - HA SCSI ID,
       - max target devices (not including LUNs) that the HA supports,
       - max buffer len that the HA supports.
  ********************************************************************}
Var Dev_ID_counter,
    HA_ID_counter,
    HA_ID_start,
    Dev_ID_start     : Byte;
    found_CDROM      : Boolean;
    stop_loop        : Boolean;
    Temp_Max_dev     : Byte;
    Temp_Max_buf_len : LongWord;
    Temp_HA_SCSI_ID  : Byte;
    Temp_Res_supp    : Byte;
    Saved_Cur_HA_ID   : Byte;
    Saved_Cur_Dev_ID  : Byte;
    Saved_Cur_LUN     : Byte;
    P_offset         : Pointer;
Begin
     found_CDROM:=False;

     If Get_Num_HA=0 Then
         Set_SRB_status_OK(True)
     Else
     If (Found_HA_ID>=(Get_Num_HA-1)) AND (Found_Dev_ID>=(Found_Max_dev-1)) Then
          Set_SRB_status_OK(True)
     Else
     Begin
          Saved_Cur_HA_ID:=Get_Cur_HA_ID;
          Saved_Cur_Dev_ID:=Get_Cur_Dev_ID;
          Saved_Cur_LUN:=Get_Cur_LUN;

          HA_ID_start:=Found_HA_ID;
          If Is_first_find Then
              Dev_ID_start:=Found_Dev_ID
          Else
              If Get_Cur_Dev_ID<(Found_Max_dev-1) Then
                  Dev_ID_start:=Found_Dev_ID+1
              Else
                  Begin
                       HA_ID_start:=HA_ID_start+1;
                       Dev_ID_start:=0;
                  End;

          stop_loop:=False;
          HA_ID_counter:=HA_ID_start;
          { This loop iterates the Win ASPI32 Host Adapter ID. }
          While ((HA_ID_counter<Get_Num_HA) And (stop_loop=False)) Do
          Begin
               Set_Cur_HA_ID(HA_ID_counter);
               Get_HA_inquiry;
               If Get_SRB_status_OK Then
               Begin
                    With T_SRB_for_HA_inq(P_SRB^) Do
                    Begin
                         Temp_HA_SCSI_ID:=HA_SCSI_ID; //Saves HA SCSI ID.

                         P_offset:=@HA_uniq_parms;

                         Inc(LongWord(P_offset),2);
                         Temp_Res_supp:=Byte(P_offset^); //Saves is residual supported.

                         Inc(LongWord(P_offset));
                         Temp_Max_dev:=Byte(P_offset^); //Saves max number of device IDs.
                         If (Temp_Max_dev<>8) AND (Temp_Max_dev<>16) Then
                         Begin
                              Temp_Max_dev:=ASPI_DEFAULT_MAX_DEV_ID+1; //Assume n max no. of IDs.
                         End;

                         Inc(LongWord(P_offset));
                         Temp_Max_buf_len:=LongWord(P_offset^); //Saves max HA buf transfer len.
                    End;

                    { This loop iterates the device IDs to look for a CDROM device type. }
                    For Dev_ID_counter:=Dev_ID_start To Temp_Max_dev-1 Do
                    Begin
                         Set_Cur_Dev_ID(Dev_ID_counter);
                         Get_dev_type;
                         If Get_SRB_status_OK Then
                         Begin
                              If T_SRB_for_get_dev_type(P_SRB^).Dev_Type=DTYPE_CDROM Then
                              Begin
                                   found_CDROM:=True;
                                   stop_loop:=True;
                                   Break;
                              End;
                         End
                         Else If Get_SRB_status=SRB_ST_NO_DEV Then
                              Begin
                                   { Set SRB status to be OK since we want to ignore
                                   no device errors for scanning devices. }
                                   Set_SRB_status_OK(True);
                                   Set_SRB_err_msg('');
                              End
                              Else
                                  Begin
                                       stop_loop:=True;
                                       Break;
                                  End;
                    End;

                    If Not stop_loop Then
                    Begin
                         Inc(HA_ID_counter);
                         Dev_ID_start:=0;
                    End;
               End
               Else
                   stop_loop:=True;
          End;

          If found_CDROM Then
          Begin
               Found_HA_ID:=Get_Cur_HA_ID;
               Found_Dev_ID:=Get_Cur_Dev_ID;
               Found_LUN:=Get_Cur_LUN;
               Found_HA_SCSI_ID:=Temp_HA_SCSI_ID;
               Found_Max_dev:=Temp_Max_dev;
               Found_Max_buf_len:=Temp_Max_buf_len;
               If Temp_Res_supp=1 Then
                   Found_Res_supp:=True
               Else
                   Found_Res_supp:=False;

               Set_Cur_HA_ID(Saved_Cur_HA_ID);
               Set_Cur_Dev_ID(Saved_Cur_Dev_ID);
               Set_Cur_LUN(Saved_Cur_LUN);
          End;
     End;

     Find_CDROM:=found_CDROM;
End;
{$WARNINGS ON}

Function T_WNASPI32.Find_first_CDROM : Boolean;
{ ********************************************************************
  Calls Find_CDROM method to find the first CDROM.
  If found, it will store the following:
       - HA ID,
       - device target ID,
       - LUN,
       - HA SCSI ID,
       - max target devices (not including LUNs) that the HA supports,
       - max buffer len that the HA supports.
  ********************************************************************}
Begin
     Found_HA_ID:=0;
     Found_Dev_ID:=0;
     Found_LUN:=0;
     Found_HA_SCSI_ID:=0;
     Found_Max_dev:=ASPI_DEFAULT_MAX_DEV_ID+1;
     Found_Max_buf_len:=0;
     Found_Res_supp:=False;

     Result:=Find_CDROM(True);
End;

Function T_WNASPI32.Find_next_CDROM : Boolean;
{ ************************************************************************
  Calls Find_CDROM method to search for the next CDROM.
  The search starts from the next sequence where Get_first_CDROM left off,
  i.e. from Found_HA_ID and Found_Dev_ID.
  If found, it will store the following:
       - HA ID,
       - device target ID,
       - LUN,
       - HA SCSI ID,
       - max target devices (not including LUNs) that the HA supports,
       - max buffer len that the HA supports.
  ************************************************************************}
Begin
     Result:=Find_CDROM(False);
End;

{ Fills in values of SRB for execute SCSI I/O command (02h). Note
  that CDB values are not filled in. }
Procedure T_WNASPI32.Fill_SRB_exec_SCSI(In_flags : Byte;
                                        In_buf_len : LongWord;
                                        In_CDB_len : Byte);
Begin
     With T_exec_SCSI_hdr((@SRB)^) Do
     Begin
          With SRB_hdr Do
          Begin
               Cmd:=AS_CMD_EXEC_SCSI_CMD;
               HA_ID:=Cur_HA_ID; //Selects SCSI adapter
               Flags:=In_flags;
          End;

          Dev_ID:=Cur_Dev_ID; //Selects device on the above SCSI adapter
          LUN:=Cur_LUN; //Selects logical unit of the above device ID
          //Check whether 'requested data transfer buffer len'>'actual data transfer buffer length'.
          If In_buf_len>Get_data_buf_size Then
          Begin
               //Truncate requested length to maximum allowed.
               Buf_len:=Get_data_buf_size;
          End
          Else
          Begin
               //Use requested length.
               Buf_len:=In_buf_len; //Amount of data transfer
          End;
          P_Buf:=Active_data_buf.p_buf;
          Sense_len:=AS_SENSE_LEN;
          CDB_len:=In_CDB_len;
     End;
End;

Procedure T_WNASPI32.Set_found_CDROM_active;
Begin
     Cur_HA_ID:=Found_HA_ID;       //Current Host Adapter ID.
     Cur_Dev_ID:=Found_Dev_ID;     //Current Device Target ID.
     Cur_LUN:=Found_LUN;           //Current Logical Unit Num.
     Cur_Res_supp:=Found_Res_supp; //Indicates that residual byte is supported.
End;

end.
