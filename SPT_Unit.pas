unit SPT_Unit;

interface

uses
    Windows,
    SysUtils;

{$I my_ntddscsi.pas}

{ Start of SPT class. }
type T_SPT=
class
private
      h_event : Integer; //Handle to event for waiting.
      active_drive_letter : char;
public
      spt_bufs : T_SPT_BUFS; //SCSI pass through structure including buffers.
      drive_letter_found : char;
      hVolume : cardinal;
      time_out_value : cardinal;
      Desc_data_offset   : word; //Used for read TOC/PMA/ATIP SCSI command.

      { Device error messages (part of SPC SCSI spec).. }
      Dev_status_OK      : Boolean; //Whether target device status is ok.
      Dev_err_msg        : String;  //Target device error message.

      { Sense error messages (part of SPC SCSI spec).. }
      Std_sense_exists   : boolean; //Whether non-vendor sense data exists.
      Sense_key_err_msg  : string;  //Sense key error message.
      Sense_code_err_msg : string;  //Sense ASC and ASCQ error message.

      dev_io_success : boolean;
      err_msg : string;

      constructor Create;
      destructor Destroy; Override;
      procedure Open_set_active_drive_letter(in_active_drive_letter : char);
      function Get_active_drive_letter : char;
      function Get_data_buf : Pointer;
      function Get_data_buf_size : LongWord;
      function Get_sense_buf : Pointer;
      function Get_p_Cdb : Pointer;
      procedure Fill_spt(In_data_transfer_dir : Byte;
                         In_data_transfer_len : LongWord;
                         In_CDB_len : Byte);
      procedure Send_SCSI_cmd_wait;
      Procedure Zero_data_buf;
      Procedure Zero_sense_buf;
      function Is_it_CDROM_drive(cDriveLetter : char) : Boolean;
      function OpenVolume(cDriveLetter : char) : integer;
      function CloseVolume : boolean;
      function Find_CDROM(Is_first_find : Boolean) : Boolean;
      function Find_first_CDROM : Boolean;
      function Find_next_CDROM : Boolean;
      procedure Set_found_CDROM_active;
end;

implementation

constructor T_SPT.Create;
begin
     inherited; //Calls the original Delphi constructor.

     time_out_value:=108000;  //Default SCSI timeout value (max 108000 sec = time 30 min)
     hVolume:=INVALID_HANDLE_VALUE
end;

destructor T_SPT.Destroy;
begin
     CloseVolume;

     inherited; //Calls the original Delphi destructor.
end;

procedure T_SPT.Open_set_active_drive_letter(in_active_drive_letter : char);
begin
     active_drive_letter := in_active_drive_letter;
     OpenVolume(active_drive_letter);
end;

function T_SPT.Get_active_drive_letter : char;
begin
     result := active_drive_letter;
end;

function T_SPT.Get_data_buf : Pointer;
begin
     Result:=Ptr(Integer(@spt_bufs) + SizeOf(SCSI_PASS_THROUGH) + MAX_SENSE_LEN);
end;

function T_SPT.Get_data_buf_size : LongWord;
begin
     Result:=65536; //64K bytes
end;

function T_SPT.Get_sense_buf : Pointer;
begin
     Result:=Ptr(Integer(@spt_bufs) + SizeOf(SCSI_PASS_THROUGH));
end;

function T_SPT.Get_p_Cdb : Pointer;
begin
     Result:=@(spt_bufs.spt.Cdb);
end;

procedure T_SPT.Fill_spt(In_data_transfer_dir : Byte;
                         In_data_transfer_len : LongWord;
                         In_CDB_len : Byte);
begin
     spt_bufs.spt.Length:=SizeOf(SCSI_PASS_THROUGH);
     spt_bufs.spt.PathId:=0;   //SCSI card ID will be filled in automatically
     spt_bufs.spt.TargetId:=0; //SCSI target ID will also be filled in
     spt_bufs.spt.Lun:=0;      //SCSI lun ID will also be filled in
     spt_bufs.spt.CdbLength:=In_CDB_len; //CDB size is 12 for ReadCD MMC1 command
     spt_bufs.spt.SenseInfoLength:=MAX_SENSE_LEN; //Max sense data that can be returned
     spt_bufs.spt.DataIn:=In_data_transfer_dir; //Direction of data transfer buffer
     //Check whether 'requested data transfer buffer len'>'actual data transfer buffer length'.
     If In_data_transfer_len>Get_data_buf_size Then
     Begin
          //Truncate requested length to maximum allowed.
          spt_bufs.spt.DataTransferLength:=Get_data_buf_size;
     End
     Else
     Begin
          //Use requested length.
          spt_bufs.spt.DataTransferLength:=In_data_transfer_len; //Amount of data transfer
     End;
     spt_bufs.spt.TimeOutValue:=time_out_value; //SCSI timeout value
     spt_bufs.spt.DataBufferOffset:=SizeOf(SCSI_PASS_THROUGH)+MAX_SENSE_LEN;
     spt_bufs.spt.SenseInfoOffset:=SizeOf(SCSI_PASS_THROUGH);
end;

Procedure T_SPT.Send_SCSI_cmd_wait;
{ ************************************************************************
  This procedure is for calling SCSI commands through DeviceIOCTL function
  with SPT command code.
  Main functions:
  1. Creates a file handle to the device indicated by active_drive_letter.
  2. DeviceIOCTL is synchronous, so this method will wait for it to be
     completed.
  3. Sets the dev_io_success to indicate success.

  Assumptions
       The SPT structure parameters are already set.
       The data & sense buffer requirements are already allocated.
  ************************************************************************ }
var
   dwBytesReturned : cardinal;
   ErrorCode : cardinal;
begin
     if (hVolume <> INVALID_HANDLE_VALUE) then
     begin
          If spt_bufs.spt.DataIn=SCSI_IOCTL_DATA_IN Then
          Begin
               Zero_data_buf;
          End;
          Zero_sense_buf;
          //Send the SCSI command to the drive and wait until it completes.
          dev_io_success:=
          DeviceIoControl(hVolume,
                          IOCTL_SCSI_PASS_THROUGH,
                          @spt_bufs, SizeOf(spt_bufs),
                          @spt_bufs, SizeOf(spt_bufs),
                          dwBytesReturned,
                          nil);
          if dev_io_success=false then
          begin
               err_msg:='Win32 DeviceIOCTL with SCSI_PASS_THROUGH command failed.'
          end;
     end
     else
     begin
          err_msg:='Invalid drive handle.';
     end;
end;

Procedure T_SPT.Zero_data_buf;
{ *****************************
  Zeroise data transfer buffer.
  ***************************** }
Begin
     ZeroMemory(Get_data_buf, SizeOf(spt_bufs.DataBuf)); //Uses Win32 function.
End;

Procedure T_SPT.Zero_sense_buf;
{ *****************************
  Zeroise sense transfer buffer.
  ***************************** }
Begin
     ZeroMemory(Get_sense_buf, SizeOf(spt_bufs.SenseBuf)); //Uses Win32 function.
End;

function T_SPT.Is_it_CDROM_drive(cDriveLetter : char) : Boolean;
var
   szRootName : array[0..4] of char;
   uDriveType : cardinal;
begin
     szRootName[0]:=cDriveLetter;
     szRootName[1]:=':';
     szRootName[2]:='\';
     szRootName[3]:=chr(0);

     uDriveType := GetDriveType(szRootName);

     case uDriveType of
     DRIVE_CDROM:
     begin
          result := true;
     end
     else
     begin
          result := false;
     end;
     end;
end;

function T_SPT.OpenVolume(cDriveLetter : char) : integer;
{
	1. Checks the drive type to see if it's identified as a CDROM drive

	2. Uses Win32 CreateFile function to get a handle to a drive
	   that is specified by cDriveLetter.

   Under Windows 2K/XP and later the access rights field for CreateFile
   function must be GENERIC_READ and GENERIC_WRITE when using
   SCSI_PASS_THROUGH or SCSI_PASS_THROUGH_DIRECT with DeviceIOControl.
   Under Windows NT it must only be GENERIC_READ.

   If you don't have windows.pas, some of the define constants are
   listed as comments.
}
var
   szVolumeName : array[0..6] of char;
   ErrorCode : cardinal;
begin
     if Is_it_CDROM_drive(cDriveLetter) then
     begin
          CloseVolume;

          szVolumeName[0]:='\';
          szVolumeName[1]:='\';
          szVolumeName[2]:='.';
          szVolumeName[3]:='\';
          szVolumeName[4]:=cDriveLetter;
          szVolumeName[5]:=':';
          szVolumeName[6]:=chr(0);

          //const GENERIC_READ = $80000000
          //const GENERIC_WRITE = $40000000
          //const FILE_SHARE_READ = $00000001
          //const FILE_SHARE_WRITE = $00000002
          //const OPEN_EXISTING = 3
          //const FILE_ATTRIBUTE_NORMAL = $00000080
          hVolume := CreateFile(szVolumeName,
                                GENERIC_READ or GENERIC_WRITE,
                                FILE_SHARE_READ or FILE_SHARE_WRITE,
                                nil,
                                OPEN_EXISTING,
                                FILE_ATTRIBUTE_NORMAL,
                                0);
          if (hVolume = INVALID_HANDLE_VALUE) then
          begin
               //Previous CreateFile failed, so try again with only GENERIC_READ.
               //This is for Windows NT 4.
               hVolume := CreateFile(szVolumeName,
                                     GENERIC_READ,
                                     FILE_SHARE_READ or FILE_SHARE_WRITE,
                                     nil,
                                     OPEN_EXISTING,
                                     FILE_ATTRIBUTE_NORMAL,
                                     0);
          end;
     end
     else
         hVolume := INVALID_HANDLE_VALUE;

     if (hVolume = INVALID_HANDLE_VALUE) then
     begin
          ErrorCode := GetLastError();
          err_msg:='Could not create handle for drive.' + Chr(13) +
                   'Windows error code: ' + IntToStr(ErrorCode);
     end;

     result := hVolume;
end;

function T_SPT.CloseVolume : boolean;
var
   success : Boolean;
begin
     success:=true;

     if hVolume <> INVALID_HANDLE_VALUE then
     begin
          success:=CloseHandle(hVolume);

          hVolume := INVALID_HANDLE_VALUE
     end;

     result := success;
end;

{ *********************************************************************
  Searches for a CDROM.  It iterates through the Windows drive letters.
  If is_first_find is true it starts from letter 'A', else it starts
  from active_drive_letter.
  If a CDROM drive type is found then drive_letter_found contains the
  letter and a true result is returned.
  ********************************************************************* }
function T_SPT.Find_CDROM(Is_first_find : Boolean) : Boolean;
var drive_letter : char;
    drive_letter_start : char;
begin
     if Is_first_find then
     begin
          drive_letter_start:='A';
     end
     else
     begin
          drive_letter_start:=Chr(Ord(drive_letter_found)+1);
     end;

     for drive_letter:=drive_letter_start to 'Z' do
     begin
          if Is_it_CDROM_drive(drive_letter) then
          begin
               drive_letter_found:=drive_letter;
               Result:=True;
               Exit;
          end;
     end;

     Result:=False;
end;

{ *******************************************************************
  Calls Find_CDROM method to find the first CDROM.
  If a CDROM drive type is found then drive_letter_found contains the
  letter and a true result is returned.
  *******************************************************************}
function T_SPT.Find_first_CDROM : Boolean;
begin
     Result:=Find_CDROM(True);
end;

{ *******************************************************************
  Calls Find_CDROM method to find the next CDROM.
  If a CDROM drive type is found then drive_letter_found contains the
  letter and a true result is returned.
  *******************************************************************}
function T_SPT.Find_next_CDROM : Boolean;
begin
     Result:=Find_CDROM(False);
end;

procedure T_SPT.Set_found_CDROM_active;
begin
     active_drive_letter:=drive_letter_found;
     OpenVolume(active_drive_letter);
end;

end.
 