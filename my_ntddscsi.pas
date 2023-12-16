//Include file: my_ntddscsi.pas

//** Defines taken from ntddscsi.h in MS Windows DDK CD
const SCSI_IOCTL_DATA_OUT         = 0; //Give data to SCSI device (e.g. for writing)
const SCSI_IOCTL_DATA_IN          = 1; //Get data from SCSI device (e.g. for reading)
const SCSI_IOCTL_DATA_UNSPECIFIED = 2; //No data (e.g. for ejecting)

const IOCTL_SCSI_PASS_THROUGH = $4D004;
type T_byte_array16 = array[0..15] of byte;
type _SCSI_PASS_THROUGH =
record
      Length : word;
      ScsiStatus : byte;
      PathId : byte;
      TargetId : byte;
      Lun : byte;
      CdbLength : byte;
      SenseInfoLength : byte;
      DataIn : byte;
      DataTransferLength : cardinal;
      TimeOutValue : cardinal;
      DataBufferOffset : cardinal;
      SenseInfoOffset : cardinal;
      Cdb : T_byte_array16;
end;
type SCSI_PASS_THROUGH = _SCSI_PASS_THROUGH;
type PSCSI_PASS_THROUGH = ^_SCSI_PASS_THROUGH;

const IOCTL_SCSI_PASS_THROUGH_DIRECT = $4D014;
type _SCSI_PASS_THROUGH_DIRECT =
record
      Length : word;
      ScsiStatus : byte;
      PathId : byte;
      TargetId : byte;
      Lun : byte;
      CdbLength : byte;
      SenseInfoLength : byte;
      DataIn : byte;
      DataTransferLength : cardinal;
      TimeOutValue : cardinal;
      DataBuffer : Pointer;
      SenseInfoOffset : cardinal;
      Cdb : T_byte_array16;
end;
type SCSI_PASS_THROUGH_DIRECT = _SCSI_PASS_THROUGH_DIRECT;
type PSCSI_PASS_THROUGH_DIRECT = ^_SCSI_PASS_THROUGH_DIRECT;
//** End of defines taken from ntddscsi.h from MS Windows DDK CD

const MAX_SENSE_LEN=18;

type _SCSI_PASS_THROUGH_AND_BUFFERS =
record
      spt : SCSI_PASS_THROUGH;
      SenseBuf : array[0..MAX_SENSE_LEN-1] of byte;
      DataBuf : array[0..64*1024-1] of byte; //64K bytes main data buffer
end;
type T_SPT_BUFS = _SCSI_PASS_THROUGH_AND_BUFFERS;

type _SCSI_PASS_THROUGH_DIRECT_AND_SENSE_BUFFER =
record
      sptd : SCSI_PASS_THROUGH_DIRECT;
      SenseBuf : array[1..MAX_SENSE_LEN] of byte;
end;
type T_SPDT_SBUF = _SCSI_PASS_THROUGH_DIRECT_AND_SENSE_BUFFER;
