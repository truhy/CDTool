unit ReadISRCThreadUnit;

interface

uses
  Classes,
  MMC1Unit;

type
  T_ReadISRCThread = class(TThread)
  private
    { Private declarations }
    track_no : Byte;
  protected
    procedure Execute; override;
  public
    Constructor Create(thread_done_proc : TNotifyEvent;
                       in_track_no      : Byte);
  end;

implementation

Uses
    MainFormUnit;

Constructor T_ReadISRCThread.Create(thread_done_proc : TNotifyEvent;
                                    in_track_no      : Byte);
Begin
     track_no:=in_track_no;
     OnTerminate:=thread_done_proc;
     FreeOnTerminate := True;
     Inherited Create(False);
End;

procedure T_ReadISRCThread.Execute;
begin
     Form1.SCSI.MMC1_any_link.Do_read_subch_ISRC_out_CDB10(track_no,
                                                           Form1.CommonCDSettings.TC_valid,
                                                           Form1.CommonCDSettings.ISRC_str);
end;

end.

 