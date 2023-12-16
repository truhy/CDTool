unit Read_TOC_MSF_first_out_ThreadUnit;

interface

uses
  Classes,
  MMC1Unit;

type
  T_Read_TOC_MSF_first_out_Thread = class(TThread)
  private
    { Private declarations }
  protected
    Read_TOC_mode : Byte;
    Trk_sess_no : Byte;
    procedure Execute; override;
  public
    Constructor Create(thread_done_proc : TNotifyEvent; in_read_TOC_mode : Byte; in_trk_sess_no : Byte);
  end;

implementation

Uses
    MainFormUnit;

Constructor T_Read_TOC_MSF_first_out_Thread.Create(thread_done_proc : TNotifyEvent; in_read_TOC_mode : Byte; in_trk_sess_no : Byte);
Begin
     Read_TOC_mode:=in_read_TOC_mode;
     Trk_sess_no:=in_trk_sess_no;
     OnTerminate:=thread_done_proc;
     FreeOnTerminate := True;
     Inherited Create(False);
End;

procedure T_Read_TOC_MSF_first_out_Thread.Execute;
begin
     Case Read_TOC_mode Of
     MMC_READ_T_P_A_FORMAT_TOC:
          Form1.SCSI.MMC1_any_link.Do_read_T_P_A_TOC_MSF_first_out_CDB10(Form1.CommonCDSettings.First_trk_sess_no,
                                                                         Form1.CommonCDSettings.Last_trk_sess_no,
                                                                         Form1.CommonCDSettings.TOC_data,
                                                                         Trk_sess_no);
     MMC_READ_T_P_A_FORMAT_SESS_INFO:
          Form1.SCSI.MMC1_any_link.Do_read_T_P_A_sess_MSF_out_CDB10(Form1.CommonCDSettings.First_trk_sess_no,
                                                                    Form1.CommonCDSettings.Last_trk_sess_no,
                                                                    Form1.CommonCDSettings.Sess_TOC_data);
     MMC_READ_T_P_A_FORMAT_FULL_TOC:
          Form1.SCSI.MMC1_any_link.Do_read_T_P_A_full_TOC_first_out_CDB10(Form1.CommonCDSettings.First_trk_sess_no,
                                                                          Form1.CommonCDSettings.Last_trk_sess_no,
                                                                          Form1.CommonCDSettings.Full_TOC_data,
                                                                          Trk_sess_no);
     End;
end;

end.
