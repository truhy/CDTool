unit ReadMCNThreadUnit;

interface

uses
  Classes,
  MMC1Unit;

type
  T_ReadMCNThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    Constructor Create(thread_done_proc : TNotifyEvent);
  end;

implementation

Uses
    MainFormUnit;

Constructor T_ReadMCNThread.Create(thread_done_proc : TNotifyEvent);
Begin
     OnTerminate:=thread_done_proc;
     FreeOnTerminate := True;
     Inherited Create(False);
End;

procedure T_ReadMCNThread.Execute;
begin
     Form1.SCSI.MMC1_any_link.Do_read_subch_MCN_out_CDB10(Form1.CommonCDSettings.MC_valid,
                                                          Form1.CommonCDSettings.MCN_str);
end;

end.
 