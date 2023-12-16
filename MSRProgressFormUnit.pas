unit MSRProgressFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MSRThreadUnit;

type
  TMSRProgressForm = class(TForm)
    Sect_reading_disp_Lbl: TLabel;
    Reading_sector_Lbl: TLabel;
    Stop_Image_Btn: TButton;
    LBL_curr_read_speed: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Stop_Image_BtnClick(Sender: TObject);
  private
    { Private declarations }
    On_FormClose_proc : TNotifyEvent;
    MSRThread      : TMSRThread;
    Procedure Thread_done(Sender: TObject);
  public
    { Public declarations }
    Constructor Create(In_On_FormClose_proc : TNotifyEvent; in_AOwner: TComponent);
    Procedure Start_thread(in_First_sector : LongInt;
                           in_Last_sector  : LongInt;
                           in_save_speed   : Boolean;
                           in_elasped_times_str : TStrings;
                           in_command_to_MSR : Byte;
                           in_SubChSelMode : Byte);
    Procedure ShowErrMsg(s : String);
  end;

var
  MSRProgressForm: TMSRProgressForm;

implementation

{$R *.dfm}

Constructor TMSRProgressForm.Create(In_On_FormClose_proc : TNotifyEvent; in_AOwner: TComponent);
Begin
     On_FormClose_proc:=In_On_FormClose_proc;

     inherited Create(in_AOwner);
End;

Procedure TMSRProgressForm.Start_thread(in_First_sector : LongInt;
                                        in_Last_sector  : LongInt;
                                        in_save_speed   : Boolean;
                                        in_elasped_times_str : TStrings;
                                        in_command_to_MSR : Byte;
                                        in_SubChSelMode : Byte);
Begin
     If MSRThread=nil Then
     Begin
          MSRThread:=TMSRThread.Create(Thread_done,
                                       in_First_sector,
                                       in_Last_sector,
                                       in_save_speed,
                                       in_elasped_times_str,
                                       in_command_to_MSR,
                                       in_SubChSelMode);
     End;
End;

Procedure TMSRProgressForm.Thread_done(Sender: TObject);
Begin
     MSRThread:=nil;
     Stop_Image_Btn.Enabled:=False;
     MessageDlg('Done.', mtInformation, [mbOk], 0);
End;

procedure TMSRProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     If MSRThread<>nil Then
     Begin
          Try
                MSRThread.Terminate;
          Except
                On EAccessViolation Do;
          End;

          Try
                MSRThread.WaitFor;
          Except
                
          End;
     End;

     On_FormClose_proc(Sender);

     Action:=caFree;
end;

procedure TMSRProgressForm.Stop_Image_BtnClick(Sender: TObject);
begin
     If MSRThread<>nil Then
     Begin
          Try
                MSRThread.Terminate;
          Except
                On EAccessViolation Do;
          End;
     End;
end;

Procedure TMSRProgressForm.ShowErrMsg(s : String);
Begin
     Application.NormalizeTopMosts;
     MessageDlg(s, mtError, [mbOk], 0);
     Application.RestoreTopMosts;
End;

end.
