unit ImageToCDProgressFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageToCDReadThreadUnit;

Type Ptr_text_file=^TextFile;

type
  TImageToCDProgressForm = class(TForm)
    BTN_stop_write: TButton;
    LBL_write_sect: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    LBL_curr_write_speed: TLabel;
    LBL_drive_buffer_status: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LBL_read_sect: TLabel;
    Label6: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BTN_stop_writeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    On_FormClose_proc : TNotifyEvent;
    ImageToCDReadThread : TImageToCDReadThread;
    Constructor Create(In_On_FormClose_proc : TNotifyEvent; in_AOwner: TComponent);
    Procedure Start_thread(in_start_MMCLBA : Integer;
                           in_write_speed : Word;
                           in_lead_in_file_type : Byte;
                           in_lead_in_type : Byte;
                           in_f  : TFileStream;
                           in_f2 : TFileStream;
                           in_f3 : TFileStream;
                           in_f_pregap1 : TFileStream;
                           in_f_LO : TFileStream;
                           in_SSP_file : Pointer;
                           in_SSP_method : Byte);
    Procedure ShowErrMsg(s : String);
    Function ConfirmToMsg(s : String) : Boolean;
    Procedure Thread_done(Sender: TObject);
  end;

var
   ImageToCDProgressForm: TImageToCDProgressForm;

implementation

{$R *.dfm}

Constructor TImageToCDProgressForm.Create(In_On_FormClose_proc : TNotifyEvent; in_AOwner: TComponent);
Begin
     On_FormClose_proc:=In_On_FormClose_proc;

     inherited Create(in_AOwner);
End;

Procedure TImageToCDProgressForm.Start_thread(in_start_MMCLBA : Integer;
                                              in_write_speed : Word;
                                              in_lead_in_file_type : Byte;
                                              in_lead_in_type : Byte;
                                              in_f  : TFileStream;
                                              in_f2 : TFileStream;
                                              in_f3 : TFileStream;
                                              in_f_pregap1 : TFileStream;
                                              in_f_LO : TFileStream;
                                              in_SSP_file : Pointer;
                                              in_SSP_method : Byte);
Begin
     If ImageToCDReadThread=nil Then
     Begin
          ImageToCDReadThread:=TImageToCDReadThread.Create(Thread_done,
                                                           in_start_MMCLBA,
                                                           in_write_speed,
                                                           in_lead_in_file_type,
                                                           in_lead_in_type,
                                                           in_f,
                                                           in_f2,
                                                           in_f3,
                                                           in_f_pregap1,
                                                           in_f_LO,
                                                           in_SSP_file,
                                                           in_SSP_method);
     End;
End;

Procedure TImageToCDProgressForm.Thread_done(Sender: TObject);
Begin
     BTN_stop_write.Enabled:=False;
     MessageDlg('Process finished or terminated.' + chr(13) + 'Eject the CD before reading it.',
                mtInformation, [mbOk], 0);
End;

Procedure TImageToCDProgressForm.ShowErrMsg(s : String);
Begin
     Application.NormalizeTopMosts;
     MessageDlg(s, mtError, [mbOk], 0);
     Application.RestoreTopMosts;
End;

Function TImageToCDProgressForm.ConfirmToMsg(s : String) : Boolean;
Begin
     If MessageDlg(s, mtWarning, [mbYes, mbNo], 0)=mrYes Then
         ConfirmToMsg:=True
     Else
         ConfirmToMsg:=False;
End;

procedure TImageToCDProgressForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     If ImageToCDReadThread<>nil Then
     Begin
          ImageToCDReadThread.Terminate;
          ImageToCDReadThread.WaitFor;
          ImageToCDReadThread.Free;
          ImageToCDReadThread:=nil;
     End;

     On_FormClose_proc(Sender);

     Action:=caFree;
end;

procedure TImageToCDProgressForm.BTN_stop_writeClick(Sender: TObject);
begin
     If ImageToCDReadThread<>nil Then
     Begin
          ImageToCDReadThread.Terminate;
          ImageToCDReadThread.ImageToCDWriteThread.Terminate;
          BTN_stop_write.Enabled:=False;
     End;
end;

end.
