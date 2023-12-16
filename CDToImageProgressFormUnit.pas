unit CDToImageProgressFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CDToImageThreadUnit;

type
  TCDToImageProgressForm = class(TForm)
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
    CDToImageThread      : TCDToImageThread;
    Procedure Thread_done(Sender: TObject);
  public
    { Public declarations }
    Constructor Create(In_On_FormClose_proc : TNotifyEvent; in_AOwner: TComponent);
    Procedure Start_thread(in_StartSect           : LongInt;
                           in_N_Sect              : LongWord;
                           in_CD_read_speed_index : Word;
                           in_FilterMode          : Byte;
                           in_SubChSelMode        : Byte;
                           in_is_TEB              : Boolean;
                           in_f                   : TFileStream;
                           in_f2                  : TFileStream;
                           in_f3                  : TFileStream;
                           in_f_pregap1           : TFileStream;
                           in_f_LO                : TFileStream;
                           in_f_C2                : TFileStream;
                           in_C2_error_mode       : Byte;
                           in_C2_read_order       : Byte;
                           in_lead_in_type        : Byte;
                           in_lead_in_size        : LongInt;
                           in_lead_out_size       : LongInt;
                           in_deinterleave        : Boolean);
    Procedure ShowErrMsg(s : String);
  end;

var
  CDToImageProgressForm: TCDToImageProgressForm;

implementation

{$R *.dfm}

Constructor TCDToImageProgressForm.Create(In_On_FormClose_proc : TNotifyEvent; in_AOwner: TComponent);
Begin
     On_FormClose_proc:=In_On_FormClose_proc;

     inherited Create(in_AOwner);
End;

Procedure TCDToImageProgressForm.Start_thread(in_StartSect           : LongInt;
                                       in_N_Sect              : LongWord;
                                       in_CD_read_speed_index : Word;
                                       in_FilterMode          : Byte;
                                       in_SubChSelMode        : Byte;
                                       in_is_TEB              : Boolean;
                                       in_f                   : TFileStream;
                                       in_f2                  : TFileStream;
                                       in_f3                  : TFileStream;
                                       in_f_pregap1           : TFileStream;
                                       in_f_LO                : TFileStream;
                                       in_f_C2                : TFileStream;
                                       in_C2_error_mode       : Byte;
                                       in_C2_read_order       : Byte;
                                       in_lead_in_type        : Byte;
                                       in_lead_in_size        : LongInt;
                                       in_lead_out_size       : LongInt;
                                       in_deinterleave        : Boolean);
Begin
     If CDToImageThread=nil Then
     Begin
          CDToImageThread:=TCDToImageThread.Create(Thread_done,
                                                   in_StartSect,
                                                   in_N_Sect,
                                                   in_CD_read_speed_index,
                                                   in_FilterMode,
                                                   in_SubChSelMode,
                                                   in_is_TEB,
                                                   in_f,
                                                   in_f2,
                                                   in_f3,
                                                   in_f_pregap1,
                                                   in_f_LO,
                                                   in_f_C2,
                                                   in_C2_error_mode,
                                                   in_C2_read_order,
                                                   in_lead_in_type,
                                                   in_lead_in_size,
                                                   in_lead_out_size,
                                                   in_deinterleave);
     End;
End;

Procedure TCDToImageProgressForm.Thread_done(Sender: TObject);
Begin
     Stop_Image_Btn.Enabled:=False;
     MessageDlg('Finished. Image completed.', mtInformation, [mbOk], 0);
End;

procedure TCDToImageProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     If CDToImageThread<>nil Then
     Begin
          CDToImageThread.Terminate;
          CDToImageThread.WaitFor;
          CDToImageThread.Free;
          CDToImageThread:=nil;
     End;

     On_FormClose_proc(Sender);

     Action:=caFree;
end;

procedure TCDToImageProgressForm.Stop_Image_BtnClick(Sender: TObject);
begin
     If CDToImageThread<>nil Then
     Begin
          CDToImageThread.Terminate;
          Stop_Image_Btn.Enabled:=False;
     End;
end;

Procedure TCDToImageProgressForm.ShowErrMsg(s : String);
Begin
     Application.NormalizeTopMosts;
     MessageDlg(s, mtError, [mbOk], 0);
     Application.RestoreTopMosts;
End;

end.
