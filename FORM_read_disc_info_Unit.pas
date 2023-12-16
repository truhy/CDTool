unit FORM_read_disc_info_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GetCDROMListThreadUnit, Grids, MMC1Unit, Tools_Unit;

Type
    T_control_and_enabled=
    Record
          Ctrl : TControl;
          Enabled : Boolean;
    End;

type
  TForm7 = class(TForm)
    GroupBox2: TGroupBox;
    Device_Lbl: TLabel;
    RescanBtn: TButton;
    CD_dev_CB: TComboBox;
    BTN_read_disc_info: TButton;
    GroupBox1: TGroupBox;
    SGRID_disc_info: TStringGrid;
    GroupBox3: TGroupBox;
    SGRID_OPC: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RescanBtnClick(Sender: TObject);
    procedure CD_dev_CBChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BTN_read_disc_infoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    GetCDROMListThread : T_GetCDROMListThread;
    Curr_CDROM_indexNo : Integer;
    is_busy : Boolean;
    disabled_ctrl_list : TList;
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure ShowErrMsg(s : String);
    Procedure FillCDComboSelect;
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
    Procedure Decode_disc_status_bits_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_state_of_last_session_bits_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_erasable_bit_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_no_of_first_track_on_disc_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_no_of_sessions_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_first_track_no_in_last_session_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_last_track_no_in_last_session_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_DID_V_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_DBC_V_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_URU_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_disc_type_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_disc_id_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_last_session_leadin_start_time_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_last_start_time_for_start_of_leadout_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_disc_bar_code_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
    Procedure Decode_OPC_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

uses MainFormUnit;

{$R *.dfm}

Procedure TForm7.Disable_controls;
Var i, j : Integer;
    a_ctrl_enabled_state : ^T_control_and_enabled;
    GB : TGroupBox;
Begin
     disabled_ctrl_list:=TList.Create;

     For i:=0 To ControlCount-1 Do
     Begin
          New(a_ctrl_enabled_state);
          a_ctrl_enabled_state^.Ctrl:=Controls[i];
          a_ctrl_enabled_state^.Enabled:=Controls[i].Enabled;
          disabled_ctrl_list.Add(a_ctrl_enabled_state);

          Controls[i].Enabled:=False;

          If Controls[i] Is TGroupBox Then
          Begin
               GB:=(Controls[i] As TGroupBox);
               For j:=0 To GB.ControlCount-1 Do
               Begin
                    New(a_ctrl_enabled_state);
                    a_ctrl_enabled_state^.Ctrl:=GB.Controls[j];
                    a_ctrl_enabled_state^.Enabled:=GB.Controls[j].Enabled;
                    disabled_ctrl_list.Add(a_ctrl_enabled_state);

                    GB.Controls[j].Enabled:=False;
               End;
          End;
     End;
End;

Procedure TForm7.Enable_controls;
Var i : Integer;
    a_ctrl_enabled_state : ^T_control_and_enabled;
Begin
     For i:=0 To disabled_ctrl_list.Count-1 Do
     Begin
          a_ctrl_enabled_state:=disabled_ctrl_list.Items[i];
          a_ctrl_enabled_state^.Ctrl.Enabled:=a_ctrl_enabled_state^.Enabled;
          Dispose(a_ctrl_enabled_state);
     End;

     disabled_ctrl_list.Clear;
     disabled_ctrl_list.Free;
End;

procedure TForm7.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     If GetCDROMListThread<>nil Then
     Begin
          try
             GetCDROMListThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     Form1.Set_form_read_disc_info_Nil; //Prevent main form from showing this form.
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

Procedure TForm7.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

Procedure TForm7.FillCDComboSelect;
Begin
     Form1.CDFormUtils.FillCB_with_CDROM(CD_dev_CB);
     If CD_dev_CB.Items.Count>0 Then
     Begin
          If Form1.CommonCDSettings.SCSI_select_init_CDROM(0)=True Then //Attempt to select and init first CDROM.
          Begin
               CD_dev_CB.ItemIndex:=0; //Selects first item in combobox list.
               Curr_CDROM_indexNo:=0;
          End;
     End
     Else
         MessageDlg('No CD/DVD devices.', mtError, [mbOk], 0);
End;

Procedure TForm7.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;

     GetCDROMListThread:=nil;
     is_busy:=False;
End;


procedure TForm7.RescanBtnClick(Sender: TObject);
begin
     If Not is_busy Then
     Begin
          is_busy:=True;

          Disable_controls;

          GetCDROMListThread:=T_GetCDROMListThread.Create(SCSI_init_Get_CDROM_list_done,
                                                          ShowErrMsg,
                                                          False);
     End;
end;

procedure TForm7.CD_dev_CBChange(Sender: TObject);
begin
     If Form1.CommonCDSettings.SCSI_select_init_CDROM(CD_dev_CB.ItemIndex)=False Then //Attempt to select and init first CDROM.
     Begin
          CD_dev_CB.ItemIndex:=Curr_CDROM_indexNo; //Selects prev selected item in combobox list.

          MessageDlg('Cannot initialise device.', mtError, [mbOk], 0);
     End
     Else
     Begin
          Curr_CDROM_indexNo:=CD_dev_CB.ItemIndex;
     End;
end;

procedure TForm7.FormCreate(Sender: TObject);
Var i : Integer;
begin
     Curr_CDROM_indexNo:=0;
     GetCDROMListThread:=nil;

     //Field titles.
     SGRID_disc_info.Cells[0, 0]:='Field';
     SGRID_disc_info.Cells[1, 0]:='Value';
     SGRID_disc_info.Cells[2, 0]:='Information';
     //Row field values.
     SGRID_disc_info.Cells[0, 1]:='Disc status (2 bits):';
     SGRID_disc_info.Cells[0, 2]:='State of last session (2 bits):';
     SGRID_disc_info.Cells[0, 3]:='Erasable bit:';
     SGRID_disc_info.Cells[0, 4]:='No. of first track:';
     SGRID_disc_info.Cells[0, 5]:='No. of sessions:';
     SGRID_disc_info.Cells[0, 6]:='First track no. in last session:';
     SGRID_disc_info.Cells[0, 7]:='Last track no. in last session:';
     SGRID_disc_info.Cells[0, 8]:='DID_V bit';
     SGRID_disc_info.Cells[0, 9]:='DBC_V bit';
     SGRID_disc_info.Cells[0, 10]:='URU bit';
     SGRID_disc_info.Cells[0, 11]:='Disc type';
     SGRID_disc_info.Cells[0, 12]:='Disc ID';
     SGRID_disc_info.Cells[0, 13]:='Last session lead-in start time';
     SGRID_disc_info.Cells[0, 14]:='Last start time for start of lead-out';
     SGRID_disc_info.Cells[0, 15]:='Disc bar code';

     For i:=1 To SGRID_disc_info.RowCount-1 Do
     Begin
          SGRID_disc_info.Cells[2, i]:='None';
     End;

     //Field titles.
     SGRID_OPC.Cells[0, 0]:='Speed kbytes';
     SGRID_OPC.Cells[1, 0]:='OPC values';

     RescanBtn.Click;
end;

Procedure TForm7.Decode_disc_status_bits_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Var
   disc_status : Byte;
Begin
     disc_status:=out_read_disc_info_out_CDB10.Erasable_StateOfLastSess_DiscStatus And 3;
     SGRID_disc_info.Cells[1, 1]:=IntToStr(disc_status);
     Case disc_status Of
     0: Begin
             SGRID_disc_info.Cells[2, 1]:='Empty disc';
        End;
     1: Begin
             SGRID_disc_info.Cells[2, 1]:='Incomplete disc (appendable)';
        End;
     2: Begin
             SGRID_disc_info.Cells[2, 1]:='Complete disc (not writable)';
        End;
     3: Begin
             SGRID_disc_info.Cells[2, 1]:='Reserved';
        End;
     End;
End;

Procedure TForm7.Decode_state_of_last_session_bits_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Var
   state_of_last_session : Byte;
Begin
     state_of_last_session:=(out_read_disc_info_out_CDB10.Erasable_StateOfLastSess_DiscStatus And 12) Shr 2;
     SGRID_disc_info.Cells[1, 2]:=IntToStr(state_of_last_session);
     Case state_of_last_session Of
     0: Begin
             SGRID_disc_info.Cells[2, 2]:='Empty session';
        End;
     1: Begin
             SGRID_disc_info.Cells[2, 2]:='Incomplete session';
        End;
     2: Begin
             SGRID_disc_info.Cells[2, 2]:='Reserved';
        End;
     3: Begin
             SGRID_disc_info.Cells[2, 2]:='Complete session';
        End;
     End;
End;

Procedure TForm7.Decode_erasable_bit_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     If out_read_disc_info_out_CDB10.Erasable_StateOfLastSess_DiscStatus And 16=16 Then
     Begin
          SGRID_disc_info.Cells[1, 3]:='1';
          SGRID_disc_info.Cells[2, 3]:='CDRW media';
     End
     Else
     Begin
          SGRID_disc_info.Cells[1, 3]:='0';
          SGRID_disc_info.Cells[2, 3]:='CDRW media not present';
     End;
End;

Procedure TForm7.Decode_no_of_first_track_on_disc_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     SGRID_disc_info.Cells[1, 4]:=IntToStr(out_read_disc_info_out_CDB10.No_of_first_trk);
     SGRID_disc_info.Cells[2, 4]:='First track no. in TOC or PMA';
End;

Procedure TForm7.Decode_no_of_sessions_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     SGRID_disc_info.Cells[1, 5]:=IntToStr(out_read_disc_info_out_CDB10.No_of_sess);
     SGRID_disc_info.Cells[2, 5]:='Total no. of sessions (includes invisible session)';
End;

Procedure TForm7.Decode_first_track_no_in_last_session_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     SGRID_disc_info.Cells[1, 6]:=IntToStr(out_read_disc_info_out_CDB10.First_trk_no_in_last_sess);
     SGRID_disc_info.Cells[2, 6]:='Includes invisible track';
End;

Procedure TForm7.Decode_last_track_no_in_last_session_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     SGRID_disc_info.Cells[1, 7]:=IntToStr(out_read_disc_info_out_CDB10.Last_trk_no_in_last_sess);
     SGRID_disc_info.Cells[2, 7]:='Includes invisible track';
End;

Procedure TForm7.Decode_DID_V_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     If out_read_disc_info_out_CDB10.DID_V_DBC_V_URU And 128=128 Then
     Begin
          SGRID_disc_info.Cells[1, 8]:='1';
          SGRID_disc_info.Cells[2, 8]:='Disc ID info is valid';
     End
     Else
     Begin
          SGRID_disc_info.Cells[1, 8]:='0';
          SGRID_disc_info.Cells[2, 8]:='Disc ID info is not valid';
     End;
End;

Procedure TForm7.Decode_DBC_V_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     If out_read_disc_info_out_CDB10.DID_V_DBC_V_URU And 64=64 Then
     Begin
          SGRID_disc_info.Cells[1, 9]:='1';
          SGRID_disc_info.Cells[2, 9]:='Disc bar code info is valid';
     End
     Else
     Begin
          SGRID_disc_info.Cells[1, 9]:='0';
          SGRID_disc_info.Cells[2, 9]:='Disc bar code info is not valid';
     End;
End;

Procedure TForm7.Decode_URU_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     If out_read_disc_info_out_CDB10.DID_V_DBC_V_URU And 32=32 Then
     Begin
          SGRID_disc_info.Cells[1, 10]:='1';
          SGRID_disc_info.Cells[2, 10]:='Unrestricted use';
     End
     Else
     Begin
          SGRID_disc_info.Cells[1, 10]:='0';
          SGRID_disc_info.Cells[2, 10]:='Restricted use';
     End;
End;

Procedure TForm7.Decode_disc_type_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     SGRID_disc_info.Cells[1, 11]:=IntToStr(out_read_disc_info_out_CDB10.DiscType);
     Case out_read_disc_info_out_CDB10.DiscType Of
     $00: Begin
               SGRID_disc_info.Cells[2, 11]:='CD-DA or CD-ROM disc';
          End;
     $10: Begin
               SGRID_disc_info.Cells[2, 11]:='CD-I disc';
          End;
     $20: Begin
               SGRID_disc_info.Cells[2, 11]:='CD-ROM XA disc';
          End;
     $FF: Begin
               SGRID_disc_info.Cells[2, 11]:='Undefined';
          End;
     Else
         Begin
              SGRID_disc_info.Cells[2, 11]:='Reserved';
         End;
     End;
End;

Procedure TForm7.Decode_disc_id_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     If SGRID_disc_info.Cells[1, 8]='1' Then
     Begin
          SGRID_disc_info.Cells[1, 12]:=IntToStr(out_read_disc_info_out_CDB10.DiscID);
          SGRID_disc_info.Cells[2, 12]:='Disc ID from PMA';
     End
     Else
     Begin
          SGRID_disc_info.Cells[1, 12]:='Invalid';
          SGRID_disc_info.Cells[2, 12]:='None';
     End;
End;

Procedure TForm7.Decode_last_session_leadin_start_time_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     SGRID_disc_info.Cells[1, 13]:=IntToStrFixZeroes(out_read_disc_info_out_CDB10.Last_sess_lead_in_start_MSF.M, 2)+':'+
                                   IntToStrFixZeroes(out_read_disc_info_out_CDB10.Last_sess_lead_in_start_MSF.S, 2)+':'+
                                   IntToStrFixZeroes(out_read_disc_info_out_CDB10.Last_sess_lead_in_start_MSF.F, 2);
     If (out_read_disc_info_out_CDB10.Last_sess_lead_in_start_MSF.M=255) And
        (out_read_disc_info_out_CDB10.Last_sess_lead_in_start_MSF.S=255) And
        (out_read_disc_info_out_CDB10.Last_sess_lead_in_start_MSF.F=255) Then
     Begin
          SGRID_disc_info.Cells[2, 13]:='This address is due to disc being complete';
     End
     Else
     Begin
          SGRID_disc_info.Cells[2, 13]:='';
     End;
End;

Procedure TForm7.Decode_last_start_time_for_start_of_leadout_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     SGRID_disc_info.Cells[1, 14]:=IntToStrFixZeroes(out_read_disc_info_out_CDB10.Last_start_for_lead_out_MSF.M, 2)+':'+
                                   IntToStrFixZeroes(out_read_disc_info_out_CDB10.Last_start_for_lead_out_MSF.S, 2)+':'+
                                   IntToStrFixZeroes(out_read_disc_info_out_CDB10.Last_start_for_lead_out_MSF.F, 2);
     If (out_read_disc_info_out_CDB10.Last_start_for_lead_out_MSF.M=255) And
        (out_read_disc_info_out_CDB10.Last_start_for_lead_out_MSF.S=255) And
        (out_read_disc_info_out_CDB10.Last_start_for_lead_out_MSF.F=255) Then
     Begin
          SGRID_disc_info.Cells[2, 14]:='This address is due to disc being complete';
     End
     Else
     Begin
          SGRID_disc_info.Cells[2, 14]:='';
     End;
End;

Procedure TForm7.Decode_disc_bar_code_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Begin
     If SGRID_disc_info.Cells[1, 9]='1' Then
     Begin
          SGRID_disc_info.Cells[1, 15]:=IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte0)+','+
                                        IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte1)+','+
                                        IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte2)+','+
                                        IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte3)+','+
                                        IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte4)+','+
                                        IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte5)+','+
                                        IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte6)+','+
                                        IntToStr(out_read_disc_info_out_CDB10.DiscBarCode_Byte7);
          SGRID_disc_info.Cells[2, 15]:='Disc bar code';
     End
     Else
     Begin
          SGRID_disc_info.Cells[1, 15]:='Invalid';
          SGRID_disc_info.Cells[2, 15]:='None';
     End;
End;

Procedure TForm7.Decode_OPC_for_SGRID(out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block);
Var
   i : Integer;
   Ptr_OPC : T_P_read_disc_info_CDB10_OPC_table;
   speed_kBytes : Word;
Begin
     If out_read_disc_info_out_CDB10.No_of_OPC_tab_entries>0 Then
     Begin
          SGRID_OPC.RowCount:=out_read_disc_info_out_CDB10.No_of_OPC_tab_entries+1;

          Ptr_OPC:=Ptr(Integer(Form1.SCSI.SPC_any_link.Get_data_buf) + SizeOf(T_read_disc_info_CDB10_block));
          For i:=1 To out_read_disc_info_out_CDB10.No_of_OPC_tab_entries Do
          Begin
               speed_kBytes:=Form1.SCSI.SPC_any_link.SPC.ReverseBytesToWord(Ptr_OPC^.Speed_kBytes_MSB, Ptr_OPC^.Speed_kBytes_MSB);

               //Display OPC table entry.
               SGRID_OPC.Cells[0, i]:=IntToStr(speed_kBytes);
               SGRID_OPC.Cells[1, i]:=IntToStr(Ptr_OPC^.OPC_value_Byte0) + ',' +
                                      IntToStr(Ptr_OPC^.OPC_value_Byte1) + ',' +
                                      IntToStr(Ptr_OPC^.OPC_value_Byte2) + ',' +
                                      IntToStr(Ptr_OPC^.OPC_value_Byte3) + ',' +
                                      IntToStr(Ptr_OPC^.OPC_value_Byte4) + ',' +
                                      IntToStr(Ptr_OPC^.OPC_value_Byte5);

               //Go to next OPC table entry.
               Inc(Ptr_OPC);
          End;
     End
     Else
     Begin
          SGRID_OPC.RowCount:=2;
          SGRID_OPC.Cells[0, 1]:='No OPC info';
          SGRID_OPC.Cells[1, 1]:='No OPC info';
     End;
End;

procedure TForm7.BTN_read_disc_infoClick(Sender: TObject);
var
   out_read_disc_info_out_CDB10 : T_out_read_disc_info_CDB10_block;
   s : String;
   i : Integer;
begin
     If Not is_busy Then
     Begin
          is_busy:=True;
          Disable_controls;

          Form1.SCSI.MMC1_any_link.Do_read_disc_info_with_OPC_out_CDB10(out_read_disc_info_out_CDB10);
          If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               If out_read_disc_info_out_CDB10.DataLen>1 Then
               Begin
                    Decode_disc_status_bits_for_SGRID(out_read_disc_info_out_CDB10);
                    Decode_state_of_last_session_bits_for_SGRID(out_read_disc_info_out_CDB10);
                    Decode_erasable_bit_for_SGRID(out_read_disc_info_out_CDB10);

                    If out_read_disc_info_out_CDB10.DataLen>2 Then
                    Begin
                         Decode_no_of_first_track_on_disc_for_SGRID(out_read_disc_info_out_CDB10);

                         If out_read_disc_info_out_CDB10.DataLen>3 Then
                         Begin
                              Decode_no_of_sessions_for_SGRID(out_read_disc_info_out_CDB10);

                              If out_read_disc_info_out_CDB10.DataLen>4 Then
                              Begin
                                   Decode_first_track_no_in_last_session_for_SGRID(out_read_disc_info_out_CDB10);

                                   If out_read_disc_info_out_CDB10.DataLen>5 Then
                                   Begin
                                        Decode_last_track_no_in_last_session_for_SGRID(out_read_disc_info_out_CDB10);

                                        If out_read_disc_info_out_CDB10.DataLen>6 Then
                                        Begin
                                             Decode_DID_V_for_SGRID(out_read_disc_info_out_CDB10);
                                             Decode_DBC_V_for_SGRID(out_read_disc_info_out_CDB10);
                                             Decode_URU_for_SGRID(out_read_disc_info_out_CDB10);

                                             If out_read_disc_info_out_CDB10.DataLen>7 Then
                                             Begin
                                                  Decode_disc_type_for_SGRID(out_read_disc_info_out_CDB10);

                                                  If out_read_disc_info_out_CDB10.DataLen>14 Then
                                                  Begin
                                                       Decode_disc_id_for_SGRID(out_read_disc_info_out_CDB10);

                                                       If out_read_disc_info_out_CDB10.DataLen>18 Then
                                                       Begin
                                                            Decode_last_session_leadin_start_time_for_SGRID(out_read_disc_info_out_CDB10);

                                                            If out_read_disc_info_out_CDB10.DataLen>22 Then
                                                            Begin
                                                                 Decode_last_start_time_for_start_of_leadout_for_SGRID(out_read_disc_info_out_CDB10);

                                                                 If out_read_disc_info_out_CDB10.DataLen>30 Then
                                                                 Begin
                                                                      Decode_disc_bar_code_for_SGRID(out_read_disc_info_out_CDB10);
                                                                 End;
                                                            End;
                                                       End;
                                                  End;
                                             End;
                                        End;
                                   End;
                              End;
                         End;
                    End;
               End;

               Decode_OPC_for_SGRID(out_read_disc_info_out_CDB10);
          End
          Else
          Begin
               For i:=1 To SGRID_disc_info.RowCount-1 Do
               Begin
                    SGRID_disc_info.Cells[1, i]:='';
                    SGRID_disc_info.Cells[2, i]:='None';

                    SGRID_OPC.RowCount:=2;
                    SGRID_OPC.Cells[0, 1]:='No OPC info';
                    SGRID_OPC.Cells[1, 1]:='No OPC info';
               End;

               s:='Error while processing read disc info command.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               MessageDlg(s, mtError, [mbOk], 0);
          End;

          is_busy:=False;
          Enable_controls;
     End;
end;

procedure TForm7.FormDestroy(Sender: TObject);
begin
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

end.
