unit MSR_form_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math, GetCDROMListThreadUnit, MMC1Unit, SBC_Unit, CommonCDSettingsUnit,
  ExtCtrls, MSRProgressFormUnit;

type
  T_MSR_form = class(TForm)
    RescanBtn: TButton;
    Device_Lbl: TLabel;
    CD_dev_CB: TComboBox;
    BTN_measure: TButton;
    FirstSector_Lbl: TLabel;
    LastSector_Lbl: TLabel;
    FirstSector_Edit: TEdit;
    LastSector_Edit: TEdit;
    LastSectorFromTOC_Btn: TButton;
    DataReadSpeed_Lbl: TLabel;
    DataReadSpeed_CB: TComboBox;
    ImageSaveDialog: TSaveDialog;
    ImageSaveAsBtn: TButton;
    Subch_Lbl: TLabel;
    Subch_CB: TComboBox;
    CB_MMC_command_to_measure: TComboBox;
    Label1: TLabel;
    CHK_save_rep_speed: TCheckBox;
    BTN_gen_SSP: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RescanBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CD_dev_CBChange(Sender: TObject);
    procedure DataReadSpeed_CBChange(Sender: TObject);
    procedure LastSectorFromTOC_BtnClick(Sender: TObject);
    procedure BTN_measureClick(Sender: TObject);
    procedure Subch_CBChange(Sender: TObject);
    procedure ImageSaveAsBtnClick(Sender: TObject);
    procedure CB_MMC_command_to_measureChange(Sender: TObject);
    procedure BTN_gen_SSPClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc;
    GetCDROMListThread : T_GetCDROMListThread;
    Curr_CDROM_indexNo : Integer;
    Curr_data_read_speed   : Word;
    is_busy : Boolean;
    disabled_ctrl_list : TList;
    SubChSelMode : Byte;
    elasped_times_str : TStrings;
    SSP_entries : TStrings;
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure ShowErrMsg(s : String);
    Procedure FillSpeedCB;
    Procedure FillCDComboSelect;
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
    Procedure Measure;
    Procedure MSRProgressForm_close(Sender: TObject);
  public
    { Public declarations }
    MSRProgressForm  : TMSRProgressForm;
  end;

var
  MSR_form: T_MSR_form;

implementation

uses MainFormUnit;

{$R *.dfm}

Procedure T_MSR_form.Disable_controls;
Var i : Integer;
    a_ctrl_enabled_state : ^Boolean;
Begin
     disabled_ctrl_list:=TList.Create;

     For i:=0 To ControlCount-1 Do
     Begin
          New(a_ctrl_enabled_state);
          a_ctrl_enabled_state^:=Controls[i].Enabled;
          disabled_ctrl_list.Add(a_ctrl_enabled_state);

          Controls[i].Enabled:=False;
     End;
End;

Procedure T_MSR_form.Enable_controls;
Var i : Integer;
    a_ctrl_enabled_state : ^Boolean;
Begin
     For i:=0 To ControlCount-1 Do
     Begin
          a_ctrl_enabled_state:=disabled_ctrl_list.Items[i];
          If a_ctrl_enabled_state^=True Then
          Begin
               Controls[i].Enabled:=True;
          End;
          Dispose(a_ctrl_enabled_state);
     End;

     disabled_ctrl_list.Clear;
     disabled_ctrl_list.Free;
End;

Procedure T_MSR_form.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

Procedure T_MSR_form.FillSpeedCB;
Var XSpeed    : Word;
    MaxXSpeed : Single;
    s         : String;
    Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;
Begin
     DataReadSpeed_CB.Clear;
     //AudioReadSpeed_CB.Clear;
     Form1.CommonCDSettings.Del_read_speed_list_items;
     //Form1.SCSI.MMC1.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
     Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_sub6_out(Out_CD_cap_mech_st);
     If Not Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          Form1.SCSI.MMC1_any_link.Do_sense6_CD_cap_mech_st_sub6_out(Out_CD_cap_mech_st);
     End;
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          MaxXSpeed:=Out_CD_cap_mech_st.MaxReadSpeed/C_1X_KBYTES_CDSPEED;

          If Trunc(MaxXSpeed)>0 Then
          Begin
               s:='1X, 176 kb/s';
               DataReadSpeed_CB.Items.Add(s);
               //AudioReadSpeed_CB.Items.Add(s);
               Form1.CommonCDSettings.Add_read_speed(176);
          End;
          XSpeed:=2;
          While XSpeed<Trunc(MaxXSpeed) Do
          Begin
               s:=IntToStr(XSpeed)+'X, '+FloatToStrF(XSpeed*C_1X_KBYTES_CDSPEED, ffFixed, 7, 0)+' kb/s';
               DataReadSpeed_CB.Items.Add(s);
               //AudioReadSpeed_CB.Items.Add(s);
               Form1.CommonCDSettings.Add_read_speed(Ceil(XSpeed*C_1X_KBYTES_CDSPEED));
               XSpeed:=XSpeed+2;
          End;
          s:=FormatFloat('#.#', MaxXSpeed);
          If Length(s)>2 Then
          Begin
               If Copy(s, Length(s), 1)='9' Then
               Begin
                    s:=IntToStr(Ceil(MaxXSpeed));
               End
          End;
          s:=s+'X, '+IntToStr(Out_CD_cap_mech_st.MaxReadSpeed)+' kb/s';
          DataReadSpeed_CB.Items.Add(s);
          DataReadSpeed_CB.Items.Add('Max');
          //AudioReadSpeed_CB.Items.Add(s);
          //AudioReadSpeed_CB.Items.Add('Max');
          Form1.CommonCDSettings.Add_read_speed(Out_CD_cap_mech_st.MaxReadSpeed);

          Curr_data_read_speed:=MMC_SET_CD_SPEED_MAX;
          //Curr_audio_read_speed:=MMC_SET_CD_SPEED_MAX;
          DataReadSpeed_CB.ItemIndex:=DataReadSpeed_CB.Items.Count-1;
          //AudioReadSpeed_CB.ItemIndex:=AudioReadSpeed_CB.Items.Count-1;
     End
     Else
         Begin
              s:='Error reading CD capabilities and mechanism status parameters.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;
End;

Procedure T_MSR_form.FillCDComboSelect;
Begin
     Form1.CDFormUtils.FillCB_with_CDROM(CD_dev_CB);
     If CD_dev_CB.Items.Count>0 Then
     Begin
          If Form1.CommonCDSettings.SCSI_select_init_CDROM(0)=True Then //Attempt to select and init first CDROM.
          Begin
               CD_dev_CB.ItemIndex:=0; //Selects first item in combobox list.
               Curr_CDROM_indexNo:=0;

               FillSpeedCB;
          End;
     End
     Else
         MessageDlg('No CD/DVD devices.', mtError, [mbOk], 0);
End;

Procedure T_MSR_form.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;

     GetCDROMListThread:=nil;
     is_busy:=False;
End;

procedure T_MSR_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     If GetCDROMListThread<>nil Then
     Begin
          try
             GetCDROMListThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     elasped_times_str.Free;

     Form1.Set_MSRFormNil; //Prevent main form from showing this form.
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

procedure T_MSR_form.RescanBtnClick(Sender: TObject);
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

procedure T_MSR_form.FormCreate(Sender: TObject);
begin
     Curr_CDROM_indexNo:=0;
     Curr_data_read_speed:=0;
     GetCDROMListThread:=nil;
     SubChSelMode:=MMC_READCD_NO_SUBCH;
     elasped_times_str:=TStringList.Create;

     RescanBtn.Click;
end;

procedure T_MSR_form.CD_dev_CBChange(Sender: TObject);
begin
     If Form1.CommonCDSettings.SCSI_select_init_CDROM(CD_dev_CB.ItemIndex)=False Then //Attempt to select and init first CDROM.
     Begin
          CD_dev_CB.ItemIndex:=Curr_CDROM_indexNo; //Selects prev selected item in combobox list.

          MessageDlg('Cannot initialise device.', mtError, [mbOk], 0);
     End
     Else
     Begin
          Curr_CDROM_indexNo:=CD_dev_CB.ItemIndex;

          FillSpeedCB;
     End;
end;

procedure T_MSR_form.DataReadSpeed_CBChange(Sender: TObject);
Var A_read_speed : T_P_read_speed_rec;
begin
     If DataReadSpeed_CB.ItemIndex<DataReadSpeed_CB.Items.Count-1 Then
     Begin
          A_read_speed:=Form1.CommonCDSettings.Read_speed_list.Items[DataReadSpeed_CB.ItemIndex];
          Curr_data_read_speed:=A_read_speed^.KbSpeed;
     End
     Else
         Curr_data_read_speed:=MMC_SET_CD_SPEED_MAX;
end;

procedure T_MSR_form.LastSectorFromTOC_BtnClick(Sender: TObject);
Var
   First_trkNo, Last_TrkNo : Byte;
   TOC_data : T_out_read_T_P_A_TOC_desc_MMCLBA;
   s : String;
begin
     Form1.SCSI.MMC1_any_link.Do_read_T_P_A_TOC_MMCLBA_first_out_CDB10(First_trkNo,
                                                                       Last_trkNo,
                                                                       TOC_data,
                                                                       0);
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          //Traverse tbe list till the end.  In future improvements I will write
          //retrieve last item directly, so you won't need to traverse.
          Repeat
          Until Form1.SCSI.MMC1_any_link.Do_read_T_P_A_TOC_MMCLBA_next_out_CDB10(TOC_data)=False;
          //Checks if the item is the lead out.
          If TOC_data.TrackNo=$AA Then
          Begin
               LastSector_Edit.Text:=IntToStr(Form1.SCSI.MMC1_any_link.Get_MMC1.MMCLBA_to_LBA(TOC_data.StartMMCLBA)-1);
          End
          Else
              LastSector_Edit.Text:='';
     End
     Else
         Begin
              s:='Error while processing read TOC.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;
end;

procedure T_MSR_form.BTN_measureClick(Sender: TObject);
begin
     Measure;
end;

procedure T_MSR_form.Subch_CBChange(Sender: TObject);
begin
     Case SubCh_CB.ItemIndex Of
          0: SubChSelMode:=MMC_READCD_NO_SUBCH;
          1: SubChSelMode:=MMC_READCD_SUBCH_RAWPW;
          2: SubChSelMode:=MMC_READCD_SUBCH_PACKPW;
          3: SubChSelMode:=MMC_READCD_SUBCH_FORMPQ;
     End;
end;

procedure T_MSR_form.ImageSaveAsBtnClick(Sender: TObject);
begin
     If elasped_times_str.Count=0 Then
     Begin
          MessageDlg('You need to press measure first.', mtError, [mbOk], 0);
     End
     Else
     Begin
          If ImageSaveDialog.Execute Then
          Begin
               elasped_times_str.SaveToFile(ImageSaveDialog.FileName);
          End;
     End;
end;

procedure T_MSR_form.CB_MMC_command_to_measureChange(Sender: TObject);
begin
     If CB_MMC_command_to_measure.ItemIndex=0 Then
     Begin
          Subch_CB.Enabled:=True;
     End
     Else
     Begin
          Subch_CB.Enabled:=False;
     End;
end;

procedure T_MSR_form.BTN_gen_SSPClick(Sender: TObject);
Var i : Integer;
    is_start : Boolean;
    is_sector_ok : Byte;
    sector : LongInt;
    separator_pos : Integer;
    prev_time : Extended;
    curr_time : Extended;
    tolerance_limit : Extended;
    tolerance_val : Extended;
    diff : Extended;
    s : String;
    saved_filter : String;
    saved_default : String;
begin
     If elasped_times_str.Count=0 Then
     Begin
          MessageDlg('You need to press measure first.', mtError, [mbOk], 0);
     End
     Else
     Begin
          saved_filter:=ImageSaveDialog.Filter;
          ImageSaveDialog.Filter:='Sector Skip Position file (.SSP)|*.SSP';

          saved_default:=ImageSaveDialog.DefaultExt;
          ImageSaveDialog.DefaultExt:='*.SSP';

          If ImageSaveDialog.Execute Then
          Begin
               SSP_entries:=TStringList.Create;

               tolerance_limit:=1.5;

               is_start:=True;

               For i:=1 To elasped_times_str.Count-1 Do
               Begin
                    is_sector_ok:=StrToInt(Copy(elasped_times_str[i], 1, 1));

                    If is_sector_ok=1 Then
                    Begin
                         s:=Copy(elasped_times_str[i], 3, Length(elasped_times_str[i])-2);
                         separator_pos:=Pos(',', s);
                         sector:=StrToInt(Copy(s, 1, separator_pos-1));

                         s:=Copy(s, separator_pos+1, Length(s)-separator_pos);
                         separator_pos:=Pos(',', s);
                         If separator_pos=0 Then separator_pos:=Length(s)+1;

                         If is_start Then
                         Begin
                              prev_time:=StrToFloat(Copy(s, 1, separator_pos-1));
                              s:=Copy(s, 1, separator_pos-1);
                              MessageDlg(s, mtError, [mbOk], 0);
                              is_start:=False;
                         End
                         Else
                         Begin
                              curr_time:=StrToFloat(Copy(s, 1, separator_pos-1));

                              If curr_time>prev_time Then
                              Begin
                                   tolerance_val:=curr_time/prev_time;
                              End
                              Else
                              Begin
                                   tolerance_val:=prev_time/curr_time;
                              End;

                              If tolerance_val>tolerance_limit Then
                              Begin
                                   s:=IntToStr(sector)+','+FloatToStrF(tolerance_val, ffFixed, 18, 0);
                                   SSP_entries.Add(s);
                              End
                              Else
                              Begin
                                   prev_time:=curr_time;
                              End;
                         End;
                    End;
               End;

               SSP_entries.SaveToFile(ImageSaveDialog.FileName);

               SSP_entries.Free;
          End;

          ImageSaveDialog.Filter:=saved_filter;
          ImageSaveDialog.DefaultExt:=saved_default;
     End;
end;

Procedure T_MSR_form.Measure;
Var
   First_sector    : LongInt;
   Last_sector     : LongInt;
   {Data_1block_size  : Word;
   Sector_block_size : Word;
   SubCh_block_size  : Byte;}
Begin
     First_sector:=StrToIntDef(FirstSector_Edit.Text, 0);
     Last_sector:=StrToIntDef(LastSector_Edit.Text, 0);
     If Last_sector>=First_sector Then
     Begin
          elasped_times_str.Clear;

          {Sector_block_size:=Form1.SCSI.MMC1.GetBlockSizeFromFilterReadFormat(MMC_SECTORTYPE_ANY,
                                                                              MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC);
          SubCh_block_size:=Form1.SCSI.MMC1.GetBlockSizeFromSubCh(SubChSelMode);
          Data_1block_size:=SubCh_block_size+Sector_block_size;}

          //Form1.SCSI.SPC_any_link.Zero_data_buf;

          Form1.SCSI.MMC1_any_link.Do_set_CD_speed_CDB12(curr_data_read_speed, MMC_SET_CD_SPEED_MAX);

          If MSRProgressForm=Nil Then
          Begin
               MSRProgressForm:=TMSRProgressForm.Create(MSRProgressForm_close, MSR_form);
               MSRProgressForm.Start_thread(First_sector,
                                            Last_sector,
                                            CHK_save_rep_speed.Checked,
                                            elasped_times_str,
                                            CB_MMC_command_to_measure.ItemIndex,
                                            SubChSelMode);
          End;
     End
     Else
     Begin
          MessageDlg('Last sector must be greater than First sector.', mtError, [mbOk], 0);
     End;
End;

Procedure T_MSR_form.MSRProgressForm_close(Sender: TObject);
Begin
     MSRProgressForm:=Nil;

     Self.Enabled:=True;
End;

procedure T_MSR_form.FormDestroy(Sender: TObject);
begin
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

end.
