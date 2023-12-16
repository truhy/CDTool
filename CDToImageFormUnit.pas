unit CDToImageFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Math, CDToImageProgressFormUnit, MMC1Unit, GetCDROMListThreadUnit,
  CommonCDSettingsUnit;

type
  TForm4 = class(TForm)
    CD_dev_CB: TComboBox;
    RescanBtn: TButton;
    ImageNameEdit: TEdit;
    ImageSaveAsBtn: TButton;
    ImageSaveDialog: TSaveDialog;
    DataReadSpeed_CB: TComboBox;
    AudioReadSpeed_CB: TComboBox;
    DataReadSpeed_Lbl: TLabel;
    AudioReadSpeed_Lbl: TLabel;
    ReadCDBtn: TButton;
    ImageFileName_Lbl: TLabel;
    FirstSector_Edit: TEdit;
    LastSector_Edit: TEdit;
    FirstSector_Lbl: TLabel;
    LastSector_Lbl: TLabel;
    LastSectorFromTOC_Btn: TButton;
    TEC_Check: TCheckBox;
    CDModeFilter_CB: TComboBox;
    CDModeFilter_Lbl: TLabel;
    Subch_CB: TComboBox;
    Subch_Lbl: TLabel;
    Device_Lbl: TLabel;
    TB_retry_count: TEdit;
    Lbl_retry_error_count: TLabel;
    CHK_SeparateFiles: TCheckBox;
    CHK_deinterleave: TCheckBox;
    CHK_NoECC_TEC: TCheckBox;
    CHK_full_LI_LO_files: TCheckBox;
    ED_LI_size: TEdit;
    ED_LO_size: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CB_lead_in_type: TComboBox;
    Label3: TLabel;
    BTN_read_ATIP: TButton;
    CB_TOC_reading_mode: TComboBox;
    Label4: TLabel;
    CB_c2_error_options: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    CB_C2_buffer_order: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CD_dev_CBChange(Sender: TObject);
    procedure RescanBtnClick(Sender: TObject);
    procedure ImageSaveAsBtnClick(Sender: TObject);
    procedure DataReadSpeed_CBChange(Sender: TObject);
    procedure AudioReadSpeed_CBChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LastSectorFromTOC_BtnClick(Sender: TObject);
    procedure TEC_CheckClick(Sender: TObject);
    procedure ReadCDBtnClick(Sender: TObject);
    procedure Subch_CBChange(Sender: TObject);
    procedure CDModeFilter_CBChange(Sender: TObject);
    procedure TB_retry_countKeyPress(Sender: TObject; var Key: Char);
    procedure TB_retry_countExit(Sender: TObject);
    procedure CHK_NoECC_TECClick(Sender: TObject);
    procedure CHK_full_LI_LO_filesClick(Sender: TObject);
    procedure BTN_read_ATIPClick(Sender: TObject);
    procedure CB_c2_error_optionsChange(Sender: TObject);
    procedure CB_C2_buffer_orderChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Curr_CDROM_indexNo      : Integer;
    Curr_data_read_speed    : Word;
    Curr_audio_read_speed   : Word;
    FilterMode              : Byte;
    SubChSelMode            : Byte;
    C2_error_mode           : Byte;
    C2_read_order           : Byte;
    f_main                  : TFileStream;
    f_subch                 : TFileStream;
    f_toc                   : TFileStream;
    f_pregap1               : TFileStream;
    f_LO                    : TFileStream;
    f_C2                    : TFileStream;
    Curr_read_err_rec_param : Byte;
    Curr_retry_count        : Byte;
    New_retry_count         : Byte;
    is_TEC_check_disable_code : Boolean;
    is_CHK_NoECC_TEC_disable_code : Boolean;
    is_busy : Boolean;
    disabled_ctrl_list : TList;
    GetCDROMListThread : T_GetCDROMListThread;
    toc_file_name  : String;
    main_file_name : String;
    sub_file_name  : String;
    pregap1_file_name  : String;
    LO_file_name   : String;
    C2_file_name   : String;
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure ShowErrMsg(s : String);
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
    Procedure FillCDComboSelect;
    Procedure FillSpeedCB;
    Procedure ReadErrorRetryCount;
    Procedure SetErrorRetryCount;
    Procedure CDToImageProgressForm_close(Sender: TObject);
  public
    { Public declarations }
    CDToImageProgressForm  : TCDToImageProgressForm;
  end;

var
  Form4: TForm4;

implementation

uses MainFormUnit;

{$R *.DFM}

Procedure TForm4.Disable_controls;
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

Procedure TForm4.Enable_controls;
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

Procedure TForm4.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

Procedure TForm4.FillCDComboSelect;
Begin
     Form1.CDFormUtils.FillCB_with_CDROM(CD_dev_CB);
     If CD_dev_CB.Items.Count>0 Then
     Begin
          If Form1.CommonCDSettings.SCSI_select_init_CDROM(0)=True Then //Attempt to select and init first CDROM.
          Begin
               CD_dev_CB.ItemIndex:=0; //Selects first item in combobox list.
               Curr_CDROM_indexNo:=0;

               ReadErrorRetryCount;
               FillSpeedCB;
          End;
     End
     Else
         MessageDlg('No CD/DVD devices.', mtError, [mbOk], 0);
End;

Procedure TForm4.FillSpeedCB;
Var XSpeed    : Word;
    MaxXSpeed : Single;
    s         : String;
    Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;
Begin
     DataReadSpeed_CB.Clear;
     AudioReadSpeed_CB.Clear;
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
               AudioReadSpeed_CB.Items.Add(s);
               Form1.CommonCDSettings.Add_read_speed(176);
          End;
          XSpeed:=2;
          While XSpeed<Trunc(MaxXSpeed) Do
          Begin
               s:=IntToStr(XSpeed)+'X, '+FloatToStrF(XSpeed*C_1X_KBYTES_CDSPEED, ffFixed, 7, 0)+' kb/s';
               DataReadSpeed_CB.Items.Add(s);
               AudioReadSpeed_CB.Items.Add(s);
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
          AudioReadSpeed_CB.Items.Add(s);
          AudioReadSpeed_CB.Items.Add('Max');
          Form1.CommonCDSettings.Add_read_speed(Out_CD_cap_mech_st.MaxReadSpeed);

          Curr_data_read_speed:=MMC_SET_CD_SPEED_MAX;
          Curr_audio_read_speed:=MMC_SET_CD_SPEED_MAX;
          DataReadSpeed_CB.ItemIndex:=DataReadSpeed_CB.Items.Count-1;
          AudioReadSpeed_CB.ItemIndex:=AudioReadSpeed_CB.Items.Count-1;
     End
     Else
         Begin
              s:='Error reading CD capabilities and mechanism status parameters.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;
End;

Procedure TForm4.ReadErrorRetryCount;
Begin
     Form1.SCSI.MMC1_any_link.Do_sense10_read_err_rec;
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          Curr_retry_count:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Read_retry_count;
          Curr_read_err_rec_param:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Err_rec_param;

          is_TEC_check_disable_code:=True;
          If Curr_read_err_rec_param=MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TEC Then
          Begin
               TEC_check.Checked:=True;
          End
          Else
          Begin
               TEC_check.Checked:=False;
          End;
          is_TEC_check_disable_code:=False;

          is_CHK_NoECC_TEC_disable_code:=True;
          If Curr_read_err_rec_param=MMC_MODE_PARAM_READ_ERR_REC_CIRC_TEC Then
          Begin
               CHK_NoECC_TEC.Checked:=True;
          End
          Else
          Begin
               CHK_NoECC_TEC.Checked:=False;
          End;
          is_CHK_NoECC_TEC_disable_code:=False;

          TB_retry_count.Text:=IntToStr(Curr_retry_count);
          TB_retry_count.Enabled:=True;
     End
     Else
     Begin
          TB_retry_count.Enabled:=False;
     End;
End;

Procedure TForm4.SetErrorRetryCount;
Var
   s : String;
   Err_rec_param : Byte;
Begin
     Form1.SCSI.MMC1_any_link.Do_sense10_read_err_rec;
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          If TEC_Check.Checked Then
          Begin
               Err_rec_param:=MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TEC;
          End
          Else If CHK_NoECC_TEC.Checked Then
               Begin
                    Err_rec_param:=MMC_MODE_PARAM_READ_ERR_REC_CIRC_TEC;
               End
               Else
                   Err_rec_param:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Err_rec_param;

          If Form1.CommonCDSettings.Set_read_err_rec_verify_count(Err_rec_param, New_retry_count) Then
          Begin
               Curr_retry_count:=New_retry_count;
          End
          Else
          Begin
               s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
               s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               MessageDlg(s, mtError, [mbOk], 0);
          End;
     End
     Else
     Begin
          s:='Could not read the read retry count from the drive.' + Chr(10) + Chr(13);
          s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
          s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
          MessageDlg(s, mtError, [mbOk], 0);
     End;

     TB_retry_count.Text:=IntToStr(Curr_retry_count);
End;

Procedure TForm4.CDToImageProgressForm_close(Sender: TObject);
Begin
     CDToImageProgressForm:=Nil;
     If f_main.Size=0 Then
     Begin
          f_main.Free;
          f_main:=nil;
          DeleteFile(main_file_name);
     End
     Else
     Begin
          f_main.Free;
          f_main:=nil;
     End;

     If f_subch<>nil Then
     Begin
          If f_subch.Size=0 Then
          Begin
               f_subch.Free;
               f_subch:=nil;
               DeleteFile(sub_file_name);
          End
          Else
          Begin
               f_subch.Free;
               f_subch:=nil;
          End;
     End;

     If f_toc<>nil Then
     Begin
          If f_toc.Size=0 Then
          Begin
               f_toc.Free;
               f_toc:=nil;
               DeleteFile(toc_file_name);
          End
          Else
          Begin
               f_toc.Free;
               f_toc:=nil;
          End;
     End;

     If f_pregap1<>nil Then
     Begin
          If f_pregap1.Size=0 Then
          Begin
               f_pregap1.Free;
               f_pregap1:=nil;
               DeleteFile(pregap1_file_name);
          End
          Else
          Begin
               f_pregap1.Free;
               f_pregap1:=nil;
          End;
     End;

     If f_LO<>nil Then
     Begin
          If f_LO.Size=0 Then
          Begin
               f_LO.Free;
               f_LO:=nil;
               DeleteFile(LO_file_name);
          End
          Else
          Begin
               f_LO.Free;
               f_LO:=nil;
          End;
     End;

     If f_C2<>nil Then
     Begin
          If f_C2.Size=0 Then
          Begin
               f_C2.Free;
               f_C2:=nil;
               DeleteFile(C2_file_name);
          End
          Else
          Begin
               f_C2.Free;
               f_C2:=nil;
          End;
     End;

     Self.Enabled:=True;
End;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     If GetCDROMListThread<>nil Then
     Begin
          try
             GetCDROMListThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     Form1.SetCDToImageFormNil;
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

procedure TForm4.CD_dev_CBChange(Sender: TObject);
begin
     If Form1.CommonCDSettings.SCSI_select_init_CDROM(CD_dev_CB.ItemIndex)=True Then //Attempt to select and init first CDROM.
     Begin
          Curr_CDROM_indexNo:=CD_dev_CB.ItemIndex;

          FillSpeedCB;
     End
     Else
     Begin
          CD_dev_CB.ItemIndex:=Curr_CDROM_indexNo; //Selects prev selected item in combobox list.

          MessageDlg('Cannot initialise device.', mtError, [mbOk], 0);
     End;

     ReadErrorRetryCount;
end;

procedure TForm4.RescanBtnClick(Sender: TObject);
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

Procedure TForm4.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;
     GetCDROMListThread:=nil;
     is_busy:=False;

     ReadErrorRetryCount;
End;

procedure TForm4.ImageSaveAsBtnClick(Sender: TObject);
begin
     If ImageSaveDialog.Execute Then
     Begin
          ImageNameEdit.Text:=ImageSaveDialog.FileName;
     End;
end;

procedure TForm4.DataReadSpeed_CBChange(Sender: TObject);
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

procedure TForm4.AudioReadSpeed_CBChange(Sender: TObject);
Var A_read_speed : T_P_read_speed_rec;
begin
     If AudioReadSpeed_CB.ItemIndex<AudioReadSpeed_CB.Items.Count-1 Then
     Begin
          A_read_speed:=Form1.CommonCDSettings.Read_speed_list.Items[AudioReadSpeed_CB.ItemIndex];
          Curr_audio_read_speed:=A_read_speed^.KbSpeed;
     End
     Else
         Curr_audio_read_speed:=MMC_SET_CD_SPEED_MAX;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
     is_TEC_check_disable_code:=False;
     Curr_data_read_speed:=0;
     Curr_audio_read_speed:=0;
     SubChSelMode:=MMC_READCD_SUBCH_RAWPW;
     FilterMode:=MMC_SECTORTYPE_ANY;
     C2_error_mode:=MMC_READCD_C2_NONE;
     C2_read_order:=0;

     f_main:=nil;
     f_subch:=nil;
     f_toc:=nil;
     f_pregap1:=nil;
     f_LO:=nil;
     f_C2:=nil;

     GetCDROMListThread:=nil;

     RescanBtn.Click;
end;

procedure TForm4.LastSectorFromTOC_BtnClick(Sender: TObject);
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
               LastSector_Edit.Text:=IntToStr(TOC_data.StartMMCLBA-1);
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

procedure TForm4.TEC_CheckClick(Sender: TObject);
Var
   s : String;
begin
     If Not is_TEC_check_disable_code Then
     Begin
         If TEC_Check.Checked=True Then
         Begin
              If Not Form1.CommonCDSettings.Set_read_err_rec_verify_param(MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TEC, Curr_retry_count) Then
              Begin
                   s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
                   s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
                   s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
                   MessageDlg(s, mtError, [mbOk], 0);

                   is_TEC_check_disable_code:=True;
                   TEC_Check.Checked:=False;
                   is_TEC_check_disable_code:=False;
              End
              Else
              Begin
                   is_CHK_NoECC_TEC_disable_code:=True;
                   CHK_NoECC_TEC.Checked:=False;
                   is_CHK_NoECC_TEC_disable_code:=False;
              End;
         End
         Else
         Begin
              If Not Form1.CommonCDSettings.Set_read_err_rec_verify_param(MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC, Curr_retry_count) Then
              Begin
                   s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
                   s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
                   s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
                   MessageDlg(s, mtError, [mbOk], 0);

                   is_TEC_check_disable_code:=True;
                   TEC_Check.Checked:=True;
                   is_TEC_check_disable_code:=False;
              End
              Else
              Begin
                   is_CHK_NoECC_TEC_disable_code:=True;
                   CHK_NoECC_TEC.Checked:=False;
                   is_CHK_NoECC_TEC_disable_code:=False;
              End;
         End;
     End;
end;

procedure TForm4.ReadCDBtnClick(Sender: TObject);
Var
   N_Sect          : LongWord;
   First_sector    : LongInt;
   Last_sector     : LongInt;
   Lead_in_size    : LongInt;
   Lead_out_size   : LongInt;
   is_file_valid   : Boolean;
   s               : String;
begin
     If ImageNameEdit.Text<>'' Then
     Begin
          If Length(LastSector_Edit.Text)>0 Then
          Begin
               First_sector:=StrToIntDef(FirstSector_Edit.Text, 0);
               Last_sector:=StrToIntDef(LastSector_Edit.Text, 0);
               If Last_sector>=First_sector Then
               Begin
                    Lead_in_size:=StrToIntDef(ED_LI_size.Text, 0);
                    Lead_out_size:=StrToIntDef(ED_LO_size.Text, 0);
                    If (CHK_full_LI_LO_files.Checked And
                       ((Lead_in_size<=0) Or (Lead_out_size<=0))) Then
                    Begin
                         MessageDlg('Lead-in/lead-out sizes must be greater than 0.', mtError, [mbOk], 0);
                    End
                    Else
                    Begin
                         If CHK_full_LI_LO_files.Checked Then
                         Begin
                              If CHK_SeparateFiles.Checked Then
                              Begin
                                   toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.LI');
                                   pregap1_file_name:=ChangeFileExt(ImageNameEdit.Text, '.PG1');
                                   LO_file_name:=ChangeFileExt(ImageNameEdit.Text, '.LO');
                                   main_file_name:=ChangeFileExt(ImageNameEdit.Text, '.IMG');
                              End
                              Else
                              Begin
                                   main_file_name:=ChangeFileExt(ImageNameEdit.Text, '.DAO');
                              End;
                         End
                         Else
                         Begin
                              If CHK_deinterleave.Checked Then
                                  toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.TU')
                              Else
                                  toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.TUI');

                              main_file_name:=ChangeFileExt(ImageNameEdit.Text, '.IMG');
                         End;

                         is_file_valid:=True;
                         If FileExists(main_file_name) Then
                         Begin
                              s:='File exists.: ' + main_file_name + Chr(10) + Chr(13);
                              s:=s + 'Do you want to replace the file?';
                              If MessageDlg(s, mtWarning, [mbYes, mbNo], 0)=mrNo Then
                              Begin
                                   is_file_valid:=False;
                              End;
                         End;

                         If is_file_valid Then
                         Begin
                              Try
                                 f_main:=TFileStream.Create(main_file_name, fmCreate);
                              Except
                                 On EFCreateError Do
                                 Begin
                                      f_main:=nil;
                                      is_file_valid:=False;
                                      MessageDlg('Cannot create main file '+main_file_name, mtError, [mbOk], 0);
                                 End;
                              End;
                         End;

                         //If not DAO file type.
                         If Not((CHK_full_LI_LO_files.Checked=True) And
                                (CHK_SeparateFiles.Checked=False)) Then
                         Begin
                              //Create sub-channel file
                              If is_file_valid Then
                              Begin
                                   If SubChSelMode<>MMC_READCD_NO_SUBCH Then
                                   Begin
                                        If CHK_deinterleave.Checked Then
                                            sub_file_name:=ChangeFileExt(ImageNameEdit.Text, '.SUB')
                                        Else
                                            sub_file_name:=ChangeFileExt(ImageNameEdit.Text, '.SBI');

                                        Try
                                           f_subch:=TFileStream.Create(sub_file_name, fmCreate);
                                        Except
                                           On EFCreateError Do
                                           Begin
                                                f_subch:=nil;
                                                f_main.Free;
                                                f_main:=nil;
                                                DeleteFile(main_file_name);

                                                is_file_valid:=False;
                                                MessageDlg('Cannot create sub file '+sub_file_name, mtError, [mbOk], 0);
                                           End;
                                        End;
                                   End;
                              End;

                              //Create toc/lead-in file
                              If is_file_valid Then
                              Begin
                                   Try
                                      f_toc:=TFileStream.Create(toc_file_name, fmCreate);
                                   Except
                                      On EFCreateError Do
                                      Begin
                                           f_toc:=nil;
                                           f_main.Free;
                                           f_subch.Free;
                                           f_main:=nil;
                                           f_subch:=nil;
                                           DeleteFile(main_file_name);
                                           DeleteFile(sub_file_name);
                                           is_file_valid:=False;
                                           MessageDlg('Cannot create TOC/lead-in file '+toc_file_name, mtError, [mbOk], 0);
                                      End;
                                   End;
                              End;

                              //Create C2 error report file
                              If is_file_valid Then
                              Begin
                                   If C2_error_mode<>MMC_READCD_C2_NONE Then
                                   Begin
                                        Case C2_error_mode Of
                                             MMC_READCD_C2_PTR:
                                             Begin
                                                  C2_file_name:=ChangeFileExt(ImageNameEdit.Text, '.C2');

                                             End;
                                             MMC_READCD_C2_PTR_BLK:
                                             Begin
                                                  C2_file_name:=ChangeFileExt(ImageNameEdit.Text, '.C2+');
                                             End;
                                        End;

                                        Try
                                           f_C2:=TFileStream.Create(C2_file_name, fmCreate);
                                        Except
                                           On EFCreateError Do
                                           Begin
                                                f_C2:=nil;
                                                f_toc.Free;
                                                f_main.Free;
                                                f_subch.Free;
                                                f_main:=nil;
                                                f_subch:=nil;
                                                f_toc:=nil;
                                                DeleteFile(main_file_name);
                                                DeleteFile(sub_file_name);
                                                DeleteFile(toc_file_name);
                                                is_file_valid:=False;
                                                MessageDlg('Cannot create C2 file '+C2_file_name, mtError, [mbOk], 0);
                                           End;
                                       End;
                                   End;
                              End;

                              If CHK_full_LI_LO_files.Checked Then
                              Begin
                                   //Create 1st pregap file
                                   If is_file_valid Then
                                   Begin
                                        Try
                                           f_pregap1:=TFileStream.Create(pregap1_file_name, fmCreate);
                                        Except
                                           On EFCreateError Do
                                           Begin
                                                f_pregap1:=nil;
                                                f_main.Free;
                                                f_subch.Free;
                                                f_toc.Free;
                                                f_main:=nil;
                                                f_subch:=nil;
                                                f_toc:=nil;
                                                DeleteFile(main_file_name);
                                                DeleteFile(sub_file_name);
                                                DeleteFile(toc_file_name);
                                                is_file_valid:=False;
                                                MessageDlg('Cannot create first pregap file '+pregap1_file_name, mtError, [mbOk], 0);
                                           End;
                                        End;
                                   End;

                                   //Create lead-out file
                                   If is_file_valid Then
                                   Begin
                                        Try
                                           f_LO:=TFileStream.Create(LO_file_name, fmCreate);
                                        Except
                                           On EFCreateError Do
                                           Begin
                                                f_LO:=nil;
                                                f_main.Free;
                                                f_subch.Free;
                                                f_toc.Free;
                                                f_pregap1.Free;
                                                f_main:=nil;
                                                f_subch:=nil;
                                                f_toc:=nil;
                                                f_pregap1:=nil;
                                                DeleteFile(main_file_name);
                                                DeleteFile(sub_file_name);
                                                DeleteFile(toc_file_name);
                                                DeleteFile(pregap1_file_name);
                                                is_file_valid:=False;
                                                MessageDlg('Cannot create lead-out file '+LO_file_name, mtError, [mbOk], 0);
                                           End;
                                        End;
                                   End;
                              End;
                         End;

                         If is_file_valid Then
                         Begin
                              //MessageDlg(IntToStr(Form1.SCSI.MMC1_any_link.Get_MMC1.GetBlockSizeFromC2ErrorField(MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC Or C2_error_mode)), mtError, [mbOk], 0);;
                               N_Sect:=Last_sector-First_sector+1;

                               If CDToImageProgressForm=Nil Then
                               Begin
                                    CDToImageProgressForm:=TCDToImageProgressForm.Create(CDToImageProgressForm_close, Form4);
                                    CDToImageProgressForm.Start_thread(First_sector,
                                                                       N_Sect,
                                                                       curr_data_read_speed,
                                                                       FilterMode,
                                                                       SubChSelMode,
                                                                       TEC_Check.Checked,
                                                                       f_main,
                                                                       f_subch,
                                                                       f_toc,
                                                                       f_pregap1,
                                                                       f_LO,
                                                                       f_C2,
                                                                       C2_error_mode,
                                                                       C2_read_order,
                                                                       CB_lead_in_type.ItemIndex,
                                                                       Lead_in_size,
                                                                       Lead_out_size,
                                                                       CHK_deinterleave.Checked);
                               End;
                               If CDToImageProgressForm<>Nil Then
                               Begin
                                    CDToImageProgressForm.Show;
                                    Self.Enabled:=False;
                               End;
                         End;
                    End;
               End
               Else
               Begin
                    MessageDlg('Last sector must be greater than First sector.', mtError, [mbOk], 0);
               End;
          End
          Else
          Begin
               MessageDlg('Please enter last sector.', mtError, [mbOk], 0);
          End;
     End
     Else
     Begin
          MessageDlg('Please enter or browse a path and a filename for the image file.', mtError, [mbOk], 0);
     End;
end;

procedure TForm4.Subch_CBChange(Sender: TObject);
begin
     Case SubCh_CB.ItemIndex Of
          0: Begin
                  SubChSelMode:=MMC_READCD_NO_SUBCH;
                  CHK_deinterleave.Enabled := False;
                  CHK_deinterleave.Checked := False;
             End;
          1: Begin
                  SubChSelMode:=MMC_READCD_SUBCH_RAWPW;
                  CHK_deinterleave.Enabled := True;
             End;
          2: Begin
                  SubChSelMode:=MMC_READCD_SUBCH_PACKPW;
                  CHK_deinterleave.Enabled := False;
                  CHK_deinterleave.Checked := False;
             End;
          3: Begin
                  SubChSelMode:=MMC_READCD_SUBCH_FORMPQ;
                  CHK_deinterleave.Enabled := False;
                  CHK_deinterleave.Checked := False;
             End;
     End;
end;

procedure TForm4.CDModeFilter_CBChange(Sender: TObject);
begin
     Case CDModeFilter_CB.ItemIndex Of
          0: FilterMode:=MMC_SECTORTYPE_ANY;
          1: FilterMode:=MMC_SECTORTYPE_CDDA;
          2: FilterMode:=MMC_SECTORTYPE_MODE1;
          3: FilterMode:=MMC_SECTORTYPE_MODE2FORMLESS;
          4: FilterMode:=MMC_SECTORTYPE_MODE2FORM1;
          5: FilterMode:=MMC_SECTORTYPE_MODE2FORM2;
     End;
end;

procedure TForm4.TB_retry_countKeyPress(Sender: TObject; var Key: Char);
Var
   New_retry_count_int : Integer;
begin
     Case Key Of
          Chr(13):
          Begin
               New_retry_count_int:=StrToIntDef(TB_retry_count.Text, Curr_retry_count);
               If (New_retry_count_int>=0) AND (New_retry_count_int<=255) Then
               Begin
                    New_retry_count:=New_retry_count_int;
                    SetErrorRetryCount;
               End
               Else
               Begin
                    MessageDlg('Read retry count must be a number in the range 0..255', mtError, [mbOk], 0);
               End;
          End;
     End;
end;

procedure TForm4.TB_retry_countExit(Sender: TObject);
Var
   New_retry_count_int : Integer;
begin
     If IntToStr(Curr_retry_count)<>TB_retry_count.Text Then
     Begin
          New_retry_count_int:=StrToIntDef(TB_retry_count.Text, Curr_retry_count);
          If (New_retry_count_int>=0) AND (New_retry_count_int<=255) Then
          Begin
               New_retry_count:=New_retry_count_int;
               SetErrorRetryCount;
          End
          Else
          Begin
               //MessageDlg('Read retry count must be a number in the range 0..255', mtError, [mbOk], 0);
               TB_retry_count.Text:=IntToStr(Curr_retry_count);
          End;
     End;
end;

procedure TForm4.CHK_NoECC_TECClick(Sender: TObject);
Var
   s : String;
begin
     If Not is_CHK_NoECC_TEC_disable_code Then
     Begin
         If CHK_NoECC_TEC.Checked=True Then
         Begin
              If Not Form1.CommonCDSettings.Set_read_err_rec_verify_param(MMC_MODE_PARAM_READ_ERR_REC_CIRC_TEC, Curr_retry_count) Then
              Begin
                   s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
                   s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
                   s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
                   MessageDlg(s, mtError, [mbOk], 0);

                   is_CHK_NoECC_TEC_disable_code:=True;
                   CHK_NoECC_TEC.Checked:=False;
                   is_CHK_NoECC_TEC_disable_code:=False;
              End
              Else
              Begin
                   is_TEC_check_disable_code:=True;
                   TEC_check.Checked:=False;
                   is_TEC_check_disable_code:=False;
              End;
         End
         Else
         Begin
              If Not Form1.CommonCDSettings.Set_read_err_rec_verify_param(MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC, Curr_retry_count) Then
              Begin
                   s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
                   s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
                   s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
                   MessageDlg(s, mtError, [mbOk], 0);

                   is_CHK_NoECC_TEC_disable_code:=True;
                   CHK_NoECC_TEC.Checked:=True;
                   is_CHK_NoECC_TEC_disable_code:=False;
              End
              Else
              Begin
                   is_TEC_check_disable_code:=True;
                   TEC_check.Checked:=False;
                   is_TEC_check_disable_code:=False;
              End;
         End;
     End;
end;

procedure TForm4.CHK_full_LI_LO_filesClick(Sender: TObject);
begin
     If CHK_full_LI_LO_files.Checked Then
     Begin
          CHK_SeparateFiles.Enabled:=True;
     End
     Else
     Begin
          CHK_SeparateFiles.Enabled:=False;
     End;
end;

procedure TForm4.BTN_read_ATIPClick(Sender: TObject);
Var
   There_is_ATIP_data : Boolean;
   Out_ATIP_desc : T_out_read_T_P_A_ATIP_desc;
begin
     There_is_ATIP_data:=Form1.SCSI.MMC1_any_link.Do_read_T_P_A_ATIP_out_CDB10(Out_ATIP_desc);
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          If There_is_ATIP_data Then
          Begin
               //Convert ATIP (MSF) to MMCLBA units and convert to positive.
               ED_LI_size.Text:=IntToStr(Abs(Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_MMCLBA(Out_ATIP_desc.StartMin,
                                                                                             Out_ATIP_desc.StartSec,
                                                                                             Out_ATIP_desc.StartFrame)+150));

          End
          Else
          Begin
               MessageDlg('Read TOC/ATIP command was sent, but there is no ATIP info returned.',
                           mtError, [mbOk], 0);
          End;
     End
     Else
     Begin
          MessageDlg('Error while processing read TOC/ATIP command.' +
                      Chr(10) + Chr(13) +
                      Form1.SCSI.MMC1_any_link.Get_err_msg,
                      mtError, [mbOk], 0);
     End;
end;

procedure TForm4.CB_c2_error_optionsChange(Sender: TObject);
begin
     Case CB_c2_error_options.ItemIndex Of
          0: Begin
                  C2_error_mode:=MMC_READCD_C2_NONE;
             End;
          1: Begin
                  C2_error_mode:=MMC_READCD_C2_PTR;
             End;
          2: Begin
                  C2_error_mode:=MMC_READCD_C2_PTR_BLK;
             End;
     End;
end;

procedure TForm4.CB_C2_buffer_orderChange(Sender: TObject);
begin
     Case CB_C2_buffer_order.ItemIndex Of
          0: Begin
                  C2_read_order:=0;
             End;
          1: Begin
                  C2_read_order:=1;
             End;
     End;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

end.
