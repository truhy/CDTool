unit ImageToCDFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math, ImageToCDProgressFormUnit, GetCDROMListThreadUnit, MMC1Unit,
  CommonCDSettingsUnit, TOCUnit, CDROM_struct_Unit, ExtCtrls;

type
  TForm5 = class(TForm)
    Device_Lbl: TLabel;
    CD_dev_CB: TComboBox;
    RescanBtn: TButton;
    DataWriteSpeed_Lbl: TLabel;
    DataWriteSpeed_CB: TComboBox;
    ImageFileName_Lbl: TLabel;
    ImageNameEdit: TEdit;
    ImageSaveAsBtn: TButton;
    BTN_write: TButton;
    OpenDialog1: TOpenDialog;
    ED_SSP_file_name: TEdit;
    BTN_browse_SSP: TButton;
    CB_lead_in_type: TComboBox;
    Label3: TLabel;
    Label1: TLabel;
    ED_start_LBA: TEdit;
    BTN_read_ATIP: TButton;
    RG_ssp_method: TRadioGroup;
    CHK_show_all_CDROMs: TCheckBox;
    CHK_specify_start_write: TCheckBox;
    BTN_set_PG1_at_0: TButton;
    CHK_test_write: TCheckBox;
    CB_write_type: TComboBox;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CD_dev_CBChange(Sender: TObject);
    procedure RescanBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageSaveAsBtnClick(Sender: TObject);
    procedure DataWriteSpeed_CBChange(Sender: TObject);
    procedure BTN_writeClick(Sender: TObject);
    procedure BTN_browse_SSPClick(Sender: TObject);
    procedure ED_start_LBAKeyPress(Sender: TObject; var Key: Char);
    procedure ED_start_LBAExit(Sender: TObject);
    procedure BTN_read_ATIPClick(Sender: TObject);
    procedure CHK_show_all_CDROMsClick(Sender: TObject);
    procedure CHK_specify_start_writeClick(Sender: TObject);
    procedure BTN_set_PG1_at_0Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Curr_CDROM_indexNo : Integer;
    is_busy            : Boolean;
    disabled_ctrl_list : TList;
    GetCDROMListThread : T_GetCDROMListThread;
    Curr_data_write_speed : Word;
    toc_file_name  : String;
    main_file_name : String;
    sub_file_name  : String;
    pregap1_file_name : String;
    LO_file_name : String;
    lead_in_file_type : Byte;
    f_toc          : TFileStream;
    f_main         : TFileStream;
    f_subch        : TFileStream;
    f_pregap1      : TFileStream;
    f_LO           : TFileStream;
    SSP_file       : TextFile;
    Start_LBA      : Integer;
    Start_MMCLBA   : Integer;
    Out_ATIP_desc  : T_out_read_T_P_A_ATIP_desc;
  public
    { Public declarations }
    ImageToCDProgressForm : TImageToCDProgressForm;
    Procedure ImageToCDProgressForm_close(Sender: TObject);
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure FillCDComboSelect;
    Procedure FillSpeedCB;
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
    Procedure ShowErrMsg(s : String);
    Procedure Read_ATIP;
  end;

var
  Form5: TForm5;

implementation

uses MainFormUnit;

{$R *.dfm}

Procedure TForm5.ImageToCDProgressForm_close(Sender: TObject);
Begin
     ImageToCDProgressForm:=nil;
     f_toc.Free;
     f_toc:=nil;
     f_main.Free;
     f_main:=nil;
     f_subch.Free;
     f_subch:=nil;
     f_pregap1.Free;
     f_pregap1:=nil;
     f_LO.Free;
     f_LO:=nil;
     If (RG_ssp_method.ItemIndex=1) Or
        (RG_ssp_method.ItemIndex=2) Or
        (RG_ssp_method.ItemIndex=3) Then
        CloseFile(SSP_file);
     Self.Enabled:=True;
End;

Procedure TForm5.Disable_controls;
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

Procedure TForm5.Enable_controls;
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

Procedure TForm5.FillCDComboSelect;
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

Procedure TForm5.FillSpeedCB;
Var XSpeed    : Word;
    MaxXSpeed : Single;
    s         : String;
    Out_CD_cap_mech_st : T_pub_CD_cap_mech_st;
Begin
     DataWriteSpeed_CB.Clear;
     Form1.CommonCDSettings.Del_read_speed_list_items;
     Form1.SCSI.MMC1_any_link.Do_sense10_CD_cap_mech_st_out(Out_CD_cap_mech_st);
     //Form1.SCSI.MMC1.Do_sense10_CD_cap_mech_st_sub6_out(Out_CD_cap_mech_st);
     {Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          Form1.SCSI.MMC1.Do_sense6_CD_cap_mech_st_sub6_out(Out_CD_cap_mech_st);
     End;}
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          MaxXSpeed:=Out_CD_cap_mech_st.MaxWriteSpeed/C_1X_KBYTES_CDSPEED;

          If Trunc(MaxXSpeed)>0 Then
          Begin
               s:='1X, 176 kb/s';
               DataWriteSpeed_CB.Items.Add(s);
               Form1.CommonCDSettings.Add_read_speed(176);
          End;
          XSpeed:=2;
          While XSpeed<Trunc(MaxXSpeed) Do
          Begin
               s:=IntToStr(XSpeed)+'X, '+FloatToStrF(XSpeed*C_1X_KBYTES_CDSPEED, ffFixed, 7, 0)+' kb/s';
               DataWriteSpeed_CB.Items.Add(s);
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
          s:=s+'X, '+IntToStr(Out_CD_cap_mech_st.MaxWriteSpeed)+' kb/s';
          DataWriteSpeed_CB.Items.Add(s);
          DataWriteSpeed_CB.Items.Add('Max');
          Form1.CommonCDSettings.Add_read_speed(Out_CD_cap_mech_st.MaxWriteSpeed);

          Curr_data_write_speed:=MMC_SET_CD_SPEED_MAX;
          DataWriteSpeed_CB.ItemIndex:=DataWriteSpeed_CB.Items.Count-1;
     End
     Else
         Begin
              s:='Error reading CD capabilities and mechanism status parameters.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;
End;

Procedure TForm5.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;
     GetCDROMListThread:=nil;
     is_busy:=False;
End;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Form1.SetImageToCDFormNil; //Prevent main form from showing this form.
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

procedure TForm5.CD_dev_CBChange(Sender: TObject);
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
end;

procedure TForm5.RescanBtnClick(Sender: TObject);
begin
     If Not is_busy Then
     Begin
          is_busy:=True;
          Disable_controls;

          GetCDROMListThread:=T_GetCDROMListThread.Create(SCSI_init_Get_CDROM_list_done,
                                                          ShowErrMsg,
                                                          Not CHK_show_all_CDROMs.Checked);
     End;
end;

Procedure TForm5.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

procedure TForm5.FormCreate(Sender: TObject);
begin
     Curr_CDROM_indexNo:=0;
     is_busy:=False;
     GetCDROMListThread:=nil;

     f_toc:=nil;
     f_main:=nil;
     f_subch:=nil;
     f_pregap1:=nil;
     f_LO:=nil;

     RescanBtn.Click;
end;

procedure TForm5.ImageSaveAsBtnClick(Sender: TObject);
begin
     If OpenDialog1.Execute Then
     Begin
          ImageNameEdit.Text:=OpenDialog1.FileName;
     End;
end;

procedure TForm5.DataWriteSpeed_CBChange(Sender: TObject);
Var A_read_speed : T_P_read_speed_rec;
begin
     If DataWriteSpeed_CB.ItemIndex<DataWriteSpeed_CB.Items.Count-1 Then
     Begin
          A_read_speed:=Form1.CommonCDSettings.Read_speed_list.Items[DataWriteSpeed_CB.ItemIndex];
          Curr_data_write_speed:=A_read_speed^.KbSpeed;
     End
     Else
         Curr_data_write_speed:=MMC_SET_CD_SPEED_MAX;
end;

procedure TForm5.BTN_writeClick(Sender: TObject);
Var
   is_file_valid : Boolean;
   is_set_write_mode_ok : Boolean;
begin
     If ImageNameEdit.Text<>'' Then
     Begin
          If (ExtractFileExt(ImageNameEdit.Text)='.DAO') Then
          Begin
               toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.DAO');
               lead_in_file_type:=TOC_FILE_TYPE_2448_FULL_DAO_FILE;

               is_file_valid:=True;

               Try
                  f_toc:=TFileStream.Create(toc_file_name, fmOpenRead);
               Except
                     On EFOpenError Do
                     Begin
                          f_toc:=nil;
                          is_file_valid:=False;
                          MessageDlg('CD image binary file '+toc_file_name+' cannot be opened.', mtError, [mbOk], 0);
                     End;
               End;
          End
          Else
          Begin
               If (ExtractFileExt(ImageNameEdit.Text)='.LI') Then
               Begin
                    toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.LI');
                    lead_in_file_type:=TOC_FILE_TYPE_2448_FULL_LI_FILE;

                    pregap1_file_name:=ChangeFileExt(ImageNameEdit.Text, '.PG1');
                    LO_file_name:=ChangeFileExt(ImageNameEdit.Text, '.LO');
               End
               Else
               Begin
                    If (ExtractFileExt(ImageNameEdit.Text)='.TR') Then
                    Begin
                         toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.TR');
                         lead_in_file_type:=TOC_FILE_TYPE_96_REPEATED_DEINT;
                         sub_file_name:=ChangeFileExt(ImageNameEdit.Text, '.SUB');
                    End
                    Else
                    Begin
                         If (ExtractFileExt(ImageNameEdit.Text)='.TU') Then
                         Begin
                              toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.TU');
                              lead_in_file_type:=TOC_FILE_TYPE_96_UNIQUE_DEINT;
                              sub_file_name:=ChangeFileExt(ImageNameEdit.Text, '.SUB');
                         End
                         Else
                         Begin
                              toc_file_name:=ChangeFileExt(ImageNameEdit.Text, '.TUI');
                              lead_in_file_type:=TOC_FILE_TYPE_96_UNIQUE_INT;
                              sub_file_name:=ChangeFileExt(ImageNameEdit.Text, '.SBI');
                         End;
                    End;
               End;
               main_file_name:=ChangeFileExt(ImageNameEdit.Text, '.IMG');

               is_file_valid:=True;

               Try
                  f_toc:=TFileStream.Create(toc_file_name, fmOpenRead);
               Except
                     On EFOpenError Do
                     Begin
                          f_toc:=nil;
                          is_file_valid:=False;
                          MessageDlg('TOC/lead-in image file '+toc_file_name+' could not be opened.', mtError, [mbOk], 0);
                     End;
               End;

               If lead_in_file_type=TOC_FILE_TYPE_2448_FULL_LI_FILE Then
               Begin
                    Try
                       f_pregap1:=TFileStream.Create(pregap1_file_name, fmOpenRead);
                    Except
                          On EFOpenError Do
                          Begin
                               f_pregap1:=nil;
                               is_file_valid:=False;
                               MessageDlg('1st pre-gap image file '+pregap1_file_name+' could not be opened.', mtError, [mbOk], 0);
                          End;
                    End;

                    Try
                       f_LO:=TFileStream.Create(LO_file_name, fmOpenRead);
                    Except
                          On EFOpenError Do
                          Begin
                               f_LO:=nil;
                               is_file_valid:=False;
                               MessageDlg('Lead-out image file '+LO_file_name+' could not be opened.', mtError, [mbOk], 0);
                          End;
                    End;
               End;

               Try
                  f_main:=TFileStream.Create(main_file_name, fmOpenRead);
               Except
                     On EFOpenError Do
                     Begin
                          f_main:=nil;
                          is_file_valid:=False;
                          MessageDlg('Main image file '+main_file_name+' could not be opened.', mtError, [mbOk], 0);
                     End;
               End;

               Try
                  f_subch:=TFileStream.Create(sub_file_name, fmOpenRead);
               Except
                     On EFOpenError Do
                     Begin
                          f_subch:=nil;
                          is_file_valid:=False;
                          MessageDlg('Sub-channel image file '+sub_file_name+' could not be opened.', mtError, [mbOk], 0);
                     End;
               End;
          End;

          If (RG_ssp_method.ItemIndex=1) Or (RG_ssp_method.ItemIndex=2) Then
          Begin
               If is_file_valid Then
               Begin
                    AssignFile(SSP_file, ED_SSP_file_name.Text);
                    {$I-}
                    Reset(SSP_file);
                    {$I+}
                    If IOResult<>0 then
                    Begin
                         is_file_valid:=False;
                         MessageDlg('Cannot open skip sector position file.', mtError, [mbOk], 0);
                    End;
               End;
          End;

          If is_file_valid Then
          Begin
               If Not CHK_specify_start_write.Checked Then
               Begin
                    Read_ATIP;
               End;
               Start_MMCLBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MMCLBA(Start_LBA);

               If CB_write_type.ItemIndex=0 Then
               Begin
                    is_set_write_mode_ok:=Form1.CommonCDSettings.Set_write_params(CHK_test_write.Checked,
                                                                                  MMC_MODE_PARAM_BTYPE_RAW_RAWPW_2448);
               End
               Else
               Begin
                    is_set_write_mode_ok:=Form1.CommonCDSettings.Set_write_params(CHK_test_write.Checked,
                                                                                  MMC_MODE_PARAM_BTYPE_RAW_PACKPW_2448);
               End;

               If is_set_write_mode_ok Then
               Begin
                    If ImageToCDProgressForm=Nil Then
                    Begin
                         ImageToCDProgressForm:=TImageToCDProgressForm.Create(ImageToCDProgressForm_close, Form5);
                         ImageToCDProgressForm.Start_thread(Start_MMCLBA,
                                                            Curr_data_write_speed,
                                                            lead_in_file_type,
                                                            CB_lead_in_type.ItemIndex,
                                                            f_toc,
                                                            f_main,
                                                            f_subch,
                                                            f_pregap1,
                                                            f_LO,
                                                            @SSP_file,
                                                            RG_ssp_method.ItemIndex);
                    End;
                    If ImageToCDProgressForm<>Nil Then
                    Begin
                         ImageToCDProgressForm.Show;
                         Self.Enabled:=False;
                    End;
               End;
          End;
     End
     Else
     Begin
          MessageDlg('Please enter or browse a path and a filename for the image file.', mtError, [mbOk], 0);
     End;
end;

procedure TForm5.BTN_browse_SSPClick(Sender: TObject);
Var s : String;
begin
     s:=OpenDialog1.Filter;
     OpenDialog1.Filter:='Sector Skip Position file (.SSP)|*.SSP|All files (*.*)|*.*';
     If OpenDialog1.Execute Then
     Begin
          ED_SSP_file_name.Enabled:=True;
          ED_SSP_file_name.Text:=OpenDialog1.FileName;
          If RG_ssp_method.ItemIndex=0 Then
             RG_ssp_method.ItemIndex:=1;
     End;
     OpenDialog1.Filter:=s;
end;

procedure TForm5.ED_start_LBAKeyPress(Sender: TObject; var Key: Char);
begin
     Case Key Of
          Chr(13):
          Begin
               Start_LBA:=StrToIntDef(ED_start_LBA.Text, Start_LBA);
               ED_start_LBA.Text:=IntToStr(Start_LBA);
          End;
     End;
end;

procedure TForm5.ED_start_LBAExit(Sender: TObject);
begin
     If (IntToStr(Start_LBA)<>ED_start_LBA.Text) Then
     Begin
          Start_LBA:=StrToIntDef(ED_start_LBA.Text, Start_LBA);
          ED_start_LBA.Text:=IntToStr(Start_LBA);
     End;
end;

procedure TForm5.BTN_read_ATIPClick(Sender: TObject);
begin
     Read_ATIP;
     If Not Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          MessageDlg('Error while processing read TOC/ATIP command.' +
                      Chr(10) + Chr(13) +
                      Form1.SCSI.MMC1_any_link.Get_err_msg,
                      mtError, [mbOk], 0);
     End;
end;

Procedure TForm5.Read_ATIP;
Var
   There_is_ATIP_data : Boolean;
begin
     There_is_ATIP_data:=Form1.SCSI.MMC1_any_link.Do_read_T_P_A_ATIP_out_CDB10(Out_ATIP_desc);
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          If There_is_ATIP_data Then
          Begin
               //Convert ATIP (MSF) to MMCLBA units.
               Start_LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_MMCLBA(Out_ATIP_desc.StartMin,
                                                                          Out_ATIP_desc.StartSec,
                                                                          Out_ATIP_desc.StartFrame);

               ED_start_LBA.Text:=IntToStr(Start_LBA);
          End
          Else
              Start_LBA:=0;
     End
     Else
         Start_LBA:=0;
end;

procedure TForm5.CHK_show_all_CDROMsClick(Sender: TObject);
begin
     RescanBtnClick(Sender);
end;

procedure TForm5.CHK_specify_start_writeClick(Sender: TObject);
begin
     If CHK_specify_start_write.Checked Then
     Begin
          ED_start_LBA.Enabled:=True;
          BTN_read_ATIP.Enabled:=True;
     End
     Else
     Begin
          ED_start_LBA.Enabled:=False;
          BTN_read_ATIP.Enabled:=False;
     End;
end;

procedure TForm5.BTN_set_PG1_at_0Click(Sender: TObject);
Var
   is_file_valid : Boolean;
   Lead_in_len : Integer;
begin
     Disable_controls;

     If (ExtractFileExt(ImageNameEdit.Text)='.DAO') Then
     Begin
          is_file_valid:=True;
          toc_file_name:=ImageNameEdit.Text;
          Try
             f_toc:=TFileStream.Create(toc_file_name, fmOpenRead);
          Except
                On EFOpenError Do
                Begin
                     f_toc:=nil;
                     is_file_valid:=False;
                     MessageDlg('TOC/lead-in image file '+toc_file_name+' could not be opened.', mtError, [mbOk], 0);
                End;
          End;

          If is_file_valid Then
          Begin
               //Find the lead-in TOC length in the file.
               Lead_in_len:=Find_track_no_from_2448_file(f_toc, 1);
               Start_LBA:=-(Lead_in_len+150);
               ED_start_LBA.Text:=IntToStr(Start_LBA);
               CHK_specify_start_write.Checked:=True;

               f_toc.Free;
               f_toc:=nil;
          End;
     End
     Else
     If (ExtractFileExt(ImageNameEdit.Text)='.LI') Then
     Begin
          is_file_valid:=True;
          toc_file_name:=ImageNameEdit.Text;
          Try
             f_toc:=TFileStream.Create(toc_file_name, fmOpenRead);
          Except
                On EFOpenError Do
                Begin
                     f_toc:=nil;
                     is_file_valid:=False;
                     MessageDlg('TOC/lead-in image file '+toc_file_name+' could not be opened.', mtError, [mbOk], 0);
                End;
          End;

          If is_file_valid Then
          Begin
               Start_LBA:=-((f_toc.Size DIV 2448)+150);
               ED_start_LBA.Text:=IntToStr(Start_LBA);
               CHK_specify_start_write.Checked:=True;

               f_toc.Free;
               f_toc:=nil;
          End;
     End
     Else
     Begin
          MessageDlg('Image file must be .DAO or .LI format.',
                      mtError, [mbOk], 0);
     End;

     Enable_controls;
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

end.
