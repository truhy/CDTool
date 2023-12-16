unit SectorViewerUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CDFormUtilsUnit, MMC1Unit, SPC_Unit, ReadCDSectorThreadUnit,
  GetCDROMListThreadUnit, ComCtrls, CommonCDSettingsUnit, Tools_Unit,
  CDROMTableSectorScramblerUnit, ExtCtrls;

Type
    T_control_and_enabled=
    Record
          Ctrl : TControl;
          Enabled : Boolean;
    End;

type
  TForm2 = class(TForm)
    Shape1: TShape;
    GroupBox1: TGroupBox;
    RescanBtn: TButton;
    Devices_Lbl: TLabel;
    CD_dev_CB: TComboBox;
    Subch_Lbl: TLabel;
    Subch_CB: TComboBox;
    Label2: TLabel;
    CB_error_recv_param: TComboBox;
    Label1: TLabel;
    CHK_YB_scramble: TCheckBox;
    CHK_deint_subs: TCheckBox;
    TB_retry_count: TEdit;
    LBA_edit: TEdit;
    PrevSect_Btn: TButton;
    NextSect_Btn: TButton;
    ED_line_len: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    CB_disp_format: TComboBox;
    CB_word_wrap: TCheckBox;
    RereadBtn: TButton;
    NoErrorMsg_Chk: TCheckBox;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    SectorView: TMemo;
    Splitter1: TSplitter;
    SubChView: TMemo;
    procedure CD_dev_CBChange(Sender: TObject);
    procedure NextSect_BtnClick(Sender: TObject);
    procedure PrevSect_BtnClick(Sender: TObject);
    procedure LBA_editKeyPress(Sender: TObject; var Key: Char);
    procedure RereadBtnClick(Sender: TObject);
    procedure RescanBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LBA_editExit(Sender: TObject);
    procedure Subch_CBChange(Sender: TObject);
    procedure TB_retry_countExit(Sender: TObject);
    procedure TB_retry_countKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CHK_deint_subsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CHK_YB_scrambleClick(Sender: TObject);
    procedure CB_error_recv_paramChange(Sender: TObject);
    procedure CB_word_wrapClick(Sender: TObject);
    procedure CB_disp_formatChange(Sender: TObject);
  private
    { Private declarations }
    Curr_CDROM_indexNo      : Integer;
    Curr_LBA                : LongInt;
    NewSect                 : LongInt;
    SubChSelMode            : Byte;
    SubChSelModeRead        : Byte;
    Curr_read_err_rec_param : Byte;
    Curr_retry_count        : Byte;
    New_retry_count         : Byte;
    is_busy                 : Boolean;
    disabled_ctrl_list      : TList;
    ReadCDSectorThread      : T_ReadCDSectorThread;
    is_TEC_check_disable_code : Boolean;
    is_CHK_NoECC_TEC_disable_code : Boolean;
    GetCDROMListThread : T_GetCDROMListThread;
    sector_buf : Pointer;
    is_disp_data : Boolean;
    prev_column_width : Integer;
  public
    { Public declarations }
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure ShowErrMsg(s : String);
    Procedure FillCDComboSelect;
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
    Procedure Reread;
    Procedure Reread_done(Sender : TObject);
    Procedure Display_main_sector_data;
    Procedure Display_subch_data;
    Procedure ReadErrorRetryCount;
    Procedure SetCB_recv_param_to_prev;
    Procedure SetErrorRetryCount;
  end;

var
  Form2: TForm2;

implementation

uses MainFormUnit;

{$R *.DFM}

Procedure TForm2.Disable_controls;
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

Procedure TForm2.Enable_controls;
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

Procedure TForm2.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

Procedure TForm2.FillCDComboSelect;
Begin
     Form1.CDFormUtils.FillCB_with_CDROM(CD_dev_CB);
     If CD_dev_CB.Items.Count>0 Then
     Begin
          If Form1.CommonCDSettings.SCSI_select_init_CDROM(0)=True Then //Attempt to select and init first CDROM.
          Begin
               CD_dev_CB.ItemIndex:=0; //Selects first item in combobox list.
               Curr_CDROM_indexNo:=0;

               ReadErrorRetryCount;
          End;
     End
     Else
         MessageDlg('No CD/DVD devices.', mtError, [mbOk], 0);
End;

Procedure TForm2.Reread;
Var
   TEB_YN : Boolean;
Begin
     If Not is_busy Then
     Begin
          is_busy:=True;

          Disable_controls;

          If CB_error_recv_param.ItemIndex>0 Then
          Begin
               TEB_YN:=True;
          End
          Else
          Begin
               TEB_YN:=False;
          End;

          //If (NewSect>=-2147483648) AND (NewSect<=2147483647) Then
          //Begin
               Form1.CommonCDSettings.SectFormattedData:='';
               Form1.CommonCDSettings.SubChFormattedData:='';
               ReadCDSectorThread:=T_ReadCDSectorThread.Create(Reread_done,
                                                               ShowErrMsg,
                                                               NewSect,
                                                               SubChSelMode,
                                                               TEB_YN,
                                                               NoErrorMsg_Chk.Checked);
          //End
          //Else
          //Begin
          //     MessageDlg('Sector no. must be in the range –2147483648 to 2147483647', mtError, [mbOk], 0);
          //     LBA_edit.Text:=IntToStr(Curr_LBA);
          //End;
     End;
End;

Procedure TForm2.Reread_done(Sender : TObject);
Var
   s : String;
Begin
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Or
        (Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_RECV_ERR) Then
     Begin
          is_disp_data:=True;
          SubChSelModeRead:=SubChSelMode;
     End
     Else
     Begin
          //Below is to ignore any errors due to unrecoverable errors.
          If (CB_error_recv_param.ItemIndex>0) AND
             ((Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_MEDIUM_ERR) OR
              (Form1.SCSI.SPC_any_link.Get_sense_key=SEN_KEY_RECV_ERR)) Then
          Begin
               is_disp_data:=True;
               SubChSelModeRead:=SubChSelMode;
          End
          Else
          Begin
               s:='Sector could not be read.' + Chr(10) + Chr(13);
               s:=s + 'Error while processing ReadCD command.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               SectorView.Clear;
               SectorView.Lines.Text:=s;
               SubChView.Clear;
               is_disp_data:=False;
          End;
     End;

     CopyMemory(sector_buf, Form1.SCSI.SPC_any_link.Get_data_buf, 2448);

     If is_disp_data Then
     Begin
          //SectorView.Clear;
          Display_main_sector_data;
          If SubChSelModeRead=MMC_READCD_NO_SUBCH Then
          Begin
               SubChView.Lines.Text:='Subchannel reading mode is set to off.';
          End
          Else
          Begin
               //SubChView.Clear;
               Display_subch_data;
          End;
     End;
     LBA_edit.Text:=IntToStr(NewSect);
     Curr_LBA:=NewSect;

     Enable_controls;
     LBA_edit.SetFocus;
     ReadCDSectorThread:=nil;
     is_busy:=False;
End;

Procedure TForm2.Display_main_sector_data;
Var
   s : String;
   main_data_buf : ^Byte;
Begin
     If CHK_YB_scramble.Checked Then
     Begin
          GetMem(main_data_buf, 2352);
          CopyMemory(main_data_buf, sector_buf, 2352);
          Scramble(main_data_buf);
          Case CB_disp_format.ItemIndex Of
          0:
          Begin
               Buf2PasStrHexASCIILines(s, main_data_buf, 2352, '.', StrToIntDef(ED_line_len.Text, 0));
          End;
          1:
          Begin
               Buf2PasStrHexLines(s, main_data_buf, 2352, StrToIntDef(ED_line_len.Text, 0));
          End;
          2:
          Begin
               Buf2PasStrBinLines(s, main_data_buf, 2352, StrToIntDef(ED_line_len.Text, 0));
          End;
          3:
          Begin
               Buf2PasStrDecLines(s, main_data_buf, 2352, StrToIntDef(ED_line_len.Text, 0));
          End;
          End;
          FreeMem(main_data_buf);
     End
     Else
     Begin
          Case CB_disp_format.ItemIndex Of
          0:
          Begin
               Buf2PasStrHexASCIILines(s, sector_buf, 2352, '.', StrToIntDef(ED_line_len.Text, 0));
          End;
          1:
          Begin
               Buf2PasStrHexLines(s, sector_buf, 2352, StrToIntDef(ED_line_len.Text, 0));
          End;
          2:
          Begin
               Buf2PasStrBinLines(s, sector_buf, 2352, StrToIntDef(ED_line_len.Text, 0));
          End;
          3:
          Begin
               Buf2PasStrDecLines(s, sector_buf, 2352, StrToIntDef(ED_line_len.Text, 0));
          End;
          End;
     End;
     SectorView.Lines.Text:=s;
End;

Procedure TForm2.Display_subch_data;
Var
   s : String;
   sub_int_PW_96 : T_sub_int_PW_96;
   SubCh_buf : ^Byte;
Begin
     SubCh_buf:=sector_buf;
     Inc(SubCh_buf, 2352);

     Case SubChSelModeRead Of
     MMC_READCD_SUBCH_RAWPW:
     Begin
          If CHK_deint_subs.Checked Then
          Begin
               Form1.CommonCDSettings.Deint_subs(SubCh_buf, @sub_int_PW_96);
               Case CB_disp_format.ItemIndex Of
               0:
               Begin
                    Buf2PasStrHexASCIILines(s, @sub_int_PW_96, 96, '.', StrToIntDef(ED_line_len.Text, 0));
               End;
               1:
               Begin
                    Buf2PasStrHexLines(s, @sub_int_PW_96, 96, StrToIntDef(ED_line_len.Text, 0));
               End;
               2:
               Begin
                    Buf2PasStrBinLines(s, @sub_int_PW_96, 96, StrToIntDef(ED_line_len.Text, 0));
               End;
               3:
               Begin
                    Buf2PasStrDecLines(s, @sub_int_PW_96, 96, StrToIntDef(ED_line_len.Text, 0));
               End;
               End;
          End
          Else
          Begin
               Case CB_disp_format.ItemIndex Of
               0:
               Begin
                    Buf2PasStrHexASCIILines(s, SubCh_buf, 96, '.', StrToIntDef(ED_line_len.Text, 0));
               End;
               1:
               Begin
                    Buf2PasStrHexLines(s, SubCh_buf, 96, StrToIntDef(ED_line_len.Text, 0));
               End;
               2:
               Begin
                    Buf2PasStrBinLines(s, SubCh_buf, 96, StrToIntDef(ED_line_len.Text, 0));
               End;
               3:
               Begin
                    Buf2PasStrDecLines(s, SubCh_buf, 96, StrToIntDef(ED_line_len.Text, 0));
               End;
               End;
          End;
     End;
     MMC_READCD_SUBCH_PACKPW:
     Begin
          Case CB_disp_format.ItemIndex Of
          0:
          Begin
               Buf2PasStrHexASCIILines(s, SubCh_buf, 96, '.', StrToIntDef(ED_line_len.Text, 0));
          End;
          1:
          Begin
               Buf2PasStrHexLines(s, SubCh_buf, 96, StrToIntDef(ED_line_len.Text, 0));
          End;
          2:
          Begin
               Buf2PasStrBinLines(s, SubCh_buf, 96, StrToIntDef(ED_line_len.Text, 0));
          End;
          3:
          Begin
               Buf2PasStrDecLines(s, SubCh_buf, 96, StrToIntDef(ED_line_len.Text, 0));
          End;
          End;
     End;
     MMC_READCD_SUBCH_FORMPQ:
     Begin
          Case CB_disp_format.ItemIndex Of
          0:
          Begin
               Buf2PasStrHexASCIILines(s, SubCh_buf, 16, '.', StrToIntDef(ED_line_len.Text, 0));
          End;
          1:
          Begin
               Buf2PasStrHexLines(s, SubCh_buf, 16, StrToIntDef(ED_line_len.Text, 0));
          End;
          2:
          Begin
               Buf2PasStrBinLines(s, SubCh_buf, 16, StrToIntDef(ED_line_len.Text, 0));
          End;
          3:
          Begin
               Buf2PasStrDecLines(s, SubCh_buf, 16, StrToIntDef(ED_line_len.Text, 0));
          End;
          End;
     End;
     End;
     SubChView.Lines.Text:=s;
End;

procedure TForm2.CD_dev_CBChange(Sender: TObject);
begin
     If Form1.CommonCDSettings.SCSI_select_init_CDROM(CD_dev_CB.ItemIndex)=False Then //Attempt to select and init CDROM.
     Begin
          CD_dev_CB.ItemIndex:=Curr_CDROM_indexNo; //Selects prev selected item in combobox list.

          MessageDlg('Cannot initialise device.', mtError, [mbOk], 0);
     End
     Else
     Begin
          Curr_CDROM_indexNo:=CD_dev_CB.ItemIndex;
     End;
     ReadErrorRetryCount;
end;

Procedure TForm2.ReadErrorRetryCount;
Begin
     Form1.SCSI.MMC1_any_link.Do_sense10_read_err_rec;
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          Curr_retry_count:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Read_retry_count;
          Curr_read_err_rec_param:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Err_rec_param;

          Case Curr_read_err_rec_param Of
          MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC:
          Begin
               CB_error_recv_param.ItemIndex:=0;
          End;
          MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TES:
          Begin
               CB_error_recv_param.ItemIndex:=1;
          End;
          MMC_MODE_PARAM_READ_ERR_REC_CIRC_TES:
          Begin
               CB_error_recv_param.ItemIndex:=2;
          End
          Else
          Begin
               CB_error_recv_param.ItemIndex:=-1;
          End;
          End;

          TB_retry_count.Text:=IntToStr(Curr_retry_count);
          TB_retry_count.Enabled:=True;
     End
     Else
     Begin
          TB_retry_count.Enabled:=False;
     End;
End;

Procedure TForm2.SetCB_recv_param_to_prev;
Begin
     Case Curr_read_err_rec_param Of
     MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC:
     Begin
          CB_error_recv_param.ItemIndex:=0;
     End;
     MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TES:
     Begin
          CB_error_recv_param.ItemIndex:=1;
     End;
     MMC_MODE_PARAM_READ_ERR_REC_CIRC_TES:
     Begin
          CB_error_recv_param.ItemIndex:=2;
     End
     Else
         CB_error_recv_param.ItemIndex:=-1;
     End;
End;

Procedure TForm2.SetErrorRetryCount;
Var
   s : String;
   Err_rec_param : Byte;
Begin
     Form1.SCSI.MMC1_any_link.Do_sense10_read_err_rec;
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          Case CB_error_recv_param.ItemIndex Of
          1:
          Begin
               Err_rec_param:=MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TES;
          End;
          2:
          Begin
               Err_rec_param:=MMC_MODE_PARAM_READ_ERR_REC_CIRC_TES;
          End
          Else
              Err_rec_param:=T_mode_pg_read_err_rec(Form1.SCSI.MMC1_any_link.Calc_modepage10_ptr^).Err_rec_param;
          End;

          If Form1.CommonCDSettings.Set_read_err_rec_verify_count(Err_rec_param, New_retry_count) Then
          Begin
               Curr_retry_count:=New_retry_count;
               Curr_read_err_rec_param:=Err_rec_param;
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
     SetCB_recv_param_to_prev;
End;

procedure TForm2.NextSect_BtnClick(Sender: TObject);
begin
     If Curr_LBA<2147483647 Then
     Begin
          NewSect:=Curr_LBA+1;
          Reread;
     End
     Else
         MessageDlg('Largest value you can use is 2147483647', mtError, [mbOk], 0);
end;

procedure TForm2.PrevSect_BtnClick(Sender: TObject);
begin
     If Curr_LBA>-2147483647 Then
     Begin
          NewSect:=Curr_LBA-1;
          Reread;
     End
     Else
         MessageDlg('Smallest value you can use is –2147483648.', mtError, [mbOk], 0);
end;

Procedure TForm2.LBA_editKeyPress(Sender: TObject; var Key: Char);
begin
     Case Key Of
          Chr(13):
          Begin
               NewSect:=StrToIntDef(LBA_edit.Text, 0);
               Reread;
          End;
     End;
end;

procedure TForm2.RereadBtnClick(Sender: TObject);
begin
     NewSect:=StrToIntDef(LBA_edit.Text, 0);
     Reread;
end;

procedure TForm2.RescanBtnClick(Sender: TObject);
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

Procedure TForm2.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;
     GetCDROMListThread:=nil;
     is_busy:=False;

     ReadErrorRetryCount;
End;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Form1.SetSectorViewerFormNil; //Prevent main form from showing this form.
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
     is_disp_data:=False;
     LBA_edit.Text:='0';
     Curr_LBA:=0;
     Curr_CDROM_indexNo:=0;
     is_busy:=False;
     Subch_CBChange(Sender);
     //SubChSelMode:=MMC_READCD_SUBCH_RAWPW;
     GetCDROMListThread:=nil;
     ReadCDSectorThread:=nil;

     GetMem(sector_buf, 2448);

     RescanBtn.Click;
end;

procedure TForm2.LBA_editExit(Sender: TObject);
begin
     If IntToStr(Curr_LBA)<>LBA_edit.Text Then
     Begin
          NewSect:=StrToIntDef(LBA_edit.Text, 0);
          Reread;
     End;
end;

procedure TForm2.Subch_CBChange(Sender: TObject);
begin
     Case SubCh_CB.ItemIndex Of
          0: Begin
                  SubChSelMode:=MMC_READCD_NO_SUBCH;
                  CHK_deint_subs.Enabled := False;
                  CHK_deint_subs.Checked := False;
             End;
          1: Begin
                  SubChSelMode:=MMC_READCD_SUBCH_RAWPW;
                  CHK_deint_subs.Enabled := True;
             End;
          2: Begin
                  SubChSelMode:=MMC_READCD_SUBCH_PACKPW;
                  CHK_deint_subs.Enabled := False;
                  CHK_deint_subs.Checked := False;
             End;
          3: Begin
                  SubChSelMode:=MMC_READCD_SUBCH_FORMPQ;
                  CHK_deint_subs.Enabled := False;
                  CHK_deint_subs.Checked := False;
             End;
     End;
end;

procedure TForm2.TB_retry_countExit(Sender: TObject);
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

procedure TForm2.TB_retry_countKeyPress(Sender: TObject; var Key: Char);
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

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
     If GetCDROMListThread<>nil Then
     Begin
          try
             GetCDROMListThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     If ReadCDSectorThread<>nil Then
     Begin
          try
             ReadCDSectorThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;
end;

procedure TForm2.CHK_deint_subsClick(Sender: TObject);
begin
     If is_disp_data Then
     Begin
          If SubChSelModeRead<>MMC_READCD_NO_SUBCH Then
          Begin
               Display_subch_data
          End;
     End;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
     FreeMem(sector_buf);
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

procedure TForm2.CHK_YB_scrambleClick(Sender: TObject);
Begin
     If is_disp_data Then
     Begin
          Display_main_sector_data
     End;
End;

procedure TForm2.CB_error_recv_paramChange(Sender: TObject);
Var
   s : String;
begin
     Case CB_error_recv_param.ItemIndex Of
     0:
     Begin
          If Not Form1.CommonCDSettings.Set_read_err_rec_verify_param(MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC, Curr_retry_count) Then
          Begin
               s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
               s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               MessageDlg(s, mtError, [mbOk], 0);

               SetCB_recv_param_to_prev;
          End
          Else
          Begin
               Curr_read_err_rec_param:=MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC;
          End;
     End;
     1:
     Begin
          If Not Form1.CommonCDSettings.Set_read_err_rec_verify_param(MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TES, Curr_retry_count) Then
          Begin
               s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
               s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               MessageDlg(s, mtError, [mbOk], 0);

               SetCB_recv_param_to_prev;
          End
          Else
          Begin
               Curr_read_err_rec_param:=MMC_MODE_PARAM_READ_ERR_REC_CIRC_ECC_TES;
          End;
     End;
     2:
     Begin
          If Not Form1.CommonCDSettings.Set_read_err_rec_verify_param(MMC_MODE_PARAM_READ_ERR_REC_CIRC_TES, Curr_retry_count) Then
          Begin
               s:='Could not set this option on the drive.' + Chr(10) + Chr(13);
               s:=s + 'Hardware setting not supported by the drive.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               MessageDlg(s, mtError, [mbOk], 0);

               SetCB_recv_param_to_prev;
          End
          Else
          Begin
               Curr_read_err_rec_param:=MMC_MODE_PARAM_READ_ERR_REC_CIRC_TES;
          End;
     End;
     Else
         CB_error_recv_param.ItemIndex:=-1;
     End;
end;

procedure TForm2.CB_word_wrapClick(Sender: TObject);
begin
     If CB_word_wrap.Checked Then
     Begin
          ED_line_len.Enabled:=False;
          prev_column_width:=StrToIntDef(ED_line_len.Text, 0);
          ED_line_len.Text:='0';

          SectorView.ScrollBars:=ssVertical;
          SectorView.WordWrap:=True;

          SubChView.ScrollBars:=ssVertical;
          SubChView.WordWrap:=True;
     End
     Else
     Begin
          ED_line_len.Enabled:=True;
          ED_line_len.Text:=IntToStr(prev_column_width);

          SectorView.ScrollBars:=ssBoth;
          SectorView.WordWrap:=False;

          SubChView.ScrollBars:=ssBoth;
          SubChView.WordWrap:=False;
     End;

     If is_disp_data Then
     Begin
          //SectorView.Clear;
          Display_main_sector_data;
          If SubChSelModeRead=MMC_READCD_NO_SUBCH Then
          Begin
               SubChView.Lines.Text:='Subchannel reading mode is set to off.';
          End
          Else
          Begin
               //SubChView.Clear;
               Display_subch_data;
          End;
     End;
end;

procedure TForm2.CB_disp_formatChange(Sender: TObject);
begin
     If is_disp_data Then
     Begin
          //SectorView.Clear;
          Display_main_sector_data;
          If SubChSelModeRead=MMC_READCD_NO_SUBCH Then
          Begin
               SubChView.Lines.Text:='Subchannel reading mode is set to off.';
          End
          Else
          Begin
               //SubChView.Clear;
               Display_subch_data;
          End;
     End;
end;

end.
