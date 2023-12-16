unit TOCViewerUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CDFormUtilsUnit, MMC1Unit, Tools_Unit, GetCDROMListThreadUnit,
  ReadMCNThreadUnit, ReadISRCThreadUnit, Read_TOC_MSF_first_out_ThreadUnit, Grids,
  Tracks_Unit, WNASPI32Unit, CDROM_struct_Unit;

Type
    T_P_byte=^Byte;

type
  TForm3 = class(TForm)
    CD_dev_CB: TComboBox;
    RescanBtn: TButton;
    RereadTOCBtn: TButton;
    MCN_Lbl: TLabel;
    MCN_Edit: TEdit;
    MCN_Check: TCheckBox;
    TOC_StringGrid: TStringGrid;
    Device_Lbl: TLabel;
    Read_ISRC_Check: TCheckBox;
    PrevSess_Btn: TButton;
    ED_sess_no: TEdit;
    NextSess_Btn: TButton;
    CB_read_TOC_mode: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    CHK_disp_raw_Q: TCheckBox;
    CHK_disp_all_sess: TCheckBox;
    CHK_read_CD_mode: TCheckBox;
    procedure CD_dev_CBChange(Sender: TObject);
    procedure RescanBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RereadTOCBtnClick(Sender: TObject);
    procedure MCN_CheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Read_ISRC_CheckClick(Sender: TObject);
    procedure CB_read_TOC_modeChange(Sender: TObject);
    procedure ED_sess_noExit(Sender: TObject);
    procedure ED_sess_noKeyPress(Sender: TObject; var Key: Char);
    procedure PrevSess_BtnClick(Sender: TObject);
    procedure NextSess_BtnClick(Sender: TObject);
    procedure CHK_disp_all_sessClick(Sender: TObject);
    procedure CHK_disp_raw_QClick(Sender: TObject);
    procedure CHK_read_CD_modeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Curr_CDROM_indexNo : Integer;
    track_no : Byte;
    is_valid_TOC : Boolean;
    is_busy : Boolean;
    stop_CD_calls : Boolean;
    disabled_ctrl_list : TList;
    GetCDROMListThread : T_GetCDROMListThread;
    ReadMCNThread : T_ReadMCNThread;
    ReadISRCThread : T_ReadISRCThread;
    Read_TOC_MSF_first_out_Thread : T_Read_TOC_MSF_first_out_Thread;
    Read_TOC_format : Byte;
    new_sess_no : Byte;
    curr_sess_no : Byte;
    disp_all_sess : Boolean;
    tracks : T_tracks;
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure Disable_raw_TOC_controls;
    Procedure Enable_raw_TOC_controls;
    Procedure ShowErrMsg(s : String);
    Procedure FillCDComboSelect;
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
    Procedure Reread;
    Procedure Read_TOC_MSF_first_out;
    Procedure Read_TOC_MSF_next_StringGrid(Sender : TObject);
    Procedure Read_sess_TOC_MSF_next_StringGrid(Sender : TObject);
    Procedure Read_full_TOC_next_StringGrid(Sender : TObject);
    Procedure Read_full_TOC_next_StringGrid_raw_Q(Sender : TObject);
    Procedure ReadDisplayMCN;
    Procedure ReadDisplayMCN_done(Sender : TObject);
    Procedure ReadDisplayISRC;
    Procedure ReadDisplayISRC_done(Sender : TObject);
    Procedure Scan_format_of_data_tracks_from_CD;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses MainFormUnit;

{$R *.DFM}

Procedure TForm3.Disable_controls;
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

Procedure TForm3.Enable_controls;
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

Procedure TForm3.Disable_raw_TOC_controls;
Begin
     ED_sess_no.Enabled:=False;
     PrevSess_Btn.Enabled:=False;
     NextSess_Btn.Enabled:=False;
     CHK_disp_all_sess.Enabled:=False;
     CHK_disp_raw_Q.Enabled:=False;
End;

Procedure TForm3.Enable_raw_TOC_controls;
Begin
     ED_sess_no.Enabled:=True;
     PrevSess_Btn.Enabled:=True;
     NextSess_Btn.Enabled:=True;
     CHK_disp_all_sess.Enabled:=True;
     CHK_disp_raw_Q.Enabled:=True;
End;

Procedure TForm3.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

Procedure TForm3.FillCDComboSelect;
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

Procedure TForm3.Reread;
Begin
     Read_TOC_MSF_first_out;

     //Application.ProcessMessages;
End;

Procedure TForm3.Read_TOC_MSF_first_out;
Begin
     If Not is_busy Then
     Begin
          is_busy:=True;
          Disable_controls;

          Case Read_TOC_format Of
          MMC_READ_T_P_A_FORMAT_TOC:
               Read_TOC_MSF_first_out_Thread:=T_Read_TOC_MSF_first_out_Thread.Create(Read_TOC_MSF_next_StringGrid,
                                                                                     MMC_READ_T_P_A_FORMAT_TOC,
                                                                                     0);
          MMC_READ_T_P_A_FORMAT_SESS_INFO:
               Read_TOC_MSF_first_out_Thread:=T_Read_TOC_MSF_first_out_Thread.Create(Read_sess_TOC_MSF_next_StringGrid,
                                                                                     MMC_READ_T_P_A_FORMAT_SESS_INFO,
                                                                                     0);
          MMC_READ_T_P_A_FORMAT_FULL_TOC:
          Begin
               If CHK_disp_raw_Q.Checked Then
                  Read_TOC_MSF_first_out_Thread:=T_Read_TOC_MSF_first_out_Thread.Create(Read_full_TOC_next_StringGrid_raw_Q,
                                                                                        MMC_READ_T_P_A_FORMAT_FULL_TOC,
                                                                                        new_sess_no)
               Else
                   Read_TOC_MSF_first_out_Thread:=T_Read_TOC_MSF_first_out_Thread.Create(Read_full_TOC_next_StringGrid,
                                                                                         MMC_READ_T_P_A_FORMAT_FULL_TOC,
                                                                                         new_sess_no);
          End;
          End;
     End;
End;

Procedure TForm3.Read_TOC_MSF_next_StringGrid(Sender : TObject);
Var
   s : String;
   LBA : LongInt;
   Curr_row_count : Integer;
   Ctrl : Byte;
   track_no : Byte;
   track_entry : T_track_entry;
Begin
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          is_valid_TOC:=True;

          TOC_StringGrid.ColCount:=7;
          For Curr_row_count:=0 To TOC_StringGrid.ColCount-1 Do
              TOC_StringGrid.Cols[Curr_row_count].Clear;

          TOC_StringGrid.Cells[0, 0]:='Track';
          TOC_StringGrid.Cells[1, 0]:='Starting MSF';
          TOC_StringGrid.Cells[2, 0]:='Starting LBA';
          TOC_StringGrid.Cells[3, 0]:='Q ADR (mode-n)';
          TOC_StringGrid.Cells[4, 0]:='Q Control';
          TOC_StringGrid.Cells[5, 0]:='ISRC';
          TOC_StringGrid.Cells[6, 0]:='CD mode';

          TOC_StringGrid.FixedCols:=0;
          Curr_row_count:=1;

          tracks.Del_entries;
          Repeat
                Curr_row_count:=Curr_row_count+1;
                TOC_StringGrid.RowCount:=Curr_row_count;
                If Form1.CommonCDSettings.TOC_data.TrackNo<>$AA Then
                Begin
                     TOC_StringGrid.Cells[0, Curr_row_count-1]:=IntToStrFixZeroes(Form1.CommonCDSettings.TOC_data.TrackNo, 2);
                End
                Else
                    TOC_StringGrid.Cells[0, Curr_row_count-1]:='Lead out';

                TOC_StringGrid.Cells[1, Curr_row_count-1]:=IntToStrFixZeroes(Form1.CommonCDSettings.TOC_data.StartM, 2) + ':' +
                                                           IntToStrFixZeroes(Form1.CommonCDSettings.TOC_data.StartS, 2) + ':' +
                                                           IntToStrFixZeroes(Form1.CommonCDSettings.TOC_data.StartF, 2);
                LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(Form1.CommonCDSettings.TOC_data.StartM,
                                                                  Form1.CommonCDSettings.TOC_data.StartS,
                                                                  Form1.CommonCDSettings.TOC_data.StartF);
                If LBA>=0 Then
                Begin
                     TOC_StringGrid.Cells[2, Curr_row_count-1]:=IntToStrFixZeroes(LBA, 6);
                End
                Else
                    TOC_StringGrid.Cells[2, Curr_row_count-1]:=IntToStr(LBA);

                TOC_StringGrid.Cells[3, Curr_row_count-1]:=IntToStr((Form1.CommonCDSettings.TOC_data.ADR_Ctrl AND $F0) SHR 4);

                Ctrl:=Form1.CommonCDSettings.TOC_data.ADR_Ctrl AND MMC_READ_SUBCH_ADRCTRL_MASK_CTRL;
                TOC_StringGrid.Cells[4, Curr_row_count-1]:=IntToStr(Ctrl);
                If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DATA)=MMC_READ_SUBCH_CTRL_DATA Then
                Begin
                     If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DAO)=MMC_READ_SUBCH_CTRL_DAO Then
                     Begin
                          TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] + ' (Data, DAO)';
                     End
                     Else
                     Begin
                          TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] + ' (Data, INC)';
                     End;

                     track_entry.CD_mode:=MMC_SECTORTYPE_ANY;
                End
                Else
                Begin
                     TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] + ' (Audio, DAO)';

                     track_entry.CD_mode:=MMC_SECTORTYPE_CDDA;
                End;

                track_entry.Number:=Form1.CommonCDSettings.TOC_data.TrackNo;
                track_entry.First_MSF.M:=Form1.CommonCDSettings.TOC_data.StartM;
                track_entry.First_MSF.S:=Form1.CommonCDSettings.TOC_data.StartS;
                track_entry.First_MSF.F:=Form1.CommonCDSettings.TOC_data.StartF;

                If CHK_read_CD_mode.Checked Then
                Begin
                     If track_entry.CD_mode=MMC_SECTORTYPE_ANY Then
                     Begin
                          track_entry.CD_mode:=Scan_format_of_data_track_from_CD(track_entry.First_MSF);

                          Case track_entry.CD_mode Of
                          CDROM_SECTORTYPE_ANY:
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='Reserved';
                          CDROM_SECTORTYPE_MODE1:
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 1';
                          CDROM_SECTORTYPE_MODE2FORMLESS:
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 2 formless';
                          CDROM_SECTORTYPE_MODE2FORM1:
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 2 form 1';
                          CDROM_SECTORTYPE_MODE2FORM2:
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 2 form 2';
                          CDROM_SECTORTYPE_MODE0:
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 0';
                          CDROM_SECTORTYPE_UNKNOWN:
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='Unknown';
                          End;
                     End
                     Else
                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='CDDA';
                End
                Else
                Begin
                     TOC_StringGrid.Cells[6, Curr_row_count-1]:='';
                End;

                tracks.Add_entry(track_entry);
          Until Form1.SCSI.MMC1_any_link.Do_read_T_P_A_TOC_MSF_next_out_CDB10(Form1.CommonCDSettings.TOC_data)=False;

          ED_sess_no.Text:='All';
          CHK_disp_all_sess.Checked:=True;
     End
     Else
         Begin
              is_valid_TOC:=False;

              TOC_StringGrid.RowCount:=2;
              TOC_StringGrid.Rows[1].Clear;

              s:='Error while processing read TOC.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;

     Enable_controls;
     Read_TOC_MSF_first_out_Thread:=nil;
     is_busy:=False;

     If Not stop_CD_calls Then
     Begin
          If MCN_Check.Checked Then
          Begin
               ReadDisplayMCN;
          End
          Else
          Begin
               MCN_Edit.Text:='Not read yet.';

               If Read_ISRC_Check.Checked Then
                  ReadDisplayISRC
               Else
               Begin
                    If is_valid_TOC Then
                    Begin
                         For track_no:=Form1.CommonCDSettings.First_trk_sess_no To Form1.CommonCDSettings.Last_trk_sess_no Do
                         Begin
                              TOC_StringGrid.Cells[5, track_no]:='Not read yet.';
                         End;
                         TOC_StringGrid.Cells[5, Form1.CommonCDSettings.Last_trk_sess_no+1]:='';
                    End;
                End;
          End;
     End;
End;

Procedure TForm3.Read_sess_TOC_MSF_next_StringGrid(Sender : TObject);
Var
   s : String;
   LBA : LongInt;
   Curr_row_count : Integer;
   Ctrl : Byte;
   track_entry : T_track_entry;
Begin
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          is_valid_TOC:=True;

          TOC_StringGrid.ColCount:=6;
          For Curr_row_count:=0 To TOC_StringGrid.ColCount-1 Do
              TOC_StringGrid.Cols[Curr_row_count].Clear;

          TOC_StringGrid.Cells[0, 0]:='1st trk in last sess';
          TOC_StringGrid.Cells[1, 0]:='Starting MSF';
          TOC_StringGrid.Cells[2, 0]:='Starting LBA';
          TOC_StringGrid.Cells[3, 0]:='Q ADR (mode-n)';
          TOC_StringGrid.Cells[4, 0]:='Q Control';
          TOC_StringGrid.Cells[5, 0]:='CD mode';

          TOC_StringGrid.FixedCols:=0;

          TOC_StringGrid.RowCount:=2;

          tracks.Del_entries;

          TOC_StringGrid.Cells[0, 1]:=IntToStrFixZeroes(Form1.CommonCDSettings.sess_TOC_data.FirstTrkNo_InLastSess, 2);

          TOC_StringGrid.Cells[1, 1]:=IntToStrFixZeroes(Form1.CommonCDSettings.sess_TOC_data.StartM, 2) + ':' +
                                      IntToStrFixZeroes(Form1.CommonCDSettings.sess_TOC_data.StartS, 2) + ':' +
                                      IntToStrFixZeroes(Form1.CommonCDSettings.sess_TOC_data.StartF, 2);
          LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(Form1.CommonCDSettings.sess_TOC_data.StartM,
                                                            Form1.CommonCDSettings.sess_TOC_data.StartS,
                                                            Form1.CommonCDSettings.sess_TOC_data.StartF);
          If LBA>=0 Then
          Begin
               TOC_StringGrid.Cells[2, 1]:=IntToStrFixZeroes(LBA, 6);
          End
          Else
              TOC_StringGrid.Cells[2, 1]:=IntToStr(LBA);

          TOC_StringGrid.Cells[3, 1]:=IntToStr((Form1.CommonCDSettings.sess_TOC_data.ADR_Ctrl AND $F0) SHR 4);

          Ctrl:=Form1.CommonCDSettings.TOC_data.ADR_Ctrl AND MMC_READ_SUBCH_ADRCTRL_MASK_CTRL;
          TOC_StringGrid.Cells[4, 1]:=IntToStr(Ctrl);
          If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DATA)=MMC_READ_SUBCH_CTRL_DATA Then
          Begin
               If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DAO)=MMC_READ_SUBCH_CTRL_DAO Then
               Begin
                    TOC_StringGrid.Cells[4, 1]:=TOC_StringGrid.Cells[4, 1] + ' (Data, DAO)';
               End
               Else
               Begin
                    TOC_StringGrid.Cells[4, 1]:=TOC_StringGrid.Cells[4, 1] + ' (Data, INC)';
               End;

               track_entry.CD_mode:=MMC_SECTORTYPE_ANY;
          End
          Else
          Begin
               TOC_StringGrid.Cells[4, 1]:=TOC_StringGrid.Cells[4, 1] + ' (Audio, DAO)';

               track_entry.CD_mode:=MMC_SECTORTYPE_CDDA;
          End;

          track_entry.Number:=Form1.CommonCDSettings.sess_TOC_data.FirstTrkNo_InLastSess;
          track_entry.First_MSF.M:=Form1.CommonCDSettings.sess_TOC_data.StartM;
          track_entry.First_MSF.S:=Form1.CommonCDSettings.sess_TOC_data.StartS;
          track_entry.First_MSF.F:=Form1.CommonCDSettings.sess_TOC_data.StartF;

          If CHK_read_CD_mode.Checked Then
          Begin
               If track_entry.CD_mode=MMC_SECTORTYPE_ANY Then
               Begin
                    track_entry.CD_mode:=Scan_format_of_data_track_from_CD(track_entry.First_MSF);

                    Case track_entry.CD_mode Of
                    CDROM_SECTORTYPE_ANY:
                         TOC_StringGrid.Cells[5, 1]:='Reserved';
                    CDROM_SECTORTYPE_MODE1:
                         TOC_StringGrid.Cells[5, 1]:='Mode 1';
                    CDROM_SECTORTYPE_MODE2FORMLESS:
                         TOC_StringGrid.Cells[5, 1]:='Mode 2 formless';
                    CDROM_SECTORTYPE_MODE2FORM1:
                         TOC_StringGrid.Cells[5, 1]:='Mode 2 form 1';
                    CDROM_SECTORTYPE_MODE2FORM2:
                         TOC_StringGrid.Cells[5, 1]:='Mode 2 form 2';
                    CDROM_SECTORTYPE_MODE0:
                         TOC_StringGrid.Cells[5, 1]:='Mode 0';
                    CDROM_SECTORTYPE_UNKNOWN:
                         TOC_StringGrid.Cells[5, 1]:='Unknown';
                    End;
               End
               Else
                   TOC_StringGrid.Cells[5, 1]:='CDDA';
          End
          Else
          Begin
               TOC_StringGrid.Cells[5, 1]:='';
          End;

          tracks.Add_entry(track_entry);

          ED_sess_no.Text:=IntToStr(Form1.CommonCDSettings.Last_trk_sess_no);
     End
     Else
         Begin
              is_valid_TOC:=False;

              TOC_StringGrid.RowCount:=2;
              TOC_StringGrid.Rows[1].Clear;

              s:='Error while processing read TOC, session format mode.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;

     Enable_controls;
     Read_TOC_MSF_first_out_Thread:=nil;
     is_busy:=False;

     If Not stop_CD_calls Then
     Begin
          If MCN_Check.Checked Then
          Begin
               ReadDisplayMCN;
          End
          Else
          Begin
               MCN_Edit.Text:='Not read yet.';
          End;
     End;
End;

Procedure TForm3.Read_full_TOC_next_StringGrid(Sender : TObject);
Var
   s : String;
   LBA : LongInt;
   Curr_row_count : Integer;
   ADR, Ctrl : Byte;
   track_entry : T_track_entry;
Begin
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          is_valid_TOC:=True;

          TOC_StringGrid.ColCount:=7;
          For Curr_row_count:=0 To TOC_StringGrid.ColCount-1 Do
              TOC_StringGrid.Cols[Curr_row_count].Clear;

          TOC_StringGrid.Cells[0, 0]:='Session';
          TOC_StringGrid.Cells[1, 0]:='Track/Q Point';
          TOC_StringGrid.Cells[2, 0]:='Starting MSF';
          TOC_StringGrid.Cells[3, 0]:='Starting LBA';
          TOC_StringGrid.Cells[4, 0]:='Q ADR (mode-n)';
          TOC_StringGrid.Cells[5, 0]:='Q Control';
          TOC_StringGrid.Cells[6, 0]:='CD mode';

          TOC_StringGrid.FixedCols:=0;
          Curr_row_count:=1;

          Form1.CommonCDSettings.Convert_full_TOC_Hex_to_Dec;

          curr_sess_no:=new_sess_no;

          tracks.Del_entries;
          Repeat
                ADR:=(Form1.CommonCDSettings.Full_TOC_data.ADR_Ctrl AND $F0) SHR 4;

                If ((ADR=1) AND (
                   (Form1.CommonCDSettings.Full_TOC_data.POINT>=1) AND
                   (Form1.CommonCDSettings.Full_TOC_data.POINT<=99) OR
                   (Form1.CommonCDSettings.Full_TOC_data.POINT=$A2)))

                   OR

                   ((ADR=5) AND (
                   (Form1.CommonCDSettings.Full_TOC_data.POINT=$B0) OR
                   (Form1.CommonCDSettings.Full_TOC_data.POINT=$C0)))
                   Then
                Begin
                     Curr_row_count:=Curr_row_count+1;
                     TOC_StringGrid.RowCount:=Curr_row_count;

                     TOC_StringGrid.Cells[0, Curr_row_count-1]:=IntToStr(Form1.CommonCDSettings.Full_TOC_data.SessionNo);

                     TOC_StringGrid.Cells[4, Curr_row_count-1]:=IntToStr(ADR);

                     Ctrl:=Form1.CommonCDSettings.Full_TOC_data.ADR_Ctrl AND MMC_READ_SUBCH_ADRCTRL_MASK_CTRL;
                     TOC_StringGrid.Cells[5, Curr_row_count-1]:=IntToStr(Ctrl);
                     If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DATA)=MMC_READ_SUBCH_CTRL_DATA Then
                     Begin
                          If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DAO)=MMC_READ_SUBCH_CTRL_DAO Then
                          Begin
                               TOC_StringGrid.Cells[5, Curr_row_count-1]:=TOC_StringGrid.Cells[5, Curr_row_count-1] + ' (Data, DAO)';
                          End
                          Else
                          Begin
                               TOC_StringGrid.Cells[5, Curr_row_count-1]:=TOC_StringGrid.Cells[5, Curr_row_count-1] + ' (Data, INC)';
                          End;

                          track_entry.CD_mode:=MMC_SECTORTYPE_ANY;
                     End
                     Else
                     Begin
                          TOC_StringGrid.Cells[5, Curr_row_count-1]:=TOC_StringGrid.Cells[5, Curr_row_count-1] + ' (Audio, DAO)';

                          track_entry.CD_mode:=MMC_SECTORTYPE_CDDA;
                     End;

                     If ADR=1 Then
                     Begin
                          If (Form1.CommonCDSettings.Full_TOC_data.POINT>=1) AND
                             (Form1.CommonCDSettings.Full_TOC_data.POINT<=99)
                             Then
                          Begin
                               TOC_StringGrid.Cells[1, Curr_row_count-1]:=IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.POINT, 2);
                          End
                          Else
                          Begin
                               TOC_StringGrid.Cells[1, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.POINT, 2) + 'h';
                          End;

                          track_entry.Number:=Form1.CommonCDSettings.Full_TOC_data.POINT;
                          track_entry.First_MSF.M:=Form1.CommonCDSettings.Full_TOC_data.PMin;
                          track_entry.First_MSF.S:=Form1.CommonCDSettings.Full_TOC_data.PSec;
                          track_entry.First_MSF.F:=Form1.CommonCDSettings.Full_TOC_data.PFrame;

                          If CHK_read_CD_mode.Checked Then
                          Begin
                               If track_entry.CD_mode=MMC_SECTORTYPE_ANY Then
                               Begin
                                    track_entry.CD_mode:=Scan_format_of_data_track_from_CD(track_entry.First_MSF);

                                    Case track_entry.CD_mode Of
                                    CDROM_SECTORTYPE_ANY:
                                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='Reserved';
                                    CDROM_SECTORTYPE_MODE1:
                                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 1';
                                    CDROM_SECTORTYPE_MODE2FORMLESS:
                                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 2 formless';
                                    CDROM_SECTORTYPE_MODE2FORM1:
                                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 2 form 1';
                                    CDROM_SECTORTYPE_MODE2FORM2:
                                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 2 form 2';
                                    CDROM_SECTORTYPE_MODE0:
                                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='Mode 0';
                                    CDROM_SECTORTYPE_UNKNOWN:
                                         TOC_StringGrid.Cells[6, Curr_row_count-1]:='Unknown';
                                    End;
                               End
                               Else
                                   TOC_StringGrid.Cells[6, Curr_row_count-1]:='CDDA';
                          End
                          Else
                          Begin
                               TOC_StringGrid.Cells[6, Curr_row_count-1]:='';
                          End;

                          tracks.Add_entry(track_entry);

                          Case Form1.CommonCDSettings.Full_TOC_data.POINT Of
                          $A2: TOC_StringGrid.Cells[1, Curr_row_count-1]:=TOC_StringGrid.Cells[1, Curr_row_count-1] +
                                                                          ' - lead-out';
                          1..99: TOC_StringGrid.Cells[1, Curr_row_count-1]:=TOC_StringGrid.Cells[1, Curr_row_count-1] +
                                                                          ' - track no.';
                          End;

                          TOC_StringGrid.Cells[2, Curr_row_count-1]:=IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.PMin, 2) + ':' +
                                                                     IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.PSec, 2) + ':' +
                                                                     IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.PFrame, 2);
                          LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(Form1.CommonCDSettings.Full_TOC_data.PMin,
                                                                            Form1.CommonCDSettings.Full_TOC_data.PSec,
                                                                            Form1.CommonCDSettings.Full_TOC_data.PFrame);
                          If LBA>=0 Then
                          Begin
                             TOC_StringGrid.Cells[3, Curr_row_count-1]:=IntToStrFixZeroes(LBA, 6);
                          End
                          Else
                              TOC_StringGrid.Cells[3, Curr_row_count-1]:=IntToStr(LBA);
                     End;

                     If ADR=5 Then
                     Begin
                          TOC_StringGrid.Cells[1, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.POINT, 2) + 'h';

                          Case Form1.CommonCDSettings.Full_TOC_data.POINT Of
                          $B0:
                          Begin
                               TOC_StringGrid.Cells[1, Curr_row_count-1]:=TOC_StringGrid.Cells[1, Curr_row_count-1] +
                                                                          ' - next session';

                               TOC_StringGrid.Cells[2, Curr_row_count-1]:=IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.Min, 2) + ':' +
                                                                     IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.Sec, 2) + ':' +
                                                                     IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.Frame, 2);
                               LBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(Form1.CommonCDSettings.Full_TOC_data.Min,
                                                                                 Form1.CommonCDSettings.Full_TOC_data.Sec,
                                                                                 Form1.CommonCDSettings.Full_TOC_data.Frame);

                               If LBA>=0 Then
                               Begin
                                    TOC_StringGrid.Cells[3, Curr_row_count-1]:=IntToStrFixZeroes(LBA, 6);
                               End
                               Else
                                   TOC_StringGrid.Cells[3, Curr_row_count-1]:=IntToStr(LBA);
                          End;
                          $C0: TOC_StringGrid.Cells[1, Curr_row_count-1]:=TOC_StringGrid.Cells[1, Curr_row_count-1] +
                                                                          ' - lead-in';
                          End;
                     End;
                End;
          Until (Form1.SCSI.MMC1_any_link.Do_read_T_P_A_full_TOC_next_out_CDB10(Form1.CommonCDSettings.Full_TOC_data)=False) Or
                (Not(disp_all_sess) And (Form1.CommonCDSettings.Full_TOC_data.SessionNo<>curr_sess_no));

          If disp_all_sess Then
          Begin
               ED_sess_no.Text:='All';
               CHK_disp_all_sess.Checked:=True;
          End
          Else
              ED_sess_no.Text:=IntToStr(curr_sess_no);
     End
     Else
         Begin
              is_valid_TOC:=False;

              new_sess_no:=curr_sess_no;

              //TOC_StringGrid.RowCount:=2;
              //TOC_StringGrid.Rows[1].Clear;

              s:='Error while processing read TOC.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;

     Enable_controls;
     Read_TOC_MSF_first_out_Thread:=nil;
     is_busy:=False;

     If Not stop_CD_calls Then
     Begin
          If MCN_Check.Checked Then
          Begin
               ReadDisplayMCN;
          End
          Else
          Begin
               MCN_Edit.Text:='Not read yet.';

               {If Read_ISRC_Check.Checked Then
                  ReadDisplayISRC
               Else
               Begin
                    If is_valid_TOC Then
                    Begin
                         For track_no:=Form1.CommonCDSettings.First_trk_sess_no To Form1.CommonCDSettings.Last_trk_sess_no Do
                         Begin
                              TOC_StringGrid.Cells[5, track_no]:='Not read yet.';
                         End;
                         TOC_StringGrid.Cells[5, Form1.CommonCDSettings.Last_trk_sess_no+1]:='';
                    End;
                End;}
          End;
     End;
End;

Procedure TForm3.Read_full_TOC_next_StringGrid_raw_Q(Sender : TObject);
Var
   s : String;
   Curr_row_count : Integer;
   ADR, Ctrl : Byte;
   track_entry : T_track_entry;
Begin
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          is_valid_TOC:=True;

          TOC_StringGrid.ColCount:=12;
          For Curr_row_count:=0 To TOC_StringGrid.ColCount-1 Do
              TOC_StringGrid.Cols[Curr_row_count].Clear;

          TOC_StringGrid.Cells[0, 0]:='Session';
          TOC_StringGrid.Cells[1, 0]:='Q Control';
          TOC_StringGrid.Cells[2, 0]:='Q ADR (mode-n)';
          TOC_StringGrid.Cells[3, 0]:='TNO';
          TOC_StringGrid.Cells[4, 0]:='Point';
          TOC_StringGrid.Cells[5, 0]:='Min';
          TOC_StringGrid.Cells[6, 0]:='Sec';
          TOC_StringGrid.Cells[7, 0]:='Frame';
          TOC_StringGrid.Cells[8, 0]:='Zero';
          TOC_StringGrid.Cells[9, 0]:='P Min';
          TOC_StringGrid.Cells[10, 0]:='P Sec';
          TOC_StringGrid.Cells[11, 0]:='P Frame';

          TOC_StringGrid.FixedCols:=0;
          Curr_row_count:=1;

          Form1.CommonCDSettings.Convert_full_TOC_Hex_to_Dec;

          curr_sess_no:=new_sess_no;

          tracks.Del_entries;
          Repeat
                ADR:=(Form1.CommonCDSettings.Full_TOC_data.ADR_Ctrl AND $F0) SHR 4;

                Curr_row_count:=Curr_row_count+1;
                TOC_StringGrid.RowCount:=Curr_row_count;

                TOC_StringGrid.Cells[0, Curr_row_count-1]:=IntToStr(Form1.CommonCDSettings.Full_TOC_data.SessionNo);

                Ctrl:=Form1.CommonCDSettings.Full_TOC_data.ADR_Ctrl AND MMC_READ_SUBCH_ADRCTRL_MASK_CTRL;
                TOC_StringGrid.Cells[1, Curr_row_count-1]:=IntToHex(Ctrl, 1)+
                                                           'h ('+
                                                           IntToStr(Ctrl)+
                                                           ')';
                If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DATA)=MMC_READ_SUBCH_CTRL_DATA Then
                Begin
                     If (Ctrl AND MMC_READ_SUBCH_CTRL_MASK_DAO)=MMC_READ_SUBCH_CTRL_DAO Then
                     Begin
                          TOC_StringGrid.Cells[1, Curr_row_count-1]:=TOC_StringGrid.Cells[1, Curr_row_count-1] + ' (Data, DAO)';
                     End
                     Else
                     Begin
                          TOC_StringGrid.Cells[1, Curr_row_count-1]:=TOC_StringGrid.Cells[1, Curr_row_count-1] + ' (Data, INC)';
                     End
                End
                Else
                Begin
                     TOC_StringGrid.Cells[1, Curr_row_count-1]:=TOC_StringGrid.Cells[1, Curr_row_count-1] + ' (Audio, DAO)';
                End;

                TOC_StringGrid.Cells[2, Curr_row_count-1]:=IntToHex(ADR, 1)+
                                                           'h ('+
                                                           IntToStr(ADR)+
                                                           ')';

                TOC_StringGrid.Cells[3, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.TNO, 2)+
                                                           'h ('+
                                                           IntToStr(Form1.CommonCDSettings.Full_TOC_data.TNO)+
                                                           ')';

                {If (Form1.CommonCDSettings.Full_TOC_data.POINT>=1) AND
                   (Form1.CommonCDSettings.Full_TOC_data.POINT<=99) Then
                Begin}
                     TOC_StringGrid.Cells[4, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.POINT, 2)+
                                                                'h ('+
                                                                IntToStrFixZeroes(Form1.CommonCDSettings.Full_TOC_data.POINT, 2)+
                                                                ')';
                {End
                Else
                Begin
                     TOC_StringGrid.Cells[4, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.POINT, 2) + 'h';
                End; }

                Case ADR Of
                1:
                     Case Form1.CommonCDSettings.Full_TOC_data.POINT Of
                     $A0: TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - first track';
                     $A1: TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - last track';
                     $A2:
                     Begin
                          TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - lead-out';

                          track_entry.Number:=Form1.CommonCDSettings.Full_TOC_data.POINT;
                          track_entry.First_MSF.M:=Form1.CommonCDSettings.Full_TOC_data.PMin;
                          track_entry.First_MSF.S:=Form1.CommonCDSettings.Full_TOC_data.PSec;
                          track_entry.First_MSF.F:=Form1.CommonCDSettings.Full_TOC_data.PFrame;
                          tracks.Add_entry(track_entry);
                     End;
                     1..99:
                     Begin
                          TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - track no.';

                          track_entry.Number:=Form1.CommonCDSettings.Full_TOC_data.POINT;
                          track_entry.First_MSF.M:=Form1.CommonCDSettings.Full_TOC_data.PMin;
                          track_entry.First_MSF.S:=Form1.CommonCDSettings.Full_TOC_data.PSec;
                          track_entry.First_MSF.F:=Form1.CommonCDSettings.Full_TOC_data.PFrame;
                          tracks.Add_entry(track_entry);
                     End;
                     End;
                5:
                     Case Form1.CommonCDSettings.Full_TOC_data.POINT Of
                     $B0: TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - next session';
                     $B1: TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - CDDA n skips';
                     $B2, $B3, $B4:
                          TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - CDDA skip trks';
                     $C0: TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - sess lead-in';
                     1..40:
                          TOC_StringGrid.Cells[4, Curr_row_count-1]:=TOC_StringGrid.Cells[4, Curr_row_count-1] +
                                                                     ' - CDDA skip gaps';
                     End;
                End;

                TOC_StringGrid.Cells[5, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.Min, 2)+
                                                           'h ('+
                                                           IntToStr(Form1.CommonCDSettings.Full_TOC_data.Min)+
                                                           ')';
                TOC_StringGrid.Cells[6, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.Sec, 2)+
                                                           'h ('+
                                                           IntToStr(Form1.CommonCDSettings.Full_TOC_data.Sec)+
                                                           ')';
                TOC_StringGrid.Cells[7, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.Frame, 2)+
                                                           'h ('+
                                                           IntToStr(Form1.CommonCDSettings.Full_TOC_data.Frame)+
                                                           ')';

                TOC_StringGrid.Cells[8, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.Zero, 2)+
                                                           'h ('+
                                                           IntToStr(Form1.CommonCDSettings.Full_TOC_data.Zero)+
                                                           ')';

                TOC_StringGrid.Cells[9, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.PMin, 2)+
                                                           'h ('+
                                                           IntToStr(Form1.CommonCDSettings.Full_TOC_data.PMin)+
                                                           ')';
                TOC_StringGrid.Cells[10, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.PSec, 2)+
                                                            'h ('+
                                                            IntToStr(Form1.CommonCDSettings.Full_TOC_data.PSec)+
                                                            ')';
                TOC_StringGrid.Cells[11, Curr_row_count-1]:=IntToHex(Form1.CommonCDSettings.Full_TOC_data.PFrame, 2)+
                                                            'h ('+
                                                            IntToStr(Form1.CommonCDSettings.Full_TOC_data.PFrame)+
                                                            ')';

          Until (Form1.SCSI.MMC1_any_link.Do_read_T_P_A_full_TOC_next_out_CDB10(Form1.CommonCDSettings.Full_TOC_data)=False) Or
                (Not(disp_all_sess) And (Form1.CommonCDSettings.Full_TOC_data.SessionNo<>curr_sess_no));

          If disp_all_sess Then
          Begin
               ED_sess_no.Text:='All';
               CHK_disp_all_sess.Checked:=True;
          End
          Else
              ED_sess_no.Text:=IntToStr(curr_sess_no);
     End
     Else
         Begin
              is_valid_TOC:=False;

              new_sess_no:=curr_sess_no;

              //TOC_StringGrid.RowCount:=2;
              //TOC_StringGrid.Rows[1].Clear;

              s:='Error while processing read TOC.' + Chr(10) + Chr(13);
              s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
              MessageDlg(s, mtError, [mbOk], 0);
         End;

     Enable_controls;
     Read_TOC_MSF_first_out_Thread:=nil;
     is_busy:=False;

     If Not stop_CD_calls Then
     Begin
          If MCN_Check.Checked Then
          Begin
               ReadDisplayMCN;
          End
          Else
          Begin
               MCN_Edit.Text:='Not read yet.';

               {If Read_ISRC_Check.Checked Then
                  ReadDisplayISRC
               Else
               Begin
                    If is_valid_TOC Then
                    Begin
                         For track_no:=Form1.CommonCDSettings.First_trk_sess_no To Form1.CommonCDSettings.Last_trk_sess_no Do
                         Begin
                              TOC_StringGrid.Cells[5, track_no]:='Not read yet.';
                         End;
                         TOC_StringGrid.Cells[5, Form1.CommonCDSettings.Last_trk_sess_no+1]:='';
                    End;
                End;}
          End;
     End;
End;

Procedure TForm3.ReadDisplayMCN;
Begin
     If Not is_busy Then
     Begin
          is_busy:=True;
          Disable_controls;

          ReadMCNThread:=T_ReadMCNThread.Create(ReadDisplayMCN_done);
     End;
End;

Procedure TForm3.ReadDisplayMCN_done(Sender : TObject);
Var
   track_no : Byte;
Begin
     If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
     Begin
          If Form1.CommonCDSettings.MC_valid Then
          Begin
               MCN_Edit.Text:=Form1.CommonCDSettings.MCN_str;
          End
          Else
          Begin
               MCN_Edit.Text:='Not encoded on the CD.'
               //MessageDlg('MCN is not encoded on the CD.'+IntToStr(T_read_subch_MCN_data(Form1.SCSI.SPC_any_link.Get_data_buf^).Data_format_code), mtInformation, [mbOk], 0);
          End;
     End
     Else
         MessageDlg('Read MCN error.' + Chr(10) + Chr(13) + Form1.SCSI.SPC_any_link.Get_sendcmd_err_msg, mtError, [mbOk], 0);

     Enable_controls;
     ReadMCNThread:=nil;
     is_busy:=False;

     If Not stop_CD_calls Then
     Begin
          If Read_ISRC_Check.Checked Then
              ReadDisplayISRC
          Else
          Begin
               For track_no:=Form1.CommonCDSettings.First_trk_sess_no To Form1.CommonCDSettings.Last_trk_sess_no Do
               Begin
                    TOC_StringGrid.Cells[5, track_no]:='Not read yet.';
               End;
               TOC_StringGrid.Cells[5, Form1.CommonCDSettings.Last_trk_sess_no+1]:='';
          End;
     End;
End;

Procedure TForm3.ReadDisplayISRC;
Begin
     If Not is_busy Then
     Begin
          If is_valid_TOC Then
          Begin
               is_busy:=True;
               Disable_controls;

               track_no:=Form1.CommonCDSettings.First_trk_sess_no;

               ReadISRCThread:=T_ReadISRCThread.Create(ReadDisplayISRC_done, track_no);
          End
          Else
          Begin
               MessageDlg('Cannot read ISRC, TOC could not be read. ', mtError, [mbOk], 0);
          End;
     End;

End;

Procedure TForm3.ReadDisplayISRC_done(Sender : TObject);
Var i : Byte;
    ISRC_ASCII_code : String;
Begin
     If track_no<=Form1.CommonCDSettings.Last_trk_sess_no Then
     Begin
          If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               If Form1.CommonCDSettings.TC_valid Then
               Begin
                    ISRC_ASCII_code:='';
                    For i:=1 To 12 Do
                    Begin
                         ISRC_ASCII_code:=
                         ISRC_ASCII_code+IntToStrFixZeroes(
                         Ord(Form1.CommonCDSettings.ISRC_str[i]), 2);
                    End;
                    TOC_StringGrid.Cells[5, track_no]:=ISRC_ASCII_code;
               End
               Else
               Begin
                    TOC_StringGrid.Cells[5, track_no]:='Not encoded.'
               End;
          End
          Else
              MessageDlg('Read ISRC error for track no. ' +
                         IntToStr(track_no) + Chr(10) + Chr(13) +
                         Form1.SCSI.SPC_any_link.Get_sendcmd_err_msg, mtError, [mbOk], 0);

          track_no:=track_no+1;
          If stop_CD_calls Then
          Begin
               Enable_controls;
               ReadISRCThread:=nil;
               is_busy:=False;
          End
          Else
             ReadISRCThread:=T_ReadISRCThread.Create(ReadDisplayISRC_done, track_no);
     End
     Else
     Begin
          Enable_controls;
          ReadISRCThread:=nil;
          is_busy:=False;
     End;
End;

procedure TForm3.CD_dev_CBChange(Sender: TObject);
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

procedure TForm3.RescanBtnClick(Sender: TObject);
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

Procedure TForm3.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;

     GetCDROMListThread:=nil;
     is_busy:=False;
End;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     stop_CD_calls:=True;

     If GetCDROMListThread<>nil Then
     Begin
          try
             GetCDROMListThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     If Read_TOC_MSF_first_out_Thread<>nil Then
     Begin
          try
             Read_TOC_MSF_first_out_Thread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     If ReadMCNThread<>nil Then
     Begin
          try
             ReadMCNThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     If ReadISRCThread<>nil Then
     Begin
          try
             ReadISRCThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     tracks.Destroy;

     Form1.SetTOCViewerFormNil;
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

procedure TForm3.RereadTOCBtnClick(Sender: TObject);
begin
     Reread;
end;

procedure TForm3.MCN_CheckClick(Sender: TObject);
begin
     If MCN_Check.Checked Then
         ReadDisplayMCN
     //Else
     //    MCN_Edit.Text:='Not read yet.';
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
     Curr_CDROM_indexNo:=0;
     stop_CD_calls:=False;

     new_sess_no:=1;
     curr_sess_no:=1;

     disp_all_sess:=False;

     GetCDROMListThread:=nil;
     ReadMCNThread:=nil;
     ReadISRCThread:=nil;
     Read_TOC_MSF_first_out_Thread:=nil;

     tracks:=T_tracks.Create;

     Read_TOC_format:=MMC_READ_T_P_A_FORMAT_TOC;

     Disable_raw_TOC_controls;

     RescanBtn.Click;
end;

procedure TForm3.Read_ISRC_CheckClick(Sender: TObject);
begin
     If Read_ISRC_Check.Checked Then
        Reread;
end;

procedure TForm3.CB_read_TOC_modeChange(Sender: TObject);
begin
     Case CB_read_TOC_mode.ItemIndex Of
          0: Begin
                  Read_TOC_format:=MMC_READ_T_P_A_FORMAT_TOC;
                  Disable_raw_TOC_controls;
             End;
          1: Begin
                  Read_TOC_format:=MMC_READ_T_P_A_FORMAT_SESS_INFO;
                  Disable_raw_TOC_controls;
             End;
          2: Begin
                  Read_TOC_format:=MMC_READ_T_P_A_FORMAT_FULL_TOC;
                  new_sess_no:=1;
                  curr_sess_no:=1;

                  Enable_raw_TOC_controls;
             End;
     End;
end;

procedure TForm3.ED_sess_noExit(Sender: TObject);
begin
     If Not disp_all_sess Then
     Begin
          If IntToStr(curr_sess_no)<>ED_sess_no.Text Then
          Begin
               new_sess_no:=StrToIntDef(ED_sess_no.Text, 0);
               Reread;
          End;
     End;
end;

procedure TForm3.ED_sess_noKeyPress(Sender: TObject; var Key: Char);
begin
     If Not disp_all_sess Then
     Begin
          Case Key Of
               Chr(13):
               Begin
                    new_sess_no:=StrToIntDef(ED_sess_no.Text, 0);
                    Reread;
               End;
          End;
     End;
end;

procedure TForm3.PrevSess_BtnClick(Sender: TObject);
begin
     If Not disp_all_sess Then
     Begin
          If curr_sess_no>1 Then
          Begin
               new_sess_no:=curr_sess_no-1;
               Reread;
          End
          Else
              MessageDlg('Smallest session no. is 1.', mtError, [mbOk], 0);
     End;
end;

procedure TForm3.NextSess_BtnClick(Sender: TObject);
begin
     If Not disp_all_sess Then
     Begin
          If curr_sess_no<99 Then
          Begin
               new_sess_no:=curr_sess_no+1;
               Reread;
          End
          Else
              MessageDlg('Largest session no. is 99', mtError, [mbOk], 0);
     End;
end;

procedure TForm3.CHK_disp_all_sessClick(Sender: TObject);
begin
     new_sess_no:=1;
     disp_all_sess:=CHK_disp_all_sess.Checked;
     Reread;
end;

procedure TForm3.CHK_disp_raw_QClick(Sender: TObject);
begin
     Reread;
end;

Procedure TForm3.Scan_format_of_data_tracks_from_CD;
Var
   i : Byte;
   MMCLBA : LongInt;
Begin
     If tracks.Tracks.Count>0 Then
     Begin
          For i:=0 To tracks.Tracks.Count-1 Do
          Begin
               MMCLBA:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_MMCLBA(tracks.Get_entry(i).First_MSF.M,
                                                                       tracks.Get_entry(i).First_MSF.S,
                                                                       tracks.Get_entry(i).First_MSF.F);
               Form1.SCSI.MMC1_any_link.Do_readCD_byFormat_CDB12(MMCLBA,
                                                                 1,
                                                                 MMC_SECTORTYPE_ANY,
                                                                 MMC_READCD_SYNC_ALLHDR_USERDATA_EDCECC,
                                                                 MMC_READCD_NO_SUBCH);
               If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
               Begin
                    Case (T_CDROM_format_sync_block(Form1.SCSI.SPC_any_link.Get_data_buf^).Hdr.Data_mode And 3) Of
                    0: tracks.Get_P_entry(i)^.CD_mode:=CDROM_SECTORTYPE_MODE0;
                    1: tracks.Get_P_entry(i)^.CD_mode:=CDROM_SECTORTYPE_MODE1;
                    2: Begin
                            If CompareMem(@(T_CDROM_format_sync_block_mode2_sub_hdrs(Form1.SCSI.SPC_any_link.Get_data_buf^).Sub_hdr.Hdr1),
                                          @(T_CDROM_format_sync_block_mode2_sub_hdrs(Form1.SCSI.SPC_any_link.Get_data_buf^).Sub_hdr.Hdr2), 4)
                                          Then
                            Begin
                                 If ((T_CDROM_format_sync_block_mode2_sub_hdrs(Form1.SCSI.SPC_any_link.Get_data_buf^).Sub_hdr.Hdr1.Sub_mode) And
                                 $20)=0
                                 Then
                                 Begin
                                      tracks.Get_P_entry(i)^.CD_mode:=CDROM_SECTORTYPE_MODE2FORM1;
                                 End
                                 Else
                                 Begin
                                      tracks.Get_P_entry(i)^.CD_mode:=CDROM_SECTORTYPE_MODE2FORM2;
                                 End
                            End
                            Else
                            Begin
                                 tracks.Get_P_entry(i)^.CD_mode:=CDROM_SECTORTYPE_MODE2FORMLESS;
                            End;
                       End;
                    Else
                        tracks.Get_P_entry(i)^.CD_mode:=CDROM_SECTORTYPE_ANY;
                    End;
               End
               Else
                   tracks.Get_P_entry(i)^.CD_mode:=CDROM_SECTORTYPE_UNKNOWN;
          End;
     End;
End;

procedure TForm3.CHK_read_CD_modeClick(Sender: TObject);
begin
     If CHK_read_CD_mode.Checked Then
        Reread;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

end.

