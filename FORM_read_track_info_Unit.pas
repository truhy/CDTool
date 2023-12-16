unit FORM_read_track_info_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GetCDROMListThreadUnit, Grids, MMC1Unit;

Type
    T_control_and_enabled=
    Record
          Ctrl : TControl;
          Enabled : Boolean;
    End;

type
  TForm6 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Device_Lbl: TLabel;
    RescanBtn: TButton;
    CD_dev_CB: TComboBox;
    CB_track_mode: TComboBox;
    Label1: TLabel;
    ED_trackno_LBA: TEdit;
    LBL_enter_trackno_LBA: TLabel;
    BTN_read_track_info: TButton;
    SGRID_track_info: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RescanBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CB_track_modeChange(Sender: TObject);
    procedure ED_trackno_LBAKeyPress(Sender: TObject; var Key: Char);
    procedure ED_trackno_LBAExit(Sender: TObject);
    procedure BTN_read_track_infoClick(Sender: TObject);
    procedure CD_dev_CBChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    GetCDROMListThread : T_GetCDROMListThread;
    Curr_CDROM_indexNo : Integer;
    is_busy : Boolean;
    disabled_ctrl_list : TList;
    track_no : Byte;
    LBA_of_track : LongInt;
    Procedure Disable_controls;
    Procedure Enable_controls;
    Procedure ShowErrMsg(s : String);
    Procedure FillCDComboSelect;
    Procedure SCSI_init_Get_CDROM_list_done(Sender : TObject);
    Procedure Verify_input_of_trackno_LBA;
    Procedure Decode_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_copy_bit_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_damage_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_track_mode_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_RT_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_blank_bit_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_data_mode_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_NWA_V_bit_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_start_address_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_NWA_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_free_blocks_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_fixed_packet_size_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_track_size_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
    Procedure Decode_writing_parameter_restrictions_and_track_status_for_SGRID;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses MainFormUnit;

{$R *.dfm}

Procedure TForm6.Disable_controls;
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

Procedure TForm6.Enable_controls;
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

Procedure TForm6.ShowErrMsg(s : String);
Begin
     MessageDlg(s, mtError, [mbOk], 0);
End;

Procedure TForm6.FillCDComboSelect;
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

Procedure TForm6.SCSI_init_Get_CDROM_list_done(Sender : TObject);
Begin
     FillCDComboSelect;

     Enable_controls;

     GetCDROMListThread:=nil;
     is_busy:=False;
End;

procedure TForm6.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     If GetCDROMListThread<>nil Then
     Begin
          try
             GetCDROMListThread.WaitFor;
          except
                //Ignore exceptions (doesn't display a dialog for them)
          end;
     End;

     Form1.Set_form_read_track_info_Nil; //Prevent main form from showing this form.
     Form1.Visible:=True;
     Action:=caFree; //Remove itself from memory.
end;

procedure TForm6.RescanBtnClick(Sender: TObject);
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

procedure TForm6.FormCreate(Sender: TObject);
Var i : Integer;
begin
     Curr_CDROM_indexNo:=0;
     GetCDROMListThread:=nil;
     track_no:=1;
     LBA_of_track:=0;
     //Field titles.
     SGRID_track_info.Cells[0, 0]:='Field';
     SGRID_track_info.Cells[1, 0]:='Value';
     SGRID_track_info.Cells[2, 0]:='Information';
     //Row field values.
     SGRID_track_info.Cells[0, 1]:='Track no.:';
     SGRID_track_info.Cells[0, 2]:='Session no.:';
     SGRID_track_info.Cells[0, 3]:='Copy bit:';
     SGRID_track_info.Cells[0, 4]:='Damage bit:';
     SGRID_track_info.Cells[0, 5]:='';
     SGRID_track_info.Cells[0, 6]:='Track mode (low 4 bits of Q ctrl):';
     SGRID_track_info.Cells[0, 7]:='';
     SGRID_track_info.Cells[0, 8]:='Reserved bit:';
     SGRID_track_info.Cells[0, 9]:='Blank bit:';
     SGRID_track_info.Cells[0, 10]:='Packet bit:';
     SGRID_track_info.Cells[0, 11]:='Fixed packet bit:';
     SGRID_track_info.Cells[0, 12]:='Data mode (4 bits):';
     SGRID_track_info.Cells[0, 13]:='Starting address (LBA):';
     SGRID_track_info.Cells[0, 14]:='NWA_V bit:';
     SGRID_track_info.Cells[0, 15]:='Next writable address (LBA):';
     SGRID_track_info.Cells[0, 16]:='Free blocks:';
     SGRID_track_info.Cells[0, 17]:='Fixed packet size:';
     SGRID_track_info.Cells[0, 18]:='Track size:';
     SGRID_track_info.Cells[0, 19]:='Write restrictions (if writable):';
     SGRID_track_info.Cells[0, 20]:='';
     SGRID_track_info.Cells[0, 21]:='';
     SGRID_track_info.Cells[0, 22]:='';
     SGRID_track_info.Cells[0, 23]:='Track status indications:';
     SGRID_track_info.Cells[0, 24]:='';

     For i:=1 To SGRID_track_info.RowCount-1 Do
     Begin
          SGRID_track_info.Cells[2, i]:='None';
     End;

     RescanBtn.Click;
end;

procedure TForm6.CB_track_modeChange(Sender: TObject);
begin
     Case CB_track_mode.ItemIndex Of
          0: Begin
                  LBL_enter_trackno_LBA.Caption:='Enter track number:';
                  ED_trackno_LBA.Text:=IntToStr(track_no);
             End;
          1: Begin
                  LBL_enter_trackno_LBA.Caption:='Enter LBA of track:';
                  ED_trackno_LBA.Text:=IntToStr(LBA_of_track);
             End;
     End;
end;

Procedure TForm6.Verify_input_of_trackno_LBA;
var
   new_track_no : Byte;
   new_LBA_of_track : LongInt;
Begin
     Case CB_track_mode.ItemIndex Of
     0: Begin
             new_track_no:=StrToIntDef(ED_trackno_LBA.Text, track_no);
             track_no:=new_track_no;
             ED_trackno_LBA.Text:=IntToStr(track_no);
        End;
     1: Begin
             new_LBA_of_track:=StrToIntDef(ED_trackno_LBA.Text, new_LBA_of_track);
             LBA_of_track:=new_LBA_of_track;
             ED_trackno_LBA.Text:=IntToStr(LBA_of_track);
        End;
     End;
End;

procedure TForm6.ED_trackno_LBAKeyPress(Sender: TObject; var Key: Char);
begin
     Case Key Of
          Chr(13):
          Begin
               Verify_input_of_trackno_LBA;
          End;
     End;
end;

procedure TForm6.ED_trackno_LBAExit(Sender: TObject);
begin
     Verify_input_of_trackno_LBA;
end;

Procedure TForm6.Decode_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode packet bit.
     If out_read_track_info_out_CDB10.RT_Blank_Packet_FP_DataMode And 32=32 Then
     Begin
          SGRID_track_info.Cells[1, 10]:='1';
          SGRID_track_info.Cells[2, 10]:='Restricted to packet writing (if writable)';

          //Decode FP bit.
          If out_read_track_info_out_CDB10.RT_Blank_Packet_FP_DataMode And 16=16 Then
          Begin
               SGRID_track_info.Cells[1, 11]:='1';
               SGRID_track_info.Cells[2, 11]:='Fixed packets only';
          End
          Else
          Begin
               SGRID_track_info.Cells[1, 11]:='0';
               SGRID_track_info.Cells[2, 11]:='Variable packets only';
          End;
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 10]:='0';
          SGRID_track_info.Cells[2, 10]:='Not restricted to packet writing (if writable)';

          If out_read_track_info_out_CDB10.RT_Blank_Packet_FP_DataMode And 16=16 Then
          Begin
               SGRID_track_info.Cells[1, 11]:='1';
          End
          Else
          Begin
               SGRID_track_info.Cells[1, 11]:='0';
          End;
          SGRID_track_info.Cells[2, 11]:='Not restricted to packet writing (if writable)';
     End;
End;

Procedure TForm6.Decode_copy_bit_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode copy bit.
     If out_read_track_info_out_CDB10.Damage_Copy_TrkMode And 16=16 Then
     Begin
          SGRID_track_info.Cells[1, 3]:='1';
          SGRID_track_info.Cells[2, 3]:='Second or higher generation copy';
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 3]:='0';
          SGRID_track_info.Cells[2, 3]:='First generation copy';
     End;
End;

Procedure TForm6.Decode_damage_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode damage bit.
     If out_read_track_info_out_CDB10.Damage_Copy_TrkMode And 32=32 Then
     Begin
          SGRID_track_info.Cells[1, 4]:='1';
          SGRID_track_info.Cells[2, 4]:='Damaged';
          //Need to check NWA_V value for further info.
          If out_read_track_info_out_CDB10.DataLen>6 Then
          Begin
               If out_read_track_info_out_CDB10.NWA_V And 1=1 Then
               Begin
                    SGRID_track_info.Cells[2, 4]:=
                    SGRID_track_info.Cells[2, 4] +
                    ' - not closed due to an incomplete write';

                    SGRID_track_info.Cells[2, 5]:='A repair may be attempted by CLOSE TRACK/SESSION command';
               End
               Else
               Begin
                    SGRID_track_info.Cells[2, 5]:='A repair may be attempted by the next write command';
               End;
          End;

          //Decode packet and FP bits.
          If out_read_track_info_out_CDB10.DataLen>5 Then
          Begin
               Decode_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10);
          End;
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 4]:='0';
          SGRID_track_info.Cells[2, 4]:='Not damaged';
          SGRID_track_info.Cells[2, 5]:='';
     End;
End;

Procedure TForm6.Decode_track_mode_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Var
   Ctrl : Byte;
Begin
     Ctrl:=out_read_track_info_out_CDB10.Damage_Copy_TrkMode AND $0F;
     SGRID_track_info.Cells[1, 6]:=IntToStr(Ctrl);
     //Decode track mode (4 bits).
     If (Ctrl AND 12)=4 Then
     Begin
          SGRID_track_info.Cells[2, 6]:='Data track, ';
          If (Ctrl AND 13)=4 Then
          Begin
               SGRID_track_info.Cells[2, 7]:='recorded uninterrupted (DAO)';
          End
          Else
          Begin
               SGRID_track_info.Cells[2, 7]:='recorded incremental';

          End;
     End
     Else
     Begin
          SGRID_track_info.Cells[2, 6]:='Audio track, ';
          If (Ctrl AND 13)=1 Then
          Begin
               SGRID_track_info.Cells[2, 7]:='2 channels with pre-emphasis of 50/15 us';
          End
          Else
          Begin
               SGRID_track_info.Cells[2, 7]:='2 channels without pre-emphasis';
          End;
     End;
     If (Ctrl AND 2)=2 Then
     Begin
          SGRID_track_info.Cells[2, 6]:=SGRID_track_info.Cells[2, 6] + 'digital copy permitted,';
     End
     Else
     Begin
          SGRID_track_info.Cells[2, 6]:=SGRID_track_info.Cells[2, 6] + 'digital copy prohibited,';
     End;
End;

Procedure TForm6.Decode_RT_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode RT bit.
     If out_read_track_info_out_CDB10.RT_Blank_Packet_FP_DataMode And 128=128 Then
     Begin
          SGRID_track_info.Cells[1, 8]:='1';
          SGRID_track_info.Cells[2, 8]:='Track is reserved';
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 8]:='0';
          SGRID_track_info.Cells[2, 8]:='Track is not reserved';
     End;

     //Decode packet and FP bits.
     Decode_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10);
End;

Procedure TForm6.Decode_blank_bit_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode blank bit.
     If out_read_track_info_out_CDB10.RT_Blank_Packet_FP_DataMode And 64=64 Then
     Begin
          SGRID_track_info.Cells[1, 9]:='1';
          SGRID_track_info.Cells[2, 9]:='Track is blank';
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 9]:='0';
          SGRID_track_info.Cells[2, 9]:='Track is not blank';
     End;
End;

Procedure TForm6.Decode_data_mode_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Var
   data_mode : Byte;
Begin
     data_mode:=out_read_track_info_out_CDB10.RT_Blank_Packet_FP_DataMode And $0F;
     SGRID_track_info.Cells[1, 12]:=IntToStr(data_mode);
     //Decode data mode bit.
     Case data_mode Of
     1: Begin
             SGRID_track_info.Cells[2, 12]:='Mode 1 (ISO/IEC 10149)';
        End;
     2: Begin
             SGRID_track_info.Cells[2, 12]:='Mode 2 (ISO/IEC 10149 or CD-ROM XA)';
        End;
     $0F: Begin
               SGRID_track_info.Cells[2, 12]:='Data Block Type unknown (no track descriptor block)';
          End;
     Else Begin
               SGRID_track_info.Cells[2, 12]:='Reserved';
          End
     End;
End;

Procedure TForm6.Decode_NWA_V_bit_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode NWA_V bit.
     If out_read_track_info_out_CDB10.NWA_V And 1=1 Then
     Begin
          SGRID_track_info.Cells[1, 14]:='1';
          SGRID_track_info.Cells[2, 14]:='Next writable address is valid';
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 14]:='0';
          SGRID_track_info.Cells[2, 14]:='Next writable address is invalid';
     End;
End;

Procedure TForm6.Decode_start_address_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode track start address.
     SGRID_track_info.Cells[1, 13]:=IntToStr(out_read_track_info_out_CDB10.TrkStart_MMCLBA);
     SGRID_track_info.Cells[2, 13]:='Starting address of this track (LBA)';
End;

Procedure TForm6.Decode_NWA_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode Next Writable Address (NWA).
     If SGRID_track_info.Cells[1, 14]='1' Then
     Begin
          SGRID_track_info.Cells[1, 15]:=IntToStr(out_read_track_info_out_CDB10.NextWr_MMCLBA);
          SGRID_track_info.Cells[2, 15]:='Next Writable Address (LBA)';
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 15]:='Invalid';
          SGRID_track_info.Cells[2, 15]:='None';
     End;
End;

Procedure TForm6.Decode_free_blocks_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode free blocks.
     SGRID_track_info.Cells[1, 16]:=IntToStr(out_read_track_info_out_CDB10.FreeBlocks);
     SGRID_track_info.Cells[2, 16]:='Maximum free user data blocks for recording';
End;

Procedure TForm6.Decode_fixed_packet_size_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode fixed packet size.
     If out_read_track_info_out_CDB10.RT_Blank_Packet_FP_DataMode And 48=48 Then
     Begin
          SGRID_track_info.Cells[1, 17]:=IntToStr(out_read_track_info_out_CDB10.FixPackSize);
          SGRID_track_info.Cells[2, 17]:='Size of fixed packets';
     End
     Else
     Begin
          SGRID_track_info.Cells[1, 17]:='Invalid';
          SGRID_track_info.Cells[2, 17]:='None';
     End;
End;

Procedure TForm6.Decode_track_size_for_SGRID(out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block);
Begin
     //Decode track size.
     SGRID_track_info.Cells[1, 18]:=IntToStr(out_read_track_info_out_CDB10.TrkSize);
     SGRID_track_info.Cells[2, 18]:='No. of user data blocks in this track';
End;

Procedure TForm6.Decode_writing_parameter_restrictions_and_track_status_for_SGRID;
Var
   RT_bit : String;
   Blank_bit : String;
   Packet_bit : String;
   FP_bit : String;
Begin
     SGRID_track_info.Cells[1, 19]:='-';
     SGRID_track_info.Cells[1, 23]:='-';

     RT_bit:=SGRID_track_info.Cells[1, 8];
     Blank_bit:=SGRID_track_info.Cells[1, 9];
     Packet_bit:=SGRID_track_info.Cells[1, 10];
     FP_bit:=SGRID_track_info.Cells[1, 11];
     If RT_bit='0' Then
     Begin
          If Blank_bit='0' Then
          Begin
               If Packet_bit='0' Then
               Begin
                    SGRID_track_info.Cells[2, 19]:='Cannot write to stamped disc, or during TAO on';
                    SGRID_track_info.Cells[2, 20]:='invisible track, or writing SAO mode';
                    SGRID_track_info.Cells[2, 21]:='';
                    SGRID_track_info.Cells[2, 22]:='';

                    SGRID_track_info.Cells[2, 23]:='Write method: DAO/TAO/SAO';
                    SGRID_track_info.Cells[2, 24]:='Track status: Complete/During TAO/SAO';
               End
               Else
               Begin
                    SGRID_track_info.Cells[2, 19]:='Packet writing only. In write parameters mode page';
                    SGRID_track_info.Cells[2, 20]:='parameters common to READ TRACK INFO must match';
                    SGRID_track_info.Cells[2, 21]:='';
                    SGRID_track_info.Cells[2, 22]:='';

                    If FP_bit='0' Then
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Write method: Variable packets';
                         SGRID_track_info.Cells[2, 24]:='Track status: Incomplete';
                    End
                    Else
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Write method: Fixed packets';
                         SGRID_track_info.Cells[2, 24]:='Track status: Incomplete';
                    End;
               End;
          End
          Else
          Begin
               If Packet_bit='0' Then
               Begin
                    SGRID_track_info.Cells[2, 19]:='Packet or TAO writing allowed. If this track is first track';
                    SGRID_track_info.Cells[2, 20]:='of session, then SAO is allowed';
                    SGRID_track_info.Cells[2, 21]:='';
                    SGRID_track_info.Cells[2, 22]:='';

                    If FP_bit='0' Then
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Write method: TAO/Variable or fixed packets/May be SAO';
                         SGRID_track_info.Cells[2, 24]:='Track status: Invisible';
                    End
                    Else
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Invalid state';
                         SGRID_track_info.Cells[2, 24]:='';
                    End;
               End
               Else
               Begin
                    SGRID_track_info.Cells[2, 19]:='Invalid state';
                    SGRID_track_info.Cells[2, 20]:='';
                    SGRID_track_info.Cells[2, 21]:='';
                    SGRID_track_info.Cells[2, 22]:='';

                    If FP_bit='0' Then
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Invalid state';
                         SGRID_track_info.Cells[2, 24]:='';
                    End
                    Else
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Invalid state';
                         SGRID_track_info.Cells[2, 24]:='';
                    End;
               End;
          End;
     End
     Else
     Begin
          If Blank_bit='0' Then
          Begin
               If Packet_bit='0' Then
               Begin
                    SGRID_track_info.Cells[2, 19]:='Can''t write to recorded track or during TAO on';
                    SGRID_track_info.Cells[2, 20]:='reserved track';
                    SGRID_track_info.Cells[2, 21]:='';
                    SGRID_track_info.Cells[2, 22]:='';

                    SGRID_track_info.Cells[2, 23]:='Write method: TAO';
                    SGRID_track_info.Cells[2, 24]:='Track status: Complete/During TAO';
               End
               Else
               Begin
                    SGRID_track_info.Cells[2, 19]:='Packet writing only. In write parameters mode page';
                    SGRID_track_info.Cells[2, 20]:='parameters common to READ TRACK INFO must match';
                    SGRID_track_info.Cells[2, 21]:='';
                    SGRID_track_info.Cells[2, 22]:='';

                    If FP_bit='0' Then
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Write method: Variable packets';
                         SGRID_track_info.Cells[2, 24]:='Track status: Complete partially recorded reserve';
                    End
                    Else
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Write method: Fixed packets';
                         SGRID_track_info.Cells[2, 24]:='Track status: Complete partially recorded reserve';
                    End;
               End;
          End
          Else
          Begin
               If Packet_bit='0' Then
               Begin
                    SGRID_track_info.Cells[2, 19]:='TAO writing only. In write parameters mode page:';
                    SGRID_track_info.Cells[2, 20]:='Track mode set to same as in READ TRACK INFO.';
                    SGRID_track_info.Cells[2, 21]:='If copyright bit is clear then copy bit may be set.';
                    SGRID_track_info.Cells[2, 22]:='All other common parameters must match.';

                    SGRID_track_info.Cells[2, 23]:='Write method: TAO';
                    SGRID_track_info.Cells[2, 24]:='Track status: Empty reserved';
               End
               Else
               Begin
                    SGRID_track_info.Cells[2, 19]:='Packet writing only. In write parameters mode page:';
                    SGRID_track_info.Cells[2, 20]:='Track mode set to same as in READ TRACK INFO.';
                    SGRID_track_info.Cells[2, 21]:='If copyright bit is clear then copy bit may be set.';
                    SGRID_track_info.Cells[2, 22]:='All other common parameters must match.';

                    If FP_bit='0' Then
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Write method: Variable or fixed packets';
                         SGRID_track_info.Cells[2, 24]:='Track status: Empty reserved';
                    End
                    Else
                    Begin
                         SGRID_track_info.Cells[2, 23]:='Invalid state';
                         SGRID_track_info.Cells[2, 24]:='';
                    End;
               End;
          End;
     End;
End;

procedure TForm6.BTN_read_track_infoClick(Sender: TObject);
Var
   MMCLBA_TrkNo : Integer;
   out_read_track_info_out_CDB10 : T_out_read_track_info_CDB10_block;
   s : String;
   i : Integer;
begin
     If Not is_busy Then
     Begin
          is_busy:=True;
          Disable_controls;

          Case CB_track_mode.ItemIndex Of
          0: Begin
                  MMCLBA_TrkNo:=track_no;
             End;
          1: Begin
                  MMCLBA_TrkNo:=LBA_of_track;
             End;
          End;

          Form1.SCSI.MMC1_any_link.Do_read_track_info_out_CDB10(1-CB_track_mode.ItemIndex, MMCLBA_TrkNo, out_read_track_info_out_CDB10);
          If Form1.SCSI.SPC_any_link.Get_is_sendcmd_OK Then
          Begin
               If out_read_track_info_out_CDB10.DataLen>1 Then
               Begin
                    SGRID_track_info.Cells[1, 1]:=IntToStr(out_read_track_info_out_CDB10.TrkNo);
                    SGRID_track_info.Cells[2, 1]:='Track no. of this information';

                    If out_read_track_info_out_CDB10.DataLen>2 Then
                    Begin
                         SGRID_track_info.Cells[1, 2]:=IntToStr(out_read_track_info_out_CDB10.SessNo);
                         SGRID_track_info.Cells[2, 2]:='Session no. of track';

                         If out_read_track_info_out_CDB10.DataLen>4 Then
                         Begin
                              Decode_copy_bit_for_SGRID(out_read_track_info_out_CDB10);

                              Decode_damage_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10);

                              Decode_track_mode_for_SGRID(out_read_track_info_out_CDB10);

                              If out_read_track_info_out_CDB10.DataLen>5 Then
                              Begin
                                   Decode_RT_packet_FP_bits_for_SGRID(out_read_track_info_out_CDB10);

                                   Decode_blank_bit_for_SGRID(out_read_track_info_out_CDB10);

                                   Decode_data_mode_for_SGRID(out_read_track_info_out_CDB10);

                                   If out_read_track_info_out_CDB10.DataLen>6 Then
                                   Begin
                                        Decode_NWA_V_bit_for_SGRID(out_read_track_info_out_CDB10);

                                        If out_read_track_info_out_CDB10.DataLen>10 Then
                                        Begin
                                             Decode_start_address_for_SGRID(out_read_track_info_out_CDB10);

                                             If out_read_track_info_out_CDB10.DataLen>14 Then
                                             Begin
                                                  Decode_NWA_for_SGRID(out_read_track_info_out_CDB10);

                                                  If out_read_track_info_out_CDB10.DataLen>18 Then
                                                  Begin
                                                       Decode_free_blocks_for_SGRID(out_read_track_info_out_CDB10);

                                                       If out_read_track_info_out_CDB10.DataLen>22 Then
                                                       Begin
                                                            Decode_fixed_packet_size_for_SGRID(out_read_track_info_out_CDB10);

                                                            If out_read_track_info_out_CDB10.DataLen>26 Then
                                                            Begin
                                                                 Decode_track_size_for_SGRID(out_read_track_info_out_CDB10);
                                                            End;
                                                       End;
                                                  End;
                                             End;
                                        End;
                                   End;
                              End;

                              If SGRID_track_info.Cells[1, 10]='' Then
                              Begin
                                   SGRID_track_info.Cells[1, 10]:='Invalid';
                                   SGRID_track_info.Cells[2, 10]:='None';

                                   SGRID_track_info.Cells[1, 11]:='Invalid';
                                   SGRID_track_info.Cells[2, 11]:='None';
                              End;

                              Decode_writing_parameter_restrictions_and_track_status_for_SGRID;
                         End;
                    End;
               End;
          End
          Else
          Begin
               For i:=1 To SGRID_track_info.RowCount-1 Do
               Begin
                    SGRID_track_info.Cells[1, i]:='';
                    SGRID_track_info.Cells[2, i]:='None';
               End;

               s:='Error while processing read track info command.' + Chr(10) + Chr(13);
               s:=s + Form1.SCSI.MMC1_any_link.Get_err_msg;
               MessageDlg(s, mtError, [mbOk], 0);
          End;

          is_busy:=False;
          Enable_controls;
     End;
end;

procedure TForm6.CD_dev_CBChange(Sender: TObject);
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

procedure TForm6.FormDestroy(Sender: TObject);
begin
     Form1.CommonCDSettings.SCSI_select_deinit_CDROM;
end;

end.
