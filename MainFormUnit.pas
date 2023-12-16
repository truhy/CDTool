unit MainFormUnit;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  { My units.. }
  SCSIUnit,
  WNASPI32Unit,
  SPC_Unit,
  MMC1Unit,
  CDFormUtilsUnit,
  CDToImageFormUnit,
  CommonCDSettingsUnit,
  ImageToCDFormUnit,
  ATIP_form_Unit,
  MSR_form_Unit,
  FORM_read_track_info_Unit,
  FORM_read_disc_info_Unit,
  Tools_Unit,
  XPMan, ExtCtrls;

type
  TForm1 = class(TForm)
    SectorViewerBtn: TButton;
    TOCViewerBtn: TButton;
    CDToImageBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    BTN_image_to_CD: TButton;
    BTN_ATIP_form: TButton;
    BTN_MSR: TButton;
    XPManifest1: TXPManifest;
    BTN_read_track_info: TButton;
    BTN_read_disc_info: TButton;
    RG_interface: TRadioGroup;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SectorViewerBtnClick(Sender: TObject);
    procedure TOCViewerBtnClick(Sender: TObject);
    procedure CDToImageBtnClick(Sender: TObject);
    procedure BTN_image_to_CDClick(Sender: TObject);
    procedure BTN_ATIP_formClick(Sender: TObject);
    procedure BTN_MSRClick(Sender: TObject);
    procedure BTN_read_track_infoClick(Sender: TObject);
    procedure BTN_read_disc_infoClick(Sender: TObject);
    procedure RG_interfaceClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SCSI        : T_SCSI;
    CDFormUtils : T_CDFormUtils;
    Form4       : TForm4;
    Form5       : TForm5;
    MSR_form    : T_MSR_form;
    Form6       : TForm6;
    Form7       : TForm7;
    CommonCDSettings : T_CommonCDSettings;
    Procedure Disable_BTNs;
    Procedure Enable_BTNs;
    Procedure Open_SPT;
    Procedure Open_WNASPI32;
    Procedure Calc_new_height_top(in_height : Integer; in_top : Integer; Var new_height : Integer; Var new_top : Integer);
    Procedure Calc_new_width_left(in_width : Integer; in_left : Integer; Var new_width : Integer; Var new_left : Integer);
    Function Calc_top(in_height : Integer; in_top : Integer) : Integer;
    Function Calc_left(in_width : Integer; in_left : Integer) : Integer;
    Procedure SetSectorViewerFormNil;
    Procedure SetTOCViewerFormNil;
    Procedure SetCDToImageFormNil;
    Procedure SetImageToCDFormNil;
    procedure SetATIPFormNil;
    procedure Set_MSRFormNil;
    procedure Set_form_read_track_info_Nil;
    procedure Set_form_read_disc_info_Nil;
  end;

{ Global variables.. }
var
  Form1: TForm1;

implementation

{ Private to this unit.. }
Uses
    SectorViewerUnit,
    TOCViewerUnit;

Var
    Form2 : TForm2;
    Form3 : TForm3;
    ATIP_form : TATIP_form;

{$R *.DFM}

Procedure TForm1.Open_SPT;
Begin
     //Create SCSI class object for use with Win32 DeviceIoControl() & SCSI PASS THRUOGH
     SCSI:=T_SCSI.Create(U_METHOD_SCSI_PASS_THRU);
     CommonCDSettings:=T_CommonCDSettings.Create;
     CDFormUtils:=T_CDFormUtils.Create;

     RG_interface.ItemIndex:=0;

     Enable_BTNs;
End;

Procedure TForm1.Open_WNASPI32;
Var
   s : String;
Begin
     //Create SCSI class object for use with Adaptec's WNASPI32
     SCSI:=T_SCSI.Create(U_METHOD_WNASPI);
     //Try to open and link to WNASPI32.DLL
     If SCSI.SPC_any_link.WNASPI32.Get_OpenASPIStatus=USER_WNASPI32_OPENASPI_DLL_LOAD_ERR Then
     Begin
          Show;
          s:='WNASPI32.DLL could not be loaded.' + Chr(10) + Chr(13);
          s:=s + 'Install or reinstall WinASPI32 drivers.';
          MessageDlg(s, mtError, [mbOk], 0);
          Disable_BTNs;
          //Application.Terminate;
     End
     Else
     Begin
          If SCSI.SPC_any_link.WNASPI32.Get_OpenASPIStatus=USER_WNASPI32_OPENASPI_FX_IMPORT_ERR Then
          Begin
               Show;
               s:='Could not import the minimum required functions from WNASPI32.DLL.' + Chr(10) + Chr(13);
               s:=s + 'You need to install updated WinASPI32 drivers.';
               MessageDlg(s, mtError, [mbOk], 0);
               Disable_BTNs;
               //Application.Terminate;
          End
          Else
          Begin
               If SCSI.SPC_any_link.WNASPI32.Get_SRB_status_OK=False Then
               Begin
                    Show;
                    s:='Initialisation: GetASPI32SupportInfo function call failed.' + Chr(10) + Chr(13);
                    s:=s + 'WinASPI32 driver error.' + Chr(10) + Chr(13);
                    s:=s + SCSI.SPC_any_link.WNASPI32.Get_SRB_err_msg;
                    MessageDlg(s, mtError, [mbOk], 0);
                    Disable_BTNs;
                    //Application.Terminate;
               End
               Else
               Begin
                    CommonCDSettings:=T_CommonCDSettings.Create;

                    CDFormUtils:=T_CDFormUtils.Create;

                    CommonCDSettings.Set_default_timeout_buf;

                    RG_interface.ItemIndex:=1;

                    Enable_BTNs;
               End;
          End;
     End;
End;

procedure TForm1.FormCreate(Sender: TObject);
begin
     If IsWinNT4OrHigher Then
     Begin
          Open_SPT;
     End
     Else
     Begin
          Open_WNASPI32;
     End;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     CommonCDSettings.Free;
     CDFormUtils.Free;
     SCSI.Free;
end;

Procedure TForm1.Disable_BTNs;
Begin
     SectorViewerBtn.Enabled:=False;
     TOCViewerBtn.Enabled:=False;
     CDToImageBtn.Enabled:=False;
     BTN_image_to_CD.Enabled:=False;
     BTN_MSR.Enabled:=False;
     BTN_ATIP_form.Enabled:=False;
     BTN_read_track_info.Enabled:=False;
     BTN_read_disc_info.Enabled:=False;
End;

Procedure TForm1.Enable_BTNs;
Begin
     SectorViewerBtn.Enabled:=True;
     TOCViewerBtn.Enabled:=True;
     CDToImageBtn.Enabled:=True;
     BTN_image_to_CD.Enabled:=True;
     BTN_MSR.Enabled:=True;
     BTN_ATIP_form.Enabled:=True;
     BTN_read_track_info.Enabled:=True;
     BTN_read_disc_info.Enabled:=True;
End;

Procedure TForm1.Calc_new_height_top(in_height : Integer; in_top : Integer; Var new_height : Integer; Var new_top : Integer);
Begin
     new_height:=Screen.DesktopHeight-200;
     new_top:=in_top - (new_height-in_height) Div 2;
End;

Procedure TForm1.Calc_new_width_left(in_width : Integer; in_left : Integer; Var new_width : Integer; Var new_left : Integer);
Begin
     new_width:=Screen.DesktopWidth-500;
     new_left:=in_left - (new_width-in_width) Div 2;
End;

Function TForm1.Calc_top(in_height : Integer; in_top : Integer) : Integer;
Var
   new_top : Integer;
Begin
     //Check if form will be cut off at the top of the screen.
     If in_top<0 Then
     Begin
          new_top:=0;
     End
     Else
     Begin
          //Check if form will be cut off at the bottom of the screen.
          If (in_top+in_height)>Screen.DesktopHeight Then
          Begin
               new_top:=Screen.DesktopHeight-in_height;
          End
          Else
          Begin
              new_top:=-1;
          End;
     End;

     Result:=new_top;
End;

Function TForm1.Calc_left(in_width : Integer; in_left : Integer) : Integer;
Var
   new_left : Integer;
Begin
     //Check if form will be cut off at the left of the screen.
     If in_left<0 Then
     Begin
          new_left:=0;
     End
     Else
     Begin
          //Check if form will be cut off at the right of the screen.
          If (in_left+in_width)>Screen.DesktopWidth Then
          Begin
               new_left:=Screen.DesktopWidth-in_width;
          End
          Else
          Begin
               new_left:=-1;
          End;
     End;

     Result:=new_left;
End;

Procedure TForm1.SetSectorViewerFormNil;
Begin
     Form2:=Nil;
End;

procedure TForm1.SectorViewerBtnClick(Sender: TObject);
var
   new_height : Integer;
   new_top : Integer;
   new_width : Integer;
   new_left : Integer;
begin
     If Form2=Nil Then
     Begin
          Form2:=TForm2.Create(Form1);
     End;
     If Form2<>Nil Then
     Begin
          Form2.Show;
          Self.Visible:=False;

          Calc_new_height_top(Form2.Height, Form2.Top, new_height, new_top);
          If new_height>Form2.Height Then
          Begin
               Form2.Top:=new_top;
               Form2.Height:=new_height;
          End;
          new_top:=Calc_top(Form2.Height, Form2.Top);
          If new_top>-1 Then
          Begin
               Form2.Top:=new_top;
          End;
          Calc_new_width_left(Form2.Width, Form2.Left, new_width, new_left);
          If new_width>Form2.Width Then
          Begin
               Form2.Left:=new_left;
               Form2.Width:=new_width;
          End;
          new_left:=Calc_left(Form2.Width, Form2.Left);
          If new_left>-1 Then
          Begin
               Form2.Left:=new_left;
          End;
     End;
end;

Procedure TFOrm1.SetTOCViewerFormNil;
Begin
     Form3:=Nil;
End;

procedure TForm1.TOCViewerBtnClick(Sender: TObject);
var
   new_height : Integer;
   new_top : Integer;
   new_width : Integer;
   new_left : Integer;
begin
     If Form3=Nil Then
     Begin
          Form3:=TForm3.Create(Form1);
     End;
     If Form3<>Nil Then
     Begin
          Form3.Show;
          Self.Visible:=False;

          Calc_new_height_top(Form3.Height, Form3.Top, new_height, new_top);
          If new_height>Form3.Height Then
          Begin
               Form3.Top:=new_top;
               Form3.Height:=new_height;
          End;
          new_top:=Calc_top(Form3.Height, Form3.Top);
          If new_top>-1 Then
          Begin
               Form3.Top:=new_top;
          End;
          Calc_new_width_left(Form3.Width, Form3.Left, new_width, new_left);
          If new_width>Form3.Width Then
          Begin
               Form3.Left:=new_left;
               Form3.Width:=new_width;
          End;
          new_left:=Calc_left(Form3.Width, Form3.Left);
          If new_left>-1 Then
          Begin
               Form3.Left:=new_left;
          End;
     End;
end;

Procedure TForm1.SetCDToImageFormNil;
Begin
     Form4:=Nil;
End;

procedure TForm1.CDToImageBtnClick(Sender: TObject);
var
   new_top : Integer;
   new_left : Integer;
begin
     If Form4=Nil Then
     Begin
          Form4:=TForm4.Create(Form1);
     End;
     If Form4<>Nil Then
     Begin
          Form4.Show;
          Self.Visible:=False;

          new_top:=Calc_top(Form4.Height, Form4.Top);
          If new_top>-1 Then
          Begin
               Form4.Top:=new_top;
          End;
          new_left:=Calc_left(Form4.Width, Form4.Left);
          If new_left>-1 Then
          Begin
               Form4.Left:=new_left;
          End;
     End
end;

Procedure TForm1.SetImageToCDFormNil;
Begin
     Form5:=Nil;
End;

procedure TForm1.BTN_image_to_CDClick(Sender: TObject);
var
   new_height : Integer;
   new_top : Integer;
   new_width : Integer;
   new_left : Integer;
begin
     If Form5=Nil Then
     Begin
          Form5:=TForm5.Create(Form1);
     End;
     If Form5<>Nil Then
     Begin
          Form5.Show;
          Self.Visible:=False;

          new_top:=Calc_top(Form5.Height, Form5.Top);
          If new_top>-1 Then
          Begin
               Form5.Top:=new_top;
          End;
          new_left:=Calc_left(Form5.Width, Form5.Left);
          If new_left>-1 Then
          Begin
               Form5.Left:=new_left;
          End;
     End
end;

Procedure TForm1.SetATIPFormNil;
Begin
     ATIP_form:=Nil;
End;

procedure TForm1.BTN_ATIP_formClick(Sender: TObject);
var
   new_height : Integer;
   new_top : Integer;
   new_width : Integer;
   new_left : Integer;
begin
     If ATIP_form=Nil Then
     Begin
          ATIP_form:=TATIP_form.Create(Form1);
     End;
     If ATIP_form<>Nil Then
     Begin
          ATIP_form.Show;
          Self.Visible:=False;

          Calc_new_height_top(ATIP_form.Height, ATIP_form.Top, new_height, new_top);
          If new_height>ATIP_form.Height Then
          Begin
               ATIP_form.Top:=new_top;
               ATIP_form.Height:=new_height;
          End;
          new_top:=Calc_top(ATIP_form.Height, ATIP_form.Top);
          If new_top>-1 Then
          Begin
               ATIP_form.Top:=new_top;
          End;
          Calc_new_width_left(ATIP_form.Width, ATIP_form.Left, new_width, new_left);
          If new_width>ATIP_form.Width Then
          Begin
               ATIP_form.Left:=new_left;
               ATIP_form.Width:=new_width;
          End;
          new_left:=Calc_left(ATIP_form.Width, ATIP_form.Left);
          If new_left>-1 Then
          Begin
               ATIP_form.Left:=new_left;
          End;
     End
end;

Procedure TForm1.Set_MSRFormNil;
Begin
     MSR_form:=Nil;
End;

procedure TForm1.BTN_MSRClick(Sender: TObject);
var
   new_height : Integer;
   new_top : Integer;
   new_width : Integer;
   new_left : Integer;
begin
     If MSR_form=Nil Then
     Begin
          MSR_form:=T_MSR_form.Create(Form1);
     End;
     If MSR_form<>Nil Then
     Begin
          MSR_form.Show;
          Self.Visible:=False;

          new_top:=Calc_top(MSR_form.Height, MSR_form.Top);
          If new_top>-1 Then
          Begin
               MSR_form.Top:=new_top;
          End;
          new_left:=Calc_left(MSR_form.Width, MSR_form.Left);
          If new_left>-1 Then
          Begin
               MSR_form.Left:=new_left;
          End;
     End
end;

Procedure TForm1.Set_form_read_track_info_Nil;
Begin
     Form6:=Nil;
End;

procedure TForm1.BTN_read_track_infoClick(Sender: TObject);
var
   new_height : Integer;
   new_top : Integer;
   new_width : Integer;
   new_left : Integer;
begin
     If Form6=Nil Then
     Begin
          Form6:=TForm6.Create(Form6);
     End;
     If Form6<>Nil Then
     Begin
          Form6.Show;
          Self.Visible:=False;

          Calc_new_height_top(Form6.Height, Form6.Top, new_height, new_top);
          If new_height>Form6.Height Then
          Begin
               Form6.Top:=new_top;
               Form6.Height:=new_height;
          End;
          new_top:=Calc_top(Form6.Height, Form6.Top);
          If new_top>-1 Then
          Begin
               Form6.Top:=new_top;
          End;
          Calc_new_width_left(Form6.Width, Form6.Left, new_width, new_left);
          If new_width>Form6.Width Then
          Begin
               Form6.Left:=new_left;
               Form6.Width:=new_width;
          End;
          new_left:=Calc_left(Form6.Width, Form6.Left);
          If new_left>-1 Then
          Begin
               Form6.Left:=new_left;
          End;
     End
end;

Procedure TForm1.Set_form_read_disc_info_Nil;
Begin
     Form7:=Nil;
End;

procedure TForm1.BTN_read_disc_infoClick(Sender: TObject);
var
   new_height : Integer;
   new_top : Integer;
   new_width : Integer;
   new_left : Integer;
begin
     If Form7=Nil Then
     Begin
          Form7:=TForm7.Create(Form7);
     End;
     If Form7<>Nil Then
     Begin
          Form7.Show;
          Self.Visible:=False;

          Calc_new_height_top(Form7.Height, Form7.Top, new_height, new_top);
          If new_height>Form7.Height Then
          Begin
               Form7.Top:=new_top;
               Form7.Height:=new_height;
          End;
          new_top:=Calc_top(Form7.Height, Form7.Top);
          If new_top>-1 Then
          Begin
               Form7.Top:=new_top;
          End;
          Calc_new_width_left(Form7.Width, Form7.Left, new_width, new_left);
          If new_width>Form7.Width Then
          Begin
               Form7.Left:=new_left;
               Form7.Width:=new_width;
          End;
          new_left:=Calc_left(Form7.Width, Form7.Left);
          If new_left>-1 Then
          Begin
               Form7.Left:=new_left;
          End;
     End
end;

procedure TForm1.RG_interfaceClick(Sender: TObject);
begin
     Case RG_interface.ItemIndex Of
     0: Begin
             SCSI.Free;
             Open_SPT;
        End;
     1: Begin
             SCSI.Free;
             Open_WNASPI32;
        End;
     End;
end;

end.
