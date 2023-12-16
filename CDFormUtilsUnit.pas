unit CDFormUtilsUnit;

interface

Uses Classes, Stdctrls, Dialogs, Sysutils, Grids, Forms,
     { My units.. }
     SCSIUnit, WNASPI32Unit, SPC_Unit, MMC1Unit, Tools_Unit, CommonCDSettingsUnit;

Type
    P_TForm=^TForm;

Type T_CDFormUtils=Class
                   Private
                   Public
                          Procedure FillCB_with_CDROM(In_CD_dev_CB: TComboBox);
                   End;

implementation

uses MainFormUnit;

Procedure T_CDFormUtils.FillCB_with_CDROM(In_CD_dev_CB: TComboBox);
Var i       : Integer;
    Str_msg : String;
Begin
     In_CD_dev_CB.Items.BeginUpdate;
     In_CD_dev_CB.Items.Clear;
     If Form1.CommonCDSettings.CDROM_list.Count>=1 Then
     Begin
          For i:=0 To Form1.CommonCDSettings.CDROM_list.Count-1 Do
          Begin
               With Form1.CommonCDSettings Do
               Begin
                    Case Form1.SCSI.interface_method Of
                    U_METHOD_WNASPI:
                    Begin
                         Str_msg:=IntToStr(T_CDROM_rec(CDROM_list[i]^).HA_ID) + ':';
                         Str_msg:=Str_msg + IntToStr(T_CDROM_rec(CDROM_list[i]^).Dev_ID) + ':';
                         Str_msg:=Str_msg + IntToStr(T_CDROM_rec(CDROM_list[i]^).LUN) + ' / ';
                         Str_msg:=Str_msg + T_CDROM_rec(CDROM_list[i]^).Name;
                    End;
                    U_METHOD_SCSI_PASS_THRU,
                    U_METHOD_SCSI_PASS_THRU_D:
                    Begin
                         Str_msg:=T_CDROM_SPT_rec(CDROM_list[i]^).Drive_letter + ': ';
                         Str_msg:=Str_msg + T_CDROM_SPT_rec(CDROM_list[i]^).Vendor + ' / ';
                         Str_msg:=Str_msg + T_CDROM_SPT_rec(CDROM_list[i]^).Product + ' / ';
                         Str_msg:=Str_msg + T_CDROM_SPT_rec(CDROM_list[i]^).Revision;
                    End;
                    End;
               End;
               In_CD_dev_CB.Items.Add(Str_msg);
          End;
     End;
     In_CD_dev_CB.Items.EndUpdate;
End;

end.
