program CDTool;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form1},
  SectorViewerUnit in 'SectorViewerUnit.pas' {Form2},
  TOCViewerUnit in 'TOCViewerUnit.pas' {Form3},
  CDToImageFormUnit in 'CDToImageFormUnit.pas' {Form4},
  CDFormUtilsUnit in 'CDFormUtilsUnit.pas',
  Tools_Unit in 'Tools_Unit.pas',
  SCSIUnit in 'SCSIUnit.pas',
  WNASPI32Unit in 'WNASPI32Unit.pas',
  MMC1Unit in 'MMC1Unit.pas',
  SPC_Unit in 'SPC_Unit.pas',
  CDToImageThreadUnit in 'CDToImageThreadUnit.pas',
  GetCDROMListThreadUnit in 'GetCDROMListThreadUnit.pas',
  CommonCDSettingsUnit in 'CommonCDSettingsUnit.pas',
  ReadCDSectorThreadUnit in 'ReadCDSectorThreadUnit.pas',
  ReadMCNThreadUnit in 'ReadMCNThreadUnit.pas',
  ReadISRCThreadUnit in 'ReadISRCThreadUnit.pas',
  Read_TOC_MSF_first_out_ThreadUnit in 'Read_TOC_MSF_first_out_ThreadUnit.pas',
  ImageToCDFormUnit in 'ImageToCDFormUnit.pas' {Form5},
  ImageToCDProgressFormUnit in 'ImageToCDProgressFormUnit.pas' {ImageToCDProgressForm},
  ImageToCDReadThreadUnit in 'ImageToCDReadThreadUnit.pas',
  ImageToCDWriteThreadUnit in 'ImageToCDWriteThreadUnit.pas',
  TOCUnit in 'TOCUnit.pas',
  EDC_ECC_Unit in 'EDC_ECC_Unit.pas',
  Tracks_Unit in 'Tracks_Unit.pas',
  Sessions_Unit in 'Sessions_Unit.pas',
  Disk_info_Unit in 'Disk_info_Unit.pas',
  CDROM_struct_Unit in 'CDROM_struct_Unit.pas',
  ATIP_form_Unit in 'ATIP_form_Unit.pas' {ATIP_form},
  CRC_Unit in 'CRC_Unit.pas',
  MSR_form_Unit in 'MSR_form_Unit.pas' {MSR_form},
  SBC_Unit in 'SBC_Unit.pas',
  CDToImageProgressFormUnit in 'CDToImageProgressFormUnit.pas' {CDToImageProgressForm},
  MSRProgressFormUnit in 'MSRProgressFormUnit.pas' {MSRProgressForm},
  MSRThreadUnit in 'MSRThreadUnit.pas',
  CDROMTableSectorScramblerUnit in 'CDROMTableSectorScramblerUnit.pas',
  ImageToCD_DispStatus_Thread_Unit in 'ImageToCD_DispStatus_Thread_Unit.pas',
  SPT_Unit in 'SPT_Unit.pas',
  SPC_WNASPI32_Unit in 'SPC_WNASPI32_Unit.pas',
  SBC_WNASPI32_Unit in 'SBC_WNASPI32_Unit.pas',
  MMC1_WNASPI32_Unit in 'MMC1_WNASPI32_Unit.pas',
  SPC_any_link_Unit in 'SPC_any_link_Unit.pas',
  SBC_any_link_Unit in 'SBC_any_link_Unit.pas',
  MMC1_any_link_Unit in 'MMC1_any_link_Unit.pas',
  SPC_SPT_Unit in 'SPC_SPT_Unit.pas',
  MMC1_SPT_Unit in 'MMC1_SPT_Unit.pas',
  SBC_SPT_Unit in 'SBC_SPT_Unit.pas',
  FORM_read_track_info_Unit in 'FORM_read_track_info_Unit.pas' {Form6},
  FORM_read_disc_info_Unit in 'FORM_read_disc_info_Unit.pas' {Form7};

{$R *.RES}

begin
     Application.Initialize;
     Application.CreateForm(TForm1, Form1);
     Application.Run;
end.
