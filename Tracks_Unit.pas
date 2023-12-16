unit Tracks_Unit;

interface

uses Classes,
     SysUtils,
     CDROM_struct_Unit,
     MMC1Unit,
     TOCUnit;

type T_P_track_entry=^T_track_entry;
     T_track_entry=Record
                         Number    : Byte;
                         First_MSF : T_MSF;
                         Last_MSF  : T_MSF;
                         CD_mode   : Byte;
                   End;

type T_tracks=class
     private
            Track_entry : T_track_entry;
     public
           Tracks : TList;
           Constructor Create; Overload;
           Constructor Create(in_TOC : T_TOC); Overload;
           Constructor Create(in_sector_file : TFileStream;
                              in_sector_file_block_size : Word;
                              in_sector_file_offset : Integer;
                              in_TOC : T_TOC); Overload;
           Destructor Destroy; Override;
           Procedure Add_entry(in_track_entry : T_track_entry);
           Procedure Del_entries;
           Function Cmp_LE_MSF(Time1 : T_MSF; Time2 : T_MSF) : Boolean;
           Procedure Fill_last_MSF;
           Function Get_entry(entry_index : Byte) : T_track_entry;
           Function Get_P_entry(entry_index : Byte) : T_P_track_entry;
           Function Find_track_no_entry_by_track_no(Var out_track_entry : T_track_entry; track_no : Byte) : Boolean;
           Function Find_track_no_entry_by_MSF(Var out_track_entry : T_track_entry; in_MSF : T_MSF) : Boolean;
           Function Find_track_no_entry_by_LBA(Var out_track_entry : T_track_entry; in_LBA : LongInt) : Boolean;
     end;

implementation

uses MainFormUnit;

Constructor T_tracks.Create;
Begin
     Inherited Create;

     tracks:=TList.Create;
End;

Constructor T_tracks.Create(in_TOC : T_TOC);
Var i : Integer;
    track_entry : T_track_entry;
Begin
     Inherited Create;

     tracks:=TList.Create;

     For i:=0 To in_TOC.TOC_data.Count-1 Do
     Begin
          If T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Control_ADR AND $0F=1 Then
          Begin
               Case T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Index_or_Point Of
               1..99: //, $A2:
               Begin
                    track_entry.Number:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Index_or_Point;
                    track_entry.First_MSF.M:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Abs_or_Point_M;
                    track_entry.First_MSF.S:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Abs_or_Point_S;
                    track_entry.First_MSF.F:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Abs_or_Point_F;

                    Case (T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Control_ADR AND $D0) Of
                    $00,$10,$80,$90:
                    Begin
                         track_entry.CD_mode:=CDROM_SECTORTYPE_CDDA;
                    End;
                    $40,$50:
                    Begin
                         track_entry.CD_mode:=Scan_format_of_data_track_from_cd(track_entry.First_MSF);
                    End
                    Else
                        track_entry.CD_mode:=CDROM_SECTORTYPE_CDDA;
                    End;

                    Add_entry(track_entry);
               End;
               End;
          End;
     End;

     Fill_last_MSF;
End;

Constructor T_tracks.Create(in_sector_file : TFileStream;
                            in_sector_file_block_size : Word;
                            in_sector_file_offset : Integer;
                            in_TOC : T_TOC);
Var i : Integer;
    track_entry : T_track_entry;
Begin
     Inherited Create;

     tracks:=TList.Create;

     For i:=0 To in_TOC.TOC_data.Count-1 Do
     Begin
          //Only look for Q mode 1.
          If T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Control_ADR AND $0F=1 Then
          Begin
               //Only include track no.s 1..99.
               Case T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Index_or_Point Of
               1..99: //, $A2:
               Begin
                    track_entry.Number:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Index_or_Point;
                    track_entry.First_MSF.M:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Abs_or_Point_M;
                    track_entry.First_MSF.S:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Abs_or_Point_S;
                    track_entry.First_MSF.F:=T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Abs_or_Point_F;

                    Case (T_TOC_data_PQ(in_TOC.TOC_data.Items[i]^).Q.Control_ADR AND $D0) Of
                    $00,$10,$80,$90:
                    Begin
                         //TOC identifies track as CDDA audio format.
                         track_entry.CD_mode:=CDROM_SECTORTYPE_CDDA;
                    End;
                    $40,$50:
                    Begin
                         //TOC identifies track as CDROM data format.
                         //Scan file to identify which CDROM data format.
                         track_entry.CD_mode:=Scan_format_of_data_track_from_file(in_sector_file,
                                                                                  in_sector_file_block_size,
                                                                                  in_sector_file_offset,
                                                                                  track_entry.First_MSF);
                    End
                    Else
                        //Assume unknown type as CDDA audio format.
                        track_entry.CD_mode:=CDROM_SECTORTYPE_CDDA;
                    End;

                    Add_entry(track_entry);
               End;
               End;
          End;
     End;

     Fill_last_MSF;
End;

Destructor T_tracks.Destroy;
Begin
     Del_entries;
     Tracks.Free;

     Inherited;
End;

Procedure T_tracks.Add_entry(in_track_entry : T_track_entry);
Var
   P_new_track_entry : T_P_track_entry;
Begin
     New(P_new_track_entry);
     P_new_track_entry^:=in_track_entry;
     tracks.Add(P_new_track_entry);
End;

Procedure T_tracks.Del_entries;
Var i : Integer;
    P_track_entry : T_P_track_entry;
Begin
     For i:=0 To (tracks.Count - 1) Do
     Begin
          P_track_entry := tracks.Items[i];
          Dispose(P_track_entry);
     End;
     tracks.Clear;
End;

Function T_tracks.Cmp_LE_MSF(Time1 : T_MSF; Time2 : T_MSF) : Boolean;
Var is_less_than : Boolean;
Begin
     If Time1.M<Time2.M Then
     Begin
          is_less_than:=True;
     End
     Else If Time1.M=Time2.M Then
          Begin
               If Time1.S<Time2.S Then
               Begin
                    is_less_than:=True;
               End
               Else If Time1.S=Time2.S Then
                    Begin
                         If Time1.F<Time2.F Then
                         Begin
                              is_less_than:=True;
                         End
                         Else If Time1.F=Time2.F Then
                              Begin
                                   is_less_than:=True;
                              End
                              Else
                              Begin
                                   is_less_than:=False;
                              End;
                    End
                    Else
                    Begin
                         is_less_than:=False;
                    End;
          End
          Else
          Begin
               is_less_than:=False;
          End;

     Cmp_LE_MSF:=is_less_than;
End;

Procedure T_tracks.Fill_last_MSF;
Var i, j : Integer;
    out_track_entry : T_track_entry;
    LBA_less1 : LongInt;
Begin
     For i:=0 To tracks.Count-1 Do
     Begin
          Case T_track_entry(tracks.Items[i]^).Number Of
            1..99:
            Begin
                 For j:=0 To tracks.Count-1 Do
                 Begin
                      If i<>j Then
                      Begin
                           If Cmp_LE_MSF(T_track_entry(tracks.Items[j]^).First_MSF,
                                         T_track_entry(tracks.Items[i]^).First_MSF) Then
                           Begin
                                LBA_less1:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(out_track_entry.First_MSF.M,
                                                                                        out_track_entry.First_MSF.S,
                                                                                        out_track_entry.First_MSF.F)-1;
                                Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(LBA_less1,
                                                                             T_track_entry(tracks.Items[i]^).Last_MSF.M,
                                                                             T_track_entry(tracks.Items[i]^).Last_MSF.S,
                                                                             T_track_entry(tracks.Items[i]^).Last_MSF.F);
                                Break; //exit the loop.
                           End;
                      End;
                 End

                 {If Find_track_no_entry_by_track_no(out_track_entry, $A2) Then
                 Begin
                      LBA_less1:=Form1.SCSI.MMC1.MSF_to_LBA(out_track_entry.First_MSF.M,
                                                            out_track_entry.First_MSF.S,
                                                            out_track_entry.First_MSF.F)-1;
                      Form1.SCSI.MMC1.LBA_to_MSF(LBA_less1,
                                                 T_track_entry(tracks.Items[i]^).Last_MSF.M,
                                                 T_track_entry(tracks.Items[i]^).Last_MSF.S,
                                                 T_track_entry(tracks.Items[i]^).Last_MSF.F);
                 End;}
            End;
            $A2:
            Begin
                 LBA_less1:=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_track_entry(tracks.Items[i]^).First_MSF.M,
                                                                         T_track_entry(tracks.Items[i]^).First_MSF.S,
                                                                         T_track_entry(tracks.Items[i]^).First_MSF.F)+6750;
                 Form1.SCSI.MMC1_any_link.Get_MMC1.LBA_to_MSF(LBA_less1,
                                                              T_track_entry(tracks.Items[i]^).Last_MSF.M,
                                                              T_track_entry(tracks.Items[i]^).Last_MSF.S,
                                                              T_track_entry(tracks.Items[i]^).Last_MSF.F);
            End
          End;
     End;
End;

Function T_tracks.Get_entry(entry_index : Byte) : T_track_entry;
Begin
     Get_entry:=T_track_entry(tracks.Items[entry_index]^);
End;

Function T_tracks.Get_P_entry(entry_index : Byte) : T_P_track_entry;
Begin
     Get_P_entry:=T_P_track_entry(tracks.Items[entry_index]);
End;

Function T_tracks.Find_track_no_entry_by_track_no(Var out_track_entry : T_track_entry; track_no : Byte) : Boolean;
Var i : Integer;
    find_entry_result : Boolean;
Begin
     find_entry_result:=False;
     For i:=0 To tracks.Count-1 Do
     Begin
          If T_track_entry(tracks.Items[i]^).Number=track_no Then
          Begin
               out_track_entry:=T_track_entry(tracks.Items[i]^);
               find_entry_result:=True;

               Break;
          End;
     End;

     Find_track_no_entry_by_track_no:=find_entry_result;
End;

Function T_tracks.Find_track_no_entry_by_MSF(Var out_track_entry : T_track_entry; in_MSF : T_MSF) : Boolean;
Var i : Integer;
    find_entry_result : Boolean;
Begin
     find_entry_result:=False;
     For i:=0 To tracks.Count-1 Do
     Begin
          If CMP_LE_MSF(in_MSF, T_track_entry(tracks.Items[i]^).Last_MSF) Then
          Begin
               out_track_entry:=T_track_entry(tracks.Items[i]^);
               find_entry_result:=True;

               Break;
          End
     End;

     Find_track_no_entry_by_MSF:=find_entry_result;
End;

Function T_tracks.Find_track_no_entry_by_LBA(Var out_track_entry : T_track_entry; in_LBA : LongInt) : Boolean;
Var i : Integer;
    find_entry_result : Boolean;
Begin
     find_entry_result:=False;
     For i:=0 To tracks.Count-1 Do
     Begin
          If in_LBA<=Form1.SCSI.MMC1_any_link.Get_MMC1.MSF_to_LBA(T_track_entry(tracks.Items[i]^).Last_MSF.M,
                                                                  T_track_entry(tracks.Items[i]^).Last_MSF.S,
                                                                  T_track_entry(tracks.Items[i]^).Last_MSF.F) Then
          Begin
               out_track_entry:=T_track_entry(tracks.Items[i]^);
               find_entry_result:=True;

               Break;
          End
     End;

     Find_track_no_entry_by_LBA:=find_entry_result;
End;

end.
