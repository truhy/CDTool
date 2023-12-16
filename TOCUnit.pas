unit TOCUnit;

interface

uses Classes,
     SysUtils,
     CommonCDSettingsUnit;

const
     TOC_FILE_TYPE_2448_FULL_DAO_FILE=1;
     TOC_FILE_TYPE_2448_FULL_LI_FILE=2;
     TOC_FILE_TYPE_96_REPEATED_DEINT=3;
     TOC_FILE_TYPE_96_UNIQUE_DEINT=4;
     TOC_FILE_TYPE_96_UNIQUE_INT=5;

type T_P_TOC_entry=^T_TOC_entry;
     T_TOC_entry=T_subch_deint_PW_96;
     T_P_TOC_data_PQ=^T_TOC_data_PQ;
     T_TOC_data_Q=Record
                        Control_ADR    : Byte;
                        TrackNo        : Byte;
                        Index_or_Point : Byte;
                        Rel_M          : Byte;
                        Rel_S          : Byte;
                        Rel_F          : Byte;
                        Zero           : Byte;
                        Abs_or_Point_M : Byte;
                        Abs_or_Point_S : Byte;
                        Abs_or_Point_F : Byte;
                        CRC16_MSB      : Byte;
                        CRC16_LSB      : Byte;
                  End;
     T_TOC_data_PQ=Record
                         P : Array[1..12] Of Byte;
                         Q : T_TOC_data_Q;
                   End;

type T_TOC=class
     private
            TOC_entry : T_TOC_entry;
     public
           TOC_data  : TList;
           Constructor Create; Overload;
           Constructor Create(in_toc_file : TFileStream; SubCh_block_size : Byte; lead_in_file_type : Byte); Overload;
           Destructor Destroy; Override;
           Procedure Add_entry(in_TOC_entry : T_TOC_entry);
           Procedure Del_entries;
           Function Find_mode_point_entry(in_mode       : Byte;
                                          in_point      : Byte) : Boolean; overload;
           Function Find_mode_point_entry(Var out_TOC_entry : T_TOC_entry;
                                              in_mode       : Byte;
                                              in_point      : Byte) : Boolean; overload;
           Function Find_mode_point_entry_ptr(out_TOC_entry : T_P_TOC_entry;
                                              in_mode       : Byte;
                                              in_point      : Byte) : Boolean;
     end;

implementation

Uses
    MainFormUnit;

Constructor T_TOC.Create;
Begin
     Inherited Create;

     TOC_data:=TList.Create;
End;

Constructor T_TOC.Create(in_toc_file : TFileStream; SubCh_block_size : Byte; lead_in_file_type : Byte);
Var First_TOC_entry : T_TOC_entry;
    sub_int_PW_96 : T_sub_int_PW_96;
Begin
     Inherited Create;

     TOC_data:=TList.Create;

     Case lead_in_file_type Of
     TOC_FILE_TYPE_2448_FULL_DAO_FILE,
     TOC_FILE_TYPE_2448_FULL_LI_FILE: //Lead-in type: 2448 bytes per sector (sub-channel + main data)
     Begin                    //(TOC is repeated till full length of lead_in - 150 sectors)
          If in_toc_file.Position<in_toc_file.Size Then
          Begin
               in_toc_file.Seek(2352, soFromCurrent);
               in_toc_file.ReadBuffer(First_TOC_entry, SubCh_block_size);
               Add_entry(First_TOC_entry);

               While (in_toc_file.Position+7248)<in_toc_file.Size Do
               Begin
                    in_toc_file.Seek(7248, soFromCurrent);  //7248=2448+2448+2352
                    in_toc_file.ReadBuffer(TOC_entry, SubCh_block_size);
                    If CompareMem(@First_TOC_entry, @TOC_entry, SubCh_block_size-81)=True Then
                        Break
                    Else
                        Add_entry(TOC_entry);
               End;
          End;
     End;
     TOC_FILE_TYPE_96_REPEATED_DEINT: //Lead-in type: 96 bytes of sub-channel data only per sector
     Begin   //(TOC is repeated till full length of lead-in - 150 sectors)
          If in_toc_file.Position<in_toc_file.Size Then
          Begin
               in_toc_file.ReadBuffer(First_TOC_entry, SubCh_block_size);
               Add_entry(First_TOC_entry);

               While (in_toc_file.Position+192)<in_toc_file.Size Do
               Begin
                    in_toc_file.Seek(192, soFromCurrent);  //192=96+96
                    in_toc_file.ReadBuffer(TOC_entry, SubCh_block_size);
                    If CompareMem(@First_TOC_entry, @TOC_entry, SubCh_block_size-81)=True Then
                        Break
                    Else
                        Add_entry(TOC_entry);
               End;
          End;
     End;
     TOC_FILE_TYPE_96_UNIQUE_DEINT: //Lead-in type: 96 bytes of sub-channel data only per sector
     Begin   //(TOC is not repeated)
          While in_toc_file.Position<in_toc_file.Size Do
          Begin
               in_toc_file.ReadBuffer(TOC_entry, SubCh_block_size);
               Add_entry(TOC_entry);
          End;
     End;
     TOC_FILE_TYPE_96_UNIQUE_INT: //Lead-in type: 96 bytes of sub-channel data only per sector
     Begin   //(TOC is not repeated)
          While in_toc_file.Position<in_toc_file.Size Do
          Begin
               in_toc_file.ReadBuffer(sub_int_PW_96, SubCh_block_size);
               Form1.CommonCDSettings.Deint_subs(@sub_int_PW_96, @TOC_entry);
               Add_entry(TOC_entry);
          End;
     End;
     End;

     in_toc_file.Seek(0, soFromBeginning);
End;

Destructor T_TOC.Destroy;
Begin
     Del_entries;
     TOC_data.Free;

     Inherited;
End;

Procedure T_TOC.Add_entry(in_TOC_entry : T_TOC_entry);
Var
   P_new_TOC_entry : T_P_TOC_entry;
Begin
     New(P_new_TOC_entry);
     P_new_TOC_entry^:=in_TOC_entry;
     TOC_data.Add(P_new_TOC_entry);
End;

Procedure T_TOC.Del_entries;
Var i : Integer;
    P_TOC_entry : T_P_TOC_entry;
Begin
     For i:=0 To (TOC_data.Count - 1) Do
     Begin
          P_TOC_entry := TOC_data.Items[i];
          Dispose(P_TOC_entry);
     End;
     TOC_data.Clear;
End;

Function T_TOC.Find_mode_point_entry(in_mode  : Byte;
                                     in_point : Byte) : Boolean;
Var i : Integer;
    find_entry_result : Boolean;
Begin
     find_entry_result:=False;
     For i:=0 To (TOC_data.Count - 1) Do
     Begin
          If T_TOC_data_PQ(TOC_data.Items[i]^).Q.Control_ADR AND $0F=in_mode Then
          Begin
               If T_TOC_data_PQ(TOC_data.Items[i]^).Q.Index_or_Point=in_point Then
               Begin
                    find_entry_result:=True;
                    Break;
               End;
          End;
     End;
     Find_mode_point_entry:=find_entry_result;
End;

Function T_TOC.Find_mode_point_entry(Var out_TOC_entry : T_TOC_entry;
                                         in_mode       : Byte;
                                         in_point      : Byte) : Boolean;
Var i : Integer;
    find_entry_result : Boolean;
Begin
     find_entry_result:=False;
     For i:=0 To (TOC_data.Count - 1) Do
     Begin
          If T_TOC_data_PQ(TOC_data.Items[i]^).Q.Control_ADR AND $0F=in_mode Then
          Begin
               If T_TOC_data_PQ(TOC_data.Items[i]^).Q.Index_or_Point=in_point Then
               Begin
                    out_TOC_entry:=T_TOC_entry(TOC_data.Items[i]^);
                    find_entry_result:=True;
                    Break;
               End;
          End;
     End;
     Find_mode_point_entry:=find_entry_result;
End;

Function T_TOC.Find_mode_point_entry_ptr(out_TOC_entry : T_P_TOC_entry;
                                         in_mode       : Byte;
                                         in_point      : Byte) : Boolean;
Var i : Integer;
    find_entry_result : Boolean;
Begin
     find_entry_result:=False;
     For i:=0 To (TOC_data.Count - 1) Do
     Begin
          If T_TOC_data_PQ(TOC_data.Items[i]^).Q.Control_ADR AND $0F=in_mode Then
          Begin
               If T_TOC_data_PQ(TOC_data.Items[i]^).Q.Index_or_Point=in_point Then
               Begin
                    out_TOC_entry:=TOC_data.Items[i];
                    find_entry_result:=True;
                    Break;
               End;
          End;
     End;
     Find_mode_point_entry_ptr:=find_entry_result;
End;

end.
