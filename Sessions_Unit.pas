unit Sessions_Unit;

interface

uses Classes,
     TOCUnit,
     Tracks_Unit;

type T_P_session_entry=^T_session_entry;
     T_session_entry=Record
                           TOC     : T_TOC;
                           Number  : Byte;
                           Tracks  : T_tracks;
                     End;

type T_sessions=class
     private
            Session_entry : T_session_entry;
     public
           Sessions : TList;
           Constructor Create; Overload;
           Destructor Destroy; Override;
           Procedure Add_entry(in_session_entry : T_session_entry);
           Procedure Del_entries;
           Function Get_entry(entry_index : Byte) : T_session_entry;
     end;

implementation

Constructor T_sessions.Create;
Begin
     Inherited Create;

     sessions:=TList.Create;
End;

Destructor T_sessions.Destroy;
Begin
     Del_entries;
     sessions.Free;

     Inherited;
End;

Procedure T_sessions.Add_entry(in_session_entry : T_session_entry);
Var
   P_new_session_entry : T_P_session_entry;
Begin
     New(P_new_session_entry);
     P_new_session_entry^:=in_session_entry;
     sessions.Add(P_new_session_entry);
End;

Procedure T_sessions.Del_entries;
Var i : Integer;
    P_session_entry : T_P_session_entry;
Begin
     For i:=0 To (sessions.Count - 1) Do
     Begin
          P_session_entry := sessions.Items[i];
          Dispose(P_session_entry);
     End;
     sessions.Clear;
End;

Function T_sessions.Get_entry(entry_index : Byte) : T_session_entry;
Begin
     Get_entry:=T_session_entry(sessions.Items[entry_index]^);
End;

end.
