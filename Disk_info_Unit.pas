unit Disk_info_Unit;

interface

uses Classes,
     TOCUnit,
     Sessions_Unit;

type T_disk_info=class
     private
     public
           TOC      : T_TOC;
           Sessions : T_Sessions;
           CD_mode  : Byte;

           Constructor Create; Overload;
           Destructor Destroy; Override;
     end;

implementation

Constructor T_disk_info.Create;
Begin
     Inherited Create;
End;

Destructor T_disk_info.Destroy;
Begin
     Inherited;
End;

end.
