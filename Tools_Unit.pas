unit Tools_Unit;

interface

Uses SysUtils,
     Windows;

Function IsWinNT4OrHigher : Boolean;

{ A procedure to copy a string from memory to a Pascal string.
  The string must not contain any null characters. }
Procedure PStrMLCopy(Var Dest : String; Source : Pointer; Len : LongWord);

{ A procedure to copy a string from memory and append to a Pascal string.
  The string must not contain any null characters. }
Procedure PStrMLCopyAppend(Var Dest : String; Source : Pointer; Len : LongWord);

{ A procedure to copy a string from memory to a Pascal string and format it.
  The string can contain any null characters.
  Format:
    The string will be divided into lines using Chr(13) and LineLen.
    ASCII characters are appended to each line with leading 3 spaces.
    Non-displayable characters are replaced with RepChar. }
Procedure Buf2PasStrHexASCIILines(Var Dest : String;
                                  Source   : Pointer;
                                  Len      : LongWord;
                                  RepChar  : Char;
                                  LineLen  : Byte);

{ A procedure to copy a string from memory to a Pascal string and format it.
  The string can contain any null characters.
  Format:
    The string will be converted into hex string of 2 digits and divided into
    lines using Chr(13) and LineLen. }
Procedure Buf2PasStrHexLines(Var Dest : String;
                             Source   : Pointer;
                             Len      : LongWord;
                             LineLen  : Byte);

{ Converts a byte into its binary representation as a string of 1 and 0 chars. }                             
Function ByteToBin(Num : Byte) : String;

{ A procedure to copy a string from memory to a Pascal string and format it.
  The string can contain any null characters.
  Format:
    The string will be converted into hex string of 2 digits and divided into
    lines using Chr(13) and LineLen. }
Procedure Buf2PasStrBinLines(Var Dest : String;
                             Source   : Pointer;
                             Len      : LongWord;
                             LineLen  : Byte);

{ A function to append 0s to make values look fixed. }
Function IntToStrFixZeroes(i : Integer;
                           n : Byte) : String;

{ A procedure to copy a string from memory to a Pascal string and format it.
  The string can contain any null characters.
  Format:
    The string will be converted into decimal string of 3 digits and divided into
    lines using Chr(13) and LineLen. }
Procedure Buf2PasStrDecLines(Var Dest : String;
                             Source   : Pointer;
                             Len      : LongWord;
                             LineLen  : Byte);

{ Uses Win32 API to get available physical RAM (excludes virtual). }
Function GetAvailPhysMemory : Cardinal;

{ Uses Win32 API to get total physical RAM (excludes virtual). }
Function GetTotalPhysMemory : Cardinal;

implementation

Function IsWinNT4OrHigher : Boolean;
Begin
     Result := (Win32Platform = VER_PLATFORM_WIN32_NT) And (Win32MajorVersion >= 4);
End;

Procedure PStrMLCopy(Var Dest : String; Source : Pointer; Len : LongWord);
Var P_str : PChar;
Begin
     P_str:=StrAlloc(Len+1);

     StrLCopy(P_str, Source, Len);
     Dest:=String(P_str);

     StrDispose(P_str);
End;

{Procedure PStrMLCopy(Var Dest : String; Source : Pointer; Len : LongWord);
Begin
     SetLength(Dest, Len+1);
     StrLCopy(PChar(Dest), Source, Len);
End;}

Procedure PStrMLCopyAppend(Var Dest : String; Source : Pointer; Len : LongWord);
Var P_str : PChar;
Begin
     P_str:=StrAlloc(Len+1);

     StrLCopy(P_str, Source, Len);
     Dest:=Dest + String(P_str);

     StrDispose(P_str);
End;

Procedure Buf2PasStrHexASCIILines(Var Dest : String;
                                  Source   : Pointer;
                                  Len      : LongWord;
                                  RepChar  : Char;
                                  LineLen  : Byte);
Var SourcePos : Pointer;
    ASCIIStr  : String;
    HexStr    : String;
    LineStr   : String;
    i         : LongWord;
Begin
     ASCIIStr:='';
     HexStr:='';
     LineStr:='';
     SourcePos:=Source;
     For i:=1 To Len Do
     Begin
          If Char(SourcePos^) In ['A'..'Z',
                                  'a'..'z',
                                  '0'..'9',
                                  '!'..'/',
                                  ':'..'@',
                                  '['..'`',
                                  '{'..'~'] Then
          Begin
               ASCIIStr:=ASCIIStr+Char(SourcePos^);
          End
          Else
              ASCIIStr:=ASCIIStr+RepChar;

          HexStr:=HexStr+IntToHex(Byte(SourcePos^), 2);

          If LineLen>0 Then
          Begin
               If i Mod LineLen=0 Then
               Begin
                    If i<>Len Then
                        LineStr:=LineStr+HexStr+'  '+ASCIIStr+Chr(13)
                    Else
                        LineStr:=LineStr+HexStr+'  '+ASCIIStr;
                    HexStr:='';
                    ASCIIStr:='';
               End;
          End;

          Inc(Integer(SourcePos));
     End;
     If HexStr<>'' Then
     Begin
          LineStr:=LineStr+HexStr+'  '+ASCIIStr;
     End;
     Dest:=LineStr;
End;

Procedure Buf2PasStrHexLines(Var Dest : String;
                             Source   : Pointer;
                             Len      : LongWord;
                             LineLen  : Byte);
Var SourcePos : Pointer;
    HexStr    : String;
    i         : LongWord;
Begin
     HexStr:='';
     SourcePos:=Source;
     For i:=1 To Len Do
     Begin
          HexStr:=HexStr+IntToHex(Byte(SourcePos^), 2);

          If LineLen>0 Then
          Begin
               If i Mod LineLen=0 Then
               Begin
                    If i<>Len Then
                        HexStr:=HexStr+Chr(13);
               End;
          End;

          Inc(Integer(SourcePos));
     End;
     Dest:=HexStr;
End;

Function ByteToBin(Num : Byte) : String;
Var i : Byte;
    BinStr : String;
    Num_tmp : Byte;
Begin
     Num_tmp:=Num;
     BinStr:='';

     For i:=1 To 8 Do
     Begin
          If (Num_tmp AND 1)=0 Then
          Begin
               BinStr:='0'+BinStr;
          End
          Else
          Begin
               BinStr:='1'+BinStr;
          End;
          Num_tmp:=Num_tmp SHR 1;
     End;

     Result:=BinStr;
End;

Procedure Buf2PasStrBinLines(Var Dest : String;
                             Source   : Pointer;
                             Len      : LongWord;
                             LineLen  : Byte);
Var SourcePos : Pointer;
    BinStr    : String;
    i         : LongWord;
Begin
     BinStr:='';
     SourcePos:=Source;
     For i:=1 To Len Do
     Begin
          BinStr:=BinStr+ByteToBin(Byte(SourcePos^))+' ';

          If LineLen>0 Then
          Begin
               If i Mod LineLen=0 Then
               Begin
                    If i<>Len Then
                        BinStr:=BinStr+Chr(13);
               End;
          End;

          Inc(Integer(SourcePos));
     End;
     Dest:=BinStr;
End;

Function IntToStrFixZeroes(i : Integer;
                           n : Byte) : String;
Var s : String;
    l : LongWord;
Begin
     s:=IntToStr(i);
     l:=Length(s);
     If (l<n) Then
         s:=StringOfChar('0', n-l)+s;

     IntToStrFixZeroes:=s;
End;

Procedure Buf2PasStrDecLines(Var Dest : String;
                             Source   : Pointer;
                             Len      : LongWord;
                             LineLen  : Byte);
Var SourcePos : Pointer;
    DecLineStr    : String;
    i         : LongWord;
Begin
     DecLineStr:='';
     SourcePos:=Source;
     For i:=1 To Len Do
     Begin
          DecLineStr:=DecLineStr+IntToStrFixZeroes(Byte(SourcePos^), 3)+' ';

          If LineLen>0 Then
          Begin
               If i Mod LineLen=0 Then
               Begin
                    If i<>Len Then
                        DecLineStr:=DecLineStr+Chr(13);
               End;
          End;

          Inc(Integer(SourcePos));
     End;
     Dest:=DecLineStr;
End;

Function GetAvailPhysMemory : Cardinal;
var
   MemInfo : TMemoryStatus;
begin
     MemInfo.dwLength := Sizeof (MemInfo);
     GlobalMemoryStatus (MemInfo);
     result:=MemInfo.dwAvailPhys;
end;

Function GetTotalPhysMemory : Cardinal;
var
   MemInfo : TMemoryStatus;
begin
     MemInfo.dwLength := Sizeof (MemInfo);
     GlobalMemoryStatus (MemInfo);
     result:=MemInfo.dwTotalPhys;
end;

end.
