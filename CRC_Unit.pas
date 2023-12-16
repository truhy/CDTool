{ *********************************************************************
  Code written by Truong Hy.

  Provides 2 functions to calculate:
    - CRC16 X25 standard
    - CRC32
  values.

  It uses a table (array) of already calculated XORing values for
  optimised (byte wise XOR) calculation of CRC value.

  The code can still be further optimised (even faster calculation if
  CPU supports this) by using word/long word wise XORing.  New tables
  will have to be used.  Although this will have it's limitations:
    - CPU should have word/long word XOR which is faster than byte wise
      XOR
    - the input must be word/long word aligned
  ********************************************************************* }
unit CRC_Unit;

interface

{ ***********************************************************************
  Include table (array) of already calculated XORing values for optimised
  (byte wise) calculation of CRC value.
  *********************************************************************** }
{$I CRC16_table.PAS}
{$I CRC32_table.PAS}

Type p_byte=^Byte;

{ Calculates CRC16 X25 standard value with output and input bits not reversed (normal). 
  Uses forward CRC algorithm. }
Function Calc_CRC16(p_buf : Pointer; len : LongWord) : Word;

{ Calculates CRC32 value with output and input bits reversed.
  Uses reversed CRC algorithm. }
Function Calc_CRC32(p_buf : Pointer; len : LongWord) : LongWord;

implementation

Function Calc_CRC16(p_buf : Pointer; len : LongWord) : Word;
Var i       : LongWord;
    crc_val : Word;
Begin
     crc_val:=0;
     For i:=1 To len Do
     Begin
          crc_val:=CRC16_tab[((crc_val SHR 8) XOR p_byte(p_buf)^) AND $00FF] XOR (crc_val SHL 8);
          Inc(p_byte(p_buf));
     End;

     Calc_CRC16:=crc_val;
End;

Function Calc_CRC32(p_buf : Pointer; len : LongWord) : LongWord;
Var i       : LongWord;
    crc_val : LongWord;
Begin
     crc_val:=0;
     For i:=1 To len Do
     Begin
          crc_val:=CRC32_tab[(crc_val XOR p_byte(p_buf)^) AND $000000FF] XOR (crc_val SHR 8);
          Inc(p_byte(p_buf));
     End;

     Calc_CRC32:=crc_val;
End;

end.
