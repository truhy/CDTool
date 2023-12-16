{ *************************************************************
  The functions encode_L2_P and encode_L2_Q were taken from
  CDRDAO v1.1.5 source code.

  The C++ code from the source was translated into Delphi, with
  no changes made.

  The 2 functions will calculate CDROM ECC (Error Correction
  Code) parity P and Q.  The values are written at the correct
  position into the buffer that was passed in as parameter.
  ECC P is written at offset 2076 and it's length is 172 bytes.
  ECC Q is written at offset 2248 and it's length is 104 bytes.
  Calculation uses a table of calculated values to do byte wise
  XOR.  This will improve performance instead of doing bit wise
  XORing.

  The function Calc_EDC is written by me and serves as a wrapper
  to Calc_CRC32.  The EDC (Error Detection Code) values used in
  CDROM sectors are actually just the normal CRC 32 values of
  the data.
  ************************************************************* }

unit EDC_ECC_Unit;

interface

uses CRC_Unit,
     Windows;

Function Calc_EDC(in_data : Pointer; n : Word) : LongWord;

Procedure encode_L2_P(inout_data : Pointer; start : Word);

Procedure encode_L2_Q(inout_data : Pointer; start : Word);

implementation

{$I 'Encoder_tables.pas'}

Const RS_L12_BITS = 8;

Type P_byte=^Byte;

Function Calc_EDC(in_data : Pointer; n : Word) : LongWord;
Begin
     //Calc CRC32
     Calc_EDC:=Calc_CRC32(in_data, n);
End;

Procedure encode_L2_P(inout_data : Pointer; start : Word);
Var
   p, p2 : P_byte;
   i, j, base, sum : SmallInt;
   data : Byte;
Begin
     p := inout_data;
     Inc(p, 2076);
     ZeroMemory(p, 172);

     Inc(P_byte(inout_data), start);

     For j := 0 To 42 Do
     Begin
          For i := 0 To 23 Do
          Begin
               { LSB }
               p2 := inout_data;
               Inc(p2, i*2*43);
               data := p2^;
	       If (data <> 0) Then
               Begin
		    base := rs_l12_log[data];

		    sum := base + DP[0][i];

		    If (sum >= $FF) Then
		       sum := sum - $FF;

                    p^ := p^ XOR rs_l12_alog[sum];

		    sum := base + DP[1][i];

		    If (sum >= $FF) Then
		       sum := sum - $FF;

                    p2:=p;
                    Inc(p2, 86);
                    p2^ := p2^ XOR rs_l12_alog[sum];
	       End;
               { MSB }
               p2 := inout_data;
               Inc(p2, i*86+1);
               data := p2^;
	       If (data <> 0) Then
               Begin
                    base := rs_l12_log[data];

                    sum := base + DP[0][i];
	            If (sum >= $FF) Then
                       sum := sum - $FF;

                    p2 := p;
                    Inc(p2, 1);
	            p2^ := p2^ XOR rs_l12_alog[sum];

	            sum := base + DP[1][i];
	            If (sum >= $FF) Then
                       sum := sum - $FF;

                    p2 := p;
                    Inc(p2, 87);
                    p2^ := p2^ XOR rs_l12_alog[sum];
	       End;
          End;
          Inc(p, 2);

          Inc(P_byte(inout_data), 2);
     End;
End;

Procedure encode_L2_Q(inout_data : Pointer; start : Word);
Var
   q, p2 : P_byte;
   i, j, base, sum : SmallInt;
   data : Byte;
Begin
     q := inout_data;
     Inc(q, 2248);
     ZeroMemory(q, 104);

     Inc(P_byte(inout_data), start);

     For j := 0 To 25 Do
     Begin
          For i := 0 To 42 Do
          Begin
               { LSB }
               p2 := inout_data;
               Inc(p2, (j*86+i*88) MOD 2236);
               data := p2^;

	       If (data <> 0) Then
               Begin
		    base := rs_l12_log[data];

		    sum := base + DQ[0][i];
                    If (sum >= $FF) Then
		       sum := sum - $FF;

                    q^ := q^ XOR rs_l12_alog[sum];

		    sum := base + DQ[1][i];
		    If (sum >= $FF) Then
		       sum := sum - $FF;

                    p2:=q;
                    Inc(p2, 52);
                    p2^ := p2^ XOR rs_l12_alog[sum];
	       End;
               { MSB }
               p2 := inout_data;
               Inc(p2, (j*86+i*88+1) MOD 2236);
               data := p2^;

	       If (data <> 0) Then
               Begin
                    base := rs_l12_log[data];

                    sum := base + DQ[0][i];
	            If (sum >= $FF) Then
                       sum := sum - $FF;

                    p2 := q;
                    Inc(p2, 1);
	            p2^ := p2^ XOR rs_l12_alog[sum];

	            sum := base + DQ[1][i];
	            If (sum >= $FF) Then
                       sum := sum - $FF;

                    p2 := q;
                    Inc(p2, 53);
                    p2^ := p2^ XOR rs_l12_alog[sum];
	       End;
          End;
          Inc(q, 2);
     End;
End;

end.
