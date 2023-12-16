unit CDROMTableSectorScramblerUnit;

{ ******************************************************************
This pascal unit provides a procedure to do CDROM sector scrambling
as described in the yellow book - also described in ECMA-130 CDROM
specifications.  The code was written originally in C++, in CDRDAO
v1.1.5 source code available free from the internet.

I took the liberty to translate it into a Delphi unit.

Sector scrambling is to basically make any CDROM sector as random
as possible, so that the chances of regular (repeated) patterns
are minimised.  CDROM drives have a problem reading certain regular
patterns of bytes, and so this procedure is to reduce the chances.

The technique is to XOR the bytes (in the CDROM sector) by a byte
value (called a shift value).  The shift value is changed each time
we XOR the next byte.

The hardware in all CDROM drives are built to unscramble (*1) any CD
sectors that are in a CDROM track, which are recognised in the
CD TOC or sub-channel Q data with in a particular sector.

Writers which follow the MMC specs (part of SCSI-3 specs) will only
do scrambling in hardware with certain writing modes.  In RAW
writing mode, i.e. software is to provide 2352 bytes of main data +
96 bytes of sub-channel data, the software have to scramble any
CDROM sectors - the hardware will not do any scrambling.

The first 12 bytes are not scrambled, i.e. only bytes
13 to 2352 of a sector are scrambled.  Also, subchannel data are not
scrambled and are not expected here.

The 'Scramble' procedure is to take a 2352 byte CDROM sector and
scrambles it, with the result left in the parameter that was passed
in.

The code is already optimised by using a table (an array) of already
calculated shift values.  It is included from the file
Encoder_tables.pas.  Starting from offset position 12, each sector
position is XORed with each of the array subscripts in turn.

(*1) The reverse process of scrambling.
********************************************************************}

interface

Type P_Byte=^Byte;

{$I 'YB_scrambler_table.pas'}

Procedure Scramble(Sector : Pointer);

implementation

Procedure Scramble(Sector : Pointer);
Var i : Word;
    ptr_yellowbook_scrambler : ^Byte;
Begin
     ptr_yellowbook_scrambler:=@yellowbook_scrambler;
     Inc(P_Byte(Sector), 12);
     For i:=12 To 2351 Do
     Begin
          Byte(Sector^):=Byte(Sector^) XOR (ptr_yellowbook_scrambler^);
          Inc(P_Byte(Sector), 1);
          Inc(ptr_yellowbook_scrambler, 1);
     End;
End;

end.