{ Constants for memory allocation.
  I suppose these are not really needed, but constant
  declarations doesn't take up space in the compiled
  program anyway.. }
Const USER_MEM_UNIT_1K=1024;
Const USER_MEM_UNIT_4K=USER_MEM_UNIT_1K*4;
Const USER_MEM_UNIT_8K=USER_MEM_UNIT_1K*8;
Const USER_MEM_UNIT_16K=USER_MEM_UNIT_1K*16;
Const USER_MEM_UNIT_32K=USER_MEM_UNIT_1K*32;
Const USER_MEM_UNIT_64K=USER_MEM_UNIT_1K*64;
Const USER_MEM_UNIT_128K=USER_MEM_UNIT_1K*128;
Const USER_MEM_UNIT_256K=USER_MEM_UNIT_1K*256;
Const USER_MEM_UNIT_512K=USER_MEM_UNIT_1K*512;
{ VirtualAlloc doesn't actually have these bounds.
  I've made up these bounds for VirtualAlloc so that methods
  can iterate to try allocating a successful buffer size and
  to stop when an arbitrary bound is reached.. }
Const USER_MEM_UNIT_SMALLMIN=USER_MEM_UNIT_4K;   //Made up by me for smallest VirtualAlloc size.
Const USER_MEM_UNIT_SMALLMAX=USER_MEM_UNIT_64K;  //Made up by me for largest VirtualAlloc size.

{** Large buffer structure. **}
Type T_Buf=Record
                 p_buf : Pointer;
                 size  : LongWord;
           End;