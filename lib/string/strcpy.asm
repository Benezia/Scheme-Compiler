/* strcpy.asm
 * Equivalent to strcat in C:
 * R0 = dest <- strcpy(dest, src)
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 STRCPY:
  MOV(R0, STARG(0));
  MOV(R1, R0);
  MOV(R2, STARG(1));
 L_STRCPY_1:
  CMP(IND(R2), IMM('\0'));
  JUMP_EQ(L_STRCPY_2);
  MOV(IND(R1), IND(R2));
  INCR(R1);
  INCR(R2);
  JUMP(L_STRCPY_1);
 L_STRCPY_2:
  MOV(IND(R1), IMM('\0'));
  RETURN;
