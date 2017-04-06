/* strcat.asm
 * Equivalent to strcat in C:
 * R0 = dest <- strcat(dest, src)
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 STRCAT:
  MOV(R0, STARG(0));
  MOV(R1, R0);
  MOV(R2, STARG(1));
 L_STRCAT_1:
  CMP(IND(R1), IMM('\0'));
  JUMP_EQ(L_STRCAT_2);
  INCR(R1);
  JUMP(L_STRCAT_1);
 L_STRCAT_2:
  CMP(IND(R2), IMM('\0'));
  JUMP_EQ(L_STRCAT_3);
  MOV(IND(R1), IND(R2));
  INCR(R1);
  INCR(R2);
  JUMP(L_STRCAT_2);
 L_STRCAT_3:
  MOV(IND(R1), IMM('\0'));
  RETURN;
