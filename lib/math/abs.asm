/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */

 ABS:
  MOV(R0, STARG(0));
  CMP(R0, IMM(0));
  JUMP_LT(L_ABS_N);
  RETURN;
 L_ABS_N:
  MOV(R1, R0);
  MOV(R0, IMM(0));
  SUB(R0, R1);
  RETURN;
