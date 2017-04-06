/* is_negative.asm
 * Tests whether its argument is negative
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_NEGATIVE:
  CMP(STARG(0), IMM(0));
  JUMP_LT(L_IS_NEGATIVE_T);
  MOV(R0, IMM(0));
  RETURN;
 L_IS_NEGATIVE_T:
  MOV(R0, IMM(1));
  RETURN;
