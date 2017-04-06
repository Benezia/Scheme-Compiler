/* is_positive.asm
 * Tests whether its argument is positive
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_POSITIVE:
  CMP(STARG(0), IMM(0));
  JUMP_GT(L_IS_POSITIVE_T);
  MOV(R0, IMM(0));
  RETURN;
 L_IS_POSITIVE_T:
  MOV(R0, IMM(1));
  RETURN;
