/* is_even.asm
 * Tests whether its argument is even
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_EVEN:
  MOV(R0, STARG(0));
  AND(R0, IMM(1));
  CMP(R0, IMM(0));
  JUMP_EQ(L_IS_EVEN_T);
  MOV(R0, IMM(0));
  RETURN;
 L_IS_EVEN_T:
  MOV(R0, IMM(1));
  RETURN;
