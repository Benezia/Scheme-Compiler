/* is_odd.asm
 * Tests whether its argument is odd
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_ODD:
  MOV(R0, STARG(0));
  AND(R0, IMM(1));
  CMP(R0, IMM(0));
  JUMP_EQ(L_IS_ODD_F);
  MOV(R0, IMM(1));
  RETURN;
 L_IS_ODD_F:
  MOV(R0, IMM(0));
  RETURN;
