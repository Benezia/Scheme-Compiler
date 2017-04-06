/* char/digit_to_char.asm
 * 0 -> '0', ..., 9 -> '9'
 *
 * Programmer: Mayer Goldberg, 2010
 */

 DIGIT_TO_CHAR:
  MOV(R0, STARG(0));
  ADD(R0, IMM('0'));
  RETURN;

