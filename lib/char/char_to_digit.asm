/* char/char_to_digit.asm
 * '0' -> 0, ..., '9' -> 9
 *
 * Programmer: Mayer Goldberg, 2010
 */

 CHAR_TO_DIGIT:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  SUB(R0, IMM('0'));
  POP(FP);
  RETURN;
