/* char/is_char_lc.asm
 * R0 <- ('a' <= ARG[0] <= 'z')
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_CHAR_LC:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, FPARG(0));
  PUSH(IMM('z'));
  PUSH(IMM('a'));
  PUSH(R1);
  CALL(CHAR_IN_RANGE);
  POP(R1);
  POP(R1);
  POP(R1);
  POP(R1);
  POP(FP);
  RETURN;
