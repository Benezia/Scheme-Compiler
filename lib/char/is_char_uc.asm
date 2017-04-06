/* char/is_char_uc.asm
 * R0 <- ('A' <= ARG[0] <= 'Z')
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_CHAR_UC:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, FPARG(0));
  PUSH(IMM('Z'));
  PUSH(IMM('A'));
  PUSH(R1);
  CALL(CHAR_IN_RANGE);
  POP(R1);
  POP(R1);
  POP(R1);
  POP(R1);
  POP(FP);
  RETURN;
