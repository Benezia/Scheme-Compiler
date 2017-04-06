/* char/char_to_lc.asm
 * R0 <- to_lc(ARG[0])
 *
 * Programmer: Mayer Goldberg, 2010
 */

 CHAR_TO_LC:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, FPARG(0));
  PUSH(R1);
  CALL(IS_CHAR_UC);
  POP(R1);
  CMP(R0, IMM(0));
  JUMP_EQ(L_CHAR_TO_LC);
  MOV(R0, FPARG(0));
  ADD(R0, IMM('a' - 'A'));
  JUMP(L_CTLC_EXIT);
 L_CHAR_TO_LC:
  MOV(R0, FPARG(0));
 L_CTLC_EXIT:
  POP(R1);
  POP(FP);
  RETURN;
