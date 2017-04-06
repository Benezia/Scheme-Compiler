/* char/char_in_range.asm
 * R0 <- (ARG[1] <= ARG[0] <= ARG[2])
 *
 * Programmer: Mayer Goldberg, 2010
 */

 CHAR_IN_RANGE:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, FPARG(0));
  CMP(FPARG(1), R1);
  JUMP_GT(L_CIR_FALSE);
  CMP(R1, FPARG(2));
  JUMP_GT(L_CIR_FALSE);
  MOV(R0, IMM(1));
  JUMP(L_CIR_EXIT);
 L_CIR_FALSE:
  MOV(R0, IMM(0));
 L_CIR_EXIT:
  POP(R1);
  POP(FP);
  RETURN;
