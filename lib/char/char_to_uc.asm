/* char/char_to_uc.asm
 * R0 <- to_uc(ARG[0])
 *
 * Programmer: Mayer Goldberg, 2010
 */

 CHAR_TO_UC:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, FPARG(0));
  PUSH(R1);
  CALL(IS_CHAR_LC);
  POP(R1);
  CMP(R0, IMM(0));
  JUMP_EQ(L_CTUC);
  MOV(R0, FPARG(0));
  SUB(R0, IMM('a' - 'A'));
  JUMP(L_CTUC_EXIT);
 L_CTUC:
  MOV(R0, FPARG(0));
 L_CTUC_EXIT:
  POP(R1);
  POP(FP);
  RETURN;
