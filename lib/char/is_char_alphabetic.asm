/* char/is_char_uc.asm
 * R0 <- ('a' <= ARG[0] <= 'z') || ('A' <= ARG[0] <= 'Z')
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_CHAR_ALPHABETIC:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, FPARG(0));
  PUSH(R1);
  CALL(IS_CHAR_UC);
  POP(R1);
  CMP(R0, IMM(0));
  JUMP_NE(L_ICAT_EXIT);
  MOV(R1, FPARG(0));
  PUSH(R1);
  CALL(IS_CHAR_LC);
  POP(R1);
  JUMP(L_ICAT_EXIT);
 L_ICAT_EXIT:
  POP(R1);
  POP(FP);
  RETURN;
