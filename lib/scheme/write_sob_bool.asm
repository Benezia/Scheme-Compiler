/* scheme/write_sob_bool.asm
 * Take a pointer to a Scheme Boolean object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_BOOL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 1));
  CMP(R0, IMM(0));
  JUMP_EQ(L_WRITE_SOB_BOOL_FALSE);
  PUSH(IMM('#'));
  CALL(PUTCHAR);
  PUSH(IMM('t'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WRITE_SOB_BOOL_EXIT);
 L_WRITE_SOB_BOOL_FALSE:
  PUSH(IMM('#'));
  CALL(PUTCHAR);
  PUSH(IMM('f'));
  CALL(PUTCHAR);
  DROP(2);
 L_WRITE_SOB_BOOL_EXIT:
  POP(FP);
  RETURN;

