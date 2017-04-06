/* io/write_integer.asm
 * Print a decimal representation of an integer argument to stdout
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_INTEGER:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R0, FPARG(0));
  CMP(R0, IMM(0));
  JUMP_EQ(L_WI_0);
  JUMP_LT(L_WI_N);
  PUSH(R0);
  CALL(L_WI_LOOP);
  POP(R1);
  JUMP(L_WI_EX);
 L_WI_LOOP:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(R0, IMM(0));
  JUMP_EQ(L_WI_LOOP_END);
  REM(R0, IMM(10));
  PUSH(R0);
  MOV(R0, FPARG(0));
  DIV(R0, IMM(10));
  PUSH(R0);
  CALL(L_WI_LOOP);
  POP(R0);
  POP(R0);
  ADD(R0, IMM('0'));
  PUSH(R0);
  CALL(PUTCHAR);
  POP(R0);
 L_WI_LOOP_END:
  POP(FP);
  RETURN;
 L_WI_N:
  PUSH(IMM('-'));
  CALL(PUTCHAR);
  POP(R1);
  MOV(R0, FPARG(0));
  MOV(R1, IMM(0));
  SUB(R1, R0);
  PUSH(R1);
  CALL(WRITE_INTEGER);
  POP(R1);
  JUMP(L_WI_EX);
 L_WI_0:
  PUSH(IMM('0'));
  CALL(PUTCHAR);
  POP(R0);
  JUMP(L_WI_EX);
 L_WI_EX:
  POP(R1);
  POP(FP);
  RETURN;
