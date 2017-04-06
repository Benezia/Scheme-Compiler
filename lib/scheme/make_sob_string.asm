/* scheme/make_sob_string.asm
 * Takes CHAR1, ..., CHARn, n, on the stack. Places in R0 the address
 * of a newly-allocated pointer to a Scheme string.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_STRING:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  MOV(R0, FPARG(0));
  ADD(R0, IMM(2));
  PUSH(R0);
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), IMM(T_STRING));
  MOV(INDD(R0, 1), FPARG(0));
  MOV(R1, FP);
  MOV(R2, FPARG(0));
  ADD(R2, IMM(3));
  SUB(R1, R2);
  MOV(R2, R0);
  ADD(R2, IMM(2));
  MOV(R3, FPARG(0));
 L_MSS_LOOP:
  CMP(R3, IMM(0));
  JUMP_EQ(L_MSS_EXIT);
  MOV(IND(R2), STACK(R1)); 
  INCR(R1);
  INCR(R2);
  DECR(R3);
  JUMP(L_MSS_LOOP);
 L_MSS_EXIT:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

