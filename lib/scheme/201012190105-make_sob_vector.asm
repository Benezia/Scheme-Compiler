/* scheme/make_sob_vector.asm
 * Takes V1, ..., Vn, n, on the stack. Places in R0 the address
 * of a newly-allocated pointer to a Scheme vector.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_VECTOR:
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
  MOV(IND(R0), IMM(T_VECTOR));
  MOV(INDD(R0, 1), FPARG(0));
  MOV(R1, FP);
  MOV(R2, FPARG(0));
  ADD(R2, IMM(3));
  MUL(R2, IMM(WORD_SIZE));
  SUB(R1, R2);
  MOV(R2, R0);
  ADD(R2, IMM(2));
  MOV(R3, FPARG(0));
 L_MSV_LOOP:
  CMP(R3, IMM(0));
  JUMP_EQ(L_MSV_EXIT);
  MOV(IND(R2), REF(R1));
  ADD(R1, IMM(WORD_SIZE));
  INCR(R2);
  DECR(R3);
  JUMP(L_MSV_LOOP);
 L_MSV_EXIT:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;


