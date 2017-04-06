/* fib.asm
 * Compute Fibonacci numbers recursively: R0 <- FIB(ARG[0])
 *
 * Programmer: Mayer Goldberg, 2010
 */

 FIB:
  MOV(R1, STARG(0));
  CMP(R1, IMM(2));
  JUMP_GE(L_FIB_REC);
  MOV(R0, R1);
  RETURN;

 L_FIB_REC:
  DECR(R1);
  PUSH(R1);
  CALL(FIB);
  POP(R1);
  PUSH(R0);
  DECR(R1);
  PUSH(R1);
  CALL(FIB);
  POP(R1);
  POP(R1);
  ADD(R0, R1);
  RETURN

