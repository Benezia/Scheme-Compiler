/* fact.asm
 * Compute the factorial function recursively: R0 <- factorial(ARG0)
 *
 * Programmer: Mayer Goldberg, 2010	
 */
	
 FACT:
  MOV(R1, STARG(0));
  CMP(R1, IMM(0));
  JUMP_EQ(L_FACT_ZERO);
  DECR(R1);
  PUSH(R1);
  CALL(FACT);
  POP(R1); /* pop arg (N-1) to fact */
  MUL(R0, STARG(0));
  RETURN;
 L_FACT_ZERO:
  MOV(R0, IMM(1));
  RETURN;
