/* ack.asm
 * Computes Ackermann's function: R0 <- ACK(ARG[0], ARG[1])
 *
 * Programmer: Mayer Goldberg, 2010
 */

 ACK:
  MOV(R1, STARG(0)); /* ARG[0] = A */
  MOV(R2, STARG(1)); /* ARG[1] = B */
  CMP(R1, IMM(0));
  JUMP_EQ(L_ACK_0_B);
  CMP(R2, IMM(0));
  JUMP_EQ(L_ACK_A_0);
  DECR(R2);
  PUSH(R2);
  PUSH(R1);
  CALL(ACK);
  POP(R1);
  POP(R1);
  MOV(STARG(1), R0);
  DECR(STARG(0));
  JUMP(ACK); /* tail-call optimization */

 L_ACK_A_0:
  MOV(STARG(1), IMM(1));
  DECR(STARG(0));
  JUMP(ACK); /* tail-call optimization */

 L_ACK_0_B:
  MOV(R0, R2);
  INCR(R0);
  RETURN;
