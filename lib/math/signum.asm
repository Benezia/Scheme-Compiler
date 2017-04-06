/* signum.asm
 * Compute the signum function on its arg:
 * R0 <- 1, 0, -1 -- depending on the sign of ARG[0]
 *
 * Programmer: Mayer Goldberg, 2010
 */

 SIGNUM:
  CMP(STARG(0), IMM(0));
  JUMP_GT(L_SIGNUM_P);
  JUMP_LT(L_SIGNUM_N);
  MOV(R0, IMM(0));
  RETURN;
 L_SIGNUM_P:
  MOV(R0, IMM(1));
  RETURN;
 L_SIGNUM_N:
  MOV(R0, IMM(-1));
  RETURN;
