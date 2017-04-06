/* scheme/make_sob_closure.asm
 * Take pointers to an environment and some code (address of a label),
 * and place the corresponding Scheme closure in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_CLOSURE:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), IMM(T_CLOSURE));
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
  POP(FP);
  RETURN;

