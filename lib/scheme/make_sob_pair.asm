/* scheme/make_sob_pair.asm
 * Take pointers to two sexprs, and place the corresponding 
 * Scheme pair in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_PAIR:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_PAIR);
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
  POP(FP);
  RETURN;

