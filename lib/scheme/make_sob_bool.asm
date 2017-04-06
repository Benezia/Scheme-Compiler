/* scheme/make_sob_bool.asm
 * Takes 0 or 1 as an argument, and places in R0 either #f or #t
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_BOOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_BOOL);
  MOV(INDD(R0,1), FPARG(0));
  POP(FP);
  RETURN;

