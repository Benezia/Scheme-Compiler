/* scheme/make_sob_char.asm
 * Takes a ptr to string and places corresponding Scheme object in R0 
 * in R0 the corresponding character object
 * 
 * Programmer: Ben Venezia 2016
 */

 MAKE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_SYMBOL);
  MOV(INDD(R0, 1), FPARG(0));
  POP(FP);
  RETURN;

 
