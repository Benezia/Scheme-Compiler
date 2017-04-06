/* scheme/make_sob_char.asm
 * Takes an integer 0 <= n < 256 as an argument, and places 
 * in R0 the corresponding character object
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_CHAR:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_CHAR);
  MOV(INDD(R0, 1), FPARG(0));
  POP(FP);
  RETURN;

