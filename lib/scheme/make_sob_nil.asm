/* scheme/make_sob_nil.asm
 * Create a nil -- () object, and place it in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_NIL:
  PUSH(IMM(1));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_NIL);
  RETURN;
