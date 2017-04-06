/* scheme/make_sob_void.asm
 * Create a #<void> object, and place it in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_VOID:
  PUSH(IMM(1));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_VOID);
  RETURN;
