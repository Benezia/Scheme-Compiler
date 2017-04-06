/* scheme/write_sob_integer.asm
 * Take a pointer to a Scheme symbol object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Ben Venezia 2016
 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(WRITE_SOB_STRING_NO_QUOTES);
  DROP(1);
  POP(FP);
  RETURN;
 
