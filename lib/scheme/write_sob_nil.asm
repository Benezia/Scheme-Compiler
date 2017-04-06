/* scheme/write_sob_nil.asm
 * Take a pointer to a Scheme nil object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_NIL:
  PUSH(IMM('('));
  CALL(PUTCHAR);
  PUSH(IMM(')'));
  CALL(PUTCHAR);
  DROP(2);
  RETURN;

