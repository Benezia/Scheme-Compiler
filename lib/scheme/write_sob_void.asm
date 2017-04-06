/* scheme/write_sob_void.asm
 * Take a pointer to a Scheme void object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_VOID:
  PUSH(IMM('#'));
  CALL(PUTCHAR);
  PUSH(IMM('<'));
  CALL(PUTCHAR);
  PUSH(IMM('v'));
  CALL(PUTCHAR);
  PUSH(IMM('o'));
  CALL(PUTCHAR);
  PUSH(IMM('i'));
  CALL(PUTCHAR);
  PUSH(IMM('d'));
  CALL(PUTCHAR);
  PUSH(IMM('>'));
  CALL(PUTCHAR);
  DROP(7);
  RETURN;
