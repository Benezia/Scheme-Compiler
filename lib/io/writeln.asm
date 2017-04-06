/* io/write.asm
 * Takes a pointer to a null-terminated string, 
 * and prints it to STDOUT.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITELN:
  MOV(R0, STARG(0));
  PUSH(R0);
  CALL(WRITE);
  POP(R0);
  PUSH(IMM('\n'));
  CALL(PUTCHAR);
  POP(R0);
  RETURN;
