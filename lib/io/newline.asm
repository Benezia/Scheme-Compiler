/* io/newline.asm
 * Print a newline character to stdout
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 NEWLINE:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(IMM('\n'));
  CALL(PUTCHAR);
  POP(R1);
  POP(R1);
  POP(FP);
  RETURN;
