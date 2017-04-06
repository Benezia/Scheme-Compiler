/* io/tab.asm
 * Print a tab character to stdout
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 TAB:
  PUSH(IMM('\t'));
  CALL(PUTCHAR);
  POP(R0);
  RETURN;
