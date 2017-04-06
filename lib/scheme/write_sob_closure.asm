/* scheme/write_sob_closure.asm
 * Take a pointer to a Scheme closure object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_CLOSURE:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM('#'));
  CALL(PUTCHAR);
  PUSH(IMM('<'));
  CALL(PUTCHAR);
  PUSH(IMM('c'));
  CALL(PUTCHAR);
  PUSH(IMM('l'));
  CALL(PUTCHAR);
  PUSH(IMM('o'));
  CALL(PUTCHAR);
  PUSH(IMM('s'));
  CALL(PUTCHAR);
  PUSH(IMM('u'));
  CALL(PUTCHAR);
  PUSH(IMM('r'));
  CALL(PUTCHAR);
  PUSH(IMM('e'));
  CALL(PUTCHAR);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  PUSH(IMM('a'));
  CALL(PUTCHAR);
  PUSH(IMM('t'));
  CALL(PUTCHAR);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  DROP(13);
  PUSH(FPARG(0));
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  PUSH(IMM('e'));
  CALL(PUTCHAR);
  PUSH(IMM('n'));
  CALL(PUTCHAR);
  PUSH(IMM('v'));
  CALL(PUTCHAR);
  PUSH(IMM(':'));
  CALL(PUTCHAR);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  DROP(6);
  MOV(R0, FPARG(0));
  PUSH(INDD(R0, 1));
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  PUSH(IMM('c'));
  CALL(PUTCHAR);
  PUSH(IMM('o'));
  CALL(PUTCHAR);
  PUSH(IMM('d'));
  CALL(PUTCHAR);
  PUSH(IMM('e'));
  CALL(PUTCHAR);
  PUSH(IMM(':'));
  CALL(PUTCHAR);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  DROP(7);
  MOV(R0, FPARG(0));
  PUSH(INDD(R0, 2));
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH(IMM('>'));
  CALL(PUTCHAR);
  DROP(1);
  POP(FP);
  RETURN;

