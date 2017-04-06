/* scheme/write_sob_pair.asm
 * Take a pointer to a Scheme pair object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_PAIR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  PUSH(INDD(R0, 2));
  PUSH(INDD(R0, 1));
  PUSH(IMM('('));
  CALL(PUTCHAR);
  DROP(1);
  CALL(WRITE_SOB);
  DROP(1);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  PUSH(IMM('.'));
  CALL(PUTCHAR);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  DROP(3);
  CALL(WRITE_SOB);
  DROP(1);
  PUSH(IMM(')'));
  CALL(PUTCHAR);
  DROP(1);
  POP(FP);
  RETURN;

