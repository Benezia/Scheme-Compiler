/* io/readline.asm
 * Read chars until the end of the line or the end of the file,
 * and return a pointer to a null-terminated string. This 
 * routine calls MALLOC to allocate its own memory for the 
 * string.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 READLINE:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(IMM('*'));
  PUSH(IMM(0));
  CALL(READLINE_LOOP);
  POP(R1);
  POP(R1);
  MOV(IND(R0), R1);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
 READLINE_LOOP:
  IN(R1, IMM(1)); /* read a char from stdin */
  CMP(R1, IMM('\n'));
  JUMP_EQ(READLINE_DONE);
  CMP(R1, IMM(-1));
  JUMP_EQ(READLINE_DONE);
  MOV(STARG(1), R1);
  MOV(R1, STARG(0));
  INCR(R1);
  PUSH(IMM('*'));
  PUSH(R1);
  CALL(READLINE_LOOP);
  POP(R1); /* INDEX */
  POP(R2); /* CH */
  ADD(R1, R0);
  MOV(IND(R1), R2);
  RETURN;
 READLINE_DONE:
  MOV(R1, STARG(0)); /* NUMBER OF CHARS */
  INCR(R1); /* MAKE ROOM FOR '\0' */
  PUSH(R1);
  CALL(MALLOC);
  POP(R1);
  MOV(STARG(1), IMM('\0'));
  RETURN;

