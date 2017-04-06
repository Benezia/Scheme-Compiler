/* strlen.asm
 * Takes a pointer to a null-terminated string, 
 * and returns its length.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 STRLEN:
  MOV(R1, STARG(0));
  MOV(R0, IMM(0));
 L_STRLEN_LOOP:
  CMP(IND(R1), IMM('\0'));
  JUMP_EQ(L_STRLEN_END);
  INCR(R1);
  INCR(R0);
  JUMP(L_STRLEN_LOOP);
 L_STRLEN_END:
  RETURN;
