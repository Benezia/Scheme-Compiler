/* string_reverse.asm
 * Takes a pointer to a null-terminated string, 
 * and reverses it in place.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 STRING_REVERSE:
  MOV(R1, STARG(0));
  PUSH(R1);
  CALL(STRLEN);
  POP(R2);
  DECR(R0);
  ADD(R0, STARG(0));
  MOV(R1, STARG(0));
 L_STRING_REVERSE_0:
  CMP(R1, R0);
  JUMP_GE(L_STRING_REVERSE_1);
  MOV(R2, IND(R1));
  MOV(IND(R1), IND(R0));
  MOV(IND(R0), R2);
  DECR(R0);
  INCR(R1);
  JUMP(L_STRING_REVERSE_0);
 L_STRING_REVERSE_1:
  MOV(R0, STARG(0));
  RETURN;
