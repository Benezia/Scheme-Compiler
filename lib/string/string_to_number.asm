/* string_to_number.asm
 * Converts a source string to a number. Similar to 
 * atoi in C.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 STRING_TO_NUMBER:
  MOV(R1, STARG(0));
  MOV(R0, IMM(0));
 L_STRING_TO_NUMBER_0:
  CMP(IND(R1), IMM('\0'));
  JUMP_EQ(L_STRING_TO_NUMBER_1);
  MOV(R2, IND(R1));
  SUB(R2, IMM('0'));
  MUL(R0, IMM(10));
  ADD(R0, R2);
  INCR(R1);
  JUMP(L_STRING_TO_NUMBER_0);
 L_STRING_TO_NUMBER_1:
  RETURN;
