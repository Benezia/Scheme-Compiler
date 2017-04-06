/* string_to_uc.asm
 * Convert the string to uppercase
 * R0 = dest <- string_to_uc(dest)
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 STRING_TO_UC:
  MOV(R1, STARG(0));
 L_STR_TO_UC_1:
  CMP(IND(R1), IMM('\0'));
  JUMP_EQ(L_STR_TO_UC_2);
  PUSH(R1);
  PUSH(IND(R1));
  CALL(CHAR_TO_UC);
  POP(R1);
  POP(R1);
  MOV(IND(R1), R0);
  INCR(R1);
  JUMP(L_STR_TO_UC_1);
 L_STR_TO_UC_2:
  MOV(R0, STARG(0));
  RETURN;

