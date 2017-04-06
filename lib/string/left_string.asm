/* left_string.asm
 * Copy the left N chars in a string:
 * R0 = dest <- left_string(dest, src, N))
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 LEFT_STRING:
  MOV(R0, STARG(0));
  MOV(R1, R0);
  MOV(R2, STARG(1));
  MOV(R3, STARG(2));
 L_LEFT_STRING_1:
  CMP(R3, IMM('\0'));
  JUMP_EQ(L_LEFT_STRING_2);
  MOV(IND(R1), IND(R2));
  INCR(R1);
  INCR(R2);
  DECR(R3);
  JUMP(L_LEFT_STRING_1);
 L_LEFT_STRING_2:
  MOV(IND(R1), IMM('\0'));
  RETURN;  

