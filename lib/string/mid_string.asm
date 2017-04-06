/* mid_string.asm
 * Copy the middle N chars in a string, from position Pos:
 * R0 = dest <- mid_string(dest, src, Pos, N))
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MID_STRING:
  MOV(R3, STARG(3));
  MOV(R2, STARG(1));
  ADD(R2, STARG(2));
  MOV(R1, STARG(0));
  PUSH(R3);
  PUSH(R2);
  PUSH(R1);
  CALL(LEFT_STRING);
  POP(R1);
  POP(R1);
  POP(R1);
  RETURN;
