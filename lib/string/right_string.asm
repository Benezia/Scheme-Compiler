/* right_string.asm
 * Copy the right N chars in a string:
 * R0 = dest <- right_string(dest, src, N))
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 RIGHT_STRING:
  MOV(R1, STARG(1));
  PUSH(R1);
  CALL(STRLEN);
  POP(R1);
  MOV(R3, STARG(2));
  SUB(R0, R3);
  MOV(R2, STARG(1));
  MOV(R1, STARG(0));
  PUSH(R3);
  PUSH(R0);
  PUSH(R2);
  PUSH(R1);
  CALL(MID_STRING);
  POP(R1);
  POP(R1);
  POP(R1);
  POP(R1);
  RETURN;
