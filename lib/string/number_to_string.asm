/* number_to_string.asm
 * Takes a pointer to a dest string and an integer, and 
 * writes in the destination the string representation 
 * of the number
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 NUMBER_TO_STRING:
  MOV(R1, STARG(0));
  MOV(R2, R1);
  MOV(R3, STARG(1));
  CMP(R3, IMM(0));
  JUMP_EQ(L_NTS_0);
  JUMP_LT(L_NTS_N);
  PUSH(R3);
 L_NTS_3:
  CALL(L_NTS_1);
  POP(R2);
  MOV(R0, R1);
  RETURN;
 L_NTS_1:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R4, FPARG(0));
  CMP(R4, IMM(0));
  JUMP_EQ(L_NTS_2);
  MOV(R5, R4);
  DIV(R4, 10);
  REM(R5, 10);
  PUSH(R5);
  PUSH(R4);
  CALL(L_NTS_1);
  POP(R4);
  POP(R5);
  ADD(R5, IMM('0'));
  MOV(IND(R0), R5);
  INCR(R0);
  POP(FP);
  RETURN;
 L_NTS_2:
  MOV(R0, R2);
  POP(FP);
  RETURN;
 L_NTS_0:
  MOV(IND(R2), IMM('0'));
  INCR(R2);
  MOV(IND(R2), IMM('\0'));
  RETURN;  
 L_NTS_N:
  MOV(IND(R2), IMM('-'));
  INCR(R2);
  MOV(R4, IMM(0));
  SUB(R4, R3);
  PUSH(R4);
  JUMP(L_NTS_3);
