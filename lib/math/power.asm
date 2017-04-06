/* power.asm
 * Computes the integer power: R0 <- ARG[0] ^ ARG[1]
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 POWER:
  MOV(R1, STARG(0)); /* A */
  MOV(R2, STARG(1)); /* B */
  CMP(R2, IMM(0));
  JUMP_EQ(L_POWER_A_0);
  MOV(R3, R2);
  AND(R3, IMM(1));
  CMP(R3, IMM(0));
  JUMP_EQ(L_POWER_A_EVEN);
  SHR(R2, IMM(1));
  PUSH(R2);
  PUSH(R1);
  CALL(POWER);
  POP(R1);
  POP(R1);
  MUL(R0, R0);
  MUL(R0, STARG(0));
  RETURN;

 L_POWER_A_0:
  MOV(R0, IMM(1));
  RETURN;

 L_POWER_A_EVEN:
  SHR(R2, IMM(1));
  PUSH(R2);
  PUSH(R1);
  CALL(POWER);
  POP(R1);
  POP(R1);
  MUL(R0, R0);
  RETURN;
