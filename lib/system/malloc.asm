/* system/malloc.asm
 * A stub for a more intelligent memory allocation code
 * that shall be re-written later...
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MALLOC:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, FPARG(0));
  MOV(R0, ADDR(0));
  ADD(ADDR(0), R1);
  POP(R1);
  POP(FP);
  RETURN;
	
