/* scheme/is_sob_pair.asm
 * Take pointers to a Scheme object, and places in R0 either 0 or 1
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is a pair.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 IS_SOB_PAIR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_PAIR);
  JUMP_EQ(L_IS_SOB_PAIR_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_PAIR_EXIT);
 L_IS_SOB_PAIR_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_PAIR_EXIT:
  POP(FP);
  RETURN;



