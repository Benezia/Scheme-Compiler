/* char/is_char_white_space.asm
 * Returns 1 if argument is a whitespace char, 0 otherwise.
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_CHAR_WHITE_SPACE:
  MOV(R0, STARG(0));
  CMP(R0, IMM(' '));
  JUMP_LE(L_IS_CHAR_WHITE_SPACE_T);
  MOV(R0, IMM(0));
  RETURN;
 L_IS_CHAR_WHITE_SPACE_T:
  MOV(R0, IMM(1));
  RETURN;
