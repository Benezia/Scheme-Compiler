/* io/getchar.asm
 * Read a char from stdin
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 GETCHAR:
  IN(R0, IMM(1));
  RETURN;
