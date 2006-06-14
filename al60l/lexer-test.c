#include "lexer.h"
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

int
main(void)
{
  int filedes[2] = {};
  int tst = pipe(filedes);
  assert (tst == 0);

  FILE * out = fdopen (filedes[0], "r");
  FILE * in = fdopen (filedes[1], "w");

  lexer_t * lexer = new_lexer (out, "inmem_stream.a60", 1);

  char * buffer =
    "'true'  'false' 'true''false' \n"
    "15   15.5   15'e'4   15.'e'+4   0.'e'-4 'e'+5 'e'0\n"
    "+ - * / 'div' **\n"
    "'begin' 'end' 'Boolean'\n"
    "`hallo `world'!'   ``another `complex' `str `ing''' yeah!'";
  token_kind_t tokens[] =
    {KWTRUE, KWFALSE, KWTRUE, KWFALSE,
     LITINTEGER, LITFLOAT, LITFLOAT, LITFLOAT, LITFLOAT, LITFLOAT, LITFLOAT,
     AOPADD, AOPSUB, AOPMUL, AOPRDIV, AOPIDIV, AOPPOW,
     KWBEGIN, KWEND, KWBOOLEAN,
     LITSTRING, LITSTRING,
     EOFTOK, -1};

  fprintf (in, "%s", buffer);
  fflush (in);
  fclose (in);

  for (token_kind_t * it = tokens; *it != -1; ++it)
    {
      lexer_next_tok (lexer);
      int tok = lexer_get_tok_kind (lexer);
      assert (tok == *it);
    }

  delete_lexer (lexer);

  printf ("All passed.\n");
  return 0;
}
