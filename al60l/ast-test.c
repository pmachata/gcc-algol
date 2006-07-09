#include "ast-tab.h"
#include <stdlib.h>

void
yynodefailed (YYNODESTATE * state)
{
  abort ();
}

char*
yycurrfilename (YYNODESTATE * state)
{
  return "gothic-bold.net";
}

long
yycurrlinenum (YYNODESTATE * state)
{
  return 4;
}

int
main (void)
{
  YYNODESTATE ast;
  slist_t * cmds = new_slist ();
  slist_pushback (cmds, stmt_dummy_create (&ast));
  slist_pushback (cmds, stmt_dummy_create (&ast));
  statement * s = stmt_block_create (&ast, cmds);
  dump_statement (s, stdout, 0);
  return 0;
}
