#include "lexer.h"
#include "parser.h"
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

int
main(void)
{
  lexer_t * lexer = new_lexer (stdin, "<stdin>", 0);
  parser_t * parser = new_parser (lexer, 1);

  int ret = parser_parse (parser);
  printf ("%d\n", ret);

  delete_parser (parser);
  return 0;
}
