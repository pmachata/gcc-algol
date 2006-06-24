#include "lexer.h"
#include "parser.h"
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

int
main(int argc, char ** argv)
{
  if (argc != 2)
    abort ();

  lexer_t * lexer = new_lexer_filename (argv[1]);
  parser_t * parser = new_parser (lexer, 1);

  int ret = parser_parse (parser);
  printf ("%d\n", ret);

  delete_parser (parser);
  return 0;
}
