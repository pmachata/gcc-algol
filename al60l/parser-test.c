#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>

int
main(int argc, char ** argv)
{
  if (argc != 2
      && argc != 3)
    {
      fprintf (stderr,
	       "usage: %s [-string_of_options] filename\n"
	       "recognized options:\n"
	       " d : dump ast\n", argv[0]);
      fflush (stderr); // maybe superfluous, but...
      exit (-1);
    }

  char const* filename = argv[1];
  char const* argparam = NULL;
  if (argc == 3) {
    argparam = argv[2];
    if (argv[2][0] != '-')
      {
	filename = argv[2];
	argparam = argv[1];
      }
  }

  char const* dump = NULL;
  if (argparam)
    {
      dump = strchr (argparam, 'd');
    }

  lexer_t * lexer = new_lexer_filename (filename);
  lexer_set_logging (lexer, ll_warning, 1);

  parser_t * parser = new_parser (lexer, 1);

  statement_t * ast = parser_parse (parser);
  if (dump && ast)
    {
      stmt_dump (ast, stdout);
    }

  int errors =
    log_count_messages (lexer_log (lexer), ll_error)
    + log_count_messages (parser_log (parser), ll_error);

  if (errors)
    fprintf (stderr, "%d errors encountered.\n", errors);

  delete_parser (parser);
  return errors;
}
