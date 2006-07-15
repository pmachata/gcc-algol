#include "lexer.h"
#include "parser.h"
#include "ast-tab.h"
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

  lexer_t * a_lexer = new_lexer_filename (filename);
  assert (lexer (a_lexer));
  assert (logger ((void*)lexer_log (a_lexer)));
  lexer_set_logging (a_lexer, ll_warning, 1);

  parser_t * a_parser = new_parser (a_lexer, 1);
  assert (parser (a_parser));
  assert (logger ((void*)parser_log (a_parser)));

  statement * ast = parser_parse (a_parser);

  if (ast)
    {
      assert (ast_isa (ast, statement));
      if (dump)
	statement_dump (ast, stdout, 0);
    }

  int errors =
    log_count_messages (lexer_log (a_lexer), ll_error)
    + log_count_messages (parser_log (a_parser), ll_error);

  if (errors)
    fprintf (stderr, "%d errors encountered.\n", errors);

  delete_parser (a_parser);
  return errors;
}
