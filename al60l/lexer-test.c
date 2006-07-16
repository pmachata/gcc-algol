#include "lexer.h"
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <math.h>

int
main(void)
{
  printf (" + simple tokens\n");
  {
    int filedes[2] = {};
    int tst = pipe(filedes);
    assert (tst == 0);

    FILE * out = fdopen (filedes[0], "r");
    FILE * in = fdopen (filedes[1], "w");

    lexer_t * a_lexer = new_lexer (out, "inmem_stream.a60", 1);
    assert (lexer (a_lexer));

    char * buffer =
      "'true'  'false' 'true''false' \n"
      "15   15.5   15'e'4   15.'e'+4   0.'e'-4 'e'+5 'e'0\n"
      "+ - * / 'div' **\n"
      "'begin' 'comment' yep we grok comments, too!;\n"
      "'end' stuff blah comment blah stuff; \n"
      "'end' blah comment blah blah stuff 'else' \n"
      "; 'comment' blah blah blah blah ; 'Boolean'\n";
    token_kind_t tokens[] =
      {KWTRUE, KWFALSE, KWTRUE, KWFALSE,
       LITINTEGER, LITREAL, LITREAL, LITREAL, LITREAL, LITREAL, LITREAL,
       AOPADD, AOPSUB, AOPMUL, AOPRDIV, AOPIDIV, AOPPOW,
       KWBEGIN,
       KWEND, SEPSEMICOLON,
       KWEND, KWELSE,
       SEPSEMICOLON, KWBOOLEAN,
       EOFTOK, -1};

    fprintf (in, "%s", buffer);
    fflush (in);
    fclose (in);

    for (token_kind_t * it = tokens; *it != -1; ++it)
      {
	lexer_next_tok (a_lexer);
	int tok = lexer_get_tok_kind (a_lexer);
	assert (tok == *it);
      }

    delete_lexer (a_lexer);
  }


  printf (" + strings and identifiers\n");
  {
    int filedes[2] = {};
    int tst = pipe(filedes);
    assert (tst == 0);

    FILE * out = fdopen (filedes[0], "r");
    FILE * in = fdopen (filedes[1], "w");

    lexer_t * a_lexer = new_lexer (out, "inmem_stream_2.a60", 1);
    assert (lexer (a_lexer));

    char * buffer =
      "`hallo `world'!' \n"
      "  ``another `complex' `str `ing''' yeah!' \n"
      " ``''  ```''`''  ``'`'`'``'`''`'' \n"
      " `a`b'c`d'e`f'g`h`i'j`k'l'm`n'o'\n"
      "q Soup V17a\n"
      " a34kTMNs MARILYN begin  \n"
      "end";
    token_kind_t tokens[] = {
      LITSTRING,
      LITSTRING,
      LITSTRING,LITSTRING,LITSTRING,
      LITSTRING,
      IDENTIFIER,IDENTIFIER,IDENTIFIER,
      IDENTIFIER,IDENTIFIER,IDENTIFIER,
      IDENTIFIER,
      EOFTOK
    };
    char * strings[] = {
      "hallo `world'!",
      "`another `complex' `str `ing''' yeah!",
      "`'", "``''`'", "`'`'`'``'`''`'",
      "a`b'c`d'e`f'g`h`i'j`k'l'm`n'o",
      "q",        "Soup",      "V17a",
      "a34kTMNs", "MARILYN",   "begin",
      "end",
      NULL
    };

    fprintf (in, "%s", buffer);
    fflush (in);
    fclose (in);

    char ** it = strings;
    token_kind_t * itk = tokens;
    for (; *it != NULL; ++it, ++itk)
      {
	lexer_next_tok (a_lexer);
	token_kind_t tk = lexer_get_tok_kind (a_lexer);
	assert (tk == *itk);
	if (tk == LITSTRING
	    || tk == IDENTIFIER)
	  {
	    estring_t * lit = lexer_get_tok_literal (a_lexer);
	    assert (estr_compare_cstr (lit, *it) == 0);
	  }
	else
	  {
	    assert (tk == EOFTOK);
	    assert (*(it+1) == NULL);
	  }
      }

    delete_lexer (a_lexer);
  }


  printf (" + floating numbers\n");
  {
    int filedes[2] = {};
    int tst = pipe(filedes);
    assert (tst == 0);

    FILE * out = fdopen (filedes[0], "r");
    FILE * in = fdopen (filedes[1], "w");

    lexer_t * a_lexer = new_lexer (out, "inmem_stream_3.a60", 1);
    assert (lexer (a_lexer));

    char * buffer =
      "1. .1 1'e'1  1'e'+1  1'e'-1 1'e'+1 'e'+10\n"
      ".1'e'-2 .01'e'+2   5100'e'+100\n"
      "15.   15.5   15'e'4   15.'e'+4   0.'e'-4 'e'+5 'e'0\n";
    double numbers[] = {
      1., .1, 1e1, 1e1, 1e-1, 1e1, 1e10,
      .1e-2, 1.0, 5100e100,
      15.0, 15.5, 15e4, 15.e+4, 0.e-4, 1e5, 1.0,
      NAN
    };

    fprintf (in, "%s", buffer);
    fflush (in);
    fclose (in);

    for (double * it = numbers; !isnan (*it); ++it)
      {
	lexer_next_tok (a_lexer);
	token_kind_t tk = lexer_get_tok_kind (a_lexer);
	if (tk == LITREAL)
	  {
	    double val = lexer_get_tok_real (a_lexer);
	    assert (val == *it);
	  }
	else
	  {
	    assert (tk == EOFTOK);
	    assert (isnan (*(it+1)));
	  }
      }

    delete_lexer (a_lexer);
  }


  printf (" + integer numbers\n");
  {
    int filedes[2] = {};
    int tst = pipe(filedes);
    assert (tst == 0);

    FILE * out = fdopen (filedes[0], "r");
    FILE * in = fdopen (filedes[1], "w");

    lexer_t * a_lexer = new_lexer (out, "inmem_stream_4.a60", 1);
    assert (lexer (a_lexer));

    char * buffer =
      "1 10 100 10000 01 09 15\n";
    long numbers[] = {
      1, 10, 100, 10000, 1, 9, 15,
      0
    };

    fprintf (in, "%s", buffer);
    fflush (in);
    fclose (in);

    for (long * it = numbers; *it != 0; ++it)
      {
	lexer_next_tok (a_lexer);
	token_kind_t tk = lexer_get_tok_kind (a_lexer);
	if (tk == LITINTEGER)
	  {
	    long val = lexer_get_tok_integer (a_lexer);
	    assert (val == *it);
	  }
	else
	  {
	    assert (tk == EOFTOK);
	    assert (*(it+1) == 0);
	  }
      }

    delete_lexer (a_lexer);
  }

  printf ("All passed.\n");
  return 0;
}
