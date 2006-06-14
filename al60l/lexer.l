/* :mode=c: -*-c-*-
 * Copyright (c) 2005,2006 Petr Machata
 * All rights reserved.
 */
%{
#include "cursor.h"
#include "logger.h"
#include "util.h"
#include "lexer.h"

#include "parser-tab.h"

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <setjmp.h>

typedef struct struct_lexer_rep_t
{
  logger_t * log;
  logger_t * toklog;
  cursor_t * cursor;
  yyscan_t flex_scanner;
  FILE * stream;
  int managed;
  token_kind_t last_tok;
} lexer_rep_t;

void lexer_invalid_character (lexer_rep_t * rep, char what);
void lexer_unrecognized_character (lexer_rep_t * rep, char what);
token_kind_t _lexer_got_token (void * flex_scanner, token_kind_t token_kind);

#define YY_EXTRA_TYPE lexer_rep_t * 

static int string_level = 0;
%}

%x STRING

%option noyywrap

SPACE		[ \t\n\r\v\f]

/* === RRA60 2.1 === */
LETTER		[a-zA-Z]

/* === RRA60 2.2 === */
DIGIT		[0-9]

%%

 /* <logical_value> */
"'true'"  { return _lexer_got_token (yyscanner, KWTRUE); }
"'false'" { return _lexer_got_token (yyscanner, KWFALSE); }

 /* <arithmetic_operator> */
"+" { return _lexer_got_token (yyscanner, AOPADD); }
"-" { return _lexer_got_token (yyscanner, AOPSUB); }
"*" { return _lexer_got_token (yyscanner, AOPMUL); }
"/" { return _lexer_got_token (yyscanner, AOPRDIV); }
"'div'" { return _lexer_got_token (yyscanner, AOPIDIV); }
"**" { return _lexer_got_token (yyscanner, AOPPOW); }

 /* <relational_operator> */
"<"  { return _lexer_got_token (yyscanner, ROPLT); }
"<=" { return _lexer_got_token (yyscanner, ROPLTE); }
"="  { return _lexer_got_token (yyscanner, ROPEQ); }
">=" { return _lexer_got_token (yyscanner, ROPGTE); }
">"  { return _lexer_got_token (yyscanner, ROPGT); }
"!=" { return _lexer_got_token (yyscanner, ROPNEQ); }

 /* <logical_operator> */
"==" { return _lexer_got_token (yyscanner, LOPEQ); }
"=>" { return _lexer_got_token (yyscanner, LOPIMP); }
"||" { return _lexer_got_token (yyscanner, LOPOR); }
"&&" { return _lexer_got_token (yyscanner, LOPAND); }
"!"  { return _lexer_got_token (yyscanner, LOPNOT); }

 /* <sequential_operator> */
"'goto'" { return _lexer_got_token (yyscanner, KWGOTO); }
"'if'"   { return _lexer_got_token (yyscanner, KWIF); }
"'then'" { return _lexer_got_token (yyscanner, KWTHEN); }
"'else'" { return _lexer_got_token (yyscanner, KWELSE); }
"'for'"  { return _lexer_got_token (yyscanner, KWFOR); }
"'do'"   { return _lexer_got_token (yyscanner, KWDO); }

 /* <separator> */
"," { return _lexer_got_token (yyscanner, SEPCOMMA); }
":" { return _lexer_got_token (yyscanner, SEPCOLON); }
":=" { return _lexer_got_token (yyscanner, SEPASSIGN); }
"'step'" { return _lexer_got_token (yyscanner, KWSTEP); }
"'until'" { return _lexer_got_token (yyscanner, KWUNTIL); }
"'while'" { return _lexer_got_token (yyscanner, KWWHILE); }
"'comment'" { return _lexer_got_token (yyscanner, KWCOMMENT); }

 /* <bracket> */
"(" { return _lexer_got_token (yyscanner, SEPLPAREN); }
")" { return _lexer_got_token (yyscanner, SEPRPAREN); }
"[" { return _lexer_got_token (yyscanner, SEPLBRACK); }
"]" { return _lexer_got_token (yyscanner, SEPRBRACK); }
"`" {
  BEGIN STRING;
  string_level = 1;
}
<STRING>"`" {
  ++string_level;
}
<STRING>"'" {
  if (--string_level == 0)
    {
      BEGIN INITIAL;
      return _lexer_got_token (yyscanner, LITSTRING);
    }
}
"'begin'" { return _lexer_got_token (yyscanner, KWBEGIN); }
"'end'" { return _lexer_got_token (yyscanner, KWEND); }

 /* <declarator> */
"'own'" { return _lexer_got_token (yyscanner, KWOWN); }
"'Boolean'" { return _lexer_got_token (yyscanner, KWBOOLEAN); }
"'integer'" { return _lexer_got_token (yyscanner, KWINTEGER); }
"'real'" { return _lexer_got_token (yyscanner, KWREAL); }
"'array'" { return _lexer_got_token (yyscanner, KWARRAY); }
"'switch'" { return _lexer_got_token (yyscanner, KWSWITCH); }
"'procedure'" { return _lexer_got_token (yyscanner, KWPROCEDURE); }

 /* <specificator> */
"'string'" { return _lexer_got_token (yyscanner, KWSTRING); }
"'label'" { return _lexer_got_token (yyscanner, KWLABEL); }
"'value'" { return _lexer_got_token (yyscanner, KWVALUE); }

 /* === RRA60 2.4 === */
 /* <identifier> */
{LETTER}({LETTER}|{DIGIT})* { }


"//".* {
    // skip one-line comments
}

";"   { return _lexer_got_token (yyscanner, SEPSEMICOLON); }

"."   {
  // explicitly forbid single dot, which would otherwise get
  // classified as floating point number
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  lexer_unrecognized_character (lexer, *yyget_text (yyscanner));
}

([0-9]+"."[0-9]*)|([0-9]*"."[0-9]+)|([0-9]*"'e'"[+-]?[0-9]+)|([0-9]+"."[0-9]*"'e'"[+-]?[0-9]+)|([0-9]*"."[0-9]+"'e'"[+-]?[0-9]+) {
  return _lexer_got_token (yyscanner, LITFLOAT);
  /*
  yylval.litfloat = atof (yytext);
  csr_append (Lexer::g_cursor, yytext);
  return LITFLOAT;
  */
}

[0-9]+ {
  return _lexer_got_token (yyscanner, LITINTEGER);
  /*
  yylval.litinteger = atol (yytext);
  csr_append (Lexer::g_cursor, yytext); return LITINTEGER;
  */
}

[A-Za-z_][A-Za-z0-9_]* {
  /*
  csr_append (Lexer::g_cursor, yytext);
  yylval.name = new std::string (yytext);
  return IDENTIFIER;
  */
}

<<EOF>>	{ return _lexer_got_token (yyscanner, EOFTOK); }

\n {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_nl (lexer->cursor);
}

" "+ {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_move (lexer->cursor, yyget_leng (yyscanner));
}

\t {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_tab (lexer->cursor, 8);
}

. {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_move (lexer->cursor, 1);
  lexer_invalid_character (lexer, *yytext);
}

%%

lexer_t *
new_lexer (FILE * stream, char const* filename, int manage)
{
  lexer_rep_t * ret = malloc (sizeof (lexer_rep_t));
  memset (ret, 0, sizeof (lexer_rep_t));
  jmp_buf buf;

  if (setjmp (buf) == 0)
    {
      guard_ptr (buf, 1, ret->toklog = new_logger ("token"));
      guard_ptr (buf, 1, ret->log = new_logger ("lexer"));
      guard_ptr (buf, 1, ret->cursor = new_cursor (filename));

      guard_int (buf, 1, yylex_init (&ret->flex_scanner));
      yyset_extra (ret, ret->flex_scanner);

      ret->stream = stream;
      yyrestart (stream, ret->flex_scanner);

      return (void*)ret;
    }
  else
    {
      delete_logger (ret->log);
      delete_cursor (ret->cursor);
      // yylex_init is called the last, and nothing will fail
      // afterwards.  So we don't call yylex_destroy at all.
      free (ret);
      return NULL;
    }
}

lexer_t *
new_lexer_filename (char const* filename)
{
  FILE * f = fopen (filename, "r");
  if (f == NULL)
    return NULL;

  return new_lexer (f, filename, 1);
}

void
delete_lexer (lexer_t * _lexer)
{
  lexer_rep_t * lexer = (void*)_lexer;
  if (lexer->managed)
    fclose (lexer->stream);

  if (yylex_destroy (lexer->flex_scanner))
    perror ("yylex_destroy");

  delete_logger (lexer->log);
  delete_cursor (lexer->cursor);

  free (lexer);
}

/// Procedure to write out that bad character has been hit.
/// Bad characters are considered errors.
void
lexer_invalid_character (lexer_rep_t * rep, char what)
{
  log_printf (rep->log, ll_error, "Invalid character `%c' (code %d)\n", what, (int)what);
}

void
lexer_unrecognized_character (lexer_rep_t * rep, char what)
{
  log_printf (rep->log, ll_error, "Unrecognized character `%c' (code %d)\n", what, (int)what);
}

void
lexer_next_tok (lexer_t * _lexer)
{
  lexer_rep_t * lexer = (void*)_lexer;
  lexer->last_tok = yylex (lexer->flex_scanner);
}

token_kind_t
lexer_get_tok_kind (lexer_t * _lexer)
{
  lexer_rep_t * lexer = (void*)_lexer;
  return lexer->last_tok;
}

char const*
lexer_get_tok_lexeme (lexer_t * _lexer)
{
  lexer_rep_t * lexer = (void*)_lexer;
  return yyget_text (lexer->flex_scanner);
}

int
lexer_get_tok_lexeme_len (lexer_t * _lexer)
{
  lexer_rep_t * lexer = (void*)_lexer;
  return yyget_leng (lexer->flex_scanner);
}

token_kind_t
_lexer_got_token (void * flex_scanner, token_kind_t token_kind)
{
  lexer_rep_t * lexer = yyget_extra (flex_scanner);
  char const* lexeme = yyget_text (flex_scanner);
  cursor_move (lexer->cursor, yyget_leng (flex_scanner));

  log_printf (lexer->toklog, ll_debug, "%s: %s was hit",
	      cursor_to_str (lexer->cursor), lexeme);

  return token_kind;
}
